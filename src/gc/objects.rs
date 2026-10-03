//! Constructors and accessors for heap objects, list helpers, and the
//! equivalence predicates (`eq`, `eqv`, `equal`).
//!
//! Allocation never collects. The evaluator collects only at two
//! checkpoints, after a closure returns and on a tail call, when the machine
//! state is fully installed (see `eval::cek`). So a builtin can build a
//! structure from several `new_*` calls without rooting the pieces in
//! between.

#![allow(dead_code)]

use super::{Callable, GcObject, GcRef, SchemeValue};
use crate::eval::{CEKState, DynamicWind, KontRef, RunTime};
use crate::gc::heap::GcHeap;
use crate::gc_value;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

/// Returns an iterator over a Scheme list in the heap.
/// Each item is a `GcRef` pointing to the car of a pair.
pub fn heap_list_iter<'a>(
    heap: &'a GcHeap,
    mut current: GcRef,
) -> impl Iterator<Item = Result<GcRef, String>> + 'a {
    std::iter::from_fn(move || match heap.get_value(current) {
        SchemeValue::Nil => None,
        SchemeValue::Pair(car, cdr) => {
            current = *cdr;
            Some(Ok(*car))
        }
        _ => Some(Err("Improper list structure in cdr".to_string())),
    })
}

/// `eq?`: the same object, or two numbers, symbols (by name), booleans,
/// characters or singletons with the same value.
pub fn eq(heap: &GcHeap, a: GcRef, b: GcRef) -> bool {
    if std::ptr::eq(a, b) {
        true
    } else {
        match (heap.get_value(a), heap.get_value(b)) {
            (SchemeValue::Int(a), SchemeValue::Int(b)) => a == b,
            (SchemeValue::Rational(a), SchemeValue::Rational(b)) => a == b,
            (SchemeValue::Float(a), SchemeValue::Float(b)) => a == b,
            (SchemeValue::Symbol(a), SchemeValue::Symbol(b)) => a == b,
            (SchemeValue::Bool(a), SchemeValue::Bool(b)) => a == b,
            (SchemeValue::Char(a), SchemeValue::Char(b)) => a == b,
            (SchemeValue::Nil, SchemeValue::Nil) => true,
            (SchemeValue::Eof, SchemeValue::Eof) => true,
            (SchemeValue::Void, SchemeValue::Void) => true,
            (SchemeValue::Undefined, SchemeValue::Undefined) => true,
            _ => false,
        }
    }
}

/// `eqv?`: `eq` except that flonums compare by bit pattern, so `0.0` and
/// `-0.0` differ while a NaN is eqv to an identical NaN, as R7RS asks.
pub fn eqv(heap: &GcHeap, a: GcRef, b: GcRef) -> bool {
    match (heap.get_value(a), heap.get_value(b)) {
        (SchemeValue::Float(x), SchemeValue::Float(y)) => x.to_bits() == y.to_bits(),
        _ => eq(heap, a, b),
    }
}

/// `equal?`: structural equality over pairs, vectors, strings and
/// bytevectors, `eqv?` otherwise. Terminates on cyclic data, as R7RS
/// requires: after many steps it starts recording which pairs of nodes are
/// being compared, and treats meeting one again as equal (the comparison of
/// that pair is already under way higher up).
pub fn equal(heap: &GcHeap, a: GcRef, b: GcRef) -> bool {
    let mut state = EqualState {
        steps: 0,
        seen: None,
    };
    equal_with(heap, a, b, &mut state)
}

/// Cycle detection for `equal`.
struct EqualState {
    /// Node pairs compared so far.
    steps: usize,
    /// Node pairs under comparison; only kept once `steps` is large.
    seen: Option<std::collections::HashSet<(usize, usize)>>,
}

/// Steps after which `equal` starts tracking visited node pairs.
const EQUAL_TRACKING_AFTER: usize = 10_000;

impl EqualState {
    /// Note a visit to the node pair (a, b); false if it was seen before.
    fn first_visit(&mut self, a: GcRef, b: GcRef) -> bool {
        self.steps += 1;
        if self.steps > EQUAL_TRACKING_AFTER {
            let seen = self.seen.get_or_insert_with(Default::default);
            return seen.insert((a as usize, b as usize));
        }
        true
    }
}

/// `equal` with explicit state; loops on the cdr so long lists don't
/// recurse.
fn equal_with(heap: &GcHeap, mut a: GcRef, mut b: GcRef, st: &mut EqualState) -> bool {
    loop {
        match (heap.get_value(a), heap.get_value(b)) {
            (SchemeValue::Pair(a1, d1), SchemeValue::Pair(a2, d2)) => {
                if !st.first_visit(a, b) {
                    return true;
                }
                if !equal_with(heap, *a1, *a2, st) {
                    return false;
                }
                // Iterate down the cdrs so long lists don't recurse deeply.
                a = *d1;
                b = *d2;
            }
            (SchemeValue::Vector(x), SchemeValue::Vector(y)) => {
                if x.len() != y.len() {
                    return false;
                }
                if !st.first_visit(a, b) {
                    return true;
                }
                return x.iter().zip(y.iter()).all(|(p, q)| equal_with(heap, *p, *q, st));
            }
            (SchemeValue::Str(x), SchemeValue::Str(y)) => return x == y,
            (SchemeValue::Bytevector(x), SchemeValue::Bytevector(y)) => return x == y,
            (SchemeValue::Callable(_), SchemeValue::Callable(_)) => return equal_callables(heap, a, b),
            _ => return eqv(heap, a, b),
        }
    }
}

/// s1 compares procedures structurally: builtins by function, closures and
/// macros by parameters and body.
fn equal_callables(heap: &GcHeap, a: GcRef, b: GcRef) -> bool {
    let (SchemeValue::Callable(a), SchemeValue::Callable(b)) = (heap.get_value(a), heap.get_value(b)) else {
        return false;
    };
    match (&**a, &**b) {
        (Callable::Builtin { func: f1, .. }, Callable::Builtin { func: f2, .. }) => std::ptr::fn_addr_eq(*f1, *f2),
        (Callable::SpecialForm { func: f1, .. }, Callable::SpecialForm { func: f2, .. }) => {
            std::ptr::fn_addr_eq(*f1, *f2)
        }
        (
            Callable::Closure { params: p1, body: b1, .. },
            Callable::Closure { params: p2, body: b2, .. },
        )
        | (Callable::Macro { params: p1, body: b1, .. }, Callable::Macro { params: p2, body: b2, .. }) => {
            p1.len() == p2.len()
                && p1.iter().zip(p2.iter()).all(|(x, y)| equal(heap, *x, *y))
                && equal(heap, *b1, *b2)
        }
        _ => std::ptr::eq(a, b),
    }
}

/// Whether `symbol` is a symbol named `name`.
pub fn matches_sym(symbol: GcRef, name: &str) -> bool {
    match gc_value!(symbol) {
        SchemeValue::Symbol(s_name) => s_name == name,
        _ => false,
    }
}

/// Whether `value` is `#f`, the only false value in Scheme.
pub fn is_false(value: GcRef) -> bool {
    match gc_value!(value) {
        SchemeValue::Bool(b) => !b,
        _ => false,
    }
}

impl Hash for SchemeValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            SchemeValue::Symbol(s) => s.hash(state),
            SchemeValue::Int(i) => i.hash(state),
            SchemeValue::Rational(r) => r.hash(state),
            SchemeValue::Float(f) => f.to_bits().hash(state),
            SchemeValue::Str(s) => s.hash(state),
            SchemeValue::Bool(b) => b.hash(state),
            SchemeValue::Char(c) => c.hash(state),
            _ => (), // Don't hash complex types
        }
    }
}

/// Iterates over the elements of a list, stopping silently at an
/// improper tail (see `heap_list_iter` for one that reports it).
pub struct ListIter<'a> {
    /// The rest of the list.
    current: Option<GcRef>,
    /// The heap the list lives in.
    heap: &'a GcHeap,
}

impl<'a> ListIter<'a> {
    /// An iterator over the list starting at `start`.
    pub fn new(start: GcRef, heap: &'a GcHeap) -> Self {
        Self {
            current: Some(start),
            heap,
        }
    }
}

impl<'a> Iterator for ListIter<'a> {
    type Item = GcRef;

    fn next(&mut self) -> Option<Self::Item> {
        let cur = self.current.take()?;
        match &self.heap.get_value(cur) {
            SchemeValue::Pair(car, cdr) => {
                self.current = Some(*cdr);
                Some(*car)
            }
            SchemeValue::Nil => None,
            _ => None, // not a proper list
        }
    }
}

/// Whether `val` is a proper list: a chain of pairs ending in `()`. Does
/// not terminate on a circular list.
pub fn is_proper_list(heap: &GcHeap, mut val: GcRef) -> bool {
    loop {
        match &heap.get_value(val) {
            SchemeValue::Pair(_, cdr) => val = *cdr,
            SchemeValue::Nil => return true,
            _ => return false,
        }
    }
}

/// The value of an exact integer that fits in an `i64`.
pub fn get_integer(heap: &mut GcHeap, val: GcRef) -> Result<i64, String> {
    match heap.get_value(val) {
        SchemeValue::Int(val) => {
            if let Some(result) = val.to_i64() {
                return Ok(result);
            } else {
                return Err("Expected integer value".to_string());
            }
        }
        _ => Err("Expected integer value".to_string()),
    }
}

/// The value of a flonum.
pub fn get_float(heap: &mut GcHeap, val: GcRef) -> Result<f64, String> {
    match heap.get_value(val) {
        SchemeValue::Float(val) => Ok(*val),
        _ => Err("Expected float value".to_string()),
    }
}

/// A copy of a string's text.
pub fn get_string(heap: &mut GcHeap, val: GcRef) -> Result<String, String> {
    match heap.get_value(val) {
        SchemeValue::Str(val) => Ok(val.to_string()),
        _ => Err("Expected string value".to_string()),
    }
}

// ============================================================================
// CONSTRUCTOR FUNCTIONS FOR SCHEME VALUES
// ============================================================================

/// Create a new integer value.
pub fn new_int(heap: &mut GcHeap, val: BigInt) -> GcRef {
    let obj = GcObject {
        value: SchemeValue::Int(val),
        marked: 0,
    };
    heap.alloc(obj)
}

/// Create a new float value.
pub fn new_float(heap: &mut GcHeap, val: f64) -> GcRef {
    let obj = GcObject {
        value: SchemeValue::Float(val),
        marked: 0,
    };
    heap.alloc(obj)
}

/// Create a new boolean value.
pub fn new_bool(heap: &mut GcHeap, val: bool) -> GcRef {
    if val { heap.true_s() } else { heap.false_s() }
}

/// Create a new character value.
pub fn new_char(heap: &mut GcHeap, val: char) -> GcRef {
    let obj = GcObject {
        value: SchemeValue::Char(val),
        marked: 0,
    };
    heap.alloc(obj)
}

/// Create a new symbol or return the identical existing symbol.
/// There can only be a single version of each symbol name, so (eq? 's 's) is always true.
pub fn get_symbol(heap: &mut GcHeap, name: &str) -> GcRef {
    heap.intern_symbol(name)
}

/// Create a new string value.
pub fn new_string(heap: &mut GcHeap, s: &str) -> GcRef {
    let obj = GcObject {
        value: SchemeValue::Str(super::SString::from(s)),
        marked: 0,
    };
    heap.alloc(obj)
}

/// Create a new pair (cons cell).
pub fn new_pair(heap: &mut GcHeap, car: GcRef, cdr: GcRef) -> GcRef {
    let obj = GcObject {
        value: SchemeValue::Pair(car, cdr),
        marked: 0,
    };
    heap.alloc(obj)
}

/// Create a new exact rational. Callers pass a non-integer in lowest terms
/// (see `SchemeValue::Rational`); whole numbers belong in `new_int`.
pub fn new_rational(heap: &mut GcHeap, val: num_rational::BigRational) -> GcRef {
    heap.alloc(GcObject {
        value: SchemeValue::Rational(Box::new(val)),
        marked: 0,
    })
}

/// Create a new error object.
pub fn new_error_object(
    heap: &mut GcHeap,
    kind: super::ErrorKind,
    message: GcRef,
    irritants: GcRef,
) -> GcRef {
    heap.alloc(GcObject {
        value: SchemeValue::ErrorObject(Box::new(super::ErrorObject {
            kind,
            message,
            irritants,
        })),
        marked: 0,
    })
}

/// Create a new bytevector.
pub fn new_bytevector(heap: &mut GcHeap, bytes: Vec<u8>) -> GcRef {
    heap.alloc(GcObject {
        value: SchemeValue::Bytevector(bytes),
        marked: 0,
    })
}

/// Create a new vector.
pub fn new_vector(heap: &mut GcHeap, elements: Vec<GcRef>) -> GcRef {
    let obj = GcObject {
        value: SchemeValue::Vector(elements),
        marked: 0,
    };
    heap.alloc(obj)
}

/// Package `vals` as the result of a `(values ...)` call: a single value is
/// returned as itself, anything else is wrapped in `SchemeValue::Values`.
pub fn new_values(heap: &mut GcHeap, vals: Vec<GcRef>) -> GcRef {
    if vals.len() == 1 {
        return vals[0];
    }
    heap.alloc(GcObject {
        value: SchemeValue::Values(vals),
        marked: 0,
    })
}

/// The values a result stands for: the elements of a `Values` package, or
/// the result itself.
pub fn unpack_values(val: GcRef) -> Vec<GcRef> {
    match gc_value!(val) {
        SchemeValue::Values(vals) => vals.clone(),
        _ => vec![val],
    }
}

/// Create a new continuation.
pub fn new_continuation(
    heap: &mut GcHeap,
    kont: KontRef,
    dw_stack: Vec<DynamicWind>,
    arg_stack: Vec<GcRef>,
    escape_len: Option<usize>,
    handlers: GcRef,
) -> GcRef {
    let obj = GcObject {
        value: SchemeValue::Continuation(Box::new(super::ContinuationData {
            kont,
            dw_stack,
            arg_stack,
            escape_len,
            handlers,
        })),
        marked: 0,
    };
    heap.alloc(obj)
}

/// Create a new primitive function.
pub fn new_builtin(
    heap: &mut GcHeap,
    name: &str,
    f: fn(&mut GcHeap, &[GcRef]) -> Result<GcRef, String>,
    doc: String,
) -> GcRef {
    let primitive = SchemeValue::Callable(Box::new(Callable::Builtin { func: f, name: name.to_string(), doc }));
    let obj = GcObject {
        value: primitive,
        marked: 0,
    };
    heap.alloc(obj)
}

/// Create a sys-builtin procedure (see `Callable::SysBuiltin`).
pub fn new_sys_builtin(
    rt: &mut RunTime,
    name: &str,
    f: fn(&mut RunTime, &[GcRef], &mut CEKState, KontRef) -> Result<(), String>,
    doc: String,
) -> GcRef {
    let primitive = SchemeValue::Callable(Box::new(Callable::SysBuiltin { func: f, name: name.to_string(), doc }));
    let obj = GcObject {
        value: primitive,
        marked: 0,
    };
    rt.heap.alloc(obj)
}

/// Create a new special form.
pub fn new_special_form(
    heap: &mut GcHeap,
    name: &str,
    f: fn(GcRef, &mut RunTime, &mut CEKState) -> Result<(), String>,
    doc: String,
) -> GcRef {
    let primitive = SchemeValue::Callable(Box::new(Callable::SpecialForm { func: f, name: name.to_string(), doc }));
    let obj = GcObject {
        value: primitive,
        marked: 0,
    };
    heap.alloc(obj)
}

/// Create a new closure.
pub fn new_closure(
    heap: &mut GcHeap,
    params: Vec<GcRef>,
    body: GcRef,
    env: Rc<RefCell<crate::env::Frame>>,
    doc: Option<String>,
    source: GcRef,
) -> GcRef {
    let closure = SchemeValue::Callable(Box::new(Callable::Closure {
        params,
        body,
        env,
        doc,
        name: None,
        source,
    }));
    let obj = GcObject {
        value: closure,
        marked: 0,
    };
    heap.alloc(obj)
}

/// Create a new macro.
pub fn new_macro(
    heap: &mut GcHeap,
    params: Vec<GcRef>,
    body: GcRef,
    env: Rc<RefCell<crate::env::Frame>>,
    doc: Option<String>,
    source: GcRef,
) -> GcRef {
    let new_macro = SchemeValue::Callable(Box::new(Callable::Macro {
        params,
        body,
        env,
        doc,
        name: None,
        source,
    }));
    let obj = GcObject {
        value: new_macro,
        marked: 0,
    };
    heap.alloc(obj)
}

/// Create a new port value.
pub fn new_port(heap: &mut GcHeap, kind: crate::io::PortKind) -> GcRef {
    let obj = GcObject {
        value: SchemeValue::Port(Box::new(kind)),
        marked: 0,
    };
    heap.alloc(obj)
}

/// Create a new tail_call_scheduled
pub fn new_tail_call_scheduled(heap: &mut GcHeap) -> GcRef {
    let new_tail_call_scheduled = SchemeValue::TailCallScheduled;
    let obj = GcObject {
        value: new_tail_call_scheduled,
        marked: 0,
    };
    heap.alloc(obj)
}

/// Whether `expr` is the empty list.
pub fn is_nil(heap: &GcHeap, expr: GcRef) -> bool {
    match &heap.get_value(expr) {
        SchemeValue::Nil => true,
        _ => false,
    }
}

/// The car of a pair.
pub fn car(list: GcRef) -> Result<GcRef, String> {
    match gc_value!(list) {
        SchemeValue::Pair(car, _) => Ok(*car),
        _ => Err("car: not a pair".to_string()),
    }
}

/// The cdr of a pair.
pub fn cdr(list: GcRef) -> Result<GcRef, String> {
    match gc_value!(list) {
        SchemeValue::Pair(_, cdr) => Ok(*cdr),
        _ => Err("cdr: not a pair".to_string()),
    }
}

/// A new pair.
pub fn cons(car: GcRef, cdr: GcRef, heap: &mut GcHeap) -> Result<GcRef, String> {
    let obj = GcObject {
        value: SchemeValue::Pair(car, cdr),
        marked: 0,
    };
    Ok(heap.alloc(obj))
}

/// A one-element list.
pub fn list(car: GcRef, heap: &mut GcHeap) -> Result<GcRef, String> {
    let obj = GcObject {
        value: SchemeValue::Pair(car, heap.nil_s()),
        marked: 0,
    };
    Ok(heap.alloc(obj))
}

/// A two-element list.
pub fn list2(first: GcRef, second: GcRef, heap: &mut GcHeap) -> Result<GcRef, String> {
    let obj = cons(first, list(second, heap)?, heap)?;
    Ok(obj)
}

/// A three-element list.
pub fn list3(
    first: GcRef,
    second: GcRef,
    third: GcRef,
    heap: &mut GcHeap,
) -> Result<GcRef, String> {
    let obj = cons(first, cons(second, list(third, heap)?, heap)?, heap)?;
    Ok(obj)
}

/// Replace the car of a pair.
pub fn set_car(pair_ref: GcRef, new_car: GcRef) -> Result<(), String> {
    unsafe {
        match &mut (*pair_ref).value {
            SchemeValue::Pair(car, _) => {
                *car = new_car;
                Ok(())
            }
            _ => Err("set-car!: not a pair".to_string()),
        }
    }
}

/// Replace the cdr of a pair.
pub fn set_cdr(pair_ref: GcRef, new_cdr: GcRef) -> Result<(), String> {
    unsafe {
        match &mut (*pair_ref).value {
            SchemeValue::Pair(_, cdr) => {
                *cdr = new_cdr;
                Ok(())
            }
            _ => Err("set-cdr!: not a pair".to_string()),
        }
    }
}

/// The element of `list` at `index`.
pub fn list_ref(heap: &mut GcHeap, mut list: GcRef, index: usize) -> Result<GcRef, String> {
    for _ in 0..index {
        match &heap.get_value(list) {
            SchemeValue::Pair(_, cdr) => {
                list = *cdr;
            }
            _ => return Err("list_ref: index out of bounds".to_string()),
        }
    }
    match &heap.get_value(list) {
        SchemeValue::Pair(car, _) => Ok(*car),
        _ => Err("list_ref: not a proper list".to_string()),
    }
}

/// A new proper list of `exprs`, in order.
pub fn list_from_slice(exprs: &[GcRef], heap: &mut GcHeap) -> GcRef {
    let mut list = heap.nil_s();
    for element in exprs.iter().rev() {
        list = new_pair(heap, *element, list);
    }
    list
}

/// The elements of a proper list, or an error if it is improper.
pub fn list_to_vec(heap: &GcHeap, list: GcRef) -> Result<Vec<GcRef>, String> {
    let mut l = list;
    let mut result = Vec::new();
    loop {
        match &heap.get_value(l) {
            SchemeValue::Nil => break Ok(result), // normal case
            SchemeValue::Pair(car, cdr) => {
                result.push(*car);
                l = *cdr;
            }
            //_ => panic!("expected proper list"),
            _ => break Err("expected proper list".to_string()),
        }
    }
}
