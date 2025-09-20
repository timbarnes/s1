#![allow(dead_code)]

use super::{Callable, GcObject, GcRef, SchemeValue};
use crate::eval::RunTime;
use crate::gc::heap::GcHeap;
use crate::gc_value;
use crate::kont::{CEKState, KontRef};
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

pub fn eq(heap: &GcHeap, a: GcRef, b: GcRef) -> bool {
    if std::ptr::eq(a, b) {
        true
    } else {
        match (heap.get_value(a), heap.get_value(b)) {
            (SchemeValue::Int(a), SchemeValue::Int(b)) => a == b,
            (SchemeValue::Float(a), SchemeValue::Float(b)) => a == b,
            (SchemeValue::Symbol(a), SchemeValue::Symbol(b)) => a == b,
            (SchemeValue::Bool(a), SchemeValue::Bool(b)) => a == b,
            (SchemeValue::Char(a), SchemeValue::Char(b)) => a == b,
            (SchemeValue::Port(a), SchemeValue::Port(b)) => a == b,
            (SchemeValue::Nil, SchemeValue::Nil) => true,
            (SchemeValue::Eof, SchemeValue::Eof) => true,
            (SchemeValue::Void, SchemeValue::Void) => true,
            (SchemeValue::Undefined, SchemeValue::Undefined) => true,
            _ => false,
        }
    }
}

pub fn equal(heap: &GcHeap, a: GcRef, b: GcRef) -> bool {
    match (heap.get_value(a), heap.get_value(b)) {
        (SchemeValue::Pair(a1, d1), SchemeValue::Pair(a2, d2)) => {
            equal(heap, *a1, *a2) && equal(heap, *d1, *d2)
        }
        (SchemeValue::Str(a), SchemeValue::Str(b)) => a == b,
        (SchemeValue::Vector(a), SchemeValue::Vector(b)) => {
            if a.len() != b.len() {
                return false;
            }
            for (x, y) in a.iter().zip(b.iter()) {
                if !equal(heap, *x, *y) {
                    return false;
                }
            }
            true
        }
        (SchemeValue::Callable(a), SchemeValue::Callable(b)) => match (a, b) {
            (Callable::Builtin { func: f1, .. }, Callable::Builtin { func: f2, .. }) => {
                std::ptr::fn_addr_eq(*f1, *f2)
            }
            (Callable::SpecialForm { func: f1, .. }, Callable::SpecialForm { func: f2, .. }) => {
                std::ptr::fn_addr_eq(*f1, *f2)
            }
            (
                Callable::Closure {
                    params: p1,
                    body: b1,
                    ..
                },
                Callable::Closure {
                    params: p2,
                    body: b2,
                    ..
                },
            ) => {
                if p1.len() != p2.len() {
                    return false;
                }
                for (param1, param2) in p1.iter().zip(p2.iter()) {
                    if !equal(heap, *param1, *param2) {
                        return false;
                    }
                }
                equal(heap, *b1, *b2)
            }
            (
                Callable::Macro {
                    params: p1,
                    body: b1,
                    ..
                },
                Callable::Macro {
                    params: p2,
                    body: b2,
                    ..
                },
            ) => {
                if p1.len() != p2.len() {
                    return false;
                }
                for (param1, param2) in p1.iter().zip(p2.iter()) {
                    if !equal(heap, *param1, *param2) {
                        return false;
                    }
                }
                equal(heap, *b1, *b2)
            }
            _ => false,
        },
        // For Primitive, just compare type (not function pointer)
        _ => eq(heap, a, b),
    }
}

pub fn matches_sym(symbol: GcRef, name: &str) -> bool {
    match gc_value!(symbol) {
        SchemeValue::Symbol(s_name) => s_name == name,
        _ => false,
    }
}

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
            SchemeValue::Float(f) => f.to_bits().hash(state),
            SchemeValue::Str(s) => s.hash(state),
            SchemeValue::Bool(b) => b.hash(state),
            SchemeValue::Char(c) => c.hash(state),
            _ => (), // Don't hash complex types
        }
    }
}

pub struct ListIter<'a> {
    current: Option<GcRef>,
    heap: &'a GcHeap,
}

impl<'a> ListIter<'a> {
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

pub fn is_proper_list(heap: &GcHeap, mut val: GcRef) -> bool {
    loop {
        match &heap.get_value(val) {
            SchemeValue::Pair(_, cdr) => val = *cdr,
            SchemeValue::Nil => return true,
            _ => return false,
        }
    }
}

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

pub fn get_float(heap: &mut GcHeap, val: GcRef) -> Result<f64, String> {
    match heap.get_value(val) {
        SchemeValue::Float(val) => Ok(*val),
        _ => Err("Expected float value".to_string()),
    }
}

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
        marked: false,
    };
    heap.alloc(obj)
}

/// Create a new float value.
pub fn new_float(heap: &mut GcHeap, val: f64) -> GcRef {
    let obj = GcObject {
        value: SchemeValue::Float(val),
        marked: false,
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
        marked: false,
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
        value: SchemeValue::Str(s.to_string()),
        marked: false,
    };
    heap.alloc(obj)
}

/// Create a new pair (cons cell).
pub fn new_pair(heap: &mut GcHeap, car: GcRef, cdr: GcRef) -> GcRef {
    let obj = GcObject {
        value: SchemeValue::Pair(car, cdr),
        marked: false,
    };
    heap.alloc(obj)
}

/// Create a new vector.
pub fn new_vector(heap: &mut GcHeap, elements: Vec<GcRef>) -> GcRef {
    let obj = GcObject {
        value: SchemeValue::Vector(elements),
        marked: false,
    };
    heap.alloc(obj)
}

/// Create a new continuation.
pub fn new_continuation(heap: &mut GcHeap, kont: KontRef) -> GcRef {
    let obj = GcObject {
        value: SchemeValue::Continuation(kont),
        marked: false,
    };
    heap.alloc(obj)
}

/// Create a new primitive function.
pub fn new_builtin(
    heap: &mut GcHeap,
    f: fn(&mut GcHeap, &[GcRef]) -> Result<GcRef, String>,
    doc: String,
) -> GcRef {
    let primitive = SchemeValue::Callable(Callable::Builtin { func: f, doc });
    let obj = GcObject {
        value: primitive,
        marked: false,
    };
    heap.alloc(obj)
}

pub fn new_sys_builtin(
    rt: &mut RunTime,
    f: fn(&mut RunTime, &[GcRef], &mut CEKState) -> Result<(), String>,
    doc: String,
) -> GcRef {
    let primitive = SchemeValue::Callable(Callable::SysBuiltin { func: f, doc });
    let obj = GcObject {
        value: primitive,
        marked: false,
    };
    rt.heap.alloc(obj)
}

/// Create a new special form.
pub fn new_special_form(
    heap: &mut GcHeap,
    f: fn(GcRef, &mut RunTime, &mut CEKState) -> Result<(), String>,
    doc: String,
) -> GcRef {
    let primitive = SchemeValue::Callable(Callable::SpecialForm { func: f, doc });
    let obj = GcObject {
        value: primitive,
        marked: false,
    };
    heap.alloc(obj)
}

/// Create a new closure.
pub fn new_closure(
    heap: &mut GcHeap,
    params: Vec<GcRef>,
    body: GcRef,
    env: Rc<RefCell<crate::env::Frame>>,
) -> GcRef {
    let closure = SchemeValue::Callable(Callable::Closure { params, body, env });
    let obj = GcObject {
        value: closure,
        marked: false,
    };
    heap.alloc(obj)
}

/// Create a new macro.
pub fn new_macro(
    heap: &mut GcHeap,
    params: Vec<GcRef>,
    body: GcRef,
    env: Rc<RefCell<crate::env::Frame>>,
) -> GcRef {
    let new_macro = SchemeValue::Callable(Callable::Macro { params, body, env });
    let obj = GcObject {
        value: new_macro,
        marked: false,
    };
    heap.alloc(obj)
}

/// Create a new port value.
pub fn new_port(heap: &mut GcHeap, kind: crate::io::PortKind) -> GcRef {
    let obj = GcObject {
        value: SchemeValue::Port(kind),
        marked: false,
    };
    heap.alloc(obj)
}

/// Create a new tail_call_scheduled
pub fn new_tail_call_scheduled(heap: &mut GcHeap) -> GcRef {
    let new_tail_call_scheduled = SchemeValue::TailCallScheduled;
    let obj = GcObject {
        value: new_tail_call_scheduled,
        marked: false,
    };
    heap.alloc(obj)
}

pub fn is_nil(heap: &GcHeap, expr: GcRef) -> bool {
    match &heap.get_value(expr) {
        SchemeValue::Nil => true,
        _ => false,
    }
}

pub fn car(list: GcRef) -> Result<GcRef, String> {
    match gc_value!(list) {
        SchemeValue::Pair(car, _) => Ok(*car),
        _ => Err("car: not a pair".to_string()),
    }
}

pub fn cdr(list: GcRef) -> Result<GcRef, String> {
    match gc_value!(list) {
        SchemeValue::Pair(_, cdr) => Ok(*cdr),
        _ => Err("cdr: not a pair".to_string()),
    }
}

pub fn cons(car: GcRef, cdr: GcRef, heap: &mut GcHeap) -> Result<GcRef, String> {
    let obj = GcObject {
        value: SchemeValue::Pair(car, cdr),
        marked: false,
    };
    Ok(heap.alloc(obj))
}

pub fn list(car: GcRef, heap: &mut GcHeap) -> Result<GcRef, String> {
    let obj = GcObject {
        value: SchemeValue::Pair(car, heap.nil_s()),
        marked: false,
    };
    Ok(heap.alloc(obj))
}

pub fn list2(first: GcRef, second: GcRef, heap: &mut GcHeap) -> Result<GcRef, String> {
    let obj = cons(first, list(second, heap)?, heap)?;
    Ok(obj)
}

pub fn list3(
    first: GcRef,
    second: GcRef,
    third: GcRef,
    heap: &mut GcHeap,
) -> Result<GcRef, String> {
    let obj = cons(first, cons(second, list(third, heap)?, heap)?, heap)?;
    Ok(obj)
}

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

pub fn list_from_slice(exprs: &[GcRef], heap: &mut GcHeap) -> GcRef {
    let mut list = heap.nil_s();
    for element in exprs.iter().rev() {
        list = new_pair(heap, *element, list);
    }
    list
}

// General utility to convert a Scheme list into a Vec of GcRefs
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
