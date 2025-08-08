//! Mark-and-sweep memory management for Scheme values.
//!
//! This module provides a garbage-collected heap for Scheme objects, including:
//! - Basic Scheme types (integers, floats, symbols, strings, booleans, characters)
//! - Compound types (pairs, vectors, closures, environment frames)
//! - Built-in procedures and special values
//!
//! # Examples
//!
//! ```rust
//! use s1::gc::{GcHeap, new_int_simple, new_symbol_simple, new_pair_simple};
//!
//! let mut heap = GcHeap::new();
//! let x = new_int_simple(&mut heap, 42);
//! let y = new_symbol_simple(&mut heap, "foo");
//! let pair = new_pair_simple(&mut heap, x, y);
//!
//! // Manual garbage collection
//! heap.collect_garbage();
//! ```

#![allow(dead_code)]
use crate::eval::EvalContext;
use crate::io::PortKind;
use num_bigint::BigInt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

pub enum Callable {
    Builtin {
        func: fn(&mut EvalContext, &[GcRef]) -> Result<GcRef, String>,
        doc: String,
    },
    SpecialForm {
        func: fn(GcRef, &mut EvalContext, bool) -> Result<GcRef, String>,
        doc: String,
    },
    Closure {
        params: Vec<GcRef>,
        body: GcRef,
        env: Rc<RefCell<crate::env::Frame>>,
    },
    Macro {
        params: Vec<GcRef>,
        body: GcRef,
        env: Rc<RefCell<crate::env::Frame>>,
    },
}

/// Scheme values that use GcRef references.
pub enum SchemeValue {
    Int(BigInt),
    Float(f64),
    Symbol(String),
    Pair(GcRef, GcRef),
    Str(String),
    Vector(Vec<GcRef>),
    Bool(bool),
    Char(char),
    Callable(Callable),
    Nil,
    TailCallScheduled,
    Port(PortKind),
    // Extend with more types as needed.
}

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
    match (heap.get_value(a), heap.get_value(b)) {
        (SchemeValue::Int(a), SchemeValue::Int(b)) => a == b,
        (SchemeValue::Float(a), SchemeValue::Float(b)) => a == b,
        (SchemeValue::Symbol(a), SchemeValue::Symbol(b)) => a == b,
        (SchemeValue::Pair(a1, d1), SchemeValue::Pair(a2, d2)) => {
            eq(heap, *a1, *a2) && eq(heap, *d1, *d2)
        }
        (SchemeValue::Str(a), SchemeValue::Str(b)) => a == b,
        (SchemeValue::Vector(a), SchemeValue::Vector(b)) => {
            if a.len() != b.len() {
                return false;
            }
            for (x, y) in a.iter().zip(b.iter()) {
                if !eq(heap, *x, *y) {
                    return false;
                }
            }
            true
        }
        (SchemeValue::Bool(a), SchemeValue::Bool(b)) => a == b,
        (SchemeValue::Char(a), SchemeValue::Char(b)) => a == b,
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
                    if !eq(heap, *param1, *param2) {
                        return false;
                    }
                }
                eq(heap, *b1, *b2)
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
                    if !eq(heap, *param1, *param2) {
                        return false;
                    }
                }
                eq(heap, *b1, *b2)
            }
            _ => false,
        },
        // For Primitive, just compare type (not function pointer)
        (SchemeValue::Nil, SchemeValue::Nil) => true,
        (SchemeValue::Port(k1), SchemeValue::Port(k2)) => k1 == k2,
        _ => false,
    }
}

// Manual Debug implementation for SchemeValueSimple
// impl std::fmt::Debug for SchemeValue {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             SchemeValue::Int(i) => write!(f, "Int({})", i),
//             SchemeValue::Float(fl) => write!(f, "Float({})", fl),
//             SchemeValue::Symbol(s) => write!(f, "Symbol({:?})", s),
//             SchemeValue::Pair(car, cdr) => f
//                 .debug_tuple("Pair")
//                 .field(&heap.get_value(car))
//                 .field(&heap.get_value(cdr))
//                 .finish(),
//             SchemeValue::Str(s) => write!(f, "Str({:?})", s),
//             SchemeValue::Vector(v) => f.debug_tuple("Vector").field(v).finish(),
//             SchemeValue::Bool(b) => write!(f, "Bool({})", b),
//             SchemeValue::Char(c) => write!(f, "Char({:?})", c),
//             SchemeValue::Callable(c) => match c {
//                 Callable::Builtin { doc, .. } => write!(f, "Primitive({:?})", doc),
//                 Callable::SpecialForm { doc, .. } => write!(f, "SpecialForm({:?})", doc),
//                 Callable::Macro { params, body, .. } => {
//                     let param_names: Vec<String> = params
//                         .iter()
//                         .map(|p| match &heap.get_value(p) {
//                             SchemeValue::Symbol(name) => name.clone(),
//                             _ => "?".to_string(),
//                         })
//                         .collect();
//                     write!(f, "Macro({:?}, {:?})", param_names, &heap.get_value(body))
//                 }
//                 Callable::Closure { params, body, .. } => {
//                     let param_names: Vec<String> = params
//                         .iter()
//                         .map(|p| match &heap.get_value(p) {
//                             SchemeValue::Symbol(name) => name.clone(),
//                             _ => "?".to_string(),
//                         })
//                         .collect();
//                     write!(f, "Closure({:?}, {:?})", param_names, &heap.get_value(body))
//                 }
//             },
//             SchemeValue::Nil => write!(f, "Nil"),
//             SchemeValue::Port { kind } => write!(f, "Port({:?})", kind),
//             SchemeValue::TailCallScheduled => write!(f, "TailCallScheduled"),
//         }
//     }
// }

/// Reference to a garbage-collected Scheme object.
pub type GcRef = *mut GcObject;

/// A Scheme object allocated on the garbage-collected heap.
//#[derive(Debug)]
pub struct GcObject {
    /// The actual Scheme value stored in this object
    pub value: SchemeValue,
    /// Mark bit used during garbage collection
    marked: bool,
}

// impl Eq for GcObject {}

// impl PartialEq for GcObject {
//     fn eq(&self, other: &Self) -> bool {
//         heap.get_value(self) == heap.get_value(other)
//     }
// }

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

/// The garbage-collected heap that manages all Scheme objects.
pub struct GcHeap {
    // Singleton values for SchemeValueSimple
    nil_obj: Option<GcRef>,
    true_obj: Option<GcRef>,
    false_obj: Option<GcRef>,
    tail_call_obj: Option<GcRef>,
    // Free list for GcRef objects
    free_list: Vec<GcObject>,
    // All allocated GcRef objects (for potential future GC)
    objects: Vec<GcRef>,
    // Symbol table for interning symbols (name -> symbol object)
    symbol_table: HashMap<String, GcRef>,
}

impl GcHeap {
    /// Create a new empty heap.
    pub fn new() -> Self {
        let mut heap = Self {
            nil_obj: None,
            true_obj: None,
            false_obj: None,
            tail_call_obj: None,
            free_list: Vec::new(),
            objects: Vec::new(),
            symbol_table: HashMap::new(),
        };

        // Pre-allocate singleton objects
        heap.pre_allocate_simple_objects();

        heap
    }

    /// Pre-allocate singleton objects (nil, true, false) for efficiency.
    fn pre_allocate_simple_objects(&mut self) {
        // Allocate nil
        let nil_obj = GcObject {
            value: SchemeValue::Nil,
            marked: false,
        };
        self.nil_obj = Some(self.alloc(nil_obj));

        // Allocate true
        let true_obj = GcObject {
            value: SchemeValue::Bool(true),
            marked: false,
        };
        self.true_obj = Some(self.alloc(true_obj));

        // Allocate false
        let false_obj = GcObject {
            value: SchemeValue::Bool(false),
            marked: false,
        };
        self.false_obj = Some(self.alloc(false_obj));

        // Allocate tail_call
        let tail_call_obj = GcObject {
            value: SchemeValue::TailCallScheduled,
            marked: false,
        };
        self.tail_call_obj = Some(self.alloc(tail_call_obj));
    }

    /// Allocate a new object on the heap.
    pub fn alloc(&mut self, obj: GcObject) -> GcRef {
        // For now, we'll use a simple approach: allocate on the heap and leak it
        // In a real implementation, you'd want proper memory management
        let boxed = Box::new(obj);
        let raw = Box::into_raw(boxed);
        self.objects.push(raw);
        raw
    }

    pub fn get(&self, gcref: GcRef) -> &GcObject {
        unsafe { &*gcref }
    }

    pub fn get_mut(&mut self, gcref: GcRef) -> &mut GcObject {
        unsafe { &mut *gcref }
    }

    pub fn get_value(&self, r: GcRef) -> &SchemeValue {
        unsafe { &(*r).value }
    }

    pub fn get_value_mut(&self, r: GcRef) -> &mut SchemeValue {
        unsafe { &mut (*r).value }
    }

    /// Get the singleton nil value.
    pub fn nil_s(&self) -> GcRef {
        self.nil_obj.unwrap()
    }

    /// Get the singleton true value.
    pub fn true_s(&self) -> GcRef {
        self.true_obj.unwrap()
    }

    /// Get the singleton false value.
    pub fn false_s(&self) -> GcRef {
        self.false_obj.unwrap()
    }

    /// Get the tail call object.
    pub fn tail_call_s(&self) -> GcRef {
        self.tail_call_obj.unwrap()
    }

    /// Get statistics about the simple heap.
    pub fn simple_stats(&self) -> (usize, usize) {
        (self.objects.len(), self.symbol_table.len())
    }

    /// Check if a symbol exists in the symbol table.
    pub fn symbol_exists(&self, name: &str) -> Option<GcRef> {
        if let Some(existing) = self.symbol_table.get(name) {
            Some(*existing)
        } else {
            None
        }
    }

    /// Intern a symbol (ensure only one copy exists for each name).
    pub fn intern_symbol(&mut self, name: &str) -> GcRef {
        if let Some(existing) = self.symbol_table.get(name) {
            return *existing;
        }

        let symbol_obj = GcObject {
            value: SchemeValue::Symbol(name.to_string()),
            marked: false,
        };
        let symbol_ref = self.alloc(symbol_obj);
        self.symbol_table.insert(name.to_string(), symbol_ref);
        symbol_ref
    }

    /// Get statistics about the symbol table.
    pub fn symbol_table_stats(&self) -> usize {
        self.symbol_table.len()
    }

    /// Perform garbage collection (placeholder for future implementation).
    pub fn collect_garbage(&mut self) {
        // TODO: Implement mark-and-sweep garbage collection
        // For now, we just leak memory
    }

    /// Update the position of a StringPortInput in a SchemeValue::Port
    pub fn update_string_port_pos(&mut self, port_ref: PortKind, new_pos: usize) -> bool {
        crate::io::update_string_port_pos(&port_ref, new_pos)
    }

    pub fn list_iter<'a>(&'a self, list: GcRef) -> ListIter<'a> {
        ListIter {
            heap: self,
            current: Some(list),
        }
    }
}

pub struct ResultListIter {
    current: Option<GcRef>,
}

impl ResultListIter {
    pub fn new(start: GcRef) -> Self {
        Self {
            current: Some(start),
        }
    }

    pub fn next(&mut self, heap: &GcHeap) -> Result<Option<GcRef>, String> {
        let current = match self.current {
            Some(gcref) => gcref,
            None => return Ok(None),
        };

        match heap.get_value(current) {
            SchemeValue::Pair(car, cdr) => {
                self.current = Some(*cdr);
                Ok(Some(*car))
            }
            SchemeValue::Nil => {
                self.current = None;
                Ok(None)
            }
            _ => Err("Improper list in function call".to_string()),
        }
    }
}

// ============================================================================
// CONSTRUCTOR FUNCTIONS FOR SCHEME VALUES
// ============================================================================

/// Create a new object with a given value.
// pub fn new_object(heap: &mut GcHeap, value: GcRef) -> GcRef {
//     let obj = GcObject {
//         value,
//         marked: false,
//     };
//     heap.alloc(obj)
// }

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

/// Return the nil value.
pub fn get_nil(heap: &mut GcHeap) -> GcRef {
    heap.nil_s()
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

/// Create a new primitive function.
pub fn new_primitive(
    heap: &mut GcHeap,
    f: fn(&mut EvalContext, &[GcRef]) -> Result<GcRef, String>,
    doc: String,
) -> GcRef {
    let primitive = SchemeValue::Callable(Callable::Builtin { func: f, doc });
    let obj = GcObject {
        value: primitive,
        marked: false,
    };
    heap.alloc(obj)
}
/// Create a new special form.
pub fn new_special_form(
    heap: &mut GcHeap,
    f: fn(GcRef, &mut EvalContext, bool) -> Result<GcRef, String>,
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

pub fn car(heap: &GcHeap, list: GcRef) -> Result<GcRef, String> {
    match &heap.get_value(list) {
        SchemeValue::Pair(car, _) => Ok(*car),
        _ => Err("car: not a pair".to_string()),
    }
}

pub fn cdr(heap: &GcHeap, list: GcRef) -> Result<GcRef, String> {
    match &heap.get_value(list) {
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

pub fn list_from_vec(exprs: Vec<GcRef>, heap: &mut GcHeap) -> GcRef {
    let mut list = crate::gc::get_nil(heap);
    for element in exprs.iter().rev() {
        list = crate::gc::new_pair(heap, *element, list);
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
            _ => break Err("expected proper list".to_string()),
        }
    }
}

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_singleton_nil_true_false() {
        let heap = GcHeap::new();

        // Test that nil is a singleton
        let nil1 = heap.nil_s();
        let nil2 = heap.nil_s();
        assert!(std::ptr::eq(nil1, nil2));

        // Test that true is a singleton
        let true1 = heap.true_s();
        let true2 = heap.true_s();
        assert!(std::ptr::eq(true1, true2));

        // Test that false is a singleton
        let false1 = heap.false_s();
        let false2 = heap.false_s();
        assert!(std::ptr::eq(false1, false2));

        // Test that nil, true, and false are different
        assert!(!std::ptr::eq(nil1, true1));
        assert!(!std::ptr::eq(nil1, false1));
        assert!(!std::ptr::eq(true1, false1));
    }

    #[test]
    fn test_symbol_interning() {
        let mut heap = GcHeap::new();

        // Test that symbols with the same name are interned
        let sym1 = heap.intern_symbol("foo");
        let sym2 = heap.intern_symbol("foo");
        assert!(std::ptr::eq(sym1, sym2));

        // Test that different symbols are different
        let sym3 = heap.intern_symbol("bar");
        assert!(!std::ptr::eq(sym1, sym3));

        // Test symbol table stats
        assert_eq!(heap.symbol_table_stats(), 2);
    }

    #[test]
    fn test_basic_allocation() {
        let mut heap = GcHeap::new();

        // Test integer allocation
        let int_val = new_int(&mut heap, BigInt::from(42));
        match &heap.get_value(int_val) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "42"),
            _ => panic!("Expected integer"),
        }

        // Test string allocation
        let str_val = new_string(&mut heap, "hello");
        match &heap.get_value(str_val) {
            SchemeValue::Str(s) => assert_eq!(s, "hello"),
            _ => panic!("Expected string"),
        }

        // Test pair allocation
        let pair = new_pair(&mut heap, int_val, str_val);
        match &heap.get_value(pair) {
            SchemeValue::Pair(car, cdr) => {
                assert!(eq(&heap, *car, int_val));
                assert!(eq(&heap, *cdr, str_val));
            }
            _ => panic!("Expected pair"),
        }

        // Test port allocation
        let port = new_port(&mut heap, crate::io::PortKind::Stdin);
        match &heap.get_value(port) {
            SchemeValue::Port(kind) => {
                assert!(matches!(kind, crate::io::PortKind::Stdin));
            }
            _ => panic!("Expected port"),
        }
    }
}
