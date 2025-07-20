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

use std::cell::RefCell;
use std::rc::Rc;
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use num_bigint::BigInt;


/// Simple Scheme values that use GcRefSimple references.
/// This is the new system that eliminates Rc<RefCell<T>> overhead.
pub enum SchemeValueSimple {
    /// Integer values (arbitrary precision)
    Int(BigInt),
    /// Floating-point values (f64)
    Float(f64),
    /// Symbols (interned identifiers)
    Symbol(String),
    /// Cons cells for building lists and pairs
    Pair(GcRefSimple, GcRefSimple),
    /// String literals
    Str(String),
    /// Vectors (fixed-size arrays)
    Vector(Vec<GcRefSimple>),
    /// Boolean values (#t and #f)
    Bool(bool),
    /// Character literals
    Char(char),
    /// Built-in procedures (native Rust functions) with doc string
    Primitive {
        func: Rc<dyn Fn(&mut GcHeap, &[GcRefSimple]) -> Result<GcRefSimple, String>>,
        doc: String,
        is_special_form: bool,
    },
    /// Built-in procedures that need access to the evaluator
    EvaluatorPrimitive {
        func: Rc<dyn Fn(&mut crate::evalsimple::Evaluator, &[GcRefSimple]) -> Result<GcRefSimple, String>>,
        doc: String,
        is_special_form: bool,
    },
    /// Closures (functions with captured environment)
    Closure {
        /// Parameter symbols (interned symbol pointers)
        params: Vec<GcRefSimple>,
        /// Function body expression
        body: GcRefSimple,
        /// Captured environment frame
        env: Rc<RefCell<crate::env::Frame>>,
    },
    /// Empty list (nil)
    Nil,
    /// Port objects for input/output operations
    Port {
        /// Port type and configuration
        kind: crate::io::PortKind,
    },
    // Extend with more types as needed.
}

impl PartialEq for SchemeValueSimple {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (SchemeValueSimple::Int(a), SchemeValueSimple::Int(b)) => a == b,
            (SchemeValueSimple::Float(a), SchemeValueSimple::Float(b)) => a == b,
            (SchemeValueSimple::Symbol(a), SchemeValueSimple::Symbol(b)) => a == b,
            (SchemeValueSimple::Pair(a1, d1), SchemeValueSimple::Pair(a2, d2)) => {
                a1.value == a2.value && d1.value == d2.value
            }
            (SchemeValueSimple::Str(a), SchemeValueSimple::Str(b)) => a == b,
            (SchemeValueSimple::Vector(a), SchemeValueSimple::Vector(b)) => {
                if a.len() != b.len() { return false; }
                for (x, y) in a.iter().zip(b.iter()) {
                    if x.value != y.value { return false; }
                }
                true
            }
            (SchemeValueSimple::Bool(a), SchemeValueSimple::Bool(b)) => a == b,
            (SchemeValueSimple::Char(a), SchemeValueSimple::Char(b)) => a == b,
            // For Primitive, just compare type (not function pointer)
            (SchemeValueSimple::Primitive { .. }, SchemeValueSimple::Primitive { .. }) => true,
            // For EvaluatorPrimitive, just compare type (not function pointer)
            (SchemeValueSimple::EvaluatorPrimitive { .. }, SchemeValueSimple::EvaluatorPrimitive { .. }) => true,
            // For Closure, compare params and body (not env since it's captured)
            (SchemeValueSimple::Closure { params: p1, body: b1, .. }, SchemeValueSimple::Closure { params: p2, body: b2, .. }) => {
                if p1.len() != p2.len() {
                    return false;
                }
                for (param1, param2) in p1.iter().zip(p2.iter()) {
                    if param1.value != param2.value {
                        return false;
                    }
                }
                b1.value == b2.value
            }
            (SchemeValueSimple::Nil, SchemeValueSimple::Nil) => true,
            (SchemeValueSimple::Port { kind: k1 }, SchemeValueSimple::Port { kind: k2 }) => k1 == k2,
            _ => false,
        }
    }
}

// Manual Debug implementation for SchemeValueSimple
impl std::fmt::Debug for SchemeValueSimple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SchemeValueSimple::Int(i) => write!(f, "Int({})", i),
            SchemeValueSimple::Float(fl) => write!(f, "Float({})", fl),
            SchemeValueSimple::Symbol(s) => write!(f, "Symbol({:?})", s),
            SchemeValueSimple::Pair(car, cdr) => f.debug_tuple("Pair").field(&car.value).field(&cdr.value).finish(),
            SchemeValueSimple::Str(s) => write!(f, "Str({:?})", s),
            SchemeValueSimple::Vector(v) => f.debug_tuple("Vector").field(v).finish(),
            SchemeValueSimple::Bool(b) => write!(f, "Bool({})", b),
            SchemeValueSimple::Char(c) => write!(f, "Char({:?})", c),
            SchemeValueSimple::Primitive { doc, .. } => write!(f, "Primitive({})", doc),
            SchemeValueSimple::EvaluatorPrimitive { doc, .. } => write!(f, "EvaluatorPrimitive({})", doc),
            SchemeValueSimple::Closure { params, body, .. } => {
                let param_names: Vec<String> = params.iter()
                    .map(|p| match &p.value {
                        SchemeValueSimple::Symbol(name) => name.clone(),
                        _ => "?".to_string(),
                    })
                    .collect();
                write!(f, "Closure({:?}, {:?})", param_names, &body.value)
            }
            SchemeValueSimple::Nil => write!(f, "Nil"),
            SchemeValueSimple::Port { kind } => write!(f, "Port({:?})", kind),
        }
    }
}

/// Reference to a garbage-collected Scheme object.
/// This is the new system that uses static references for better performance.
pub type GcRefSimple = &'static GcObjectSimple;

/// A Scheme object allocated on the garbage-collected heap.
#[derive(Debug)]
pub struct GcObjectSimple {
    /// The actual Scheme value stored in this object
    pub value: SchemeValueSimple,
    /// Mark bit used during garbage collection
    marked: bool,
}

impl Eq for GcObjectSimple {}

impl PartialEq for GcObjectSimple {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl Hash for GcObjectSimple {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &self.value {
            SchemeValueSimple::Symbol(s) => s.hash(state),
            SchemeValueSimple::Int(i) => i.hash(state),
            SchemeValueSimple::Float(f) => f.to_bits().hash(state),
            SchemeValueSimple::Str(s) => s.hash(state),
            SchemeValueSimple::Bool(b) => b.hash(state),
            SchemeValueSimple::Char(c) => c.hash(state),
            _ => (), // Don't hash complex types
        }
    }
}

/// The garbage-collected heap that manages all Scheme objects.
pub struct GcHeap {
    // Singleton values for SchemeValueSimple
    nil_simple: Option<GcRefSimple>,
    true_simple: Option<GcRefSimple>,
    false_simple: Option<GcRefSimple>,
    // Free list for GcRefSimple objects
    free_list_simple: Vec<GcObjectSimple>,
    // All allocated GcRefSimple objects (for potential future GC)
    objects_simple: Vec<GcRefSimple>,
    // Symbol table for interning symbols (name -> symbol object)
    symbol_table: HashMap<String, GcRefSimple>,
}

impl GcHeap {
    /// Create a new empty heap.
    pub fn new() -> Self {
        let mut heap = Self {
            nil_simple: None,
            true_simple: None,
            false_simple: None,
            free_list_simple: Vec::new(),
            objects_simple: Vec::new(),
            symbol_table: HashMap::new(),
        };
        
        // Pre-allocate singleton objects
        heap.pre_allocate_simple_objects();
        
        heap
    }

    /// Pre-allocate singleton objects (nil, true, false) for efficiency.
    fn pre_allocate_simple_objects(&mut self) {
        // Allocate nil
        let nil_obj = GcObjectSimple {
            value: SchemeValueSimple::Nil,
            marked: false,
        };
        self.nil_simple = Some(self.alloc_simple(nil_obj));

        // Allocate true
        let true_obj = GcObjectSimple {
            value: SchemeValueSimple::Bool(true),
            marked: false,
        };
        self.true_simple = Some(self.alloc_simple(true_obj));

        // Allocate false
        let false_obj = GcObjectSimple {
            value: SchemeValueSimple::Bool(false),
            marked: false,
        };
        self.false_simple = Some(self.alloc_simple(false_obj));
    }

    /// Allocate a new object on the heap.
    pub fn alloc_simple(&mut self, obj: GcObjectSimple) -> GcRefSimple {
        // For now, we'll use a simple approach: allocate on the heap and leak it
        // In a real implementation, you'd want proper memory management
        let boxed = Box::new(obj);
        let ptr = Box::leak(boxed);
        self.objects_simple.push(ptr);
        ptr
    }

    /// Get the singleton nil value.
    pub fn nil_simple(&self) -> GcRefSimple {
        self.nil_simple.unwrap()
    }

    /// Get the singleton true value.
    pub fn true_simple(&self) -> GcRefSimple {
        self.true_simple.unwrap()
    }

    /// Get the singleton false value.
    pub fn false_simple(&self) -> GcRefSimple {
        self.false_simple.unwrap()
    }

    /// Get statistics about the simple heap.
    pub fn simple_stats(&self) -> (usize, usize) {
        (self.objects_simple.len(), self.symbol_table.len())
    }

    /// Intern a symbol (ensure only one copy exists for each name).
    pub fn intern_symbol(&mut self, name: &str) -> GcRefSimple {
        if let Some(existing) = self.symbol_table.get(name) {
            return *existing;
        }
        
        let symbol_obj = GcObjectSimple {
            value: SchemeValueSimple::Symbol(name.to_string()),
            marked: false,
        };
        let symbol_ref = self.alloc_simple(symbol_obj);
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
}

// ============================================================================
// CONSTRUCTOR FUNCTIONS FOR SCHEME VALUES
// ============================================================================

/// Create a new integer value.
pub fn new_int_simple(heap: &mut GcHeap, val: BigInt) -> GcRefSimple {
    let obj = GcObjectSimple {
        value: SchemeValueSimple::Int(val),
        marked: false,
    };
    heap.alloc_simple(obj)
}

/// Create a new float value.
pub fn new_float_simple(heap: &mut GcHeap, val: f64) -> GcRefSimple {
    let obj = GcObjectSimple {
        value: SchemeValueSimple::Float(val),
        marked: false,
    };
    heap.alloc_simple(obj)
}

/// Create a new boolean value.
pub fn new_bool_simple(heap: &mut GcHeap, val: bool) -> GcRefSimple {
    if val {
        heap.true_simple()
    } else {
        heap.false_simple()
    }
}

/// Create a new character value.
pub fn new_char_simple(heap: &mut GcHeap, val: char) -> GcRefSimple {
    let obj = GcObjectSimple {
        value: SchemeValueSimple::Char(val),
        marked: false,
    };
    heap.alloc_simple(obj)
}

/// Create a new symbol value (interned).
pub fn new_symbol_simple(heap: &mut GcHeap, name: &str) -> GcRefSimple {
    heap.intern_symbol(name)
}

/// Create a new string value.
pub fn new_string_simple(heap: &mut GcHeap, s: &str) -> GcRefSimple {
    let obj = GcObjectSimple {
        value: SchemeValueSimple::Str(s.to_string()),
        marked: false,
    };
    heap.alloc_simple(obj)
}

/// Create a new nil value.
pub fn new_nil_simple(heap: &mut GcHeap) -> GcRefSimple {
    heap.nil_simple()
}

/// Create a new pair (cons cell).
pub fn new_pair_simple(heap: &mut GcHeap, car: GcRefSimple, cdr: GcRefSimple) -> GcRefSimple {
    let obj = GcObjectSimple {
        value: SchemeValueSimple::Pair(car, cdr),
        marked: false,
    };
    heap.alloc_simple(obj)
}

/// Create a new vector.
pub fn new_vector_simple(heap: &mut GcHeap, elements: Vec<GcRefSimple>) -> GcRefSimple {
    let obj = GcObjectSimple {
        value: SchemeValueSimple::Vector(elements),
        marked: false,
    };
    heap.alloc_simple(obj)
}

/// Create a new primitive function.
pub fn new_primitive_simple(
    heap: &mut GcHeap,
    f: Rc<dyn Fn(&mut GcHeap, &[GcRefSimple]) -> Result<GcRefSimple, String>>,
    doc: String,
    is_special_form: bool,
) -> GcRefSimple {
    let obj = GcObjectSimple {
        value: SchemeValueSimple::Primitive {
            func: f,
            doc,
            is_special_form,
        },
        marked: false,
    };
    heap.alloc_simple(obj)
}

/// Create a new closure.
pub fn new_closure_simple(
    heap: &mut GcHeap,
    params: Vec<GcRefSimple>,
    body: GcRefSimple,
    env: Rc<RefCell<crate::env::Frame>>,
) -> GcRefSimple {
    let obj = GcObjectSimple {
        value: SchemeValueSimple::Closure {
            params,
            body,
            env,
        },
        marked: false,
    };
    heap.alloc_simple(obj)
}

/// Create a new evaluator-aware primitive function.
pub fn new_evaluator_primitive_simple(
    heap: &mut GcHeap,
    f: Rc<dyn Fn(&mut crate::evalsimple::Evaluator, &[GcRefSimple]) -> Result<GcRefSimple, String>>,
    doc: String,
    is_special_form: bool,
) -> GcRefSimple {
    let obj = GcObjectSimple {
        value: SchemeValueSimple::EvaluatorPrimitive {
            func: f,
            doc,
            is_special_form,
        },
        marked: false,
    };
    heap.alloc_simple(obj)
}

/// Create a new port value.
pub fn new_port_simple(heap: &mut GcHeap, kind: crate::io::PortKind) -> GcRefSimple {
    let obj = GcObjectSimple {
        value: SchemeValueSimple::Port { kind },
        marked: false,
    };
    heap.alloc_simple(obj)
}

mod tests {
    use super::*;
    use num_bigint::BigInt;

    #[test]
    fn test_singleton_nil_true_false() {
        let mut heap = GcHeap::new();
        
        // Test that nil is a singleton
        let nil1 = heap.nil_simple();
        let nil2 = heap.nil_simple();
        assert!(std::ptr::eq(nil1, nil2));
        
        // Test that true is a singleton
        let true1 = heap.true_simple();
        let true2 = heap.true_simple();
        assert!(std::ptr::eq(true1, true2));
        
        // Test that false is a singleton
        let false1 = heap.false_simple();
        let false2 = heap.false_simple();
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
        let int_val = new_int_simple(&mut heap, BigInt::from(42));
        match &int_val.value {
            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "42"),
            _ => panic!("Expected integer"),
        }
        
        // Test string allocation
        let str_val = new_string_simple(&mut heap, "hello");
        match &str_val.value {
            SchemeValueSimple::Str(s) => assert_eq!(s, "hello"),
            _ => panic!("Expected string"),
        }
        
        // Test pair allocation
        let pair = new_pair_simple(&mut heap, int_val, str_val);
        match &pair.value {
            SchemeValueSimple::Pair(car, cdr) => {
                assert!(std::ptr::eq(*car, int_val));
                assert!(std::ptr::eq(*cdr, str_val));
            }
            _ => panic!("Expected pair"),
        }
        
        // Test port allocation
        let port = new_port_simple(&mut heap, crate::io::PortKind::Stdin);
        match &port.value {
            SchemeValueSimple::Port { kind } => {
                assert!(matches!(kind, crate::io::PortKind::Stdin));
            }
            _ => panic!("Expected port"),
        }
    }
}