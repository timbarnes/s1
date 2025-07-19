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
//! use s1::gc::{GcHeap, new_int, new_symbol, new_pair};
//!
//! let mut heap = GcHeap::new();
//! let x = new_int(&mut heap, 42);
//! let y = new_symbol(&mut heap, "foo");
//! let pair = new_pair(&mut heap, x, y);
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
use num_traits::ToPrimitive;

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
        }
    }
}

/// The core type representing a Scheme value in the heap.
///
/// All Scheme values are allocated on the garbage-collected heap and referenced
/// through `GcRef` pointers. This enum covers the complete set of Scheme data types.
#[derive(Clone)]
pub enum SchemeValue {
    /// Integer values (arbitrary precision)
    Int(BigInt),
    /// Floating-point values (f64)
    Float(f64),
    /// Symbols (interned identifiers)
    Symbol(String),
    /// Cons cells for building lists and pairs
    Pair(GcRef, GcRef),
    /// String literals
    Str(String),
    /// Vectors (fixed-size arrays)
    Vector(Vec<GcRef>),
    /// Closures (functions with captured environment)
    Closure {
        /// Parameter symbols (interned symbol pointers)
        params: Vec<GcRef>,
        /// Function body expression
        body: GcRef,
        /// Captured environment
        env: GcRef,
    },
    /// Boolean values (#t and #f)
    Bool(bool),
    /// Character literals
    Char(char),
    /// Built-in procedures (native Rust functions) with doc string
    Primitive {
        func: Rc<dyn Fn(&mut GcHeap, &[GcRef]) -> Result<GcRef, String>>,
        doc: String,
        is_special_form: bool,
    },
    /// Special forms that need access to the evaluator
    // SpecialForm {
    //     func: Rc<dyn Fn(&mut crate::eval::Evaluator, &[GcRef]) -> Result<GcRef, String> + 'static>,
    //     doc: String,
    // },
    /// Environment frames (variable bindings)
    EnvFrame(HashMap<String, GcRef>),
    /// Empty list (nil)
    Nil,
    // Extend with more types (Errors, Continuations, etc) as needed.
}

// Manual Debug implementation for SchemeValue
impl std::fmt::Debug for SchemeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SchemeValue::Int(i) => write!(f, "Int({})", i),
            SchemeValue::Float(fl) => write!(f, "Float({})", fl),
            SchemeValue::Symbol(s) => write!(f, "Symbol({:?})", s),
            SchemeValue::Pair(car, cdr) => f.debug_tuple("Pair").field(&car.borrow().value).field(&cdr.borrow().value).finish(),
            SchemeValue::Str(s) => write!(f, "Str({:?})", s),
            SchemeValue::Vector(v) => f.debug_tuple("Vector").field(v).finish(),
            SchemeValue::Closure { params, body, env } => f.debug_struct("Closure").field("params", params).field("body", &body.borrow().value).field("env", &env.borrow().value).finish(),
            SchemeValue::Bool(b) => write!(f, "Bool({})", b),
            SchemeValue::Char(c) => write!(f, "Char({:?})", c),
            SchemeValue::Primitive { doc, .. } => write!(f, "Primitive({})", doc),
            // SchemeValue::SpecialForm { doc, .. } => write!(f, "SpecialForm({})", doc),
            SchemeValue::EnvFrame(map) => write!(f, "EnvFrame({} bindings)", map.len()),
            SchemeValue::Nil => write!(f, "Nil"),
        }
    }
}

impl PartialEq for SchemeValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (SchemeValue::Int(a), SchemeValue::Int(b)) => a == b,
            (SchemeValue::Float(a), SchemeValue::Float(b)) => a == b,
            (SchemeValue::Symbol(a), SchemeValue::Symbol(b)) => a == b,
            (SchemeValue::Pair(a1, d1), SchemeValue::Pair(a2, d2)) => {
                a1.borrow().value == a2.borrow().value && d1.borrow().value == d2.borrow().value
            }
            (SchemeValue::Str(a), SchemeValue::Str(b)) => a == b,
            (SchemeValue::Vector(a), SchemeValue::Vector(b)) => {
                if a.len() != b.len() { return false; }
                for (x, y) in a.iter().zip(b.iter()) {
                    if x.borrow().value != y.borrow().value { return false; }
                }
                true
            }
            (
                SchemeValue::Closure { params: p1, body: b1, env: e1 },
                SchemeValue::Closure { params: p2, body: b2, env: e2 },
            ) => {
                // Compare params by comparing each symbol's value
                if p1.len() != p2.len() {
                    return false;
                }
                for (param1, param2) in p1.iter().zip(p2.iter()) {
                    if param1.borrow().value != param2.borrow().value {
                        return false;
                    }
                }
                b1.borrow().value == b2.borrow().value && e1.borrow().value == e2.borrow().value
            }
            (SchemeValue::Bool(a), SchemeValue::Bool(b)) => a == b,
            (SchemeValue::Char(a), SchemeValue::Char(b)) => a == b,
            // For Primitive and Port, just compare type (not function pointer or port identity)
            (SchemeValue::Primitive { .. }, SchemeValue::Primitive { .. }) => true,
            // (SchemeValue::SpecialForm { .. }, SchemeValue::SpecialForm { .. }) => true,
            (SchemeValue::EnvFrame(a), SchemeValue::EnvFrame(b)) => {
                if a.len() != b.len() { return false; }
                for (k, va) in a.iter() {
                    match b.get(k) {
                        Some(vb) => if va.borrow().value != vb.borrow().value { return false; },
                        None => return false,
                    }
                }
                true
            }
            (SchemeValue::Nil, SchemeValue::Nil) => true,
            _ => false,
        }
    }
}

/// An indirecting pointer to a heap-allocated Scheme object.
///
/// This is the primary way to reference Scheme values. The underlying object
/// is managed by the garbage collector and will be automatically freed when
/// no longer reachable.
pub type GcRef = Rc<RefCell<GcObject>>;

/// Simple reference-based GC system.
/// Since the garbage collector is the only mechanism for deleting GcObject instances,
/// we can safely use &GcObject references throughout the system.
/// This eliminates the overhead of Rc<RefCell<T>> by using direct references.
/// # Migration Path
/// We're gradually migrating from GcRef (Rc<RefCell<GcObject>>) to GcRefSimple (&'static GcObjectSimple).
/// This provides better performance and simpler code.
pub type GcRefSimple = &'static GcObjectSimple;

/// Wrapper for heap storage; includes a mark bit for garbage collection.
///
/// Each heap object contains a Scheme value and a mark bit used during
/// garbage collection to track reachable objects.
#[derive(Debug)]
pub struct GcObject {
    /// The actual Scheme value stored in this object
    pub value: SchemeValue,
    /// Mark bit used during garbage collection
    marked: bool,
}

/// Simple wrapper for heap storage using the new GC system.
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
        std::ptr::eq(self, other)
    }
}

impl Hash for GcObjectSimple {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::ptr::hash(self, state);
    }
}

/// The garbage collected heap, with root management APIs.
///
/// This is the central memory manager for all Scheme objects. It provides:
/// - Object allocation
/// - Root set management (keeps objects alive across GC)
/// - Manual garbage collection
/// - Memory statistics
pub struct GcHeap {
    /// All objects currently allocated on the heap
    objects: Vec<GcRef>,
    /// Root references that keep objects alive
    roots: Vec<GcRef>,
    nil: Option<GcRef>,
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
    /// Create a new (empty) heap.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use s1::gc::GcHeap;
    ///
    /// let mut heap = GcHeap::new();
    /// assert_eq!(heap.heap_size(), 0);
    /// ```
    pub fn new() -> Self {
        let mut heap = GcHeap {
            objects: Vec::new(),
            roots: Vec::new(),
            nil: None,
            nil_simple: None,
            true_simple: None,
            false_simple: None,
            free_list_simple: Vec::new(),
            objects_simple: Vec::new(),
            symbol_table: HashMap::new(),
        };
        
        // Pre-allocate objects for the free list
        heap.pre_allocate_simple_objects();
        // Allocate singletons for the new system
        let nil_simple = heap.alloc_simple(GcObjectSimple {
            value: SchemeValueSimple::Nil,
            marked: false,
        });
        let t = heap.alloc_simple(GcObjectSimple {
            value: SchemeValueSimple::Bool(true),
            marked: false,
        });
        let f = heap.alloc_simple(GcObjectSimple {
            value: SchemeValueSimple::Bool(false),
            marked: false,
        });
        heap.nil_simple = Some(nil_simple);
        heap.true_simple = Some(t);
        heap.false_simple = Some(f);
        // Allocate singleton for the old system
        let nil_obj = heap.alloc(SchemeValue::Nil);
        heap.nil = Some(nil_obj);
        heap
    }

    /// Allocate a new SchemeValue on the heap and return a reference.
    ///
    /// The returned `GcRef` can be used to access and modify the object.
    /// The object will be automatically garbage collected when no longer reachable.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use s1::gc::{GcHeap, SchemeValue};
    ///
    /// let mut heap = GcHeap::new();
    /// let obj = heap.alloc(SchemeValue::Int(42));
    /// assert_eq!(obj.borrow().value, SchemeValue::Int(42));
    /// ```
    pub fn alloc(&mut self, value: SchemeValue) -> GcRef {
        let obj = Rc::new(RefCell::new(GcObject {
            value,
            marked: false,
        }));
        self.objects.push(obj.clone());
        obj
    }

    /// Add a GcRef to the root set (keeps object alive across GC).
    ///
    /// Objects in the root set are considered always reachable and will
    /// not be collected by the garbage collector.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use s1::gc::{GcHeap, new_int};
    ///
    /// let mut heap = GcHeap::new();
    /// let obj = new_int(&mut heap, 42);
    /// heap.add_root(obj.clone());
    /// heap.collect_garbage();
    /// // obj is still alive due to root reference
    /// ```
    pub fn add_root(&mut self, obj: GcRef) {
        self.roots.push(obj);
    }

    /// Remove a GcRef from the root set.
    ///
    /// After removal, the object may be collected if no other references exist.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use s1::gc::{GcHeap, new_int};
    ///
    /// let mut heap = GcHeap::new();
    /// let obj = new_int(&mut heap, 42);
    /// heap.add_root(obj.clone());
    /// heap.remove_root(&obj);
    /// // obj may be collected if no other references exist
    /// ```
    pub fn remove_root(&mut self, obj: &GcRef) {
        self.roots.retain(|r| !Rc::ptr_eq(r, obj));
    }

    /// Perform mark-and-sweep garbage collection.
    ///
    /// This method:
    /// 1. Marks all objects reachable from the root set
    /// 2. Sweeps away unmarked objects
    /// 3. Clears mark bits for remaining objects
    ///
    /// # Examples
    ///
    /// ```rust
    /// use s1::gc::{GcHeap, new_int};
    ///
    /// let mut heap = GcHeap::new();
    /// let obj1 = new_int(&mut heap, 42);
    /// let obj2 = new_int(&mut heap, 100);
    /// heap.add_root(obj1.clone());
    /// 
    /// let before = heap.heap_size();
    /// heap.collect_garbage();
    /// let after = heap.heap_size();
    /// // after should be less than before (obj2 was collected)
    /// ```
    pub fn collect_garbage(&mut self) {
        // Mark phase
        for root in &self.roots {
            Self::mark_value(root);
        }

        // Sweep phase: retain only marked objects, clear mark bits.
        self.objects.retain(|obj| {
            let marked = obj.borrow().marked;
            if marked {
                obj.borrow_mut().marked = false;
                true
            } else {
                // Dropped out of heap - will be freed if no other Rc references.
                false
            }
        });
    }

    /// Recursively mark reachable objects.
    ///
    /// This is an internal method used during garbage collection to mark
    /// all objects that are reachable from a given starting point.
    fn mark_value(obj: &GcRef) {
        let mut o = obj.borrow_mut();
        if o.marked {
            return;
        }
        o.marked = true;

        match &o.value {
            SchemeValue::Pair(car, cdr) => {
                Self::mark_value(car);
                Self::mark_value(cdr);
            }
            SchemeValue::Vector(vec) => {
                for elem in vec {
                    Self::mark_value(elem);
                }
            }
            SchemeValue::Closure { body, env, .. } => {
                Self::mark_value(body);
                Self::mark_value(env);
            }
            SchemeValue::EnvFrame(map) => {
                for v in map.values() {
                    Self::mark_value(v);
                }
            }
            // Bool, Char, Primitive, Port: nothing to mark
            _ => {}
        }
    }

    /// Return the total number of tracked heap objects (for stats/testing).
    ///
    /// This includes all objects currently allocated, whether reachable or not.
    /// After garbage collection, this number will decrease if unreachable
    /// objects were collected.
    pub fn heap_size(&self) -> usize {
        self.objects.len()
    }

    /// Return the number of root references (for stats/testing).
    ///
    /// Roots are objects that are kept alive across garbage collection.
    pub fn root_count(&self) -> usize {
        self.roots.len()
    }

    pub fn nil(&self) -> GcRef {
        self.nil.as_ref().unwrap().clone()
    }

    /// Allocate a new GcObjectSimple on the heap and return a GcRefSimple.
    /// Pre-allocate objects for the free list
    fn pre_allocate_simple_objects(&mut self) {
        const INITIAL_ALLOCATION: usize = 1000;
        for _ in 0..INITIAL_ALLOCATION {
            self.free_list_simple.push(GcObjectSimple {
                value: SchemeValueSimple::Nil, // Placeholder value
                marked: false,
            });
        }
    }

    /// Allocate a new GcObjectSimple using the free list
    pub fn alloc_simple(&mut self, obj: GcObjectSimple) -> GcRefSimple {
        // Try to get an object from the free list first
        if let Some(mut free_obj) = self.free_list_simple.pop() {
            // Reuse the free object by updating its value
            free_obj.value = obj.value;
            free_obj.marked = false;
            let boxed = Box::new(free_obj);
            let leaked = Box::leak(boxed);
            self.objects_simple.push(leaked);
            leaked
        } else {
            // Free list is empty, allocate a new object
            let boxed = Box::new(obj);
            let leaked = Box::leak(boxed);
            self.objects_simple.push(leaked);
            leaked
        }
    }

    /// Get the nil value using the reference-based system.
    pub fn nil_simple(&self) -> GcRefSimple {
        self.nil_simple.expect("nil_simple not initialized")
    }
    pub fn true_simple(&self) -> GcRefSimple {
        self.true_simple.expect("true_simple not initialized")
    }
    pub fn false_simple(&self) -> GcRefSimple {
        self.false_simple.expect("false_simple not initialized")
    }

    /// Get statistics about the simple allocation system
    pub fn simple_stats(&self) -> (usize, usize) {
        (self.objects_simple.len(), self.free_list_simple.len())
    }

    /// Intern a symbol by name, returning the same symbol object for the same name.
    ///
    /// This ensures that symbols with the same name are the same object,
    /// enabling fast comparison and memory efficiency.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use s1::gc::GcHeap;
    ///
    /// let mut heap = GcHeap::new();
    /// let sym1 = heap.intern_symbol("foo");
    /// let sym2 = heap.intern_symbol("foo");
    /// assert!(std::ptr::eq(sym1, sym2)); // Same object
    /// ```
    pub fn intern_symbol(&mut self, name: &str) -> GcRefSimple {
        if let Some(existing_symbol) = self.symbol_table.get(name) {
            *existing_symbol
        } else {
            // Create new symbol and add to table
            let new_symbol = self.alloc_simple(GcObjectSimple {
                value: SchemeValueSimple::Symbol(name.to_string()),
                marked: false,
            });
            self.symbol_table.insert(name.to_string(), new_symbol);
            new_symbol
        }
    }

    /// Get statistics about the symbol table.
    ///
    /// Returns the number of interned symbols.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use s1::gc::GcHeap;
    ///
    /// let mut heap = GcHeap::new();
    /// heap.intern_symbol("foo");
    /// heap.intern_symbol("bar");
    /// heap.intern_symbol("foo"); // Should not create new symbol
    /// assert_eq!(heap.symbol_table_stats(), 2);
    /// ```
    pub fn symbol_table_stats(&self) -> usize {
        self.symbol_table.len()
    }
}

/// Convenience constructors for building common Scheme values.

/// Create a new integer value on the heap.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_int};
///
/// let mut heap = GcHeap::new();
/// let x = new_int(&mut heap, BigInt::from(42));
/// assert_eq!(x.borrow().value, SchemeValue::Int(42));
/// ```
pub fn new_int(heap: &mut GcHeap, val: BigInt) -> GcRef {
    heap.alloc(SchemeValue::Int(val))
}

/// Create a new floating-point value on the heap.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_float};
///
/// let mut heap = GcHeap::new();
/// let x = new_float(&mut heap, 3.14);
/// assert_eq!(x.borrow().value, SchemeValue::Float(3.14));
/// ```
pub fn new_float(heap: &mut GcHeap, val: f64) -> GcRef {
    heap.alloc(SchemeValue::Float(val))
}

/// Create a new symbol on the heap.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_symbol};
///
/// let mut heap = GcHeap::new();
/// let sym = new_symbol(&mut heap, "lambda");
/// assert_eq!(sym.borrow().value, SchemeValue::Symbol("lambda".to_string()));
/// ```
pub fn new_symbol<S: Into<String>>(heap: &mut GcHeap, name: S) -> GcRef {
    heap.alloc(SchemeValue::Symbol(name.into()))
}

/// Create a new pair (cons cell) on the heap.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_int, new_symbol, new_pair};
///
/// let mut heap = GcHeap::new();
/// let car = new_int(&mut heap, 1);
/// let cdr = new_symbol(&mut heap, "foo");
/// let pair = new_pair(&mut heap, car, cdr);
/// ```
pub fn new_pair(heap: &mut GcHeap, car: GcRef, cdr: GcRef) -> GcRef {
    heap.alloc(SchemeValue::Pair(car, cdr))
}

/// Create a new string value on the heap.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_string};
///
/// let mut heap = GcHeap::new();
/// let s = new_string(&mut heap, "hello");
/// assert_eq!(s.borrow().value, SchemeValue::Str("hello".to_string()));
/// ```
pub fn new_string<S: Into<String>>(heap: &mut GcHeap, val: S) -> GcRef {
    heap.alloc(SchemeValue::Str(val.into()))
}

/// Create a new vector on the heap.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_vector, new_int};
///
/// let mut heap = GcHeap::new();
/// let elements = vec![new_int(&mut heap, 1), new_int(&mut heap, 2)];
/// let vec = new_vector(&mut heap, elements);
/// ```
pub fn new_vector(heap: &mut GcHeap, elements: Vec<GcRef>) -> GcRef {
    heap.alloc(SchemeValue::Vector(elements))
}

/// Create a new closure on the heap.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_closure, new_symbol, new_nil};
///
/// let mut heap = GcHeap::new();
/// let params = vec![new_symbol(&mut heap, "x"), new_symbol(&mut heap, "y")];
/// let body = new_symbol(&mut heap, "body");
/// let env = new_nil(&mut heap);
/// let closure = new_closure(&mut heap, params, body, env);
/// ```
pub fn new_closure(heap: &mut GcHeap, params: Vec<GcRef>, body: GcRef, env: GcRef) -> GcRef {
    heap.alloc(SchemeValue::Closure { params, body, env })
}

/// Create a new boolean value on the heap.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_bool};
///
/// let mut heap = GcHeap::new();
/// let t = new_bool(&mut heap, true);
/// let f = new_bool(&mut heap, false);
/// ```
pub fn new_bool(heap: &mut GcHeap, val: bool) -> GcRef {
    heap.alloc(SchemeValue::Bool(val))
}

/// Create a new character value on the heap.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_char};
///
/// let mut heap = GcHeap::new();
/// let c = new_char(&mut heap, 'x');
/// assert_eq!(c.borrow().value, SchemeValue::Char('x'));
/// ```
pub fn new_char(heap: &mut GcHeap, val: char) -> GcRef {
    heap.alloc(SchemeValue::Char(val))
}

/// Create a new primitive procedure on the heap.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_primitive};
/// use std::rc::Rc;
///
/// let mut heap = GcHeap::new();
/// let add = new_primitive(&mut heap, Rc::new(|_, args| {
///     // Implementation would go here
///     Ok(new_int(&mut heap, 0))
/// }), "Add two numbers".to_string());
/// ```
pub fn new_primitive(
    heap: &mut GcHeap,
    f: Rc<dyn Fn(&mut GcHeap, &[GcRef]) -> Result<GcRef, String>>,
    doc: String,
    is_special_form: bool,
) -> GcRef {
    heap.alloc(SchemeValue::Primitive { func: f, doc, is_special_form })
}

// pub fn new_special_form(
//     heap: &mut GcHeap,
//     f: Rc<dyn Fn(&mut crate::eval::Evaluator, &[GcRef]) -> Result<GcRef, String>>,
//     doc: String,
// ) -> GcRef {
//     heap.alloc(SchemeValue::SpecialForm { func: f, doc })
// }

// pub fn new_special_form_with_evaluator(
//     heap: &mut GcHeap,
//     f: Rc<dyn Fn(&mut crate::eval::Evaluator, &[GcRef]) -> Result<GcRef, String> + 'static>,
//     doc: String,
// ) -> GcRef {
//     heap.alloc(SchemeValue::SpecialForm { func: f, doc })
// }

/// Create a new environment frame on the heap.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_env_frame, new_int};
/// use std::collections::HashMap;
///
/// let mut heap = GcHeap::new();
/// let mut bindings = HashMap::new();
/// bindings.insert("x".to_string(), new_int(&mut heap, 42));
/// let env = new_env_frame(&mut heap, bindings);
/// ```
pub fn new_env_frame(heap: &mut GcHeap, map: HashMap<String, GcRef>) -> GcRef {
    heap.alloc(SchemeValue::EnvFrame(map))
}

/// Create a new nil (empty list) value on the heap.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_nil};
///
/// let mut heap = GcHeap::new();
/// let nil = new_nil(&mut heap);
/// assert!(is_nil(&nil));
/// ```
pub fn new_nil(heap: &mut GcHeap) -> GcRef {
    heap.nil()
}

// ============================================================================
// NEW REFERENCE-BASED ALLOCATION FUNCTIONS (Migration Path)
// ============================================================================
// These functions return GcRefSimple (&'static GcObjectSimple) instead of GcRef (Rc<RefCell<GcObject>>)
// for better performance and simpler code.

/// Allocate a new integer on the heap using the reference-based system.
pub fn new_int_simple(heap: &mut GcHeap, val: BigInt) -> GcRefSimple {
    let obj = GcObjectSimple { value: SchemeValueSimple::Int(val), marked: false };
    heap.alloc_simple(obj)
}

/// Allocate a new float on the heap using the reference-based system.
pub fn new_float_simple(heap: &mut GcHeap, val: f64) -> GcRefSimple {
    let obj = GcObjectSimple { value: SchemeValueSimple::Float(val), marked: false };
    heap.alloc_simple(obj)
}

/// Allocate a new boolean on the heap using the reference-based system.
pub fn new_bool_simple(heap: &mut GcHeap, val: bool) -> GcRefSimple {
    let obj = GcObjectSimple { value: SchemeValueSimple::Bool(val), marked: false };
    heap.alloc_simple(obj)
}

/// Allocate a new character on the heap using the reference-based system.
pub fn new_char_simple(heap: &mut GcHeap, val: char) -> GcRefSimple {
    let obj = GcObjectSimple { value: SchemeValueSimple::Char(val), marked: false };
    heap.alloc_simple(obj)
}

/// Allocate a new symbol on the heap using the reference-based system.
pub fn new_symbol_simple(heap: &mut GcHeap, name: &str) -> GcRefSimple {
    let obj = GcObjectSimple { value: SchemeValueSimple::Symbol(name.to_string()), marked: false };
    heap.alloc_simple(obj)
}

/// Allocate a new string on the heap using the reference-based system.
pub fn new_string_simple(heap: &mut GcHeap, s: &str) -> GcRefSimple {
    let obj = GcObjectSimple { value: SchemeValueSimple::Str(s.to_string()), marked: false };
    heap.alloc_simple(obj)
}

/// Get the nil value using the reference-based system.
pub fn new_nil_simple(heap: &mut GcHeap) -> GcRefSimple {
    heap.nil_simple()
}

/// Create a new pair using GcRefSimple references.
pub fn new_pair_simple(heap: &mut GcHeap, car: GcRefSimple, cdr: GcRefSimple) -> GcRefSimple {
    let obj = GcObjectSimple { 
        value: SchemeValueSimple::Pair(car, cdr), 
        marked: false 
    };
    heap.alloc_simple(obj)
}

/// Create a new vector using GcRefSimple references.
pub fn new_vector_simple(heap: &mut GcHeap, elements: Vec<GcRefSimple>) -> GcRefSimple {
    let obj = GcObjectSimple { 
        value: SchemeValueSimple::Vector(elements), 
        marked: false 
    };
    heap.alloc_simple(obj)
}

/// Create a new primitive function using GcRefSimple references.
pub fn new_primitive_simple(
    heap: &mut GcHeap,
    f: Rc<dyn Fn(&mut GcHeap, &[GcRefSimple]) -> Result<GcRefSimple, String>>,
    doc: String,
    is_special_form: bool,
) -> GcRefSimple {
    let obj = GcObjectSimple { 
        value: SchemeValueSimple::Primitive { func: f, doc, is_special_form }, 
        marked: false 
    };
    heap.alloc_simple(obj)
}

/// Create a new closure (lambda function) using GcRefSimple references.
pub fn new_closure_simple(
    heap: &mut GcHeap,
    params: Vec<GcRefSimple>,
    body: GcRefSimple,
    env: Rc<RefCell<crate::env::Frame>>,
) -> GcRefSimple {
    let obj = GcObjectSimple { 
        value: SchemeValueSimple::Closure { params, body, env }, 
        marked: false 
    };
    heap.alloc_simple(obj)
}

// ============================================================================
// CONVERSION FUNCTIONS (Temporary - for migration period)
// ============================================================================

// These functions are being removed as we migrate to using the new system directly.
// The parser should use GcRefSimple functions directly without conversion.

/// Accessor helpers (returns some inner data or None):

/// Extract an integer value from a GcRef, if it contains one.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_int, as_int};
///
/// let mut heap = GcHeap::new();
/// let obj = new_int(&mut heap, 42);
/// assert_eq!(as_int(&obj), Some(42));
/// 
/// let sym = new_symbol(&mut heap, "not-a-number");
/// assert_eq!(as_int(&sym), None);
/// ```
pub fn as_int(obj: &GcRef) -> Option<i64> {
    match &obj.borrow().value {
        SchemeValue::Int(n) => Some(n.to_i64().unwrap()),
        _ => None,
    }
}

/// Extract a floating-point value from a GcRef, if it contains one.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_float, as_float};
///
/// let mut heap = GcHeap::new();
/// let obj = new_float(&mut heap, 3.14);
/// assert_eq!(as_float(&obj), Some(3.14));
/// ```
pub fn as_float(obj: &GcRef) -> Option<f64> {
    match &obj.borrow().value {
        SchemeValue::Float(f) => Some(*f),
        _ => None,
    }
}

/// Extract a symbol name from a GcRef, if it contains a symbol.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_symbol, as_symbol};
///
/// let mut heap = GcHeap::new();
/// let obj = new_symbol(&mut heap, "lambda");
/// assert_eq!(as_symbol(&obj), Some("lambda".to_string()));
/// ```
pub fn as_symbol(obj: &GcRef) -> Option<String> {
    match &obj.borrow().value {
        SchemeValue::Symbol(s) => Some(s.clone()),
        _ => None,
    }
}

/// Extract a string value from a GcRef, if it contains one.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_string, as_string};
///
/// let mut heap = GcHeap::new();
/// let obj = new_string(&mut heap, "hello");
/// assert_eq!(as_string(&obj), Some("hello".to_string()));
/// ```
pub fn as_string(obj: &GcRef) -> Option<String> {
    match &obj.borrow().value {
        SchemeValue::Str(s) => Some(s.clone()),
        _ => None,
    }
}

/// Extract a vector from a GcRef, if it contains one.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_vector, new_int, as_vector};
///
/// let mut heap = GcHeap::new();
/// let elements = vec![new_int(&mut heap, 1), new_int(&mut heap, 2)];
/// let obj = new_vector(&mut heap, elements.clone());
/// assert_eq!(as_vector(&obj), Some(elements));
/// ```
pub fn as_vector(obj: &GcRef) -> Option<Vec<GcRef>> {
    match &obj.borrow().value {
        SchemeValue::Vector(v) => Some(v.clone()),
        _ => None,
    }
}

/// Extract closure components from a GcRef, if it contains a closure.
///
/// Returns a tuple of (parameters, body, environment).
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_closure, new_symbol, new_nil, as_closure};
///
/// let mut heap = GcHeap::new();
/// let params = vec![new_symbol(&mut heap, "x")];
/// let body = new_symbol(&mut heap, "body");
/// let env = new_nil(&mut heap);
/// let closure = new_closure(&mut heap, params.clone(), body.clone(), env.clone());
/// 
/// let (p, b, e) = as_closure(&closure).unwrap();
/// assert_eq!(p.len(), params.len());
/// ```
pub fn as_closure(obj: &GcRef) -> Option<(Vec<GcRef>, GcRef, GcRef)> {
    match &obj.borrow().value {
        SchemeValue::Closure { params, body, env } => Some((params.clone(), body.clone(), env.clone())),
        _ => None,
    }
}

/// Extract a boolean value from a GcRef, if it contains one.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_bool, as_bool};
///
/// let mut heap = GcHeap::new();
/// let obj = new_bool(&mut heap, true);
/// assert_eq!(as_bool(&obj), Some(true));
/// ```
pub fn as_bool(obj: &GcRef) -> Option<bool> {
    match &obj.borrow().value {
        SchemeValue::Bool(b) => Some(*b),
        _ => None,
    }
}

/// Extract a character value from a GcRef, if it contains one.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_char, as_char};
///
/// let mut heap = GcHeap::new();
/// let obj = new_char(&mut heap, 'x');
/// assert_eq!(as_char(&obj), Some('x'));
/// ```
pub fn as_char(obj: &GcRef) -> Option<char> {
    match &obj.borrow().value {
        SchemeValue::Char(c) => Some(*c),
        _ => None,
    }
}

/// Check if a GcRef contains a primitive function.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_primitive, is_primitive};
/// use std::rc::Rc;
///
/// let mut heap = GcHeap::new();
/// let prim = new_primitive(&mut heap, Rc::new(|_, _| Err("not implemented".to_string())), "Add two numbers".to_string(), false);
/// assert!(is_primitive(&prim));
/// ```
pub fn is_primitive(obj: &GcRef) -> bool {
    matches!(&obj.borrow().value, SchemeValue::Primitive { .. })
}

// pub fn is_special_form(obj: &GcRef) -> bool {
//     matches!(&obj.borrow().value, SchemeValue::SpecialForm { .. })
// }

/// Extract an environment frame from a GcRef, if it contains one.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_env_frame, as_env_frame};
/// use std::collections::HashMap;
///
/// let mut heap = GcHeap::new();
/// let mut bindings = HashMap::new();
/// bindings.insert("x".to_string(), new_int(&mut heap, 42));
/// let env = new_env_frame(&mut heap, bindings.clone());
/// assert_eq!(as_env_frame(&env), Some(bindings));
/// ```
pub fn as_env_frame(obj: &GcRef) -> Option<HashMap<String, GcRef>> {
    match &obj.borrow().value {
        SchemeValue::EnvFrame(map) => Some(map.clone()),
        _ => None,
    }
}

/// Look up a variable in an environment frame.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_env_frame, env_frame_lookup, new_int};
/// use std::collections::HashMap;
///
/// let mut heap = GcHeap::new();
/// let mut bindings = HashMap::new();
/// bindings.insert("x".to_string(), new_int(&mut heap, 42));
/// let env = new_env_frame(&mut heap, bindings);
/// 
/// let value = env_frame_lookup(&env, "x").unwrap();
/// assert_eq!(as_int(&value), Some(42));
/// ```
pub fn env_frame_lookup(obj: &GcRef, key: &str) -> Option<GcRef> {
    match &obj.borrow().value {
        SchemeValue::EnvFrame(map) => map.get(key).cloned(),
        _ => None,
    }
}

/// Insert a new binding into an environment frame.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_env_frame, env_frame_insert, new_int};
/// use std::collections::HashMap;
///
/// let mut heap = GcHeap::new();
/// let env = new_env_frame(&mut heap, HashMap::new());
/// env_frame_insert(&env, "x".to_string(), new_int(&mut heap, 42));
/// 
/// let value = env_frame_lookup(&env, "x").unwrap();
/// assert_eq!(as_int(&value), Some(42));
/// ```
pub fn env_frame_insert(obj: &GcRef, key: String, value: GcRef) {
    if let SchemeValue::EnvFrame(map) = &mut obj.borrow_mut().value {
        map.insert(key, value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_allocation_and_retrieval() {
        let mut heap = GcHeap::new();
        let int_val = new_int(&mut heap, BigInt::from(42));
        let float_val = new_float(&mut heap, 3.14);
        let sym_val = new_symbol(&mut heap, "lambda");

        assert_eq!(int_val.borrow().value, SchemeValue::Int(BigInt::from(42)));
        assert_eq!(float_val.borrow().value, SchemeValue::Float(3.14));
        assert_eq!(as_symbol(&sym_val).unwrap(), "lambda");
    }

    #[test]
    fn test_root_management() {
        let mut heap = GcHeap::new();
        let obj = new_int(&mut heap, BigInt::from(10));
        assert_eq!(heap.root_count(), 0);

        heap.add_root(obj.clone());
        assert_eq!(heap.root_count(), 1);

        heap.remove_root(&obj);
        assert_eq!(heap.root_count(), 0);
    }

    #[test]
    fn test_garbage_collection() {
        let mut heap = GcHeap::new();
        let obj1 = new_int(&mut heap, BigInt::from(10));
        let obj2 = new_int(&mut heap, BigInt::from(20));

        heap.add_root(obj1.clone());
        heap.collect_garbage();
        assert!(Rc::strong_count(&obj1) > 1); // Should still exist due to root
        assert_eq!(Rc::strong_count(&obj2), 1); // Should be cleaned up, no roots

        heap.remove_root(&obj1);
        heap.collect_garbage();
        assert_eq!(Rc::strong_count(&obj1), 1); // Should be cleaned up after removal from roots
    }

    #[test]
    fn test_string_and_vector_and_closure() {
        let mut heap = GcHeap::new();
        // String
        let s = new_string(&mut heap, "hello");
        assert_eq!(as_string(&s).unwrap(), "hello");

        // Vector
        let v1 = new_int(&mut heap, BigInt::from(1));
        let v2 = new_int(&mut heap, BigInt::from(2));
        let v = new_vector(&mut heap, vec![v1.clone(), v2.clone()]);
        let vec_contents = as_vector(&v).unwrap();
        assert_eq!(vec_contents.len(), 2);
        assert_eq!(as_int(&vec_contents[0]).unwrap(), 1);
        assert_eq!(as_int(&vec_contents[1]).unwrap(), 2);

        // Closure
        let params = vec![new_symbol(&mut heap, "x"), new_symbol(&mut heap, "y")];
        let body = new_symbol(&mut heap, "body");
        // Use the singleton nil object for env
        let env = heap.nil();
        let clo = new_closure(&mut heap, params.clone(), body.clone(), env.clone());
        let (p, b, e) = as_closure(&clo).unwrap();
        assert_eq!(p.len(), params.len());
        assert_eq!(b.borrow().value, body.borrow().value);
        assert_eq!(e.borrow().value, env.borrow().value);

        // GC: vector and closure with roots
        heap.add_root(v.clone());
        heap.add_root(clo.clone());
        heap.collect_garbage();
        assert_eq!(heap.heap_size(), 6); // v, v1, v2, clo, body, env

        // Remove roots and collect
        heap.remove_root(&v);
        heap.remove_root(&clo);
        heap.collect_garbage();
        assert_eq!(heap.heap_size(), 0); // all objects collected
    }

    #[test]
    fn test_bool_char_primitive() {
        use std::rc::Rc;
        let mut heap = GcHeap::new();
        // Bool
        let t = new_bool(&mut heap, true);
        let f = new_bool(&mut heap, false);
        assert_eq!(as_bool(&t), Some(true));
        assert_eq!(as_bool(&f), Some(false));

        // Char
        let c = new_char(&mut heap, 'x');
        assert_eq!(as_char(&c), Some('x'));

        // Primitive
        let prim = new_primitive(&mut heap, Rc::new(|_, _| Err("not implemented".to_string())), "Add two numbers".to_string(), false);
        assert!(is_primitive(&prim));
    }

    #[test]
    fn test_env_frame() {
        let mut heap = GcHeap::new();
        let mut map = HashMap::new();
        let v1 = new_int(&mut heap, BigInt::from(42));
        let v2 = new_symbol(&mut heap, "foo");
        map.insert("x".to_string(), v1.clone());
        map.insert("y".to_string(), v2.clone());
        let env = new_env_frame(&mut heap, map);
        assert_eq!(env_frame_lookup(&env, "x").unwrap().borrow().value, SchemeValue::Int(BigInt::from(42)));
        assert_eq!(env_frame_lookup(&env, "y").unwrap().borrow().value, SchemeValue::Symbol("foo".to_string()));

        // Insert new binding
        let v3 = new_float(&mut heap, 3.14);
        env_frame_insert(&env, "z".to_string(), v3.clone());
        assert_eq!(env_frame_lookup(&env, "z").unwrap().borrow().value, SchemeValue::Float(3.14));
    }

    #[test]
    fn test_pair() {
        let mut heap = GcHeap::new();
        let car = new_int(&mut heap, BigInt::from(1));
        let cdr = new_symbol(&mut heap, "foo");
        let pair = new_pair(&mut heap, car.clone(), cdr.clone());

        let (a, d) = as_pair(&pair).unwrap();
        assert_eq!(a.borrow().value, car.borrow().value);
        assert_eq!(d.borrow().value, cdr.borrow().value);
    }

    #[test]
    fn test_is_nil() {
        let mut heap = GcHeap::new();
        // Use the singleton nil object
        let nil = heap.nil();
        assert!(is_nil(&nil));

        let not_nil = new_int(&mut heap, BigInt::from(42));
        assert!(!is_nil(&not_nil));
    }

    #[test]
    fn test_free_list_allocation() {
        let mut heap = GcHeap::new();
        
        // Check initial stats
        let (allocated, free) = heap.simple_stats();
        assert_eq!(allocated, 3); // nil, true, false singletons
        assert_eq!(free, 997); // 1000 pre-allocated - 3 used for singletons
        
        // Allocate some objects
        let obj1 = heap.alloc_simple(GcObjectSimple {
            value: SchemeValueSimple::Int(BigInt::from(42)),
            marked: false,
        });
        let obj2 = heap.alloc_simple(GcObjectSimple {
            value: SchemeValueSimple::Str("hello".to_string()),
            marked: false,
        });
        
        // Check stats after allocation
        let (allocated, free) = heap.simple_stats();
        assert_eq!(allocated, 5); // 3 singletons + 2 new objects
        assert_eq!(free, 995); // 997 - 2 used objects
        
        // Verify the objects work correctly
        match &obj1.value {
            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "42"),
            _ => panic!("Expected integer"),
        }
        match &obj2.value {
            SchemeValueSimple::Str(s) => assert_eq!(s, "hello"),
            _ => panic!("Expected string"),
        }
    }

    #[test]
    fn test_singleton_nil_true_false() {
        let mut heap = GcHeap::new();
        
        // Test that singletons are the same objects
        let nil1 = heap.nil_simple();
        let nil2 = heap.nil_simple();
        assert!(std::ptr::eq(nil1, nil2));
        
        let true1 = heap.true_simple();
        let true2 = heap.true_simple();
        assert!(std::ptr::eq(true1, true2));
        
        let false1 = heap.false_simple();
        let false2 = heap.false_simple();
        assert!(std::ptr::eq(false1, false2));
        
        // Test that they're different from each other
        assert!(!std::ptr::eq(nil1, true1));
        assert!(!std::ptr::eq(nil1, false1));
        assert!(!std::ptr::eq(true1, false1));
        
        // Test their values
        match &nil1.value {
            SchemeValueSimple::Nil => {},
            _ => panic!("nil should be Nil"),
        }
        match &true1.value {
            SchemeValueSimple::Bool(true) => {},
            _ => panic!("true should be Bool(true)"),
        }
        match &false1.value {
            SchemeValueSimple::Bool(false) => {},
            _ => panic!("false should be Bool(false)"),
        }
    }

    #[test]
    fn test_symbol_interning() {
        let mut heap = GcHeap::new();
        
        // Test that same name returns same object
        let sym1 = heap.intern_symbol("foo");
        let sym2 = heap.intern_symbol("foo");
        assert!(std::ptr::eq(sym1, sym2), "Same symbol name should return same object");
        
        // Test that different names return different objects
        let sym3 = heap.intern_symbol("bar");
        assert!(!std::ptr::eq(sym1, sym3), "Different symbol names should return different objects");
        
        // Test symbol table statistics
        assert_eq!(heap.symbol_table_stats(), 2, "Should have 2 unique symbols");
        
        // Test that re-interned symbols don't increase the count
        let sym4 = heap.intern_symbol("foo");
        assert!(std::ptr::eq(sym1, sym4), "Re-interned symbol should be same object");
        assert_eq!(heap.symbol_table_stats(), 2, "Count should not increase for existing symbols");
        
        // Test symbol values
        match &sym1.value {
            SchemeValueSimple::Symbol(name) => assert_eq!(name, "foo"),
            _ => panic!("Should be a symbol"),
        }
        match &sym3.value {
            SchemeValueSimple::Symbol(name) => assert_eq!(name, "bar"),
            _ => panic!("Should be a symbol"),
        }
    }
}

/// Extract a pair (car and cdr) from a GcRef, if it contains a pair.
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_pair, new_int, new_symbol, as_pair};
///
/// let mut heap = GcHeap::new();
/// let car = new_int(&mut heap, BigInt::from(1));
/// let cdr = new_symbol(&mut heap, "foo");
/// let pair = new_pair(&mut heap, car.clone(), cdr.clone());
/// 
/// let (a, d) = as_pair(&pair).unwrap();
/// assert_eq!(a.borrow().value, car.borrow().value);
/// assert_eq!(d.borrow().value, cdr.borrow().value);
/// ```
pub fn as_pair(obj: &GcRef) -> Option<(GcRef, GcRef)> {
    match &obj.borrow().value {
        SchemeValue::Pair(car, cdr) => Some((car.clone(), cdr.clone())),
        _ => None,
    }
}

/// Check if a GcRef contains nil (empty list).
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_nil, is_nil};
///
/// let mut heap = GcHeap::new();
/// let nil = new_nil(&mut heap);
/// assert!(is_nil(&nil));
/// 
/// let not_nil = new_int(&mut heap, BigInt::from(42));
/// assert!(!is_nil(&not_nil));
/// ```
pub fn is_nil(obj: &GcRef) -> bool {
    matches!(&obj.borrow().value, SchemeValue::Nil)
}