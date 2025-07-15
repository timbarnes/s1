//! Mark-and-sweep memory management for Scheme values.
//!
//! Provides:
//!   - SchemeValue enum (int, float, symbol, pair, nil)
//!   - GcHeap: the heap with allocation, roots, and manual collection
//!   - Public API for root management and manual GC trigger

use std::cell::RefCell;

use std::rc::Rc;

use std::cmp::PartialEq;

use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum PortKind {
    Stdin,
    Stdout,
    File { name: String, write: bool },
    StringPort(String, usize),
}

#[derive(Clone, Debug)]
pub struct Port {
    pub kind: PortKind,
    pub file_id: Option<usize>, // None for non-file ports
    // In a real implementation, you might add file handles, buffers, etc.
}

/// The core type representing a Scheme value in the heap.
pub enum SchemeValue {
    Int(i64),
    Float(f64),
    Symbol(String),
    Pair(GcRef, GcRef),
    Str(String),
    Vector(Vec<GcRef>),
    Closure {
        params: Vec<String>,
        body: GcRef,
        env: GcRef,
    },
    Bool(bool),
    Char(char),
    Primitive(Rc<dyn Fn(&mut GcHeap, &[GcRef]) -> Result<GcRef, String>>),
    Port(Port),
    EnvFrame(HashMap<String, GcRef>),
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
            SchemeValue::Primitive(_) => write!(f, "Primitive(<builtin>)"),
            SchemeValue::Port(port) => f.debug_tuple("Port").field(port).finish(),
            SchemeValue::EnvFrame(map) => f.debug_map().entries(map.iter()).finish(),
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
                p1 == p2 && b1.borrow().value == b2.borrow().value && e1.borrow().value == e2.borrow().value
            }
            (SchemeValue::Bool(a), SchemeValue::Bool(b)) => a == b,
            (SchemeValue::Char(a), SchemeValue::Char(b)) => a == b,
            // For Primitive and Port, just compare type (not function pointer or port identity)
            (SchemeValue::Primitive(_), SchemeValue::Primitive(_)) => true,
            (SchemeValue::Port(a), SchemeValue::Port(b)) => {
                a.kind == b.kind && a.file_id == b.file_id
            }
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
pub type GcRef = Rc<RefCell<GcObject>>;

/// Wrapper for heap storage; includes a mark bit for GC.
#[derive(Debug)]
pub struct GcObject {
    pub value: SchemeValue,
    marked: bool,
}

/// The garbage collected heap, with root management APIs.
pub struct GcHeap {
    objects: Vec<GcRef>,
    roots: Vec<GcRef>,
}

impl GcHeap {
    /// Create a new (empty) heap.
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
            roots: Vec::new(),
        }
    }

    /// Allocate a new SchemeValue on the heap and return a reference.
    pub fn alloc(&mut self, value: SchemeValue) -> GcRef {
        let obj = Rc::new(RefCell::new(GcObject {
            value,
            marked: false,
        }));
        self.objects.push(obj.clone());
        obj
    }

    /// Add a GcRef to the root set (keeps object alive across GC).
    pub fn add_root(&mut self, obj: GcRef) {
        self.roots.push(obj);
    }

    /// Remove a GcRef from the root set.
    pub fn remove_root(&mut self, obj: &GcRef) {
        self.roots.retain(|r| !Rc::ptr_eq(r, obj));
    }

    /// Perform mark-and-sweep garbage collection.
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
            // Add more types (e.g., vectors, closures) if they store GcRefs.
            _ => {}
        }
    }

    /// Return the total number of tracked heap objects (for stats/testing).
    pub fn heap_size(&self) -> usize {
        self.objects.len()
    }

    /// Return the number of root references (for stats/testing).
    pub fn root_count(&self) -> usize {
        self.roots.len()
    }
}

/// Convenience constructors for building common Scheme values.

pub fn new_int(heap: &mut GcHeap, val: i64) -> GcRef {
    heap.alloc(SchemeValue::Int(val))
}

pub fn new_float(heap: &mut GcHeap, val: f64) -> GcRef {
    heap.alloc(SchemeValue::Float(val))
}

pub fn new_symbol<S: Into<String>>(heap: &mut GcHeap, name: S) -> GcRef {
    heap.alloc(SchemeValue::Symbol(name.into()))
}

pub fn new_pair(heap: &mut GcHeap, car: GcRef, cdr: GcRef) -> GcRef {
    heap.alloc(SchemeValue::Pair(car, cdr))
}

pub fn new_nil(heap: &mut GcHeap) -> GcRef {
    heap.alloc(SchemeValue::Nil)
}

pub fn new_string<S: Into<String>>(heap: &mut GcHeap, val: S) -> GcRef {
    heap.alloc(SchemeValue::Str(val.into()))
}

pub fn new_vector(heap: &mut GcHeap, elements: Vec<GcRef>) -> GcRef {
    heap.alloc(SchemeValue::Vector(elements))
}

pub fn new_closure(heap: &mut GcHeap, params: Vec<String>, body: GcRef, env: GcRef) -> GcRef {
    heap.alloc(SchemeValue::Closure { params, body, env })
}

pub fn new_bool(heap: &mut GcHeap, val: bool) -> GcRef {
    heap.alloc(SchemeValue::Bool(val))
}

pub fn new_char(heap: &mut GcHeap, val: char) -> GcRef {
    heap.alloc(SchemeValue::Char(val))
}

pub fn new_primitive(heap: &mut GcHeap, f: Rc<dyn Fn(&mut GcHeap, &[GcRef]) -> Result<GcRef, String>>) -> GcRef {
    heap.alloc(SchemeValue::Primitive(f))
}

pub fn new_port(heap: &mut GcHeap, kind: PortKind, file_id: Option<usize>) -> GcRef {
    heap.alloc(SchemeValue::Port(Port { kind, file_id }))
}

pub fn new_env_frame(heap: &mut GcHeap, map: HashMap<String, GcRef>) -> GcRef {
    heap.alloc(SchemeValue::EnvFrame(map))
}

pub fn as_env_frame(obj: &GcRef) -> Option<HashMap<String, GcRef>> {
    match &obj.borrow().value {
        SchemeValue::EnvFrame(map) => Some(map.clone()),
        _ => None,
    }
}

pub fn env_frame_lookup(obj: &GcRef, key: &str) -> Option<GcRef> {
    match &obj.borrow().value {
        SchemeValue::EnvFrame(map) => map.get(key).cloned(),
        _ => None,
    }
}

pub fn env_frame_insert(obj: &GcRef, key: String, value: GcRef) {
    if let SchemeValue::EnvFrame(map) = &mut obj.borrow_mut().value {
        map.insert(key, value);
    }
}

/// Accessor helpers (returns some inner data or None):

pub fn as_int(obj: &GcRef) -> Option<i64> {
    match &obj.borrow().value {
        SchemeValue::Int(n) => Some(*n),
        _ => None,
    }
}

pub fn as_float(obj: &GcRef) -> Option<f64> {
    match &obj.borrow().value {
        SchemeValue::Float(f) => Some(*f),
        _ => None,
    }
}

pub fn as_symbol(obj: &GcRef) -> Option<String> {
    match &obj.borrow().value {
        SchemeValue::Symbol(s) => Some(s.clone()),
        _ => None,
    }
}

pub fn as_string(obj: &GcRef) -> Option<String> {
    match &obj.borrow().value {
        SchemeValue::Str(s) => Some(s.clone()),
        _ => None,
    }
}

pub fn as_vector(obj: &GcRef) -> Option<Vec<GcRef>> {
    match &obj.borrow().value {
        SchemeValue::Vector(v) => Some(v.clone()),
        _ => None,
    }
}

pub fn as_closure(obj: &GcRef) -> Option<(Vec<String>, GcRef, GcRef)> {
    match &obj.borrow().value {
        SchemeValue::Closure { params, body, env } => Some((params.clone(), body.clone(), env.clone())),
        _ => None,
    }
}

pub fn as_bool(obj: &GcRef) -> Option<bool> {
    match &obj.borrow().value {
        SchemeValue::Bool(b) => Some(*b),
        _ => None,
    }
}

pub fn as_char(obj: &GcRef) -> Option<char> {
    match &obj.borrow().value {
        SchemeValue::Char(c) => Some(*c),
        _ => None,
    }
}

pub fn is_primitive(obj: &GcRef) -> bool {
    matches!(&obj.borrow().value, SchemeValue::Primitive(_))
}

pub fn is_port(obj: &GcRef) -> bool {
    matches!(&obj.borrow().value, SchemeValue::Port(_))
}

pub fn as_port(obj: &GcRef) -> Option<PortKind> {
    match &obj.borrow().value {
        SchemeValue::Port(port) => Some(port.kind.clone()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_allocation_and_retrieval() {
        let mut heap = GcHeap::new();
        let int_val = new_int(&mut heap, 42);
        let float_val = new_float(&mut heap, 3.14);
        let sym_val = new_symbol(&mut heap, "lambda");

        assert_eq!(int_val.borrow().value, SchemeValue::Int(42));
        assert_eq!(float_val.borrow().value, SchemeValue::Float(3.14));
        assert_eq!(as_symbol(&sym_val).unwrap(), "lambda");
    }

    #[test]
    fn test_root_management() {
        let mut heap = GcHeap::new();
        let obj = new_int(&mut heap, 10);
        assert_eq!(heap.root_count(), 0);

        heap.add_root(obj.clone());
        assert_eq!(heap.root_count(), 1);

        heap.remove_root(&obj);
        assert_eq!(heap.root_count(), 0);
    }

    #[test]
    fn test_garbage_collection() {
        let mut heap = GcHeap::new();
        let obj1 = new_int(&mut heap, 10);
        let obj2 = new_int(&mut heap, 20);

        heap.add_root(obj1.clone());
        heap.collect_garbage();
        // obj1 should still be in the heap, obj2 should be collected
        assert_eq!(heap.heap_size(), 1);
        assert_eq!(obj1.borrow().value, SchemeValue::Int(10));

        heap.remove_root(&obj1);
        heap.collect_garbage();
        // Now heap should be empty
        assert_eq!(heap.heap_size(), 0);
    }

    #[test]
    fn test_string_and_vector_and_closure() {
        let mut heap = GcHeap::new();
        // String
        let s = new_string(&mut heap, "hello");
        assert_eq!(as_string(&s).unwrap(), "hello");

        // Vector
        let v1 = new_int(&mut heap, 1);
        let v2 = new_int(&mut heap, 2);
        let v = new_vector(&mut heap, vec![v1.clone(), v2.clone()]);
        let vec_contents = as_vector(&v).unwrap();
        assert_eq!(vec_contents.len(), 2);
        assert_eq!(as_int(&vec_contents[0]).unwrap(), 1);
        assert_eq!(as_int(&vec_contents[1]).unwrap(), 2);

        // Closure
        let params = vec!["x".to_string(), "y".to_string()];
        let body = new_symbol(&mut heap, "body");
        let env = new_nil(&mut heap);
        let clo = new_closure(&mut heap, params.clone(), body.clone(), env.clone());
        let (p, b, e) = as_closure(&clo).unwrap();
        assert_eq!(p, params);
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
    fn test_bool_char_primitive_port() {
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
        let prim = new_primitive(&mut heap, Rc::new(|_, _| Err("not implemented".to_string())));
        assert!(is_primitive(&prim));

        // Port
        let port = new_port(&mut heap, PortKind::Stdin, None);
        assert!(is_port(&port));
    }

    #[test]
    fn test_env_frame() {
        let mut heap = GcHeap::new();
        let mut map = HashMap::new();
        let v1 = new_int(&mut heap, 42);
        let v2 = new_symbol(&mut heap, "foo");
        map.insert("x".to_string(), v1.clone());
        map.insert("y".to_string(), v2.clone());
        let env = new_env_frame(&mut heap, map);
        assert_eq!(env_frame_lookup(&env, "x").unwrap().borrow().value, SchemeValue::Int(42));
        assert_eq!(env_frame_lookup(&env, "y").unwrap().borrow().value, SchemeValue::Symbol("foo".to_string()));

        // Insert new binding
        let v3 = new_float(&mut heap, 3.14);
        env_frame_insert(&env, "z".to_string(), v3.clone());
        assert_eq!(env_frame_lookup(&env, "z").unwrap().borrow().value, SchemeValue::Float(3.14));
    }

    #[test]
    fn test_port_struct() {
        let mut heap = GcHeap::new();
        let p1 = new_port(&mut heap, PortKind::Stdin, None);
        let p2 = new_port(&mut heap, PortKind::Stdout, None);
        let p3 = new_port(&mut heap, PortKind::File { name: "foo.txt".to_string(), write: false }, None);
        let p4 = new_port(&mut heap, PortKind::File { name: "bar.txt".to_string(), write: true }, None);
        assert!(is_port(&p1));
        assert!(is_port(&p2));
        assert!(is_port(&p3));
        assert!(is_port(&p4));
        assert_eq!(as_port(&p1), Some(PortKind::Stdin));
        assert_eq!(as_port(&p2), Some(PortKind::Stdout));
        assert_eq!(as_port(&p3), Some(PortKind::File { name: "foo.txt".to_string(), write: false }));
        assert_eq!(as_port(&p4), Some(PortKind::File { name: "bar.txt".to_string(), write: true }));
    }
}

pub fn as_pair(obj: &GcRef) -> Option<(GcRef, GcRef)> {
    match &obj.borrow().value {
        SchemeValue::Pair(car, cdr) => Some((car.clone(), cdr.clone())),
        _ => None,
    }
}

pub fn is_nil(obj: &GcRef) -> bool {
    matches!(&obj.borrow().value, SchemeValue::Nil)
}
