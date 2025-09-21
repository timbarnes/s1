pub mod heap;
pub mod objects;

use crate::eval::{CEKState, KontRef, RunTime};
use crate::io::PortKind;
use num_bigint::BigInt;
use std::cell::RefCell;
use std::rc::Rc;

pub use heap::GcHeap;
pub use objects::*;

#[macro_export]
macro_rules! gc_value {
    ($r:expr) => {{
        // SAFETY: caller must ensure $r is a valid GcRef pointing to live data
        unsafe { &(*$r).value }
    }};
}

#[macro_export]
macro_rules! gc_marked {
    ($r:expr) => {{
        // SAFETY: caller must ensure $r is a valid GcRef pointing to live data
        unsafe { &(*$r).marked }
    }};
}

#[macro_export]
macro_rules! gc_marked_mut {
    ($r:expr) => {{
        // SAFETY: caller must ensure $r is a valid GcRef pointing to live data
        unsafe { &mut (*$r).marked }
    }};
}

// core types
pub type GcRef = *mut GcObject;

pub struct GcObject {
    pub value: SchemeValue,
    pub marked: bool,
}

pub enum Callable {
    // Standard library / core functions
    Builtin {
        func: fn(&mut GcHeap, &[GcRef]) -> Result<GcRef, String>,
        doc: String,
    },
    // Privileged system procedures with access to the evaluator
    SysBuiltin {
        func: fn(&mut RunTime, &[GcRef], &mut CEKState) -> Result<(), String>,
        doc: String,
    },
    // Syntax procedures called with unevaluated arguments
    SpecialForm {
        func: fn(GcRef, &mut RunTime, &mut CEKState) -> Result<(), String>,
        doc: String,
    },
    // Scheme-implemented procedures
    Closure {
        params: Vec<GcRef>,
        body: GcRef,
        env: Rc<RefCell<crate::env::Frame>>,
    },
    // Scheme-implemented macros
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
    Continuation(KontRef),
    Eof,
    Void,
    Undefined,
}

pub trait Mark {
    fn mark(&self, visit: &mut dyn FnMut(GcRef));
}

pub fn mark(gcref: GcRef) {
    unsafe {
        (*gcref).marked = true;
    }
}

pub fn unmark(gcref: GcRef) {
    unsafe {
        (*gcref).marked = false;
    }
}
