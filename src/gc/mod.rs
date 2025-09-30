pub mod heap;
pub mod objects;

use crate::eval::{CEKState, DynamicWind, KontRef, RunTime};
use crate::io::PortKind;
pub use heap::GcHeap;
use num_bigint::BigInt;
pub use objects::*;
use std::cell::RefCell;
use std::rc::Rc;

#[macro_export]
macro_rules! register_builtin_family {
    ($heap:expr, $env:expr, $($name:expr => $func:expr),* $(,)?) => {
        $(
            $env.define($heap.intern_symbol($name),
                crate::gc::new_builtin($heap, $func,
                    concat!($name, ": builtin function").to_string()));
        )*
    };
}

#[macro_export]
macro_rules! register_special_form {
    ($rt:expr, $env:expr, $($name:expr => $func:expr),* $(,)?) => {
        $(
            $env.define($rt.intern_symbol($name),
                new_special_form($rt, $func,
                    concat!($name, ": special form").to_string()));
        )*
    };
}

#[macro_export]
macro_rules! register_sys_builtins {
    ($rt:expr, $env:expr, $($name:expr => $func:expr),* $(,)?) => {
        $( $env.define($rt.heap.intern_symbol($name), new_sys_builtin($rt, $func,
            concat!($name, ": sys-builtin").to_string()));
        )*
    };
}

#[macro_export]
macro_rules! gc_value {
    ($r:expr) => {{
        // SAFETY: caller must ensure $r is a valid GcRef pointing to live data
        unsafe { &(*$r).value }
    }};
}

#[macro_export]
macro_rules! gc_value_mut {
    ($r:expr) => {{
        // SAFETY: caller must ensure $r is a valid GcRef pointing to live data
        unsafe { &mut (*$r).value }
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
        func: fn(&mut RunTime, &[GcRef], &mut CEKState, KontRef) -> Result<(), String>,
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
    Continuation(KontRef, Vec<DynamicWind>),
    Eof,
    Void,
    Undefined,
}

pub trait Mark {
    fn mark(&self, visit: &mut dyn FnMut(GcRef));
}

pub fn unmark(gcref: GcRef) {
    unsafe {
        (*gcref).marked = false;
    }
}
