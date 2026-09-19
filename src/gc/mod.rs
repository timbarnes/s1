pub mod heap;
pub mod objects;

use crate::eval::{CEKState, DynamicWind, KontRef, RunTime};
use crate::io::PortKind;
pub use heap::GcHeap;
use num_bigint::BigInt;
pub use objects::*;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::atomic::AtomicU64;

/// Mirrors `GcHeap::current_epoch`, bumped in lockstep at the start of every
/// collection. `env::Frame` reads this directly so `Mark for EnvRef`
/// (env.rs) can tell whether it's already walked a given frame (and, since
/// the walk always continues to the root, everything above it) during the
/// current cycle — without threading an epoch parameter through the whole
/// `Mark` trait, which every `mark` impl (Control, Kont, CondClause, ...)
/// would otherwise need. This interpreter is single-threaded, so `Relaxed`
/// is enough: it's a plain shared counter, not a synchronization point.
pub static GC_EPOCH: AtomicU64 = AtomicU64::new(0);

#[macro_export]
macro_rules! register_builtin_family {
    ($heap:expr, $env:expr, $($name:expr => ($func:expr, $doc:expr)),* $(,)?) => {
        $(
            $env.define($heap.intern_symbol($name),
                crate::gc::new_builtin($heap, $func, $doc.to_string()));
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
    /// The GC epoch this object was last marked reachable in (0 = never
    /// marked). Comparing against `GcHeap`'s current epoch instead of
    /// resetting a `bool` on every object at the start of every collection
    /// removes the full-heap unmark pass entirely — the epoch bump does its
    /// job implicitly. See `GcHeap::collect_garbage`.
    pub marked: u64,
}

#[derive(Debug, PartialEq)]
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
        // Extracted from a leading string literal in the lambda/define body, if present.
        doc: Option<String>,
    },
    // Scheme-implemented macros
    Macro {
        params: Vec<GcRef>,
        body: GcRef,
        env: Rc<RefCell<crate::env::Frame>>,
        // Extracted from a leading string literal in the macro body, if present.
        doc: Option<String>,
    },
}

/// Scheme values that use GcRef references.
#[derive(Debug, PartialEq)]
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

