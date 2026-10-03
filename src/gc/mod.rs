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
                crate::gc::new_builtin($heap, $name, $func, $doc.to_string()));
        )*
    };
}

#[macro_export]
macro_rules! register_special_form {
    ($rt:expr, $env:expr, $($name:expr => $func:expr),* $(,)?) => {
        $(
            $env.define($rt.intern_symbol($name),
                new_special_form($rt, $name, $func,
                    concat!($name, ": special form").to_string()));
        )*
    };
}

#[macro_export]
macro_rules! register_sys_builtins {
    ($rt:expr, $env:expr, $($name:expr => $func:expr),* $(,)?) => {
        $( $env.define($rt.heap.intern_symbol($name), new_sys_builtin($rt, $name, $func,
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

#[derive(Debug)]
pub enum Callable {
    // Standard library / core functions
    Builtin {
        func: fn(&mut GcHeap, &[GcRef]) -> Result<GcRef, String>,
        name: String,
        doc: String,
    },
    // Privileged system procedures with access to the evaluator
    SysBuiltin {
        func: fn(&mut RunTime, &[GcRef], &mut CEKState, KontRef) -> Result<(), String>,
        name: String,
        doc: String,
    },
    // Syntax procedures called with unevaluated arguments
    SpecialForm {
        func: fn(GcRef, &mut RunTime, &mut CEKState) -> Result<(), String>,
        name: String,
        doc: String,
    },
    // Scheme-implemented procedures
    Closure {
        params: Vec<GcRef>,
        body: GcRef,
        env: Rc<RefCell<crate::env::Frame>>,
        // Extracted from a leading string literal in the lambda/define body, if present.
        doc: Option<String>,
        // The name it was first bound to (see `name_procedure`), for printing.
        name: Option<String>,
        // The (lambda ...) form it was made from, for `procedure-source`.
        source: GcRef,
    },
    // A `case-lambda` procedure: one closure per clause, applied according
    // to the number of arguments (the first clause that accepts them)
    CaseLambda {
        clauses: Vec<GcRef>,
        name: Option<String>,
    },
    // A hygienic `syntax-rules` transformer (src/syntax_rules.rs)
    SyntaxRules(Box<crate::syntax_rules::SyntaxRules>),
    // Scheme-implemented macros
    Macro {
        params: Vec<GcRef>,
        body: GcRef,
        env: Rc<RefCell<crate::env::Frame>>,
        // Extracted from a leading string literal in the macro body, if present.
        doc: Option<String>,
        name: Option<String>,
        source: GcRef,
    },
}

/// Scheme values that use GcRef references.
///
/// Every heap object is sized by the largest variant, so the rare, wide
/// payloads (`Callable` is 72 bytes, `PortKind` 48) are boxed: a cons cell
/// would otherwise pay for them too. See the size assertion below.
#[derive(Debug)]
pub enum SchemeValue {
    Int(BigInt),
    /// An exact non-integer rational, always in lowest terms with a
    /// denominator above 1: arithmetic hands back `Int` whenever the result
    /// is a whole number. Boxed because `BigRational` is two `BigInt`s.
    Rational(Box<num_rational::BigRational>),
    Float(f64),
    Symbol(String),
    Pair(GcRef, GcRef),
    Str(String),
    Vector(Vec<GcRef>),
    Bytevector(Vec<u8>),
    /// The result of `(values ...)` with zero or two-plus values. A single
    /// value is never wrapped. Travelling as an ordinary value lets multiple
    /// values pass unchanged through every continuation frame (closure
    /// returns, dynamic-wind, escapes) until `call-with-values` or the
    /// top level unpacks them.
    Values(Vec<GcRef>),
    Bool(bool),
    Char(char),
    Callable(Box<Callable>),
    Nil,
    TailCallScheduled,
    Port(Box<PortKind>),
    Continuation(Box<ContinuationData>),
    /// A condition made by `error`, or by a built-in procedure failing.
    ErrorObject(Box<ErrorObject>),
    /// A record type made by `define-record-type`
    RecordType(Box<RecordType>),
    /// An instance of a record type
    Record(Box<Record>),
    /// A top-level environment, from `interaction-environment` or
    /// `environment`, for `eval` and `load` (Docs/libraries-design.md)
    Environment(crate::env::EnvRef),
    Eof,
    Void,
    Undefined,
}

/// A record type: its name (as written in `define-record-type`) and field
/// names, in order.
#[derive(Debug)]
pub struct RecordType {
    pub name: GcRef,
    pub fields: Vec<GcRef>,
}

/// A record: its type and one value per field of the type.
#[derive(Debug)]
pub struct Record {
    pub rtype: GcRef,
    pub fields: Vec<GcRef>,
}

/// Which `...-error?` predicate an error object satisfies.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ErrorKind {
    /// From `error`, or a built-in procedure's failure
    General,
    /// `read` met malformed input (`read-error?`)
    Read,
    /// A file could not be opened (`file-error?`)
    File,
}

#[derive(Debug)]
pub struct ErrorObject {
    pub kind: ErrorKind,
    /// Usually a string; `error` accepts any object
    pub message: GcRef,
    /// A proper list
    pub irritants: GcRef,
}

/// A captured continuation: everything `escape` has to reinstate.
#[derive(Debug, PartialEq)]
pub struct ContinuationData {
    pub kont: KontRef,
    pub dw_stack: Vec<DynamicWind>,
    /// Snapshot of `RunTime::arg_stack` at capture. The captured `kont` can
    /// still hold `Kont::EvalArg` frames from further up the call chain
    /// (only the innermost ones are stripped at capture), and their
    /// `args_base` indices — plus any arguments they had already evaluated —
    /// refer to this stack as it was then, not as it is when `k` is invoked.
    pub arg_stack: Vec<GcRef>,
    /// For an escape-only continuation (`%call/ec`), the length of
    /// `arg_stack` at capture, and `arg_stack` is left empty. It can only be
    /// invoked while its `kont` is still part of the current continuation,
    /// so the stack below that length is unchanged and escaping just
    /// truncates to it: capturing costs nothing however deep the stack is.
    pub escape_len: Option<usize>,
    /// The exception handler list (`RunTime::handlers`) at capture.
    pub handlers: GcRef,
}

// Keeps `GcObject` in a 48-byte allocation (one object per cache line). A new
// variant wider than 24 bytes of payload (plus `Int`'s spare sign byte) should
// be boxed rather than grow every object on the heap.
const _: () = assert!(std::mem::size_of::<SchemeValue>() <= 32);

impl SchemeValue {
    /// The callable payload, if this is a procedure. Lets callers match
    /// through the `Box` with nested patterns, e.g.
    /// `Some(Callable::SpecialForm { func, .. })`.
    #[inline]
    pub fn as_callable(&self) -> Option<&Callable> {
        match self {
            SchemeValue::Callable(c) => Some(c),
            _ => None,
        }
    }
}

pub trait Mark {
    fn mark(&self, visit: &mut dyn FnMut(GcRef));
}

