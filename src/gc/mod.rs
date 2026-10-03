//! The garbage-collected heap and the Scheme value representation.
//!
//! Every Scheme object is a `GcObject` allocated by [`GcHeap`] and referred
//! to by a raw pointer, [`GcRef`]. The object's payload is a [`SchemeValue`];
//! procedures of every kind are a [`Callable`]. Objects are reclaimed by a
//! mark-and-sweep collector (`heap`) whose roots are the evaluator state, the
//! ports, the dynamic-wind and argument stacks, and the handler list. Types
//! that hold heap references implement [`Mark`] so the collector can trace
//! them.
//!
//! - `heap`: allocation, interning, the singletons and the collector
//! - `objects`: constructors (`new_pair`, `new_int`, ...), list helpers and
//!   the equivalence predicates
//! - `sstring`: the string payload
//!
//! The `gc_value!` family of macros dereference a `GcRef`; the
//! `register_*!` macros bind families of procedures in an environment.
//! See design/gc-nursery-removal.md and design/gc-tail-loop.md.

pub mod heap;
pub mod objects;
pub mod sstring;

use crate::eval::{CEKState, DynamicWind, KontRef, RunTime};
use crate::io::PortKind;
pub use heap::GcHeap;
use num_bigint::BigInt;
pub use objects::*;
pub use sstring::SString;
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

/// Bind each `name => (func, doc)` in `$env` as a builtin procedure: one
/// that takes evaluated arguments and returns its value directly.
#[macro_export]
macro_rules! register_builtin_family {
    ($heap:expr, $env:expr, $($name:expr => ($func:expr, $doc:expr)),* $(,)?) => {
        $(
            $env.define($heap.intern_symbol($name),
                crate::gc::new_builtin($heap, $name, $func, $doc.to_string()));
        )*
    };
}

/// Bind each `name => func` in `$env` as a special form, called with the
/// unevaluated form and the evaluator.
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

/// Bind each `name => func` in `$env` as a sys-builtin: a procedure that
/// gets evaluated arguments but returns through the CEK machine, so it can
/// capture or replace the continuation.
#[macro_export]
macro_rules! register_sys_builtins {
    ($rt:expr, $env:expr, $($name:expr => $func:expr),* $(,)?) => {
        $( $env.define($rt.heap.intern_symbol($name), new_sys_builtin($rt, $name, $func,
            concat!($name, ": sys-builtin").to_string()));
        )*
    };
}

/// The `SchemeValue` a `GcRef` points to. The reference must be live.
#[macro_export]
macro_rules! gc_value {
    ($r:expr) => {{
        // SAFETY: caller must ensure $r is a valid GcRef pointing to live data
        unsafe { &(*$r).value }
    }};
}

/// The `SchemeValue` a `GcRef` points to, mutably. The reference must be
/// live, and no other borrow of the object may be in use.
#[macro_export]
macro_rules! gc_value_mut {
    ($r:expr) => {{
        // SAFETY: caller must ensure $r is a valid GcRef pointing to live data
        unsafe { &mut (*$r).value }
    }};
}

/// The mark epoch of the object a `GcRef` points to.
#[macro_export]
macro_rules! gc_marked {
    ($r:expr) => {{
        // SAFETY: caller must ensure $r is a valid GcRef pointing to live data
        unsafe { &(*$r).marked }
    }};
}

/// The mark epoch of the object a `GcRef` points to, mutably.
#[macro_export]
macro_rules! gc_marked_mut {
    ($r:expr) => {{
        // SAFETY: caller must ensure $r is a valid GcRef pointing to live data
        unsafe { &mut (*$r).marked }
    }};
}

// core types
/// A reference to a heap object. It is a raw pointer: it stays valid only
/// while the object is reachable from a GC root, so a value held only in a
/// Rust local across an allocation that may collect must be rooted first.
pub type GcRef = *mut GcObject;

/// One heap allocation: a Scheme value and its mark.
pub struct GcObject {
    /// The object's value.
    pub value: SchemeValue,
    /// The GC epoch this object was last marked reachable in (0 = never
    /// marked). Comparing against `GcHeap`'s current epoch instead of
    /// resetting a `bool` on every object at the start of every collection
    /// removes the full-heap unmark pass entirely — the epoch bump does its
    /// job implicitly. See `GcHeap::collect_garbage`.
    pub marked: u64,
}

/// The kinds of procedure, and of syntax bound like a procedure.
///
/// They differ in what they receive and how they return: a `Builtin` gets
/// evaluated arguments and returns a value; a `SysBuiltin` gets evaluated
/// arguments and the machine, and returns by setting the machine's control
/// and continuation; a `SpecialForm` gets the whole unevaluated form.
#[derive(Debug)]
pub enum Callable {
    /// Standard library / core functions
    Builtin {
        func: fn(&mut GcHeap, &[GcRef]) -> Result<GcRef, String>,
        name: String,
        doc: String,
    },
    /// Privileged system procedures with access to the evaluator
    SysBuiltin {
        func: fn(&mut RunTime, &[GcRef], &mut CEKState, KontRef) -> Result<(), String>,
        name: String,
        doc: String,
    },
    /// Syntax procedures called with unevaluated arguments
    SpecialForm {
        func: fn(GcRef, &mut RunTime, &mut CEKState) -> Result<(), String>,
        name: String,
        doc: String,
    },
    /// Scheme-implemented procedures
    Closure {
        /// The rest parameter (nil if none), then the required parameters;
        /// a lone symbol takes all the arguments (see `eval::bind_params`).
        params: Vec<GcRef>,
        /// The body forms, as a list.
        body: GcRef,
        /// The environment the closure was made in.
        env: Rc<RefCell<crate::env::Frame>>,
        /// Extracted from a leading string literal in the lambda/define body, if present.
        doc: Option<String>,
        /// The name it was first bound to (see `name_procedure`), for printing.
        name: Option<String>,
        /// The (lambda ...) form it was made from, for `procedure-source`.
        source: GcRef,
    },
    /// A `case-lambda` procedure: one closure per clause, applied according
    /// to the number of arguments (the first clause that accepts them)
    CaseLambda {
        clauses: Vec<GcRef>,
        name: Option<String>,
    },
    /// A hygienic `syntax-rules` transformer (src/syntax_rules.rs)
    SyntaxRules(Box<crate::syntax_rules::SyntaxRules>),
    /// Scheme-implemented macros
    Macro {
        /// Encoded as for `Closure`.
        params: Vec<GcRef>,
        /// The body forms, as a list.
        body: GcRef,
        /// The environment the macro was defined in.
        env: Rc<RefCell<crate::env::Frame>>,
        /// Extracted from a leading string literal in the macro body, if present.
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
    /// An exact integer.
    Int(BigInt),
    /// An exact non-integer rational, always in lowest terms with a
    /// denominator above 1: arithmetic hands back `Int` whenever the result
    /// is a whole number. Boxed because `BigRational` is two `BigInt`s.
    Rational(Box<num_rational::BigRational>),
    /// An inexact real.
    Float(f64),
    /// A symbol. Symbols read or made by `string->symbol` are interned
    /// (`GcHeap::intern_symbol`), so `eq?` is pointer equality; aliases made
    /// by macro expansion are not (see `eval::identifiers`).
    Symbol(String),
    /// A cons cell: car and cdr.
    Pair(GcRef, GcRef),
    /// A string.
    Str(SString),
    /// A vector.
    Vector(Vec<GcRef>),
    /// A bytevector.
    Bytevector(Vec<u8>),
    /// The result of `(values ...)` with zero or two-plus values. A single
    /// value is never wrapped. Travelling as an ordinary value lets multiple
    /// values pass unchanged through every continuation frame (closure
    /// returns, dynamic-wind, escapes) until `call-with-values` or the
    /// top level unpacks them.
    Values(Vec<GcRef>),
    /// `#t` or `#f`; both are singletons on the heap.
    Bool(bool),
    /// A character.
    Char(char),
    /// A procedure or syntax keyword's binding.
    Callable(Box<Callable>),
    /// The empty list; a singleton.
    Nil,
    /// A marker value; it has no Scheme meaning and prints as unprintable.
    TailCallScheduled,
    /// An input or output port.
    Port(Box<PortKind>),
    /// A continuation captured by `call/cc` or `%call/ec`.
    Continuation(Box<ContinuationData>),
    /// A condition made by `error`, or by a built-in procedure failing.
    ErrorObject(Box<ErrorObject>),
    /// A record type made by `define-record-type`
    RecordType(Box<RecordType>),
    /// An instance of a record type
    Record(Box<Record>),
    /// A top-level environment, from `interaction-environment` or
    /// `environment`, for `eval` and `load` (design/libraries-design.md)
    Environment(crate::env::EnvRef),
    /// The end-of-file object; a singleton.
    Eof,
    /// The value of a form that has nothing to return; the REPL prints
    /// nothing for it. A singleton (`GcHeap::void`).
    Void,
    /// The unspecified value R7RS leaves to the implementation, e.g. of
    /// `set!` or `vector-fill!`; prints as `#<undefined>`. A singleton
    /// (`GcHeap::unspecified`).
    Undefined,
}

/// A record type: its name (as written in `define-record-type`) and field
/// names, in order.
#[derive(Debug)]
pub struct RecordType {
    /// The type name, a symbol.
    pub name: GcRef,
    /// The field names, symbols.
    pub fields: Vec<GcRef>,
}

/// A record: its type and one value per field of the type.
#[derive(Debug)]
pub struct Record {
    /// The `RecordType` object.
    pub rtype: GcRef,
    /// The field values, in the type's field order.
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

/// An error object (R7RS 6.11): what `error-object-message` and
/// `error-object-irritants` return.
#[derive(Debug)]
pub struct ErrorObject {
    /// Which error predicate it satisfies.
    pub kind: ErrorKind,
    /// Usually a string; `error` accepts any object
    pub message: GcRef,
    /// A proper list
    pub irritants: GcRef,
}

/// A captured continuation: everything `escape` has to reinstate.
#[derive(Debug, PartialEq)]
pub struct ContinuationData {
    /// The continuation chain to return into.
    pub kont: KontRef,
    /// The dynamic-wind stack at capture; `escape` runs the `after` and
    /// `before` thunks needed to get from the current stack to this one.
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

/// Tracing for the collector: a type that holds heap references reports
/// each one to `visit`.
pub trait Mark {
    /// Call `visit` on every `GcRef` this value holds directly.
    fn mark(&self, visit: &mut dyn FnMut(GcRef));
}

