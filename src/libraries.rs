//! Libraries: the registry, the standard libraries, and `environment`
//! (R7RS 5.6 and 6.12; Docs/libraries-design.md).
//!
//! A library is an export table: external names mapped to binding cells.
//! Importing binds names in the importing environment to those same cells
//! (`EnvOps::bind_cell`), so importer and library share each variable.
//!
//! The standard libraries have no code of their own. Each is a list of the
//! names R7RS assigns it (appendix A), exported straight from the system
//! environment's cells. Names s1 doesn't implement are left out of the
//! export table and recorded, so a test can list them.

use crate::env::{CellRef, EnvOps, EnvRef, Frame};
use crate::gc::{GcHeap, GcRef, SchemeValue, list_from_slice, new_string};
use crate::gc_value;
use crate::register_builtin_family;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

/// A library name, `(scheme base)`, as its parts: symbols, or exact
/// non-negative integers written in decimal.
pub type LibraryName = Vec<String>;

pub struct Library {
    /// External name and cell, in export order. Every cell belongs to
    /// `env` (defined there or imported into it), or, for a standard
    /// library, to the system environment.
    pub exports: Vec<(GcRef, CellRef)>,
    /// The environment a `define-library` body ran in; `None` for the
    /// standard libraries, whose cells are the system environment's.
    pub env: Option<EnvRef>,
    /// Names a standard library should export that s1 doesn't define.
    pub unimplemented: Vec<&'static str>,
}

/// The registered libraries; part of the heap so that built-in procedures
/// can reach it. Exported cells' values are marked as GC roots.
#[derive(Default)]
pub struct Libraries {
    table: HashMap<LibraryName, Library>,
}

impl Libraries {
    pub fn get(&self, name: &LibraryName) -> Option<&Library> {
        self.table.get(name)
    }

    pub fn insert(&mut self, name: LibraryName, library: Library) {
        self.table.insert(name, library);
    }

    pub fn names(&self) -> Vec<LibraryName> {
        let mut names: Vec<_> = self.table.keys().cloned().collect();
        names.sort();
        names
    }

    /// Visit what the libraries keep alive, for the GC: each library
    /// environment, which holds every export's cell, and the export names.
    /// The standard libraries need nothing: their names are interned symbols
    /// and their cells the system environment's, both roots already.
    pub fn mark(&self, visit: &mut dyn FnMut(GcRef)) {
        use crate::gc::Mark;
        for library in self.table.values() {
            if let Some(env) = &library.env {
                env.mark(visit);
                for (name, _) in &library.exports {
                    visit(*name);
                }
            }
        }
    }
}

/// `(scheme base)` and friends, from R7RS appendix A. `(s1)`, everything
/// else in the system environment, is computed from these.
const STANDARD_LIBRARIES: &[(&[&str], &[&str])] = &[
    (&["scheme", "base"], &[
        "*", "+", "-", "...", "/", "<", "<=", "=", "=>", ">", ">=", "_", "abs", "and", "append", "apply",
        "assoc", "assq", "assv", "begin", "binary-port?", "boolean=?", "boolean?", "bytevector",
        "bytevector-append", "bytevector-copy", "bytevector-copy!", "bytevector-length",
        "bytevector-u8-ref", "bytevector-u8-set!", "bytevector?", "caar", "cadr",
        "call-with-current-continuation", "call-with-port", "call-with-values", "call/cc", "car",
        "case", "cdar", "cddr", "cdr", "ceiling", "char->integer", "char-ready?", "char<=?", "char<?",
        "char=?", "char>=?", "char>?", "char?", "close-input-port", "close-output-port", "close-port",
        "complex?", "cond", "cond-expand", "cons", "current-error-port", "current-input-port",
        "current-output-port", "define", "define-record-type", "define-syntax", "define-values",
        "denominator", "do", "dynamic-wind", "else", "eof-object", "eof-object?", "eq?", "equal?",
        "eqv?", "error", "error-object-irritants", "error-object-message", "error-object?", "even?",
        "exact", "exact-integer-sqrt", "exact-integer?", "exact?", "expt", "features", "file-error?",
        "floor", "floor-quotient", "floor-remainder", "floor/", "flush-output-port", "for-each", "gcd",
        "get-output-bytevector", "get-output-string", "guard", "if", "import", "include", "include-ci",
        "inexact", "inexact?", "input-port-open?", "input-port?", "integer->char", "integer?", "lambda",
        "lcm", "length", "let", "let*", "let*-values", "let-syntax", "let-values", "letrec", "letrec*",
        "letrec-syntax", "list", "list->string", "list->vector", "list-copy", "list-ref", "list-set!",
        "list-tail", "list?", "make-bytevector", "make-list", "make-parameter", "make-string",
        "make-vector", "map", "max", "member", "memq", "memv", "min", "modulo", "negative?", "newline",
        "not", "null?", "number->string", "number?", "numerator", "odd?", "open-input-bytevector",
        "open-input-string", "open-output-bytevector", "open-output-string", "or", "output-port-open?",
        "output-port?", "pair?", "parameterize", "peek-char", "peek-u8", "positive?", "procedure?",
        "quasiquote", "quote", "quotient", "raise", "raise-continuable", "rational?", "rationalize",
        "read-bytevector", "read-bytevector!", "read-char", "read-error?", "read-line", "read-string",
        "read-u8", "real?", "remainder", "reverse", "round", "set!", "set-car!", "set-cdr!", "square",
        "string", "string->list", "string->number", "string->symbol", "string->utf8", "string->vector",
        "string-append", "string-copy", "string-copy!", "string-fill!", "string-for-each",
        "string-length", "string-map", "string-ref", "string-set!", "string<=?", "string<?", "string=?",
        "string>=?", "string>?", "string?", "substring", "symbol->string", "symbol=?", "symbol?",
        "syntax-error", "syntax-rules", "textual-port?", "truncate", "truncate-quotient",
        "truncate-remainder", "truncate/", "u8-ready?", "unless", "unquote", "unquote-splicing",
        "utf8->string", "values", "vector", "vector->list", "vector->string", "vector-append",
        "vector-copy", "vector-copy!", "vector-fill!", "vector-for-each", "vector-length", "vector-map",
        "vector-ref", "vector-set!", "vector?", "when", "with-exception-handler", "write-bytevector",
        "write-char", "write-string", "write-u8", "zero?",
    ]),
    (&["scheme", "case-lambda"], &["case-lambda"]),
    (&["scheme", "char"], &[
        "char-alphabetic?", "char-ci<=?", "char-ci<?", "char-ci=?", "char-ci>=?", "char-ci>?",
        "char-downcase", "char-foldcase", "char-lower-case?", "char-numeric?", "char-upcase",
        "char-upper-case?", "char-whitespace?", "digit-value", "string-ci<=?", "string-ci<?",
        "string-ci=?", "string-ci>=?", "string-ci>?", "string-downcase", "string-foldcase",
        "string-upcase",
    ]),
    (&["scheme", "complex"], &["angle", "imag-part", "magnitude", "make-polar", "make-rectangular", "real-part"]),
    (&["scheme", "cxr"], &[
        "caaar", "caadr", "cadar", "caddr", "cdaar", "cdadr", "cddar", "cdddr", "caaaar", "caaadr",
        "caadar", "caaddr", "cadaar", "cadadr", "caddar", "cadddr", "cdaaar", "cdaadr", "cdadar",
        "cdaddr", "cddaar", "cddadr", "cdddar", "cddddr",
    ]),
    (&["scheme", "eval"], &["environment", "eval"]),
    (&["scheme", "file"], &[
        "call-with-input-file", "call-with-output-file", "delete-file", "file-exists?",
        "open-binary-input-file", "open-binary-output-file", "open-input-file", "open-output-file",
        "with-input-from-file", "with-output-to-file",
    ]),
    (&["scheme", "inexact"], &[
        "acos", "asin", "atan", "cos", "exp", "finite?", "infinite?", "log", "nan?", "sin", "sqrt", "tan",
    ]),
    (&["scheme", "lazy"], &["delay", "delay-force", "force", "make-promise", "promise?"]),
    (&["scheme", "load"], &["load"]),
    (&["scheme", "process-context"], &[
        "command-line", "emergency-exit", "exit", "get-environment-variable", "get-environment-variables",
    ]),
    (&["scheme", "read"], &["read"]),
    (&["scheme", "repl"], &["interaction-environment"]),
    (&["scheme", "time"], &["current-jiffy", "current-second", "jiffies-per-second"]),
    (&["scheme", "write"], &["display", "write", "write-shared", "write-simple"]),
    (&["scheme", "r5rs"], &[
        "*", "+", "-", "/", "<", "<=", "=", ">", ">=", "abs", "acos", "and", "angle", "append", "apply",
        "asin", "assoc", "assq", "assv", "atan", "begin", "boolean?", "caaaar", "caaadr", "caaar",
        "caadar", "caaddr", "caadr", "caar", "cadaar", "cadadr", "cadar", "caddar", "cadddr", "caddr",
        "cadr", "call-with-current-continuation", "call-with-input-file", "call-with-output-file",
        "call-with-values", "car", "case", "cdaaar", "cdaadr", "cdaar", "cdadar", "cdaddr", "cdadr",
        "cdar", "cddaar", "cddadr", "cddar", "cdddar", "cddddr", "cdddr", "cddr", "cdr", "ceiling",
        "char->integer", "char-alphabetic?", "char-ci<=?", "char-ci<?", "char-ci=?", "char-ci>=?",
        "char-ci>?", "char-downcase", "char-lower-case?", "char-numeric?", "char-ready?", "char-upcase",
        "char-upper-case?", "char-whitespace?", "char<=?", "char<?", "char=?", "char>=?", "char>?",
        "char?", "close-input-port", "close-output-port", "complex?", "cond", "cons", "cos",
        "current-input-port", "current-output-port", "define", "define-syntax", "delay", "denominator",
        "display", "do", "dynamic-wind", "eof-object?", "eq?", "equal?", "eqv?", "eval", "even?",
        "exact->inexact", "exact?", "exp", "expt", "floor", "for-each", "force", "gcd", "if",
        "imag-part", "inexact->exact", "inexact?", "input-port?", "integer->char", "integer?",
        "interaction-environment", "lambda", "lcm", "length", "let", "let*", "let-syntax", "letrec",
        "letrec-syntax", "list", "list->string", "list->vector", "list-ref", "list-tail", "list?",
        "load", "log", "magnitude", "make-polar", "make-rectangular", "make-string", "make-vector",
        "map", "max", "member", "memq", "memv", "min", "modulo", "negative?", "newline", "not",
        "null-environment", "null?", "number->string", "number?", "numerator", "odd?",
        "open-input-file", "open-output-file", "or", "output-port?", "pair?", "peek-char",
        "positive?", "procedure?", "quasiquote", "quote", "quotient", "rational?", "rationalize",
        "read", "read-char", "real-part", "real?", "remainder", "reverse", "round",
        "scheme-report-environment", "set!", "set-car!", "set-cdr!", "sin", "sqrt", "string",
        "string->list", "string->number", "string->symbol", "string-append", "string-ci<=?",
        "string-ci<?", "string-ci=?", "string-ci>=?", "string-ci>?", "string-copy", "string-fill!",
        "string-length", "string-ref", "string-set!", "string<=?", "string<?", "string=?",
        "string>=?", "string>?", "string?", "substring", "symbol->string", "symbol?", "tan",
        "truncate", "values", "vector", "vector->list", "vector-fill!", "vector-length", "vector-ref",
        "vector-set!", "vector?", "with-input-from-file", "with-output-to-file", "write",
        "write-char", "zero?",
    ]),
];

/// Register the standard libraries, and `(s1)`, over the system
/// environment's cells. Called once, after `s1-core.scm` has loaded.
pub fn register_standard_libraries(heap: &mut GcHeap, system: &EnvRef) {
    let cells: HashMap<GcRef, CellRef> = system.top_level_cells().into_iter().collect();
    let mut standard = HashSet::default();
    for (name, members) in STANDARD_LIBRARIES {
        let mut exports = Vec::new();
        let mut unimplemented = Vec::new();
        for member in *members {
            let symbol = heap.intern_symbol(member);
            standard.insert(symbol);
            match cells.get(&symbol) {
                Some(cell) => exports.push((symbol, cell.clone())),
                None => unimplemented.push(*member),
            }
        }
        let name = name.iter().map(|part| part.to_string()).collect();
        heap.libraries.insert(name, Library { exports, env: None, unimplemented });
    }
    // (s1): everything else, in name order.
    let mut rest: Vec<(GcRef, CellRef)> =
        cells.into_iter().filter(|(symbol, _)| !standard.contains(symbol)).collect();
    rest.sort_by_key(|(symbol, _)| symbol_name(*symbol));
    heap.libraries.insert(vec!["s1".to_string()], Library { exports: rest, env: None, unimplemented: Vec::new() });
}

fn symbol_name(symbol: GcRef) -> String {
    match gc_value!(symbol) {
        SchemeValue::Symbol(name) => name.clone(),
        _ => String::new(),
    }
}

/// Parse a library name datum such as `(scheme base)`.
pub fn library_name(datum: GcRef, who: &str) -> Result<LibraryName, String> {
    let mut parts = Vec::new();
    let mut rest = datum;
    loop {
        match gc_value!(rest) {
            SchemeValue::Nil if !parts.is_empty() => return Ok(parts),
            SchemeValue::Pair(part, tail) => {
                match gc_value!(*part) {
                    SchemeValue::Symbol(name) => parts.push(name.clone()),
                    SchemeValue::Int(n) if n.sign() != num_bigint::Sign::Minus => parts.push(n.to_string()),
                    _ => break,
                }
                rest = *tail;
            }
            _ => break,
        }
    }
    Err(format!("{}: not a library name: {}", who, crate::printer::print_value(&datum)))
}

pub fn format_name(name: &LibraryName) -> String {
    format!("({})", name.join(" "))
}

/// Bind every export of `library` in `env`, as imports.
pub fn import_all(env: &EnvRef, library: &Library) {
    for (name, cell) in &library.exports {
        env.bind_cell(*name, cell.clone());
    }
}

/// Make the interaction environment: a new top-level environment with every
/// system binding imported (Docs/libraries-design.md, "The environments").
pub fn make_interaction_env(system: &EnvRef) -> EnvRef {
    let env = Frame::new_top_level(true);
    for (name, cell) in system.top_level_cells() {
        env.bind_cell(name, cell);
    }
    env
}

pub fn register_library_builtins(heap: &mut GcHeap, env: EnvRef) {
    register_builtin_family!(heap, env,
        "environment" => (environment, "(environment library-name ...) A new immutable environment containing the exports of the named libraries"),
        "library-exports" => (library_exports, "(library-exports library-name) The names a registered library exports, as a list of symbols"),
        "library-names" => (library_names, "(library-names) The names of the registered libraries"),
        "%library-unimplemented" => (library_unimplemented, "(%library-unimplemented library-name) The names R7RS assigns a standard library that s1 doesn't define, as a list of strings"),
    );
}

fn lookup<'a>(heap: &'a GcHeap, datum: GcRef, who: &str) -> Result<&'a Library, String> {
    let name = library_name(datum, who)?;
    heap.libraries
        .get(&name)
        .ok_or_else(|| format!("{}: unknown library {}", who, format_name(&name)))
}

/// (environment library-name ...)
fn environment(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let env = Frame::new_top_level(false);
    for arg in args {
        import_all(&env, lookup(heap, *arg, "environment")?);
    }
    Ok(heap.alloc(crate::gc::GcObject {
        value: SchemeValue::Environment(env),
        marked: 0,
    }))
}

fn one_arg(args: &[GcRef], who: &str) -> Result<GcRef, String> {
    match args {
        [arg] => Ok(*arg),
        _ => Err(format!("{}: expected 1 argument", who)),
    }
}

fn library_exports(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let library = lookup(heap, one_arg(args, "library-exports")?, "library-exports")?;
    let names: Vec<GcRef> = library.exports.iter().map(|(name, _)| *name).collect();
    Ok(list_from_slice(&names, heap))
}

fn library_names(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if !args.is_empty() {
        return Err("library-names: takes no arguments".to_string());
    }
    let mut names = Vec::new();
    for name in heap.libraries.names() {
        let parts: Vec<GcRef> = name
            .iter()
            .map(|part| match part.parse::<u64>() {
                Ok(n) => crate::gc::new_int(heap, num_bigint::BigInt::from(n)),
                Err(_) => heap.intern_symbol(part),
            })
            .collect();
        names.push(list_from_slice(&parts, heap));
    }
    Ok(list_from_slice(&names, heap))
}

fn library_unimplemented(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let library = lookup(heap, one_arg(args, "%library-unimplemented")?, "%library-unimplemented")?;
    let names = library.unimplemented.clone();
    let strings: Vec<GcRef> = names.iter().map(|name| new_string(heap, name)).collect();
    Ok(list_from_slice(&strings, heap))
}
