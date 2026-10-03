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
    /// Library files already loaded: each is loaded at most once per run.
    loaded: HashSet<std::path::PathBuf>,
}

impl Libraries {
    pub fn get(&self, name: &LibraryName) -> Option<&Library> {
        self.table.get(name)
    }

    pub fn insert(&mut self, name: LibraryName, library: Library) {
        self.table.insert(name, library);
    }

    pub fn is_loaded(&self, path: &std::path::Path) -> bool {
        self.loaded.contains(path)
    }

    pub fn mark_loaded(&mut self, path: std::path::PathBuf) {
        self.loaded.insert(path);
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

/// The (name, cell) pairs an import set denotes (R7RS 5.2):
/// a library name, or `only`, `except`, `prefix` or `rename` applied to an
/// import set. Identifiers are compared by the name they are written as, so
/// an import produced by a macro works.
pub fn resolve_import_set(heap: &mut GcHeap, set: GcRef, who: &str) -> Result<Vec<(GcRef, CellRef)>, String> {
    let parts = crate::gc::list_to_vec(heap, set)
        .map_err(|_| format!("{}: not an import set: {}", who, crate::printer::print_value(&set)))?;
    let keyword = parts.first().map(|head| identifier_name(heap, *head));
    let modifier = matches!(keyword.as_deref(), Some("only" | "except" | "prefix" | "rename"));
    if !modifier || parts.len() < 2 {
        let name = library_name(set, who)?;
        let library = heap
            .libraries
            .get(&name)
            .ok_or_else(|| format!("{}: unknown library {}", who, format_name(&name)))?;
        return Ok(library.exports.clone());
    }
    let keyword = keyword.unwrap();
    let mut bindings = resolve_import_set(heap, parts[1], who)?;
    let args = &parts[2..];
    let position = |bindings: &Vec<(GcRef, CellRef)>, heap: &GcHeap, id: GcRef| -> Result<usize, String> {
        let name = identifier_name(heap, id);
        bindings
            .iter()
            .position(|(n, _)| symbol_name(*n) == name)
            .ok_or_else(|| format!("{}: {} is not in the import set {}", who, name, crate::printer::print_value(&parts[1])))
    };
    match keyword.as_str() {
        "only" => {
            let mut kept = Vec::with_capacity(args.len());
            for id in args {
                kept.push(bindings[position(&bindings, heap, *id)?].clone());
            }
            bindings = kept;
        }
        "except" => {
            for id in args {
                let i = position(&bindings, heap, *id)?;
                bindings.remove(i);
            }
        }
        "prefix" => {
            let [prefix] = args else {
                return Err(format!("{}: prefix takes an import set and one identifier", who));
            };
            let prefix = identifier_name(heap, *prefix);
            for (name, _) in bindings.iter_mut() {
                *name = heap.intern_symbol(&format!("{}{}", prefix, symbol_name(*name)));
            }
        }
        _ => {
            // rename: each argument is (from to)
            for pair in args {
                let ids = crate::gc::list_to_vec(heap, *pair).unwrap_or_default();
                let [from, to] = ids[..] else {
                    return Err(format!("{}: rename expects (from to) pairs", who));
                };
                let i = position(&bindings, heap, from)?;
                bindings[i].0 = heap.intern_symbol(&identifier_name(heap, to));
            }
        }
    }
    Ok(bindings)
}

/// The name an identifier is written as (an alias's original name), or ""
/// for anything that isn't an identifier.
fn identifier_name(heap: &GcHeap, id: GcRef) -> String {
    symbol_name(crate::eval::identifiers::strip(heap, id))
}

/// Bind `bindings` in the top-level environment `env`, as imports.
pub fn import_bindings(env: &EnvRef, bindings: Vec<(GcRef, CellRef)>) {
    for (name, cell) in bindings {
        env.bind_cell(name, cell);
    }
}

/// (import import-set ...)
/// Valid where definitions are, at the top level of an environment. Every
/// import set is resolved before anything is bound, so a failing import
/// binds nothing.
pub fn import_sf(expr: GcRef, ec: &mut crate::eval::RunTime, state: &mut crate::eval::CEKState) -> Result<(), String> {
    if !matches!(state.env.borrow().bindings, crate::env::Bindings::Large(_)) {
        return Err("import: only allowed at top level".to_string());
    }
    if !state.env.is_mutable() {
        return Err("import: this environment is immutable".to_string());
    }
    let sets = crate::gc::list_to_vec(ec.heap, crate::gc::cdr(expr)?)?;
    if load_then_retry(ec, state, &sets, expr, "import")? {
        return Ok(());
    }
    let mut bindings = Vec::new();
    for set in sets {
        bindings.extend(resolve_import_set(ec.heap, set, "import")?);
    }
    import_bindings(&state.env, bindings);
    crate::eval::insert_value(state, ec.heap.unspecified());
    Ok(())
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
        "environment" => (environment, "(environment import-set ...) A new immutable environment containing the bindings of the import sets"),
        "scheme-report-environment" => (scheme_report_environment, "(scheme-report-environment 5) An immutable environment of (scheme r5rs)"),
        "null-environment" => (null_environment, "(null-environment 5) An immutable environment of the syntax in (scheme r5rs)"),
        "features" => (features, "(features) The feature identifiers cond-expand recognizes, as a list of symbols"),
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

fn environment_value(heap: &mut GcHeap, bindings: Vec<(GcRef, CellRef)>) -> GcRef {
    let env = Frame::new_top_level(false);
    import_bindings(&env, bindings);
    heap.alloc(crate::gc::GcObject {
        value: SchemeValue::Environment(env),
        marked: 0,
    })
}

/// (environment import-set ...)
fn environment(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let mut bindings = Vec::new();
    for arg in args {
        bindings.extend(resolve_import_set(heap, *arg, "environment")?);
    }
    Ok(environment_value(heap, bindings))
}

/// The bindings of `(scheme r5rs)`, for an R5RS environment of `version`
/// (which must be 5).
fn r5rs_bindings(heap: &GcHeap, args: &[GcRef], who: &str) -> Result<Vec<(GcRef, CellRef)>, String> {
    match args {
        [v] if matches!(gc_value!(*v), SchemeValue::Int(n) if *n == num_bigint::BigInt::from(5)) => {}
        _ => return Err(format!("{}: the only version supported is 5", who)),
    }
    let name = vec!["scheme".to_string(), "r5rs".to_string()];
    Ok(heap.libraries.get(&name).map(|l| l.exports.clone()).unwrap_or_default())
}

/// (scheme-report-environment 5)
fn scheme_report_environment(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let bindings = r5rs_bindings(heap, args, "scheme-report-environment")?;
    Ok(environment_value(heap, bindings))
}

/// (null-environment 5): the syntactic keywords of (scheme r5rs) only.
fn null_environment(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let mut bindings = r5rs_bindings(heap, args, "null-environment")?;
    bindings.retain(|(_, cell)| {
        cell.get().is_some_and(|value| {
            matches!(gc_value!(value), SchemeValue::Callable(c) if matches!(**c,
                crate::gc::Callable::SpecialForm { .. } | crate::gc::Callable::SyntaxRules(_) | crate::gc::Callable::Macro { .. }))
        })
    });
    Ok(environment_value(heap, bindings))
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

// ---------------------------------------------------------------------------
// define-library and include

use crate::eval::{CEKState, Control, Kont, KontRef, RunTime};

/// Read every datum in the file `name`, with case folding for `include-ci`.
/// Relative names are relative to the current directory.
pub fn read_file_forms(heap: &mut GcHeap, name: &str, fold_case: bool, who: &str) -> Result<Vec<GcRef>, String> {
    let content = std::fs::read_to_string(name).map_err(|e| format!("{}: could not read {}: {}", who, name, e))?;
    let mut port = crate::io::new_string_port_input(&content);
    if let crate::io::PortKind::StringPortInput { fold_case: fold, .. } = &port {
        fold.set(fold_case);
    }
    let mut forms = Vec::new();
    loop {
        match crate::parser::parse(heap, &mut port) {
            Ok(form) => forms.push(form),
            Err(crate::parser::ParseError::Eof) => return Ok(forms),
            Err(crate::parser::ParseError::Syntax(e)) => return Err(format!("{}: {}: {}", who, name, e)),
        }
    }
}

/// The forms of the files named by `files` (a list of strings), in order.
fn include_forms(heap: &mut GcHeap, files: GcRef, fold_case: bool, who: &str) -> Result<Vec<GcRef>, String> {
    let names = crate::gc::list_to_vec(heap, files)?;
    if names.is_empty() {
        return Err(format!("{}: expects at least one file name", who));
    }
    let mut forms = Vec::new();
    for name in names {
        let SchemeValue::Str(name) = gc_value!(name) else {
            return Err(format!("{}: file names must be strings", who));
        };
        let name = name.clone();
        forms.extend(read_file_forms(heap, &name, fold_case, who)?);
    }
    Ok(forms)
}

fn include(expr: GcRef, ec: &mut RunTime, state: &mut CEKState, fold_case: bool, who: &str) -> Result<(), String> {
    let forms = include_forms(ec.heap, crate::gc::cdr(expr)?, fold_case, who)?;
    if forms.is_empty() {
        crate::eval::insert_value(state, ec.heap.unspecified());
        return Ok(());
    }
    // (begin form ...) where the include was, in its tail position.
    let begin = ec.heap.core_form("begin");
    let body = list_from_slice(&forms, ec.heap);
    let begin_form = crate::gc::new_pair(ec.heap, begin, body);
    crate::eval::insert_eval(state, begin_form, state.tail);
    Ok(())
}

/// (include file ...): the files' forms, as a `begin`, in place of the include.
pub fn include_sf(expr: GcRef, ec: &mut RunTime, state: &mut CEKState) -> Result<(), String> {
    include(expr, ec, state, false, "include")
}

/// (include-ci file ...): as `include`, reading with case folding.
pub fn include_ci_sf(expr: GcRef, ec: &mut RunTime, state: &mut CEKState) -> Result<(), String> {
    include(expr, ec, state, true, "include-ci")
}

/// What a `define-library`'s declarations add up to.
#[derive(Default)]
struct Declarations {
    /// (internal, external) names
    exports: Vec<(GcRef, GcRef)>,
    imports: Vec<GcRef>,
    body: Vec<GcRef>,
}

fn collect_declarations(heap: &mut GcHeap, decls: &[GcRef], out: &mut Declarations) -> Result<(), String> {
    let who = "define-library";
    for decl in decls {
        let parts = crate::gc::list_to_vec(heap, *decl)
            .map_err(|_| format!("{}: not a declaration: {}", who, crate::printer::print_value(decl)))?;
        let keyword = parts.first().map(|k| identifier_name(heap, *k)).unwrap_or_default();
        let args = &parts[1.min(parts.len())..];
        match keyword.as_str() {
            "export" => {
                for spec in args {
                    match gc_value!(*spec) {
                        SchemeValue::Symbol(_) => out.exports.push((*spec, *spec)),
                        _ => {
                            let spec_parts = crate::gc::list_to_vec(heap, *spec).unwrap_or_default();
                            match spec_parts[..] {
                                [kw, internal, external] if identifier_name(heap, kw) == "rename" => {
                                    out.exports.push((internal, external))
                                }
                                _ => {
                                    return Err(format!(
                                        "{}: bad export spec {}",
                                        who,
                                        crate::printer::print_value(spec)
                                    ));
                                }
                            }
                        }
                    }
                }
            }
            "import" => out.imports.extend_from_slice(args),
            "begin" => out.body.extend_from_slice(args),
            "include" | "include-ci" => {
                let forms = include_forms(heap, crate::gc::cdr(*decl)?, keyword == "include-ci", &keyword)?;
                out.body.extend(forms);
            }
            "include-library-declarations" => {
                let forms = include_forms(heap, crate::gc::cdr(*decl)?, false, &keyword)?;
                collect_declarations(heap, &forms, out)?;
            }
            "cond-expand" => {
                let chosen = choose_cond_expand_clause(heap, args, who)?;
                collect_declarations(heap, &chosen, out)?;
            }
            _ => return Err(format!("{}: unknown declaration {}", who, crate::printer::print_value(decl))),
        }
    }
    Ok(())
}

/// (define-library name declaration ...)
///
/// The imports are made into a new library environment, then the body runs
/// there, through the machine, with the caller's environment restored after.
/// The body's last step is a call to `%register-library`, which checks that
/// every export is defined and only then registers the library, so a
/// library whose body fails isn't registered (Docs/libraries-design.md).
pub fn define_library_sf(expr: GcRef, ec: &mut RunTime, state: &mut CEKState) -> Result<(), String> {
    let who = "define-library";
    let parts = crate::gc::list_to_vec(ec.heap, expr)?;
    if parts.len() < 2 {
        return Err(format!("{}: expects a library name", who));
    }
    if !matches!(state.env.borrow().bindings, crate::env::Bindings::Large(_)) {
        return Err(format!("{}: only allowed at top level", who));
    }
    library_name(parts[1], who)?;
    let mut decls = Declarations::default();
    collect_declarations(ec.heap, &parts[2..], &mut decls)?;
    if load_then_retry(ec, state, &decls.imports, expr, who)? {
        return Ok(());
    }

    // Imports. Importing one name with two different bindings is an error.
    let env = Frame::new_top_level(true);
    let mut seen: HashMap<GcRef, CellRef> = HashMap::default();
    for set in &decls.imports {
        for (name, cell) in resolve_import_set(ec.heap, *set, who)? {
            match seen.get(&name) {
                Some(existing) if !std::rc::Rc::ptr_eq(existing, &cell) => {
                    return Err(format!("{}: {} is imported with two different bindings", who, symbol_name(name)));
                }
                _ => {
                    seen.insert(name, cell.clone());
                    env.bind_cell(name, cell);
                }
            }
        }
    }

    // The body, then (%register-library 'name '((internal . external) ...) env).
    let quote = ec.heap.core_form("quote");
    let mut export_pairs = Vec::with_capacity(decls.exports.len());
    for (internal, external) in &decls.exports {
        export_pairs.push(crate::gc::new_pair(ec.heap, *internal, *external));
    }
    let export_list = list_from_slice(&export_pairs, ec.heap);
    let quoted_name = list_from_slice(&[quote, parts[1]], ec.heap);
    let quoted_exports = list_from_slice(&[quote, export_list], ec.heap);
    let env_value = ec.heap.alloc(crate::gc::GcObject {
        value: SchemeValue::Environment(env.clone()),
        marked: 0,
    });
    let register = crate::gc::new_sys_builtin(
        ec,
        "%register-library",
        register_library_sp,
        "%register-library: sys-builtin".to_string(),
    );
    let register_call = list_from_slice(&[register, quoted_name, quoted_exports, env_value], ec.heap);
    let mut body = decls.body;
    body.push(register_call);
    let begin = ec.heap.core_form("begin");
    let body_list = list_from_slice(&body, ec.heap);
    let begin_form = crate::gc::new_pair(ec.heap, begin, body_list);

    let old_env = state.env.clone();
    state.kont = std::rc::Rc::new(Kont::RestoreEnv { old_env, next: state.kont.clone() });
    state.env = env;
    state.control = Control::Expr(begin_form);
    state.tail = false;
    Ok(())
}

/// (%register-library 'name '((internal . external) ...) env): the last
/// step of a `define-library` body.
fn register_library_sp(ec: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    let [name_datum, export_list, env_value] = args else {
        return Err("%register-library: expects 3 arguments".to_string());
    };
    let SchemeValue::Environment(env) = gc_value!(*env_value) else {
        return Err("%register-library: not an environment".to_string());
    };
    let env = env.clone();
    let name = library_name(*name_datum, "define-library")?;
    let mut exports = Vec::new();
    for pair in crate::gc::list_to_vec(ec.heap, *export_list)? {
        let SchemeValue::Pair(internal, external) = gc_value!(pair) else {
            return Err("%register-library: bad export list".to_string());
        };
        let cell = env.cell(*internal).expect("a library environment is top level");
        if cell.get().is_none() {
            return Err(format!(
                "define-library: {} exports {}, which it doesn't define",
                format_name(&name),
                identifier_name(ec.heap, *internal)
            ));
        }
        // The external name is what importers write, so a plain symbol even
        // if a macro produced the declaration.
        exports.push((crate::eval::identifiers::strip(ec.heap, *external), cell));
    }
    ec.heap.libraries.insert(name, Library { exports, env: Some(env), unimplemented: Vec::new() });
    state.control = Control::Value(ec.heap.unspecified());
    state.kont = next;
    Ok(())
}

// ---------------------------------------------------------------------------
// Library files

/// The directories searched for library files: those in `S1_LIBRARY_PATH`
/// (colon separated), then the current directory, then `scheme/lib`.
fn search_path() -> Vec<std::path::PathBuf> {
    let mut dirs: Vec<std::path::PathBuf> = std::env::var("S1_LIBRARY_PATH")
        .map(|p| p.split(':').filter(|d| !d.is_empty()).map(std::path::PathBuf::from).collect())
        .unwrap_or_default();
    dirs.push(".".into());
    dirs.push("scheme/lib".into());
    dirs
}

/// The file a library is looked for in: its name parts joined by `/`, with
/// `.sld` appended.
fn library_file(name: &LibraryName) -> String {
    format!("{}.sld", name.join("/"))
}

fn find_library_file(name: &LibraryName) -> Option<std::path::PathBuf> {
    let file = library_file(name);
    search_path().into_iter().map(|dir| dir.join(&file)).find(|path| path.is_file())
}

/// The library name an import set ultimately refers to.
fn import_set_library(heap: &GcHeap, set: GcRef) -> Option<GcRef> {
    let mut set = set;
    loop {
        let SchemeValue::Pair(head, rest) = gc_value!(set) else { return None };
        let modifier = matches!(identifier_name(heap, *head).as_str(), "only" | "except" | "prefix" | "rename");
        match gc_value!(*rest) {
            SchemeValue::Pair(inner, _) if modifier => set = *inner,
            _ => return Some(set),
        }
    }
}

/// If any library the import sets name is unregistered but has a file on
/// the search path that hasn't been loaded, arrange to load those files and
/// then evaluate `expr` again, returning true. A library whose file was
/// loaded without defining it is an error. Libraries with no file are left
/// for the caller to report as unknown.
fn load_then_retry(ec: &mut RunTime, state: &mut CEKState, sets: &[GcRef], expr: GcRef, who: &str) -> Result<bool, String> {
    let mut to_load = Vec::new();
    for set in sets {
        let Some(name_datum) = import_set_library(ec.heap, *set) else { continue };
        let Ok(name) = library_name(name_datum, who) else { continue };
        if ec.heap.libraries.get(&name).is_some() {
            continue;
        }
        let Some(path) = find_library_file(&name) else { continue };
        if ec.heap.libraries.is_loaded(&path) {
            return Err(format!("{}: {} does not define {}", who, path.display(), format_name(&name)));
        }
        if !to_load.contains(&path) {
            to_load.push(path);
        }
    }
    if to_load.is_empty() {
        return Ok(false);
    }
    // (begin (%load-library "path") ... expr)
    let load = crate::gc::new_sys_builtin(ec, "%load-library", load_library_sp, "%load-library: sys-builtin".to_string());
    let mut forms = Vec::new();
    for path in to_load {
        let path = new_string(ec.heap, &path.to_string_lossy());
        forms.push(list_from_slice(&[load, path], ec.heap));
    }
    forms.push(expr);
    let begin = ec.heap.core_form("begin");
    let body = list_from_slice(&forms, ec.heap);
    let begin_form = crate::gc::new_pair(ec.heap, begin, body);
    crate::eval::insert_eval(state, begin_form, state.tail);
    Ok(true)
}

/// (%load-library "path"): evaluate a library file's forms in the
/// interaction environment, marking it loaded first so that it is loaded at
/// most once (and a library that imports itself can't loop). Relative file
/// names in the `include` declarations of its `define-library` forms are
/// made relative to the file's directory.
fn load_library_sp(ec: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    let [path] = args else {
        return Err("%load-library: expects a file name".to_string());
    };
    let SchemeValue::Str(path) = gc_value!(*path) else {
        return Err("%load-library: expects a file name".to_string());
    };
    let path = std::path::PathBuf::from(path);
    ec.heap.libraries.mark_loaded(path.clone());
    let dir = path.parent().map(|d| d.to_path_buf()).unwrap_or_default();
    let forms = read_file_forms(ec.heap, &path.to_string_lossy(), false, "import")?;
    let mut rewritten = Vec::with_capacity(forms.len());
    for form in forms {
        rewritten.push(relocate_includes(ec.heap, form, &dir));
    }
    let env = ec.heap.interaction_env().ok_or("%load-library: no interaction environment")?;
    state.kont = std::rc::Rc::new(Kont::RestoreEnv { old_env: state.env.clone(), next });
    state.env = env;
    state.tail = false;
    if rewritten.is_empty() {
        state.control = Control::Value(ec.heap.unspecified());
    } else {
        let begin = ec.heap.core_form("begin");
        let body = list_from_slice(&rewritten, ec.heap);
        state.control = Control::Expr(crate::gc::new_pair(ec.heap, begin, body));
    }
    Ok(())
}

/// `form`, if it is a `define-library`, with the relative file names in its
/// include, include-ci and include-library-declarations declarations joined
/// to `dir`; otherwise `form` itself.
fn relocate_includes(heap: &mut GcHeap, form: GcRef, dir: &std::path::Path) -> GcRef {
    let Ok(parts) = crate::gc::list_to_vec(heap, form) else { return form };
    if parts.len() < 2 || identifier_name(heap, parts[0]) != "define-library" {
        return form;
    }
    let mut new_parts = parts[..2].to_vec();
    for decl in &parts[2..] {
        let decl_parts = crate::gc::list_to_vec(heap, *decl).unwrap_or_default();
        let is_include = decl_parts.first().is_some_and(|k| {
            matches!(identifier_name(heap, *k).as_str(), "include" | "include-ci" | "include-library-declarations")
        });
        if !is_include {
            new_parts.push(*decl);
            continue;
        }
        let mut relocated = vec![decl_parts[0]];
        for file in &decl_parts[1..] {
            match gc_value!(*file) {
                SchemeValue::Str(name) if std::path::Path::new(name).is_relative() => {
                    let joined = dir.join(name).to_string_lossy().into_owned();
                    relocated.push(new_string(heap, &joined));
                }
                _ => relocated.push(*file),
            }
        }
        new_parts.push(list_from_slice(&relocated, heap));
    }
    list_from_slice(&new_parts, heap)
}

// ---------------------------------------------------------------------------
// cond-expand and features

/// s1's feature identifiers (R7RS appendix B), as strings.
fn feature_names() -> Vec<String> {
    let mut names: Vec<String> = ["r7rs", "exact-closed", "ieee-float", "full-unicode", "ratios", "s1"]
        .iter()
        .map(|f| f.to_string())
        .collect();
    names.push(std::env::consts::OS.to_string());
    names.push(std::env::consts::FAMILY.to_string());
    if std::env::consts::FAMILY == "unix" {
        names.push("posix".to_string());
    }
    // R7RS spells architectures with hyphens: x86-64, not x86_64.
    names.push(std::env::consts::ARCH.replace('_', "-"));
    names.push(if cfg!(target_endian = "little") { "little-endian" } else { "big-endian" }.to_string());
    names
}

/// (features)
fn features(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if !args.is_empty() {
        return Err("features: takes no arguments".to_string());
    }
    let symbols: Vec<GcRef> = feature_names().iter().map(|f| heap.intern_symbol(f)).collect();
    Ok(list_from_slice(&symbols, heap))
}

/// Whether a cond-expand feature requirement holds: a feature identifier,
/// (library name) for a library that is registered or has a file on the
/// search path, or and / or / not of requirements.
fn requirement_holds(heap: &GcHeap, req: GcRef, features: &[String], who: &str) -> Result<bool, String> {
    let bad = || format!("{}: bad feature requirement {}", who, crate::printer::print_value(&req));
    match gc_value!(req) {
        SchemeValue::Symbol(_) => Ok(features.contains(&identifier_name(heap, req))),
        SchemeValue::Pair(head, _) => {
            let parts = crate::gc::list_to_vec(heap, req).map_err(|_| bad())?;
            let args = &parts[1..];
            match identifier_name(heap, *head).as_str() {
                "and" => {
                    for r in args {
                        if !requirement_holds(heap, *r, features, who)? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                }
                "or" => {
                    for r in args {
                        if requirement_holds(heap, *r, features, who)? {
                            return Ok(true);
                        }
                    }
                    Ok(false)
                }
                "not" => match args {
                    [r] => Ok(!requirement_holds(heap, *r, features, who)?),
                    _ => Err(bad()),
                },
                "library" => match args {
                    [name] => {
                        let name = library_name(*name, who)?;
                        Ok(heap.libraries.get(&name).is_some() || find_library_file(&name).is_some())
                    }
                    _ => Err(bad()),
                },
                _ => Err(bad()),
            }
        }
        _ => Err(bad()),
    }
}

/// The body of the first cond-expand clause whose requirement holds (an
/// `else` clause, last, always does), or nothing if none does.
fn choose_cond_expand_clause(heap: &GcHeap, clauses: &[GcRef], who: &str) -> Result<Vec<GcRef>, String> {
    let features = feature_names();
    for (i, clause) in clauses.iter().enumerate() {
        let parts = crate::gc::list_to_vec(heap, *clause).unwrap_or_default();
        let Some(req) = parts.first() else {
            return Err(format!("{}: bad clause {}", who, crate::printer::print_value(clause)));
        };
        let holds = if identifier_name(heap, *req) == "else" {
            if i + 1 != clauses.len() {
                return Err(format!("{}: else must be the last clause", who));
            }
            true
        } else {
            requirement_holds(heap, *req, &features, who)?
        };
        if holds {
            return Ok(parts[1..].to_vec());
        }
    }
    Ok(Vec::new())
}

/// (cond-expand (requirement form ...) ... [(else form ...)])
/// The forms of the first clause whose requirement holds, as a `begin` in
/// place of the cond-expand; unspecified if no clause holds.
pub fn cond_expand_sf(expr: GcRef, ec: &mut RunTime, state: &mut CEKState) -> Result<(), String> {
    let clauses = crate::gc::list_to_vec(ec.heap, crate::gc::cdr(expr)?)?;
    let forms = choose_cond_expand_clause(ec.heap, &clauses, "cond-expand")?;
    if forms.is_empty() {
        crate::eval::insert_value(state, ec.heap.unspecified());
        return Ok(());
    }
    let begin = ec.heap.core_form("begin");
    let body = list_from_slice(&forms, ec.heap);
    let begin_form = crate::gc::new_pair(ec.heap, begin, body);
    crate::eval::insert_eval(state, begin_form, state.tail);
    Ok(())
}
