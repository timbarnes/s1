pub mod char;
pub mod display;
pub mod fileio;
pub mod list;
pub mod number;
pub mod predicate;
pub mod string;
pub mod vector;

use crate::env::{EnvOps, EnvRef};
use crate::gc::{GcHeap, GcRef, SchemeValue, new_int, new_string};
use num_traits::ToPrimitive;

/// Macro to register builtin functions in the environment
///
/// Usage: register_builtin!(heap, env,
///     "name" => function,
///     "another" => another_function,
/// );
macro_rules! register_builtin_family {
    ($heap:expr, $env:expr, $($name:expr => $func:expr),* $(,)?) => {
        $(
            $env.define($heap.intern_symbol($name),
                crate::gc::new_builtin($heap, $func,
                    concat!($name, ": builtin function").to_string()));
        )*
    };
}

// ============================================================================
// BUILTIN FUNCTIONS
// ============================================================================

/// (void)
/// Returns the void value.
fn void(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if !args.is_empty() {
        return Err("void: expected 0 arguments".to_string());
    }

    Ok(heap.void())
}

/// Builtin function: (exit)
///
/// Exits the Scheme interpreter with exit code 0.
/// This works in both programs and the REPL.
fn exit(_heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if !args.is_empty() {
        return Err("exit: expected 0 arguments".to_string());
    }
    // Exit the system cleanly
    std::process::exit(0);
}

// (help 'symbol): returns the doc string for the given symbol as a Scheme string
fn help(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if let Some(arg) = args.get(0) {
        if let SchemeValue::Symbol(sym) = &heap.get_value(*arg) {
            // In a real implementation, you would have access to the environment here.
            // For now, return a placeholder string.
            Ok(new_string(heap, &format!("Help for {}: ...", sym)))
        } else {
            Err("help: argument must be a symbol".to_string())
        }
    } else {
        Err("help: expected 1 argument".to_string())
    }
}

fn gc_threshold(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    match args.len() {
        0 => Ok(new_int(heap, num_bigint::BigInt::from(heap.threshold))),
        1 => {
            let new_threshold = match heap.get_value(args[0]) {
                SchemeValue::Int(i) => {
                    i.to_usize().ok_or("threshold must be a positive integer")?
                }
                _ => return Err("threshold must be an integer".to_string()),
            };
            heap.threshold = new_threshold;
            Ok(new_int(heap, num_bigint::BigInt::from(new_threshold)))
        }
        _ => Err("gc-threshold: expected 0 or 1 arguments".to_string()),
    }
}

/// Register all builtin functions in the environment
pub fn register_builtins(heap: &mut GcHeap, env: EnvRef) {
    char::register_char_builtins(heap, env.clone());
    display::register_display_builtins(heap, env.clone());
    list::register_list_builtins(heap, env.clone());
    fileio::register_fileio_builtins(heap, env.clone());
    number::register_number_builtins(heap, env.clone());
    predicate::register_predicate_builtins(heap, env.clone());
    string::register_string_builtins(heap, env.clone());
    vector::register_vector_builtins(heap, env.clone());
    register_builtin_family!(heap, env.clone(),
        "help" => help,
        "exit" => exit,
        "void" => void,
        "gc-threshold" => gc_threshold,
    );
}
