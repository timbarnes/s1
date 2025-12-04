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
use crate::gc_value;
use crate::register_builtin_family;
use num_traits::ToPrimitive;

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
        "shell" => shell,
    );
}

// ============================================================================
// BUILTIN FUNCTIONS
// ============================================================================

/// (system cmd)
/// Executes the given command in a subprocess and returns the output as a string.
fn shell(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("system: expected 1 argument".to_string());
    }
    let cmd_string = match gc_value!(args[0]) {
        SchemeValue::Str(s) => s,
        _ => return Err("system: expected a string argument".to_string()),
    };

    let result = crate::utilities::run_command(cmd_string);
    match result {
        Ok(output) => Ok(new_string(heap, output.as_str())),
        Err(e) => Err(e.to_string()),
    }
}

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::{RunTime, RunTimeStruct};
    use crate::gc::{new_bool, new_int, new_string};
    use num_bigint::BigInt;

    #[test]
    fn test_void_builtin() {
        let mut ev = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut ev);
        let heap = &mut ec.heap;

        let result = void(heap, &[]).unwrap();
        assert!(matches!(&heap.get_value(result), SchemeValue::Void));

        let arg = new_int(heap, BigInt::from(1));
        let result = void(heap, &[arg]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "void: expected 0 arguments");
    }

    #[test]
    fn test_help_builtin() {
        let mut ev = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut ev);
        let heap = &mut ec.heap;

        let sym = heap.intern_symbol("my-symbol");
        let result = help(heap, &[sym]).unwrap();
        assert!(matches!(&heap.get_value(result), SchemeValue::Str(_)));
        if let SchemeValue::Str(s) = heap.get_value(result) {
            assert_eq!(s, "Help for my-symbol: ...");
        }

        let result = help(heap, &[]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "help: expected 1 argument");

        let arg = new_int(heap, BigInt::from(1));
        let result = help(heap, &[arg]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "help: argument must be a symbol");
    }

    #[test]
    fn test_gc_threshold_builtin() {
        let mut ev = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut ev);
        let heap = &mut ec.heap;

        // Get current threshold
        let initial_threshold_val = gc_threshold(heap, &[]).unwrap();
        if let SchemeValue::Int(i) = heap.get_value(initial_threshold_val) {
            assert_eq!(*i, BigInt::from(100000)); // Default threshold from GcHeap::new()
        } else {
            panic!("Expected integer");
        }

        // Set new threshold
        let new_threshold_arg = new_int(heap, BigInt::from(200000));
        let result_set_threshold = gc_threshold(heap, &[new_threshold_arg]).unwrap();
        if let SchemeValue::Int(i) = heap.get_value(result_set_threshold) {
            assert_eq!(*i, BigInt::from(200000));
        } else {
            panic!("Expected integer");
        }
        assert_eq!(heap.threshold, 200000);

        // Error with non-integer
        let non_int_arg = new_bool(heap, true);
        let result_non_int = gc_threshold(heap, &[non_int_arg]);
        assert!(result_non_int.is_err());
        assert_eq!(result_non_int.unwrap_err(), "threshold must be an integer");

        // Error with too many arguments
        let arg1 = new_int(heap, BigInt::from(1));
        let arg2 = new_int(heap, BigInt::from(2));
        let result_too_many = gc_threshold(heap, &[arg1, arg2]);
        assert!(result_too_many.is_err());
        assert_eq!(result_too_many.unwrap_err(), "gc-threshold: expected 0 or 1 arguments");
    }

    #[test]
    fn test_shell_builtin() {
        let mut ev = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut ev);
        let heap = &mut ec.heap;

        // Test with a simple command
        let cmd_arg = new_string(heap, "echo hello");
        let result = shell(heap, &[cmd_arg]).unwrap();
        assert!(matches!(&heap.get_value(result), SchemeValue::Str(_)));
        if let SchemeValue::Str(s) = heap.get_value(result) {
            // Output might include a newline depending on the OS
            assert!(s.trim() == "hello");
        }

        // Test with no arguments
        let result = shell(heap, &[]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "system: expected 1 argument");

        // Test with non-string argument
        let non_string_arg = new_int(heap, BigInt::from(1));
        let result = shell(heap, &[non_string_arg]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "system: expected a string argument");
    }
}
