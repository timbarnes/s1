pub mod debug;
pub mod display;
pub mod fileio;
pub mod list;
pub mod number;
pub mod predicate;
pub mod string;
pub mod vector;

use crate::env::Environment;
use crate::eval::EvalContext;
use crate::gc::{GcHeap, GcRef, SchemeValue, new_string};

/// Macro to register builtin functions in the environment
///
/// Usage: register_builtin!(heap, env,
///     "name" => function,
///     "another" => another_function,
/// );
macro_rules! register_builtin_family {
    ($heap:expr, $env:expr, $($name:expr => $func:expr),* $(,)?) => {
        $(
            $env.set_symbol($heap.intern_symbol($name),
                crate::gc::new_primitive($heap, $func,
                    concat!($name, ": builtin function").to_string()));
        )*
    };
}

// ============================================================================
// BUILTIN FUNCTIONS
// ============================================================================

/// (void)
/// Returns the void value.
fn void(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if !args.is_empty() {
        return Err("void: expected 0 arguments".to_string());
    }

    Ok(ec.heap.void())
}

/// Builtin function: (exit)
///
/// Exits the Scheme interpreter with exit code 0.
/// This works in both programs and the REPL.
fn exit(_ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if !args.is_empty() {
        return Err("exit: expected 0 arguments".to_string());
    }

    // Exit the system cleanly
    std::process::exit(0);
}

// (help 'symbol): returns the doc string for the given symbol as a Scheme string
fn help(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if let Some(arg) = args.get(0) {
        if let SchemeValue::Symbol(sym) = &ec.heap.get_value(*arg) {
            // In a real implementation, you would have access to the environment here.
            // For now, return a placeholder string.
            Ok(new_string(ec.heap, &format!("Help for {}: ...", sym)))
        } else {
            Err("help: argument must be a symbol".to_string())
        }
    } else {
        Err("help: expected 1 argument".to_string())
    }
}

/// Builtin function: (load filename)
///
/// Loads and evaluates a Scheme file.
// pub fn load_builtin(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
//     if args.len() != 1 {
//         return Err("load: expected exactly 1 argument".to_string());
//     }
//     let filename = match &args[0].value {
//         SchemeValue::Str(filename) => filename.clone(),
//         _ => return Err("load: argument must be a string".to_string()),
//     };
//     // Read the file content
//     let content = match std::fs::read_to_string(&filename) {
//         Ok(content) => content,
//         Err(e) => return Err(format!("load: could not read file '{}': {}", filename, e)),
//     };
//     // Create a new port for the file
//     let file_port = crate::gc::new_port_simple(heap, crate::io::PortKind::StringPortInput {
//         content,
//         pos: 0,
//     });
//     // Get the symbol for **port-stack**
//     let port_stack_sym = heap.intern_symbol("**port-stack**");
//     // Get the current port stack from the environment
//     let env = heap.get_env_mut(); // This assumes you have a way to get the current environment from the heap or context
//     let current_stack = env.get_symbol(port_stack_sym).unwrap_or_else(|| heap.nil_simple());
//     // Push the new port onto the stack
//     let new_stack = crate::gc::new_pair_simple(heap, file_port, current_stack);
//     env.set_symbol(port_stack_sym, new_stack);
//     Ok(new_string_simple(heap, &format!("Loaded file: {}", filename)))
// }

/// Register all builtin functions in the environment
pub fn register_builtins(heap: &mut GcHeap, env: &mut Environment) {
    debug::register_debug_builtins(heap, env);
    display::register_display_builtins(heap, env);
    list::register_list_builtins(heap, env);
    fileio::register_fileio_builtins(heap, env);
    number::register_number_builtins(heap, env);
    predicate::register_predicate_builtins(heap, env);
    string::register_string_builtins(heap, env);
    vector::register_vector_builtins(heap, env);
    register_builtin_family!(heap, env,
        "help" => help,
        "exit" => exit,
        "void" => void,
    );
}
