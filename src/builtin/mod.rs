pub mod fileio;
pub mod list;
pub mod number;
pub mod predicate;

use crate::gc::{GcHeap, GcRef, SchemeValue, new_bool, new_string};
// use num_bigint::BigInt;
//  use crate::gc::SchemeValue;
use fileio::open_input_file_builtin;
use list::{append_builtin, car_builtin, cdr_builtin, cons_builtin, list_builtin};
use number::{
    div_builtin, eq_builtin, gt_builtin, lt_builtin, minus_builtin, mod_builtin, plus_builtin,
    times_builtin,
};
// use crate::printer::scheme_display;
use crate::printer::print_scheme_value;

/// Macro to register builtin functions in the environment
///
/// Usage: register_builtin!(heap, env,
///     "name" => function,
///     "another" => another_function,
/// );
macro_rules! register_builtin {
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

pub fn display_builtin(_heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 1 || args.len() > 2 {
        return Err("display: expected 1 or 2 arguments".to_string());
    }
    let val = &args[0];
    use crate::gc::SchemeValue;
    let s = match &val.value {
        SchemeValue::Str(s) => s.clone(),    // Print raw string, no quotes
        _ => print_scheme_value(&val.value), // Use pretty-printer for other types
    };
    // If a port is provided as the second argument, write to it
    if args.len() == 2 {
        // For now, just write to stdout
        print!("{}", s);
        use std::io::Write;
        std::io::stdout().flush().unwrap();
    } else {
        // Default behavior: write to stdout
        print!("{}", s);
        use std::io::Write;
        std::io::stdout().flush().unwrap();
    }
    Ok(*val)
}

pub fn newline_builtin(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() > 1 {
        return Err("newline: expected 0 or 1 arguments".to_string());
    }

    // For now, just print a newline to stdout
    // In a full implementation, we'd write to the specified port
    println!();

    // Return undefined (we'll use #f for now)
    Ok(new_bool(heap, false))
}

/// Builtin function: (quit)
///
/// Exits the Scheme interpreter with exit code 0.
/// This works in both programs and the REPL.
pub fn quit_builtin(_heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if !args.is_empty() {
        return Err("quit: expected 0 arguments".to_string());
    }

    // Exit the system cleanly
    std::process::exit(0);
}

// (help 'symbol): returns the doc string for the given symbol as a Scheme string
pub fn help_builtin(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if let Some(arg) = args.get(0) {
        if let SchemeValue::Symbol(sym) = &arg.value {
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

/// Builtin function: (load filename) - evaluator-aware version
///
/// Loads and evaluates a Scheme file using the evaluator's port stack.
pub fn load_builtin_evaluator(
    evaluator: &mut crate::eval::Evaluator,
    args: &[GcRef],
) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("load: expected exactly 1 argument".to_string());
    }

    let filename = match &args[0].value {
        crate::gc::SchemeValue::Str(filename) => filename.clone(),
        _ => return Err("load: argument must be a string".to_string()),
    };

    // For now, just return a success message
    // TODO: Implement actual file loading with proper port stack integration
    Ok(crate::gc::new_string(
        evaluator.heap_mut(),
        &format!("Loaded file: {}", filename),
    ))
}

/// Register all builtin functions in the environment
pub fn register_builtins(heap: &mut GcHeap, env: &mut crate::env::Environment) {
    // Register builtins using the macro for cleaner syntax
    register_builtin!(heap, env,
        "number?" => predicate::number_q,
        "eq?" => predicate::eq_q,
        "help" => help_builtin,
        "type-of" => predicate::type_of,
        "+" => plus_builtin,
        "-" => minus_builtin,
        "*" => times_builtin,
        "/" => div_builtin,
        "mod" => mod_builtin,
        "=" => eq_builtin,
        "<" => lt_builtin,
        ">" => gt_builtin,
        "car" => car_builtin,
        "cdr" => cdr_builtin,
        "cons" => cons_builtin,
        "list" => list_builtin,
        "append" => append_builtin,
        "display" => display_builtin,
        "newline" => newline_builtin,
        "quit" => quit_builtin,
        "open-input-file" => open_input_file_builtin,
    );
}
