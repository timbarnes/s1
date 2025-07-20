pub mod number;
pub mod predicate;

use crate::gc::{GcHeap, GcRefSimple, new_string_simple, new_bool_simple, SchemeValueSimple};
use std::rc::Rc;
// use num_bigint::BigInt;
//  use crate::gc::SchemeValue;
use number::{plus_builtin, minus_builtin, times_builtin, div_builtin, mod_builtin};
// use crate::printer::scheme_display;

// ============================================================================
// BUILTIN FUNCTIONS
// ============================================================================

pub fn display_builtin_simple(_heap: &mut GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if args.len() < 1 || args.len() > 2 {
        return Err("display: expected 1 or 2 arguments".to_string());
    }
    
    let val = &args[0];
    // let s = scheme_display(&val.value);
    let s = format!("{:?}", &val.value);
    
    // If a port is provided as the second argument, write to it
    if args.len() == 2 {
        let _port_arg = &args[1];
        // For now, we'll just write to stdout since we don't have port objects in GC yet
        // In a full implementation, we'd check if port_arg is a port and write to it
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

pub fn newline_builtin_simple(heap: &mut GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if args.len() > 1 {
        return Err("newline: expected 0 or 1 arguments".to_string());
    }
    
    // For now, just print a newline to stdout
    // In a full implementation, we'd write to the specified port
    println!();
    
    // Return undefined (we'll use #f for now)
    Ok(new_bool_simple(heap, false))
}

/// Builtin function: (quit)
/// 
/// Exits the Scheme interpreter with exit code 0.
/// This works in both programs and the REPL.
pub fn quit_builtin_simple(_heap: &mut GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if !args.is_empty() {
        return Err("quit: expected 0 arguments".to_string());
    }
    
    // Exit the system cleanly
    std::process::exit(0);
}

// (help 'symbol): returns the doc string for the given symbol as a Scheme string
pub fn help_builtin_simple(heap: &mut GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if let Some(arg) = args.get(0) {
        if let SchemeValueSimple::Symbol(sym) = &arg.value {
            // In a real implementation, you would have access to the environment here.
            // For now, return a placeholder string.
            Ok(new_string_simple(heap, &format!("Help for {}: ...", sym)))
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
pub fn load_builtin_simple(heap: &mut GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if args.len() != 1 {
        return Err("load: expected exactly 1 argument".to_string());
    }
    
    let filename = match &args[0].value {
        SchemeValueSimple::Str(filename) => {
            filename.clone()
        }
        _ => return Err("load: argument must be a string".to_string()),
    };
    
    // For now, just return a placeholder
    // TODO: Implement actual file loading with evaluator's port stack
    Ok(new_string_simple(heap, &format!("Loaded file: {}", filename)))
}

/// Builtin function: (load filename) - evaluator-aware version
/// 
/// Loads and evaluates a Scheme file using the evaluator's port stack.
pub fn load_builtin_evaluator(evaluator: &mut crate::evalsimple::Evaluator, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if args.len() != 1 {
        return Err("load: expected exactly 1 argument".to_string());
    }
    
    let filename = match &args[0].value {
        crate::gc::SchemeValueSimple::Str(filename) => {
            filename.clone()
        }
        _ => return Err("load: argument must be a string".to_string()),
    };
    
    // For now, just return a success message
    // TODO: Implement actual file loading with proper port stack integration
    Ok(crate::gc::new_string_simple(evaluator.heap_mut(), &format!("Loaded file: {}", filename)))
}



pub fn register_all_simple_frames(heap: &mut GcHeap, env: &mut crate::env::Environment) {
    // Register builtins in the environment frame using symbol keys
    env.set_symbol(heap.intern_symbol("number?"), crate::gc::new_primitive_simple(heap, Rc::new(predicate::number_q), "number?: returns #t if argument is a number".to_string(), false));
    env.set_symbol(heap.intern_symbol("help"), crate::gc::new_primitive_simple(heap, Rc::new(help_builtin_simple), "help: returns help for a symbol".to_string(), false));
    env.set_symbol(heap.intern_symbol("type-of"), crate::gc::new_primitive_simple(heap, Rc::new(predicate::type_of), "type-of: returns the type of an object".to_string(), false));
    env.set_symbol(heap.intern_symbol("+"), crate::gc::new_primitive_simple(heap, Rc::new(plus_builtin), "+: adds numbers".to_string(), false));
    env.set_symbol(heap.intern_symbol("-"), crate::gc::new_primitive_simple(heap, Rc::new(minus_builtin), "-: subtracts numbers".to_string(), false));
    env.set_symbol(heap.intern_symbol("*"), crate::gc::new_primitive_simple(heap, Rc::new(times_builtin), "*: multiplies numbers".to_string(), false));
    env.set_symbol(heap.intern_symbol("/"), crate::gc::new_primitive_simple(heap, Rc::new(div_builtin), "/: divides numbers".to_string(), false));
    env.set_symbol(heap.intern_symbol("mod"), crate::gc::new_primitive_simple(heap, Rc::new(mod_builtin), "mod: returns remainder of division".to_string(), false));
    env.set_symbol(heap.intern_symbol("display"), crate::gc::new_primitive_simple(heap, Rc::new(display_builtin_simple), "display: displays a value".to_string(), false));
    env.set_symbol(heap.intern_symbol("newline"), crate::gc::new_primitive_simple(heap, Rc::new(newline_builtin_simple), "newline: prints a newline".to_string(), false));
    env.set_symbol(heap.intern_symbol("quit"), crate::gc::new_primitive_simple(heap, Rc::new(quit_builtin_simple), "quit: exits the interpreter".to_string(), false));
    env.set_symbol(heap.intern_symbol("load"), crate::gc::new_evaluator_primitive_simple(heap, Rc::new(load_builtin_evaluator), "load: loads a Scheme file".to_string(), false));
}

 