/// Display output functions
///
///
use crate::eval::EvalContext;
use crate::gc::{GcHeap, GcRef, get_nil};
use crate::printer::{display_value, print_value};

/// write a SchemeValue in Scheme-readable format
///
pub fn write(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 1 || args.len() > 2 {
        return Err("display: expected 1 or 2 arguments".to_string());
    }

    // Extract the SchemeValue reference in its own block to shorten the borrow
    let s = print_value(&args[0]);

    if args.len() == 2 {
        print!("{}", s);
        use std::io::Write;
        std::io::stdout().flush().unwrap();
    } else {
        print!("{}", s);
        use std::io::Write;
        std::io::stdout().flush().unwrap();
    }

    Ok(ec.heap.nil_s())
}

/// print a SchemeValue in human-readable format
///
pub fn display(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 1 || args.len() > 2 {
        return Err("display: expected 1 or 2 arguments".to_string());
    }

    // Extract the SchemeValue reference in its own block to shorten the borrow
    let s = display_value(&args[0]);

    if args.len() == 2 {
        print!("{}", s);
        use std::io::Write;
        std::io::stdout().flush().unwrap();
    } else {
        print!("{}", s);
        use std::io::Write;
        std::io::stdout().flush().unwrap();
    }

    Ok(ec.heap.nil_s())
}

pub fn newline(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() > 1 {
        return Err("newline: expected 0 or 1 arguments".to_string());
    }

    // For now, just print a newline to stdout
    // TODO: In a full implementation, we'd write to the specified port
    println!();

    // Return undefined (we'll use nil for now)
    Ok(get_nil(ec.heap))
}

macro_rules! register_builtin_family {
    ($heap:expr, $env:expr, $($name:expr => $func:expr),* $(,)?) => {
        $(
            $env.set_symbol($heap.intern_symbol($name),
                crate::gc::new_primitive($heap, $func,
                    concat!($name, ": builtin function").to_string()));
        )*
    };
}

pub fn register_display_builtins(heap: &mut GcHeap, env: &mut crate::env::Environment) {
    register_builtin_family!(heap, env,
        "write" => write,
        "display" => display,
        "newline" => newline,
    );
}
