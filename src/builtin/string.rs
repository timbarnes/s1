// String builtin functions
//
//
use crate::eval::EvalContext;
use crate::gc::{GcHeap, GcRef, new_string};
use crate::printer::print_scheme_value;

fn to_string(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 1 {
        let result = new_string(ec.heap, print_scheme_value(ec, &args[0]).as_str());
        Ok(result)
    } else {
        Err("to-string expects exactly one argument".to_string())
    }
}

/// (string-upcase string) TODO
fn string_upcase(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 1 {
        let result = new_string(ec.heap, print_scheme_value(ec, &args[0]).as_str());
        Ok(result)
    } else {
        Err("to-string expects exactly one argument".to_string())
    }
}

/// (string-downcase string) TODO
fn string_downcase(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 1 {
        let result = new_string(ec.heap, print_scheme_value(ec, &args[0]).as_str());
        Ok(result)
    } else {
        Err("to-string expects exactly one argument".to_string())
    }
}

/// (substring string start end) TODO
fn substring(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 1 {
        let result = new_string(ec.heap, print_scheme_value(ec, &args[0]).as_str());
        Ok(result)
    } else {
        Err("to-string expects exactly one argument".to_string())
    }
}

/// (string-append string1 string2 ...) TODO
fn string_append(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 1 {
        let result = new_string(ec.heap, print_scheme_value(ec, &args[0]).as_str());
        Ok(result)
    } else {
        Err("to-string expects exactly one argument".to_string())
    }
}

/////////////////////////////////////////////////
/// Builtin registration for string functions
///
macro_rules! register_builtin_family {
    ($heap:expr, $env:expr, $($name:expr => $func:expr),* $(,)?) => {
        $(
            $env.set_symbol($heap.intern_symbol($name),
                crate::gc::new_primitive($heap, $func,
                    concat!($name, ": builtin function").to_string()));
        )*
    };
}

pub fn register_string_builtins(heap: &mut GcHeap, env: &mut crate::env::Environment) {
    register_builtin_family!(heap, env,
        ">string" => to_string,
    );
}
