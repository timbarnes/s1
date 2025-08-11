// String builtin functions
//
//
use crate::eval::EvalContext;
use crate::gc::{GcHeap, GcRef, SchemeValue, get_integer, get_string, new_int, new_string};
use crate::printer::print_scheme_value;
use num_bigint::BigInt;

fn to_string(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 1 {
        let result = new_string(ec.heap, print_scheme_value(ec, &args[0]).as_str());
        Ok(result)
    } else {
        Err("to-string expects exactly one argument".to_string())
    }
}

/// (string-upcase string)
fn string_upcase(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 1 {
        let string_val = get_string(ec.heap, args[0])?;
        let result = string_val.to_uppercase();
        return Ok(new_string(ec.heap, result.as_str()));
    } else {
        Err("string-downcase expects exactly one argument".to_string())
    }
}

/// (string-downcase string)
fn string_downcase(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 1 {
        let string_val = get_string(ec.heap, args[0])?;
        let result = string_val.to_lowercase();
        return Ok(new_string(ec.heap, result.as_str()));
    } else {
        Err("string-downcase expects exactly one argument".to_string())
    }
}

/// (substring string start end)
fn substring(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 3 {
        let start = get_integer(ec.heap, args[1]).unwrap() as usize;
        let end = get_integer(ec.heap, args[2]).unwrap() as usize;
        let string_val = get_string(ec.heap, args[0])?;
        let result = string_val[start..end].to_string();
        Ok(new_string(ec.heap, result.as_str()))
    } else {
        Err("to-string expects string, start, end arguments".to_string())
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

/// (string-copy string [start [end]]) TODO
fn string_copy(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    let result;
    match args.len() {
        1 => {
            let s = get_string(ec.heap, args[0]).unwrap();
            result = new_string(ec.heap, s.as_str());
        }
        2 => {
            let s = get_string(ec.heap, args[0]).unwrap();
            let start = get_integer(ec.heap, args[1]).unwrap() as usize;
            result = new_string(ec.heap, &s.as_str()[start..]);
        }
        3 => {
            let s = get_string(ec.heap, args[0]).unwrap();
            let start = get_integer(ec.heap, args[1]).unwrap() as usize;
            let end = get_integer(ec.heap, args[2]).unwrap() as usize;
            result = new_string(ec.heap, &s[start..end]);
        }
        _ => return Err("string-copy expects 1 to 3 arguments".to_string()),
    }
    Ok(result)
}

/// (string-copy string [start [end]]) TODO
fn string_length(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 1 {
        let arg = get_string(ec.heap, args[0])?;
        let result = new_int(ec.heap, BigInt::from(arg.len()));
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
        "string-upcase" => string_upcase,
        "string-downcase" => string_downcase,
        "substring" => substring,
        "string-copy" => string_copy,
        "string-append" => string_append,
        "string-length" => string_length,
    );
}
