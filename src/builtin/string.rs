// String builtin functions
//
//
use crate::env::{EnvOps, EnvRef};
use crate::gc::{GcHeap, GcRef, get_integer, get_string, new_bool, new_char, new_int, new_string};
use crate::printer::display_value;
use num_bigint::BigInt;

/// (string char1 [char2 ..])
/// Create a string from the provided characters
// fn string(ec: &mut EvalContext, args) {

// }

/// (>string arg)
/// Convert a lisp object to a string
fn to_string(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 1 {
        let result = new_string(heap, display_value(&args[0]).as_str());
        Ok(result)
    } else {
        Err("to-string expects exactly one argument".to_string())
    }
}

/// (string-upcase string)
/// Convert a string to uppercase
fn string_upcase(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 1 {
        let string_val = get_string(heap, args[0])?;
        let result = string_val.to_uppercase();
        return Ok(new_string(heap, result.as_str()));
    } else {
        Err("string-downcase expects exactly one argument".to_string())
    }
}

/// (string-downcase string)
/// Convert a string to lowercase
fn string_downcase(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 1 {
        let string_val = get_string(heap, args[0])?;
        let result = string_val.to_lowercase();
        return Ok(new_string(heap, result.as_str()));
    } else {
        Err("string-downcase expects exactly one argument".to_string())
    }
}

/// (substring string start end)
/// Extract a substring from a string (equivalent to string-copy with the same arguments)
fn substring(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 3 {
        let start = get_integer(heap, args[1]).unwrap() as usize;
        let end = get_integer(heap, args[2]).unwrap() as usize;
        let string_val = get_string(heap, args[0])?;
        let result = string_val[start..end + 1].to_string();
        Ok(new_string(heap, result.as_str()))
    } else {
        Err("to-string expects string, start, end arguments".to_string())
    }
}

/// (string-append string1 string2 ...)
/// Create a new string by concatenating the given strings.
fn string_append(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() > 0 {
        let mut r_string = get_string(heap, args[0])?;
        for arg in args.iter().skip(1) {
            let arg_str = get_string(heap, *arg)?;
            r_string.push_str(&arg_str);
        }
        let result = new_string(heap, r_string.as_str());
        Ok(result)
    } else {
        Err("string-append expects at least one string argument".to_string())
    }
}

/// (string-copy string [start [end]])
/// Create a new string by copying all or part of the given string.
/// Start and end are optional, but if end is provided, start must also be provided.
fn string_copy(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let result;
    match args.len() {
        1 => {
            let s = get_string(heap, args[0]).unwrap();
            result = new_string(heap, s.as_str());
        }
        2 => {
            let s = get_string(heap, args[0]).unwrap();
            let start = get_integer(heap, args[1]).unwrap() as usize;
            result = new_string(heap, &s.as_str()[start..]);
        }
        3 => {
            let s = get_string(heap, args[0]).unwrap();
            let start = get_integer(heap, args[1]).unwrap() as usize;
            let end = get_integer(heap, args[2]).unwrap() as usize;
            result = new_string(heap, &s[start..end]);
        }
        _ => return Err("string-copy expects 1 to 3 arguments".to_string()),
    }
    Ok(result)
}

/// (string-length string)
/// Returns the length of the given string.
fn string_length(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 1 {
        let arg = get_string(heap, args[0])?;
        let result = new_int(heap, BigInt::from(arg.len()));
        Ok(result)
    } else {
        Err("to-string expects exactly one argument".to_string())
    }
}

/// (string-ref string k)
/// Returns the character at the given index in the string.
fn string_ref(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 2 {
        let s = get_string(heap, args[0]).unwrap();
        let k = get_integer(heap, args[1]).unwrap() as usize;
        if k < s.len() {
            let result = new_char(heap, s.chars().nth(k).unwrap());
            Ok(result)
        } else {
            Err("index out of bounds".to_string())
        }
    } else {
        Err("string-ref expects exactly two arguments".to_string())
    }
}

/// (string<? s1 s2)
fn string_less_than(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 2 {
        let s1 = get_string(heap, args[0]).unwrap();
        let s2 = get_string(heap, args[1]).unwrap();
        let result = new_bool(heap, s1 < s2);
        Ok(result)
    } else {
        Err("string<? expects exactly two arguments".to_string())
    }
}

/// (string<? s1 s2)
fn string_greater_than(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 2 {
        let s1 = get_string(heap, args[0]).unwrap();
        let s2 = get_string(heap, args[1]).unwrap();
        let result = new_bool(heap, s1 > s2);
        Ok(result)
    } else {
        Err("string>? expects exactly two arguments".to_string())
    }
}

/// (string=? s1 s2)
fn string_equal(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 2 {
        let s1 = get_string(heap, args[0]).unwrap();
        let s2 = get_string(heap, args[1]).unwrap();
        let result = new_bool(heap, s1 == s2);
        Ok(result)
    } else {
        Err("string=? expects two string arguments".to_string())
    }
}

/////////////////////////////////////////////////
/// Builtin registration for string functions
///
macro_rules! register_builtin_family {
    ($heap:expr, $env:expr, $($name:expr => $func:expr),* $(,)?) => {
        $(
            $env.define($heap.intern_symbol($name),
                crate::gc::new_builtin($heap, $func,
                    concat!($name, ": builtin function").to_string()));
        )*
    };
}

pub fn register_string_builtins(heap: &mut GcHeap, env: EnvRef) {
    register_builtin_family!(heap, env,
        ">string" => to_string,
        "string-upcase" => string_upcase,
        "string-downcase" => string_downcase,
        "substring" => substring,
        "string-copy" => string_copy,
        "string-append" => string_append,
        "string-length" => string_length,
        "string-ref" => string_ref,
        "string=?"=>string_equal,
        "string>?"=>string_greater_than,
        "string<?"=>string_less_than,
    );
}
