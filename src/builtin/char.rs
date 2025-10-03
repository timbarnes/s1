use crate::env::{EnvOps, EnvRef};
use crate::gc::{GcHeap, GcRef, SchemeValue, new_bool, new_char, new_int};
use crate::register_builtin_family;
use num_bigint::BigInt;

pub fn register_char_builtins(heap: &mut GcHeap, env: EnvRef) {
    register_builtin_family!(heap, env,
        "char=?" => char_eq,
        "char<?" => char_lt,
        "char>?" => char_gt,
        "char->integer" => char_to_integer,
        "integer->char" => integer_to_char,
        "char-upcase" => char_upcase,
        "char-downcase" => char_downcase,
    );
}

fn get_char(heap: &mut GcHeap, val: GcRef) -> Result<char, String> {
    match heap.get_value(val) {
        SchemeValue::Char(c) => Ok(*c),
        _ => Err("Expected a character".to_string()),
    }
}

fn get_integer(heap: &mut GcHeap, val: GcRef) -> Result<i64, String> {
    match heap.get_value(val) {
        SchemeValue::Int(i) => Ok(i.to_string().parse().unwrap()),
        _ => Err("Expected an integer".to_string()),
    }
}

fn char_upcase(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("char-upcase: expected exactly 1 argument".to_string());
    }
    let c = get_char(heap, args[0])?;
    Ok(new_char(heap, c.to_ascii_uppercase()))
}

fn char_downcase(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("char-downcase: expected exactly 1 argument".to_string());
    }
    let c = get_char(heap, args[0])?;
    Ok(new_char(heap, c.to_ascii_lowercase()))
}


fn char_eq(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("char=?: expected exactly 2 arguments".to_string());
    }
    let c1 = get_char(heap, args[0])?;
    let c2 = get_char(heap, args[1])?;
    Ok(new_bool(heap, c1 == c2))
}

fn char_lt(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("char<?: expected exactly 2 arguments".to_string());
    }
    let c1 = get_char(heap, args[0])?;
    let c2 = get_char(heap, args[1])?;
    Ok(new_bool(heap, c1 < c2))
}

fn char_gt(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("char>?: expected exactly 2 arguments".to_string());
    }
    let c1 = get_char(heap, args[0])?;
    let c2 = get_char(heap, args[1])?;
    Ok(new_bool(heap, c1 > c2))
}

fn char_to_integer(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("char->integer: expected exactly 1 argument".to_string());
    }
    let c = get_char(heap, args[0])?;
    Ok(new_int(heap, BigInt::from(c as u32)))
}

fn integer_to_char(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("integer->char: expected exactly 1 argument".to_string());
    }
    let i = get_integer(heap, args[0])?;
    if let Some(c) = std::char::from_u32(i as u32) {
        Ok(new_char(heap, c))
    } else {
        Err("integer->char: invalid character code".to_string())
    }
}
