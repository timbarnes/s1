use crate::env::{EnvOps, EnvRef};
use crate::gc::{GcHeap, GcRef, SchemeValue, new_bool, new_char, new_int};
use crate::register_builtin_family;
use num_bigint::BigInt;

pub fn register_char_builtins(heap: &mut GcHeap, env: EnvRef) {
    register_builtin_family!(heap, env,
        "char=?" => (char_eq, "(char=? char1 char2) Compare two characters for equality"),
        "char<?" => (char_lt, "(char<? char1 char2) Compare two characters for less than"),
        "char>?" => (char_gt, "(char>? char1 char2) Compare two characters for greater than"),
        "char=?=" => (char_eq, "(char=? char1 char2) Compare two characters for equality"),
        "char->integer" => (char_to_integer, "(char->integer char) Convert a character to an integer"),
        "integer->char" => (integer_to_char, "(integer->char integer) Convert an integer to a character"),
        "char-upcase" => (char_upcase, "(char-upcase char) Convert a character to uppercase"),
        "char-downcase" => (char_downcase, "(char-downcase char) Convert a character to lowercase"),
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

mod tests {
    #[allow(unused_imports)]
    use super::*;
    #[allow(unused_imports)]
    use crate::eval::{RunTime, RunTimeStruct};
    #[allow(unused_imports)]
    use crate::gc_value;

    #[test]
    fn test_char_eq() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);
        let args = vec![new_char(ec.heap, 'a'), new_char(ec.heap, 'a')];
        let result = char_eq(&mut ec.heap, &args).unwrap();
        assert!(matches!(&gc_value!(result), SchemeValue::Bool(true)));

        let args = vec![new_char(ec.heap, 'a'), new_char(ec.heap, 'b')];
        let result = char_eq(&mut ec.heap, &args).unwrap();
        assert!(matches!(&gc_value!(result), SchemeValue::Bool(false)));
    }

    #[test]
    fn test_char_lt() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);
        let args = vec![new_char(ec.heap, 'a'), new_char(ec.heap, 'b')];
        let result = char_lt(&mut ec.heap, &args).unwrap();
        assert!(matches!(&gc_value!(result), SchemeValue::Bool(true)));

        let args = vec![new_char(ec.heap, 'b'), new_char(ec.heap, 'a')];
        let result = char_lt(&mut ec.heap, &args).unwrap();
        assert!(matches!(&gc_value!(result), SchemeValue::Bool(false)));

        let args = vec![new_char(ec.heap, 'a'), new_char(ec.heap, 'a')];
        let result = char_lt(&mut ec.heap, &args).unwrap();
        assert!(matches!(&gc_value!(result), SchemeValue::Bool(false)));
    }

    #[test]
    fn test_char_gt() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);
        let args = vec![new_char(ec.heap, 'b'), new_char(ec.heap, 'a')];
        let result = char_gt(&mut ec.heap, &args).unwrap();
        assert!(matches!(&gc_value!(result), SchemeValue::Bool(true)));

        let args = vec![new_char(ec.heap, 'a'), new_char(ec.heap, 'b')];
        let result = char_gt(&mut ec.heap, &args).unwrap();
        assert!(matches!(&gc_value!(result), SchemeValue::Bool(false)));

        let args = vec![new_char(ec.heap, 'a'), new_char(ec.heap, 'a')];
        let result = char_gt(&mut ec.heap, &args).unwrap();
        assert!(matches!(&gc_value!(result), SchemeValue::Bool(false)));
    }

    #[test]
    fn test_char_to_integer() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);
        let args = vec![new_char(ec.heap, 'a')];
        let result = char_to_integer(&mut ec.heap, &args).unwrap();
        match &gc_value!(result) {
            SchemeValue::Int(i) => assert_eq!(*i, BigInt::from(97)),
            _ => panic!("Expected integer"),
        }
    }

    #[test]
    fn test_integer_to_char() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);
        let args = vec![new_int(ec.heap, BigInt::from(97))];
        let result = integer_to_char(&mut ec.heap, &args).unwrap();
        match &gc_value!(result) {
            SchemeValue::Char(c) => assert_eq!(*c, 'a'),
            _ => panic!("Expected char"),
        }
    }

    #[test]
    fn test_integer_to_char_invalid() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);
        let args = vec![new_int(ec.heap, BigInt::from(0x110000))];
        let result = integer_to_char(&mut ec.heap, &args);
        assert!(result.is_err());
    }
}
