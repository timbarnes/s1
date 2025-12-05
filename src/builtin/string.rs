// String builtin functions
//
//
use crate::env::{EnvOps, EnvRef};
use crate::gc::{
    GcHeap, GcRef, SchemeValue, get_integer, get_string, new_bool, new_char, new_int, new_pair,
    new_string,
};
use crate::printer::display_value;
use crate::register_builtin_family;
use num_bigint::BigInt;

/// (string char1 [char2 ..])
/// Create a string from the provided characters
// fn string(ec: &mut EvalContext, args) {

// }

/////////////////////////////////////////////////
/// Builtin registration for string functions
///

pub fn register_string_builtins(heap: &mut GcHeap, env: EnvRef) {
    register_builtin_family!(heap, env,
        ">string" => (to_string, "(>string <char1> [<char2> ..]) Create a string from the provided characters"),
        "string-upcase" => (string_upcase, "(string-upcase <string>) Convert a string to uppercase"),
        "string-downcase" => (string_downcase, "(string-downcase <string>) Convert a string to lowercase"),
        "substring" => (substring, "(substring <string> <start> [<end>]) Extract a substring from the given string"),
        "string-copy" => (string_copy, "(string-copy <string>) Create a copy of the given string"),
        "string-append" => (string_append, "(string-append <string1> <string2> ..) Concatenate the provided strings"),
        "string-length" => (string_length, "(string-length <string>) Return the length of the given string"),
        "string-ref" => (string_ref, "(string-ref <string> <index>) Return the character at the given index"),
        "string=?"=>(string_equal, "(string=? <string1> <string2>) Compare two strings for equality"),
        "string>?"=>(string_greater_than, "(string>? <string1> <string2>) Compare two strings lexicographically"),
        "string<?"=>(string_less_than, "(string<? <string1> <string2>) Compare two strings lexicographically"),
        "make-string" => (make_string, "(make-string <length> [<fill-char>]) Create a string of the given length"),
        "string-set!" => (string_set, "(string-set! <string> <index> <char>) Set the character at the given index"),
        "string->list" => (string_to_list, "(string->list <string>) Convert a string to a list of characters"),
        "string" => (string, "(string <char1> [<char2> ..]) Create a string from the provided characters"),
        "string<=?" => (string_less_than_equal, "(string<=? <string1> <string2>) Compare two strings lexicographically"),
        "list->string" => (list_to_string, "(list->string <list>) Convert a list of characters to a string"),
        "string-fill!" => (string_fill, "(string-fill! <string> <char>) Fill the string with the given character"),
    );
}

/// (string char1 [char2 ..])
/// Create a string from the provided characters
fn string(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let mut s = String::new();
    for arg in args {
        let c = get_char(heap, *arg)?;
        s.push(c);
    }
    Ok(new_string(heap, &s))
}

/// (string<=? s1 s2)
fn string_less_than_equal(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 2 {
        let s1 = get_string(heap, args[0])?;
        let s2 = get_string(heap, args[1])?;
        let result = new_bool(heap, s1 <= s2);
        Ok(result)
    } else {
        Err("string<=? expects exactly two arguments".to_string())
    }
}

/// (list->string list)
/// Returns a newly allocated string of the characters that make up the given list.
fn list_to_string(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 1 {
        let mut s = String::new();
        let mut current = args[0];
        loop {
            let (h, t) = match heap.get_value(current) {
                SchemeValue::Pair(h, t) => (*h, *t),
                SchemeValue::Nil => break,
                _ => return Err("list->string: not a proper list".to_string()),
            };
            let c = get_char(heap, h)?;
            s.push(c);
            current = t;
        }
        Ok(new_string(heap, &s))
    } else {
        Err("list->string expects exactly one argument".to_string())
    }
}

/// (string-fill! string char)
/// Stores char in every element of string and returns an unspecified value.
fn string_fill(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 2 {
        let str_ref = args[0];
        let c = get_char(heap, args[1])?;
        match heap.get_value_mut(str_ref) {
            SchemeValue::Str(s) => {
                let len = s.chars().count();
                let new_s: String = std::iter::repeat(c).take(len).collect();
                *s = new_s;
                Ok(heap.unspecified())
            }
            _ => Err("string-fill!: not a string".to_string()),
        }
    } else {
        Err("string-fill! expects two arguments".to_string())
    }
}

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
        let result = new_int(heap, BigInt::from(arg.chars().count()));
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
        if k < s.chars().count() {
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

fn get_char(heap: &mut GcHeap, val: GcRef) -> Result<char, String> {
    match heap.get_value(val) {
        SchemeValue::Char(val) => Ok(*val),
        _ => Err("Expected char value".to_string()),
    }
}

/// (make-string k [char])
/// Returns a newly allocated string of length k.
/// If char is given, then all elements of the string are initialized to char,
/// otherwise the contents of the string are unspecified.
fn make_string(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 1 {
        let k = get_integer(heap, args[0])? as usize;
        let s: String = std::iter::repeat(' ').take(k).collect();
        Ok(new_string(heap, &s))
    } else if args.len() == 2 {
        let k = get_integer(heap, args[0])? as usize;
        let c = get_char(heap, args[1])?;
        let s: String = std::iter::repeat(c).take(k).collect();
        Ok(new_string(heap, &s))
    } else {
        Err("make-string expects one or two arguments".to_string())
    }
}

/// (string-set! string k char)
/// Stores char in element k of string and returns an unspecified value.
fn string_set(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 3 {
        let str_ref = args[0];
        let k = get_integer(heap, args[1])? as usize;
        let c = get_char(heap, args[2])?;

        match heap.get_value_mut(str_ref) {
            SchemeValue::Str(s) => {
                if let Some((byte_index, old_char)) = s.char_indices().nth(k) {
                    s.replace_range(byte_index..byte_index + old_char.len_utf8(), &c.to_string());
                    Ok(heap.unspecified())
                } else {
                    Err("string-set!: index out of bounds".to_string())
                }
            }
            _ => Err("string-set!: not a string".to_string()),
        }
    } else {
        Err("string-set! expects three arguments".to_string())
    }
}

/// (string->list string)
/// Returns a newly allocated list of the characters that make up the given string.
fn string_to_list(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 1 {
        let s = get_string(heap, args[0])?;
        let mut list = heap.nil_s();
        for c in s.chars().rev() {
            let char_ref = new_char(heap, c);
            list = new_pair(heap, char_ref, list);
        }
        Ok(list)
    } else {
        Err("string->list expects exactly one argument".to_string())
    }
}
