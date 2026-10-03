//! Strings (R7RS 6.7), including the case-insensitive comparisons and
//! conversions to and from lists and vectors.

use crate::env::{EnvOps, EnvRef};
use crate::gc::{
    GcHeap, GcRef, SString, SchemeValue, get_integer, get_string, new_bool, new_char, new_int, new_pair,
    new_string,
};
use super::char::fold_string;
use super::range_args;
use crate::printer::{display_value, print_value};
use crate::{gc_value, gc_value_mut, register_builtin_family};
use std::cmp::Ordering;
use num_bigint::BigInt;

// `(string char1 [char2 ..])`
// Create a string from the provided characters
// fn string(ec: &mut EvalContext, args) {

// }

/////////////////////////////////////////////////
/// Bind the string procedures in `env`.
pub fn register_string_builtins(heap: &mut GcHeap, env: EnvRef) {
    register_builtin_family!(heap, env,
        ">string" => (to_string, "(>string obj ...) returns a string of the objects as display would print them, concatenated"),
        "string-upcase" => (string_upcase, "(string-upcase <string>) Convert a string to uppercase"),
        "string-downcase" => (string_downcase, "(string-downcase <string>) Convert a string to lowercase"),
        "substring" => (substring, "(substring <string> <start> <end>) The characters from index start up to, not including, end"),
        "string-copy" => (string_copy, "(string-copy <string> [<start> [<end>]]) Copy all of string, or the characters from start up to, not including, end"),
        "string-append" => (string_append, "(string-append <string1> <string2> ..) Concatenate the provided strings"),
        "string-length" => (string_length, "(string-length <string>) Return the length of the given string"),
        "string-ref" => (string_ref, "(string-ref <string> <index>) Return the character at the given index"),
        "make-string" => (make_string, "(make-string <length> [<fill-char>]) Create a string of the given length"),
        "string-set!" => (string_set, "(string-set! <string> <index> <char>) Set the character at the given index"),
        "string" => (string, "(string <char1> [<char2> ..]) Create a string from the provided characters"),
        "string=?" => (string_eq, "(string=? s1 s2 s3 ...) Returns #t if all the strings are the same"),
        "string<?" => (string_lt, "(string<? s1 s2 s3 ...) Returns #t if the strings are in increasing lexicographic order"),
        "string>?" => (string_gt, "(string>? s1 s2 s3 ...) Returns #t if the strings are in decreasing lexicographic order"),
        "string<=?" => (string_le, "(string<=? s1 s2 s3 ...) Returns #t if the strings are in non-decreasing order"),
        "string>=?" => (string_ge, "(string>=? s1 s2 s3 ...) Returns #t if the strings are in non-increasing order"),
        "string-ci=?" => (string_ci_eq, "(string-ci=? s1 s2 s3 ...) string=? after case folding"),
        "string-ci<?" => (string_ci_lt, "(string-ci<? s1 s2 s3 ...) string<? after case folding"),
        "string-ci>?" => (string_ci_gt, "(string-ci>? s1 s2 s3 ...) string>? after case folding"),
        "string-ci<=?" => (string_ci_le, "(string-ci<=? s1 s2 s3 ...) string<=? after case folding"),
        "string-ci>=?" => (string_ci_ge, "(string-ci>=? s1 s2 s3 ...) string>=? after case folding"),
        "string-foldcase" => (string_foldcase, "(string-foldcase string) Returns string with full Unicode case folding applied"),
        "string->list" => (string_to_list, "(string->list string [start [end]]) Returns a list of the characters of string from start to end"),
        "string-fill!" => (string_fill, "(string-fill! string char [start [end]]) Stores char in the elements of string from start to end"),
        "string-copy!" => (string_copy_to, "(string-copy! to at from [start [end]]) Copies the characters start to end of from into to, starting at index at"),
        "string->vector" => (string_to_vector, "(string->vector string [start [end]]) Returns a vector of the characters of string from start to end"),
        "vector->string" => (vector_to_string, "(vector->string vector [start [end]]) Returns a string of the characters in vector from start to end"),
        "list->string" => (list_to_string, "(list->string <list>) Convert a list of characters to a string"),
    );
}

/// `(string char1 [char2 ..])`
/// Create a string from the provided characters
fn string(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let mut s = String::new();
    for arg in args {
        let c = get_char(heap, *arg)?;
        s.push(c);
    }
    Ok(new_string(heap, &s))
}

/// `(list->string list)`
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

/// `(>string obj ...)`
/// The objects' `display` representations, concatenated, as a new string.
fn to_string(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.is_empty() {
        return Err(">string: expects at least 1 argument".to_string());
    }
    let text: String = args.iter().map(display_value).collect();
    Ok(new_string(heap, &text))
}

/// `(string-upcase string)`
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

/// `(string-downcase string)`
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

/// `(substring string start end)`
/// The characters of `string` from index `start` up to, not including, `end`.
fn substring(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 3 {
        return Err("substring: expects string, start and end arguments".to_string());
    }
    copy_range(heap, args, "substring")
}

/// The characters of `args[0]` between the optional start (`args[1]`,
/// default 0) and end (`args[2]`, default the length) character indexes,
/// end exclusive, as a new string. Indexes are checked rather than trusted:
/// slicing a Rust `String` by them directly would panic on a bad range or a
/// multi-byte character.
fn copy_range(heap: &mut GcHeap, args: &[GcRef], name: &str) -> Result<GcRef, String> {
    let s = sstr_of(args[0], name)?;
    let len = s.char_len();
    let index = |heap: &mut GcHeap, i: usize, default: usize| -> Result<usize, String> {
        match args.get(i) {
            None => Ok(default),
            Some(arg) => get_integer(heap, *arg)
                .ok()
                .and_then(|n| usize::try_from(n).ok())
                .ok_or_else(|| format!("{}: index must be a non-negative integer", name)),
        }
    };
    let start = index(heap, 1, 0)?;
    let end = index(heap, 2, len)?;
    if start > end || end > len {
        return Err(format!(
            "{}: range {}..{} is out of bounds for a string of length {}",
            name, start, end, len
        ));
    }
    let result = s.substring(start, end).expect("range checked above").to_string();
    Ok(new_string(heap, &result))
}

/// `(string-append string1 string2 ...)`
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

/// `(string-copy string [start [end]])`
/// Create a new string by copying all or part of the given string.
fn string_copy(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.is_empty() || args.len() > 3 {
        return Err("string-copy expects 1 to 3 arguments".to_string());
    }
    copy_range(heap, args, "string-copy")
}

/// `(string-length string)`
/// Returns the length of the given string.
fn string_length(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 1 {
        let len = sstr_of(args[0], "string-length")?.char_len();
        Ok(new_int(heap, BigInt::from(len)))
    } else {
        Err("string-length expects exactly one argument".to_string())
    }
}

/// `(string-ref string k)`
/// Returns the character at the given index in the string.
fn string_ref(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 2 {
        let s = sstr_of(args[0], "string-ref")?;
        let k = index_arg(heap, args[1], "string-ref")?;
        match s.char_at(k) {
            Some(c) => Ok(new_char(heap, c)),
            None => Err("string-ref: index out of bounds".to_string()),
        }
    } else {
        Err("string-ref expects exactly two arguments".to_string())
    }
}

fn get_char(heap: &mut GcHeap, val: GcRef) -> Result<char, String> {
    match heap.get_value(val) {
        SchemeValue::Char(val) => Ok(*val),
        _ => Err("Expected char value".to_string()),
    }
}

/// `(make-string k [char])`
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

/// `(string-set! string k char)`
/// Stores char in element k of string and returns an unspecified value.
fn string_set(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 3 {
        let k = index_arg(heap, args[1], "string-set!")?;
        let c = get_char(heap, args[2]).map_err(|_| "string-set!: expected a character".to_string())?;
        if str_mut(args[0], "string-set!")?.set_char(k, c) {
            Ok(heap.unspecified())
        } else {
            Err("string-set!: index out of bounds".to_string())
        }
    } else {
        Err("string-set! expects three arguments".to_string())
    }
}


// ---------------------------------------------------------------------------
// R7RS 6.7 procedures over whole strings and ranges
// ---------------------------------------------------------------------------

fn str_of(v: GcRef, who: &str) -> Result<&'static str, String> {
    sstr_of(v, who).map(|s| s.as_str())
}

fn sstr_of(v: GcRef, who: &str) -> Result<&'static SString, String> {
    match gc_value!(v) {
        SchemeValue::Str(s) => Ok(s),
        _ => Err(format!("{}: expected a string, got {}", who, print_value(&v))),
    }
}

/// A string index argument: a non-negative exact integer.
fn index_arg(heap: &mut GcHeap, v: GcRef, who: &str) -> Result<usize, String> {
    get_integer(heap, v)
        .ok()
        .and_then(|n| usize::try_from(n).ok())
        .ok_or_else(|| format!("{}: index must be a non-negative integer", who))
}

fn str_mut(v: GcRef, who: &str) -> Result<&'static mut SString, String> {
    match gc_value_mut!(v) {
        SchemeValue::Str(s) => Ok(s),
        _ => Err(format!("{}: expected a string, got {}", who, print_value(&v))),
    }
}

/// A chained comparison of strings (code point order, which is UTF-8 byte
/// order), optionally after case folding.
fn compare(heap: &mut GcHeap, args: &[GcRef], who: &str, fold: bool, ok: fn(Ordering) -> bool) -> Result<GcRef, String> {
    if args.len() < 2 {
        return Err(format!("{}: expects at least 2 arguments", who));
    }
    let strs = args
        .iter()
        .map(|a| str_of(*a, who).map(|s| if fold { fold_string(s) } else { s.to_string() }))
        .collect::<Result<Vec<String>, String>>()?;
    let result = strs.windows(2).all(|w| ok(w[0].cmp(&w[1])));
    Ok(new_bool(heap, result))
}

fn string_eq(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare(heap, args, "string=?", false, Ordering::is_eq)
}
fn string_lt(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare(heap, args, "string<?", false, Ordering::is_lt)
}
fn string_gt(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare(heap, args, "string>?", false, Ordering::is_gt)
}
fn string_le(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare(heap, args, "string<=?", false, Ordering::is_le)
}
fn string_ge(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare(heap, args, "string>=?", false, Ordering::is_ge)
}
fn string_ci_eq(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare(heap, args, "string-ci=?", true, Ordering::is_eq)
}
fn string_ci_lt(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare(heap, args, "string-ci<?", true, Ordering::is_lt)
}
fn string_ci_gt(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare(heap, args, "string-ci>?", true, Ordering::is_gt)
}
fn string_ci_le(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare(heap, args, "string-ci<=?", true, Ordering::is_le)
}
fn string_ci_ge(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare(heap, args, "string-ci>=?", true, Ordering::is_ge)
}

fn string_foldcase(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    match args {
        [s] => {
            let folded = fold_string(str_of(*s, "string-foldcase")?);
            Ok(new_string(heap, &folded))
        }
        _ => Err("string-foldcase: expects 1 argument".to_string()),
    }
}

fn arity(args: &[GcRef], min: usize, max: usize, who: &str) -> Result<(), String> {
    if args.len() < min || args.len() > max {
        Err(format!("{}: wrong number of arguments ({})", who, args.len()))
    } else {
        Ok(())
    }
}

/// The characters of string `args[0]` between the range at `args[at..]`.
fn char_range(args: &[GcRef], at: usize, who: &str) -> Result<Vec<char>, String> {
    let chars: Vec<char> = str_of(args[0], who)?.chars().collect();
    let (start, end) = range_args(args, at, chars.len(), who)?;
    Ok(chars[start..end].to_vec())
}

fn string_to_list(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, 3, "string->list")?;
    let chars = char_range(args, 1, "string->list")?;
    let mut list = heap.nil_s();
    for c in chars.into_iter().rev() {
        let ch = new_char(heap, c);
        list = new_pair(heap, ch, list);
    }
    Ok(list)
}

fn string_to_vector(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, 3, "string->vector")?;
    let chars = char_range(args, 1, "string->vector")?;
    let items = chars.into_iter().map(|c| new_char(heap, c)).collect();
    Ok(crate::gc::new_vector(heap, items))
}

fn vector_to_string(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, 3, "vector->string")?;
    let items = match gc_value!(args[0]) {
        SchemeValue::Vector(v) => v,
        _ => return Err("vector->string: expected a vector".to_string()),
    };
    let (start, end) = range_args(args, 1, items.len(), "vector->string")?;
    let mut out = String::new();
    for item in &items[start..end] {
        match gc_value!(*item) {
            SchemeValue::Char(c) => out.push(*c),
            _ => return Err(format!("vector->string: not a character: {}", print_value(item))),
        }
    }
    Ok(new_string(heap, &out))
}

fn string_fill(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 2, 4, "string-fill!")?;
    let fill = get_char(heap, args[1]).map_err(|_| "string-fill!: expected a character".to_string())?;
    let target = str_mut(args[0], "string-fill!")?;
    let (start, end) = range_args(args, 2, target.char_len(), "string-fill!")?;
    let fills = vec![fill; end - start];
    target.replace_chars(start, &fills);
    Ok(heap.unspecified())
}

fn string_copy_to(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 3, 5, "string-copy!")?;
    // Copy the source characters out first: `to` and `from` may be the same.
    let src = char_range(&args[2..], 1, "string-copy!")?;
    let target = str_mut(args[0], "string-copy!")?;
    let fits = match gc_value!(args[1]) {
        SchemeValue::Int(i) => num_traits::ToPrimitive::to_usize(i).is_some_and(|at| target.replace_chars(at, &src)),
        _ => false,
    };
    if !fits {
        return Err("string-copy!: the copied characters don't fit at that index".to_string());
    }
    Ok(heap.unspecified())
}
