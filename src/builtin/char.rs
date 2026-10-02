//! Characters (R7RS 6.6), Unicode-aware, and the case-folding and
//! digit-value helpers the string procedures share.

use crate::env::{EnvOps, EnvRef};
use crate::gc::{GcHeap, GcRef, SchemeValue, new_bool, new_char, new_int};
use crate::printer::print_value;
use crate::{gc_value, register_builtin_family};
use num_bigint::BigInt;
use std::cmp::Ordering;

pub fn register_char_builtins(heap: &mut GcHeap, env: EnvRef) {
    register_builtin_family!(heap, env,
        "char=?" => (char_eq, "(char=? c1 c2 c3 ...) Returns #t if all the characters are the same"),
        "char<?" => (char_lt, "(char<? c1 c2 c3 ...) Returns #t if the characters are in increasing order"),
        "char>?" => (char_gt, "(char>? c1 c2 c3 ...) Returns #t if the characters are in decreasing order"),
        "char<=?" => (char_le, "(char<=? c1 c2 c3 ...) Returns #t if the characters are in non-decreasing order"),
        "char>=?" => (char_ge, "(char>=? c1 c2 c3 ...) Returns #t if the characters are in non-increasing order"),
        "char-ci=?" => (char_ci_eq, "(char-ci=? c1 c2 c3 ...) char=? after case folding"),
        "char-ci<?" => (char_ci_lt, "(char-ci<? c1 c2 c3 ...) char<? after case folding"),
        "char-ci>?" => (char_ci_gt, "(char-ci>? c1 c2 c3 ...) char>? after case folding"),
        "char-ci<=?" => (char_ci_le, "(char-ci<=? c1 c2 c3 ...) char<=? after case folding"),
        "char-ci>=?" => (char_ci_ge, "(char-ci>=? c1 c2 c3 ...) char>=? after case folding"),
        "char-alphabetic?" => (char_alphabetic, "(char-alphabetic? c) Returns #t if c is a Unicode letter"),
        "char-numeric?" => (char_numeric, "(char-numeric? c) Returns #t if c is a Unicode decimal digit"),
        "char-whitespace?" => (char_whitespace, "(char-whitespace? c) Returns #t if c is Unicode white space"),
        "char-upper-case?" => (char_upper, "(char-upper-case? c) Returns #t if c is an upper-case letter"),
        "char-lower-case?" => (char_lower, "(char-lower-case? c) Returns #t if c is a lower-case letter"),
        "digit-value" => (digit_value_b, "(digit-value c) Returns the value of decimal digit c (any script), or #f"),
        "char->integer" => (char_to_integer, "(char->integer char) Returns the Unicode scalar value of char"),
        "integer->char" => (integer_to_char, "(integer->char n) Returns the character with Unicode scalar value n"),
        "char-upcase" => (char_upcase, "(char-upcase c) Returns the upper-case form of c (c itself if there is no single-character one)"),
        "char-downcase" => (char_downcase, "(char-downcase c) Returns the lower-case form of c (c itself if there is no single-character one)"),
        "char-foldcase" => (char_foldcase, "(char-foldcase c) Returns c with simple Unicode case folding applied"),
    );
}

// ---------------------------------------------------------------------------
// Unicode helpers
// ---------------------------------------------------------------------------

/// The first code point of each run of ten Unicode decimal digits
/// (General_Category Nd), 0 to 9 in order.
const DIGIT_ZEROS: &[u32] = &[
    0x0030, 0x0660, 0x06F0, 0x07C0, 0x0966, 0x09E6, 0x0A66, 0x0AE6, 0x0B66, 0x0BE6, 0x0C66,
    0x0CE6, 0x0D66, 0x0DE6, 0x0E50, 0x0ED0, 0x0F20, 0x1040, 0x1090, 0x17E0, 0x1810, 0x1946,
    0x19D0, 0x1A80, 0x1A90, 0x1B50, 0x1BB0, 0x1C40, 0x1C50, 0xA620, 0xA8D0, 0xA900, 0xA9D0,
    0xA9F0, 0xAA50, 0xABF0, 0xFF10, 0x104A0, 0x10D30, 0x11066, 0x110F0, 0x11136, 0x111D0,
    0x112F0, 0x11450, 0x114D0, 0x11650, 0x116C0, 0x11730, 0x118E0, 0x11950, 0x11C50, 0x11D50,
    0x11DA0, 0x16A60, 0x16AC0, 0x16B50, 0x1D7CE, 0x1D7D8, 0x1D7E2, 0x1D7EC, 0x1D7F6, 0x1E140,
    0x1E2F0, 0x1E950, 0x1FBF0,
];

/// The value of `c` as a decimal digit in any script.
pub fn digit_value(c: char) -> Option<u32> {
    let cp = c as u32;
    DIGIT_ZEROS
        .iter()
        .find(|&&zero| cp >= zero && cp < zero + 10)
        .map(|zero| cp - zero)
}

/// `c`'s single-character mapping under `f`, or `c` when the mapping isn't
/// a single character (`ß` upcases to `SS`).
fn single(c: char, f: impl Fn(char) -> String) -> char {
    let mapped = f(c);
    let mut chars = mapped.chars();
    match (chars.next(), chars.next()) {
        (Some(m), None) => m,
        _ => c,
    }
}

pub fn upcase(c: char) -> char {
    single(c, |c| c.to_uppercase().collect())
}

pub fn downcase(c: char) -> char {
    single(c, |c| c.to_lowercase().collect())
}

/// Simple case folding (one character to one).
pub fn fold_char(c: char) -> char {
    match c {
        '\u{17F}' => 's',        // long s
        '\u{3C2}' => '\u{3C3}', // final sigma to sigma
        '\u{1E9E}' => '\u{DF}', // capital sharp s to sharp s
        c => downcase(c),
    }
}

/// Full case folding, as `string-foldcase` uses: like lower-casing, but
/// `ß` becomes `ss` and final sigma an ordinary sigma.
pub fn fold_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\u{DF}' | '\u{1E9E}' => out.push_str("ss"),
            '\u{17F}' => out.push('s'),
            '\u{3C2}' => out.push('\u{3C3}'),
            c => out.extend(c.to_lowercase()),
        }
    }
    out
}

// ---------------------------------------------------------------------------
// Procedures
// ---------------------------------------------------------------------------

fn get_char(heap: &mut GcHeap, val: GcRef) -> Result<char, String> {
    match heap.get_value(val) {
        SchemeValue::Char(c) => Ok(*c),
        _ => Err(format!("expected a character, got {}", print_value(&val))),
    }
}

fn chars(args: &[GcRef], who: &str) -> Result<Vec<char>, String> {
    args.iter()
        .map(|a| match gc_value!(*a) {
            SchemeValue::Char(c) => Ok(*c),
            _ => Err(format!("{}: expected a character, got {}", who, print_value(a))),
        })
        .collect()
}

/// A chained comparison: every adjacent pair, mapped by `key`, is ordered
/// as `ok` accepts.
fn compare(
    heap: &mut GcHeap,
    args: &[GcRef],
    who: &str,
    key: fn(char) -> char,
    ok: fn(Ordering) -> bool,
) -> Result<GcRef, String> {
    if args.len() < 2 {
        return Err(format!("{}: expects at least 2 arguments", who));
    }
    let cs = chars(args, who)?;
    let result = cs.windows(2).all(|w| ok(key(w[0]).cmp(&key(w[1]))));
    Ok(new_bool(heap, result))
}

fn same(c: char) -> char {
    c
}

fn char_eq(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare(heap, args, "char=?", same, Ordering::is_eq)
}
fn char_lt(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare(heap, args, "char<?", same, Ordering::is_lt)
}
fn char_gt(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare(heap, args, "char>?", same, Ordering::is_gt)
}
fn char_le(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare(heap, args, "char<=?", same, Ordering::is_le)
}
fn char_ge(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare(heap, args, "char>=?", same, Ordering::is_ge)
}
fn char_ci_eq(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare(heap, args, "char-ci=?", fold_char, Ordering::is_eq)
}
fn char_ci_lt(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare(heap, args, "char-ci<?", fold_char, Ordering::is_lt)
}
fn char_ci_gt(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare(heap, args, "char-ci>?", fold_char, Ordering::is_gt)
}
fn char_ci_le(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare(heap, args, "char-ci<=?", fold_char, Ordering::is_le)
}
fn char_ci_ge(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    compare(heap, args, "char-ci>=?", fold_char, Ordering::is_ge)
}

fn one_char(heap: &mut GcHeap, args: &[GcRef], who: &str) -> Result<char, String> {
    match args {
        [c] => get_char(heap, *c).map_err(|e| format!("{}: {}", who, e)),
        _ => Err(format!("{}: expects 1 argument", who)),
    }
}

fn char_pred(heap: &mut GcHeap, args: &[GcRef], who: &str, test: fn(char) -> bool) -> Result<GcRef, String> {
    let c = one_char(heap, args, who)?;
    Ok(new_bool(heap, test(c)))
}

fn char_alphabetic(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    char_pred(heap, args, "char-alphabetic?", char::is_alphabetic)
}
fn char_numeric(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    char_pred(heap, args, "char-numeric?", |c| digit_value(c).is_some())
}
fn char_whitespace(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    char_pred(heap, args, "char-whitespace?", char::is_whitespace)
}
fn char_upper(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    char_pred(heap, args, "char-upper-case?", char::is_uppercase)
}
fn char_lower(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    char_pred(heap, args, "char-lower-case?", char::is_lowercase)
}

fn digit_value_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let c = one_char(heap, args, "digit-value")?;
    Ok(match digit_value(c) {
        Some(d) => new_int(heap, BigInt::from(d)),
        None => heap.false_s(),
    })
}

fn char_map(heap: &mut GcHeap, args: &[GcRef], who: &str, f: fn(char) -> char) -> Result<GcRef, String> {
    let c = one_char(heap, args, who)?;
    Ok(new_char(heap, f(c)))
}

fn char_upcase(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    char_map(heap, args, "char-upcase", upcase)
}
fn char_downcase(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    char_map(heap, args, "char-downcase", downcase)
}
fn char_foldcase(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    char_map(heap, args, "char-foldcase", fold_char)
}

fn char_to_integer(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let c = one_char(heap, args, "char->integer")?;
    Ok(new_int(heap, BigInt::from(c as u32)))
}

fn integer_to_char(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let n = match args {
        [n] => match gc_value!(*n) {
            SchemeValue::Int(i) => num_traits::ToPrimitive::to_u32(i),
            _ => None,
        },
        _ => return Err("integer->char: expects 1 argument".to_string()),
    };
    match n.and_then(char::from_u32) {
        Some(c) => Ok(new_char(heap, c)),
        None => Err("integer->char: invalid character code".to_string()),
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
