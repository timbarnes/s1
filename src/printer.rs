// Only keep this function for pretty-printing SchemeValueSimple:
use crate::gc::SchemeValue::*;
use crate::gc::{Callable, GcRef};
use crate::gc_value;

/// The external representation `display` produces: strings and characters
/// appear as their raw text, at any depth.
pub fn display_value(obj: &GcRef) -> String {
    let mut out = String::new();
    print_into(&mut out, *obj, false);
    out
}

/// The external representation `write` produces: strings and characters in
/// the escaped form the reader accepts.
pub fn print_value(obj: &GcRef) -> String {
    let mut out = String::new();
    print_into(&mut out, *obj, true);
    out
}

fn print_into(out: &mut String, obj: GcRef, write: bool) {
    match gc_value!(obj) {
        Pair(_, _) => {
            out.push('(');
            let mut current = obj;
            let mut first = true;
            loop {
                match gc_value!(current) {
                    Pair(car, cdr) => {
                        if !first {
                            out.push(' ');
                        }
                        print_into(out, *car, write);
                        current = *cdr;
                        first = false;
                    }
                    Nil => break,
                    _ => {
                        out.push_str(" . ");
                        print_into(out, current, write);
                        break;
                    }
                }
            }
            out.push(')');
        }
        Vector(v) => {
            out.push_str("#(");
            print_separated(out, v, write);
            out.push(')');
        }
        // The values of a `(values ...)` package that reached a printer.
        Values(v) => print_separated(out, v, write),
        Symbol(s) if write => write_symbol(out, s),
        Symbol(s) => out.push_str(s),
        Int(i) => out.push_str(&i.to_string()),
        Float(f) => out.push_str(&format_float(*f)),
        Str(s) if write => write_string(out, s),
        Str(s) => out.push_str(s),
        Char(c) if write => write_char(out, *c),
        Char(c) => out.push(*c),
        Bool(true) => out.push_str("#t"),
        Bool(false) => out.push_str("#f"),
        Nil => out.push_str("()"),
        Void => {}
        Undefined => out.push_str("#<undefined>"),
        Eof => out.push_str("#<eof>"),
        Callable(variant) => out.push_str(&match &**variant {
            Callable::Builtin { func: _, doc } => format!("Primitive {} ", doc),
            Callable::SpecialForm { doc, .. } => format!("SpecialForm {} ", doc),
            Callable::Closure { params, body, .. } => print_callable("Closure", params, *body),
            Callable::Macro { params, body, .. } => print_callable("Macro", params, *body),
            Callable::SysBuiltin { func: _, doc } => format!("SysBuiltin {}", doc),
        }),
        Port(port) => out.push_str(&format!("Port<{:?}>", port)),
        Continuation(k) => out.push_str(&format!("Continuation<{:?}>", k.kont)),
        TailCallScheduled => out.push_str("print_value: unprintable."),
    }
}

fn print_separated(out: &mut String, items: &[GcRef], write: bool) {
    for (i, item) in items.iter().enumerate() {
        if i > 0 {
            out.push(' ');
        }
        print_into(out, *item, write);
    }
}

/// Format a flonum so it reads back as one: always with a decimal point or
/// exponent (`2.0`, not `2`), and the R7RS spellings of the infinities and
/// NaN. Rust's `Debug` output is already shortest-round-trip and switches
/// to exponent notation for very large and small magnitudes (`1e21`).
pub fn format_float(f: f64) -> String {
    if f.is_nan() {
        "+nan.0".to_string()
    } else if f.is_infinite() {
        if f > 0.0 { "+inf.0" } else { "-inf.0" }.to_string()
    } else {
        format!("{:?}", f)
    }
}

/// Write a symbol, in `|...|` form when its bare name would read back as
/// something else.
fn write_symbol(out: &mut String, s: &str) {
    if !needs_bars(s) {
        out.push_str(s);
        return;
    }
    out.push('|');
    for c in s.chars() {
        match c {
            '|' => out.push_str("\\|"),
            '\\' => out.push_str("\\\\"),
            c if c.is_control() => out.push_str(&format!("\\x{:x};", c as u32)),
            c => out.push(c),
        }
    }
    out.push('|');
}

fn needs_bars(s: &str) -> bool {
    let mut chars = s.chars();
    let Some(first) = chars.next() else {
        return true; // the empty symbol
    };
    let second = chars.next();
    let numeric_start = first.is_ascii_digit()
        || (first == '.' && second.is_some_and(|c| c.is_ascii_digit()))
        || (matches!(first, '+' | '-')
            && (second.is_some_and(|c| c.is_ascii_digit() || c == '.')
                || s[1..].to_ascii_lowercase().starts_with("inf")
                || s[1..].to_ascii_lowercase().starts_with("nan")));
    s == "."
        // `nil` reads as the empty list in s1
        || s == "nil"
        || first == '#'
        || numeric_start
        || crate::number_syntax::parse_number(s, 10) != crate::number_syntax::NumberSyntax::NotANumber
        || s.chars().any(|c| c.is_whitespace() || c.is_control() || "()[]\";'`,|\\".contains(c))
}

/// Write a string literal with the escapes R7RS's reader understands.
fn write_string(out: &mut String, s: &str) {
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            '\u{7}' => out.push_str("\\a"),
            '\u{8}' => out.push_str("\\b"),
            c if c.is_control() => out.push_str(&format!("\\x{:x};", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
}

/// Write a character literal, using R7RS's character names where they
/// exist and hex escapes for other control characters.
fn write_char(out: &mut String, c: char) {
    out.push_str("#\\");
    match c {
        '\u{7}' => out.push_str("alarm"),
        '\u{8}' => out.push_str("backspace"),
        '\u{7f}' => out.push_str("delete"),
        '\u{1b}' => out.push_str("escape"),
        '\n' => out.push_str("newline"),
        '\0' => out.push_str("null"),
        '\r' => out.push_str("return"),
        ' ' => out.push_str("space"),
        '\t' => out.push_str("tab"),
        c if c.is_control() => out.push_str(&format!("x{:x}", c as u32)),
        c => out.push(c),
    }
}

fn print_callable(callable_type: &str, params: &Vec<GcRef>, body: GcRef) -> String {
    let mut s = callable_type.to_string();
    match params.len() {
        0 => s.push_str(" () "),
        1 => {
            s.push(' ');
            s.push_str(print_value(&params[0]).as_str());
            s.push(' ');
        }
        _ => {
            // two cases: list and dotted, depending on the value of params[0]
            s.push_str(" (");
            for arg in params[1..].iter() {
                s.push_str(print_value(arg).as_str());
                s.push(' ');
            }
            match &gc_value!(params[0]) {
                Symbol(name) => {
                    s.push_str(". ");
                    s.push_str(name.as_str());
                    s.push(' ');
                }
                _ => (),
            }
            s.pop();
            s.push_str(") ");
        }
    }
    s.push_str(print_value(&body).as_str());
    s
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::{RunTime, RunTimeStruct};
    use crate::gc::*;
    use num_bigint::BigInt;

    #[test]
    fn test_print_value_basic_types() {
        let mut ev = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut ev);
        let heap = &mut ec.heap;

        assert_eq!(print_value(&new_int(heap, BigInt::from(123))), "123");
        assert_eq!(print_value(&new_float(heap, 3.14)), "3.14");
        assert_eq!(print_value(&new_bool(heap, true)), "#t");
        assert_eq!(print_value(&new_bool(heap, false)), "#f");
        assert_eq!(print_value(&new_string(heap, "hello")), "\"hello\"");
        assert_eq!(print_value(&new_char(heap, 'a')), "#\\a");
        assert_eq!(print_value(&new_char(heap, '\n')), "#\\newline");
        assert_eq!(print_value(&heap.intern_symbol("foo")), "foo");
        assert_eq!(print_value(&heap.nil_s()), "()");
        assert_eq!(print_value(&heap.void()), "");
        assert_eq!(print_value(&heap.unspecified()), "#<undefined>");
        assert_eq!(print_value(&heap.eof()), "#<eof>");
    }

    #[test]
    fn test_write_escapes_and_float_format() {
        let mut ev = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut ev);
        let heap = &mut ec.heap;

        // Strings: quote, backslash and control characters are escaped.
        let s = new_string(heap, "a\"b\\c\nd\te\u{1}");
        assert_eq!(print_value(&s), r#""a\"b\\c\nd\te\x1;""#);
        assert_eq!(display_value(&s), "a\"b\\c\nd\te\u{1}");

        // Characters use R7RS names.
        assert_eq!(print_value(&new_char(heap, ' ')), "#\\space");
        assert_eq!(print_value(&new_char(heap, '\u{7}')), "#\\alarm");
        assert_eq!(print_value(&new_char(heap, '\0')), "#\\null");
        assert_eq!(print_value(&new_char(heap, '\u{1}')), "#\\x1");

        // Flonums always read back as inexact.
        assert_eq!(print_value(&new_float(heap, 2.0)), "2.0");
        assert_eq!(print_value(&new_float(heap, -0.0)), "-0.0");
        assert_eq!(print_value(&new_float(heap, 1e21)), "1e21");
        assert_eq!(print_value(&new_float(heap, f64::INFINITY)), "+inf.0");
        assert_eq!(print_value(&new_float(heap, f64::NEG_INFINITY)), "-inf.0");
        assert_eq!(print_value(&new_float(heap, f64::NAN)), "+nan.0");

        // Symbols that wouldn't read back bare are written with bars.
        for (name, written) in [
            ("abc", "abc"),
            ("->x", "->x"),
            ("...", "..."),
            ("a b", "|a b|"),
            ("", "||"),
            (".", "|.|"),
            ("2", "|2|"),
            ("+3", "|+3|"),
            ("-.4", "|-.4|"),
            ("+i", "|+i|"),
            ("+NaN.0abc", "|+NaN.0abc|"),
            ("|", "|\\||"),
            ("\\123", "|\\\\123|"),
            ("nil", "|nil|"),
        ] {
            let sym = heap.intern_symbol(name);
            assert_eq!(print_value(&sym), written, "symbol {:?}", name);
            assert_eq!(display_value(&sym), name);
        }

        // display reaches strings and characters nested in a list.
        let c = new_char(heap, 'x');
        let str_in_list = new_string(heap, "s");
        let tail = new_pair(heap, c, heap.nil_s());
        let list = new_pair(heap, str_in_list, tail);
        assert_eq!(display_value(&list), "(s x)");
        assert_eq!(print_value(&list), "(\"s\" #\\x)");
    }

    #[test]
    fn test_print_value_compound_types() {
        let mut ev = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut ev);
        let heap = &mut ec.heap;

        // Proper List
        let val1 = new_int(heap, BigInt::from(1));
        let val2 = new_int(heap, BigInt::from(2));
        let val3 = new_int(heap, BigInt::from(3));
        let l1 = new_pair(heap, val3, heap.nil_s());
        let l2 = new_pair(heap, val2, l1);
        let list = new_pair(heap, val1, l2);
        assert_eq!(print_value(&list), "(1 2 3)");

        // Dotted List
        let val1_dotted = new_int(heap, BigInt::from(1));
        let val2_dotted = new_int(heap, BigInt::from(2));
        let dotted_list = new_pair(heap, val1_dotted, val2_dotted);
        assert_eq!(print_value(&dotted_list), "(1 . 2)");

        // Vector
        let vec_val1 = new_int(heap, BigInt::from(1));
        let vec_val2 = new_bool(heap, true);
        let vec_val3 = new_string(heap, "foo");
        let vector = new_vector(heap, vec![vec_val1, vec_val2, vec_val3]);
        assert_eq!(print_value(&vector), "#(1 #t \"foo\")");
    }

    #[test]
    fn test_display_value_basic_types() {
        let mut ev = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut ev);
        let heap = &mut ec.heap;

        assert_eq!(display_value(&new_int(heap, BigInt::from(123))), "123");
        assert_eq!(display_value(&new_float(heap, 3.14)), "3.14");
        assert_eq!(display_value(&new_bool(heap, true)), "#t");
        assert_eq!(display_value(&new_bool(heap, false)), "#f");
        assert_eq!(display_value(&new_string(heap, "hello")), "hello"); // No quotes
        assert_eq!(display_value(&new_char(heap, 'a')), "a"); // No #\ prefix
        assert_eq!(display_value(&new_char(heap, '\n')), "\n");
        assert_eq!(display_value(&heap.intern_symbol("foo")), "foo");
        assert_eq!(display_value(&heap.nil_s()), "()");
        assert_eq!(display_value(&heap.void()), "");
        assert_eq!(display_value(&heap.unspecified()), "#<undefined>"); // Corrected
        assert_eq!(display_value(&heap.eof()), "#<eof>"); // Corrected
    }

    #[test]
    fn test_display_value_compound_types() {
        let mut ev = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut ev);
        let heap = &mut ec.heap;

        // Proper List
        let val1 = new_int(heap, BigInt::from(1));
        let val2 = new_int(heap, BigInt::from(2));
        let val3 = new_int(heap, BigInt::from(3));
        let l1 = new_pair(heap, val3, heap.nil_s());
        let l2 = new_pair(heap, val2, l1);
        let list = new_pair(heap, val1, l2);
        assert_eq!(display_value(&list), "(1 2 3)");

        // Dotted List
        let val1_dotted = new_int(heap, BigInt::from(1));
        let val2_dotted = new_int(heap, BigInt::from(2));
        let dotted_list = new_pair(heap, val1_dotted, val2_dotted);
        assert_eq!(display_value(&dotted_list), "(1 . 2)");

        // Vector
        let vec_val1 = new_int(heap, BigInt::from(1));
        let vec_val2 = new_bool(heap, true);
        let vec_val3 = new_string(heap, "foo");
        let vector = new_vector(heap, vec![vec_val1, vec_val2, vec_val3]);
        // display applies at every depth, so the nested string is unquoted.
        assert_eq!(display_value(&vector), "#(1 #t foo)");
    }
}
