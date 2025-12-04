// Only keep this function for pretty-printing SchemeValueSimple:
use crate::gc::SchemeValue::*;
use crate::gc::{Callable, GcRef};
use crate::gc_value;

pub fn display_value(obj: &GcRef) -> String {
    let val = gc_value!(*obj);
    match val {
        Str(s) => s.clone(),
        // Char(c) => format!("{c}"),
        _ => print_value(obj),
    }
}

pub fn print_value(obj: &GcRef) -> String {
    let val = gc_value!(*obj);
    match val {
        Pair(_, _) => {
            let mut s = String::from("(");
            let mut first = true;
            let mut current = *obj;
            loop {
                let value = gc_value!(current);
                match value {
                    Pair(car, cdr) => {
                        if !first {
                            s.push(' ');
                        }
                        s.push_str(&print_value(car));
                        current = *cdr;
                        first = false;
                    }
                    Nil => {
                        s.push(')');
                        break;
                    }
                    _ => {
                        s.push_str(" . ");
                        s.push_str(&print_value(&current));
                        s.push(')');
                        break;
                    }
                }
            }
            s
        }
        Symbol(s) => s.clone(),
        Vector(v) => {
            let mut s = "#(".to_string();
            let mut first = true;
            for value in v.iter() {
                if !first {
                    s.push(' ');
                }
                s.push_str(print_value(&value).as_str());
                first = false;
            }
            s.push(')');
            s
        }
        Int(i) => i.to_string(),
        Float(f) => f.to_string(),
        Str(s) => {
            let mut res = "\"".to_string();
            res.push_str(s);
            res.push('"');
            res
        }
        Bool(true) => "#t".to_string(),
        Bool(false) => "#f".to_string(),
        Char(c) => {
            let mut ch = String::new();
            match c {
                '\n' => ch.push_str("newline"),
                '\t' => ch.push_str("tab"),
                '\r' => ch.push_str("return"),
                _ => ch.push(*c),
            };
            format!("#\\{}", ch) // Changed to use format! with named argument
        }
        Nil => "()".to_string(),
        Void => "".to_string(),
        //Void => "#<void>".to_string(),
        Undefined => "#<undefined>".to_string(),
        Eof => "#<eof>".to_string(),
        Callable(variant) => match variant {
            Callable::Builtin { func: _, doc } => format!("Primitive {} ", doc),
            Callable::SpecialForm { doc, .. } => format!("SpecialForm {} ", doc), // Changed
            Callable::Closure { params, body, .. } => print_callable("Closure", params, *body),
            Callable::Macro { params, body, .. } => print_callable("Macro", params, *body),
            Callable::SysBuiltin { func: _, doc } => format!("SysBuiltin {}", doc), // Changed
        },
        Port(port) => format!("Port<{:?}>", port), // Changed
        Continuation(kont, _dw) => format!("Continuation<{:?}>", kont), // Changed
        _ => format!("print_value: unprintable."),
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
    use crate::gc_value;
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
        assert_eq!(display_value(&new_char(heap, 'a')), "#\\a");
        assert_eq!(display_value(&new_char(heap, '\n')), "#\\newline");
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
        // Note: display_value for compound types calls print_value for elements,
        // so strings within vectors will still have quotes if not specifically handled.
        // The current implementation of display_value only special cases the top-level string.
        assert_eq!(display_value(&vector), "#(1 #t \"foo\")");
    }
}
