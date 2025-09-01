// Only keep this function for pretty-printing SchemeValueSimple:
use crate::gc::SchemeValue::*;
use crate::gc::{Callable, GcRef};
use crate::gc_value;

pub fn display_value(obj: &GcRef) -> String {
    let val = gc_value!(*obj);
    match val {
        Str(s) => s.clone(),
        Char(c) => format!("{c}"),
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
        Char(c) => format!("#\\{}", c),
        Nil => "nil".to_string(),
        Void => "#<void>".to_string(),
        Undefined => "#<undefined>".to_string(),
        Eof => "#<eof>".to_string(),
        Callable(variant) => match variant {
            Callable::Builtin { func: _, doc } => format!("Primitive    {}", doc),
            Callable::SpecialForm { doc, .. } => format!("SpecialForm  {}", doc),
            Callable::Closure { params, body, .. } => print_callable("Closure     ", params, *body),
            Callable::Macro { params, body, .. } => print_callable("Macro       ", params, *body),
            Callable::SysBuiltin { func: _, doc } => format!("SysBuiltin   {}", doc),
        },
        Port(port) => format!("Port         ({:?})", port),
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
                    println!("Symbol: {}", name);
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
