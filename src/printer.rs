// Only keep this function for pretty-printing SchemeValueSimple:
use crate::eval::EvalContext;
use crate::gc::{Callable, GcRef, SchemeValue};

pub fn print_scheme_value(ec: &EvalContext, obj: &GcRef) -> String {
    let val = ec.heap.get_value(*obj);
    match val {
        SchemeValue::Pair(_, _) => {
            let mut s = String::from("(");
            let mut first = true;
            let mut current = *obj;
            loop {
                let value = ec.heap.get_value(current);
                match value {
                    SchemeValue::Pair(car, cdr) => {
                        if !first {
                            s.push(' ');
                        }
                        s.push_str(&print_scheme_value(ec, car));
                        current = *cdr;
                        first = false;
                    }
                    SchemeValue::Nil => {
                        s.push(')');
                        break;
                    }
                    _ => {
                        s.push_str(" . ");
                        s.push_str(&print_scheme_value(ec, &current));
                        s.push(')');
                        break;
                    }
                }
            }
            s
        }
        SchemeValue::Symbol(s) => s.clone(),
        SchemeValue::Vector(v) => {
            let mut s = "#(".to_string();
            let mut first = true;
            for value in v.iter() {
                if !first {
                    s.push(' ');
                }
                s.push_str(print_scheme_value(ec, &value).as_str());
                first = false;
            }
            s.push(')');
            s
        }
        SchemeValue::Int(i) => i.to_string(),
        SchemeValue::Float(f) => f.to_string(),
        SchemeValue::Str(s) => s.clone(), // raw string, no quotes
        SchemeValue::Bool(true) => "#t".to_string(),
        SchemeValue::Bool(false) => "#f".to_string(),
        SchemeValue::Char(c) => format!("#\\{}", c),
        SchemeValue::Nil => "nil".to_string(),
        SchemeValue::Callable(variant) => match variant {
            Callable::Builtin { doc, .. } => format!("Primitive: {}", doc),
            Callable::SpecialForm { doc, .. } => format!("SpecialForm: {}", doc),
            Callable::Closure { params, body, .. } => print_callable(ec, "Closure", params, *body),
            Callable::Macro { params, body, .. } => print_callable(ec, "Macro", params, *body),
        },
        _ => format!("print_scheme_value: unprintable."),
    }
}

fn print_callable(
    ec: &EvalContext,
    callable_type: &str,
    params: &Vec<GcRef>,
    body: GcRef,
) -> String {
    let mut s = callable_type.to_string();
    match params.len() {
        0 => s.push_str(" () "),
        1 => {
            s.push(' ');
            s.push_str(print_scheme_value(&ec, &params[0]).as_str());
            s.push(' ');
        }
        _ => {
            // two cases: list and dotted, depending on the value of params[0]
            s.push_str(" (");
            for arg in params[1..].iter() {
                s.push_str(print_scheme_value(&ec, arg).as_str());
                s.push(' ');
            }
            match &ec.heap.get_value(params[0]) {
                SchemeValue::Symbol(name) => {
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
    s.push_str(print_scheme_value(&ec, &body).as_str());
    s
}
