// Only keep this function for pretty-printing SchemeValueSimple:
use crate::gc::{GcObject, SchemeValue};

pub fn print_scheme_value(val: &SchemeValue) -> String {
    match val {
        SchemeValue::Pair(_, _) => {
            let mut s = String::from("(");
            let mut first = true;
            let mut current = val;
            loop {
                match current {
                    SchemeValue::Pair(car, cdr) => {
                        if !first {
                            s.push(' ');
                        }
                        s.push_str(&print_scheme_value(&car.value));
                        current = &cdr.value;
                        first = false;
                    }
                    SchemeValue::Nil => {
                        s.push(')');
                        break;
                    }
                    _ => {
                        s.push_str(" . ");
                        s.push_str(&print_scheme_value(current));
                        s.push(')');
                        break;
                    }
                }
            }
            s
        }
        SchemeValue::Symbol(s) => s.clone(),
        SchemeValue::Int(i) => i.to_string(),
        SchemeValue::Float(f) => f.to_string(),
        SchemeValue::Str(s) => s.clone(), // raw string, no quotes
        SchemeValue::Bool(true) => "#t".to_string(),
        SchemeValue::Bool(false) => "#f".to_string(),
        SchemeValue::Char(c) => format!("#\\{}", c),
        SchemeValue::Nil => "nil".to_string(),
        SchemeValue::Closure { params, body, .. } => print_callable("Closure", params, *body),
        SchemeValue::Macro { params, body, .. } => print_callable("Macro", params, *body),
        _ => format!("{:?}", val),
    }
}

fn print_callable(callable_type: &str, params: &Vec<&GcObject>, body: &GcObject) -> String {
    let mut s = callable_type.to_string();
    match params.len() {
        0 => s.push_str(" () "),
        1 => {
            s.push(' ');
            s.push_str(print_scheme_value(&params[0].value).as_str());
            s.push(' ');
        }
        _ => {
            // two cases: list and dotted, depending on the value of params[0]
            s.push_str(" (");
            for arg in params[1..].iter() {
                s.push_str(print_scheme_value(&arg.value).as_str());
                s.push(' ');
            }
            match &params[0].value {
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
    s.push_str(print_scheme_value(&body.value).as_str());
    s
}
