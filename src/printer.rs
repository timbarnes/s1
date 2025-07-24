// Only keep this function for pretty-printing SchemeValueSimple:
use crate::gc::SchemeValue;

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
        SchemeValue::Closure { params, body, env } => {
            let mut s = "Closure: ( ".to_string();
            for arg in params {
                s.push_str(print_scheme_value(&arg.value).as_str());
                s.push_str(" ");
            }
            s.push_str(") ");
            s.push_str(print_scheme_value(&body.value).as_str());
            s
        }
        _ => format!("{:?}", val),
    }
}
