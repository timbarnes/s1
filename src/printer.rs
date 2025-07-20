use crate::gc::{SchemeValue, SchemeValueSimple};

pub fn scheme_display(val: &SchemeValue) -> String {
    match val {
        SchemeValue::Int(i) => i.to_string(),
        SchemeValue::Float(f) => f.to_string(),
        SchemeValue::Bool(true) => "#t".to_string(),
        SchemeValue::Bool(false) => "#f".to_string(),
        SchemeValue::Char(c) => format!("#\\{}", c),
        SchemeValue::Str(s) => format!("\"{}\"", s),
        SchemeValue::Nil => "()".to_string(),
        SchemeValue::Symbol(s) => s.clone(),
        SchemeValue::Pair(car, cdr) => {
            let mut s = String::from("(");
            let mut cur = Some(car.clone());
            let mut cdr_ref = cdr.clone();
            loop {
                s.push_str(&scheme_display(&cur.as_ref().unwrap().borrow().value));
                // Only borrow cdr_ref for this block
                let next = {
                    let cdr_borrow = cdr_ref.borrow();
                    match &cdr_borrow.value {
                        SchemeValue::Nil => {
                            s.push(')');
                            None
                        }
                        SchemeValue::Pair(next_car, next_cdr) => {
                            Some((next_car.clone(), next_cdr.clone()))
                        }
                        _ => {
                            s.push_str(" . ");
                            s.push_str(&scheme_display(&cdr_borrow.value));
                            s.push(')');
                            None
                        }
                    }
                };
                if let Some((next_car, next_cdr)) = next {
                    s.push(' ');
                    cur = Some(next_car);
                    cdr_ref = next_cdr;
                } else {
                    break;
                }
            }
            s
        }
        SchemeValue::Vector(v) => {
            let contents = v.iter()
                .map(|elem| scheme_display(&elem.borrow().value))
                .collect::<Vec<_>>()
                .join(" ");
            format!("#({})", contents)
        }
        SchemeValue::Primitive { .. } => "#<procedure>".to_string(),
        SchemeValue::Closure { .. } => "#<closure>".to_string(),
        SchemeValue::EnvFrame(_) => "#<env-frame>".to_string(),
        SchemeValue::SpecialForm { .. } => "#<special-form>".to_string(),
    }
}

pub fn print_scheme_value(val: &SchemeValueSimple) -> String {
    match val {
        SchemeValueSimple::Pair(_, _) => {
            let mut s = String::from("(");
            let mut first = true;
            let mut current = val;
            loop {
                match current {
                    SchemeValueSimple::Pair(car, cdr) => {
                        if !first { s.push(' '); }
                        s.push_str(&print_scheme_value(&car.value));
                        current = &cdr.value;
                        first = false;
                    }
                    SchemeValueSimple::Nil => {
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
        SchemeValueSimple::Symbol(s) => s.clone(),
        SchemeValueSimple::Int(i) => i.to_string(),
        SchemeValueSimple::Float(f) => f.to_string(),
        SchemeValueSimple::Str(s) => s.clone(), // raw string, no quotes
        SchemeValueSimple::Bool(true) => "#t".to_string(),
        SchemeValueSimple::Bool(false) => "#f".to_string(),
        SchemeValueSimple::Char(c) => format!("#\\{}", c),
        SchemeValueSimple::Nil => "nil".to_string(),
        _ => format!("{:?}", val),
    }
} 