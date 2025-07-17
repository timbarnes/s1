use crate::gc::SchemeValue;

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
    }
} 