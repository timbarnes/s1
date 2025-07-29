use crate::eval::Evaluator;
use crate::eval::eval_logic;
/// Modular macro expansion engine for Scheme
use crate::gc::{GcRef, SchemeValue, car, cdr, get_nil, list_from_vec, list_to_vec, new_pair};

#[derive(Debug)]
enum Expanded {
    Single(GcRef),
    Splice(Vec<GcRef>),
}

pub fn expand_macro(
    expr: &GcRef,
    depth: usize,
    evaluator: &mut Evaluator,
) -> Result<GcRef, String> {
    match expand_macro_internal(expr, depth, evaluator)? {
        Expanded::Single(val) => Ok(val),
        Expanded::Splice(vals) => {
            let mut filtered: Vec<GcRef> = vals
                .into_iter()
                .filter(|v| !matches!(v.value, SchemeValue::Nil))
                .collect();

            if filtered.is_empty() {
                Ok(get_nil(&mut evaluator.heap))
            } else if filtered.len() == 1 {
                Ok(filtered.remove(0))
            } else {
                Ok(list_from_vec(filtered, &mut evaluator.heap))
            }
        }
    }
}

fn expand_macro_internal(
    expr: &GcRef,
    depth: usize,
    evaluator: &mut Evaluator,
) -> Result<Expanded, String> {
    match &expr.value {
        SchemeValue::Pair(_, _) => expand_list_pair(expr, depth, evaluator),
        SchemeValue::Symbol(_) => expand_atom(expr),
        _ => Ok(Expanded::Single(*expr)),
    }
}

fn expand_atom(expr: &GcRef) -> Result<Expanded, String> {
    Ok(Expanded::Single(*expr))
}

fn expand_list_pair(
    expr: &GcRef,
    depth: usize,
    evaluator: &mut Evaluator,
) -> Result<Expanded, String> {
    let tag = list_ref(expr, 0)?;
    if let SchemeValue::Symbol(sym) = &tag.value {
        match sym.as_str() {
            "quasiquote" => {
                let body = list_ref(expr, 1)?;
                let inner = expand_macro_internal(&body, depth + 1, evaluator)?;
                if depth == 0 {
                    return Ok(inner);
                } else {
                    return wrap("quasiquote", inner, evaluator);
                }
            }
            "unquote" => {
                let body = list_ref(expr, 1)?;
                if depth == 1 {
                    let val = eval_logic(&body, evaluator)?;
                    return Ok(Expanded::Single(val));
                } else {
                    let inner = expand_macro_internal(&body, depth - 1, evaluator)?;
                    return wrap("unquote", inner, evaluator);
                }
            }
            "unquote-splicing" => {
                let body = list_ref(expr, 1)?;
                if depth == 1 {
                    let val = eval_logic(&body, evaluator)?;
                    if let SchemeValue::Pair(_, _) = &val.value {
                        // Proper list: flatten it
                        let vec = list_to_vec(&val)?;
                        return Ok(Expanded::Splice(vec));
                    } else if let SchemeValue::Nil = &val.value {
                        return Ok(Expanded::Splice(vec![]));
                    } else {
                        return Err("Splice requires a proper list".to_string());
                    }
                }
            }
            _ => {}
        }
    }

    // Generic list case (not special form)
    let car = car(expr)?;
    let cdr = cdr(expr)?;

    let car_expanded = expand_macro_internal(&car, depth, evaluator)?;
    let cdr_expanded = expand_macro_internal(&cdr, depth, evaluator)?;

    match (car_expanded, cdr_expanded) {
        (Expanded::Single(h), Expanded::Single(t)) => {
            Ok(Expanded::Single(new_pair(&mut evaluator.heap, h, t)))
        }
        (Expanded::Single(h), Expanded::Splice(mut ts)) => {
            let mut vec = vec![h];
            vec.append(&mut ts);
            Ok(Expanded::Splice(vec))
        }
        (Expanded::Splice(mut hs), Expanded::Single(t)) => {
            hs.push(t);
            Ok(Expanded::Splice(hs))
        }
        (Expanded::Splice(mut hs), Expanded::Splice(mut ts)) => {
            hs.append(&mut ts);
            Ok(Expanded::Splice(hs))
        }
    }
}

fn wrap(tag: &str, body: Expanded, evaluator: &mut Evaluator) -> Result<Expanded, String> {
    let tag_sym = evaluator.heap.intern_symbol(tag);
    match body {
        Expanded::Single(val) => Ok(Expanded::Single(list_from_vec(
            vec![tag_sym, val],
            &mut evaluator.heap,
        ))),
        Expanded::Splice(vals) => {
            let mut list = vec![tag_sym];
            list.extend(vals);
            Ok(Expanded::Single(list_from_vec(list, &mut evaluator.heap)))
        }
    }
}

fn list_ref(list: &GcRef, index: usize) -> Result<GcRef, String> {
    let mut current = *list;
    for _ in 0..index {
        current = match &current.value {
            SchemeValue::Pair(_, cdr) => *cdr,
            _ => return Err("Invalid list structure".into()),
        };
    }
    match &current.value {
        SchemeValue::Pair(car, _) => Ok(*car),
        _ => Err("Invalid list structure at index".into()),
    }
}
