use crate::cek::eval_main;
use crate::eval::EvalContext;
/// Modular macro expansion engine for Scheme
use crate::gc::{
    GcRef, SchemeValue, car, cdr, get_nil, heap_list_iter, list_from_slice, list_to_vec, new_pair,
};

#[derive(Debug)]
enum Expanded {
    Single(GcRef),
    Splice(Vec<GcRef>),
}

pub fn expand_macro(
    expr: &GcRef,
    depth: usize,
    evaluator: &mut EvalContext,
) -> Result<GcRef, String> {
    match expand_macro_internal(expr, depth, evaluator)? {
        Expanded::Single(val) => Ok(val),
        Expanded::Splice(vals) => {
            let mut filtered: Vec<GcRef> = vals
                .into_iter()
                .filter(|v| !matches!(evaluator.heap.get_value(*v), SchemeValue::Nil))
                .collect();

            if filtered.is_empty() {
                Ok(get_nil(&mut evaluator.heap))
            } else if filtered.len() == 1 {
                Ok(filtered.remove(0))
            } else {
                Ok(list_from_slice(&filtered[..], &mut evaluator.heap))
            }
        }
    }
}

fn expand_macro_internal(
    expr: &GcRef,
    depth: usize,
    evaluator: &mut EvalContext,
) -> Result<Expanded, String> {
    let expanded = match &evaluator.heap.get_value(*expr) {
        SchemeValue::Pair(_, _) => expand_list_pair(expr, depth, evaluator),
        SchemeValue::Symbol(_) => expand_atom(expr),
        _ => Ok(Expanded::Single(*expr)),
    }?;

    // Fix: if we're at top-level (depth == 0), Splice must be wrapped into a list
    if depth == 0 {
        match expanded {
            Expanded::Single(_) => Ok(expanded),
            Expanded::Splice(vec) => {
                let list = list_from_slice(&vec[..], &mut evaluator.heap);
                Ok(Expanded::Single(list))
            }
        }
    } else {
        Ok(expanded)
    }
}

fn expand_atom(expr: &GcRef) -> Result<Expanded, String> {
    Ok(Expanded::Single(*expr))
}

fn expand_list_pair(
    expr: &GcRef,
    depth: usize,
    evaluator: &mut EvalContext,
) -> Result<Expanded, String> {
    let tag = list_ref(evaluator, expr, 0)?;
    if let SchemeValue::Symbol(sym) = &evaluator.heap.get_value(tag) {
        match sym.as_str() {
            "quasiquote" => {
                let body = list_ref(evaluator, expr, 1)?;
                let inner = expand_macro_internal(&body, depth + 1, evaluator)?;
                if depth == 0 {
                    return Ok(inner);
                } else {
                    return wrap("quasiquote", inner, evaluator);
                }
            }
            "unquote" => {
                if depth == 0 {
                    return Err("Unquote must be nested within a quasiquote".to_string());
                }
                let body = list_ref(evaluator, expr, 1)?;
                if depth == 1 {
                    let val = eval_main(body, evaluator)?;
                    return Ok(Expanded::Single(val[0]));
                } else {
                    let inner = expand_macro_internal(&body, depth - 1, evaluator)?;
                    return wrap("unquote", inner, evaluator);
                }
            }
            "unquote-splicing" => {
                if depth == 0 {
                    return Err("Unquote-splicing must be nested within a quasiquote".to_string());
                }
                let body = list_ref(evaluator, expr, 1)?;
                if depth == 1 {
                    let val = eval_main(body, evaluator)?;
                    if let SchemeValue::Pair(_, _) = &evaluator.heap.get_value(val[0]) {
                        // Proper list: flatten it
                        let vec = list_to_vec(&evaluator.heap, val[0])?;
                        return Ok(Expanded::Splice(vec));
                    } else if let SchemeValue::Nil = &evaluator.heap.get_value(val[0]) {
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
    // Extract car and cdr from heap first (produces GcRef, not references)
    let car = car(*expr)?;
    let cdr = cdr(*expr)?;

    // Then safely call into mutating code
    let car_expanded = expand_macro_internal(&car, depth, evaluator)?;

    // Handle splice case
    match car_expanded {
        Expanded::Splice(mut hs) => {
            // Use GcRef to iterate without re-borrowing evaluator.heap
            let elements: Vec<GcRef> =
                heap_list_iter(&evaluator.heap, cdr).collect::<Result<_, _>>()?;

            for elem in elements {
                let expanded = expand_macro_internal(&elem, depth, evaluator)?;
                match expanded {
                    Expanded::Single(e) => hs.push(e),
                    Expanded::Splice(mut es) => hs.append(&mut es),
                }
            }
            Ok(Expanded::Splice(hs))
        }

        // Non-splice case — safe to evaluate rest now
        _ => {
            let cdr_expanded = expand_macro_internal(&cdr, depth, evaluator)?;

            match (car_expanded, cdr_expanded) {
                (Expanded::Single(h), Expanded::Single(t)) => {
                    Ok(Expanded::Single(new_pair(&mut evaluator.heap, h, t)))
                }
                (Expanded::Single(h), Expanded::Splice(ts)) => {
                    let mut vec = vec![h];
                    vec.extend(ts);
                    Ok(Expanded::Single(list_from_slice(
                        &vec[..],
                        &mut evaluator.heap,
                    )))
                }
                (Expanded::Splice(mut hs), Expanded::Single(t)) => {
                    if !matches!(evaluator.heap.get_value(t), SchemeValue::Nil) {
                        hs.push(t);
                    }
                    Ok(Expanded::Splice(hs))
                }
                (Expanded::Splice(mut hs), Expanded::Splice(mut ts)) => {
                    hs.append(&mut ts);
                    Ok(Expanded::Single(list_from_slice(
                        &hs[..],
                        &mut evaluator.heap,
                    )))
                }
            }
        }
    }
}

fn wrap(tag: &str, body: Expanded, evaluator: &mut EvalContext) -> Result<Expanded, String> {
    let tag_sym = evaluator.heap.intern_symbol(tag);
    match body {
        Expanded::Single(val) => Ok(Expanded::Single(list_from_slice(
            &[tag_sym, val],
            &mut evaluator.heap,
        ))),
        Expanded::Splice(vals) => {
            let mut list = vec![tag_sym];
            list.extend(vals);
            Ok(Expanded::Single(list_from_slice(
                &list[..],
                &mut evaluator.heap,
            )))
        }
    }
}

fn list_ref(ev: &mut EvalContext, list: &GcRef, index: usize) -> Result<GcRef, String> {
    let mut current = *list;
    for _ in 0..index {
        current = match &ev.heap.get_value(current) {
            SchemeValue::Pair(_, cdr) => *cdr,
            _ => return Err("Invalid list structure".into()),
        };
    }
    match &ev.heap.get_value(current) {
        SchemeValue::Pair(car, _) => Ok(*car),
        _ => Err("Invalid list structure at index".into()),
    }
}
