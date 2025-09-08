use crate::cek::eval_main;
use crate::eval::RunTime;
/// Modular macro expansion engine for Scheme
use crate::gc::{
    GcRef, SchemeValue, car, cdr, get_nil, heap_list_iter, list_from_slice, list_to_vec, new_pair,
};
use crate::kont::CEKState;

#[derive(Debug)]
enum Expanded {
    Single(GcRef),
    Splice(Vec<GcRef>),
}

pub fn expand_macro(
    expr: &GcRef,
    depth: usize,
    rt: &mut RunTime,
    state: &mut CEKState,
) -> Result<GcRef, String> {
    match expand_macro_internal(expr, depth, rt, state)? {
        Expanded::Single(val) => Ok(val),
        Expanded::Splice(vals) => {
            let mut filtered: Vec<GcRef> = vals
                .into_iter()
                .filter(|v| !matches!(rt.heap.get_value(*v), SchemeValue::Nil))
                .collect();

            if filtered.is_empty() {
                Ok(get_nil(&mut rt.heap))
            } else if filtered.len() == 1 {
                Ok(filtered.remove(0))
            } else {
                Ok(list_from_slice(&filtered[..], &mut rt.heap))
            }
        }
    }
}

fn expand_macro_internal(
    expr: &GcRef,
    depth: usize,
    evaluator: &mut RunTime,
    state: &mut CEKState,
) -> Result<Expanded, String> {
    let expanded = match &evaluator.heap.get_value(*expr) {
        SchemeValue::Pair(_, _) => expand_list_pair(expr, depth, evaluator, state),
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
    rt: &mut RunTime,
    state: &mut CEKState,
) -> Result<Expanded, String> {
    let tag = list_ref(rt, expr, 0)?;
    if let SchemeValue::Symbol(sym) = &rt.heap.get_value(tag) {
        match sym.as_str() {
            "quasiquote" => {
                let body = list_ref(rt, expr, 1)?;
                let inner = expand_macro_internal(&body, depth + 1, rt, state)?;
                if depth == 0 {
                    return Ok(inner);
                } else {
                    return wrap("quasiquote", inner, rt);
                }
            }
            "unquote" => {
                if depth == 0 {
                    return Err("Unquote must be nested within a quasiquote".to_string());
                }
                let body = list_ref(rt, expr, 1)?;
                if depth == 1 {
                    let val = eval_main(body, state, rt)?;
                    return Ok(Expanded::Single(val[0]));
                } else {
                    let inner = expand_macro_internal(&body, depth - 1, rt, state)?;
                    return wrap("unquote", inner, rt);
                }
            }
            "unquote-splicing" => {
                if depth == 0 {
                    return Err("Unquote-splicing must be nested within a quasiquote".to_string());
                }
                let body = list_ref(rt, expr, 1)?;
                if depth == 1 {
                    let val = eval_main(body, state, rt)?;
                    if let SchemeValue::Pair(_, _) = &rt.heap.get_value(val[0]) {
                        // Proper list: flatten it
                        let vec = list_to_vec(&rt.heap, val[0])?;
                        return Ok(Expanded::Splice(vec));
                    } else if let SchemeValue::Nil = &rt.heap.get_value(val[0]) {
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
    let car_expanded = expand_macro_internal(&car, depth, rt, state)?;

    // Handle splice case
    match car_expanded {
        Expanded::Splice(mut hs) => {
            // Use GcRef to iterate without re-borrowing evaluator.heap
            let elements: Vec<GcRef> = heap_list_iter(&rt.heap, cdr).collect::<Result<_, _>>()?;

            for elem in elements {
                let expanded = expand_macro_internal(&elem, depth, rt, state)?;
                match expanded {
                    Expanded::Single(e) => hs.push(e),
                    Expanded::Splice(mut es) => hs.append(&mut es),
                }
            }
            Ok(Expanded::Splice(hs))
        }

        // Non-splice case — safe to evaluate rest now
        _ => {
            let cdr_expanded = expand_macro_internal(&cdr, depth, rt, state)?;

            match (car_expanded, cdr_expanded) {
                (Expanded::Single(h), Expanded::Single(t)) => {
                    Ok(Expanded::Single(new_pair(&mut rt.heap, h, t)))
                }
                (Expanded::Single(h), Expanded::Splice(ts)) => {
                    let mut vec = vec![h];
                    vec.extend(ts);
                    Ok(Expanded::Single(list_from_slice(&vec[..], &mut rt.heap)))
                }
                (Expanded::Splice(mut hs), Expanded::Single(t)) => {
                    if !matches!(rt.heap.get_value(t), SchemeValue::Nil) {
                        hs.push(t);
                    }
                    Ok(Expanded::Splice(hs))
                }
                (Expanded::Splice(mut hs), Expanded::Splice(mut ts)) => {
                    hs.append(&mut ts);
                    Ok(Expanded::Single(list_from_slice(&hs[..], &mut rt.heap)))
                }
            }
        }
    }
}

fn wrap(tag: &str, body: Expanded, evaluator: &mut RunTime) -> Result<Expanded, String> {
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

fn list_ref(ev: &mut RunTime, list: &GcRef, index: usize) -> Result<GcRef, String> {
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
