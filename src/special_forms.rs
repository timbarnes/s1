/// special_forms.rs
/// Definitions of special forms implemented internally
///
/// Special forms are given the full expression, including the name of the form.
/// This is required for lambda and macro differentiation.
///
use crate::cek::{
    AndOrKind, CEKState, CondClause, eval_main, insert_and_or, insert_bind, insert_cond, 
    insert_eval, insert_eval_eval, insert_if, insert_seq, insert_value,
};
use crate::eval::{EvalContext, expect_at_least_n_args, expect_n_args, expect_symbol};
use crate::gc::{
    GcHeap, GcRef, SchemeValue, car, cdr, cons, get_nil, list_from_vec, list_to_vec, matches_sym,
    new_float, new_macro, new_special_form,
};
use crate::gc_value;
use crate::macros::expand_macro;
use crate::printer::print_scheme_value;
use std::collections::HashMap;
use std::time::Instant;

enum Ptype {
    Empty,    // No arguments to function
    Variadic, // Variadic argument to function
    List,     // List of arguments to function
    Dotted,   // Dotted list of arguments to function
}

/// Macro to register special forms in the environment
///
/// Usage: register_special_form!(heap, env,
///     "name" => function,
///     "another" => another_function,
/// );
macro_rules! register_special_form {
    ($heap:expr, $env:expr, $($name:expr => $func:expr),* $(,)?) => {
        $(
            $env.set_symbol($heap.intern_symbol($name),
                new_special_form($heap, $func,
                    concat!($name, ": special form").to_string()));
        )*
    };
}

pub fn register_special_forms(heap: &mut GcHeap, env: &mut crate::env::Environment) {
    register_special_form!(heap, env,
        "eval" => eval_eval_sf,
        "apply" => apply_sf,
        "quote" => quote_sf,
        "define" => define_sf,
        "set!" => set_sf,
        "let" => let_sf,
        "if" => if_sf,
        "cond" => cond_sf,
        "begin" => begin_sf,
        "and" => and_sf,
        "or" => or_sf,
        "lambda" => create_callable,
        "macro" => create_callable,
        "expand" => expand_sf,
        "with-timer" => with_timer_sf,
    );
}

/// Callable logic: create a closure or a macro with captured environment
/// (lambda (params...) body1 body2 ...) => return closure
/// (macro (params...) body1 body2 ...) => return macro
///     params can take one of four forms:
///     - an empty vec, meaning no arguments
///     - a vec with a single entry, meaning variadic arguments bound as a list
///     - a vec with multiple entries following a nil first entry, meaning named arguments
///     - a vec with variadic arguments bound as a list and named arguments
///
fn create_callable(expr: GcRef, ec: &mut EvalContext, state: &mut CEKState) -> Result<(), String> {
    let form = expect_at_least_n_args(&ec.heap, expr, 3)?;
    let (params, ptype) = params_to_vec(&mut ec.heap, form[1]);
    let closure = create_lambda_or_macro(&form, &params, ptype, ec).unwrap();
    insert_value(state, closure);
    Ok(())
}

fn create_lambda_or_macro(
    form: &Vec<GcRef>,
    params: &Vec<GcRef>,
    ptype: Ptype,
    ec: &mut EvalContext,
) -> Result<GcRef, String> {
    use crate::gc::new_closure;
    //eprintln!("Creating lambda or macro");
    *ec.depth -= 1;

    let heap = &mut ec.heap;
    let mut args = Vec::new();

    match ptype {
        Ptype::Empty => { /* no params */ }
        Ptype::List => {
            let nil = get_nil(heap);
            args.push(nil);

            for arg in params.iter() {
                let val = heap.get_value(*arg);
                if let SchemeValue::Symbol(name) = val {
                    let name: String = name.clone();
                    let sym = heap.intern_symbol(&name);
                    args.push(sym);
                }
            }
        }
        Ptype::Variadic => {
            match heap.get_value(params[0]) {
                SchemeValue::Symbol(name) => {
                    let name: String = name.clone();
                    let sym = heap.intern_symbol(&name);
                    args.push(sym);
                }
                _ => { /* ignore */ }
            }
        }
        Ptype::Dotted => {
            for arg in params.iter() {
                match heap.get_value(*arg) {
                    SchemeValue::Symbol(name) => {
                        let name: String = name.clone();
                        let sym = heap.intern_symbol(&name);
                        args.push(sym);
                    }
                    _ => { /* ignore */ }
                }
            }
        }
    }
    // Process body
    let body_exprs = form[2..].to_vec();

    // Wrap the body expressions in (begin ...) if needed
    let wrapped_body = wrap_body_in_begin(body_exprs, ec.heap);

    // Intern and preserve parameter symbols
    let mut param_map = HashMap::new();
    for param in params {
        match &ec.heap.get_value(*param) {
            SchemeValue::Symbol(name) => {
                param_map.insert(name.clone(), *param);
            }
            _ => {
                return Err("lambda: parameter must be a symbol".to_string());
            }
        }
    }

    let captured_frame = ec.env.current_frame();
    match &ec.heap.get_value(form[0]) {
        SchemeValue::Symbol(name) => {
            if name == "lambda" || name == "let" {
                let new_closure = new_closure(
                    ec.heap,
                    args,
                    wrapped_body,
                    //deduplicated_body,
                    captured_frame,
                );
                Ok(new_closure)
            } else if name == "macro" {
                let new_macro = new_macro(
                    ec.heap,
                    args,
                    wrapped_body,
                    //deduplicated_body,
                    captured_frame,
                );
                Ok(new_macro)
            } else {
                return Err("Callable must be lambda or macro".to_string());
            }
        }
        _ => return Err("Callable type must be a symbol".to_string()),
    }
}

fn eval_eval_sf(expr: GcRef, ec: &mut EvalContext, state: &mut CEKState) -> Result<(), String> {
    *ec.depth -= 1;
    let argvec = expect_n_args(&ec.heap, expr, 2)?;
    match argvec.len() {
        2 => insert_eval_eval(state, argvec[1], None, false),
        3 => insert_eval_eval(state, argvec[1],Some(argvec[2]), false),
        _ => return Err("eval: requires 1 or 2 arguments".to_string()),
    }
    Ok(())
}

fn apply_sf(expr: GcRef, ec: &mut EvalContext, state: &mut CEKState) -> Result<(), String> {
    *ec.depth -= 1;
    let func = car(&ec.heap, cdr(&ec.heap, expr)?)?;
    let unevaluated_args = car(&ec.heap, cdr(&ec.heap, cdr(&ec.heap, expr)?)?)?;
    let args = eval_main(unevaluated_args, ec)?;
    let apply_expr = cons(func, args, &mut ec.heap)?;
    insert_eval(state, apply_expr, true);
    Ok(())
}

/// Quote logic: return first argument unevaluated
fn quote_sf(expr: GcRef, ec: &mut EvalContext, state: &mut CEKState) -> Result<(), String> {
    // (quote x) => return x unevaluated
    *ec.depth -= 1;
    match &ec.heap.get_value(expr) {
        SchemeValue::Pair(_, cdr) => match &ec.heap.get_value(*cdr) {
            SchemeValue::Pair(_, _) => {
                let arg = car(ec.heap, *cdr)?;
                insert_value(state, arg);
                Ok(())
            }
            _ => Err("Malformed quote: missing argument".to_string()),
        },
        _ => Err("Malformed quote: not a pair".to_string()),
    }
}

/// (begin form1 ..)
fn begin_sf(expr: GcRef, ec: &mut EvalContext, state: &mut CEKState) -> Result<(), String> {
    // (begin expr1 expr2 ... exprN) => evaluate each in sequence, return last
    *ec.depth -= 1;
    let mut argvec = expect_at_least_n_args(&ec.heap, expr, 2)?;

    match argvec.len() {
        0 => return Err("begin: no arguments provided".to_string()),
        1 => insert_value(state, ec.heap.false_s()),
        2 => insert_eval(state, argvec[1], true),
        _ => {
            // If there are more than two arguments, evaluate the sequence.
            argvec = argvec[1..].to_vec();
            insert_seq(state, argvec);
        }
    }
    Ok(())
}

/// (define sym expr)
pub fn define_sf(expr: GcRef, ec: &mut EvalContext, state: &mut CEKState) -> Result<(), String> {
    // (define symbol expr)
    let args = expect_n_args(&ec.heap, expr, 3)?; // including 'define
    let sym = expect_symbol(&mut ec.heap, &args[1])?;
    insert_bind(state, sym);
    insert_eval(state, args[2], false);
    Ok(())
}

/// (if test consequent alternate)
/// Requires three arguments.
pub fn if_sf(expr: GcRef, evaluator: &mut EvalContext, state: &mut CEKState) -> Result<(), String> {
    *evaluator.depth -= 1;
    let args = expect_at_least_n_args(&evaluator.heap, expr, 3)?;
    insert_if(state, args[2], args[3]);
    insert_eval(state, args[1], false);
    Ok(())
}

/// (cond (test1 expr) [(test 2...)] [(else expr)])
pub fn cond_sf(expr: GcRef, ec: &mut EvalContext, state: &mut CEKState) -> Result<(), String> {
    *ec.depth -= 1;
    let mut args = expect_at_least_n_args(&ec.heap, expr, 2)?;
    args = args.into_iter().skip(1).collect();
    args.reverse();

    let mut clauses = Vec::new();
    for clause in args.iter() { 
        let parts = list_to_vec(ec.heap, *clause)?;
        if parts.is_empty() {
            return Err("cond: clause cannot be empty".to_string());
        }

        if matches_sym(parts[0], "else") {
            let body = if parts.len() == 1 {
                None
            } else {
                Some(wrap_body_in_begin(parts[1..].to_vec(), ec.heap))
            };
            clauses.push(CondClause::Normal {
                test: ec.heap.true_s(),
                body,
            });
        } else if parts.len() >= 2 && matches_sym(parts[1], "=>") {
            clauses.push(CondClause::Arrow {
                test: parts[0],
                arrow_proc: parts[2],
            });
        } else {
            let body = if parts.len() == 1 {
                Some(parts[0])
            } else {
                Some(wrap_body_in_begin(parts[1..].to_vec(), ec.heap))
            };
            clauses.push(CondClause::Normal {
                test: parts[0],
                body,
            });
        }
    }

insert_cond(state, clauses);
Ok(())
}

/// let (basic version, without labels)
pub fn let_sf(expr: GcRef, ec: &mut EvalContext, state: &mut CEKState) -> Result<(), String> {
    let formvec = expect_at_least_n_args(&ec.heap, expr, 3)?;
    let bindings = list_to_vec(&mut ec.heap, formvec[1])?; // (let bindings . body)

    // Separate bindings into variables and expressions
    let mut vars = get_nil(ec.heap);
    let mut exprs = vars;
    // Build them up as lists
    for binding in bindings.into_iter().rev() {
        match &ec.heap.get_value(binding) {
            SchemeValue::Pair(var, binding_expr) => {
                let var_sym = expect_symbol(&ec.heap, &var)?;
                let binding_expr = car(&ec.heap, *binding_expr)?;
                vars = cons(var_sym, vars, ec.heap)?;
                exprs = cons(binding_expr, exprs, ec.heap)?;
            }
            _ => return Err("Invalid binding in let".to_string()),
        }
    }
    // println!("let vars: {}", print_scheme_value(&vars.value));
    // println!("let exprs: {}", print_scheme_value(&exprs.value));
    let (params, ptype) = params_to_vec(&mut ec.heap, vars);
    // println!("let params: {:?}", params);
    let lambda_expr = create_lambda_or_macro(&formvec, &params, ptype, ec)?;
    // cons the lambda to the list of values
    let call = cons(lambda_expr, exprs, ec.heap)?;

    insert_eval(state, call, false);
    Ok(())
}

fn expand_sf(expr: GcRef, ec: &mut EvalContext, _state: &mut CEKState) -> Result<(), String> {
    let m = car(&ec.heap, cdr(&ec.heap, expr)?)?;
    expand_macro(&m, 0, ec)?;
    Ok(())
}

/// (and expr1 expr2 ... exprN)
/// Stops on first false value
pub fn and_sf(expr: GcRef, ec: &mut EvalContext, state: &mut CEKState) -> Result<(), String> {
    *ec.depth -= 1;
    // (and expr1 expr2 ... exprN)
    let mut argvec = expect_at_least_n_args(&ec.heap, expr, 1)?;

    match argvec.len() {
        0 => return Err("and: no arguments provided".to_string()),
        1 => insert_value(state, ec.heap.true_s()),
        2 => insert_eval(state, argvec[1], true),
        _ => {
            // If there are more than two arguments, evaluate the sequence.
            argvec = argvec[1..].to_vec();

            insert_and_or(state, AndOrKind::And, argvec);
        }
    }
    Ok(())
}

/// (or expr1 expr2 ... exprN)
/// Stops on first true value
pub fn or_sf(expr: GcRef, ec: &mut EvalContext, state: &mut CEKState) -> Result<(), String> {
    *ec.depth -= 1;
    // (and expr1 expr2 ... exprN)
    let mut argvec = expect_at_least_n_args(&ec.heap, expr, 1)?;

    match argvec.len() {
        0 => return Err("or: no arguments provided".to_string()),
        1 => insert_value(state, ec.heap.false_s()),
        2 => insert_eval(state, argvec[1], true),
        _ => {
            // If there are more than two arguments, evaluate the sequence.
            argvec = argvec[1..].to_vec();
            insert_and_or(state, AndOrKind::Or, argvec);
        }
    }
    Ok(())
}

/// (set! sym expr)
/// sym must have been previously defined.
pub fn set_sf(expr: GcRef, ec: &mut EvalContext, state: &mut CEKState) -> Result<(), String> {
    *ec.depth -= 1;
    let args = expect_n_args(&ec.heap, expr, 3)?;
    let sym = expect_symbol(&ec.heap, &args[1])?;
    
    match &ec.env.get_symbol(sym) {
        Some(_) => {
            insert_bind(state, sym);
            insert_eval(state, args[2], false);
            Ok(())
        }
        None => Err("set!: symbol not found".to_string())
    }
}

fn with_timer_sf(expr: GcRef, ec: &mut EvalContext, state: &mut CEKState) -> Result<(), String> {
    let args = expect_n_args(&ec.heap, expr, 2)?;
    let timer = Instant::now();
    eval_main(args[1], ec)?;
    let elapsed_time = timer.elapsed().as_secs_f64();
    let time = new_float(&mut ec.heap, elapsed_time);
    insert_value(state, time);
    Ok(())
}

/// Utility functions
///

pub fn wrap_body_in_begin(body_exprs: Vec<GcRef>, heap: &mut GcHeap) -> GcRef {
    if body_exprs.len() == 1 {
        body_exprs[0]
    } else {
        // Create (begin expr1 expr2 ...)
        let begin_sym = heap.intern_symbol("begin");
        let mut exprs = body_exprs;
        exprs.insert(0, begin_sym);
        list_from_vec(exprs, heap)
    }
}

// Convert a parameter list, returning the list and a flag indicating the type of list
fn params_to_vec(heap: &mut GcHeap, mut list: GcRef) -> (Vec<GcRef>, Ptype) {
    let mut result = Vec::new();
    match &heap.get_value(list) {
        SchemeValue::Nil => return (result, Ptype::Empty),
        SchemeValue::Symbol(_) => {
            result.push(list);
            return (result, Ptype::Variadic);
        }
        _ => (),
    }
    loop {
        match &heap.get_value(list) {
            SchemeValue::Nil => break (result, Ptype::List), // norma case
            SchemeValue::Pair(car, cdr) => {
                result.push(*car);
                list = *cdr;
            }
            SchemeValue::Symbol(_) => {
                // Dotted list tail
                result.insert(0, list);
                return (result, Ptype::Dotted);
            }
            _ => (),
        }
    }
}
