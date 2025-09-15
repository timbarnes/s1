/// special_forms.rs
/// Definitions of special forms implemented internally
///
/// Special forms are given the full expression, including the name of the form.
/// This is required for lambda and macro differentiation.
///
use crate::cek::eval_main;
use crate::env::{EnvOps, EnvRef};
use crate::eval::{RunTime, expect_at_least_n_args, expect_n_args, expect_symbol};
use crate::gc::{
    GcHeap, GcRef, SchemeValue, car, cdr, cons, get_nil, list_from_slice, list_to_vec, list2,
    matches_sym, new_float, new_macro, new_pair, new_special_form,
};
use crate::kont::{
    AndOrKind, CEKState, CondClause, insert_and_or, insert_bind, insert_cond, insert_eval,
    insert_if, insert_seq, insert_value,
};
use crate::macros::expand_macro;
use crate::utilities::post_error;
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
    ($rt:expr, $env:expr, $($name:expr => $func:expr),* $(,)?) => {
        $(
            $env.define($rt.intern_symbol($name),
                new_special_form($rt, $func,
                    concat!($name, ": special form").to_string()));
        )*
    };
}

pub fn register_special_forms(heap: &mut GcHeap, env: EnvRef) {
    register_special_form!(heap, env,
        "quote" => quote_sf,
        "define" => define_sf,
        "set!" => set_sf,
        "let" => let_sf,
        "let*" => let_star_sf,
        "letrec" => letrec_sf,
        "do" => do_sf,
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
pub fn create_callable(expr: GcRef, ec: &mut RunTime, state: &mut CEKState) -> Result<(), String> {
    let form = expect_at_least_n_args(&ec.heap, expr, 3)?;
    let (params, ptype) = params_to_vec(&mut ec.heap, form[1]);
    let closure = create_lambda_or_macro(&form, &params, ptype, ec, state.env.clone());
    match closure {
        Ok(closure) => {
            insert_value(state, closure);
            Ok(())
        }
        Err(err) => Err(err),
    }
}

fn create_lambda_or_macro(
    form: &Vec<GcRef>,
    params: &Vec<GcRef>,
    ptype: Ptype,
    ec: &mut RunTime,
    env: EnvRef,
) -> Result<GcRef, String> {
    use crate::gc::new_closure;
    //eprintln!("Creating lambda or macro");

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

    let captured_frame = env;
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

/// Quote logic: return first argument unevaluated
fn quote_sf(expr: GcRef, ec: &mut RunTime, state: &mut CEKState) -> Result<(), String> {
    // (quote x) => return x unevaluated
    match &ec.heap.get_value(expr) {
        SchemeValue::Pair(_, cdr) => match &ec.heap.get_value(*cdr) {
            SchemeValue::Pair(_, _) => {
                let arg = car(*cdr)?;
                insert_value(state, arg);
                Ok(())
            }
            _ => {
                post_error(state, ec, "Malformed quote: missing argument");
                Ok(())
            }
        },
        _ => {
            post_error(state, ec, "Malformed quote: not a pair");
            Ok(())
        }
    }
}

/// (begin form1 ..)
fn begin_sf(expr: GcRef, ec: &mut RunTime, state: &mut CEKState) -> Result<(), String> {
    // (begin expr1 expr2 ... exprN) => evaluate each in sequence, return last
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
pub fn define_sf(expr: GcRef, ec: &mut RunTime, state: &mut CEKState) -> Result<(), String> {
    // (define symbol expr)
    let args = expect_n_args(&ec.heap, expr, 3)?; // including 'define
    let sym = expect_symbol(&mut ec.heap, &args[1])?;

    insert_bind(state, sym, None);
    insert_eval(state, args[2], false);
    Ok(())
}

/// (set! sym expr)
/// sym must have been previously defined.
pub fn set_sf(expr: GcRef, ec: &mut RunTime, state: &mut CEKState) -> Result<(), String> {
    let args = expect_n_args(&ec.heap, expr, 3)?;
    //let sym = expect_symbol(&ec.heap, &args[1])?;

    match &state.env.lookup_with_frame(args[1]) {
        Some((_val, binding_env)) => {
            insert_bind(state, args[1], Some(binding_env.clone()));
            insert_eval(state, args[2], false);
            Ok(())
        }
        None => Err("set!: symbol not found".to_string()),
    }
}

/// (if test consequent alternate)
/// Requires three arguments.
pub fn if_sf(expr: GcRef, evaluator: &mut RunTime, state: &mut CEKState) -> Result<(), String> {
    let args = expect_at_least_n_args(&evaluator.heap, expr, 3);
    match args {
        Ok(a) => {
            let else_clause = if a.len() == 4 {
                a[3]
            } else {
                evaluator.heap.unspecified()
            };
            insert_if(state, a[2], else_clause);
            insert_eval(state, a[1], false);
            Ok(())
        }
        Err(e) => Err(e),
    }
}

/// (cond (test1 expr) [(test 2...)] [(else expr)])
pub fn cond_sf(expr: GcRef, ec: &mut RunTime, state: &mut CEKState) -> Result<(), String> {
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

/// let (basic version, with named let support)
pub fn let_sf(expr: GcRef, ec: &mut RunTime, state: &mut CEKState) -> Result<(), String> {
    let formvec = expect_at_least_n_args(&ec.heap, expr, 3)?;

    // Check if this is named let: (let name bindings body...)
    // vs normal let: (let bindings body...)
    match &ec.heap.get_value(formvec[1]) {
        SchemeValue::Symbol(_) => {
            // Named let: (let name bindings body...)
            if formvec.len() < 4 {
                return Err("Named let requires at least 4 arguments".to_string());
            }

            let name = formvec[1];
            let bindings = list_to_vec(&mut ec.heap, formvec[2])?;
            let body_exprs = formvec[3..].to_vec();

            // Extract parameters and initial values
            let mut params = Vec::new();
            let mut init_vals = Vec::new();

            for binding in bindings.iter() {
                match &ec.heap.get_value(*binding) {
                    SchemeValue::Pair(var, binding_expr) => {
                        let var_sym = expect_symbol(&ec.heap, &var)?;
                        let binding_expr = car(*binding_expr)?;
                        params.push(var_sym);
                        init_vals.push(binding_expr);
                    }
                    _ => return Err("Invalid binding in named let".to_string()),
                }
            }

            // Transform to: (letrec ((name (lambda (params...) body...))) (name init_vals...))
            let lambda_sym = ec.heap.intern_symbol("lambda");
            let params_list = list_from_slice(&params, ec.heap);
            let body = wrap_body_in_begin(body_exprs, ec.heap);

            let lambda_expr = list_from_slice(&[lambda_sym, params_list, body], ec.heap);
            let name_binding = list_from_slice(&[name, lambda_expr], ec.heap);
            let name_bindings = list_from_slice(&[name_binding], ec.heap);

            let mut call_args = vec![name];
            call_args.extend_from_slice(&init_vals);
            let call_expr = list_from_slice(&call_args, ec.heap);

            let letrec_sym = ec.heap.intern_symbol("letrec");
            let letrec_expr = list_from_slice(&[letrec_sym, name_bindings, call_expr], ec.heap);

            insert_eval(state, letrec_expr, false);
            Ok(())
        }
        _ => {
            // Normal let: (let bindings body...)
            let bindings = list_to_vec(&mut ec.heap, formvec[1])?; // (let bindings . body)

            // Separate bindings into variables and expressions
            let mut vars = get_nil(ec.heap);
            let mut exprs = vars;
            // Build them up as lists
            for binding in bindings.into_iter().rev() {
                match &ec.heap.get_value(binding) {
                    SchemeValue::Pair(var, binding_expr) => {
                        let var_sym = expect_symbol(&ec.heap, &var)?;
                        let binding_expr = car(*binding_expr)?;
                        vars = cons(var_sym, vars, ec.heap)?;
                        exprs = cons(binding_expr, exprs, ec.heap)?;
                    }
                    _ => return Err("Invalid binding in let".to_string()),
                }
            }
            let (params, ptype) = params_to_vec(&mut ec.heap, vars);
            let lambda_expr =
                create_lambda_or_macro(&formvec, &params, ptype, ec, state.env.clone())?;
            // cons the lambda to the list of values
            let call = cons(lambda_expr, exprs, ec.heap)?;

            insert_eval(state, call, false);
            Ok(())
        }
    }
}

/// let*
pub fn let_star_sf(expr: GcRef, ec: &mut RunTime, state: &mut CEKState) -> Result<(), String> {
    let formvec = expect_at_least_n_args(&ec.heap, expr, 3)?;
    let bindings = list_to_vec(&mut ec.heap, formvec[1])?; // (let* bindings . body)

    if bindings.is_empty() {
        // No bindings, just evaluate the body in sequence
        let body_exprs = formvec[2..].to_vec();
        let wrapped_body = wrap_body_in_begin(body_exprs, ec.heap);
        insert_eval(state, wrapped_body, false);
        return Ok(());
    }

    // Transform (let* ((v1 e1) (v2 e2) ...) body) into nested lets:
    // (let ((v1 e1)) (let* ((v2 e2) ...) body))
    let first_binding = bindings[0];
    let remaining_bindings = list_from_slice(&bindings[1..], ec.heap);

    // Create the inner let* or body
    let inner_expr = if bindings.len() == 1 {
        // Last binding, just use the body
        wrap_body_in_begin(formvec[2..].to_vec(), ec.heap)
    } else {
        // More bindings, create nested let*
        let let_star_sym = ec.heap.intern_symbol("let*");
        let wrapped_body = wrap_body_in_begin(formvec[2..].to_vec(), ec.heap);
        list_from_slice(&[let_star_sym, remaining_bindings, wrapped_body], ec.heap)
    };

    // Create the outer let with first binding
    let let_sym = ec.heap.intern_symbol("let");
    let first_binding_list = list_from_slice(&[first_binding], ec.heap);
    let outer_let = list_from_slice(&[let_sym, first_binding_list, inner_expr], ec.heap);

    insert_eval(state, outer_let, false);
    Ok(())
}

/// letrec (recursive binding version)
pub fn letrec_sf(expr: GcRef, ec: &mut RunTime, state: &mut CEKState) -> Result<(), String> {
    let formvec = expect_at_least_n_args(&ec.heap, expr, 3)?;
    let bindings = list_to_vec(&mut ec.heap, formvec[1])?; // (letrec bindings . body)

    if bindings.is_empty() {
        // No bindings, just evaluate the body in sequence
        let body_exprs = formvec[2..].to_vec();
        let wrapped_body = wrap_body_in_begin(body_exprs, ec.heap);
        insert_eval(state, wrapped_body, false);
        return Ok(());
    }

    // Transform (letrec ((v1 e1) (v2 e2) ...) body) into:
    // (let ((v1 #f) (v2 #f) ...) (set! v1 e1) (set! v2 e2) ... body)

    // Create initial bindings with #f values
    let mut init_bindings = Vec::new();
    let mut set_exprs = Vec::new();
    let false_val = ec.heap.false_s();
    let set_sym = ec.heap.intern_symbol("set!");

    for binding in bindings.iter() {
        match &ec.heap.get_value(*binding) {
            SchemeValue::Pair(var, binding_expr) => {
                let var_sym = expect_symbol(&ec.heap, &var)?;
                let binding_expr = car(*binding_expr)?;

                // Create (var #f) binding
                let init_binding = list2(var_sym, false_val, ec.heap)?;
                init_bindings.push(init_binding);

                // Create (set! var expr)
                let set_expr = list_from_slice(&[set_sym, var_sym, binding_expr], ec.heap);
                set_exprs.push(set_expr);
            }
            _ => return Err("Invalid binding in letrec".to_string()),
        }
    }

    // Build the complete expression
    let let_sym = ec.heap.intern_symbol("let");
    let init_bindings_list = list_from_slice(&init_bindings[..], ec.heap);

    // Combine set! expressions with body
    let mut all_exprs = set_exprs;
    all_exprs.extend_from_slice(&formvec[2..]);

    let let_body = wrap_body_in_begin(all_exprs, ec.heap);
    let letrec_as_let = list_from_slice(&[let_sym, init_bindings_list, let_body], ec.heap);

    insert_eval(state, letrec_as_let, false);
    Ok(())
}

fn expand_sf(expr: GcRef, ec: &mut RunTime, state: &mut CEKState) -> Result<(), String> {
    let m = car(cdr(expr)?)?;
    match expand_macro(&m, 0, ec, state) {
        Ok(expr) => {
            insert_value(state, expr);
        }
        Err(err) => return Err(err),
    }
    Ok(())
}

/// (and expr1 expr2 ... exprN)
/// Stops on first false value
pub fn and_sf(expr: GcRef, ec: &mut RunTime, state: &mut CEKState) -> Result<(), String> {
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
pub fn or_sf(expr: GcRef, ec: &mut RunTime, state: &mut CEKState) -> Result<(), String> {
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

fn with_timer_sf(expr: GcRef, ec: &mut RunTime, state: &mut CEKState) -> Result<(), String> {
    let args = expect_n_args(&ec.heap, expr, 2)?;
    let timer = Instant::now();
    eval_main(args[1], state, ec)?;
    let elapsed_time = timer.elapsed().as_secs_f64();
    let time = new_float(&mut ec.heap, elapsed_time);
    insert_value(state, time);
    Ok(())
}

/// do (iterative loop construct)
/// Syntax: (do ((var1 init1 step1) (var2 init2 step2) ...) (test expr ...) command ...)
pub fn do_sf(expr: GcRef, ec: &mut RunTime, state: &mut CEKState) -> Result<(), String> {
    let formvec = expect_at_least_n_args(&ec.heap, expr, 3)?;

    let bindings = list_to_vec(&mut ec.heap, formvec[1])?;
    let test_clause = list_to_vec(&mut ec.heap, formvec[2])?;
    let commands = formvec[3..].to_vec();

    if test_clause.is_empty() {
        return Err("do: test clause cannot be empty".to_string());
    }

    let test_expr = test_clause[0];
    let result_exprs = test_clause[1..].to_vec();

    // Extract variables, initial values, and step expressions
    let mut vars = Vec::new();
    let mut init_vals = Vec::new();
    let mut step_exprs = Vec::new();

    for binding in bindings.iter() {
        let binding_parts = list_to_vec(&mut ec.heap, *binding)?;

        if binding_parts.len() < 2 || binding_parts.len() > 3 {
            return Err("do: binding must have 2 or 3 elements (var init [step])".to_string());
        }

        let var = expect_symbol(&ec.heap, &binding_parts[0])?;
        let init = binding_parts[1];
        let step = if binding_parts.len() == 3 {
            binding_parts[2]
        } else {
            var // If no step, use the variable itself (no change)
        };

        vars.push(var);
        init_vals.push(init);
        step_exprs.push(step);
    }

    // Transform to: (let loop ((var1 init1) (var2 init2) ...)
    //                 (if test
    //                     (begin result_expr ...)
    //                     (begin command ... (loop step1 step2 ...))))

    let let_sym = ec.heap.intern_symbol("let");
    let loop_sym = ec.heap.intern_symbol("loop");
    let if_sym = ec.heap.intern_symbol("if");
    let begin_sym = ec.heap.intern_symbol("begin");

    // Create initial bindings: ((var1 init1) (var2 init2) ...)
    let mut init_bindings = Vec::new();
    for (var, init) in vars.iter().zip(init_vals.iter()) {
        init_bindings.push(list_from_slice(&[*var, *init], ec.heap));
    }
    let init_bindings_list = list_from_slice(&init_bindings, ec.heap);

    // Create result expression: (begin result_expr ...) or unspecified if empty
    let result_expr = if result_exprs.is_empty() {
        ec.heap.unspecified()
    } else if result_exprs.len() == 1 {
        result_exprs[0]
    } else {
        let mut begin_exprs = vec![begin_sym];
        begin_exprs.extend_from_slice(&result_exprs);
        list_from_slice(&begin_exprs, ec.heap)
    };

    // Create loop call: (loop step1 step2 ...)
    let mut loop_call = vec![loop_sym];
    loop_call.extend_from_slice(&step_exprs);
    let loop_call_expr = list_from_slice(&loop_call, ec.heap);

    // Create loop body: (begin command ... (loop step1 step2 ...))
    let loop_body = if commands.is_empty() {
        loop_call_expr
    } else {
        let mut body_exprs = vec![begin_sym];
        body_exprs.extend_from_slice(&commands);
        body_exprs.push(loop_call_expr);
        list_from_slice(&body_exprs, ec.heap)
    };

    // Create if expression: (if test result_expr loop_body)
    let if_expr = list_from_slice(&[if_sym, test_expr, result_expr, loop_body], ec.heap);

    // Create the complete named let: (let loop ((var1 init1) ...) (if ...))
    let named_let = list_from_slice(&[let_sym, loop_sym, init_bindings_list, if_expr], ec.heap);

    insert_eval(state, named_let, false);
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
        list_from_slice(&exprs[..], heap)
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
