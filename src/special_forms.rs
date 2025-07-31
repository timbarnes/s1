/// special_forms.rs
/// Definitions of special forms implemented internally
///
use crate::eval::{
    Evaluator, deduplicate_symbols_preserve_params, eval_callable, eval_main,
    expect_at_least_n_args, expect_n_args, expect_symbol,
};
use crate::gc::{
    GcHeap, GcRef, SchemeValue, car, cdr, cons, get_nil, list_from_vec, new_float, new_macro,
    new_special_form,
};
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
        "trace" => trace_sf,
        "quote" => quote_sf,
        "define" => define_sf,
        "set!" => set_sf,
        "if" => if_sf,
        "cond" => cond_sf,
        "begin" => begin_sf,
        "and" => and_sf,
        "or" => or_sf,
        "push-port!" => push_port_sf,
        "pop-port!" => pop_port_sf,
        "lambda" => create_callable,
        "macro" => create_callable,
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
fn create_callable(expr: GcRef, evaluator: &mut Evaluator, _tail: bool) -> Result<GcRef, String> {
    // (lambda params body ..)
    use crate::gc::new_closure;
    //use crate::gc_util::{list_from_vec, list_to_vec};
    evaluator.depth -= 1;

    let form = expect_at_least_n_args(expr, 3)?;
    // Process argument lists
    let (params, ptype) = params_to_vec(&form[1]); // Special processing for the range of argument structures
    let mut args: Vec<&crate::gc::GcObject> = Vec::new();
    match ptype {
        Ptype::Empty => {}
        Ptype::List => {
            args.push(get_nil(evaluator.heap_mut()));
            for arg in params.iter() {
                match &arg.value {
                    SchemeValue::Symbol(name) => {
                        args.push(evaluator.heap_mut().intern_symbol(&name));
                    }
                    _ => (),
                }
            }
        }
        Ptype::Variadic => match &params[0].value {
            SchemeValue::Symbol(name) => {
                args.push(evaluator.heap_mut().intern_symbol(&name));
            }
            _ => (),
        },
        Ptype::Dotted => {
            for arg in params.iter() {
                match &arg.value {
                    SchemeValue::Symbol(name) => {
                        args.push(evaluator.heap_mut().intern_symbol(&name));
                    }
                    _ => (),
                }
            }
        }
    }
    // Process body
    let body_exprs = form[2..].to_vec();

    // Wrap the body expressions in (begin ...) if needed
    let wrapped_body = wrap_body_in_begin(body_exprs, evaluator.heap_mut());

    // Intern and preserve parameter symbols
    let mut param_map = HashMap::new();
    for param in &params {
        match &param.value {
            SchemeValue::Symbol(name) => {
                param_map.insert(name.clone(), *param);
            }
            _ => {
                return Err("lambda: parameter must be a symbol".to_string());
            }
        }
    }

    let deduplicated_body =
        deduplicate_symbols_preserve_params(wrapped_body, evaluator.heap_mut(), &param_map);

    let captured_frame = evaluator.env().current_frame();
    match &form[0].value {
        SchemeValue::Symbol(name) => {
            if name == "lambda" {
                let new_closure = new_closure(
                    evaluator.heap_mut(),
                    args,
                    deduplicated_body,
                    captured_frame,
                );
                Ok(new_closure)
            } else if name == "macro" {
                let new_macro = new_macro(
                    evaluator.heap_mut(),
                    args,
                    deduplicated_body,
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

fn eval_eval_sf(expr: GcRef, evaluator: &mut Evaluator, tail: bool) -> Result<GcRef, String> {
    evaluator.depth -= 1;
    let args = expect_n_args(expr, 2)?;
    let result = eval_main(args[1], evaluator, tail)?;
    eval_main(result, evaluator, tail)
}

fn apply_sf(expr: GcRef, evaluator: &mut Evaluator, tail: bool) -> Result<GcRef, String> {
    evaluator.depth -= 1;
    let func = car(cdr(expr)?)?;
    let unevaluated_args = car(cdr(cdr(expr)?)?)?;
    let args = eval_main(unevaluated_args, evaluator, tail)?;
    let apply_expr = cons(func, args, &mut evaluator.heap)?;
    eval_callable(apply_expr, evaluator, tail)
}

/// Trace logic: turn the trace function on or off
/// This function takes an integer value, and sets evaluator.trace to match the argument.
fn trace_sf(expr: GcRef, evaluator: &mut Evaluator, tail: bool) -> Result<GcRef, String> {
    use num_traits::ToPrimitive;
    evaluator.depth -= 1;
    let args = expect_n_args(expr, 2)?;
    let trace_val = eval_main(args[1], evaluator, tail)?;
    match &trace_val.value {
        SchemeValue::Int(v) => match v.to_i32() {
            Some(value) => {
                evaluator.trace = value;
            }
            None => return Err("trace: requires integer argument".to_string()),
        },
        _ => return Err("trace: requires integer argument".to_string()),
    }
    Ok(trace_val)
}

/// Quote logic: return first argument unevaluated
fn quote_sf(expr: GcRef, evaluator: &mut Evaluator, _tail: bool) -> Result<GcRef, String> {
    // (quote x) => return x unevaluated
    evaluator.depth -= 1;
    match &expr.value {
        SchemeValue::Pair(_, cdr) => match &cdr.value {
            SchemeValue::Pair(arg, _) => Ok(*arg),
            _ => Err("Malformed quote: missing argument".to_string()),
        },
        _ => Err("Malformed quote: not a pair".to_string()),
    }
}

/// (begin form1 ..)
fn begin_sf(expr: GcRef, evaluator: &mut Evaluator, tail: bool) -> Result<GcRef, String> {
    // (begin expr1 expr2 ... exprN) => evaluate each in sequence, return last
    evaluator.depth -= 1;
    let argvec = expect_at_least_n_args(expr, 2)?;
    let mut result = get_nil(&mut evaluator.heap);
    for (i, arg) in argvec.iter().enumerate() {
        result = eval_main(*arg, evaluator, tail && i == argvec.len() - 1)?;
    }
    Ok(result)
}

/// (define sym expr)
pub fn define_sf(expr: GcRef, evaluator: &mut Evaluator, _tail: bool) -> Result<GcRef, String> {
    // (define symbol expr)
    let args = expect_n_args(expr, 3)?; // including 'define
    let sym = expect_symbol(&args[1])?;
    let value = eval_main(args[2], evaluator, true)?;
    evaluator.env_mut().set_symbol(sym, value);
    Ok(sym)
}

/// (if test consequent alternate)
/// Requires three arguments.
pub fn if_sf(expr: GcRef, evaluator: &mut Evaluator, _tail: bool) -> Result<GcRef, String> {
    evaluator.depth -= 1;
    // (if test consequent [alternate])
    let args = expect_at_least_n_args(expr, 2)?;
    let test = eval_main(args[1], evaluator, false)?;
    match test.value {
        SchemeValue::Bool(val) => {
            if val {
                return eval_main(args[2], evaluator, true);
            } else {
                if args.len() == 4 {
                    return eval_main(args[3], evaluator, true);
                } else {
                    return Ok(get_nil(&mut evaluator.heap));
                }
            }
        }
        _ => return Err("if: invalid test expression".to_string()),
    }
}

/// (cond (test1 expr) [(test 2...)] [(else expr)])
pub fn cond_sf(expr: GcRef, evaluator: &mut Evaluator, tail: bool) -> Result<GcRef, String> {
    evaluator.depth -= 1;
    let args = expect_at_least_n_args(expr, 2)?;

    for clause in &args[1..] {
        match &clause.value {
            SchemeValue::Pair(test, body) => {
                let test_result = match &test.value {
                    SchemeValue::Symbol(s) if s == "else" => true,
                    _ => {
                        let evaluated = eval_main(test, evaluator, tail)?;
                        !matches!(evaluated.value, SchemeValue::Bool(false))
                    }
                };

                if test_result {
                    // Evaluate all expressions in the body
                    let mut result = get_nil(&mut evaluator.heap);
                    let mut current = *body;
                    while let SchemeValue::Pair(car, cdr) = &current.value {
                        result = eval_main(car, evaluator, false)?;
                        current = cdr;
                    }
                    return Ok(result);
                }
            }
            _ => return Err("cond: clause must be a pair".to_string()),
        }
    }

    Ok(get_nil(&mut evaluator.heap))
}

/// (and expr1 expr2 ... exprN)
/// Stops on first false value
pub fn and_sf(expr: GcRef, evaluator: &mut Evaluator, _tail: bool) -> Result<GcRef, String> {
    evaluator.depth -= 1;
    // (and expr1 expr2 ... exprN)
    match &expr.value {
        SchemeValue::Pair(_, cdr) => {
            let mut current = *cdr;
            let mut last = evaluator.heap.true_s();
            loop {
                match &current.value {
                    SchemeValue::Nil => break,
                    SchemeValue::Pair(car, next) => {
                        // Review for tail recursion
                        let val = eval_main(*car, evaluator, false)?;
                        match &val.value {
                            SchemeValue::Bool(false) => return Ok(val),
                            _ => last = val,
                        }
                        current = *next;
                    }
                    _ => return Err("and: improper list".to_string()),
                }
            }
            Ok(last)
        }
        _ => Err("and: not a pair".to_string()),
    }
}

/// (or expr1 expr2 ... exprN)
/// Stops on first true value
pub fn or_sf(expr: GcRef, evaluator: &mut Evaluator, _tail: bool) -> Result<GcRef, String> {
    evaluator.depth -= 1;
    // (or expr1 expr2 ... exprN)
    match &expr.value {
        SchemeValue::Pair(_, cdr) => {
            let mut current = *cdr;
            loop {
                match &current.value {
                    SchemeValue::Nil => break,
                    SchemeValue::Pair(car, next) => {
                        // Review for tail recursion
                        let val = eval_main(*car, evaluator, false)?;
                        match &val.value {
                            SchemeValue::Bool(false) => current = *next,
                            _ => return Ok(val),
                        }
                    }
                    _ => return Err("or: improper list".to_string()),
                }
            }
            Ok(evaluator.heap.false_s())
        }
        _ => Err("or: not a pair".to_string()),
    }
}

/// (set! sym expr)
/// sym must have been previously defined.
pub fn set_sf(expr: GcRef, evaluator: &mut Evaluator, tail: bool) -> Result<GcRef, String> {
    evaluator.depth -= 1;
    let args = expect_n_args(expr, 3)?;
    let sym = expect_symbol(&args[1])?;
    let value = eval_main(args[2], evaluator, tail)?;

    if evaluator.env().get_symbol(sym).is_some() {
        evaluator.env_mut().set_symbol(sym, value);
        Ok(value)
    } else {
        Err("set!: unbound variable".to_string())
    }
}

/// (push-port! port)
/// Pushes a port onto the port stack, causing the evaluator to load scheme code from it.
/// At EOF, the evaluator will pop the port and continue evaluating code from the next port on the stack.
///
/// # Examples
///
/// ```
/// let mut evaluator = Evaluator::new();
/// evaluator.push_port("example.txt");
/// ```
pub fn push_port_sf(expr: GcRef, evaluator: &mut Evaluator, _tail: bool) -> Result<GcRef, String> {
    // (push-port! port)
    evaluator.depth -= 1;
    match &expr.value {
        SchemeValue::Pair(_, cdr) => {
            // Extract the argument from the cdr
            match &cdr.value {
                SchemeValue::Pair(arg, _) => {
                    // Evaluate the argument to get the port value
                    let port = eval_main(*arg, evaluator, true)?;
                    let port_stack_sym = evaluator.heap.intern_symbol("**port-stack**");
                    let current_stack = evaluator
                        .env()
                        .get_symbol(port_stack_sym)
                        .unwrap_or_else(|| evaluator.heap.nil_s());
                    let new_stack = crate::gc::new_pair(evaluator.heap_mut(), port, current_stack);
                    evaluator.env_mut().set_symbol(port_stack_sym, new_stack);
                    evaluator.new_port = true;
                    Ok(evaluator.heap.true_s())
                }
                _ => Err("push-port!: expected 1 argument".to_string()),
            }
        }
        _ => Err("push-port!: expected 1 argument".to_string()),
    }
}

pub fn pop_port_sf(expr: GcRef, evaluator: &mut Evaluator, _tail: bool) -> Result<GcRef, String> {
    // (pop-port!)
    use crate::gc::SchemeValue;
    evaluator.depth -= 1;
    match &expr.value {
        SchemeValue::Pair(_, cdr) => {
            // Check that cdr is nil (no arguments)
            match &cdr.value {
                SchemeValue::Nil => {
                    let port_stack_sym = evaluator.heap.intern_symbol("**port-stack**");
                    let current_stack = evaluator
                        .env()
                        .get_symbol(port_stack_sym)
                        .unwrap_or_else(|| evaluator.heap.nil_s());
                    match &current_stack.value {
                        SchemeValue::Pair(_car, cdr) => {
                            evaluator.env_mut().set_symbol(port_stack_sym, *cdr);
                            Ok(evaluator.heap.true_s())
                        }
                        SchemeValue::Nil => Err("pop-port!: **port-stack** is empty".to_string()),
                        _ => Err("pop-port!: **port-stack** is not a proper list".to_string()),
                    }
                }
                _ => Err("pop-port!: expected 0 arguments".to_string()),
            }
        }
        _ => Err("pop-port!: expected 0 arguments".to_string()),
    }
}

fn with_timer_sf(expr: GcRef, evaluator: &mut Evaluator, tail: bool) -> Result<GcRef, String> {
    let args = expect_n_args(expr, 2)?;
    let timer = Instant::now();
    eval_main(args[1], evaluator, tail)?;
    let elapsed_time = timer.elapsed().as_secs_f64();
    let time = new_float(&mut evaluator.heap, elapsed_time);
    Ok(time)
}

/// Utility functions
///

fn wrap_body_in_begin(body_exprs: Vec<GcRef>, heap: &mut GcHeap) -> GcRef {
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
fn params_to_vec(mut list: GcRef) -> (Vec<GcRef>, Ptype) {
    let mut result = Vec::new();
    match &list.value {
        SchemeValue::Nil => return (result, Ptype::Empty),
        SchemeValue::Symbol(_) => {
            result.push(&list);
            return (result, Ptype::Variadic);
        }
        _ => (),
    }
    loop {
        match &list.value {
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
