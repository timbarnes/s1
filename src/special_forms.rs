/// special_forms.rs
/// Definitions of special forms implemented internally
///
use crate::cek::eval_main;
use crate::eval::{
    EvalContext, eval_callable, eval_non_tail, expect_at_least_n_args, expect_n_args,
    expect_symbol,
};
use crate::gc::{
    GcHeap, GcRef, SchemeValue, car, cdr, cons, get_nil, list_from_vec, list_to_vec, new_float,
    new_macro, new_port, new_special_form,
};
use crate::io::port_kind_from_scheme_port;
use crate::macros::expand_macro;
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
        "let" => let_sf,
        "if" => if_sf,
        "cond" => cond_sf,
        "begin" => begin_sf,
        "and" => and_sf,
        "or" => or_sf,
        "push-port!" => push_port_sf,
        "pop-port!" => pop_port_sf,
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
fn create_callable(expr: GcRef, evaluator: &mut EvalContext, tail: bool) -> Result<GcRef, String> {
    let form = expect_at_least_n_args(&evaluator.heap, expr, 3)?;
    let (params, ptype) = params_to_vec(&mut evaluator.heap, form[1]);
    create_lambda_or_macro(&form, &params, ptype, evaluator, tail)
}

fn create_lambda_or_macro(
    form: &Vec<GcRef>,
    params: &Vec<GcRef>,
    ptype: Ptype,
    evaluator: &mut EvalContext,
    _tail: bool,
) -> Result<GcRef, String> {
    use crate::gc::new_closure;
    //eprintln!("Creating lambda or macro");
    *evaluator.depth -= 1;

    let heap = &mut evaluator.heap;

    let mut args = Vec::new();

    match ptype {
        Ptype::Empty => { /* no params */ }

        Ptype::List => {
            let nil = get_nil(heap);
            args.push(nil);

            for arg in params.iter() {
                let val = heap.get_value(*arg); // <-- clone the SchemeValue
                if let SchemeValue::Symbol(name) = val {
                    let name: String = name.clone(); // force ownership — no ref
                    let sym = heap.intern_symbol(&name); // name is a String, passed as &str
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
    let wrapped_body = wrap_body_in_begin(body_exprs, evaluator.heap);

    // Intern and preserve parameter symbols
    let mut param_map = HashMap::new();
    for param in params {
        match &evaluator.heap.get_value(*param) {
            SchemeValue::Symbol(name) => {
                param_map.insert(name.clone(), *param);
            }
            _ => {
                return Err("lambda: parameter must be a symbol".to_string());
            }
        }
    }

    // let deduplicated_body =
    //     deduplicate_symbols_preserve_params(wrapped_body, evaluator.heap_mut(), &param_map);

    let captured_frame = evaluator.env.current_frame();
    match &evaluator.heap.get_value(form[0]) {
        SchemeValue::Symbol(name) => {
            if name == "lambda" || name == "let" {
                let new_closure = new_closure(
                    evaluator.heap,
                    args,
                    wrapped_body,
                    //deduplicated_body,
                    captured_frame,
                );
                Ok(new_closure)
            } else if name == "macro" {
                let new_macro = new_macro(
                    evaluator.heap,
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

fn eval_eval_sf(expr: GcRef, evaluator: &mut EvalContext, _tail: bool) -> Result<GcRef, String> {
    *evaluator.depth -= 1;
    let args = expect_n_args(&evaluator.heap, expr, 2)?;
    let result = eval_non_tail(&args[1], evaluator)?;
    eval_main(result, evaluator, true)
}

fn apply_sf(expr: GcRef, evaluator: &mut EvalContext, _tail: bool) -> Result<GcRef, String> {
    *evaluator.depth -= 1;
    let func = car(&evaluator.heap, cdr(&evaluator.heap, expr)?)?;
    let unevaluated_args = car(
        &evaluator.heap,
        cdr(&evaluator.heap, cdr(&evaluator.heap, expr)?)?,
    )?;
    let args = eval_non_tail(&unevaluated_args, evaluator)?;
    let apply_expr = cons(func, args, &mut evaluator.heap)?;
    eval_callable(apply_expr, evaluator, false)
}

/// Trace logic: turn the trace function on or off
/// This function takes an integer value, and sets evaluator.trace to match the argument.
/// BUG: SHOULD EVALUATE ITS ARGUMENT!
fn trace_sf(expr: GcRef, evaluator: &mut EvalContext, _tail: bool) -> Result<GcRef, String> {
    use num_traits::ToPrimitive;
    *evaluator.depth -= 1;
    let args = expect_n_args(&evaluator.heap, expr, 2)?;
    let trace_val = eval_main(args[1], evaluator, false)?;
    match &evaluator.heap.get_value(trace_val) {
        SchemeValue::Int(v) => match v.to_i32() {
            Some(value) => {
                *evaluator.trace = value;
            }
            None => return Err("trace: requires integer argument".to_string()),
        },
        _ => return Err("trace: requires integer argument".to_string()),
    }
    Ok(trace_val)
}

/// Quote logic: return first argument unevaluated
fn quote_sf(expr: GcRef, evaluator: &mut EvalContext, _tail: bool) -> Result<GcRef, String> {
    // (quote x) => return x unevaluated
    *evaluator.depth -= 1;
    match &evaluator.heap.get_value(expr) {
        SchemeValue::Pair(_, cdr) => match &evaluator.heap.get_value(*cdr) {
            SchemeValue::Pair(arg, _) => Ok(*arg),
            _ => Err("Malformed quote: missing argument".to_string()),
        },
        _ => Err("Malformed quote: not a pair".to_string()),
    }
}

/// (begin form1 ..)
fn begin_sf(expr: GcRef, evaluator: &mut EvalContext, _tail: bool) -> Result<GcRef, String> {
    // (begin expr1 expr2 ... exprN) => evaluate each in sequence, return last
    *evaluator.depth -= 1;
    let argvec = expect_at_least_n_args(&evaluator.heap, expr, 2)?;
    //let mut result = get_nil(&mut evaluator.heap);
    for arg in argvec[..argvec.len() - 1].iter() {
        //result = eval_main(*arg, evaluator, tail && i == argvec.len() - 1)?;
        eval_non_tail(arg, evaluator)?;
    }
    let result = eval_main(*argvec.last().unwrap(), evaluator, true)?;
    Ok(result)
}

/// (define sym expr)
pub fn define_sf(expr: GcRef, evaluator: &mut EvalContext, _tail: bool) -> Result<GcRef, String> {
    // (define symbol expr)
    let args = expect_n_args(&evaluator.heap, expr, 3)?; // including 'define
    let sym = expect_symbol(&mut evaluator.heap, &args[1])?;
    let value = eval_non_tail(&args[2], evaluator)?;
    evaluator.env.set_symbol(sym, value);
    Ok(sym)
}

/// (if test consequent alternate)
/// Requires three arguments.
pub fn if_sf(expr: GcRef, evaluator: &mut EvalContext, _tail: bool) -> Result<GcRef, String> {
    *evaluator.depth -= 1;
    // (if test consequent [alternate])
    let args = expect_at_least_n_args(&evaluator.heap, expr, 2)?;
    let test = eval_non_tail(&args[1], evaluator)?;
    match evaluator.heap.get_value(test) {
        SchemeValue::Bool(val) => {
            if *val {
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
pub fn cond_sf(expr: GcRef, evaluator: &mut EvalContext, _tail: bool) -> Result<GcRef, String> {
    *evaluator.depth -= 1;
    let args = expect_at_least_n_args(&evaluator.heap, expr, 2)?;

    for clause in &args[1..] {
        let (test, body): (GcRef, GcRef) = match evaluator.heap.get_value(*clause) {
            SchemeValue::Pair(t, b) => (*t, *b),
            _ => return Err("cond: clause is not a pair".to_string()),
        };

        let test_is_else: Option<String>;
        {
            let val = evaluator.heap.get_value(test);
            test_is_else = match val {
                SchemeValue::Symbol(s) => Some(s.clone()),
                _ => None,
            };
        } // borrow ends

        let test_result = match test_is_else {
            Some(ref s) if s == "else" => true,
            _ => {
                let evaluated = eval_non_tail(&test, evaluator)?;
                !matches!(
                    evaluator.heap.get_value(evaluated),
                    SchemeValue::Bool(false)
                )
            }
        };

        if test_result {
            let body_vec = list_to_vec(&evaluator.heap, body)?;
            let count = body_vec.len();
            if count > 2 {
                for expr in body_vec[..count - 1].iter() {
                    eval_non_tail(expr, evaluator)?;
                }
            }
            let result = eval_main(body_vec[count - 1], evaluator, true);
            return result;
        }
    }

    Ok(get_nil(&mut evaluator.heap))
}

/// let (basic version, without labels)
pub fn let_sf(expr: GcRef, evaluator: &mut EvalContext, tail: bool) -> Result<GcRef, String> {
    let formvec = expect_at_least_n_args(&evaluator.heap, expr, 3)?;
    let bindings = list_to_vec(&mut evaluator.heap, formvec[1])?; // (let bindings . body)

    // Separate bindings into variables and expressions
    let mut vars = get_nil(evaluator.heap);
    let mut exprs = vars;
    // Build them up as lists
    for binding in bindings.into_iter().rev() {
        match &evaluator.heap.get_value(binding) {
            SchemeValue::Pair(var, binding_expr) => {
                let var_sym = expect_symbol(&evaluator.heap, &var)?;
                let binding_expr = car(&evaluator.heap, *binding_expr)?;
                vars = cons(var_sym, vars, evaluator.heap)?;
                exprs = cons(binding_expr, exprs, evaluator.heap)?;
            }
            _ => return Err("Invalid binding in let".to_string()),
        }
    }
    // println!("let vars: {}", print_scheme_value(&vars.value));
    // println!("let exprs: {}", print_scheme_value(&exprs.value));
    let (params, ptype) = params_to_vec(&mut evaluator.heap, vars);
    // println!("let params: {:?}", params);
    let lambda_expr = create_lambda_or_macro(&formvec, &params, ptype, evaluator, false)?;

    // cons the lambda to the list of values
    let call = cons(lambda_expr, exprs, evaluator.heap)?;

    eval_main(call, evaluator, tail)
}

fn expand_sf(expr: GcRef, evaluator: &mut EvalContext, _tail: bool) -> Result<GcRef, String> {
    //let ec = EvalContext::from_eval(evaluator);
    let m = car(&evaluator.heap, cdr(&evaluator.heap, expr)?)?;
    // println!(
    //     "expand_sf: {}",
    //     print_scheme_value(&mut ec, ec.heap.get_value(m))
    // );
    expand_macro(&m, 0, evaluator)
}

/// (and expr1 expr2 ... exprN)
/// Stops on first false value
pub fn and_sf(expr: GcRef, evaluator: &mut EvalContext, _tail: bool) -> Result<GcRef, String> {
    *evaluator.depth -= 1;
    // (and expr1 expr2 ... exprN)
    let exprs = expect_at_least_n_args(evaluator.heap, expr, 1)?;

    // Returns true if no args
    if exprs.len() == 1 {
        return Ok(evaluator.heap.true_s());
    }
    let mut val = evaluator.heap.false_s();
    for e in exprs.into_iter().skip(1) {
        val = eval_non_tail(&e, evaluator)?;
        match &evaluator.heap.get_value(val) {
            SchemeValue::Bool(false) => return Ok(evaluator.heap.false_s()),
            _ => continue,
        }
    }
    Ok(val)
}

/// (or expr1 expr2 ... exprN)
/// Stops on first true value
pub fn or_sf(expr: GcRef, evaluator: &mut EvalContext, _tail: bool) -> Result<GcRef, String> {
    *evaluator.depth -= 1;
    // (or expr1 expr2 ... exprN)
    match evaluator.heap.get_value(expr) {
        SchemeValue::Pair(_, _) => {
            let exprs = list_to_vec(&evaluator.heap, expr)?;
            for e in exprs.into_iter().skip(1) {
                let val = eval_non_tail(&e, evaluator)?;
                match &evaluator.heap.get_value(val) {
                    SchemeValue::Bool(false) => continue,
                    _ => return Ok(val),
                }
            }
            Ok(evaluator.heap.false_s())
        }
        _ => Err("or: not a proper list".to_string()),
    }
}

/// (set! sym expr)
/// sym must have been previously defined.
pub fn set_sf(expr: GcRef, evaluator: &mut EvalContext, _tail: bool) -> Result<GcRef, String> {
    *evaluator.depth -= 1;
    let args = expect_n_args(&evaluator.heap, expr, 3)?;
    let sym = expect_symbol(&evaluator.heap, &args[1])?;
    let value = eval_non_tail(&args[2], evaluator)?;

    match evaluator.env.get_symbol_and_frame(sym) {
        Some((_, sym_frame)) => {
            let current_frame = evaluator.env.current_frame();
            evaluator.env.set_current_frame(sym_frame);
            evaluator.env.set_symbol(sym, value);
            evaluator.env.set_current_frame(current_frame);
            return Ok(value);
        }
        None => return Err("set!: symbol not found".to_string()),
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
pub fn push_port_sf(
    expr: GcRef,
    evaluator: &mut EvalContext,
    _tail: bool,
) -> Result<GcRef, String> {
    // (push-port! port)
    *evaluator.depth -= 1;
    let args = expect_n_args(&evaluator.heap, expr, 2)?;
    let value = eval_non_tail(&args[1], evaluator)?;
    let port_kind = port_kind_from_scheme_port(evaluator, value);
    evaluator.port_stack.push(port_kind);
    Ok(evaluator.heap.true_s())
}

pub fn pop_port_sf(
    _expr: GcRef,
    evaluator: &mut EvalContext,
    _tail: bool,
) -> Result<GcRef, String> {
    // (pop-port!)
    *evaluator.depth -= 1;
    let port_kind = evaluator.port_stack.pop();
    match port_kind {
        Some(port_kind) => {
            let port = new_port(&mut evaluator.heap, port_kind);
            Ok(port)
        }
        None => Err("Port Stack is empty".to_string()),
    }
}

fn with_timer_sf(expr: GcRef, evaluator: &mut EvalContext, _tail: bool) -> Result<GcRef, String> {
    let args = expect_n_args(&evaluator.heap, expr, 2)?;
    let timer = Instant::now();
    eval_non_tail(&args[1], evaluator)?;
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
