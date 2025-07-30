use crate::env::Frame;
use crate::eval::{
    Evaluator, bind_params, eval_logic, expect_at_least_n_args, expect_n_args, expect_symbol,
};
use crate::gc::{GcRef, SchemeValue, get_nil};
use crate::macros::expand_macro;
use crate::printer::print_scheme_value;
use std::cell::RefCell;
//use std::collections::HashMap;
use std::rc::Rc;

/// Macro handler
pub fn eval_macro_logic(
    params: &[GcRef],
    body: GcRef,
    env: &Rc<RefCell<Frame>>,
    args: &[GcRef],
    evaluator: &mut Evaluator,
) -> Result<GcRef, String> {
    // Bind unevaluated arguments to macro parameters
    let new_env = bind_params(params, args, env, evaluator.heap_mut())?;
    let original_env = evaluator.env_mut().current_frame();
    evaluator
        .env_mut()
        .set_current_frame(new_env.current_frame());
    // Expand the macro
    //println!("Unexpanded macro: {}", print_scheme_value(&body.value));
    let expanded = expand_macro(&body, 0, evaluator)?;
    println!("After expansion: {}", print_scheme_value(&expanded.value));

    evaluator.env_mut().set_current_frame(original_env);
    eval_logic(expanded, evaluator)
}

// fn quasiquote_logic(expr: GcRef, evaluator: &mut Evaluator) -> Result<GcRef, String> {
//     evaluator.depth -= 1;
//     let args = expect_n_args(expr, 2)?;
//     let body = args[1];
//     crate::macros::expand_macro(&body, 0, evaluator)
// }

/// Trace logic: turn the trace function on or off
/// This function takes an integer value, and sets evaluator.trace to match the argument.
fn trace_logic(expr: GcRef, evaluator: &mut Evaluator) -> Result<GcRef, String> {
    use num_traits::ToPrimitive;
    evaluator.depth -= 1;
    let args = expect_n_args(expr, 2)?;
    let trace_val = eval_logic(args[1], evaluator)?;
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
fn quote_logic(expr: GcRef, evaluator: &mut Evaluator) -> Result<GcRef, String> {
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
fn begin_logic(expr: GcRef, evaluator: &mut Evaluator) -> Result<GcRef, String> {
    // (begin expr1 expr2 ... exprN) => evaluate each in sequence, return last
    evaluator.depth -= 1;
    match &expr.value {
        SchemeValue::Pair(_, cdr) => {
            let mut current = *cdr;
            let mut last_result = None;
            loop {
                match &current.value {
                    SchemeValue::Nil => break,
                    SchemeValue::Pair(car, next) => {
                        last_result = Some(eval_logic(*car, evaluator)?);
                        current = *next;
                    }
                    _ => return Err("Malformed begin: improper list".to_string()),
                }
            }
            last_result.ok_or_else(|| "Malformed begin: no expressions".to_string())
        }
        _ => Err("Malformed begin: not a pair".to_string()),
    }
}

/// (define sym expr)
pub fn define_logic(expr: GcRef, evaluator: &mut Evaluator) -> Result<GcRef, String> {
    // (define symbol expr)
    let args = expect_n_args(expr, 3)?; // including 'define
    let sym = expect_symbol(&args[1])?;
    let value = eval_logic(args[2], evaluator)?;
    evaluator.env_mut().set_symbol(sym, value);
    Ok(sym)
}

/// (if test consequent alternate)
/// Requires three arguments.
pub fn if_logic(expr: GcRef, evaluator: &mut Evaluator) -> Result<GcRef, String> {
    evaluator.depth -= 1;
    // (if test consequent [alternate])
    let args = expect_at_least_n_args(expr, 2)?;
    let test = eval_logic(args[1], evaluator)?;
    match test.value {
        SchemeValue::Bool(val) => {
            if val {
                return eval_logic(args[2], evaluator);
            } else {
                if args.len() == 4 {
                    return eval_logic(args[3], evaluator);
                } else {
                    return Ok(get_nil(&mut evaluator.heap));
                }
            }
        }
        _ => return Err("if: invalid test expression".to_string()),
    }
}

/// (cond (test1 expr) [(test 2...)] [(else expr)])
pub fn cond_logic(expr: GcRef, evaluator: &mut Evaluator) -> Result<GcRef, String> {
    evaluator.depth -= 1;
    let args = expect_n_args(expr, 3)?;

    for clause in &args[1..] {
        match &clause.value {
            SchemeValue::Pair(test, body) => {
                let test_result = match &test.value {
                    SchemeValue::Symbol(s) if s == "else" => true,
                    _ => {
                        let evaluated = eval_logic(test, evaluator)?;
                        !matches!(evaluated.value, SchemeValue::Bool(false))
                    }
                };

                if test_result {
                    // Evaluate all expressions in the body
                    let mut result = get_nil(&mut evaluator.heap);
                    let mut current = *body;
                    while let SchemeValue::Pair(car, cdr) = &current.value {
                        result = eval_logic(car, evaluator)?;
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
pub fn and_logic(expr: GcRef, evaluator: &mut Evaluator) -> Result<GcRef, String> {
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
                        let val = eval_logic(*car, evaluator)?;
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
pub fn or_logic(expr: GcRef, evaluator: &mut Evaluator) -> Result<GcRef, String> {
    evaluator.depth -= 1;
    // (or expr1 expr2 ... exprN)
    match &expr.value {
        SchemeValue::Pair(_, cdr) => {
            let mut current = *cdr;
            loop {
                match &current.value {
                    SchemeValue::Nil => break,
                    SchemeValue::Pair(car, next) => {
                        let val = eval_logic(*car, evaluator)?;
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
pub fn set_logic(expr: GcRef, evaluator: &mut Evaluator) -> Result<GcRef, String> {
    evaluator.depth -= 1;
    let args = expect_n_args(expr, 3)?;
    let sym = expect_symbol(&args[1])?;
    let value = eval_logic(args[2], evaluator)?;

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
pub fn push_port_logic(expr: GcRef, evaluator: &mut Evaluator) -> Result<GcRef, String> {
    // (push-port! port)
    evaluator.depth -= 1;
    match &expr.value {
        SchemeValue::Pair(_, cdr) => {
            // Extract the argument from the cdr
            match &cdr.value {
                SchemeValue::Pair(arg, _) => {
                    // Evaluate the argument to get the port value
                    let port = eval_logic(*arg, evaluator)?;
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

pub fn pop_port_logic(expr: GcRef, evaluator: &mut Evaluator) -> Result<GcRef, String> {
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
