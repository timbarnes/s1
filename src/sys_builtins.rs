use crate::cek::apply_proc;
use crate::eval::EvalContext;
use crate::gc::{
    Callable, GcHeap, GcRef, SchemeValue, get_symbol, list, list_to_vec, list3, new_continuation,
    new_sys_builtin,
};
use crate::gc_value;
use crate::kont::{CEKState, Control, Kont, KontRef, insert_eval_eval};
//use crate::printer::print_value;
use crate::special_forms::create_callable;
use crate::utilities::dump_cek;
use std::rc::Rc;

/// System Builtin Functions
///
/// These functions are classified as procedures, and receive evaluated arguments.
/// They differ from normal builtins in that they also have access to the CEK Evaluator.
///
/// Like special forms, they return values through the CEK evaluator rather than directly.

macro_rules! register_sys_builtins {
    ($heap:expr, $env:expr, $($name:expr => $func:expr),* $(,)?) => {
        $(
            $env.set_symbol($heap.intern_symbol($name),
                new_sys_builtin($heap, $func,
                    concat!($name, ": sys-builtin").to_string()));
        )*
    };
}

pub fn register_sys_builtins(heap: &mut GcHeap, env: &mut crate::env::Environment) {
    register_sys_builtins!(heap, env,
        "eval-string" => eval_string_sp,
        "eval" => eval_eval_sp,
        "apply" => apply_sp,
        "debug-stack" => debug_stack_sp,
        "call/cc" => call_cc_sp,
        "call-with-current-continuation" => call_cc_sp,
        "escape" => escape_sp,
        "values" => values_sp,
        "call-with-values" => call_with_values_sp,
    );
}

fn eval_string_sp(
    ec: &mut EvalContext,
    args: &[GcRef],
    state: &mut CEKState,
) -> Result<(), String> {
    if args.len() != 1 {
        return Err("eval-string: expected exactly 1 argument".to_string());
    }
    let string = match &ec.heap.get_value(args[0]) {
        SchemeValue::Str(string) => string.clone(),
        _ => return Err("eval-string: argument must be a string".to_string()),
    };
    // Evaluate the string
    let result = crate::eval::eval_string(ec, &string)?;
    if result.len() == 1 {
        state.control = Control::Value(result[0]);
    } else {
        state.control = Control::Values(result);
    }
    Ok(())
}

/// (eval expr [env])
fn eval_eval_sp(_ec: &mut EvalContext, args: &[GcRef], state: &mut CEKState) -> Result<(), String> {
    match args.len() {
        1 => insert_eval_eval(state, args[0], None, false),
        2 => insert_eval_eval(state, args[0], Some(args[1]), false),
        _ => return Err("eval: requires 1 or 2 arguments".to_string()),
    }
    Ok(())
}

/// (apply func args)
/// Applies a function to a list of arguments
fn apply_sp(ec: &mut EvalContext, args: &[GcRef], state: &mut CEKState) -> Result<(), String> {
    if args.len() < 2 {
        return Err("apply: requires at least 2 arguments".to_string());
    }
    // eprintln!("apply_sp func: {}", print_value(&args[0]));
    // eprintln!("apply_sp args: {}", print_value(&args[1]));
    let func = gc_value!(args[0]);
    match func {
        SchemeValue::Callable(func) => match func {
            Callable::Builtin { func, .. } => {
                let args = list_to_vec(ec.heap, args[1])?;
                let result = func(ec, &args[..])?;
                state.control = crate::kont::Control::Value(result);
                return Ok(());
            }
            Callable::Closure { .. } => {
                let frame = Rc::new(Kont::ApplyProc {
                    proc: args[0],
                    evaluated_args: list_to_vec(ec.heap, args[1])?,
                    next: state.kont.clone(),
                });
                apply_proc(state, ec, frame)?;
                return Ok(());
            }
            _ => return Err("apply: first argument must be a function".to_string()),
        },
        _ => return Err("apply: first argument must be a function".to_string()),
    }
}

/// (debug-stack)
/// Prints the stack
fn debug_stack_sp(
    ec: &mut EvalContext,
    _args: &[GcRef],
    state: &mut CEKState,
) -> Result<(), String> {
    dump_cek("", state);
    state.control = crate::kont::Control::Value(ec.heap.void());
    Ok(())
}

/// (call/cc func)
/// Creates and returns an escape procedure that resets the continuation to the current state
/// at the time call/cc was invoked.
fn call_cc_sp(ec: &mut EvalContext, args: &[GcRef], state: &mut CEKState) -> Result<(), String> {
    // 1. Check arguments
    if args.len() != 1 {
        return Err("call/cc: requires a single function argument".to_string());
    }
    let func = gc_value!(args[0]);
    match &func {
        SchemeValue::Callable(func) => match *func {
            Callable::Closure { .. } | Callable::Builtin { .. } | Callable::SysBuiltin { .. } => {}
            _ => return Err("call/cc: argument must be a function".to_string()),
        },
        _ => return Err("call/cc: argument must be a function".to_string()),
    }
    // 2. Capture the current continuation
    // eprintln!("Entering with continuation:");
    // crate::utilities::dump_kont(&state);
    let kont = new_continuation(ec.heap, capture_call_site_kont(&state.kont));
    // 3. Build the escape call
    let sym_val = get_symbol(ec.heap, "val");
    let sym_lambda = get_symbol(ec.heap, "lambda");
    let sym_escape = get_symbol(ec.heap, "escape");

    let params = list(sym_val, ec.heap)?; // (val)
    let body = list3(sym_escape, kont, sym_val, ec.heap)?;
    let lambda = list3(sym_lambda, params, body, ec.heap)?;
    create_callable(lambda, ec, state)?;
    let closure;
    match &state.control {
        Control::Value(cl) => {
            closure = cl;
        }
        _ => return Err("call/cc: unexpected return value".to_string()),
    }
    //eprintln!("escape closure: {}", print_value(&closure));
    let mut call = Vec::<GcRef>::new();
    call.push(args[0]);
    call.push(list(*closure, ec.heap)?);
    // 4. Call func with the escape closure as an argument
    apply_sp(ec, &call[..], state)?;
    Ok(())
}

/// (escape continuation arg)
/// This is the internal mechanism for call/cc. It is bound by a lambda to the escape continuation,
/// and when called, it resets the continuation and returns the provided arg.
fn escape_sp(_ec: &mut EvalContext, args: &[GcRef], state: &mut CEKState) -> Result<(), String> {
    if args.len() != 2 {
        return Err("escape: requires two arguments".to_string());
    }
    match gc_value!(args[0]) {
        SchemeValue::Continuation(new_kont) => {
            // Assign to the new continuation
            //state.kont = Rc::clone(new_kont);
            let result = args[1];
            // Return the second argument
            //state.control = crate::kont::Control::Escape(result, Rc::clone(new_kont));
            state.control = crate::kont::Control::Escape(result, Rc::clone(new_kont));
            //eprintln!("Escaping to continuation:");
            //crate::utilities::dump_kont(Rc::clone(new_kont));
            Ok(())
        }
        _ => return Err("escape: first argument must be a continuation".to_string()),
    }
}

fn values_sp(_ec: &mut EvalContext, args: &[GcRef], state: &mut CEKState) -> Result<(), String> {
    if args.len() < 1 {
        return Err("values: requires at least one argument".to_string());
    }
    state.control = Control::Values(args.to_vec());
    Ok(())
}

fn call_with_values_sp(
    _ec: &mut EvalContext,
    args: &[GcRef],
    state: &mut CEKState,
) -> Result<(), String> {
    if args.len() != 2 {
        return Err("call-with-values: expected 2 arguments".to_string());
    }
    let producer = args[0];
    let consumer = args[1];

    // Push continuation that remembers consumer
    let prev = Rc::clone(&state.kont);
    state.kont = Rc::new(Kont::CallWithValues {
        consumer,
        next: prev,
    });

    // Evaluate the producer thunk
    state.control = Control::Expr(producer);
    Ok(())
}

/// Utility functions
///
fn capture_call_site_kont(k: &KontRef) -> KontRef {
    match &**k {
        Kont::EvalArg { next, .. } | Kont::ApplyProc { next, .. } => capture_call_site_kont(next),
        _ => Rc::clone(k),
    }
}
