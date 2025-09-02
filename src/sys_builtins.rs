use crate::cek::apply_proc;
use crate::eval::EvalContext;
use crate::gc::{Callable, GcHeap, GcRef, SchemeValue, list_to_vec, new_sys_builtin};
use crate::gc_value;
use crate::kont::{CEKState, Kont, insert_eval_eval};
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
        "eval" => eval_eval_sp,
        "apply" => apply_sp,
        "debug-stack" => debug_stack_sp,
        "call/cc" => call_cc_sp,
        "call-with-current-continuation" => call_cc_sp,
    );
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
    let func = gc_value!(args[0]);
    match func {
        SchemeValue::Callable(func) => match func {
            Callable::Builtin { func, .. } => {
                let args = list_to_vec(ec.heap, args[1])?;
                let result = func(ec, &args[..])?;
                state.control = crate::kont::Control::Value(result);
                return Ok(());
            }
            Callable::Closure { params, body, env } => {
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

/// (debug-stack)
/// Prints the stack at the time of calling
fn call_cc_sp(_ec: &mut EvalContext, args: &[GcRef], _state: &mut CEKState) -> Result<(), String> {
    if args.len() != 1 {
        return Err("call/cc: requires a single function argument".to_string());
    }
    Ok(())
}
