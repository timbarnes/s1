use crate::env::Environment;
/// Debugging utilities
///
/// Trace logic: turn the trace function on or off
/// This function takes an integer value, and sets evaluator.trace to match the argument.
///
use crate::eval::EvalContext;
use crate::gc::{GcHeap, GcRef, SchemeValue};
use crate::utilities::print_env;

fn trace(evaluator: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    use num_traits::ToPrimitive;
    *evaluator.depth -= 1;
    if args.len() != 1 {
        return Err("trace: requires integer argument".to_string());
    }
    match &evaluator.heap.get_value(args[1]) {
        SchemeValue::Int(v) => match v.to_i32() {
            Some(value) => {
                *evaluator.trace = value;
                return Ok(args[1]);
            }
            None => return Err("trace: requires integer argument".to_string()),
        },
        _ => return Err("trace: requires integer argument".to_string()),
    }
}

/// (debug-env)
/// Prints the environment up to the given depth, or all if.
fn debug_env(evaluator: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    *evaluator.depth -= 1;
    if args.len() != 0 {
        return Err("debug-env: requires no arguments".to_string());
    }
    let env = evaluator.env.current_frame.clone();
    print_env(Some(env));
    Ok(evaluator.heap.true_s())
}

macro_rules! register_builtin_family {
    ($heap:expr, $env:expr, $($name:expr => $func:expr),* $(,)?) => {
        $(
            $env.set_symbol($heap.intern_symbol($name),
                crate::gc::new_primitive($heap, $func,
                    concat!($name, ": builtin function").to_string()));
        )*
    };
}

pub fn register_debug_builtins(heap: &mut GcHeap, env: &mut Environment) {
    register_builtin_family!(heap, env,
        "trace" => trace,
        "debug-env" => debug_env,
    );
}
