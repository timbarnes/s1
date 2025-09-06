use crate::env::Environment;
/// Debugging utilities
///
/// Trace logic: turn the trace function on or off
/// This function takes an integer value, and sets evaluator.trace to match the argument.
///
use crate::eval::{EvalContext, TraceType};
use crate::gc::{GcHeap, GcRef, SchemeValue};
use crate::gc_value;
use crate::utilities::dbg_env;

/// (trace [arg])
/// Controls step and tracing options
/// (trace)         - returns the current trace setting
/// (trace 'all)    - print state.control and state.kont each time through the evaluator
/// (trace 'expr)   - show trace when control is an expr, or when a value is returned
/// (trace 'step)   - enable single stepping
/// (trace 'off)    - disable tracing and stepping
fn trace(evaluator: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    *evaluator.depth -= 1;
    if args.len() == 0 {
        let result = match evaluator.trace {
            TraceType::Off => evaluator.heap.intern_symbol("off"),
            TraceType::Full => evaluator.heap.intern_symbol("all"),
            TraceType::Control => evaluator.heap.intern_symbol("expr"),
            TraceType::Step => evaluator.heap.intern_symbol("step"),
        };
        return Ok(result);
    }
    match &gc_value!(args[0]) {
        SchemeValue::Symbol(cmd) => {
            *evaluator.trace = match &cmd[..] {
                "o" | "off" => TraceType::Off,
                "a" | "all" => TraceType::Full,
                "e" | "expr" => TraceType::Control,
                "s" | "step" => TraceType::Step,
                _ => TraceType::Off,
            };
        }
        _ => return Err("trace: expects o(ff), a(ll), e(xpr), or s(tep)".to_string()),
    };
    Ok(args[0])
}

// fn stepper(evaluator: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
//     *evaluator.depth -= 1;
//     if args.len() != 1 {
//         return Err("step: requires boolean argument".to_string());
//     }
//     match &evaluator.heap.get_value(args[0]) {
//         SchemeValue::Bool(v) => {
//             *evaluator.step = *v;
//             if *evaluator.trace == 0 {
//                 *evaluator.trace = 10;
//             }
//         }
//         _ => return Err("step: requires boolean argument".to_string()),
//     }
//     Ok(args[0])
// }

/// (debug-env)
/// Prints the environment up to the given depth, or all if.
fn trace_env(evaluator: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    *evaluator.depth -= 1;
    if args.len() != 0 {
        return Err("debug-env: requires no arguments".to_string());
    }
    let env = evaluator.env.current_frame.clone();
    dbg_env(Some(env));
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
        "trace-env" => trace_env,
        //"step" => stepper,
    );
}
