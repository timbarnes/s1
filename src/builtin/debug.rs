/// Debugging utilities
///
/// Trace logic: turn the trace function on or off
/// This function takes an integer value, and sets evaluator.trace to match the argument.
///
use crate::eval::EvalContext;
use crate::gc::{GcHeap, GcRef, SchemeValue};

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

macro_rules! register_builtin_family {
    ($heap:expr, $env:expr, $($name:expr => $func:expr),* $(,)?) => {
        $(
            $env.set_symbol($heap.intern_symbol($name),
                crate::gc::new_primitive($heap, $func,
                    concat!($name, ": builtin function").to_string()));
        )*
    };
}

pub fn register_list_builtins(heap: &mut GcHeap, env: &mut crate::env::Environment) {
    register_builtin_family!(heap, env,
        "trace" => trace,
    );
}
