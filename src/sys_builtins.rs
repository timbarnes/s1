use crate::cek::apply_proc;
use crate::env::{EnvOps, EnvRef};
use crate::eval::{RunTime, TraceType};
use crate::gc::{
    Callable, GcHeap, GcRef, SchemeValue, get_symbol, list, list_to_vec, list3, new_continuation,
    new_port, new_sys_builtin,
};
use crate::gc_value;
use crate::io::{PortKind, port_kind_from_scheme_port};
use crate::kont::{CEKState, Control, Kont, KontRef, insert_eval_eval};
//use crate::printer::print_value;
use crate::parser::{ParseError, parse};
use crate::special_forms::create_callable;
use crate::utilities::{dbg_cek, dbg_kont, post_error};

//use crate::utilities::dump_cek;
use std::rc::Rc;

/// System Builtin Functions
///
/// These functions are classified as procedures, and receive evaluated arguments.
/// They differ from normal builtins in that they also have access to the CEK Evaluator.
///
/// Like special forms, they return values through the CEK evaluator rather than directly.

macro_rules! register_sys_builtins {
    ($rt:expr, $env:expr, $($name:expr => $func:expr),* $(,)?) => {
        $(
            $env.define($rt.heap.intern_symbol($name),
                new_sys_builtin($rt, $func,
                    concat!($name, ": sys-builtin").to_string()));
        )*
    };
}

pub fn register_sys_builtins(runtime: &mut RunTime, env: EnvRef) {
    register_sys_builtins!(runtime, env,
        "eval-string" => eval_string_sp,
        "eval" => eval_eval_sp,
        "apply" => apply_sp,
        "debug-stack" => debug_stack_sp,
        "call/cc" => call_cc_sp,
        "call-with-current-continuation" => call_cc_sp,
        "escape" => escape_sp,
        "values" => values_sp,
        "call-with-values" => call_with_values_sp,
        "trace" => trace_sp,
        "trace-env" => trace_env_sp,
        "read" => read_sp,
        "push-port!" => push_port_sp,
        "pop-port!" => pop_port_sp,
    );
}

fn eval_string_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState) -> Result<(), String> {
    if args.len() != 1 {
        return Err("eval-string: expected exactly 1 argument".to_string());
    }
    let string = match &rt.heap.get_value(args[0]) {
        SchemeValue::Str(string) => string.clone(),
        _ => return Err("eval-string: argument must be a string".to_string()),
    };
    // Evaluate the string
    let result = crate::eval::eval_string(&string, state, rt)?;
    if result.len() == 1 {
        state.control = Control::Value(result[0]);
    } else {
        state.control = Control::Values(result);
    }
    Ok(())
}

/// (eval expr [env])
fn eval_eval_sp(_ec: &mut RunTime, args: &[GcRef], state: &mut CEKState) -> Result<(), String> {
    match args.len() {
        1 => insert_eval_eval(state, args[0], None, false),
        2 => insert_eval_eval(state, args[0], Some(args[1]), false),
        _ => return Err("eval: requires 1 or 2 arguments".to_string()),
    }
    Ok(())
}

/// (apply func args)
/// Applies a function to a list of arguments
fn apply_sp(ec: &mut RunTime, args: &[GcRef], state: &mut CEKState) -> Result<(), String> {
    if args.len() < 2 {
        return Err("apply: requires at least 2 arguments".to_string());
    }
    // combine args into a single list
    let arglist = apply_arg_list(&args[1..], ec.heap);
    let func = gc_value!(args[0]);
    match func {
        SchemeValue::Callable(func) => match func {
            Callable::Builtin { func, .. } => {
                let args = list_to_vec(ec.heap, arglist)?;
                let result = func(ec.heap, &args);
                match &result {
                    Err(err) => {
                        post_error(state, ec, &err);
                    }
                    Ok(value) => {
                        state.control = crate::kont::Control::Value(*value);
                    }
                }
                return Ok(());
            }
            Callable::SysBuiltin { func, .. } => {
                let args = list_to_vec(ec.heap, arglist)?;
                let result = func(ec, &args, state);
                match &result {
                    Err(err) => {
                        post_error(state, ec, &err);
                    }
                    Ok(_) => {}
                }
                return Ok(());
            }
            Callable::Closure { .. } => {
                let eval_expr = crate::gc::cons(args[0], arglist, ec.heap)?;
                state.control = Control::Expr(eval_expr);
                return Ok(());
            }
            _ => return Err("apply: first argument must be a function".to_string()),
        },
        _ => return Err("apply: first argument must be a function".to_string()),
    }
}

/// (debug-stack)
/// Prints the stack
fn debug_stack_sp(ec: &mut RunTime, _args: &[GcRef], state: &mut CEKState) -> Result<(), String> {
    state.control = crate::kont::Control::Value(ec.heap.void());
    Ok(())
}

/// (call/cc func)
/// Creates and returns an escape procedure that resets the continuation to the current state
/// at the time call/cc was invoked.
fn call_cc_sp(ec: &mut RunTime, args: &[GcRef], state: &mut CEKState) -> Result<(), String> {
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
fn escape_sp(_ec: &mut RunTime, args: &[GcRef], state: &mut CEKState) -> Result<(), String> {
    if args.len() != 2 {
        return Err("escape: requires two arguments".to_string());
    }
    match gc_value!(args[0]) {
        SchemeValue::Continuation(new_kont) => {
            // Assign to the new continuation
            //state.kont = Rc::clone(new_kont);
            let result = args[1];
            // Return the second argument
            state.control = crate::kont::Control::Escape(result, Rc::clone(new_kont));
            //eprintln!("Escaping to continuation:");
            //crate::utilities::dump_kont(Rc::clone(new_kont));
            Ok(())
        }
        _ => return Err("escape: first argument must be a continuation".to_string()),
    }
}

fn values_sp(_ec: &mut RunTime, args: &[GcRef], state: &mut CEKState) -> Result<(), String> {
    match args.len() {
        0 => return Err("values: requires at least one argument".to_string()),
        1 => state.control = Control::Value(args[0]),
        _ => state.control = Control::Values(args.to_vec()),
    }
    Ok(())
}

fn call_with_values_sp(
    ec: &mut RunTime,
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
    state.control = Control::Expr(list(producer, ec.heap)?);
    // dump_cek("call_with_values_sp", state);
    Ok(())
}

/// Debug Functions
///

/// (trace [arg])
/// Controls step and tracing options
/// (trace)         - returns the current trace setting
/// (trace 'all)    - print state.control and state.kont each time through the evaluator
/// (trace 'expr)   - show trace when control is an expr, or when a value is returned
/// (trace 'step)   - enable single stepping
/// (trace 'off)    - disable tracing and stepping
fn trace_sp(ec: &mut RunTime, args: &[GcRef], state: &mut CEKState) -> Result<(), String> {
    *ec.depth -= 1;
    if args.len() == 0 {
        let result = match ec.trace {
            TraceType::Off => ec.heap.intern_symbol("off"),
            TraceType::Full => ec.heap.intern_symbol("all"),
            TraceType::Control => ec.heap.intern_symbol("expr"),
            TraceType::Step => ec.heap.intern_symbol("step"),
            TraceType::Reset => ec.heap.intern_symbol("reset"),
        };
        state.control = Control::Value(result);
        return Ok(());
    }
    match &gc_value!(args[0]) {
        SchemeValue::Symbol(cmd) => {
            *ec.trace = match &cmd[..] {
                "o" | "off" => TraceType::Off,
                "a" | "all" => TraceType::Full,
                "e" | "expr" => TraceType::Control,
                "s" | "step" => TraceType::Step,
                "r" | "reset" => TraceType::Reset,
                _ => TraceType::Off,
            };
        }
        _ => return Err("trace: expects o(ff), a(ll), e(xpr), or s(tep)".to_string()),
    };
    state.control = Control::Value(args[0]);
    Ok(())
}

/// (debug-env ['g(lobal)])
/// Prints the environment, optionally including the global env.
fn trace_env_sp(ec: &mut RunTime, args: &[GcRef], state: &mut CEKState) -> Result<(), String> {
    *ec.depth -= 1;
    let mut global = false;
    if args.len() == 1 {
        global = true;
    }
    let env = state.env.clone();
    state.control = Control::Value(ec.heap.void());
    Ok(())
}

/// (read [port])
/// Reads an s-expression from a port. Port defaults to the current input port (normally stdin).
fn read_sp(ec: &mut RunTime, args: &[GcRef], state: &mut CEKState) -> Result<(), String> {
    if args.len() > 1 {
        return Err("read: expected at most 1 argument".to_string());
    }
    let mut port_kind = ec.port_stack.last_mut().unwrap().clone();
    if args.len() == 1 {
        match args.get(0) {
            Some(port) => port_kind = port_kind_from_scheme_port(ec, *port),
            None => {
                return Err("read: invalid port argument".to_string());
            }
        }
    }

    match port_kind {
        PortKind::Stdin | PortKind::StringPortInput { .. } /* | PortKind::File { .. } */ => {
            let result = parse(ec.heap, &mut port_kind);
            match result {
                Ok(expr) => {
                    state.control = Control::Value(expr);
                    Ok(())
                }
                Err(err) => match err {
                    ParseError::Eof => {
                        state.control = Control::Value(ec.heap.eof());
                        Ok(())
                    },
                    ParseError::Syntax(err) => Err(format!("read: syntax error:{}", err)),
                    //ParseError::Other(err) => Err(format!("read: other error:{}", err)),
                }
            }
        }
        _ => Err("read: port must be a string port or file port".to_string()),
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
pub fn push_port_sp(ec: &mut RunTime, args: &[GcRef], state: &mut CEKState) -> Result<(), String> {
    // (push-port! port)
    if args.len() != 1 {
        return Err("push_port!: expected exactly 1 argument".to_string());
    }
    let port_kind = port_kind_from_scheme_port(ec, args[0]);
    ec.port_stack.push(port_kind);
    state.control = Control::Value(ec.heap.void());
    Ok(())
}

pub fn pop_port_sp(ec: &mut RunTime, _args: &[GcRef], state: &mut CEKState) -> Result<(), String> {
    // (pop-port!)
    let port_kind = ec.port_stack.pop();
    match port_kind {
        Some(port_kind) => {
            let port = new_port(&mut ec.heap, port_kind);
            state.control = Control::Value(port);
            Ok(())
        }
        None => Err("Port Stack is empty".to_string()),
    }
}

/// Utility functions
///
fn capture_call_site_kont(k: &KontRef) -> KontRef {
    match &**k {
        Kont::EvalArg { next, .. } | Kont::ApplyProc { next, .. } => capture_call_site_kont(next),
        _ => Rc::clone(k),
    }
}

fn apply_arg_list(args: &[GcRef], heap: &mut GcHeap) -> GcRef {
    match args.len() {
        0 => heap.nil_s(),
        1 => args[0],
        _ => {
            let mut list = *args.last().unwrap();
            for arg in args[..args.len() - 1].iter().rev() {
                list = crate::gc::cons(*arg, list, heap).unwrap();
            }
            list
        }
    }
}
