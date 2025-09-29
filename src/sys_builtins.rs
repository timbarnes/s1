use crate::env::{EnvOps, EnvRef};
use crate::eval::kont::DynamicWindPhase;
use crate::eval::{
    CEKState, Control, DynamicWind, Kont, KontRef, RunTime, TraceType,
    insert_dynamic_wind, insert_eval_eval,
};
use crate::gc::{
    Callable, GcHeap, GcRef, SchemeValue, get_symbol, list, list_to_vec, list3, new_bool,
    new_continuation, new_float, new_port, new_sys_builtin,
};
use crate::gc_value;
use crate::io::{PortKind, port_kind_from_scheme_port};
use crate::parser::{ParseError, parse};
use crate::special_forms::create_callable;
use crate::utilities::post_error;

use std::io::Write;
use std::rc::Rc;
use std::time::Instant;

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
        "dynamic-wind" => dynamic_wind_sp,
        "values" => values_sp,
        "call-with-values" => call_with_values_sp,
        "trace" => trace_sp,
        "trace-env" => trace_env_sp,
        "open-input-file" => open_input_file_sp,
        "open-output-file" => open_output_file_sp,
        "close-input-port" => close_input_port_sp,
        "close-output-port" => close_output_port_sp,
        "read" => read_sp,
        "read-char" => read_char_sp,
        "write-char" => write_char_sp,
        "peek-char" => peek_char_sp,
        "char-ready?" => char_ready_sp,
        "current-input-port" => current_input_port_sp,
        "current-output-port" => current_output_port_sp,
        "push-port!" => push_port_sp,
        "pop-port!" => pop_port_sp,
        "write" => write_sp,
        "display" => display_sp,
        "newline" => newline_sp,
        "flush-output" => flush_output_sp,
        "garbage-collect" => garbage_collect_sp,
        "gc" => garbage_collect_sp,
    );
}

fn eval_string_sp(
    rt: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
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

    state.kont = next;
    Ok(())
}

/// (eval expr [env])
fn eval_eval_sp(
    _ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    match args.len() {
        1 => insert_eval_eval(state, args[0], None, false),
        2 => insert_eval_eval(state, args[0], Some(args[1]), false),
        _ => return Err("eval: requires 1 or 2 arguments".to_string()),
    }
    state.kont = next;
    Ok(())
}

/// (apply func args)
/// Applies a function to a list of arguments
fn apply_sp(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    nxt: KontRef,
) -> Result<(), String> {
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
                        state.control = Control::Value(*value);
                    }
                }
                state.kont = nxt;
                return Ok(());
            }
            Callable::SysBuiltin { func, .. } => {
                let args = list_to_vec(ec.heap, arglist)?;
                let result = func(ec, &args, state, Rc::clone(&nxt));
                match &result {
                    Err(err) => {
                        post_error(state, ec, &err);
                    }
                    Ok(_) => {}
                }
                return Ok(());
            }
            Callable::Closure {
                params,
                body,
                env: closure_env,
            } => {
                let applied_args = list_to_vec(ec.heap, arglist)?;
                let new_env =
                    crate::eval::bind_params(&params[..], &applied_args, &closure_env, ec.heap)?;
                let old_env = state.env.clone();

                // `apply` is never a tail call, so always push RestoreEnv
                state.kont = Rc::new(Kont::RestoreEnv { old_env, next: nxt });
                state.env = new_env;
                state.control = Control::Expr(*body);
                return Ok(());
            }
            _ => return Err("apply: first argument must be a function".to_string()),
        },
        _ => return Err("apply: first argument must be a function".to_string()),
    }
}

fn garbage_collect_sp(
    ec: &mut RunTime,
    _args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    let timer = Instant::now();
    ec.heap
        .collect_garbage(state, *ec.current_output_port, ec.port_stack);
    let elapsed_time = timer.elapsed().as_secs_f64();
    let time = new_float(&mut ec.heap, elapsed_time);
    state.control = Control::Value(time);
    state.kont = next;
    Ok(())
}

/// (debug-stack)
/// Prints the stack
fn debug_stack_sp(
    ec: &mut RunTime,
    _args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    state.control = Control::Value(ec.heap.void());
    state.kont = next;
    Ok(())
}

/// (call/cc func)
/// Creates and returns an escape procedure that resets the continuation to the current state
/// at the time call/cc was invoked.
fn call_cc_sp(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
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
    // Capture the current continuation
    let captured_kont = capture_call_site_kont(&state.kont);
    // eprintln!("call_cc_sp: captured kont = {:?}", captured_kont);
    let kont = new_continuation(ec.heap, captured_kont, ec.dynamic_wind.clone());
    // Build the escape call
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
    apply_sp(ec, &call[..], state, Rc::clone(&next))?;
    state.kont = next;
    Ok(())
}

/// (escape continuation arg)
/// This is the internal mechanism for call/cc. It is bound by a lambda to the escape continuation,
/// and when called, it resets the continuation and returns the provided arg.
fn escape_sp(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    _next: KontRef,
) -> Result<(), String> {
    // eprintln!(
    //     "escape_sp: args[0] = {}, args[1] = {}",
    //     print_value(&args[0]),
    //     print_value(&args[1])
    // );
    if args.len() != 2 {
        return Err("escape: requires two arguments".to_string());
    }
    match gc_value!(args[0]) {
        SchemeValue::Continuation(new_kont, new_dw_stack) => {
            let result = args[1];
            // eprintln!(
            //     "escape_sp: new_kont = {:?}, new_dw_stack.len() = {}",
            //     new_kont,
            //     new_dw_stack.len()
            // );
            let thunks = schedule_dynamic_wind_transitions(&ec.dynamic_wind, new_dw_stack);
            // eprintln!("escape_sp: thunks.len() = {}", thunks.len());
            crate::eval::kont::insert_escape(
                state,
                result,
                thunks,
                Rc::clone(new_kont),
                new_dw_stack.clone(),
            );
            Ok(())
        }
        _ => return Err("escape: first argument must be a continuation".to_string()),
    }
}

fn dynamic_wind_sp(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    if args.len() != 3 {
        return Err("dynamic-wind: requires three arguments".to_string());
    }
    let before = list(args[0], ec.heap)?;
    let thunk = list(args[1], ec.heap)?;
    let after = list(args[2], ec.heap)?;
    ec.dynamic_wind
        .push(DynamicWind::new(*ec.dw_next, before, after));
    *ec.dw_next += 1;
    state.kont = next; // Delete the ApplyProc before installing the new continuation
    insert_dynamic_wind(state, before, thunk, after);
    state.control = Control::Expr(before);
    Ok(())
}

fn values_sp(
    _ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    match args.len() {
        0 => return Err("values: requires at least one argument".to_string()),
        1 => state.control = Control::Value(args[0]),
        _ => state.control = Control::Values(args.to_vec()),
    }
    state.kont = next;
    Ok(())
}

fn call_with_values_sp(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
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
    state.kont = next;
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
fn trace_sp(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
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
    state.kont = next;
    Ok(())
}

/// (debug-env ['g(lobal)])
/// Prints the environment, optionally including the global env.
fn trace_env_sp(
    ec: &mut RunTime,
    _args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    *ec.depth -= 1;
    state.control = Control::Value(ec.heap.void());
    state.kont = next;
    Ok(())
}

/// (open-input-file filename) -> port
fn open_input_file_sp(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    if args.len() != 1 {
        return Err("open-input-file: expected exactly 1 argument".to_string());
    }
    let filename = match &gc_value!(args[0]) {
        SchemeValue::Str(s) => s,
        _ => return Err("open-input-file: argument must be a string".to_string()),
    };
    // Try to open the file
    match std::fs::File::open(filename) {
        Ok(file) => {
            // Read the file into a string (for now, use StringPortInput for simplicity)
            use std::io::Read;
            let mut content = String::new();
            let mut reader = std::io::BufReader::new(file);
            if let Err(e) = reader.read_to_string(&mut content) {
                return Err(format!(
                    "open-input-file: could not read file '{}': {}",
                    filename, e
                ));
            }
            let port = new_port(ec.heap, crate::io::new_string_port_input(&content));
            state.control = Control::Value(port);
            state.kont = next;
            Ok(())
        }
        Err(e) => Err(format!(
            "open-input-file: could not open file '{}': {}",
            filename, e
        )),
    }
}

/// (open-output-file filename) -> port
fn open_output_file_sp(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    if args.len() != 1 {
        return Err("open-output-file: expected exactly 1 argument".to_string());
    }
    let filename = match &gc_value!(args[0]) {
        SchemeValue::Str(s) => s,
        _ => return Err("open-output-file: argument must be a string".to_string()),
    };
    // Try to open the file
    let result = ec.file_table.open_file(filename, true);
    match result {
        Ok(id) => {
            let port = new_port(
                ec.heap,
                crate::io::PortKind::File {
                    name: filename.to_string(),
                    id,
                    write: true,
                    pos: std::cell::Cell::new(0),
                },
            );
            state.control = Control::Value(port);
            state.kont = next;
            Ok(())
        }
        Err(e) => Err(format!(
            "open-output-file: could not open file '{}': {}",
            filename, e
        )),
    }
}

fn close_input_port_sp(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    if args.len() != 1 {
        return Err("close-input-port: expected exactly 1 argument".to_string());
    }
    let port = args[0];
    match &gc_value!(port) {
        SchemeValue::Port(kind) => {
            if let crate::io::PortKind::File {
                id, write: false, ..
            } = kind
            {
                ec.file_table.close_file(*id);
                state.control = Control::Value(ec.heap.void());
                Ok(())
            } else {
                // Closing non-file input ports is a no-op.
                state.control = Control::Value(ec.heap.void());
                state.kont = next;
                Ok(())
            }
        }
        _ => Err("close-input-port: argument must be a port".to_string()),
    }
}

/// (close-output-port port)
fn close_output_port_sp(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    if args.len() != 1 {
        return Err("close-output-port: expected exactly 1 argument".to_string());
    }
    let port = args[0];
    match &gc_value!(port) {
        SchemeValue::Port(kind) => {
            if let crate::io::PortKind::File {
                id, write: true, ..
            } = kind
            {
                ec.file_table.close_file(*id);
                state.control = Control::Value(ec.heap.void());
                state.kont = next;
                Ok(())
            } else {
                Err("close-output-port: argument must be an output file port".to_string())
            }
        }
        _ => Err("close-output-port: argument must be a port".to_string()),
    }
}

/// (read [port])
/// Reads an s-expression from a port. Port defaults to the current input port (normally stdin).
fn read_sp(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    if args.len() > 1 {
        return Err("read: expected at most 1 argument".to_string());
    }

    let result = if args.is_empty() {
        let port_ref = *ec.port_stack.last().unwrap();
        // Parse directly using unsafe access like gc_value! macro pattern
        if let SchemeValue::Port(port_kind) = unsafe { &mut (*port_ref).value } {
            match port_kind {
                PortKind::Stdin | PortKind::StringPortInput { .. } /* | PortKind::File { .. } */ => {
                    parse(ec.heap, port_kind)
                }
                _ => return Err("read: port must be a string port or file port".to_string()),
            }
        } else {
            return Err("Expected port on port stack".to_string());
        }
    } else {
        let mut port_kind = port_kind_from_scheme_port(ec, args[0]);
        match port_kind {
            PortKind::Stdin | PortKind::StringPortInput { .. } /* | PortKind::File { .. } */ => {
                parse(ec.heap, &mut port_kind)
            }
            _ => return Err("read: port must be a string port or file port".to_string()),
        }
    };

    match result {
        Ok(expr) => {
            state.control = Control::Value(expr);
            state.kont = next;
            Ok(())
        }
        Err(err) => match err {
            ParseError::Eof => {
                state.control = Control::Value(ec.heap.eof());
                state.kont = next;
                Ok(())
            }
            ParseError::Syntax(err) => Err(format!("read: syntax error:{}", err)),
        },
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
pub fn push_port_sp(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    // (push-port! port)
    if args.len() != 1 {
        return Err("push_port!: expected exactly 1 argument".to_string());
    }
    ec.port_stack.push(args[0]);
    state.control = Control::Value(ec.heap.void());
    state.kont = next;
    Ok(())
}

pub fn pop_port_sp(
    ec: &mut RunTime,
    _args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    // (pop-port!)
    let port_ref = ec.port_stack.pop();
    match port_ref {
        Some(port_ref) => {
            state.control = Control::Value(port_ref);
            state.kont = next;
            Ok(())
        }
        None => Err("Port Stack is empty".to_string()),
    }
}

fn write_sp(
    rt: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    if args.len() < 1 || args.len() > 2 {
        return Err("write: expected 1 or 2 arguments".to_string());
    }
    let s = crate::printer::print_value(&args[0]);

    let port = if args.len() == 2 {
        args[1]
    } else {
        *rt.current_output_port
    };
    let mut port_kind = crate::io::port_kind_from_scheme_port(rt, port);

    match port_kind {
        PortKind::Stdout | PortKind::Stderr | PortKind::StringPortOutput { .. } => {}
        PortKind::File { write, .. } => {
            if !write {
                return Err("write: provided port is not an output port".to_string());
            }
        }
        _ => return Err("write: provided port is not an output port".to_string()),
    }
    let success = crate::io::write_line(&mut port_kind, rt.file_table, &s);
    if success {
        state.control = Control::Value(rt.heap.void());
        state.kont = next;
        return Ok(());
    } else {
        return Err("write: failed to write to port".to_string());
    };
}

fn display_sp(
    rt: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    if args.len() < 1 || args.len() > 2 {
        return Err("display: expected 1 or 2 arguments".to_string());
    }
    let s = crate::printer::display_value(&args[0]);

    let port = if args.len() == 2 {
        args[1]
    } else {
        *rt.current_output_port
    };
    let mut port_kind = crate::io::port_kind_from_scheme_port(rt, port);

    match port_kind {
        PortKind::Stdout | PortKind::Stderr | PortKind::StringPortOutput { .. } => {}
        PortKind::File { write, .. } => {
            if !write {
                return Err("display: provided port is not an output port".to_string());
            }
        }
        _ => return Err("display: provided port is not an output port".to_string()),
    }
    crate::io::write_line(&mut port_kind, rt.file_table, &s);

    state.control = Control::Value(rt.heap.void());
    state.kont = next;
    Ok(())
}

fn newline_sp(
    rt: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    if args.len() > 1 {
        return Err("newline: expected 0 or 1 arguments".to_string());
    }

    let port_gcref = if args.len() == 1 {
        args[0]
    } else {
        *rt.current_output_port
    };

    if let SchemeValue::Port(port_kind) = rt.heap.get_value(port_gcref) {
        match port_kind {
            PortKind::Stdout | PortKind::Stderr | PortKind::StringPortOutput { .. } => {}
            PortKind::File { write, .. } => {
                if !*write {
                    return Err("newline: provided port is not an output port".to_string());
                }
            }
            _ => return Err("newline: provided port is not an output port".to_string()),
        }
        crate::io::write_char(port_kind, rt.file_table, '\n');
    } else {
        return Err("newline: argument must be a port".to_string());
    }

    state.control = Control::Value(rt.heap.void());
    state.kont = next;
    Ok(())
}

/// (read-char [input-port]) -> Character | EOF
fn read_char_sp(
    rt: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    if args.len() > 1 {
        return Err("read-char: expected 0 or 1 arguments".to_string());
    }

    let result_char = if args.is_empty() {
        let port_ref = *rt.port_stack.last().unwrap();
        let port_kind = rt.heap.get_port_mut(port_ref);
        crate::io::read_char(port_kind, rt.file_table)
    } else {
        let port = args[0];
        if let SchemeValue::Port(port_kind) = rt.heap.get_value(port) {
            match port_kind {
                PortKind::Stdin | PortKind::StringPortInput { .. } => {}
                PortKind::File { write, .. } => {
                    if *write {
                        return Err("read-char: provided port is not an input port".to_string());
                    }
                }
                _ => return Err("read-char: provided port is not an input port".to_string()),
            }
            crate::io::read_char(port_kind, rt.file_table)
        } else {
            return Err("read-char: argument must be a port".to_string());
        }
    };

    let result_val = match result_char {
        Some(c) => crate::gc::new_char(rt.heap, c),
        None => rt.heap.eof(),
    };

    state.control = Control::Value(result_val);
    state.kont = next;
    Ok(())
}

/// (write-char char [output-port])
/// Writes a character to the current or specified output port.
fn write_char_sp(
    rt: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    let port = match args.len() {
        1 => port_kind_from_scheme_port(rt, *rt.current_output_port),
        2 => port_kind_from_scheme_port(rt, args[1]),
        _ => return Err("write-char: expected 1 or 2 arguments".to_string()),
    };
    match gc_value!(args[0]) {
        SchemeValue::Char(c) => {
            crate::io::write_char(&port, rt.file_table, *c);
        }
        _ => return Err("write-char: expected a character".to_string()),
    }

    state.control = Control::Value(rt.heap.void());
    state.kont = next;
    Ok(())
}

/// (peek-char [input port]) -> Character | EOF
/// Returns the next character from the current input port without consuming it.
fn peek_char_sp(
    rt: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    if args.len() > 1 {
        return Err("peek-char: expected 0 or 1 arguments".to_string());
    }

    let result_char = if args.is_empty() {
        let port_ref = *rt.port_stack.last().unwrap();
        let port_kind = rt.heap.get_value(port_ref);
        if let SchemeValue::Port(port_kind) = port_kind {
            crate::io::peek_char(port_kind, rt.file_table)
        } else {
            return Err("Expected port on port stack".to_string());
        }
    } else {
        let port_kind = crate::io::port_kind_from_scheme_port(rt, args[0]);
        match port_kind {
            PortKind::Stdin | PortKind::StringPortInput { .. } => {}
            PortKind::File { write, .. } => {
                if write {
                    return Err("peek-char: provided port is not an input port".to_string());
                }
            }
            _ => return Err("peek-char: provided port is not an input port".to_string()),
        }
        crate::io::peek_char(&port_kind, rt.file_table)
    };

    let result_val = match result_char {
        Some(c) => crate::gc::new_char(rt.heap, c),
        None => rt.heap.eof(),
    };

    state.control = Control::Value(result_val);
    state.kont = next;
    Ok(())
}

/// (char-ready? [input-port]) -> Boolean
/// Returns true if there is a character available to be read from the current input port.
fn char_ready_sp(
    rt: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    if args.len() > 1 {
        return Err("char-ready?: expected 0 or 1 arguments".to_string());
    }

    let is_ready = if args.is_empty() {
        let port_ref = *rt.port_stack.last().unwrap();
        let port_kind = rt.heap.get_value(port_ref);
        if let SchemeValue::Port(port_kind) = port_kind {
            crate::io::char_ready(port_kind)
        } else {
            return Err("Expected port on port stack".to_string());
        }
    } else {
        let port_kind = crate::io::port_kind_from_scheme_port(rt, args[0]);
        match port_kind {
            PortKind::Stdin | PortKind::StringPortInput { .. } => {}
            PortKind::File { write, .. } => {
                if write {
                    return Err("char-ready?: provided port is not an input port".to_string());
                }
            }
            _ => return Err("char-ready?: provided port is not an input port".to_string()),
        }
        crate::io::char_ready(&port_kind)
    };

    state.control = Control::Value(new_bool(rt.heap, is_ready));
    state.kont = next;
    Ok(())
}

/// (current-input-port) -> Port
/// Returns the port currently used for input - typically stdin for the repl, but more
/// specifically the port that is on the top of the port stack.
fn current_input_port_sp(
    rt: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    if !args.is_empty() {
        return Err("current-input-port: expected 0 arguments".to_string());
    }
    let port = *rt.port_stack.last().unwrap();
    state.control = Control::Value(port);
    state.kont = next;
    Ok(())
}

/// (current-output-port) -> Port
/// Returns the port currently used for output - typically stdout for the repl.
fn current_output_port_sp(
    rt: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    if !args.is_empty() {
        return Err("current-output-port: expected 0 arguments".to_string());
    }
    state.control = Control::Value(*rt.current_output_port);
    state.kont = next;
    Ok(())
}

/// (flush-output [port]) -> #<void>
///    If no arg, flush the current output port;
///    otherwise flush the given port if it's an output file port.
fn flush_output_sp(
    rt: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    let port = if args.len() == 0 {
        *rt.current_output_port
    } else {
        args[0]
    };

    let f = port_kind_from_scheme_port(rt, port);
    match f {
        crate::io::PortKind::Stdout => {
            std::io::stdout().flush().ok();
        }
        crate::io::PortKind::File { id, write, .. } => {
            if write {
                let f = rt.file_table.get(id);
                match f {
                    Some(file) => {
                        file.flush().ok();
                        return Ok(());
                    }
                    None => return Err("flush-output: file not found".to_string()),
                }
            }
        }
        _ => return Err("flush-output: invalid port".to_string()),
    }

    state.control = Control::Value(port);
    state.kont = next;
    Ok(())
}

/// Utility functions
///
fn capture_call_site_kont(k: &KontRef) -> KontRef {
    // eprintln!("capture_call_site_kont: k = {:?}", k);
    match &**k {
        Kont::EvalArg { next, .. } | Kont::ApplyProc { next, .. } => capture_call_site_kont(next),
        Kont::DynamicWind {
            before,
            thunk,
            after,
            thunk_result,
            phase,
            next,
        } => {
            // eprintln!(
            //     "capture_call_site_kont: DynamicWind before = {}, thunk = {}, after = {}, phase = {:?}",
            //     print_value(before),
            //     print_value(thunk),
            //     print_value(after),
            //     phase
            // );
            match phase {
                DynamicWindPhase::Return => {
                    // If dynamic-wind is already in Return phase, skip it completely
                    // eprintln!("capture_call_site_kont: skipping Return phase DynamicWind frame");
                    capture_call_site_kont(next)
                }
                DynamicWindPhase::After => {
                    // If dynamic-wind is in After phase, we need to let it run its after thunk
                    // and then return the result. Keep it in After phase so it will execute normally.
                    let new_kont = Rc::new(Kont::DynamicWind {
                        before: *before,
                        thunk: *thunk,
                        after: *after,
                        thunk_result: *thunk_result,
                        phase: DynamicWindPhase::After, // Keep in After phase
                        next: capture_call_site_kont(next),
                    });
                    // eprintln!(
                    //     "capture_call_site_kont: preserving After phase DynamicWind frame = {:?}",
                    //     new_kont
                    // );
                    new_kont
                }
                DynamicWindPhase::Thunk => {
                    // If we are capturing a continuation inside a dynamic-wind thunk,
                    // the phase should be After, as the thunk has already run.
                    let new_kont = Rc::new(Kont::DynamicWind {
                        before: *before,
                        thunk: *thunk,
                        after: *after,
                        thunk_result: *thunk_result,
                        phase: DynamicWindPhase::After, // <--- Change phase here
                        next: capture_call_site_kont(next), // Recursively process the next continuation
                    });
                    // eprintln!(
                    //     "capture_call_site_kont: returning new_kont = {:?}",
                    //     new_kont
                    // );
                    new_kont
                }
            }
        }
        _ => Rc::clone(k),
    }
}

fn apply_arg_list(args: &[GcRef], heap: &mut GcHeap) -> GcRef {
    if args.is_empty() {
        heap.nil_s()
    } else {
        let (fixed, last) = args.split_at(args.len() - 1);
        // last argument must be a list
        let mut list = last[0];
        for arg in fixed.iter().rev() {
            list = crate::gc::cons(*arg, list, heap).unwrap();
        }
        list
    }
}

/// Schedule the necessary `after` calls for frames we are *leaving* and the
/// `before` calls for frames we are *entering* when making a non-local exit.
///
/// * `state` – current CEK state to which new frames are added
/// * `old_stack` – dynamic-wind stack of the current continuation
/// * `new_stack` – dynamic-wind stack of the target continuation
pub fn schedule_dynamic_wind_transitions(
    old_stack: &[DynamicWind],
    new_stack: &[DynamicWind],
) -> Vec<GcRef> {
    let mut thunks = Vec::new();
    // Find common prefix length
    let mut common = 0;
    while common < old_stack.len()
        && common < new_stack.len()
        && old_stack[common].id == new_stack[common].id
    {
        common += 1;
    }

    // Frames we are *entering*: run their `before` in forward order (outer to inner).
    // To execute in reverse order via pop(), we must push them in reverse.
    for dw in new_stack.iter().skip(common).rev() {
        thunks.push(dw.before);
    }

    // Frames we are *leaving*: run their `after` in reverse order (inner to outer).
    // To execute in reverse order via pop(), we must push them in forward order.
    for dw in old_stack.iter().skip(common) {
        thunks.push(dw.after);
    }
    // eprintln!("Thunk list is:");
    // for th in &thunks {
    //     println!(" => {}", print_value(&th));
    // }
    thunks
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::eval::RunTimeStruct;
//     use crate::gc::{SchemeValue, new_int, new_string};
//     use std::io::Write;
//     use tempfile::NamedTempFile;

//     #[test]
//     fn test_open_input_file_success() {
//         let mut ev = RunTimeStruct::new();
//         let mut ec = crate::eval::RunTime::from_eval(&mut ev);
//         // Create a temp file with some content
//         let mut tmpfile = NamedTempFile::new().unwrap();
//         write!(tmpfile, "hello world").unwrap();
//         let path = tmpfile.path().to_str().unwrap().to_string();
//         let filename = new_string(&mut ec.heap, &path);
//         let result = open_input_file_sp(&mut ec, &[filename], &mut state);
//         assert!(result.is_ok());
//         let port = result.unwrap();
//         match &ec.heap.get_value(port) {
//             SchemeValue::Port(kind) => match kind {
//                 crate::io::PortKind::StringPortInput { content, .. } => {
//                     assert_eq!(content, "hello world");
//                 }
//                 _ => panic!("Expected StringPortInput"),
//             },
//             _ => panic!("Expected Port"),
//         }
//     }

//     #[test]
//     fn test_open_input_file_nonexistent() {
//         let mut ev = RunTimeStruct::new();
//         let mut ec = crate::eval::RunTime::from_eval(&mut ev);
//         let filename = new_string(&mut ec.heap, "/no/such/file/hopefully.txt");
//         let result = open_input_file(&mut ec.heap, &[filename]);
//         assert!(result.is_err());
//         assert!(result.unwrap_err().contains("could not open file"));
//     }

//     #[test]
//     fn test_open_input_file_nonstring_arg() {
//         let mut ev = RunTimeStruct::new();
//         let mut ec = crate::eval::RunTime::from_eval(&mut ev);
//         let not_a_string = new_int(&mut ec.heap, 42.into());
//         let result = open_input_file(&mut ec.heap, &[not_a_string]);
//         assert!(result.is_err());
//         assert!(result.unwrap_err().contains("argument must be a string"));
//     }
// }
