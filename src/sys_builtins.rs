//! System builtins: procedures that need the evaluator.
//!
//! These receive evaluated arguments like ordinary builtins, but also the
//! machine (`RunTime`, `CEKState` and their own continuation `next`). They
//! return by setting the control and continuation rather than returning a
//! value, so they can call procedures (`apply`, `call-with-values`,
//! `dynamic-wind`), capture and invoke continuations (`call/cc`), evaluate
//! code (`eval`, `eval-string`), or end the process (`exit`). The tracing
//! and debugging procedures are here too.

use crate::env::{EnvOps, EnvRef};
use crate::eval::kont::EvalSeqForms;
use crate::eval::{
    CEKState, Control, DynamicWind, Kont, KontRef, RunTime, TraceType, insert_dynamic_wind,
};
use crate::gc::{
    Callable, GcHeap, GcRef, SchemeValue, get_symbol, list, list_to_vec, list3,
    new_continuation, new_float, new_string, new_sys_builtin,
};
use crate::gc_value;
use crate::parser::{ParseError, parse};
use crate::register_sys_builtins;
use crate::special_forms::create_callable;
use crate::utilities::post_error;
use std::rc::Rc;
use std::time::Instant;

/// Bind the system builtins in `env`.
pub fn register_sys_builtins(runtime: &mut RunTime, env: EnvRef) {
    register_sys_builtins!(runtime, env,
        "eval-string" => eval_string_sp,
        "eval" => eval_eval_sp,
        "interaction-environment" => interaction_environment_sp,
        "apply" => apply_sp,
        "debug-stack" => debug_stack_sp,
        "call/cc" => call_cc_sp,
        "call-with-current-continuation" => call_cc_sp,
        "%call/ec" => call_ec_sp,
        "escape" => escape_sp,
        "dynamic-wind" => dynamic_wind_sp,
        "exit" => exit_sp,
        "emergency-exit" => emergency_exit_sp,
        "values" => values_sp,
        "call-with-values" => call_with_values_sp,
        "trace" => trace_sp,
        "trace-env" => trace_env_sp,
        "%kont-depth" => kont_depth_sp,
        "garbage-collect" => garbage_collect_sp,
        "gc" => garbage_collect_sp,
        "help" => help_sp,
    );
}

/// `(eval-string string)`
///
/// Parses every form in `string` up front, then drives them one at a time
/// from a `Kont::EvalSeq` frame (see `handle_eval_seq` in `eval/cek.rs`)
/// instead of calling back into `eval_main`: a nested `eval_main` call would
/// reset `state.kont` to `Halt`, making the remaining forms and results
/// (held only in Rust locals) invisible to the GC for the duration of the
/// call. Returns a list of the results.
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
        SchemeValue::Str(string) => string.to_string(),
        _ => return Err("eval-string: argument must be a string".to_string()),
    };

    let mut port_kind = crate::io::new_string_port_input(&string);
    let mut forms = Vec::new();
    loop {
        match parse(&mut rt.heap, &mut port_kind) {
            Err(ParseError::Syntax(e)) => return Err(e),
            Err(ParseError::Eof) => break,
            Ok(expr) => forms.push(expr),
        }
    }

    if forms.is_empty() {
        state.control = Control::Value(rt.heap.nil_s());
        state.kont = next;
        return Ok(());
    }

    forms.reverse(); // so .pop() below yields the forms in source order
    let first = forms.pop().unwrap();
    state.kont = Rc::new(Kont::EvalSeq {
        forms: Box::new(EvalSeqForms {
            remaining: forms,
            results: Vec::new(),
        }),
        next,
    });
    state.control = Control::Expr(first);
    state.tail = false;
    Ok(())
}

/// `(eval expr [env])`
fn eval_eval_sp(
    _ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    // With no environment argument (an s1 extension), `expr` is evaluated
    // in the caller's environment.
    let env = match args {
        [_] => state.env.clone(),
        [_, env] => match gc_value!(*env) {
            SchemeValue::Environment(env) => env.clone(),
            _ => return Err("eval: the second argument must be an environment".to_string()),
        },
        _ => return Err("eval: requires 1 or 2 arguments".to_string()),
    };
    // Evaluate in `env`, then restore the caller's environment. Not a tail
    // call, so the RestoreEnv frame is always pushed.
    state.kont = Rc::new(Kont::RestoreEnv {
        old_env: state.env.clone(),
        next,
    });
    state.env = env;
    state.control = Control::Expr(args[0]);
    state.tail = false;
    Ok(())
}

/// `(interaction-environment)`
/// The environment the REPL and loaded files run in.
fn interaction_environment_sp(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    if !args.is_empty() {
        return Err("interaction-environment: takes no arguments".to_string());
    }
    let env = ec.heap.interaction_env().ok_or("interaction-environment: no interaction environment")?;
    let value = ec.heap.alloc(crate::gc::GcObject {
        value: SchemeValue::Environment(env),
        marked: 0,
    });
    state.control = Control::Value(value);
    state.kont = next;
    Ok(())
}

/// `(apply func args)`
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
        SchemeValue::Callable(func) => match &**func {
            Callable::Builtin { func, .. } => {
                let args = list_to_vec(ec.heap, arglist)?;
                let result = func(ec.heap, &args);
                // As in cek::apply_proc: post_error halts the machine, so only
                // a successful call continues to `nxt`.
                match &result {
                    Err(err) => {
                        post_error(state, ec, &err);
                    }
                    Ok(value) => {
                        state.control = Control::Value(*value);
                        state.kont = nxt;
                    }
                }
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
                ..
            } => {
                let applied_args = list_to_vec(ec.heap, arglist)?;
                let new_env =
                    crate::eval::bind_params(&params[..], &applied_args, &closure_env, ec.heap)?;
                // A tail call if `apply` (or `call/cc`) was called in tail
                // position (R7RS 3.5).
                crate::eval::cek::enter_closure(state, ec, new_env, *body, nxt);
                return Ok(());
            }
            Callable::CaseLambda { clauses, .. } => {
                let count = list_to_vec(ec.heap, arglist)?.len();
                let chosen = crate::eval::select_clause(clauses, count)?;
                return apply_sp(ec, &[chosen, arglist], state, nxt);
            }
            _ => return Err("apply: first argument must be a function".to_string()),
        },
        _ => return Err("apply: first argument must be a function".to_string()),
    }
}

/// `(%kont-depth)`
/// The number of continuation frames waiting for this call's value. Lets
/// the regression suite check that a loop runs in constant space: a proper
/// tail call leaves the depth unchanged however many times it repeats.
fn kont_depth_sp(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    if !args.is_empty() {
        return Err("%kont-depth: expected 0 arguments".to_string());
    }
    let mut depth = 0usize;
    let mut k = &next;
    while let Some(n) = k.next() {
        depth += 1;
        k = n;
    }
    state.control = Control::Value(crate::gc::new_int(ec.heap, num_bigint::BigInt::from(depth)));
    state.kont = next;
    Ok(())
}

/// `(gc)`: collect garbage now; returns the seconds it took.
fn garbage_collect_sp(
    ec: &mut RunTime,
    _args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    let timer = Instant::now();
    ec.heap.collect_garbage(
        state,
        &ec.current_ports[..],
        ec.port_stack,
        ec.dynamic_wind,
        ec.arg_stack,
        *ec.handlers,
    );
    let elapsed_time = timer.elapsed().as_secs_f64();
    let time = new_float(&mut ec.heap, elapsed_time);
    state.control = Control::Value(time);
    state.kont = next;
    Ok(())
}

/// `(help symbol)`
/// Returns the documentation for `symbol` as a Scheme string. Resolution
/// order:
///   1. A doc attached directly to the symbol via `add-doc`, which works
///      whether or not the symbol is bound to anything.
///   2. Otherwise, if the symbol is bound, the doc intrinsic to its value
///      (a builtin/sys-builtin/special-form's doc, or a closure/macro's
///      leading-docstring, if any).
///   3. Otherwise "no documentation available".
/// An unbound symbol with no `add-doc` entry is still an error.
fn help_sp(
    rt: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    if args.len() != 1 {
        return Err("help: expected 1 argument".to_string());
    }
    let sym_name = match &gc_value!(args[0]) {
        SchemeValue::Symbol(sym) => sym.clone(),
        _ => return Err("help: argument must be a symbol".to_string()),
    };
    let doc = if let Some(doc) = rt.heap.get_doc(args[0]) {
        doc.clone()
    } else {
        let binding = state
            .env
            .lookup(args[0])
            .ok_or_else(|| format!("help: unbound variable: {}", sym_name))?;
        match rt.heap.get_value(binding).as_callable() {
            Some(
                Callable::Builtin { doc, .. }
                | Callable::SysBuiltin { doc, .. }
                | Callable::SpecialForm { doc, .. },
            ) => doc.clone(),
            Some(
                Callable::Closure { doc: Some(doc), .. } | Callable::Macro { doc: Some(doc), .. },
            ) => doc.clone(),
            _ => format!("{}: no documentation available", sym_name),
        }
    };
    let result = new_string(rt.heap, &doc);
    state.control = Control::Value(result);
    state.kont = next;
    Ok(())
}

/// `(debug-stack)`
/// Prints the continuation frames waiting for this call's value, innermost
/// first.
fn debug_stack_sp(
    ec: &mut RunTime,
    _args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    crate::utilities::dbg_kont("", &next);
    state.control = Control::Value(ec.heap.void());
    state.kont = next;
    Ok(())
}

/// `(call/cc func)`
/// Creates and returns an escape procedure that resets the continuation to the current state
/// at the time call/cc was invoked.
fn call_cc_sp(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    capture(ec, args, state, next, false)
}

/// `(%call/ec func)`
/// call/cc for an escape-only continuation: one that is only invoked while
/// the %call/ec call is still in progress (to jump out of it), never to
/// re-enter it. It records the argument stack's length instead of copying
/// the stack, so it costs the same however deep the computation is.
/// `guard` uses it. Invoking it after the call has returned is an error.
fn call_ec_sp(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    capture(ec, args, state, next, true)
}

/// call/cc, or with `escape_only`, %call/ec.
fn capture(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
    escape_only: bool,
) -> Result<(), String> {
    // 1. Check arguments
    if args.len() != 1 {
        return Err("call/cc: requires a single function argument".to_string());
    }
    let func = gc_value!(args[0]);
    match &func {
        SchemeValue::Callable(func) => match **func {
            Callable::Closure { .. }
            | Callable::Builtin { .. }
            | Callable::SysBuiltin { .. }
            | Callable::CaseLambda { .. } => {}
            _ => return Err("call/cc: argument must be a function".to_string()),
        },
        _ => return Err("call/cc: argument must be a function".to_string()),
    }
    // Capture the continuation of the call/cc call itself (`next`, i.e. with
    // call/cc's own ApplyProc already popped), frames and all: they are never
    // mutated in place, so sharing the chain is safe for re-entry. The
    // pending EvalArg frames' arguments live on `arg_stack`, snapshotted here.
    let (arg_stack, escape_len) = if escape_only {
        (Vec::new(), Some(ec.arg_stack.len()))
    } else {
        (ec.arg_stack.clone(), None)
    };
    let kont = new_continuation(
        ec.heap,
        Rc::clone(&next),
        ec.dynamic_wind.clone(),
        arg_stack,
        escape_len,
        *ec.handlers,
    );
    // Build the escape procedure `(lambda vals (<escape-values> k vals))`.
    // It is variadic so that `(k)` and `(k 1 2)` deliver zero or several
    // values, and the escape primitive is embedded as an object rather than
    // named, so a local binding of `escape` at the call site can't capture it.
    let sym_vals = get_symbol(ec.heap, "vals");
    let sym_lambda = get_symbol(ec.heap, "lambda");
    let escape_values = new_sys_builtin(
        ec,
        "escape-values",
        escape_values_sp,
        "escape-values: sys-builtin".to_string(),
    );

    let body = list3(escape_values, kont, sym_vals, ec.heap)?;
    let lambda = list3(sym_lambda, sym_vals, body, ec.heap)?;
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
    Ok(())
}

/// `(escape continuation arg)`
/// This is the internal mechanism for call/cc. It is bound by a lambda to the escape continuation,
/// and when called, it resets the continuation and returns the provided arg.
fn escape_sp(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    _next: KontRef,
) -> Result<(), String> {
    if args.len() != 2 {
        return Err("escape: requires two arguments".to_string());
    }
    escape_to(ec, state, args[0], args[1])
}

/// `(<escape-values> continuation list-of-values)`
/// The body of the procedure call/cc hands out: delivers every value in the
/// list to the continuation, packaged by `new_values`.
fn escape_values_sp(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    _next: KontRef,
) -> Result<(), String> {
    if args.len() != 2 {
        return Err("escape-values: requires two arguments".to_string());
    }
    let vals = list_to_vec(ec.heap, args[1])?;
    let result = crate::gc::new_values(ec.heap, vals);
    escape_to(ec, state, args[0], result)
}

/// Reinstate continuation `k`, running any dynamic-wind transitions on the
/// way, and deliver `result` to it.
fn escape_to(
    ec: &mut RunTime,
    state: &mut CEKState,
    k: GcRef,
    result: GcRef,
) -> Result<(), String> {
    match gc_value!(k) {
        SchemeValue::Continuation(k) => {
            let new_arg_stack = match k.escape_len {
                None => Some(k.arg_stack.clone()),
                Some(len) => {
                    // Escape-only: valid while k's frames are still below
                    // us, which also guarantees the stack is at least `len`
                    // long and unchanged below it.
                    let mut frame = &state.kont;
                    while !Rc::ptr_eq(frame, &k.kont) {
                        match frame.next() {
                            Some(next) => frame = next,
                            None => {
                                return Err(
                                    "escape continuation invoked after its extent has ended".to_string()
                                );
                            }
                        }
                    }
                    ec.arg_stack.truncate(len);
                    None
                }
            };
            let thunks = schedule_dynamic_wind_transitions(&ec.dynamic_wind, &k.dw_stack);
            crate::eval::kont::insert_escape(
                state,
                result,
                thunks,
                Rc::clone(&k.kont),
                k.dw_stack.clone(),
                new_arg_stack,
                k.handlers,
            );
            Ok(())
        }
        _ => Err("escape: first argument must be a continuation".to_string()),
    }
}

/// The process exit status for `exit`'s optional argument: none or #t is
/// success (0), #f is failure (1), and an exact integer is used as it is.
fn exit_status(args: &[GcRef], who: &str) -> Result<i32, String> {
    use num_traits::ToPrimitive;
    match args {
        [] => Ok(0),
        [obj] => match gc_value!(*obj) {
            SchemeValue::Bool(true) => Ok(0),
            SchemeValue::Bool(false) => Ok(1),
            SchemeValue::Int(n) => n
                .to_i32()
                .ok_or_else(|| format!("{}: exit status {} is out of range", who, n)),
            _ => Err(format!("{}: expected a boolean or an exact integer", who)),
        },
        _ => Err(format!("{}: expected at most 1 argument", who)),
    }
}

/// End the process now, with standard output flushed.
pub fn exit_now(code: i32) -> ! {
    std::io::Write::flush(&mut std::io::stdout()).ok();
    std::process::exit(code)
}

/// `(exit [obj])`
///
/// Runs the `after` thunks of every dynamic-wind extent the call is inside,
/// innermost first, then ends the process (R7RS 6.14). The unwinding is the
/// same as an uncaught error's (`uncaught` in eval/exceptions.rs), except
/// that it ends in `Kont::Exit` rather than back at the top level. If an
/// `after` thunk raises, that error is reported as usual and the process
/// carries on.
fn exit_sp(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    _next: KontRef,
) -> Result<(), String> {
    let code = exit_status(args, "exit")?;
    let thunks = schedule_dynamic_wind_transitions(ec.dynamic_wind, &[]);
    let void = ec.heap.void();
    let nil = ec.heap.nil_s();
    state.control = Control::Value(void);
    crate::eval::kont::insert_escape(
        state,
        void,
        thunks,
        Rc::new(Kont::Exit { code }),
        Vec::new(),
        Some(Vec::new()),
        nil,
    );
    Ok(())
}

/// `(emergency-exit [obj])`
///
/// Ends the process at once, without running any dynamic-wind `after`
/// thunks.
fn emergency_exit_sp(
    _ec: &mut RunTime,
    args: &[GcRef],
    _state: &mut CEKState,
    _next: KontRef,
) -> Result<(), String> {
    exit_now(exit_status(args, "emergency-exit")?)
}

/// `(dynamic-wind before thunk after)`: call `before`, then push the
/// `Kont::DynamicWind` frame that runs `thunk` and `after`.
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
    // The wind entry is pushed once `before` returns (handle_dynamic_wind),
    // not here: `before` runs outside the extent, so a continuation captured
    // inside it must not re-run `before` on re-entry.
    state.kont = next; // Delete the ApplyProc before installing the new continuation
    insert_dynamic_wind(state, before, thunk, after);
    state.control = Control::Expr(before);
    state.tail = false;
    Ok(())
}

/// `(values obj ...)`: return the objects as multiple values.
fn values_sp(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    state.control = Control::Value(crate::gc::new_values(ec.heap, args.to_vec()));
    state.kont = next;
    Ok(())
}

/// `(call-with-values producer consumer)`
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
    // Push a frame that applies the consumer to whatever the producer
    // returns. It continues to `next`: `state.kont` is still this call's own
    // ApplyProc frame.
    state.kont = Rc::new(Kont::CallWithValues { consumer, next });
    // Evaluate the producer thunk
    state.control = Control::Expr(list(producer, ec.heap)?);
    state.tail = false;
    // dump_cek("call_with_values_sp", state);
    Ok(())
}

// Debug Functions

/// `(trace [arg])`
/// Controls step and tracing options:
/// - `(trace)`: returns the current trace setting
/// - `(trace 'all)`: print state.control and state.kont each time through the evaluator
/// - `(trace 'expr)`: show trace when control is an expr, or when a value is returned
/// - `(trace 'step)`: enable single stepping
/// - `(trace 'off)`: disable tracing and stepping
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
        state.kont = next;
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

/// `(trace-env ['g(lobal)])`
/// Prints the caller's local environment frames, innermost first; with `g`
/// or `global`, the top-level frame too.
fn trace_env_sp(
    ec: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    *ec.depth -= 1;
    let global = match args {
        [] => false,
        [arg] => match &gc_value!(*arg) {
            SchemeValue::Symbol(s) if s == "g" || s == "global" => true,
            _ => return Err("trace-env: expects no argument, or g(lobal)".to_string()),
        },
        _ => return Err("trace-env: expects at most 1 argument".to_string()),
    };
    crate::utilities::dbg_env("", state.env.clone(), global);
    state.control = Control::Value(ec.heap.void());
    state.kont = next;
    Ok(())
}

// Utility functions

/// The arguments of `(apply f a ... list)` as one list: the leading ones
/// consed onto the last.
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::env::Frame;
    use crate::eval::{RunTimeStruct, eval_string, initialize_scheme_globals};
    use crate::gc::{car, cdr, new_int};
    use std::cell::RefCell;

    /// Walks `eval_string`'s bundled result list to the `index`-th (0-based)
    /// per-form result and asserts it's a string equal to `expected`.
    fn assert_nth_result_is_string(results: &[GcRef], heap: &GcHeap, index: usize, expected: &str) {
        let mut cursor = results[0];
        for _ in 0..index {
            cursor = cdr(cursor).unwrap();
        }
        let value = car(cursor).unwrap();
        match &heap.get_value(value) {
            SchemeValue::Str(s) => assert_eq!(s, expected),
            _ => panic!("Expected a string result, got something else"),
        }
    }

    /// `help_sp` is called directly (bypassing the CEK dispatch loop), so its
    /// `Result::Err` is observed directly instead of being converted into a
    /// halted, void-valued state by `post_error`.
    #[test]
    fn test_help_returns_real_doc_string() {
        let mut runtime = RunTimeStruct::new();
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut ec = RunTime::from_eval(&mut runtime);
        initialize_scheme_globals(&mut ec, env.clone()).unwrap();
        let mut state = CEKState::new(env);

        let sym = ec.heap.intern_symbol("car");
        let halt = Rc::new(Kont::Halt);
        help_sp(&mut ec, &[sym], &mut state, halt).unwrap();

        match &state.control {
            Control::Value(v) => match &ec.heap.get_value(*v) {
                SchemeValue::Str(s) => assert_eq!(s, "(car pair) -> first element of pair"),
                _ => panic!("Expected a doc string"),
            },
            _ => panic!("Expected Control::Value"),
        }
    }

    #[test]
    fn test_help_unbound_symbol_is_error() {
        let mut runtime = RunTimeStruct::new();
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut ec = RunTime::from_eval(&mut runtime);
        initialize_scheme_globals(&mut ec, env.clone()).unwrap();
        let mut state = CEKState::new(env);

        let sym = ec.heap.intern_symbol("no-such-symbol");
        let halt = Rc::new(Kont::Halt);
        let err = help_sp(&mut ec, &[sym], &mut state, halt).unwrap_err();
        assert!(err.contains("Unbound") || err.contains("help"));
    }

    #[test]
    fn test_help_non_symbol_arg_is_error() {
        let mut runtime = RunTimeStruct::new();
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut ec = RunTime::from_eval(&mut runtime);
        initialize_scheme_globals(&mut ec, env.clone()).unwrap();
        let mut state = CEKState::new(env);

        let arg = new_int(ec.heap, num_bigint::BigInt::from(42));
        let halt = Rc::new(Kont::Halt);
        let err = help_sp(&mut ec, &[arg], &mut state, halt).unwrap_err();
        assert_eq!(err, "help: argument must be a symbol");
    }

    #[test]
    fn test_help_wrong_arity_is_error() {
        let mut runtime = RunTimeStruct::new();
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut ec = RunTime::from_eval(&mut runtime);
        initialize_scheme_globals(&mut ec, env.clone()).unwrap();
        let mut state = CEKState::new(env);

        let halt = Rc::new(Kont::Halt);
        let err = help_sp(&mut ec, &[], &mut state, halt).unwrap_err();
        assert_eq!(err, "help: expected 1 argument");
    }

    #[test]
    fn test_add_doc_documents_unbound_symbol() {
        let mut runtime = RunTimeStruct::new();
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut ec = RunTime::from_eval(&mut runtime);
        initialize_scheme_globals(&mut ec, env.clone()).unwrap();
        let mut state = CEKState::new(env);

        let results = eval_string(
            "(add-doc 'my-global \"a global counter\") (help 'my-global)",
            &mut state,
            &mut ec,
        )
        .unwrap();
        assert_nth_result_is_string(&results, ec.heap, 1, "a global counter");
    }

    #[test]
    fn test_add_doc_overrides_builtin_doc() {
        let mut runtime = RunTimeStruct::new();
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut ec = RunTime::from_eval(&mut runtime);
        initialize_scheme_globals(&mut ec, env.clone()).unwrap();
        let mut state = CEKState::new(env);

        let results = eval_string(
            "(add-doc 'car \"overridden doc\") (help 'car)",
            &mut state,
            &mut ec,
        )
        .unwrap();
        assert_nth_result_is_string(&results, ec.heap, 1, "overridden doc");
    }

    #[test]
    fn test_lambda_docstring_is_extracted_and_not_evaluated() {
        let mut runtime = RunTimeStruct::new();
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut ec = RunTime::from_eval(&mut runtime);
        initialize_scheme_globals(&mut ec, env.clone()).unwrap();
        let mut state = CEKState::new(env);

        let results = eval_string(
            "(define (f x) \"doubles x\" (* x 2)) (help 'f) (f 5)",
            &mut state,
            &mut ec,
        )
        .unwrap();
        assert_nth_result_is_string(&results, ec.heap, 1, "doubles x");
        let call_result = car(cdr(cdr(results[0]).unwrap()).unwrap()).unwrap();
        match &ec.heap.get_value(call_result) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "10"),
            _ => panic!("Expected (f 5) to evaluate to 10, docstring should not be in the body"),
        }
    }

    #[test]
    fn test_single_string_body_is_not_mistaken_for_docstring() {
        let mut runtime = RunTimeStruct::new();
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut ec = RunTime::from_eval(&mut runtime);
        initialize_scheme_globals(&mut ec, env.clone()).unwrap();
        let mut state = CEKState::new(env);

        // A lambda whose entire body is a single string literal: that
        // string is the return value, not a docstring (there's no body
        // left over if it were treated as one).
        let results = eval_string(
            "(define g (lambda () \"just a string\")) (g) (help 'g)",
            &mut state,
            &mut ec,
        )
        .unwrap();
        assert_nth_result_is_string(&results, ec.heap, 1, "just a string");
        assert_nth_result_is_string(&results, ec.heap, 2, "g: no documentation available");
    }

    #[test]
    fn test_add_doc_overrides_closure_own_docstring() {
        let mut runtime = RunTimeStruct::new();
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut ec = RunTime::from_eval(&mut runtime);
        initialize_scheme_globals(&mut ec, env.clone()).unwrap();
        let mut state = CEKState::new(env);

        let results = eval_string(
            "(define (h x) \"own doc\" x) (add-doc 'h \"override\") (help 'h)",
            &mut state,
            &mut ec,
        )
        .unwrap();
        assert_nth_result_is_string(&results, ec.heap, 2, "override");
    }
}
