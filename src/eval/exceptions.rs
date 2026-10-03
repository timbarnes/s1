//! Exceptions: R7RS section 6.11.
//!
//! The current handlers are a Scheme list in `RunTime::handlers`, innermost
//! first. `with-exception-handler` conses a handler on for the extent of its
//! thunk (a `Kont::RestoreHandlers` frame puts the old list back), and
//! continuations capture and restore the list along with the dynamic-wind
//! stack, so escaping out of or back into a handler's extent keeps it right.
//!
//! Every error goes through `raise`: `raise` and `raise-continuable`
//! themselves, `error`, and the failures of built-in procedures, which
//! `post_error` turns into error objects. `raise` calls the innermost
//! handler with the outer handlers installed, beneath a `Kont::RaiseReturn`
//! frame that either resumes the raiser (`raise-continuable`) or reports that
//! a handler returned from a non-continuable `raise`. With no handler the
//! exception is uncaught: it is printed, pending dynamic-wind `after` thunks
//! run, and the top-level form is abandoned.

use crate::env::{EnvOps, EnvRef};
use crate::eval::kont::insert_escape;
use crate::eval::{CEKState, Control, Kont, KontRef, RunTime, TraceMode};
use crate::gc::{
    Callable, ErrorKind, GcHeap, GcRef, SchemeValue, list_from_slice, new_bool, new_error_object,
    new_pair, new_string, new_sys_builtin,
};
use crate::gc_value;
use crate::printer::{display_value, print_value};
use crate::{register_builtin_family, register_sys_builtins};
use std::rc::Rc;

/// Bind the exception procedures of R7RS 6.11 in `env`.
pub fn register_exception_builtins(rt: &mut RunTime, env: EnvRef) {
    register_sys_builtins!(rt, env,
        "raise" => raise_sp,
        "raise-continuable" => raise_continuable_sp,
        "with-exception-handler" => with_exception_handler_sp,
        "error" => error_sp,
    );
    register_builtin_family!(rt.heap, env,
        "error-object?" => (error_object_q, "(error-object? obj) Returns #t if obj was created by error or by a failing built-in procedure"),
        "error-object-message" => (error_object_message, "(error-object-message e) Returns the message of error object e"),
        "error-object-irritants" => (error_object_irritants, "(error-object-irritants e) Returns the list of irritants of error object e"),
        "read-error?" => (read_error_q, "(read-error? obj) Returns #t if obj is an error object raised by read on malformed input"),
        "file-error?" => (file_error_q, "(file-error? obj) Returns #t if obj is an error object raised because a file could not be opened"),
    );
}

/// Raise `obj`. If a handler is installed, it is called with `obj` and with
/// the handlers outside it installed; `continuable` decides what happens if
/// it returns (see `handle_raise_return`). `next` is the raiser's
/// continuation, which a `raise-continuable` handler's value returns to.
pub fn raise(state: &mut CEKState, rt: &mut RunTime, obj: GcRef, continuable: bool, next: KontRef) {
    match gc_value!(*rt.handlers) {
        SchemeValue::Pair(handler, outer) => {
            let (handler, outer) = (*handler, *outer);
            let saved = *rt.handlers;
            *rt.handlers = outer;
            let frame = Rc::new(Kont::RaiseReturn {
                payload: obj,
                saved,
                continuable,
                next,
            });
            call(state, rt, handler, vec![obj], frame);
        }
        _ => uncaught(state, rt, obj),
    }
}

/// Raise a new error object (non-continuably) from the current point.
pub fn raise_error(
    state: &mut CEKState,
    rt: &mut RunTime,
    kind: ErrorKind,
    message: &str,
    irritants: GcRef,
) {
    let message = new_string(rt.heap, message);
    let obj = new_error_object(rt.heap, kind, message, irritants);
    let next = Rc::clone(&state.kont);
    raise(state, rt, obj, false, next);
}

/// Apply `proc` to already-evaluated `args`, continuing with `next`. A
/// failure to apply (not a procedure, wrong number of arguments) is raised
/// as an error in the current dynamic environment.
fn call(state: &mut CEKState, rt: &mut RunTime, proc: GcRef, args: Vec<GcRef>, next: KontRef) {
    state.kont = Rc::new(Kont::ApplyProc {
        proc,
        evaluated_args: Rc::new(args),
        next,
    });
    state.tail = false;
    if let Err(err) = crate::eval::cek::apply_proc(state, rt) {
        let nil = rt.heap.nil_s();
        raise_error(state, rt, ErrorKind::General, &err, nil);
    }
}

/// No handler is installed: report `obj`, then abandon the top-level form,
/// running the `after` thunks of any dynamic-wind extents being left.
fn uncaught(state: &mut CEKState, rt: &mut RunTime, obj: GcRef) {
    // stdout is line-buffered; flush it so the error appears after the output
    // that preceded it rather than ahead of a pending partial line.
    std::io::Write::flush(&mut std::io::stdout()).ok();
    let message = describe(obj);
    // An error in an expression the debugger's `p` is evaluating goes back
    // to the debugger prompt.
    if crate::debugger::error_in_print(state, rt, &message) {
        return;
    }
    eprintln!("Error: {message}");

    if rt.debug.break_on_error || rt.debug.mode != TraceMode::Off {
        crate::debugger::post_mortem(state, rt);
    }

    // Running a script, an uncaught error ends it, once the thunks have run.
    let halt = if crate::builtin::system::script_mode() {
        Rc::new(crate::eval::Kont::Exit { code: crate::builtin::system::SCRIPT_ERROR_STATUS })
    } else {
        Rc::clone(&state.halt)
    };
    abandon_form(state, rt, halt);
}

/// Abandon the top-level form, continuing with `halt` once the `after`
/// thunks of the dynamic-wind extents being left have run.
pub fn abandon_form(state: &mut CEKState, rt: &mut RunTime, halt: KontRef) {
    // Clear the stacks before running the `after` thunks, so a thunk that
    // fails in turn is itself uncaught at top level instead of re-running
    // the unwind.
    let thunks = crate::sys_builtins::schedule_dynamic_wind_transitions(rt.dynamic_wind, &[]);
    rt.dynamic_wind.clear();
    let nil = rt.heap.nil_s();
    *rt.handlers = nil;
    state.control = Control::Value(rt.heap.void());
    if thunks.is_empty() {
        state.kont = halt;
    } else {
        let void = rt.heap.void();
        insert_escape(state, void, thunks, halt, Vec::new(), Some(Vec::new()), nil);
    }
}

/// The text after "Error: " for an uncaught `obj`: an error object's
/// message and irritants, or the raised object itself.
fn describe(obj: GcRef) -> String {
    match gc_value!(obj) {
        SchemeValue::ErrorObject(e) => {
            let mut s = display_value(&e.message);
            let mut rest = e.irritants;
            while let SchemeValue::Pair(car, cdr) = gc_value!(rest) {
                s.push(' ');
                s.push_str(&print_value(car));
                rest = *cdr;
            }
            s
        }
        _ => format!("uncaught exception: {}", print_value(&obj)),
    }
}

/// `Kont::RestoreHandlers`: the `with-exception-handler` thunk returned.
pub fn handle_restore_handlers(
    state: &mut CEKState,
    rt: &mut RunTime,
    handlers: GcRef,
    next: KontRef,
) -> Result<(), String> {
    *rt.handlers = handlers;
    state.kont = next;
    Ok(())
}

/// `Kont::RaiseReturn`: a handler returned (its value is in `state.control`).
pub fn handle_raise_return(
    state: &mut CEKState,
    rt: &mut RunTime,
    payload: GcRef,
    saved: GcRef,
    continuable: bool,
    next: KontRef,
) -> Result<(), String> {
    if continuable {
        *rt.handlers = saved;
        state.kont = next;
    } else {
        // R7RS: "a secondary exception is raised in the same dynamic
        // environment as the handler", whose handlers are still installed.
        let irritants = list_from_slice(&[payload], rt.heap);
        raise_error(
            state,
            rt,
            ErrorKind::General,
            "exception handler returned from a non-continuable raise of",
            irritants,
        );
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Procedures
// ---------------------------------------------------------------------------

/// Check that `who` got exactly `n` arguments.
fn expect_args(args: &[GcRef], n: usize, who: &str) -> Result<(), String> {
    if args.len() == n {
        Ok(())
    } else {
        Err(format!("{}: expects {} argument{}, got {}", who, n, if n == 1 { "" } else { "s" }, args.len()))
    }
}

/// Whether `v` is a procedure (as opposed to syntax).
fn is_procedure(v: GcRef) -> bool {
    matches!(
        gc_value!(v),
        SchemeValue::Callable(c) if matches!(
            **c,
            Callable::Builtin { .. }
                | Callable::SysBuiltin { .. }
                | Callable::Closure { .. }
                | Callable::CaseLambda { .. }
        )
    )
}

/// `(raise obj)`
fn raise_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    expect_args(args, 1, "raise")?;
    raise(state, rt, args[0], false, next);
    Ok(())
}

/// `(raise-continuable obj)`
fn raise_continuable_sp(
    rt: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    expect_args(args, 1, "raise-continuable")?;
    raise(state, rt, args[0], true, next);
    Ok(())
}

/// `(with-exception-handler handler thunk)`
fn with_exception_handler_sp(
    rt: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
) -> Result<(), String> {
    expect_args(args, 2, "with-exception-handler")?;
    if !is_procedure(args[0]) || !is_procedure(args[1]) {
        return Err("with-exception-handler: handler and thunk must be procedures".to_string());
    }
    let saved = *rt.handlers;
    *rt.handlers = new_pair(rt.heap, args[0], saved);
    let frame = Rc::new(Kont::RestoreHandlers {
        handlers: saved,
        next,
    });
    call(state, rt, args[1], Vec::new(), frame);
    Ok(())
}

/// `(error message irritant ...)`
fn error_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    if args.is_empty() {
        return Err("error: expects a message".to_string());
    }
    let irritants = list_from_slice(&args[1..], rt.heap);
    let obj = new_error_object(rt.heap, ErrorKind::General, args[0], irritants);
    raise(state, rt, obj, false, next);
    Ok(())
}

/// The kind of `v`, if it is an error object.
fn error_kind(v: GcRef) -> Option<ErrorKind> {
    match gc_value!(v) {
        SchemeValue::ErrorObject(e) => Some(e.kind),
        _ => None,
    }
}

fn error_object_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    expect_args(args, 1, "error-object?")?;
    Ok(new_bool(heap, error_kind(args[0]).is_some()))
}

fn read_error_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    expect_args(args, 1, "read-error?")?;
    Ok(new_bool(heap, error_kind(args[0]) == Some(ErrorKind::Read)))
}

fn file_error_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    expect_args(args, 1, "file-error?")?;
    Ok(new_bool(heap, error_kind(args[0]) == Some(ErrorKind::File)))
}

fn error_object_message(_heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    expect_args(args, 1, "error-object-message")?;
    match gc_value!(args[0]) {
        SchemeValue::ErrorObject(e) => Ok(e.message),
        _ => Err("error-object-message: expected an error object".to_string()),
    }
}

fn error_object_irritants(_heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    expect_args(args, 1, "error-object-irritants")?;
    match gc_value!(args[0]) {
        SchemeValue::ErrorObject(e) => Ok(e.irritants),
        _ => Err("error-object-irritants: expected an error object".to_string()),
    }
}
