use super::kont::{
    AndOrKind, CEKState, CondClause, Control, EvalPhase, Kont, KontRef, MacroMode, insert_eval,
};
/// Continuation-Passing Style (CPS) evaluator.
///
use crate::env::{EnvOps, EnvRef};
use crate::eval::kont::{DynamicWindPhase, DynamicWindProcs, EscapePayload, EvalSeqForms};
use crate::eval::{DynamicWind, RunTime, bind_params};
use crate::gc::SchemeValue::*;
use crate::gc::{Callable, GcRef, cons, is_false, list_to_vec, new_float};
use crate::gc_value;
use crate::printer::print_value;
use crate::eval::{exceptions, identifiers};
use crate::utilities::{debugger, post_error};
use std::rc::Rc;
use std::time::Instant;

/// CEK evaluator entry point from the repl (not used recursively)
///
pub fn eval_main(
    expr: GcRef,
    state: &mut CEKState,
    ec: &mut RunTime,
) -> Result<Vec<GcRef>, String> {
    state.control = Control::Expr(expr);
    state.kont = Rc::clone(&state.halt);
    state.tail = true;
    // Each top-level form starts outside any dynamic-wind extent or
    // exception handler, with no pending arguments. (An uncaught error
    // already leaves them so; this also covers anything else that halted
    // mid-form.) Continuations re-entered later restore their own.
    ec.dynamic_wind.clear();
    ec.arg_stack.clear();
    *ec.handlers = ec.heap.nil_s();
    run_cek(state, ec)
}

/// Run the CEK evaluator loop until a result is produced.
///
/// All the state information is contained in the CEKState struct, so no result is returned until the end.
///
fn run_cek(mut state: &mut CEKState, rt: &mut RunTime) -> Result<Vec<GcRef>, String> {
    loop {
        // Step the CEK machine; mutates state in place
        //eprintln!("Pre-step {}", frame_debug_short(&state.kont));
        // Errors the machine itself reports (applying a non-procedure, a
        // wrong argument count) are raised like any other, so handlers see
        // them too.
        if let Err(err) = step(&mut state, rt) {
            post_error(&mut state, rt, &err);
        }
        match &state.control {
            Control::Value(val) => match *state.kont {
                // Fully evaluated: hand back each value of a (values ...)
                // package separately.
                Kont::Halt => return Ok(crate::gc::unpack_values(*val)),
                _ => continue, // Some continuation remains; continue loop
            },
            Control::Expr(_) => continue, // Still evaluating an expression; continue loop
            Control::Empty => return Ok(vec![rt.heap.void()]),
        }
    }
}

/// Perform one step of the CEK evaluation.
///
/// This function looks at the current control expression and continuation frame,
/// resolving symbols, starting applications, and handling continuations as needed.
///

fn step(state: &mut CEKState, ec: &mut RunTime) -> Result<(), String> {
    //dump_cek("  step", &state);
    // Hoisted out of debugger() so the common (tracing off) case is a branch on
    // an already-hot field instead of a call. This runs on every CEK step.
    if !matches!(*ec.trace, crate::eval::TraceType::Off) {
        debugger("", &state, ec);
    }

    let control = std::mem::replace(&mut state.control, Control::Empty);
    match control {
        Control::Expr(expr) => {
            *ec.depth += 1;
            eval_cek(expr, ec, state);
            Ok(())
        }
        Control::Value(val) => {
            *ec.depth -= 1;
            state.control = Control::Value(val);
            dispatch_kont(state, ec, val)
        }
        Control::Empty => Err("Unexpected Control::Halt in step()".to_string()),
    }
}

/// Capture the current environment and control state, then pass the CEKState to the CEK loop.
///
pub fn eval_cek(expr: GcRef, rt: &mut RunTime, state: &mut CEKState) {
    //dump_cek("   eval_cek", &state);
    match &gc_value!(expr) {
        // Self-evaluating values are returned unchanged
        Int(_) | Rational(_) | Float(_) | Str(_) | Bool(_) | Vector(_) | Char(_) | Nil | Callable(_)
        | Continuation(_) | ErrorObject(_) | RecordType(_) | Record(_) | Void | Undefined => {
            state.control = Control::Value(expr);
        }
        // Symbols are looked up in the environment
        Symbol(name) => {
            if let Some(val) = identifiers::lookup(rt.heap, expr, &state.env) {
                state.control = Control::Value(val);
            } else {
                post_error(state, rt, &format!("Unbound variable: {}", name));
            }
        }
        // Pair: potentially a function application
        Pair(car, cdr) => {
            // If the head is a symbol, resolve it once here. A regular
            // application reuses this value below instead of throwing it
            // away and re-looking it up via EvalArg's operator-evaluation
            // phase — that redundant second lookup used to happen on every
            // non-special-form application.
            let op_val = match gc_value!(*car) {
                Symbol(_) => identifiers::lookup(rt.heap, *car, &state.env),
                // A special-form object put in operator position by a
                // rewrite (GcHeap::core_form): dispatch it directly.
                Callable(c) if matches!(**c, Callable::SpecialForm { .. }) => Some(*car),
                _ => None,
            };

            // Quick path: is this a special form, or a syntax-rules macro?
            if let Some(op) = op_val {
                match gc_value!(op).as_callable() {
                    Some(Callable::SpecialForm { func, .. }) => {
                        // Call the special-form handler in-place.
                        state.control = Control::Expr(*car);
                        if let Err(err) = func(expr, rt, state) {
                            post_error(state, rt, &err);
                        }
                        return; // let run_cek loop continue
                    }
                    Some(Callable::SyntaxRules(sr)) => {
                        // Replace the use by its expansion, in the same
                        // environment and continuation, keeping tail position.
                        match expand_use(rt, op, sr, expr, &state.env) {
                            Ok(expansion) => insert_eval(state, expansion, state.tail),
                            Err(err) => post_error(state, rt, &err),
                        }
                        return;
                    }
                    _ => {}
                }
            }

            let prev = Rc::clone(&state.kont);

            // Whether *this* application is in tail position. Operator and
            // argument evaluation are not, so stash it in the frame and hand it
            // back to apply_proc once the arguments are done.
            let is_tail = state.tail;

            // `*cdr` is already the argument list in the right (forward)
            // order — walk it directly with EvalArg instead of copying it
            // into a `Vec` up front (that used to cost a `list_to_vec`
            // allocation plus a `.rev().collect()` on every application).
            // Evaluated arguments accumulate on the shared `arg_stack`
            // rather than in a private `Vec` per call; `args_base` marks
            // where this call's slice of it begins.
            let args_base = rt.arg_stack.len() as u32;

            match op_val {
                Some(op) => {
                    // Already resolved above (and confirmed not a special
                    // form): hand it to EvalArg as a Value directly rather
                    // than re-evaluating `car`. Control::Value dispatch
                    // always decrements `rt.depth`, so bump it here to
                    // balance the Control::Expr step this replaces.
                    *rt.depth += 1;
                    state.control = Control::Value(op);
                }
                None => {
                    state.control = Control::Expr(*car);
                }
            }
            state.tail = false; // reset after using it
            state.kont = Rc::new(Kont::EvalArg {
                have_proc: false,
                remaining_exprs: *cdr,
                args_base,
                tail: is_tail,
                original_call: expr,
                env: state.env.clone(),
                next: prev,
            });
        }
        _ => post_error(
            state,
            rt,
            &format!("Unsupported expression in eval_cek: {}", print_value(&expr)),
        ),
    }
}

/// Take ownership of the top continuation frame.
///
/// Frames carry `Vec`s (`EvalArg`'s pending and evaluated arguments, `Cond`'s
/// remaining clauses, ...). Borrowing the frame forces those to be cloned on
/// every single dispatch — measured at 19 clones per Scheme call on `fib`.
/// Taking the frame by value lets the handlers consume them instead.
///
/// `try_unwrap` succeeds whenever we hold the only reference, which is the
/// common case. A continuation captured by `call/cc` keeps its own `Rc`, so
/// that path still clones, exactly as before.
///
/// The frame we hand back is no longer reachable from the GC roots, so
/// `state.kont` is left pointing at the frame's `next`. That keeps the rest of
/// the continuation chain rooted for the duration of the dispatch; every handler
/// overwrites `state.kont` anyway, either with `next` or with a new frame built
/// on it, so this is only a safer transient value than `Halt`.
///
/// INVARIANT: the frame's own payload (`GcRef`s such as `EvalArg`'s evaluated
/// arguments) is *not* rooted while the handler runs, so a handler must install
/// them into `state` before anything can collect. The only collection points
/// are `handle_restore_env`, whose frame holds no `GcRef`s and which now
/// restores before collecting, and the `(gc)` builtin, which runs after
/// `apply_proc` has installed its frame.
#[inline]
fn take_kont(state: &mut CEKState) -> Kont {
    let parent = match state.kont.next() {
        Some(next) => Rc::clone(next),
        None => Rc::clone(&state.halt),
    };
    let kont = std::mem::replace(&mut state.kont, parent);
    match Rc::try_unwrap(kont) {
        Ok(k) => k,
        Err(shared) => (*shared).clone(),
    }
}

#[inline]
fn dispatch_kont(state: &mut CEKState, ec: &mut RunTime, val: GcRef) -> Result<(), String> {
    match take_kont(state) {
        Kont::AndOr { kind, rest, next } => handle_and_or(state, kind, rest, next),
        Kont::ApplySpecial {
            proc,
            original_call,
            next,
        } => handle_apply_special(state, ec, proc, original_call, next),
        Kont::Bind {
            symbol,
            env,
            is_define,
            next,
        } => handle_bind(state, ec, symbol, env, is_define, next),
        Kont::Cond {
            remaining,
            tail,
            next,
        } => handle_cond(state, ec, remaining, tail, next),
        Kont::CondClause { clause, next } => handle_cond_clause(state, ec, clause, next),
        Kont::DynamicWind { procs, phase, next } => {
            handle_dynamic_wind(state, ec.dynamic_wind, ec.dw_next, procs, phase, next)
        }
        Kont::EvalArg {
            have_proc,
            remaining_exprs,
            args_base,
            original_call,
            tail,
            env,
            next,
        } => handle_eval_arg(
            state,
            ec,
            val,
            have_proc,
            remaining_exprs,
            args_base,
            original_call,
            tail,
            env,
            next,
        ),
        Kont::Eval {
            expr,
            env,
            phase,
            next,
        } => handle_eval(state, expr, env, phase, next),
        Kont::If {
            then_branch,
            else_branch,
            tail,
            next,
        } => handle_if(state, then_branch, else_branch, tail, next),
        Kont::RestoreEnv { old_env, next } => handle_restore_env(state, ec, old_env, next),
        Kont::Escape { payload, new_kont } => handle_escape(state, ec, payload, new_kont),
        Kont::Seq { rest, tail, next } => handle_seq(state, rest, tail, next),
        Kont::MacroExpand {
            call_env,
            mode,
            next,
        } => handle_macro_expand(state, call_env, mode, next),
        Kont::ExpandArg { env, next } => handle_expand_arg(state, ec, env, next),
        Kont::EvalSeq { forms, next } => handle_eval_seq(state, ec, forms, next),
        Kont::Timer { start, next } => handle_timer(state, ec, start, next),
        Kont::RestoreHandlers { handlers, next } => {
            exceptions::handle_restore_handlers(state, ec, handlers, next)
        }
        Kont::RaiseReturn {
            payload,
            saved,
            continuable,
            next,
        } => exceptions::handle_raise_return(state, ec, payload, saved, continuable, next),
        Kont::Halt => Ok(()),
        Kont::CallWithValues { consumer, next } => {
            // The producer has returned: apply the consumer to its values.
            state.kont = Rc::new(Kont::ApplyProc {
                proc: consumer,
                evaluated_args: Rc::new(crate::gc::unpack_values(val)),
                next,
            });
            apply_proc(state, ec)
        }
        Kont::ApplyProc { .. } => {
            Err("Kont::ApplyProc reached dispatch_kont instead of being run directly".to_string())
        }
    }
}

fn handle_and_or(
    state: &mut CEKState,
    kind: AndOrKind,
    mut rest: Vec<GcRef>,
    next: KontRef,
) -> Result<(), String> {
    if let Control::Value(val) = &state.control {
        let short_circuit = match kind {
            AndOrKind::And => is_false(*val), // false ⇒ stop early
            AndOrKind::Or => !is_false(*val), // truthy ⇒ stop early
        };

        if short_circuit || rest.is_empty() {
            // Done: this value is the result of the whole (and ...) / (or ...)
            state.tail = false;
            state.kont = next;
            return Ok(());
        }
    } else {
        unreachable!("AndOr should only see a Value here");
    }
    let next_expr = rest.pop().unwrap(); // safe if not empty
    state.control = Control::Expr(next_expr);
    state.kont = Rc::new(Kont::AndOr { kind, rest, next });
    state.tail = false;
    Ok(())
}

fn handle_apply_special(
    state: &mut CEKState,
    ec: &mut RunTime,
    proc: GcRef,
    original_call: GcRef,
    next: KontRef,
) -> Result<(), String> {
    let frame = Rc::new(Kont::ApplySpecial {
        proc,
        original_call,
        next,
    });
    state.tail = false;
    state.kont = frame;
    apply_unevaluated(state, ec)?;
    Ok(())
}

fn handle_bind(
    state: &mut CEKState,
    ec: &mut RunTime,
    symbol: GcRef,
    env: EnvRef,
    is_define: bool,
    next: KontRef,
) -> Result<(), String> {
    // borrow cont fields
    if let Control::Value(val) = state.control {
        // `env` is the frame captured where the define/set! was written. Using
        // state.env here would bind into whatever frame happens to be current,
        // which is the wrong one after a non-local exit through this frame.
        // For set! the frame already holds the binding, so insert overwrites it.
        env.define(symbol, val);
        state.control = if is_define {
            Control::Value(symbol)
        } else {
            Control::Value(ec.heap.unspecified())
        };
        state.tail = false;
        state.kont = next;
        Ok(())
    } else {
        Err("Kont::Bind reached but control is not a Value".to_string())
    }
}

fn handle_cond(
    state: &mut CEKState,
    ec: &mut RunTime,
    mut remaining: Vec<CondClause>,
    tail: bool,
    next: KontRef,
) -> Result<(), String> {
    if let Some(clause) = remaining.pop() {
        state.kont = Rc::new(Kont::Cond {
            remaining,
            tail,
            next,
        });
        let test_expr = match &clause {
            CondClause::Normal { test, .. } => test,
            CondClause::Arrow { test, .. } => test,
        };
        // Evaluate the test
        state.control = Control::Expr(*test_expr);
        state.tail = false;
        // Push a CondClause frame whose `next` points to the Cond we just installed.
        let cond_frame = Rc::clone(&state.kont);
        state.kont = Rc::new(Kont::CondClause {
            clause, // move the owned clause here
            next: cond_frame,
        });
        Ok(())
    } else {
        // no more clauses: return false and restore the continuation after Cond
        state.control = Control::Value(ec.heap.false_s());
        state.kont = next; // move inner Kont out of the Box
        Ok(())
    }
}

fn handle_cond_clause(
    state: &mut CEKState,
    ec: &mut RunTime,
    cond_clause: CondClause,
    next: KontRef,
) -> Result<(), String> {
    // pull the test value
    let test_value;
    match &state.control {
        Control::Value(v) => test_value = v,
        Control::Expr(e) => {
            return Err(format!(
                "CondClause reached with unevaluated test: {}",
                print_value(&e)
            ));
        }
        Control::Empty => return Err("CondClause: Control::Empty".to_string()),
    };
    // The Cond frame that wraps each clause, and whether the cond is in
    // tail position (a chosen clause's body inherits that).
    let cond_tail = match next.as_ref() {
        Kont::Cond { tail, .. } => *tail,
        _ => return Err("expected Cond frame after CondClause".to_string()),
    };
    let skip_cond = |k: &Rc<Kont>| -> Result<Rc<Kont>, String> {
        if let Kont::Cond {
            next: cond_next, ..
        } = k.as_ref()
        {
            Ok(Rc::clone(cond_next))
        } else {
            return Err("expected Cond frame after CondClause".to_string());
        }
    };

    match cond_clause {
        CondClause::Normal { body, .. } => {
            if !is_false(*test_value) {
                if let Some(body_expr) = body {
                    state.kont = skip_cond(&next)?; // drop Cond
                    insert_eval(state, body_expr, cond_tail);
                } else {
                    state.kont = skip_cond(&next)?; // drop Cond
                    state.control = Control::Value(*test_value);
                }
            } else {
                // keep Cond so it can try remaining clauses
                state.kont = next;
            }
        }

        CondClause::Arrow { arrow_proc, .. } => {
            if !is_false(*test_value) {
                state.kont = skip_cond(&next)?; // drop Cond

                // build (arrow_proc (quote test_value)): the value is already
                // evaluated, so it must not be evaluated again as an argument
                // (a list value would be taken for a call).
                let quote = ec.heap.core_form("quote");
                let nil = ec.heap.nil_s();
                let quoted = cons(*test_value, nil, ec.heap)?;
                let quoted = cons(quote, quoted, ec.heap)?;
                let mut call_expr = cons(quoted, nil, ec.heap)?;
                call_expr = cons(arrow_proc, call_expr, ec.heap)?;
                insert_eval(state, call_expr, cond_tail);
            } else {
                state.kont = next; // keep Cond for more clauses
            }
        }
    }
    Ok(())
}

fn handle_dynamic_wind(
    state: &mut CEKState,
    dw: &mut Vec<DynamicWind>,
    dw_next: &mut u32,
    mut procs: Box<DynamicWindProcs>,
    phase: DynamicWindPhase,
    next: KontRef,
) -> Result<(), String> {
    // `procs` is moved from frame to frame across phases, so the box is
    // allocated once per dynamic-wind, not once per phase.
    let after = procs.after;
    match phase {
        DynamicWindPhase::Thunk => {
            // `before` has returned (its value is dropped): enter the extent.
            dw.push(DynamicWind::new(*dw_next, procs.before, after));
            *dw_next += 1;
            state.control = Control::Expr(procs.thunk);
            state.tail = false;
            procs.thunk_result = None;
            state.kont = Rc::new(Kont::DynamicWind {
                procs,
                phase: DynamicWindPhase::After,
                next,
            });
            Ok(())
        }
        DynamicWindPhase::After => {
            // Save incoming value from thunk
            if let Control::Value(result) = state.control {
                state.control = Control::Expr(after);
                state.tail = false;
                procs.thunk_result = Some(result);
                state.kont = Rc::new(Kont::DynamicWind {
                    procs,
                    phase: DynamicWindPhase::Return,
                    next,
                });
            } else {
                return Err("dynamic-wind: Expected thunk value".to_string());
            }
            match dw.pop() {
                Some(_) => {
                    state.control = Control::Expr(after);
                    return Ok(());
                }
                None => return Err("No dynamic wind to pop".to_string()),
            }
        }
        DynamicWindPhase::Return => match procs.thunk_result {
            Some(value) => {
                state.control = Control::Value(value);
                state.kont = next;
                return Ok(());
            }
            None => return Err("dynamic-wind: Expected thunk value".to_string()),
        },
    }
}

fn handle_escape(
    state: &mut CEKState,
    ec: &mut RunTime,
    mut payload: Box<EscapePayload>,
    new_kont: KontRef,
) -> Result<(), String> {
    // eprintln!(
    //     "handle_escape: result = {}, thunks.len() = {}, new_kont = {:?}, new_dw_stack.len() = {}",
    //     print_value(&result),
    //     thunks.len(),
    //     new_kont,
    //     new_dw_stack.len()
    // );
    if let Some(thunk) = payload.thunks.pop() {
        // eprintln!("handle_escape: running thunk = {}", print_value(&thunk));
        state.kont = Rc::new(Kont::Escape { payload, new_kont });
        state.control = Control::Expr(thunk);
        state.tail = false;
    } else {
        // eprintln!(
        //     "handle_escape: no more thunks, restoring state. result = {}",
        //     print_value(&result)
        // );
        state.kont = new_kont;
        state.control = Control::Value(payload.result);
        *ec.dynamic_wind = payload.new_dw_stack;
        *ec.arg_stack = payload.new_arg_stack;
        *ec.handlers = payload.new_handlers;
    }
    Ok(())
}

fn handle_eval(
    state: &mut CEKState,
    expr: GcRef,
    env: Option<GcRef>,
    phase: EvalPhase,
    next: KontRef,
) -> Result<(), String> {
    match phase {
        EvalPhase::EvalEnv => {
            if let Control::Value(env_val) = state.control {
                // Capture environment and move to first expression evaluation
                let new_env = Some(env_val);
                state.control = Control::Expr(expr);
                state.tail = false;
                state.kont = Rc::new(Kont::Eval {
                    expr,
                    env: new_env,
                    phase: EvalPhase::EvalExpr,
                    next: next,
                });
            } else {
                unreachable!("EvalEnv phase should yield a value");
            }
        }
        EvalPhase::EvalExpr => {
            if let Control::Value(val) = state.control {
                // Save the intermediate result, then prepare to re-evaluate
                state.control = Control::Expr(val);
                state.tail = false;
                state.kont = Rc::new(Kont::Eval {
                    expr: val,
                    env,
                    phase: EvalPhase::Done,
                    next: next,
                });
            } else {
                unreachable!("EvalExpr phase should yield a value");
            }
        }
        EvalPhase::Done => {
            if let Control::Value(val) = state.control {
                // Final value: propagate it
                state.control = Control::Value(val);
                state.kont = next;
            } else {
                unreachable!("Done phase should yield a value");
            }
        }
    }
    Ok(())
}

fn handle_eval_arg(
    state: &mut CEKState,
    ec: &mut RunTime,
    val: GcRef,
    have_proc: bool,
    remaining_exprs: GcRef,
    args_base: u32,
    original_call: GcRef,
    tail: bool,
    env: EnvRef,
    next: KontRef,
) -> Result<(), String> {
    // The argument we just finished may have been a tail call, which by design
    // leaves `state.env` inside the callee. EvalArg saved the call's own env for
    // exactly this reason: restore it before evaluating the remaining arguments.
    state.env = env;

    if !have_proc {
        // Evaluating operator (car of the call)
        if let Some(
            Callable::SpecialForm { .. } | Callable::Macro { .. } | Callable::SyntaxRules(_),
        ) = gc_value!(val).as_callable()
        {
            state.kont = Rc::new(Kont::ApplySpecial {
                proc: val,
                original_call,
                next,
            });
            apply_unevaluated(state, ec)?;
            return Ok(());
        }
    }
    // `val` is the operator (which goes first, at `args_base`) or an evaluated
    // argument. Either way it joins the shared stack rather than a private
    // Vec; continuations snapshot and restore that stack (see
    // RunTimeStruct::arg_stack).
    ec.arg_stack.push(val);
    match gc_value!(remaining_exprs) {
        Nil => {
            let evaluated_args = Rc::new(ec.arg_stack.split_off(args_base as usize + 1));
            let proc = ec
                .arg_stack
                .pop()
                .expect("EvalArg: operator missing from arg_stack");
            state.kont = Rc::new(Kont::ApplyProc {
                proc,
                evaluated_args,
                next,
            });
            state.tail = tail;
            apply_proc(state, ec)?;
        }
        Pair(head, rest) => {
            state.control = Control::Expr(*head);
            let rest = *rest;
            state.tail = false;
            state.kont = Rc::new(Kont::EvalArg {
                have_proc: true,
                remaining_exprs: rest,
                args_base,
                original_call,
                tail,
                env: state.env.clone(),
                next,
            });
        }
        _ => return Err("apply: improper argument list".to_string()),
    }
    Ok(())
}

fn handle_if(
    state: &mut CEKState,
    then_branch: GcRef,
    else_branch: GcRef,
    tail: bool,
    next: KontRef,
) -> Result<(), String> {
    //dump_cek("handle_if", state);
    match &state.control {
        Control::Value(val) => {
            if is_false(*val) {
                state.control = Control::Expr(else_branch);
            } else {
                state.control = Control::Expr(then_branch);
            }
            // A branch is in tail position only if the if form is. Forcing it
            // true made a closure call in the branch of a non-tail if skip its
            // RestoreEnv frame, so code after the if ran in the callee's env.
            state.tail = tail;
            state.kont = next;
            Ok(())
        }
        _ => Err("Kont::If reached but control is not a value".to_string()),
    }
}

fn handle_restore_env(
    state: &mut CEKState,
    ec: &mut RunTime,
    old_env: EnvRef,
    next: Rc<Kont>,
) -> Result<(), String> {
    // Restore environment
    state.env = old_env;
    // Pop this frame unconditionally
    state.kont = next;

    // Collect only after the restore. dispatch_kont owns the frame it is
    // dispatching, so it is not reachable from the GC roots; collecting before
    // the two assignments above would have swept the continuation we are about
    // to return into. This is also the tighter root set: the callee's
    // environment is genuinely dead by now.
    if ec.heap.needs_gc() {
        ec.heap.collect_garbage(
            state,
            *ec.current_output_port,
            ec.port_stack,
            ec.dynamic_wind,
            ec.arg_stack,
            *ec.handlers,
        );
    }

    Ok(())
}

fn handle_seq(
    state: &mut CEKState,
    mut rest: Vec<GcRef>,
    tail: bool,
    next: Rc<Kont>,
) -> Result<(), String> {
    // Pop the next expression from the sequence
    let next_expr = rest.pop().unwrap(); // safe: Seq always has >=2 exprs

    state.control = Control::Expr(next_expr);
    state.tail = false;

    // Determine if this is the last expression
    if rest.is_empty() {
        // Last expression: in tail position if the sequence is
        state.tail = tail;
        state.kont = next;
    } else {
        state.kont = Rc::new(Kont::Seq { rest, tail, next });
    }
    Ok(())
}

/// Restore the call site's environment once a macro body (or an `(expand
/// form)` argument that turned out to be a macro call) has produced a
/// value, then either evaluate it as the expansion or return it as-is.
fn handle_macro_expand(
    state: &mut CEKState,
    call_env: EnvRef,
    mode: MacroMode,
    next: KontRef,
) -> Result<(), String> {
    match state.control {
        Control::Value(val) => {
            state.env = call_env;
            match mode {
                MacroMode::Evaluate => {
                    state.control = Control::Expr(val);
                    // The use's own tail position isn't recorded in this
                    // frame; false is always safe (it only costs a frame).
                    state.tail = false;
                }
                MacroMode::Expand => {
                    state.control = Control::Value(val);
                    state.tail = false;
                }
            }
            state.kont = next;
            Ok(())
        }
        _ => Err("Kont::MacroExpand reached but control is not a Value".to_string()),
    }
}

/// `(expand form)`: `form` has just been evaluated. If it is a macro call,
/// expand it one level (via `Kont::MacroExpand`); otherwise return it
/// unchanged.
fn handle_expand_arg(
    state: &mut CEKState,
    ec: &mut RunTime,
    env: EnvRef,
    next: KontRef,
) -> Result<(), String> {
    let form = match state.control {
        Control::Value(v) => v,
        _ => return Err("Kont::ExpandArg reached but control is not a Value".to_string()),
    };
    // Restore the env captured before evaluating the argument, for the same
    // reason EvalArg does: a tail call inside that evaluation may have left
    // state.env inside the callee.
    state.env = env;

    let (head, tail) = match gc_value!(form) {
        Pair(h, t) => (*h, *t),
        _ => {
            state.control = Control::Value(form);
            state.kont = next;
            return Ok(());
        }
    };
    if let Symbol(_) = gc_value!(head) {
        if let Some(callable) = identifiers::lookup(ec.heap, head, &state.env) {
            if let Some(Callable::SyntaxRules(sr)) = gc_value!(callable).as_callable() {
                // (expand form): one level of syntax-rules expansion
                let expansion = sr.expand(ec.heap, form, &state.env)?;
                state.control = Control::Value(expansion);
                state.kont = next;
                return Ok(());
            }
            if let Some(Callable::Macro {
                params, body, env, ..
            }) = gc_value!(callable).as_callable()
            {
                let raw_args = list_to_vec(ec.heap, tail)?;
                let macro_env = bind_params(params, &raw_args, env, ec.heap)?;
                let call_env = state.env.clone();
                let body = *body;
                state.kont = Rc::new(Kont::MacroExpand {
                    call_env,
                    mode: MacroMode::Expand,
                    next,
                });
                state.env = macro_env;
                state.control = Control::Expr(body);
                state.tail = false;
                return Ok(());
            }
        }
    }
    state.control = Control::Value(form);
    state.kont = next;
    Ok(())
}

/// Drives the forms parsed from an `eval-string` argument one at a time,
/// collecting each result into `results`.
fn handle_eval_seq(
    state: &mut CEKState,
    ec: &mut RunTime,
    mut forms: Box<EvalSeqForms>,
    next: KontRef,
) -> Result<(), String> {
    match state.control {
        Control::Value(val) => forms.results.push(val),
        _ => return Err("Kont::EvalSeq reached but control is not a Value".to_string()),
    }
    if let Some(next_form) = forms.remaining.pop() {
        state.control = Control::Expr(next_form);
        // More forms (or the result list) follow: not a tail position.
        state.tail = false;
        state.kont = Rc::new(Kont::EvalSeq { forms, next });
    } else {
        let list = crate::gc::list_from_slice(&forms.results, ec.heap);
        state.control = Control::Value(list);
        state.kont = next;
    }
    Ok(())
}

/// `(with-timer body)`: the body has finished; replace its value with the
/// elapsed wall-clock time.
fn handle_timer(
    state: &mut CEKState,
    ec: &mut RunTime,
    start: Instant,
    next: KontRef,
) -> Result<(), String> {
    match state.control {
        Control::Value(_) => {
            let elapsed = start.elapsed().as_secs_f64();
            let time = new_float(ec.heap, elapsed);
            state.control = Control::Value(time);
            state.kont = next;
            Ok(())
        }
        _ => Err("Kont::Timer reached but control is not a Value".to_string()),
    }
}

/// Expand the macro use `form` with `transformer` (whose payload is `sr`),
/// reusing a cached expansion of the same form by the same transformer.
/// Expansion is deterministic given those two, short of a literal like
/// `else` being rebound between evaluations of the same code, so a use in a
/// loop or a frequently called procedure is expanded once.
fn expand_use(
    rt: &mut RunTime,
    transformer: GcRef,
    sr: &crate::syntax_rules::SyntaxRules,
    form: GcRef,
    env: &EnvRef,
) -> Result<GcRef, String> {
    if let Some(expansion) = rt.heap.cached_expansion(form, transformer) {
        return Ok(expansion);
    }
    let expansion = sr.expand(rt.heap, form, env)?;
    rt.heap.cache_expansion(form, transformer, expansion);
    Ok(expansion)
}

/// Process applications that do not require argument evaluation: macros, special forms, and call-with-values.
///
fn apply_unevaluated(state: &mut CEKState, ec: &mut RunTime) -> Result<(), String> {
    let (proc, original_call, next) = match &*state.kont {
        Kont::ApplySpecial {
            proc,
            original_call,
            next,
        } => (proc, original_call, Rc::clone(next)),
        _ => return Err("apply_proc expected ApplySpecial continuation".to_string()),
    };
    match gc_value!(*proc).as_callable() {
        Some(Callable::SpecialForm { func, .. }) => {
            let result = func(*original_call, ec, state);
            match result {
                Err(err) => post_error(state, ec, &err),
                Ok(_) => {}
            }
            Ok(())
        }
        Some(Callable::Macro {
            params, body, env, ..
        }) => {
            let cdr = match gc_value!(*original_call) {
                Pair(_, cdr) => *cdr,
                _ => return Err("macro: not a proper call".to_string()),
            };
            let raw_args = list_to_vec(ec.heap, cdr)?;
            let macro_env = bind_params(params, &raw_args, env, ec.heap)?;
            let call_env = state.env.clone();
            let body = *body;
            state.kont = Rc::new(Kont::MacroExpand {
                call_env,
                mode: MacroMode::Evaluate,
                next,
            });
            state.env = macro_env;
            state.control = Control::Expr(body);
            state.tail = false;
            Ok(())
        }
        Some(Callable::SyntaxRules(sr)) => {
            // An operator expression that evaluated to a transformer (the
            // identifier case is handled directly in eval_cek).
            match expand_use(ec, *proc, sr, *original_call, &state.env) {
                Ok(expansion) => {
                    state.kont = next;
                    insert_eval(state, expansion, false);
                }
                Err(err) => post_error(state, ec, &err),
            }
            Ok(())
        }
        _ => Err("ApplySpecial: not a special form or macro".to_string()),
    }
}

/// Process Builtin, SysBuiltin, and Closure applications. Arguments are already evaluated.
///
pub fn apply_proc(state: &mut CEKState, ec: &mut RunTime) -> Result<(), String> {
    let (proc, evaluated_args, next) = match &*state.kont {
        Kont::ApplyProc {
            proc,
            evaluated_args,
            next,
        } => (proc, evaluated_args.clone(), Rc::clone(next)),
        _ => return Err("apply_proc expected ApplyProc continuation".to_string()),
    };
    //crate::utilities::dbg_kont("kont in apply_proc", &state.kont);
    match gc_value!(*proc) {
        Callable(callable) => match &**callable {
            Callable::Builtin { func, .. } => {
                // On error, post_error has already halted the machine; setting
                // `next` here as well would resume the caller with a void value.
                match func(ec.heap, &evaluated_args) {
                    Err(err) => post_error(state, ec, &err),
                    Ok(result) => {
                        state.control = Control::Value(result);
                        state.kont = next;
                    }
                }
                Ok(())
            }
            Callable::SysBuiltin { func, .. } => {
                // SysBuiltin functions must advance the kont themselves
                match func(ec, &evaluated_args[..], state, next) {
                    Err(err) => post_error(state, ec, &err),
                    Ok(_) => {}
                }
                Ok(())
            }
            Callable::Closure {
                params,
                body,
                env: closure_env,
                ..
            } => {
                let new_env = bind_params(&params[..], &evaluated_args, &closure_env, ec.heap)?;
                let old_env = state.env.clone();

                if state.tail {
                    // Tail-call optimization: no RestoreEnv frame.
                    state.env = new_env;
                    state.kont = next; // reuse continuation depth
                    state.control = Control::Expr(*body);

                    // A tail call never pushes RestoreEnv, so a purely
                    // tail-recursive loop would otherwise never hit the only
                    // other automatic GC checkpoint (handle_restore_env) and
                    // could grow unbounded. Same invariant as there: state is
                    // fully installed above, so collecting now is safe.
                    if ec.heap.needs_gc() {
                        ec.heap.collect_garbage(
                            state,
                            *ec.current_output_port,
                            ec.port_stack,
                            ec.dynamic_wind,
                            ec.arg_stack,
                            *ec.handlers,
                        );
                    }

                    Ok(())
                } else {
                    // Normal (non-tail) call: push a RestoreEnv barrier.
                    state.kont = Rc::new(Kont::RestoreEnv {
                        old_env,
                        next: next,
                    });
                    state.env = new_env;
                    state.control = Control::Expr(*body);
                    // The body is in tail position relative to its own
                    // frame: the RestoreEnv below restores the caller's env,
                    // so calls in tail position inside it need no frame of
                    // their own. (Forms inside the body inherit this.)
                    state.tail = true;
                    Ok(())
                }
            }
            Callable::CaseLambda { clauses } => {
                // Apply the first clause that accepts this many arguments.
                let chosen = crate::eval::select_clause(clauses, evaluated_args.len())?;
                state.kont = Rc::new(Kont::ApplyProc {
                    proc: chosen,
                    evaluated_args,
                    next,
                });
                apply_proc(state, ec)
            }
            _ => Err("apply_proc expected Builtin or Closure".to_string()),
        },
        _ => Err("Attempt to apply non-callable".to_string()),
    }
}
