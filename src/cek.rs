/// Continuation-Passing Style (CPS) evaluator.
///
use crate::env::EnvRef;
use crate::eval::{EvalContext, bind_params, eval_macro};
use crate::gc::SchemeValue::*;
use crate::gc::{Callable, GcRef, cons, is_false, list_to_vec};
use crate::gc_value;
use crate::kont::{
    AndOrKind, CEKState, CondClause, Control, EvalPhase, Kont, KontRef, insert_eval,
};
use crate::printer::print_value;
use crate::utilities::{debug_cek, dump_cek, post_error};
use std::rc::Rc;

/// CEK evaluator entry point from the repl (not used recursively)
///
pub fn eval_main(expr: GcRef, ec: &mut EvalContext) -> Result<GcRef, String> {
    let state = CEKState {
        control: Control::Expr(expr),
        kont: Rc::new(Kont::Halt),
        env: ec.env.current_frame(),
        tail: false,
    };
    dump_cek("***eval_main***", &state);
    run_cek(state, ec)
}

/// Run the CEK evaluator loop until a result is produced.
///
/// All the state information is contained in the CEKState struct, so no result is returned until the end.
///
fn run_cek(mut state: CEKState, ctx: &mut EvalContext) -> Result<GcRef, String> {
    loop {
        // Step the CEK machine; mutates state in place
        //eprintln!("Pre-step {}", frame_debug_short(&state.kont));
        if let Err(err) = step(&mut state, ctx) {
            return Err(err);
        }
        match &state.control {
            Control::Value(val) => match *state.kont {
                Kont::Halt => return Ok(*val), // fully evaluated, exit
                _ => continue,                 // Some continuation remains; continue loop
            },
            Control::Expr(_) => continue, // Still evaluating an expression; continue loop
            Control::Empty => return Err("CEK: Control::Empty".to_string()),
            Control::Escape(val, _) => return Ok(*val),
        }
    }
}

/// Perform one step of the CEK evaluation.
///
/// This function looks at the current control expression and continuation frame,
/// resolving symbols, starting applications, and handling continuations as needed.
///

fn step(state: &mut CEKState, ec: &mut EvalContext) -> Result<(), String> {
    dump_cek("  step", &state);
    if *ec.trace > 0 && ec.depth <= ec.trace {
        debug_cek(state, ec);
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
            let kont = take_kont(state);
            state.control = Control::Value(val);
            dispatch_kont(state, ec, val, kont)
        }
        Control::Escape(val, kont) => {
            *ec.depth -= 1;
            state.control = Control::Value(val);
            state.kont = kont;
            Ok(())
        }
        Control::Empty => Err("Unexpected Control::Halt in step()".to_string()),
    }
}

#[inline]
fn take_kont(state: &mut CEKState) -> Kont {
    let old = std::mem::replace(&mut state.kont, Rc::new(Kont::Halt));
    match Rc::try_unwrap(old) {
        Ok(k) => k,                             // fast path: unique Rc → owned Kont
        Err(shared) => shared.as_ref().clone(), // fallback: shallow clone
    }
}

#[inline]
fn dispatch_kont(
    state: &mut CEKState,
    ec: &mut EvalContext,
    val: GcRef,
    kont: Kont,
) -> Result<(), String> {
    match kont {
        Kont::AndOr { kind, rest, next } => handle_and_or(state, ec, kind, rest, next),
        Kont::ApplySpecial {
            proc,
            original_call,
            next,
        } => handle_apply_special(state, ec, proc, original_call, next),
        Kont::Bind { symbol, env, next } => handle_bind(state, ec, symbol, env, next),
        Kont::Cond { remaining, next } => handle_cond(state, ec, remaining, next),
        Kont::CondClause { clause, next } => handle_cond_clause(state, ec, clause, next),
        Kont::EvalArg {
            proc,
            remaining,
            evaluated,
            original_call,
            tail,
            env,
            next,
        } => handle_eval_arg(
            state,
            ec,
            val,
            proc,
            remaining,
            evaluated,
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
        } => handle_eval(state, ec, expr, env, phase, next),
        Kont::If {
            then_branch,
            else_branch,
            next,
        } => handle_if(state, ec, then_branch, else_branch, next),
        Kont::RestoreEnv { old_env, next } => handle_restore_env(state, ec, old_env, val, next),
        Kont::Seq { rest, next } => handle_seq(state, ec, rest, next),
        Kont::Halt => Ok(()),
        _ => Err("unexpected continuation".to_string()),
    }
}

fn handle_and_or(
    state: &mut CEKState,
    _ec: &mut EvalContext,
    kind: AndOrKind,
    mut rest: Vec<GcRef>,
    next: Rc<Kont>,
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
    ec: &mut EvalContext,
    proc: GcRef,
    original_call: GcRef,
    next: KontRef,
) -> Result<(), String> {
    let frame = Kont::ApplySpecial {
        proc,
        original_call,
        next,
    };
    state.tail = false;
    apply_special(state, ec, frame)?;
    Ok(())
}

fn handle_bind(
    state: &mut CEKState,
    ec: &mut EvalContext,
    symbol: GcRef,
    env: Option<EnvRef>,
    next: KontRef,
) -> Result<(), String> {
    // borrow cont fields
    if let Control::Value(val) = state.control {
        match env {
            Some(frame) => {
                // global and set! path: update specific frame
                frame.borrow_mut().set_local(symbol, val);
                state.control = Control::Value(ec.heap.unspecified());
            }
            None => {
                // local define path: bind in current frame
                state.env.borrow_mut().set_local(symbol, val);
                state.control = Control::Value(symbol);
            }
        }
        state.tail = false;
        state.kont = next;
        Ok(())
    } else {
        Err("Kont::Bind reached but control is not a Value".to_string())
    }
}

fn handle_cond(
    state: &mut CEKState,
    ec: &mut EvalContext,
    mut remaining: Vec<CondClause>,
    next: KontRef,
) -> Result<(), String> {
    if let Some(clause) = remaining.pop() {
        state.kont = Rc::new(Kont::Cond { remaining, next });
        let test_expr = match &clause {
            CondClause::Normal { test, .. } => test,
            CondClause::Arrow { test, .. } => test,
        };
        // Evaluate the test
        state.control = Control::Expr(*test_expr);
        state.tail = false;
        // Push a CondClause frame whose `next` points to the Cond we just installed.
        //    We grab the Cond frame we just installed by replacing state.kont with Halt (cheap).
        //let cond_frame = std::mem::replace(&mut state.kont, Rc::new(Kont::Halt));
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
    ec: &mut EvalContext,
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
        _ => return Err("CondClause: Unexpected control state".to_string()),
    };
    // helper to skip the Cond frame that wraps each clause
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
                    insert_eval(state, body_expr, true);
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

                // build (arrow_proc test_value)
                let mut call_expr = cons(*test_value, ec.heap.nil_s(), ec.heap)?;
                call_expr = cons(arrow_proc, call_expr, ec.heap)?;
                insert_eval(state, call_expr, true);
            } else {
                state.kont = next; // keep Cond for more clauses
            }
        }
    }
    Ok(())
}

fn handle_eval(
    state: &mut CEKState,
    _ec: &mut EvalContext,
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
                state.kont = Rc::new(Kont::Eval {
                    expr,
                    env: new_env,
                    phase: EvalPhase::EvalExpr,
                    next: Rc::clone(&next),
                });
            } else {
                unreachable!("EvalEnv phase should yield a value");
            }
        }
        EvalPhase::EvalExpr => {
            if let Control::Value(val) = state.control {
                // Save the intermediate result, then prepare to re-evaluate
                state.control = Control::Expr(val);
                state.kont = Rc::new(Kont::Eval {
                    expr: val,
                    env,
                    phase: EvalPhase::Done,
                    next: Rc::clone(&next),
                });
            } else {
                unreachable!("EvalExpr phase should yield a value");
            }
        }
        EvalPhase::Done => {
            if let Control::Value(val) = state.control {
                // Final value: propagate it
                state.control = Control::Value(val);
                state.kont = Rc::clone(&next);
            } else {
                unreachable!("Done phase should yield a value");
            }
        }
    }
    Ok(())
}

fn handle_eval_arg(
    state: &mut CEKState,
    ec: &mut EvalContext,
    val: GcRef,
    proc: Option<GcRef>,
    mut remaining: Vec<GcRef>,
    mut evaluated: Vec<GcRef>,
    original_call: GcRef,
    tail: bool,
    env: EnvRef,
    next: KontRef,
) -> Result<(), String> {
    if proc.is_none() {
        // Evaluating operator (car of the call)
        match ec.heap.get_value(val) {
            Callable(Callable::SpecialForm { .. }) | Callable(Callable::Macro { .. }) => {
                let frame = Kont::ApplySpecial {
                    proc: val,
                    original_call,
                    next,
                };
                apply_special(state, ec, frame)?;
                return Ok(());
            }
            _ => {
                let proc = Some(val);
                if remaining.is_empty() {
                    // zero-arg call
                    let frame = Rc::new(Kont::ApplyProc {
                        proc: proc.unwrap(),
                        evaluated_args: evaluated,
                        next,
                    });
                    state.tail = true;
                    apply_proc(state, ec, frame)?;
                    return Ok(());
                } else {
                    // evaluate next arg
                    if let Some(next_expr) = remaining.pop() {
                        state.control = Control::Expr(next_expr);
                        state.tail = false;
                        state.kont = Rc::new(Kont::EvalArg {
                            proc,
                            remaining,
                            evaluated,
                            original_call,
                            tail,
                            env,
                            next,
                        });
                    }
                    return Ok(());
                }
            }
        }
    } else {
        // We already have a proc; val is an evaluated argument
        evaluated.push(val);
        if let Some(next_expr) = remaining.pop() {
            state.control = Control::Expr(next_expr);
            state.tail = false;
            state.kont = Rc::new(Kont::EvalArg {
                proc,
                remaining,
                evaluated,
                original_call,
                tail,
                env,
                next,
            });
        } else {
            // Done: apply with evaluated args
            let frame = Rc::new(Kont::ApplyProc {
                proc: proc.unwrap(),
                evaluated_args: evaluated,
                next,
            });
            apply_proc(state, ec, frame)?;
        }
        return Ok(());
    }
}

fn handle_if(
    state: &mut CEKState,
    _ec: &mut EvalContext,
    then_branch: GcRef,
    else_branch: GcRef,
    next: KontRef,
) -> Result<(), String> {
    dump_cek("handle_if", state);
    match &state.control {
        Control::Value(val) => {
            if is_false(*val) {
                state.control = Control::Expr(else_branch);
            } else {
                state.control = Control::Expr(then_branch);
            }
            state.tail = true;
            state.kont = next;
            Ok(())
        }
        _ => Err("Kont::If reached but control is not a value".to_string()),
    }
}

fn handle_restore_env(
    state: &mut CEKState,
    ec: &mut EvalContext,
    old_env: EnvRef,
    val: GcRef,
    next: Rc<Kont>,
) -> Result<(), String> {
    // Restore environment
    ec.env.set_current_frame(old_env.clone());
    state.env = old_env;
    // Propagate the value
    state.control = Control::Value(val);
    // Pop this frame unconditionally
    state.kont = next;
    // Reset tail because restoring the environment is not a tail position
    state.tail = false;

    Ok(())
}

fn handle_seq(
    state: &mut CEKState,
    _ec: &mut EvalContext,
    mut rest: Vec<GcRef>,
    next: Rc<Kont>,
) -> Result<(), String> {
    // Pop the next expression from the sequence
    let next_expr = rest.pop().unwrap(); // safe: Seq always has >=2 exprs

    state.control = Control::Expr(next_expr);
    state.tail = false;

    // Determine if this is the last expression
    if rest.is_empty() {
        // Last expression: mark tail if needed and drop the Seq frame
        state.tail = true;
        state.kont = next;
    } else {
        state.kont = Rc::new(Kont::Seq { rest, next });
    }
    Ok(())
}

/// Capture the current environment and control state, then pass the CEKState to the CEK loop.
///
pub fn eval_cek(expr: GcRef, ctx: &mut EvalContext, state: &mut CEKState) {
    dump_cek("   eval_cek", &state);
    match &gc_value!(expr) {
        // Self-evaluating values
        Int(_) | Float(_) | Str(_) | Bool(_) | Vector(_) | Char(_) | Nil | Continuation(_) => {
            state.control = Control::Value(expr);
        }
        Symbol(name) => {
            if let Some(val) = ctx.env.get_symbol(expr) {
                state.control = Control::Value(val);
            } else {
                post_error(state, ctx, format!("Unbound variable: {}", name));
            }
        }

        // Callables: Builtin, Closure, Macro, or SpecialForm
        Callable(_) => {
            state.control = Control::Value(expr);
        }

        // Pair: potentially a function application
        Pair(car, cdr) => {
            // Quick path: is this a special form?
            if let Symbol(_) = &gc_value!(*car) {
                if let Some(op) = ctx.env.get_symbol(*car) {
                    if matches!(gc_value!(op), Callable(Callable::SpecialForm { .. })) {
                        // Call the special-form handler in-place.
                        state.control = Control::Expr(*car); // or set Value/Expr as your handler expects
                        let result = apply_special_direct(expr, ctx, state);
                        match result {
                            Ok(_) => {}
                            Err(err) => post_error(state, ctx, err),
                        }
                        return; // let run_cek loop continue
                    }
                }
            }

            let args_vec = list_to_vec(&ctx.heap, *cdr)
                .map_err(|_| "invalid argument list".to_string())
                .unwrap();

            //let prev = std::mem::replace(&mut state.kont, Rc::new(Kont::Halt));
            let prev = Rc::clone(&state.kont);

            state.control = Control::Expr(*car);
            state.tail = false; // reset after using it
            state.kont = Rc::new(Kont::EvalArg {
                proc: None,
                remaining: args_vec.into_iter().rev().collect(),
                evaluated: vec![],
                tail: true,
                original_call: expr,
                env: state.env.clone(),
                next: prev,
            });
        }
        _ => post_error(
            state,
            ctx,
            format!("Unsupported expression in eval_cek: {}", print_value(&expr)),
        ),
    }
}

fn apply_special_direct(
    expr: GcRef,
    ec: &mut EvalContext,
    state: &mut CEKState,
) -> Result<(), String> {
    let op_sym = crate::gc::car(expr)?;
    let op_val = ec.env.get_symbol(op_sym).ok_or("unbound special form")?;
    //println!("op_val: {}", print_scheme_value(&op_val));
    if let Callable(Callable::SpecialForm { func, .. }) = gc_value!(op_val) {
        func(expr, ec, state) // handler mutates state; returns Ok(()) immediately
    } else {
        unreachable!("early-dispatch promised a SpecialForm");
    }
}

/// Process SpecialForm and Macro applications. Argument evaluation is deferred to the callee.
///
fn apply_special(state: &mut CEKState, ec: &mut EvalContext, frame: Kont) -> Result<(), String> {
    if let Kont::ApplySpecial {
        proc,
        original_call,
        next,
    } = frame
    {
        match gc_value!(proc) {
            Callable(Callable::SpecialForm { func, .. }) => {
                let result = func(original_call, ec, state);
                match result {
                    Err(err) => post_error(state, ec, err),
                    Ok(_) => {}
                }
                Ok(())
            }
            Callable(Callable::Macro { params, body, env }) => {
                let (_, cdr) = match gc_value!(original_call) {
                    Pair(_, cdr) => ((), *cdr),
                    _ => return Err("macro: not a proper call".to_string()),
                };
                let raw_args = list_to_vec(ec.heap, cdr)?;
                let expanded = eval_macro(&params, *body, &env, &raw_args, ec);
                match expanded {
                    Ok(exp) => state.control = Control::Expr(exp),
                    Err(err) => post_error(state, ec, err),
                }
                state.tail = true;
                state.kont = next;
                Ok(())
            }
            _ => Err("ApplySpecial: not a special form or macro".to_string()),
        }
    } else {
        unreachable!();
    }
}

/// Process Builtin and Closure applications. Arguments are already evaluated.
///
pub fn apply_proc(
    state: &mut CEKState,
    ec: &mut EvalContext,
    frame: KontRef,
) -> Result<(), String> {
    dump_cek("     apply_proc", &state);

    if let Kont::ApplyProc {
        proc,
        evaluated_args,
        next,
    } = frame.as_ref()
    {
        match gc_value!(*proc) {
            Callable(callable) => match callable {
                Callable::Builtin { func, .. } => {
                    let result = func(ec, &evaluated_args);
                    match result {
                        Err(err) => post_error(state, ec, err),
                        Ok(result) => state.control = Control::Value(result),
                    }
                    state.kont = Rc::clone(next);
                    Ok(())
                }
                Callable::SysBuiltin { func, .. } => {
                    let result = func(ec, &evaluated_args, state);
                    match result {
                        Err(err) => post_error(state, ec, err),
                        Ok(_) => {}
                    }
                    state.kont = Rc::clone(next);
                    Ok(())
                }
                Callable::Closure {
                    params,
                    body,
                    env: closure_env,
                } => {
                    let new_env = bind_params(params, &evaluated_args, closure_env, ec.heap)?;
                    let old_env = state.env.clone();

                    if state.tail {
                        // Tail-call optimization: no RestoreEnv frame.
                        ec.env.set_current_frame(new_env.current_frame());
                        state.env = new_env.current_frame();
                        state.kont = Rc::clone(next); // reuse continuation depth
                        state.control = Control::Expr(*body);
                        Ok(())
                    } else {
                        // Normal (non-tail) call: push a RestoreEnv barrier.
                        state.kont = Rc::new(Kont::RestoreEnv {
                            old_env,
                            next: Rc::clone(next),
                        });
                        ec.env.set_current_frame(new_env.current_frame());
                        state.env = new_env.current_frame();
                        state.control = Control::Expr(*body);
                        Ok(())
                    }
                }
                _ => Err("apply_proc expected Builtin or Closure".to_string()),
            },
            _ => Err("Attempt to apply non-callable".to_string()),
        }
    } else {
        unreachable!()
    }
}
