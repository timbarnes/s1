/// Continuation-Passing Style (CPS) evaluator.
///
use crate::env::EnvRef;
use crate::eval::{EvalContext, bind_params, eval_macro};
use crate::gc::SchemeValue::*;
use crate::gc::{Callable, GcRef, cons, is_false, list_to_vec};
use crate::gc_value;
use crate::printer::print_scheme_value;

#[derive(Clone)]
pub enum Kont {
    Halt,
    RestoreEnv {
        old_env: EnvRef,
        next: Box<Kont>,
    },
    EvalArg {
        proc: Option<GcRef>,
        remaining: Vec<GcRef>,
        evaluated: Vec<GcRef>,
        original_call: GcRef,
        env: EnvRef,
        next: Box<Kont>,
    },
    ApplyProc {
        proc: GcRef,
        evaluated_args: Vec<GcRef>,
        next: Box<Kont>,
    },
    ApplySpecial {
        proc: GcRef,          // Callable::SpecialForm or Callable::Macro
        original_call: GcRef, // whole form: (op . args)
        next: Box<Kont>,
    },
    If {
        // processes if
        then_branch: GcRef,
        else_branch: GcRef,
        //was_tail: bool,
        next: Box<Kont>,
    },
    Cond {
        // processes cond
        remaining: Vec<CondClause>,
        next: Box<Kont>,
    },
    CondClause {
        clause: CondClause,
        next: Box<Kont>,
    },
    Seq {
        rest: Vec<GcRef>, // remaining expressions in the sequence (head first)
        was_tail: bool,
        next: Box<Kont>,
    },
    Bind {
        // processes define
        symbol: GcRef, // body expression (or begin)
        //env_for_bind: EnvRef, // environment to use as parent for the new frame
        next: Box<Kont>,
    },
    // Exception handler frame - used later for raise/handler support
    Handler {
        handler_expr: GcRef,
        handler_env: EnvRef,
        next: Box<Kont>,
    },
}

impl std::fmt::Debug for Kont {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Kont::Halt => write!(f, "Halt"),
            Kont::RestoreEnv { old_env: _, next } => {
                write!(f, "RestoreEnv: next: {:?}", next)
            }
            Kont::EvalArg {
                proc,
                remaining,
                evaluated,
                original_call,
                next,
                ..
            } => {
                write!(
                    f,
                    "EvalArg {{ proc: {:?}, remaining: {:?}, evaluated: {:?}, original_call: {:?}, next: {:?} }}",
                    proc, remaining, evaluated, original_call, next
                )
            }
            Kont::ApplyProc {
                proc,
                evaluated_args,
                next,
            } => {
                write!(
                    f,
                    "ApplyProc {{ proc: {:?}, evaluated_args: {:?}, next: {:?} }}",
                    proc, evaluated_args, next
                )
            }
            Kont::ApplySpecial {
                proc,
                original_call,
                next,
            } => {
                write!(
                    f,
                    "ApplySpecial {{ proc: {:?}, original_call: {:?}, next: {:?} }}",
                    proc, original_call, next
                )
            }
            Kont::Bind { symbol, next } => {
                write!(
                    f,
                    "Bind {{ symbol: {}, next: {:?} }}",
                    print_scheme_value(symbol),
                    next
                )
            }
            Kont::If {
                then_branch,
                else_branch,
                next,
            } => {
                write!(
                    f,
                    "AfterTest {{ then: {}, else_: {}, next: {:?} }}",
                    print_scheme_value(then_branch),
                    print_scheme_value(else_branch),
                    next
                )
            }
            Kont::Cond { remaining, next } => {
                write!(
                    f,
                    "Cond {{ remaining: {}, next: {:?} }}",
                    remaining.len(),
                    next
                )
            }
            Kont::CondClause { clause, next } => {
                match clause {
                    CondClause::Normal { test, body} => {
                        match body {
                            Some(body) => {
                                write!(f, "CondClause {{ clause: Normal(test:{}, body:{}), next: {:?} }}", 
                                print_scheme_value(test), print_scheme_value(body), next)
                            }
                            None => {
                                write!(f, "CondClause {{ clause: Normal(test:{}, body:None), next: {:?} }}", 
                                print_scheme_value(test), next)
                            }
                        }
                    }
                    CondClause::Arrow { test, arrow_proc } => {
                        write!(f, "CondClause {{ clause: Arrow(test={}, proc={}), next: {:?} }}", 
                               print_scheme_value(test), print_scheme_value(arrow_proc), next)
                    }
                }
            }
            _ => write!(f, "Unknown continuation"),
        }
    }
}

#[derive(Clone)]
pub enum CondClause {
    Normal { test: GcRef, body: Option<GcRef> },
    Arrow { test: GcRef, arrow_proc: GcRef },
}

enum Control {
    Expr(GcRef),  // Unevaluated expression
    Value(GcRef), // Fully evaluated result
}

pub struct CEKState {
    control: Control, // Current expression
    env: EnvRef,      // Current environment (linked frame or hashmap)
    kont: Kont,       // Continuation (enum)
    tail: bool,
}

/// CEK evaluator entry point from the repl (not used recursively)
///
pub fn eval_main(expr: GcRef, ec: &mut EvalContext) -> Result<GcRef, String> {
    let state = CEKState {
        control: Control::Expr(expr),
        kont: Kont::Halt,
        env: ec.env.current_frame().clone(),
        tail: false,
    };
    dump("***eval_main***", &state);
    Ok(run_cek(state, ec))
}

/// Install `expr` into the existing CEKState and return immediately.
/// If `replace_next` is true, the installed EvalArg (if any) will have `next = Halt`
/// (i.e., it will replace the current continuation); otherwise the existing kont chain is preserved.
///
pub fn insert_eval(state: &mut CEKState, expr: GcRef, replace_next: bool) {
    // if replace_next, set state.kont to Halt so eval_cek will capture Halt as the `current_kont`
    // and set EvalArg.next = Box::new(Kont::Halt); otherwise leave state.kont as-is.
    state.tail = replace_next;
    state.control = Control::Expr(expr);
}

/// Return a value from a special form without evaluation.
///
pub fn insert_value(state: &mut CEKState, expr: GcRef) {
    state.control = Control::Value(expr);
}

/// Bind a symbol to a value. This is installed before evaluation of the right hand side.
/// The bind operation takes the value returned by the previous continuation.
///
pub fn insert_bind(state: &mut CEKState, sym: GcRef) {
    // clone the current continuation and link it under the new Bind
    let prev = std::mem::replace(&mut state.kont, Kont::Halt);
    state.kont = Kont::Bind {
        symbol: sym,
        next: Box::new(prev),
    };
    //eprintln!("bind_insert {}", frame_debug_short(&state.kont));
}

pub fn insert_cond(state: &mut CEKState, remaining: Vec<CondClause>) {
    let prev = std::mem::replace(&mut state.kont, Kont::Halt);
    state.kont = Kont::Cond {
        remaining,
        next: Box::new(prev),
    };
}

pub fn insert_if(state: &mut CEKState, then_branch: GcRef, else_branch: GcRef) {
    let prev = std::mem::replace(&mut state.kont, Kont::Halt);
    state.kont = Kont::If {
        then_branch,
        else_branch,
        next: Box::new(prev),
    };
}

/// Run the CEK evaluator loop until a result is produced.
///
/// All the state information is contained in the CEKState struct, so no result is returned until the end.
///
pub fn run_cek(mut state: CEKState, ctx: &mut EvalContext) -> GcRef {
    dump(" run_cek", &state);
    loop {
        // Step the CEK machine; mutates state in place
        //eprintln!("Pre-step {}", frame_debug_short(&state.kont));
        if let Err(err) = step(&mut state, ctx) {
            panic!("CEK evaluation error: {}", err);
        }
        //eprintln!("Post-step {}", frame_debug_short(&state.kont));
        match &state.control {
            Control::Value(val) => match &state.kont {
                Kont::Halt => return *val, // fully evaluated, exit
                _ => {
                    // Some continuation remains; continue loop
                    continue;
                }
            },
            Control::Expr(_) => {
                // Still evaluating an expression; continue loop
                continue;
            }
        }
    }
}

/// Perform one step of the CEK evaluation.
///
/// This function looks at the current control expression and continuation frame,
/// resolving symbols, starting applications, and handling continuations as needed.
///
pub fn step(state: &mut CEKState, ec: &mut EvalContext) -> Result<(), String> {
    dump("  step", &state);
    match &state.control {
        Control::Expr(expr) => {
            // Evaluate the next expression
            eval_cek(*expr, ec, state);
            Ok(())
        }

        Control::Value(val) => match &mut state.kont {
            Kont::EvalArg {
                proc,
                remaining,
                evaluated,
                original_call,
                env: _,
                next,
            } => {
                if proc.is_none() {
                    // We just evaluated the operator slot (the car). Decide what to do
                    // based on its kind.
                    match ec.heap.get_value(*val) {
                        Callable(Callable::SpecialForm { .. })
                        | Callable(Callable::Macro { .. }) => {
                            // Special forms/macros: short-circuit to ApplySpecial immediately.
                            let frame = Kont::ApplySpecial {
                                proc: *val,
                                original_call: *original_call,
                                next: next.clone(),
                            };
                            apply_special(state, ec, frame)?;
                            return Ok(());
                        }
                        _ => {
                            // Normal procedure. If there are no remaining args, apply now with zero args.
                            *proc = Some(*val);
                            if remaining.is_empty() {
                                // zero-arg call -> directly transition to ApplyProc with empty args
                                let frame = Kont::ApplyProc {
                                    proc: proc.unwrap(),
                                    evaluated_args: evaluated.clone(), // should be empty
                                    next: next.clone(),
                                };
                                apply_proc(state, ec, frame)?;
                                return Ok(());
                            } else {
                                // still have args to evaluate: evaluate next one
                                if let Some(next_expr) = remaining.pop() {
                                    state.control = Control::Expr(next_expr);
                                }
                                return Ok(());
                            }
                        }
                    }
                } else {
                    // Already have a proc; the current value is an evaluated argument.
                    evaluated.push(*val);
                    if let Some(next_expr) = remaining.pop() {
                        state.control = Control::Expr(next_expr);
                    } else {
                        // All args evaluated: go to apply with evaluated args
                        let frame = Kont::ApplyProc {
                            proc: proc.unwrap(),
                            evaluated_args: evaluated.clone(),
                            next: next.clone(),
                        };
                        apply_proc(state, ec, frame)?;
                    }
                    return Ok(());
                }
            }
            Kont::RestoreEnv { old_env, next, .. } => {
                // Restore environment and continue
                // restore EvalContext's active frame so subsequent lookups see parent's environment
                ec.env.set_current_frame(old_env.clone());
                state.env = old_env.clone();
                state.kont = *next.clone();
                state.control = Control::Value(*val);
                return Ok(());
            }

            Kont::ApplySpecial {
                proc,
                original_call,
                next,
            } => {
                let frame = Kont::ApplySpecial {
                    proc: *proc,
                    original_call: *original_call,
                    next: Box::new(*next.clone()),
                };
                apply_special(state, ec, frame)?;
                Ok(())
            }
            Kont::Bind { symbol, next } => {
                if let Control::Value(val) = state.control {
                    state.env.borrow_mut().set_local(*symbol, val);
                    // For define, we typically leave the symbol as the value
                    state.control = Control::Value(*symbol);
                    // Resume with whatever was waiting
                    state.kont = *next.clone();
                    Ok(())
                } else {
                    panic!("Kont::Bind reached but control is not a Value");
                }
            }
            Kont::If {
                then_branch,
                else_branch,
                next,
            } => {
                // The test value is in state.control
                match &state.control {
                    Control::Value(val) => {
                        match gc_value!(*val) {
                            Bool(v) => {
                                if *v {
                                    state.control = Control::Expr(*then_branch);
                                    state.kont = *next.clone();
                                    Ok(())
                                } else {
                                    state.control = Control::Expr(*else_branch);
                                    state.kont = *next.clone(); // next;
                                    Ok(())
                                }
                            }
                            _ => {
                                // All non-Bool values are treated as true
                                state.control = Control::Expr(*then_branch);
                                state.kont = *next.clone(); // next;
                                Ok(())
                            }
                        }
                    }
                    _ => panic!("Kont::AfterTest reached but control is not a value"),
                }
            }
            Kont::Cond { .. } => {
                match std::mem::replace(&mut state.kont, Kont::Halt) {
                    Kont::Cond { mut remaining, next } => {
                        if let Some(clause) = remaining.pop() {
                            state.kont = Kont::Cond { remaining, next };
                            let test_expr = match &clause {
                                CondClause::Normal { test, .. } => test.clone(),
                                CondClause::Arrow { test, .. } => test.clone(),
                            };
                            // Evaluate the test
                            state.control = Control::Expr(test_expr);
                            // Push a CondClause frame whose `next` points to the Cond we just installed.
                            //    We grab the Cond frame we just installed by replacing state.kont with Halt (cheap).
                            let cond_frame = std::mem::replace(&mut state.kont, Kont::Halt);
                            state.kont = Kont::CondClause {
                                clause,                  // move the owned clause here
                                next: Box::new(cond_frame),
                            };
                            Ok(())
                        } else {
                            // no more clauses: return false and restore the continuation after Cond
                            state.control = Control::Value(ec.heap.false_s());
                            state.kont = *next; // move inner Kont out of the Box
                            Ok(())
                        }
                    }
                    other => {
                        // Put it back and report an internal error (shouldn't happen).
                        state.kont = other;
                        Err("internal error: expected Kont::Cond".to_string())
                    }
                }
            }

            Kont::CondClause { .. } => {
                // Move the entire CondClause frame out of state.kont first
                let (clause, next_box) = match std::mem::replace(&mut state.kont, Kont::Halt) {
                    Kont::CondClause { clause, next } => (clause, next),
                    _ => return Err("internal error: expected CondClause".to_string()),
                };

                // Extract the value of the test
                let test_value = match state.control {
                    Control::Value(v) => v,
                    Control::Expr(_) => {
                        return Err("CondClause reached with unevaluated test".to_string());
                    }
                };

                match clause {
                    CondClause::Normal { body, .. } => {
                        if !is_false(test_value) {
                            if let Some(body_expr) = body {
                                // Chain two nexts: CondClause.next -> Cond frame -> rest
                                if let Kont::Cond { next: cond_next, .. } = *next_box {
                                    state.kont = *cond_next;  // skip Cond frame
                                    insert_eval(state, body_expr, false);
                                } else {
                                    return Err("expected Cond frame after CondClause".to_string());
                                }
                            } else {
                                // No body: just return test_value
                                if let Kont::Cond { next: cond_next, .. } = *next_box {
                                    state.kont = *cond_next;  // skip Cond frame
                                    state.control = Control::Value(test_value);
                                } else {
                                    return Err("expected Cond frame after CondClause".to_string());
                                }
                            }
                        } else {
                            // Test failed: keep Cond frame to continue with remaining clauses
                            state.kont = *next_box;
                        }
                    }

                    CondClause::Arrow { test: _, arrow_proc } => {
                        if !is_false(test_value) {
                            // Chain two nexts: CondClause.next -> Cond frame -> rest
                            let rest_kont = if let Kont::Cond { next: cond_next, .. } = *next_box {
                                *cond_next  // skip Cond frame
                            } else {
                                return Err("expected Cond frame after CondClause".to_string());
                            };

                            state.kont = rest_kont;

                            // Build (arrow_proc test_value) for evaluation
                            let mut call_expr = cons(test_value, ec.heap.nil_s(), ec.heap)?;
                            call_expr = cons(arrow_proc, call_expr, ec.heap)?;

                            insert_eval(state, call_expr, false);
                        } else {
                            // Test failed: continue with remaining Cond clauses
                            state.kont = *next_box;
                        }
                    }
                }

                Ok(())
            }
            // Other continuations do nothing here
            _ => Ok(()),
        },
    }
}

/// Capture the current environment and control state, then pass the CEKState to the CEK loop.
///
pub fn eval_cek(expr: GcRef, ctx: &mut EvalContext, state: &mut CEKState) {
    dump("   eval_cek", &state);
    match &gc_value!(expr) {
        // Self-evaluating values
        Int(_) | Float(_) | Str(_) | Bool(_) | Vector(_) | Char(_) | Nil => {
            state.control = Control::Value(expr);
        }

        Symbol(name) => {
            if let Some(val) = ctx.env.get_symbol(expr) {
                state.control = Control::Value(val);
            } else {
                panic!("Unbound variable: {}", name);
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
                        apply_special_direct(expr, ctx, state).unwrap();
                        return; // let run_cek loop continue
                    }
                }
            }

            let args_vec = list_to_vec(&ctx.heap, *cdr)
                .map_err(|_| "invalid argument list".to_string())
                .unwrap();

            let current_kont = state.kont.clone();
            let next_box = if state.tail {
                Box::new(Kont::Halt)
            } else {
                Box::new(current_kont)
            };

            state.control = Control::Expr(*car);
            state.kont = Kont::EvalArg {
                proc: None,
                remaining: args_vec.into_iter().rev().collect(),
                evaluated: vec![],
                original_call: expr,
                env: state.env.clone(),
                next: next_box,
            };
            state.tail = false; // reset after using it
        }
        _ => panic!(
            "Unsupported expression in eval_cek: {}",
            print_scheme_value(&expr)
        ),
    }
    //dump("eval_cek exit", &state, ctx);
}

fn apply_special_direct(
    expr: GcRef,
    ec: &mut EvalContext,
    state: &mut CEKState,
) -> Result<(), String> {
    //println!("apply_special_direct - expr: {}", print_scheme_value(&expr));
    dump("apply_special_direct", &state);
    let op_sym = crate::gc::car(ec.heap, expr)?;
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
    //dump("     apply_special", &state);
    if let Kont::ApplySpecial {
        proc,
        original_call,
        next,
    } = frame
    {
        match gc_value!(proc) {
            Callable(Callable::SpecialForm { func, .. }) => {
                func(original_call, ec, state)?;
                Ok(())
            }
            Callable(Callable::Macro { params, body, env }) => {
                let (_, cdr) = match gc_value!(original_call) {
                    Pair(_, cdr) => ((), *cdr),
                    _ => return Err("macro: not a proper call".to_string()),
                };
                let raw_args = list_to_vec(ec.heap, cdr)?;
                let expanded = eval_macro(params, *body, env, &raw_args, ec)?;
                state.control = Control::Expr(expanded);
                state.kont = *next;
                Ok(())
            }
            _ => Err("ApplySpecial: not a special form or macro".to_string()),
        }
    } else {
        unreachable!()
    }
}

/// Process Builtin and Closure applications. Arguments are already evaluated.
///
fn apply_proc(state: &mut CEKState, ec: &mut EvalContext, frame: Kont) -> Result<(), String> {
    dump("     apply_proc", &state);

    if let Kont::ApplyProc {
        proc,
        evaluated_args,
        next,
    } = frame
    {
        match gc_value!(proc) {
            Callable(callable) => match callable {
                Callable::Builtin { func, .. } => {
                    let result = func(ec, &evaluated_args)?;
                    state.control = Control::Value(result);
                    state.kont = *next;

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
                        state.kont = *next; // reuse continuation depth
                        state.control = Control::Expr(*body);
                        // keep state.tail = true; still in tail position down this path
                        Ok(())
                    } else {
                        // Normal (non-tail) call: push a RestoreEnv barrier.
                        state.kont = Kont::RestoreEnv {
                            old_env: old_env.clone(),
                            next,
                        };
                        ec.env.set_current_frame(new_env.current_frame());
                        state.env = new_env.current_frame();
                        state.control = Control::Expr(*body);
                        Ok(())
                    }
                }
                _ => Err("apply_proc: expected Builtin or Closure".to_string()),
            },
            _ => Err("Attempt to apply non-callable".to_string()),
        }
    } else {
        unreachable!()
    }
}

/// Dump a summary of the CEK machine state
///
fn dump(loc: &str, state: &CEKState) {
    if false {
        match &state.control {
            Control::Expr(obj) => {
                eprintln!(
                    "{loc:18} Control: Expr={:20}; Kont={}; Tail={}",
                    print_scheme_value(obj),
                    frame_debug_short(&state.kont),
                    state.tail
                );
            }
            Control::Value(obj) => {
                eprintln!(
                    "{loc:18} Control: Value={:20}; Kont={}",
                    print_scheme_value(obj),
                    frame_debug_short(&state.kont)
                );
            }
        }
    }
}

fn frame_debug_short(frame: &Kont) -> String {
    match frame {
        Kont::EvalArg {
            proc,
            remaining,
            evaluated,
            original_call,
            ..
        } => {
            format!(
                "EvalArg{{proc={}, rem={}, eval={}, orig={:?}}}",
                if proc.is_some() { "Some" } else { "None" },
                remaining.len(),
                evaluated.len(),
                original_call
            )
        }
        Kont::ApplyProc {
            proc,
            evaluated_args,
            ..
        } => {
            format!(
                "ApplyProc{{proc={:?}, args={}}}",
                proc,
                evaluated_args.len()
            )
        }
        Kont::ApplySpecial {
            proc,
            original_call,
            next,
        } => {
            format!(
                "ApplySpecial{{proc={}, orig={}, next={:?}}}",
                print_scheme_value(proc),
                print_scheme_value(original_call),
                next
            )
        }
        Kont::If {
            then_branch,
            else_branch,
            next,
        } => {
            format!(
                "AfterTest{{then={}, else={}, next={:?}}}",
                print_scheme_value(then_branch),
                print_scheme_value(else_branch),
                next
            )
        }
        Kont::Bind { symbol, next } => {
            format!(
                "Bind{{symbol={}, next={:?}}}",
                print_scheme_value(symbol),
                next
            )
        }
        other => format!("{:?}", other),
    }
}
