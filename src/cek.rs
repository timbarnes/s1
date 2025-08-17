/// Continuation-Passing Style (CPS) evaluator.
///
use crate::env::EnvRef;
use crate::eval::{EvalContext, bind_params, eval_macro};
use crate::gc::SchemeValue::*;
use crate::gc::{Callable, GcRef, SchemeValue, list_to_vec};
use crate::gc_value;
use crate::printer::print_scheme_value;
//use std::cell::RefCell;
//use std::rc::Rc;

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
        }
    }
}

enum Control {
    Expr(GcRef),  // Unevaluated expression
    Value(GcRef), // Fully evaluated result
}
pub struct CEKState {
    control: Control, // Current expression
    env: EnvRef,      // Current environment (linked frame or hashmap)
    kont: Kont,       // Continuation (enum)
}

/// CEK evaluator entry point
///

pub fn eval_main(expr: GcRef, ec: &mut EvalContext, _tail: bool) -> Result<GcRef, String> {
    let state = CEKState {
        control: Control::Expr(expr),
        kont: Kont::Halt,
        env: ec.env.current_frame().clone(),
    };
    Ok(run_cek(state, ec))
}

/// Run the CEK evaluator loop until a result is produced.
///
/// All the state information is contained in the CEKState struct, so no result is returned until the end.
///
pub fn run_cek(mut state: CEKState, ctx: &mut EvalContext) -> GcRef {
    loop {
        dump("run_cek", &state, ctx);
        // Step the CEK machine; mutates state in place
        if let Err(err) = step(&mut state, ctx) {
            panic!("CEK evaluation error: {}", err);
        }

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
    dump("step", &state, ec);
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
                    // First value in EvalArg is the proc
                    // If it's a SpecialForm or Macro, short-circuit to ApplySpecial.
                    match ec.heap.get_value(*val) {
                        SchemeValue::Callable(Callable::SpecialForm { .. })
                        | SchemeValue::Callable(Callable::Macro { .. }) => {
                            let frame = Kont::ApplySpecial {
                                proc: *val,
                                original_call: *original_call,
                                next: next.clone(),
                            };
                            apply_special(state, ec, frame)?;
                            return Ok(());
                        }
                        _ => {
                            // Normal path: treat as a procedure and start evaluating args
                            *proc = Some(*val);
                            if let Some(next_expr) = remaining.pop() {
                                state.control = Control::Expr(next_expr);
                            }
                            return Ok(());
                        }
                    }
                } else {
                    // Already have a proc, push evaluated argument, continue or apply
                    evaluated.push(*val);
                    if let Some(next_expr) = remaining.pop() {
                        state.control = Control::Expr(next_expr);
                    } else {
                        // All arguments evaluated; apply procedure
                        let frame = Kont::ApplyProc {
                            proc: proc.unwrap(),
                            evaluated_args: evaluated.clone(),
                            next: next.clone(),
                        };
                        apply_proc(state, ec, frame)?;
                    }
                    Ok(())
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

            // Other continuations do nothing here
            _ => Ok(()),
        },
    }
}

/// Capture the current environment and control state, then pass the CEKState to the CEK loop.
///
pub fn eval_cek(expr: GcRef, ctx: &mut EvalContext, state: &mut CEKState) {
    dump("eval_cek", &state, ctx);
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

        // Pair: potentially a function application
        Pair(car, cdr) => {
            let current_kont = std::mem::replace(&mut state.kont, Kont::Halt);

            // Turn the cdr (a proper list) into a flat Vec<GcRef]
            let args_vec = list_to_vec(&ctx.heap, *cdr)
                .map_err(|_| "invalid argument list".to_string())
                .unwrap();

            state.control = Control::Expr(*car);
            state.kont = Kont::EvalArg {
                proc: None,
                remaining: args_vec.into_iter().rev().collect(),
                evaluated: vec![],
                original_call: expr,
                env: state.env.clone(),
                next: Box::new(current_kont),
            };
        }
        // Callables: Builtin, Closure, Macro, or SpecialForm
        Callable(_) => {
            state.control = Control::Value(expr);
        }

        _ => panic!(
            "Unsupported expression in eval_cek: {}",
            print_scheme_value(ctx, &expr)
        ),
    }
    dump("eval_cek exit", &state, ctx);
}

// fn resolve_symbol(state: &mut CEKState, ec: &mut EvalContext) {
//     if let SchemeValue::Symbol(_) = gc_value!(state.control) {
//         if let Some(val) = ec.env.get_symbol(state.control) {
//             state.control = val;
//         }
//     }
// }

/// Helper: returns true if `control` is already a value
fn is_value(control: GcRef, _ec: &EvalContext) -> bool {
    match gc_value!(control) {
        Pair(_, _) => false,
        Symbol(_) => false,
        _ => true,
    }
}

// --- Helper: dispatch based on continuation frame ---
// fn handle_continuation(state: &mut CEKState, ec: &mut EvalContext) -> Result<(), String> {
//     // eprintln!(
//     //     "handle_continuation: control={}, kont={:?}",
//     //     print_scheme_value(ec, &state.control),
//     //     state.kont
//     // );
//     dump("handle_continuation", &state, ec);
//     let frame = std::mem::replace(&mut state.kont, Kont::Halt);
//     match frame {
//         Kont::Halt => Ok(()),
//         Kont::RestoreEnv { old_env, next } => {
//             ec.env.set_current_frame(old_env.clone());
//             state.env = old_env;
//             state.kont = *next;
//             Ok(())
//         }
//         Kont::EvalArg { .. } => eval_arg(state, ec, frame),
//         Kont::ApplySpecial { .. } => apply_special(state, ec, frame),
//         Kont::ApplyProc { .. } => apply_proc(state, ec, frame),
//     }
// }

/// Evaluate arguments for a Closure or Builtin function.
///
/// Also used for the evaluation of the car of an expression to determine the operator.
///
// pub fn eval_arg(state: &mut CEKState, ec: &mut EvalContext, frame: Kont) -> Result<(), String> {
//     dump("eval_arg", &state, ec);
//     match frame {
//         Kont::EvalArg {
//             proc,
//             mut remaining,
//             mut evaluated,
//             original_call,
//             env: _,
//             next,
//         } => {
//             if proc.is_none() {
//                 // First step: evaluate the procedure itself
//                 state.control = Control::Expr(original_call);
//                 state.kont = Kont::EvalArg {
//                     proc: None,
//                     remaining,
//                     evaluated,
//                     original_call,
//                     env: state.env.clone(),
//                     next,
//                 };
//             } else if let Some(arg_expr) = remaining.pop() {
//                 // Still have arguments to evaluate
//                 state.control = Control::Expr(arg_expr);
//                 evaluated.push(original_call); // push the last evaluated arg
//                 state.kont = Kont::EvalArg {
//                     proc,
//                     remaining,
//                     evaluated,
//                     original_call,
//                     env: state.env.clone(),
//                     next,
//                 };
//             } else {
//                 // All arguments evaluated, now apply
//                 evaluated.push(original_call); // push the final arg

//                 let apply_frame = match proc {
//                     Some(p) => match ec.heap.get_value(p) {
//                         SchemeValue::Callable(Callable::SpecialForm { .. })
//                         | SchemeValue::Callable(Callable::Macro { .. }) => Kont::ApplySpecial {
//                             proc: p,
//                             original_call,
//                             next,
//                         },
//                         _ => Kont::ApplyProc {
//                             proc: p,
//                             evaluated_args: evaluated,
//                             next,
//                         },
//                     },
//                     None => return Err("EvalArg: missing procedure".to_string()),
//                 };

//                 state.kont = apply_frame;
//             }

//             Ok(())
//         }
//         _ => Err("eval_arg called with non-EvalArg kont".to_string()),
//     }
// }

/// Process SpecialForm and Macro applications. Argument evaluation is deferred to the callee.
///
fn apply_special(state: &mut CEKState, ec: &mut EvalContext, frame: Kont) -> Result<(), String> {
    dump("apply_special", &state, ec);
    if let Kont::ApplySpecial {
        proc,
        original_call,
        next,
    } = frame
    {
        match gc_value!(proc) {
            Callable(Callable::SpecialForm { func, .. }) => {
                let result = func(original_call, ec, false)?;
                state.control = Control::Value(result);
                state.kont = *next;
                Ok(())
            }
            Callable(Callable::Macro { params, body, env }) => {
                let (_, cdr) = match gc_value!(original_call) {
                    SchemeValue::Pair(_, cdr) => ((), *cdr),
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
    dump("apply_proc", &state, ec);

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
                    // Bind the arguments into a new environment frame based on the closure's captured env
                    let new_env = bind_params(params, &evaluated_args, closure_env, ec.heap)?;

                    // Save the current CEK environment so we can restore it later
                    let old_env = state.env.clone();

                    // Update CEKState to use the new closure frame
                    state.env = new_env.current_frame();

                    // Update EvalContext to point to the new closure frame
                    ec.env.set_current_frame(state.env.clone());

                    // Push a continuation to restore the old environment after the closure finishes
                    state.kont = Kont::RestoreEnv { old_env, next };

                    // Start evaluating the closure body
                    state.control = Control::Expr(*body);
                    Ok(())
                }
                _ => Err("apply_proc: expected Builtin or Closure".to_string()),
            },
            _ => Err("Attempt to apply non-callable".to_string()),
        }
    } else {
        unreachable!()
    }
}

fn dump(loc: &str, state: &CEKState, ec: &EvalContext) {
    if false {
        match &state.control {
            Control::Expr(obj) => {
                eprintln!(
                    "[{loc:14}] Control: Expr={:20}; Kont={}",
                    print_scheme_value(ec, obj),
                    frame_debug_short(&state.kont)
                );
            }
            Control::Value(obj) => {
                eprintln!(
                    "[{loc:14}] Control: Value={:20}; Kont={}",
                    print_scheme_value(ec, obj),
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
        other => format!("{:?}", other),
    }
}
