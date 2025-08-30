use crate::env::EnvRef;
use crate::gc::GcRef;
use crate::printer::print_value;
use std::rc::Rc;

pub type KontRef = Rc<Kont>;

#[derive(Clone)]
pub enum Kont {
    Halt,
    AndOr {
        kind: AndOrKind,
        rest: Vec<GcRef>, // remaining expressions in the sequence (head first)
        next: KontRef,
    },
    ApplyProc {
        proc: GcRef,
        evaluated_args: Vec<GcRef>,
        next: KontRef,
    },
    ApplySpecial {
        proc: GcRef,          // Callable::SpecialForm or Callable::Macro
        original_call: GcRef, // whole form: (op . args)
        next: KontRef,
    },
    Bind {
        symbol: GcRef, // body expression (or begin)
        env: Option<EnvRef>,
        next: KontRef,
    },
    Cond {
        // processes cond
        remaining: Vec<CondClause>,
        next: KontRef,
    },
    CondClause {
        clause: CondClause,
        next: KontRef,
    },
    Eval {
        expr: GcRef,
        env: Option<GcRef>,
        phase: EvalPhase,
        next: KontRef,
    },
    EvalArg {
        proc: Option<GcRef>,
        remaining: Vec<GcRef>,
        evaluated: Vec<GcRef>,
        original_call: GcRef,
        tail: bool,
        env: EnvRef,
        next: KontRef,
    },
    If {
        // processes if
        then_branch: GcRef,
        else_branch: GcRef,
        next: KontRef,
    },
    RestoreEnv {
        old_env: EnvRef,
        next: KontRef,
    },
    Seq {
        rest: Vec<GcRef>, // remaining expressions in the sequence (head first)
        next: KontRef,
    },
    // Exception handler frame - used later for raise/handler support
    Handler {
        handler_expr: GcRef,
        handler_env: EnvRef,
        next: KontRef,
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
            Kont::Bind {
                symbol,
                env: _,
                next,
            } => {
                write!(
                    f,
                    "Bind {{ symbol: {}, next: {:?} }}",
                    print_value(symbol),
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
                    print_value(then_branch),
                    print_value(else_branch),
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
            Kont::CondClause { clause, next } => match clause {
                CondClause::Normal { test, body } => match body {
                    Some(body) => {
                        write!(
                            f,
                            "CondClause {{ clause: Normal(test:{}, body:{}), next: {:?} }}",
                            print_value(test),
                            print_value(body),
                            next
                        )
                    }
                    None => {
                        write!(
                            f,
                            "CondClause {{ clause: Normal(test:{}, body:None), next: {:?} }}",
                            print_value(test),
                            next
                        )
                    }
                },
                CondClause::Arrow { test, arrow_proc } => {
                    write!(
                        f,
                        "CondClause {{ clause: Arrow(test={}, proc={}), next: {:?} }}",
                        print_value(test),
                        print_value(arrow_proc),
                        next
                    )
                }
            },
            Kont::Seq { rest, next } => {
                write!(f, "Seq {{ rest: {:?}, next: {:?} }}", rest, next)
            }
            Kont::AndOr { kind, rest, next } => {
                let k = match kind {
                    AndOrKind::And => "And",
                    AndOrKind::Or => "Or",
                };
                write!(
                    f,
                    "Seq {{ kind: {}, rest: {:?}, next: {:?} }}",
                    k, rest, next
                )
            }
            _ => write!(f, "Unknown continuation"),
        }
    }
}

#[derive(Clone)]
pub enum EvalPhase {
    EvalEnv,
    EvalExpr,
    Done,
}

#[derive(Clone)]
pub enum CondClause {
    Normal { test: GcRef, body: Option<GcRef> },
    Arrow { test: GcRef, arrow_proc: GcRef },
}

#[derive(Clone)]
pub enum BindKind {
    Define,
    Set,
}

#[derive(Clone)]
pub enum AndOrKind {
    And,
    Or,
}

pub enum Control {
    Expr(GcRef),  // Unevaluated expression
    Value(GcRef), // Fully evaluated result
    Error(String),
    Empty,
}

pub struct CEKState {
    pub control: Control, // Current expression
    pub env: EnvRef,      // Current environment (linked frame or hashmap)
    pub kont: KontRef,    // Continuation (enum)
    pub tail: bool,
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
    crate::utilities::dump_cek(" insert_eval", &state);
}

/// Return a value from a special form without evaluation.
///
pub fn insert_value(state: &mut CEKState, expr: GcRef) {
    state.control = Control::Value(expr);
}

pub fn insert_eval_eval(state: &mut CEKState, expr: GcRef, env: Option<GcRef>, tail: bool) {
    let prev = Rc::clone(&state.kont);
    match env {
        Some(e) => {
            // Process environment evaluation first, if provided
            state.control = Control::Expr(e);
            state.kont = Rc::new(Kont::Eval {
                expr,
                env: None,
                phase: EvalPhase::EvalEnv,
                next: prev,
            });
            state.tail = tail;
        }
        None => {
            // Move straight to evaluating the expression
            state.control = Control::Expr(expr);
            state.kont = Rc::new(Kont::Eval {
                expr,
                env: None,
                phase: EvalPhase::EvalExpr,
                next: prev,
            });
            state.tail = tail;
        }
    }
}

/// Bind a symbol to a value. This is installed before evaluation of the right hand side.
/// The bind operation takes the value returned by the previous continuation.
/// Supports both define and set! semantics.
///
pub fn insert_bind(state: &mut CEKState, symbol: GcRef, env: Option<EnvRef>) {
    // clone the current continuation and link it under the new Bind
    let prev = Rc::clone(&state.kont);
    state.kont = Rc::new(Kont::Bind {
        symbol,
        env,
        next: prev,
    });
}

pub fn insert_cond(state: &mut CEKState, remaining: Vec<CondClause>) {
    let prev = Rc::clone(&state.kont);
    state.kont = Rc::new(Kont::Cond {
        remaining,
        next: prev,
    });
}

pub fn insert_if(state: &mut CEKState, then_branch: GcRef, else_branch: GcRef) {
    let prev = Rc::clone(&state.kont);
    state.kont = Rc::new(Kont::If {
        then_branch,
        else_branch,
        next: prev,
    });
}

pub fn insert_seq(state: &mut CEKState, mut exprs: Vec<GcRef>) {
    // exprs has length ≥ 2
    exprs.reverse();
    let prev = Rc::clone(&state.kont);
    state.kont = Rc::new(Kont::Seq {
        rest: exprs,
        next: prev,
    });
}

pub fn insert_and_or(state: &mut CEKState, kind: AndOrKind, mut exprs: Vec<GcRef>) {
    // exprs has length ≥ 2
    exprs.reverse();
    let prev = Rc::clone(&state.kont);
    state.control = Control::Expr(exprs.pop().unwrap());
    state.kont = Rc::new(Kont::AndOr {
        kind,
        rest: exprs,
        next: prev,
    });
}
