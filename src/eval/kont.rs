//! The CEK machine's state and continuation frames.
//!
//! The machine's state is a [`CEKState`]: the control (an expression to
//! evaluate or a value to return), the environment, and the continuation,
//! a linked chain of [`Kont`] frames saying what to do with the next value.
//! Frames are reference counted, so capturing a continuation (`call/cc`)
//! just shares the chain.
//!
//! Special forms do not recurse into the evaluator. Instead they push a frame
//! and set the control, using the `insert_*` helpers here, and `eval::cek`
//! steps the machine. The frames of forms with subexpressions record
//! whether the form is in tail position (R7RS 3.5), so that its last
//! subexpression is evaluated in tail position too, without a frame of its own.
//!
//! See design/kont-flat-stack-design.md for the shelved alternative of a
//! flat frame stack.

use crate::env::EnvRef;
use crate::eval::DynamicWind;
use crate::gc::GcRef;
use crate::printer::print_value;
use rustc_hash::FxHashSet;
use std::cell::RefCell;
use std::rc::Rc;
use std::time::Instant;

/// A shared reference to a continuation frame, and so to the whole chain
/// below it.
pub type KontRef = Rc<Kont>;

/// A continuation frame: what to do with the value the machine returns
/// next. Every frame except `Halt` and `Exit` links to the frame below it.
#[derive(Clone, PartialEq)]
pub enum Kont {
    /// The bottom of the chain: the value is the result of the top-level
    /// form.
    Halt,
    /// `exit` was called: once the `after` thunks of the dynamic-wind
    /// extents being left have run, end the process with status `code`.
    Exit {
        /// The process exit status.
        code: i32,
    },
    /// In an `and` or `or`: test the value, then stop or go on to the next
    /// expression.
    AndOr {
        /// Which of the two forms.
        kind: AndOrKind,
        /// The remaining expressions, last first, so `pop` yields the next.
        rest: Vec<GcRef>,
        /// Whether the and/or form is in tail position: its last expression
        /// then is too (R7RS 3.5).
        tail: bool,
        next: KontRef,
    },
    /// A procedure call with all its arguments evaluated. `apply_proc` runs
    /// this frame directly, from `state.kont`; it never reaches
    /// `dispatch_kont`.
    ApplyProc {
        /// The procedure.
        proc: GcRef,
        /// The arguments.
        evaluated_args: Rc<Vec<GcRef>>,
        next: KontRef,
    },
    /// A special form or macro use, applied to its unevaluated form.
    ApplySpecial {
        /// A `Callable::SpecialForm`, `Macro` or `SyntaxRules`.
        proc: GcRef,
        /// The whole form: `(op . args)`.
        original_call: GcRef,
        next: KontRef,
    },
    /// After the right-hand side of a `define` or `set!`: bind the value.
    Bind {
        /// The variable being bound.
        symbol: GcRef,
        /// The frame to bind into, captured where the define/set! was written.
        /// Must NOT be re-derived from state.env when the frame runs: a
        /// non-local exit (call/cc) can arrive here with an unrelated env.
        env: EnvRef,
        /// `define` returns the symbol; `set!` returns unspecified.
        is_define: bool,
        next: KontRef,
    },
    /// After `call-with-values`' producer: apply the consumer to its values.
    CallWithValues {
        /// The procedure that receives the values.
        consumer: GcRef,
        next: KontRef,
    },
    /// A `cond` with clauses still to try.
    Cond {
        /// The untried clauses, last first.
        remaining: Vec<CondClause>,
        /// Whether the cond form is in tail position: a clause's body (or
        /// `=>` call) is then too.
        tail: bool,
        next: KontRef,
    },
    /// After a `cond` clause's test: run the clause if the test is true,
    /// otherwise return to the `Cond` frame below (always the `next`).
    CondClause {
        /// The clause whose test was evaluated.
        clause: CondClause,
        /// The enclosing `Cond` frame.
        next: KontRef,
    },
    /// A `dynamic-wind` in progress; `phase` says which step comes next.
    DynamicWind {
        /// The three thunks and the body's result.
        procs: Box<DynamicWindProcs>,
        /// The step to take when the current thunk returns.
        phase: DynamicWindPhase,
        /// The continuation of the whole `dynamic-wind`.
        next: KontRef,
    },
    /// Invoking a continuation: run the pending `after`/`before` thunks one
    /// at a time, then install the target continuation and deliver the value.
    Escape {
        /// The value to deliver, the thunks left to run, and the state to
        /// reinstate.
        payload: Box<EscapePayload>,
        /// The continuation being invoked.
        new_kont: KontRef,
    },
    /// Runs a macro body (already installed as `state.control`/`state.env`)
    /// and, once it yields the expansion, restores the call site's
    /// environment and either evaluates the expansion (`mode: Evaluate`) or
    /// returns it as a value (`mode: Expand`, for the `expand` debugging aid).
    MacroExpand {
        /// The environment of the macro use.
        call_env: EnvRef,
        /// Evaluate the expansion, or return it.
        mode: MacroMode,
        next: KontRef,
    },
    /// Evaluates `(expand form)`'s argument, then, if the result is a
    /// macro call, pushes `MacroExpand { mode: Expand }` to expand it one
    /// level.
    ExpandArg {
        /// The environment `expand` was called in.
        env: EnvRef,
        next: KontRef,
    },
    /// Drives a sequence of top-level forms (from `eval-string`), collecting
    /// each result.
    EvalSeq {
        /// The not-yet-evaluated forms (tail first) and the values collected
        /// so far.
        forms: Box<EvalSeqForms>,
        next: KontRef,
    },
    /// Times the evaluation of the wrapped body (`with-timer`).
    Timer {
        /// When the body started.
        start: Instant,
        next: KontRef,
    },
    /// Below a `with-exception-handler` thunk: when the thunk returns,
    /// reinstate the handler list that was current before the handler was
    /// installed.
    RestoreHandlers {
        /// The handler list to reinstate.
        handlers: GcRef,
        next: KontRef,
    },
    /// Below a call to an exception handler. If the handler returns from a
    /// `raise-continuable`, reinstate `saved` (the handler list current at
    /// the raise) and deliver the value to `next`. Returning from a plain
    /// `raise` is itself an error (R7RS 6.11).
    RaiseReturn {
        /// The raised object.
        payload: GcRef,
        /// The handler list current at the raise.
        saved: GcRef,
        /// Whether it was `raise-continuable`.
        continuable: bool,
        next: KontRef,
    },
    /// Evaluating the operator and arguments of a procedure call, left to
    /// right; the value is the operator or the latest argument.
    EvalArg {
        /// Whether the operator has been evaluated yet. Once it has, it sits
        /// at `arg_stack[args_base]` (rooted there like the arguments),
        /// rather than in an `Option<GcRef>` field here: raw pointers have
        /// no niche, so that Option cost 16 bytes on the hottest frame.
        have_proc: bool,
        /// The not-yet-evaluated argument expressions, as the tail of the
        /// original call's cons-list (`Nil` once all are evaluated). Walking
        /// this directly instead of copying it into a `Vec` up front avoids
        /// an allocation per application; the GC marker follows it for free
        /// since it's ordinary list structure.
        remaining_exprs: GcRef,
        /// Index into `RunTime::arg_stack` where this call's evaluated
        /// operator and then arguments begin; they run from `args_base` to
        /// the stack's current top. The stack is shared and reused across all in-flight calls
        /// (nested calls simply extend it further and truncate back on
        /// return), which turns per-call argument accumulation into pushes
        /// onto one long-lived buffer instead of a fresh `Vec` each time.
        args_base: u32,
        /// The whole call form, for special-form dispatch (if the operator
        /// turns out to be syntax) and for error messages.
        original_call: GcRef,
        /// Whether the call is in tail position.
        tail: bool,
        /// The call's environment, restored before each argument: a tail
        /// call in the previous argument leaves `state.env` in its callee.
        env: EnvRef,
        next: KontRef,
    },
    /// After an `if` test: evaluate the chosen branch.
    If {
        /// The consequent.
        then_branch: GcRef,
        /// The alternate (unspecified if the `if` has none).
        else_branch: GcRef,
        /// Whether the if form is in tail position; its branches inherit it.
        tail: bool,
        next: KontRef,
    },
    /// Below a non-tail closure call: when the body returns, go back to the
    /// caller's environment. This is one of the two points where the
    /// evaluator collects garbage.
    RestoreEnv {
        /// The caller's environment.
        old_env: EnvRef,
        next: KontRef,
    },
    /// In a body or `begin`: discard the value and evaluate the next form.
    Seq {
        /// The remaining forms, last first, so `pop` yields the next.
        rest: Vec<GcRef>,
        /// Whether the sequence is in tail position; its last form inherits it.
        tail: bool,
        next: KontRef,
    },
    /// Below an expression the debugger's `p` command is evaluating: print
    /// the value, then put the machine back as it was at the prompt (see
    /// `debugger::handle_debug_print`). Also where an error in that
    /// expression lands, instead of abandoning the form being debugged.
    DebugPrint {
        saved: Box<DebugSaved>,
        next: KontRef,
    },
    // Exception handler frame - used later for raise/handler support
    // Handler {
    //     handler_expr: GcRef,
    //     handler_env: EnvRef,
    //     next: KontRef,
    // },
}

impl Kont {
    /// The frame below this one; `None` for `Halt` and `Exit`.
    pub fn next(&self) -> Option<&KontRef> {
        match self {
            Kont::AndOr { next, .. } => Some(next),
            Kont::ApplyProc { next, .. } => Some(next),
            Kont::ApplySpecial { next, .. } => Some(next),
            Kont::Bind { next, .. } => Some(next),
            Kont::CallWithValues { next, .. } => Some(next),
            Kont::Cond { next, .. } => Some(next),
            Kont::CondClause { next, .. } => Some(next),
            Kont::DynamicWind { next, .. } => Some(next),
            Kont::Escape { new_kont, .. } => Some(new_kont),
            Kont::EvalArg { next, .. } => Some(next),
            Kont::Halt => None,
            Kont::Exit { .. } => None,
            Kont::If { next, .. } => Some(next),
            Kont::MacroExpand { next, .. } => Some(next),
            Kont::ExpandArg { next, .. } => Some(next),
            Kont::EvalSeq { next, .. } => Some(next),
            Kont::Timer { next, .. } => Some(next),
            Kont::RestoreHandlers { next, .. } => Some(next),
            Kont::RaiseReturn { next, .. } => Some(next),
            Kont::RestoreEnv { next, .. } => Some(next),
            Kont::Seq { next, .. } => Some(next),
            Kont::DebugPrint { next, .. } => Some(next),
        }
    }
}

/// What `Kont::MacroExpand` does with an expansion.
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum MacroMode {
    /// Evaluate it: an ordinary macro use.
    Evaluate,
    /// Return it as a value, for `expand`.
    Expand,
}

impl std::fmt::Debug for Kont {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Kont::Halt => write!(f, "Halt"),
            Kont::RestoreEnv { old_env: _, next } => {
                write!(f, "RestoreEnv: next: {:?}", next)
            }
            Kont::EvalArg {
                have_proc,
                remaining_exprs,
                args_base,
                original_call,
                next,
                ..
            } => {
                write!(
                    f,
                    "EvalArg {{ have_proc: {}, remaining_exprs: {}, args_base: {}, original_call: {:?}, next: {:?} }}",
                    have_proc,
                    print_value(remaining_exprs),
                    args_base,
                    original_call,
                    next
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
            Kont::Bind { symbol, next, .. } => {
                write!(
                    f,
                    "Bind {{ symbol: {}, next: {:?} }}",
                    print_value(symbol),
                    next
                )
            }
            Kont::CallWithValues { consumer, next } => {
                write!(
                    f,
                    "CallWithValues {{ consumer: {}, next: {:?} }}",
                    print_value(consumer),
                    next
                )
            }
            Kont::DynamicWind { procs, phase, next } => {
                let DynamicWindProcs {
                    before,
                    thunk,
                    after,
                    thunk_result,
                } = &**procs;
                write!(
                    f,
                    "DynamicWind {{ before: {}, thunk: {}, after: {}, thunk_result: {:?}, phase: {:?}, next: {:?} }}",
                    print_value(before),
                    print_value(thunk),
                    print_value(after),
                    thunk_result,
                    phase,
                    next
                )
            }
            Kont::If {
                then_branch,
                else_branch,
                next, ..
            } => {
                write!(
                    f,
                    "AfterTest {{ then: {}, else_: {}, next: {:?} }}",
                    print_value(then_branch),
                    print_value(else_branch),
                    next
                )
            }
            Kont::Cond { remaining, next, .. } => {
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
            Kont::Seq { rest, next, .. } => {
                write!(f, "Seq {{ rest: {:?}, next: {:?} }}", rest, next)
            }
            Kont::AndOr { kind, rest, next, .. } => {
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
            Kont::Escape { payload, new_kont } => {
                write!(
                    f,
                    "Escape {{ thunks: {}, new_kont: {:?} }}",
                    payload.thunks.len(),
                    new_kont
                )
            }
            Kont::MacroExpand {
                call_env: _,
                mode,
                next,
            } => {
                write!(f, "MacroExpand {{ mode: {:?}, next: {:?} }}", mode, next)
            }
            Kont::ExpandArg { env: _, next } => {
                write!(f, "ExpandArg {{ next: {:?} }}", next)
            }
            Kont::EvalSeq { forms, next } => {
                write!(
                    f,
                    "EvalSeq {{ remaining: {}, results: {}, next: {:?} }}",
                    forms.remaining.len(),
                    forms.results.len(),
                    next
                )
            }
            Kont::Timer { next, .. } => {
                write!(f, "Timer {{ next: {:?} }}", next)
            }
            Kont::Exit { code } => write!(f, "Exit {{ code: {} }}", code),
            Kont::DebugPrint { next, .. } => write!(f, "DebugPrint {{ next: {:?} }}", next),
            Kont::RestoreHandlers { next, .. } => {
                write!(f, "RestoreHandlers {{ next: {:?} }}", next)
            }
            Kont::RaiseReturn {
                payload,
                continuable,
                next,
                ..
            } => write!(
                f,
                "RaiseReturn {{ payload: {}, continuable: {}, next: {:?} }}",
                print_value(payload),
                continuable,
                next
            ),
        }
    }
}

/// A parsed `cond` clause.
#[derive(Clone, PartialEq)]
pub enum CondClause {
    /// `(test body ...)` or `(test)`; `else` clauses are this with a true test.
    Normal {
        /// The test expression.
        test: GcRef,
        /// The body as one expression, or `None` to return the test's value.
        body: Option<GcRef>,
    },
    /// `(test => receiver)`.
    Arrow {
        /// The test expression.
        test: GcRef,
        /// The expression for the procedure that receives the test's value.
        arrow_proc: GcRef,
    },
}

/// Which short-circuiting form a `Kont::AndOr` frame is running.
#[derive(Clone, Copy, PartialEq)]
pub enum AndOrKind {
    /// Stop at the first false value.
    And,
    /// Stop at the first true value.
    Or,
}

/// The step a `Kont::DynamicWind` frame takes when the current thunk
/// returns.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum DynamicWindPhase {
    /// `before` has returned: enter the extent and call the body thunk.
    Thunk,
    /// The body has returned: save its value, leave the extent, call `after`.
    After,
    /// `after` has returned: return the body's value.
    Return,
}

// Cold, wide payloads are boxed so every `Rc<Kont>` allocation — the
// evaluator's most frequent — stays within 56 bytes (see the assertion
// below). Each variant keeps its `next` link inline, so generic walkers
// like `Kont::next()` don't have to look inside the box.

/// The boxed payload of `Kont::DynamicWind`.
#[derive(Clone, PartialEq)]
pub struct DynamicWindProcs {
    /// The call `(before)`, made on entering the extent.
    pub before: GcRef,
    /// The call `(thunk)`: the body.
    pub thunk: GcRef,
    /// The call `(after)`, made on leaving the extent.
    pub after: GcRef,
    /// The body's value, held while `after` runs.
    pub thunk_result: Option<GcRef>,
}

/// The boxed payload of `Kont::Escape`.
#[derive(Clone, PartialEq)]
pub struct EscapePayload {
    /// The value to deliver to the continuation.
    pub result: GcRef,
    /// The `after` and `before` thunks still to run, the next one last.
    pub thunks: Vec<GcRef>,
    /// The continuation's dynamic-wind stack, installed once the thunks
    /// have run.
    pub new_dw_stack: Vec<DynamicWind>,
    /// The continuation's `arg_stack` snapshot (see `ContinuationData`), or
    /// None to keep the stack as it is (an escape-only continuation, which
    /// truncated it already).
    pub new_arg_stack: Option<Vec<GcRef>>,
    /// The continuation's exception handler list.
    pub new_handlers: GcRef,
}

/// The machine as the debugger prompt left it, while `p` evaluates an
/// expression (`Kont::DebugPrint`).
#[derive(Clone, PartialEq)]
pub struct DebugSaved {
    /// The control to resume: `resume` as an `Expr`, or as a `Value` if
    /// `resume_value`.
    pub resume: GcRef,
    pub resume_value: bool,
    pub env: EnvRef,
    pub tail: bool,
    /// The exception handlers, removed so an error in the expression comes
    /// to the debugger rather than to the program's handlers.
    pub handlers: GcRef,
    /// The heights of `arg_stack` and `dynamic_wind`, to cut them back to
    /// after an error.
    pub arg_top: usize,
    pub dw_len: usize,
}

/// `eval-string`'s pending forms (tail first) and results collected so far.
#[derive(Clone, PartialEq)]
pub struct EvalSeqForms {
    /// The forms not yet evaluated, last first.
    pub remaining: Vec<GcRef>,
    /// The values so far.
    pub results: Vec<GcRef>,
}

const _: () = assert!(std::mem::size_of::<Kont>() <= 40);

/// The C of the CEK machine: what the next step works on.
pub enum Control {
    /// An expression to evaluate in `CEKState::env`.
    Expr(GcRef),
    /// A value to hand to the top continuation frame.
    Value(GcRef),
    /// Nothing: a transient state while a step is in progress.
    Empty,
}

/// The state of the CEK machine.
pub struct CEKState {
    /// The expression being evaluated, or the value being returned.
    pub control: Control,
    /// The current environment.
    pub env: EnvRef,
    /// The continuation.
    pub kont: KontRef,
    /// Whether `control`'s expression is in tail position. A closure call in
    /// tail position pushes no `RestoreEnv` frame, so tail calls run in
    /// constant space.
    pub tail: bool,
    /// A shared `Kont::Halt` used as a placeholder when `dispatch_kont` takes
    /// ownership of the top frame. Cloning this is a refcount bump; allocating
    /// a fresh `Rc::new(Kont::Halt)` each time would undo the saving.
    pub halt: KontRef,
    /// Whether `step()` calls the tracer/stepper (`utilities::debugger`)
    /// before each step. Kept here, beside the fields every step touches,
    /// rather than read through `RunTime::debug`, so that with tracing off
    /// the per-step cost is one test of a byte already in cache. Set with
    /// `DebugState::mode` (`utilities::set_trace_mode`).
    pub hook: bool,
}

impl CEKState {
    /// A machine with nothing to do, in `env`, with a `Halt` continuation.
    pub fn new(env: EnvRef) -> Self {
        let halt = KontRef::new(Kont::Halt);
        CEKState {
            control: Control::Empty,
            env,
            kont: Rc::clone(&halt),
            tail: false,
            halt,
            hook: false,
        }
    }
}

impl crate::gc::Mark for Control {
    fn mark(&self, visit: &mut dyn FnMut(GcRef)) {
        match self {
            Control::Expr(expr) => visit(*expr),
            Control::Value(val) => visit(*val),
            Control::Empty => {}
        }
    }
}

thread_local! {
    /// The shared continuation frames already marked in the current GC
    /// cycle: the epoch (`crate::gc::GC_EPOCH`) they belong to, and their
    /// addresses. Continuations share their frame chains, so without this
    /// every captured continuation re-marked its whole chain: n nested
    /// `guard`s, each holding a continuation, cost O(n^2) per collection.
    static KONT_SEEN: RefCell<(u64, FxHashSet<usize>)> = RefCell::new((0, FxHashSet::default()));
}

impl crate::gc::Mark for KontRef {
    fn mark(&self, visit: &mut dyn FnMut(GcRef)) {
        let epoch = crate::gc::GC_EPOCH.load(std::sync::atomic::Ordering::Relaxed);
        KONT_SEEN.with(|seen| {
            let mut seen = seen.borrow_mut();
            if seen.0 != epoch {
                seen.0 = epoch;
                seen.1.clear();
            }
        });
        mark_kont_chain(self, visit);
    }
}

/// Whether `frame` was already marked this cycle; records it if not. Only a
/// frame with more than one owner can be reached twice: `strong_count` is
/// its owners plus the worklist's own clone. Unshared frames, such as most
/// of a deep recursion's chain, skip the set and cost nothing extra.
fn already_marked(frame: &KontRef) -> bool {
    Rc::strong_count(frame) > 2
        && KONT_SEEN.with(|seen| !seen.borrow_mut().1.insert(Rc::as_ptr(frame) as usize))
}

/// Mark the frames from `start` down, stopping at any shared frame already
/// marked this cycle: everything below it was (or is being) marked from
/// there. Marking can re-enter this, through `visit` reaching a
/// continuation object, so the set is only borrowed briefly.
fn mark_kont_chain(start: &KontRef, visit: &mut dyn FnMut(GcRef)) {
    use crate::gc::Mark;
    {
        let mut worklist = vec![Rc::clone(start)];

        while let Some(kont_ref) = worklist.pop() {
            if already_marked(&kont_ref) {
                continue;
            }
            let kont = &*kont_ref;
            match kont {
                Kont::Halt => {}
                Kont::AndOr { rest, next, .. } => {
                    for item in rest {
                        visit(*item);
                    }
                    worklist.push(Rc::clone(next));
                }
                Kont::ApplyProc {
                    proc,
                    evaluated_args,
                    next,
                } => {
                    visit(*proc);
                    for arg in evaluated_args.iter() {
                        visit(*arg);
                    }
                    worklist.push(Rc::clone(next));
                }
                Kont::ApplySpecial {
                    proc,
                    original_call,
                    next,
                } => {
                    visit(*proc);
                    visit(*original_call);
                    worklist.push(Rc::clone(next));
                }
                Kont::Bind {
                    symbol, env, next, ..
                } => {
                    visit(*symbol);
                    env.mark(visit);
                    worklist.push(Rc::clone(next));
                }
                Kont::CallWithValues { consumer, next } => {
                    visit(*consumer);
                    worklist.push(Rc::clone(next));
                }
                Kont::Cond { remaining, next, .. } => {
                    for clause in remaining {
                        clause.mark(visit);
                    }
                    worklist.push(Rc::clone(next));
                }
                Kont::CondClause { clause, next } => {
                    clause.mark(visit);
                    worklist.push(Rc::clone(next));
                }
                Kont::DynamicWind { procs, next, .. } => {
                    let DynamicWindProcs {
                        before,
                        thunk,
                        after,
                        thunk_result,
                    } = &**procs;
                    visit(*before);
                    visit(*thunk);
                    visit(*after);
                    if let Some(thunk_result) = thunk_result {
                        visit(*thunk_result);
                    }
                    worklist.push(Rc::clone(next));
                }
                Kont::Escape { payload, new_kont } => {
                    let EscapePayload {
                        result,
                        thunks,
                        new_dw_stack,
                        new_arg_stack,
                        new_handlers,
                    } = &**payload;
                    visit(*result);
                    visit(*new_handlers);
                    for arg in new_arg_stack.iter().flatten() {
                        visit(*arg);
                    }
                    for thunk in thunks {
                        visit(*thunk);
                    }
                    worklist.push(Rc::clone(new_kont));
                    for dw in new_dw_stack {
                        match dw {
                            DynamicWind { before, after, .. } => {
                                visit(*before);
                                visit(*after);
                            }
                        }
                    }
                }
                Kont::EvalArg {
                    remaining_exprs,
                    original_call,
                    env,
                    next,
                    ..
                } => {
                    // Transitively marks the whole not-yet-evaluated tail:
                    // `visit` (== `mark_reachable`) already knows how to walk
                    // Pair structure.
                    visit(*remaining_exprs);
                    // The already-evaluated operator and arguments for this and every
                    // other in-flight call live in `RunTime::arg_stack`,
                    // rooted directly in `GcHeap::mark_from` — not here.
                    visit(*original_call);
                    env.mark(visit);
                    worklist.push(Rc::clone(next));
                }
                Kont::If {
                    then_branch,
                    else_branch,
                    next, ..
                } => {
                    visit(*then_branch);
                    visit(*else_branch);
                    worklist.push(Rc::clone(next));
                }
                Kont::RestoreEnv { old_env, next } => {
                    old_env.mark(visit);
                    worklist.push(Rc::clone(next));
                }
                Kont::Seq { rest, next, .. } => {
                    for item in rest {
                        visit(*item);
                    }
                    worklist.push(Rc::clone(next));
                }
                Kont::MacroExpand {
                    call_env, next, ..
                } => {
                    call_env.mark(visit);
                    worklist.push(Rc::clone(next));
                }
                Kont::ExpandArg { env, next } => {
                    env.mark(visit);
                    worklist.push(Rc::clone(next));
                }
                Kont::EvalSeq { forms, next } => {
                    for item in &forms.remaining {
                        visit(*item);
                    }
                    for item in &forms.results {
                        visit(*item);
                    }
                    worklist.push(Rc::clone(next));
                }
                Kont::Timer { next, .. } => {
                    worklist.push(Rc::clone(next));
                }
                Kont::Exit { .. } => {}
                Kont::RestoreHandlers { handlers, next } => {
                    visit(*handlers);
                    worklist.push(Rc::clone(next));
                }
                Kont::RaiseReturn {
                    payload,
                    saved,
                    next,
                    ..
                } => {
                    visit(*payload);
                    visit(*saved);
                    worklist.push(Rc::clone(next));
                }
                Kont::DebugPrint { saved, next } => {
                    visit(saved.resume);
                    saved.env.mark(visit);
                    visit(saved.handlers);
                    worklist.push(Rc::clone(next));
                }
            }
        }
    }
}

impl crate::gc::Mark for CondClause {
    fn mark(&self, visit: &mut dyn FnMut(GcRef)) {
        match self {
            CondClause::Normal { test, body } => {
                visit(*test);
                if let Some(body) = body {
                    visit(*body);
                }
            }
            CondClause::Arrow { test, arrow_proc } => {
                visit(*test);
                visit(*arrow_proc);
            }
        }
    }
}

impl crate::gc::Mark for CEKState {
    fn mark(&self, visit: &mut dyn FnMut(GcRef)) {
        self.control.mark(visit);
        self.env.mark(visit);
        self.kont.mark(visit);
    }
}

///Insert a continuation for an (and...) or (or ...) expression
pub fn insert_and_or(state: &mut CEKState, kind: AndOrKind, mut exprs: Vec<GcRef>) {
    // exprs has length ≥ 2
    exprs.reverse();
    let prev = Rc::clone(&state.kont);
    let tail = state.tail;
    state.control = Control::Expr(exprs.pop().unwrap());
    // The AndOr frame evaluates more operands after this one, so this one
    // must not be treated as a tail call (it would leave the callee's env).
    state.tail = false;
    state.kont = Rc::new(Kont::AndOr {
        kind,
        rest: exprs,
        tail,
        next: prev,
    });
}

// Bind a symbol to a value. This is installed before evaluation of the right hand side.
// Insert a frame to run a function.
//
// pub fn insert_apply_proc(state: &mut CEKState, proc: GcRef, args: Vec<GcRef>) {
//     // clone the current continuation and link it under the new Bind
//     let evaluated_args = Rc::new(args);
//     let prev = Rc::clone(&state.kont);
//     state.kont = Rc::new(Kont::ApplyProc {
//         proc,
//         evaluated_args,
//         next: prev,
//     });
// }

/// Make `expr` the next expression to evaluate, under the current
/// continuation. `replace_next` is whether it is in tail position, which
/// sets `state.tail`.
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
/// Supports both define and set! semantics.
///
pub fn insert_bind(state: &mut CEKState, symbol: GcRef, env: EnvRef, is_define: bool) {
    // clone the current continuation and link it under the new Bind
    let prev = Rc::clone(&state.kont);
    state.kont = Rc::new(Kont::Bind {
        symbol,
        env,
        is_define,
        next: prev,
    });
}

/// Insert a continuation for a (cond ...) expression
pub fn insert_cond(state: &mut CEKState, remaining: Vec<CondClause>) {
    let prev = Rc::clone(&state.kont);
    state.kont = Rc::new(Kont::Cond {
        remaining,
        tail: state.tail,
        next: prev,
    });
}

/// Push a `dynamic-wind` frame whose `before` call is about to be
/// evaluated: when it returns, the frame enters the extent and calls `thunk`.
pub fn insert_dynamic_wind(state: &mut CEKState, before: GcRef, thunk: GcRef, after: GcRef) {
    let prev = Rc::clone(&state.kont);
    state.kont = Rc::new(Kont::DynamicWind {
        procs: Box::new(DynamicWindProcs {
            before,
            thunk,
            after,
            thunk_result: None,
        }),
        phase: DynamicWindPhase::Thunk,
        next: prev,
    });
}

/// Insert a continuation for a (if ...) expression
pub fn insert_if(state: &mut CEKState, then_branch: GcRef, else_branch: GcRef) {
    let prev = Rc::clone(&state.kont);
    state.kont = Rc::new(Kont::If {
        then_branch,
        else_branch,
        tail: state.tail,
        next: prev,
    });
}

/// Insert a continuation for a (begin ...) expression
pub fn insert_seq(state: &mut CEKState, mut exprs: Vec<GcRef>) {
    // exprs has length ≥ 2
    exprs.reverse();
    let prev = Rc::clone(&state.kont);
    state.kont = Rc::new(Kont::Seq {
        rest: exprs,
        tail: state.tail,
        next: prev,
    });
}

/// Push a frame that invokes the continuation `new_kont` with `result`,
/// once the dynamic-wind `thunks` have run.
pub fn insert_escape(
    state: &mut CEKState,
    result: GcRef,
    thunks: Vec<GcRef>,
    new_kont: KontRef,
    new_dw_stack: Vec<DynamicWind>,
    new_arg_stack: Option<Vec<GcRef>>,
    new_handlers: GcRef,
) {
    state.kont = Rc::new(Kont::Escape {
        payload: Box::new(EscapePayload {
            result,
            thunks,
            new_dw_stack,
            new_arg_stack,
            new_handlers,
        }),
        new_kont,
    });
}
