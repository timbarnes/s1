//! The tracer and stepper behind `trace` and `break`, and the prompt an
//! uncaught error opens after `(trace 'off)`.
//!
//! `step()` calls [`debugger`] before each step while `CEKState::hook` is
//! set, which [`set_mode`] keeps in line with `DebugState::mode`. With
//! tracing off none of this runs; that one flag test is the whole cost.
//!
//! The prompt reads commands from standard input and writes to standard
//! error, so its output stays apart from the program's. Stepping over (`o`)
//! and out (`f`) run silently to a [`StopAt`]. `p` evaluates an expression
//! with the machine itself, beneath a `Kont::DebugPrint` frame that puts the
//! paused state back afterwards, rather than with a nested `eval_main`,
//! whose parked state the GC could not see
//! (see design/nested-evaluation.md).

use crate::env::EnvRef;
use crate::eval::kont::DebugSaved;
use crate::eval::{CEKState, Control, DebugState, Kont, KontRef, RunTime, TraceMode};
use crate::gc::GcRef;
use crate::printer::{print_value, print_value_limited};
use crate::utilities::{dbg_cek, dbg_env, dbg_one_env, dbg_one_kont};
use rustc_hash::FxHashSet;
use std::io::{self, Write};
use std::rc::Rc;

/// The most characters of an expression or value one trace line shows.
const LINE_LIMIT: usize = 120;
/// Trace lines are indented one column per continuation frame, up to this;
/// deeper ones show the depth as a number instead.
const MAX_INDENT: usize = 40;
/// How far to count frames for the depth shown on a trace line.
const MAX_COUNT: usize = 1000;
/// How many frames `(trace 'all)` shows under each line.
const ALL_FRAMES: usize = 3;
/// How many frames `bt` lists before summarising the rest.
const BT_FRAMES: usize = 40;

/// Set the trace mode, and `state.hook` to match.
pub fn set_mode(state: &mut CEKState, debug: &mut DebugState, mode: TraceMode) {
    debug.mode = mode;
    debug.stop_at = None;
    state.hook = mode != TraceMode::Off;
}

/// Where a step over or out ends: when `target` receives a value, or when
/// the machine reaches a frame below it. The second covers an expression
/// whose value was consumed on a fast path without a step, and escapes.
pub struct StopAt {
    target: KontRef,
    /// The frames below `target`, by address. Holding `target` keeps them
    /// all alive, so no address can be reused while this exists.
    below: FxHashSet<*const Kont>,
}

impl StopAt {
    fn new(target: KontRef) -> Self {
        let mut below = FxHashSet::default();
        let mut k = target.next();
        while let Some(frame) = k {
            below.insert(Rc::as_ptr(frame));
            k = frame.next();
        }
        StopAt { target, below }
    }

    fn reached(&self, state: &CEKState) -> bool {
        let k = Rc::as_ptr(&state.kont);
        (k == Rc::as_ptr(&self.target) && matches!(state.control, Control::Value(_)))
            || self.below.contains(&k)
    }
}

/// Called by `step()` before each step while `state.hook` is set: print
/// the trace line, and in step mode prompt for a command.
#[cold]
#[inline(never)]
pub fn debugger(state: &mut CEKState, rt: &mut RunTime) {
    match rt.debug.mode {
        TraceMode::Off => {}
        TraceMode::Expr => trace_line(state),
        TraceMode::All => {
            trace_line(state);
            let (indent, _) = indentation(&state.kont);
            let mut k = Some(&state.kont);
            for _ in 0..ALL_FRAMES {
                let Some(frame) = k else { break };
                eprintln!("{indent}    | {}", frame_summary(frame));
                k = frame.next();
            }
        }
        TraceMode::Step => {
            if let Some(stop) = &rt.debug.stop_at {
                if !stop.reached(state) {
                    return;
                }
                rt.debug.stop_at = None;
            }
            trace_line(state);
            prompt(state, rt, Where::Step);
        }
    }
}

/// An uncaught error, with `break_on_error` set or tracing on: let the user
/// look around before the form is abandoned.
pub fn post_mortem(state: &mut CEKState, rt: &mut RunTime) {
    eprintln!("Entering the debugger; c, q or end of input abandons the form (h for help).");
    prompt(state, rt, Where::PostMortem);
}

/// One line for the current control, indented by the continuation depth.
fn trace_line(state: &CEKState) {
    // So the trace interleaves with the program's output in order.
    io::stdout().flush().ok();
    let (indent, depth) = indentation(&state.kont);
    let depth = match depth {
        Some(d) if d > MAX_INDENT => format!("[{d}] "),
        Some(_) => String::new(),
        None => format!("[{MAX_COUNT}+] "),
    };
    eprintln!("{indent}{depth}{}", describe_control(&state.control));
}

/// The indent for a trace line under `kont`, and the frame count (`None`
/// past `MAX_COUNT`).
fn indentation(kont: &KontRef) -> (String, Option<usize>) {
    let mut depth = 0;
    let mut k = kont.next();
    while let Some(frame) = k {
        depth += 1;
        if depth > MAX_COUNT {
            return (" ".repeat(MAX_INDENT), None);
        }
        k = frame.next();
    }
    (" ".repeat(depth.min(MAX_INDENT)), Some(depth))
}

fn describe_control(control: &Control) -> String {
    match control {
        Control::Expr(e) => format!("Expr:  {}", print_value_limited(e, LINE_LIMIT)),
        Control::Value(v) => format!("Value: {}", show_value(v)),
        // An uncaught error interrupted the step.
        Control::Empty => "(stopped by the error)".to_string(),
    }
}

/// A value for a trace line; the void value, which prints as nothing, is
/// shown as `#<void>`.
fn show_value(v: &GcRef) -> String {
    match print_value_limited(v, LINE_LIMIT) {
        s if s.is_empty() => "#<void>".to_string(),
        s => s,
    }
}

/// A one-line, user-level description of a frame, for `bt` and
/// `(trace 'all)`.
fn frame_summary(frame: &Kont) -> String {
    let p = |v: &GcRef| print_value_limited(v, LINE_LIMIT);
    match frame {
        Kont::EvalArg { original_call, .. } => format!("in call  {}", p(original_call)),
        Kont::ApplySpecial { original_call, .. } => format!("in form  {}", p(original_call)),
        Kont::RestoreEnv { .. } => "-- return from procedure --".to_string(),
        Kont::If { then_branch, else_branch, .. } => {
            format!("if test  then {} else {}", p(then_branch), p(else_branch))
        }
        Kont::Seq { rest, .. } => match rest.last() {
            Some(next) => format!("in body, {} more, next {}", rest.len(), p(next)),
            None => "in body".to_string(),
        },
        Kont::Bind { symbol, is_define, .. } => {
            format!("{} {}", if *is_define { "define" } else { "set!" }, p(symbol))
        }
        Kont::Halt => "top level".to_string(),
        other => dbg_one_kont("", other).trim().to_string(),
    }
}

/// The frames from the top of `state.kont` down, with frame 0 standing for
/// the current control (and `state.env`).
fn frames(state: &CEKState) -> Vec<KontRef> {
    let mut v = vec![Rc::clone(&state.kont)];
    let mut k = state.kont.next();
    while let Some(frame) = k {
        v.push(Rc::clone(frame));
        k = frame.next();
    }
    v
}

/// The environment frame `n` (as `bt` numbers them) resumes in. Frame 0 is
/// the current control, in `state.env`. A frame further down resumes in the
/// environment the nearest `RestoreEnv` above it puts back, or, for a call
/// still evaluating its arguments, its own.
fn env_of(state: &CEKState, frames: &[KontRef], n: usize) -> EnvRef {
    if n == 0 {
        return state.env.clone();
    }
    if let Kont::EvalArg { env, .. } = &*frames[n - 1] {
        return env.clone();
    }
    for frame in frames[..n - 1].iter().rev() {
        if let Kont::RestoreEnv { old_env, .. } = &**frame {
            return old_env.clone();
        }
    }
    state.env.clone()
}

/// The first frame from `kont` down that a procedure body returns to.
fn enclosing_return(kont: &KontRef) -> Option<KontRef> {
    let mut k = Some(kont);
    while let Some(frame) = k {
        if let Kont::RestoreEnv { .. } = **frame {
            return Some(Rc::clone(frame));
        }
        k = frame.next();
    }
    None
}

fn backtrace(state: &CEKState, frames: &[KontRef], selected: usize) {
    let mark = |i: usize| if i == selected { '*' } else { ' ' };
    eprintln!("{}  0  {}", mark(0), describe_control(&state.control));
    for (i, frame) in frames.iter().enumerate().take(BT_FRAMES) {
        eprintln!("{} {:2}  {}", mark(i + 1), i + 1, frame_summary(frame));
    }
    if frames.len() > BT_FRAMES {
        eprintln!("    ... {} more frames", frames.len() - BT_FRAMES);
    }
}

#[derive(Clone, Copy, PartialEq)]
enum Where {
    /// Stopped before a step: the machine can go on.
    Step,
    /// After an uncaught error: the form will be abandoned, so commands
    /// that run the machine are not available.
    PostMortem,
}

const HELP: &str = "  Enter, n(ext)   take one step               c(ontinue)  stop stepping, run on
  o(ver)          step over this expression   f(inish)    run until this procedure returns
  q(uit)          abandon the top-level form
  bt              backtrace (* is the selected frame)
  u(p) [n], d(own) [n], fr(ame) n             select a frame for l, e and p
  l(ocals)        the selected frame's innermost bindings
  e(nv)           all its local bindings      k(ont)      the raw continuation
  x, expr         the current expression      s(tate)     the machine state
  p(rint) expr    evaluate expr in the selected frame's environment
  h(elp)          this list";

/// Read and run commands until one moves the machine on (or, post mortem,
/// until the user leaves).
fn prompt(state: &mut CEKState, rt: &mut RunTime, at: Where) {
    let frames = frames(state);
    let mut selected = 0usize;
    loop {
        eprint!("debug> ");
        io::stderr().flush().ok();
        let mut input = String::new();
        if matches!(io::stdin().read_line(&mut input), Ok(0) | Err(_)) {
            // End of input: nobody is there to answer, so run on.
            eprintln!();
            if at == Where::Step {
                set_mode(state, rt.debug, TraceMode::Off);
            }
            return;
        }
        let line = input.trim();
        let (cmd, arg) = line.split_once(char::is_whitespace).unwrap_or((line, ""));
        let arg = arg.trim();
        let step = at == Where::Step;
        match cmd {
            "" | "n" | "next" if step => return,
            "o" | "over" if step => {
                if let Control::Expr(_) = state.control {
                    rt.debug.stop_at = Some(StopAt::new(Rc::clone(&state.kont)));
                }
                return;
            }
            "f" | "finish" if step => {
                match enclosing_return(&state.kont) {
                    Some(frame) => rt.debug.stop_at = Some(StopAt::new(frame)),
                    None => set_mode(state, rt.debug, TraceMode::Off),
                }
                return;
            }
            "c" | "continue" => {
                if step {
                    set_mode(state, rt.debug, TraceMode::Off);
                }
                return;
            }
            "q" | "quit" => {
                if step {
                    set_mode(state, rt.debug, TraceMode::Off);
                    let halt = Rc::clone(&state.halt);
                    crate::eval::exceptions::abandon_form(state, rt, halt);
                }
                return;
            }
            "p" | "print" if step => {
                if arg.is_empty() {
                    eprintln!("p: expected an expression");
                } else if start_print(state, rt, arg, env_of(state, &frames, selected)) {
                    return;
                }
            }
            "p" | "print" => eprintln!("p is not available after an error"),
            "bt" | "backtrace" | "where" => backtrace(state, &frames, selected),
            "u" | "up" | "d" | "down" | "fr" | "frame" => {
                let n: Option<usize> = if arg.is_empty() { Some(1) } else { arg.parse().ok() };
                let Some(n) = n else {
                    eprintln!("{cmd}: expected a number");
                    continue;
                };
                selected = match cmd {
                    "u" | "up" => selected + n,
                    "d" | "down" => selected.saturating_sub(n),
                    _ => n,
                }
                .min(frames.len());
                let summary = match selected {
                    0 => describe_control(&state.control),
                    i => frame_summary(&frames[i - 1]),
                };
                eprintln!("* {selected:2}  {summary}");
            }
            "l" | "locals" => dbg_one_env(&env_of(state, &frames, selected), 0),
            "e" | "env" => dbg_env("", env_of(state, &frames, selected), false),
            "k" | "kont" => crate::utilities::dbg_kont("", &state.kont),
            "x" | "expr" => eprintln!("{}", describe_control(&state.control)),
            "s" | "state" => dbg_cek("state", state),
            _ => {
                if !matches!(cmd, "h" | "help" | "?") {
                    let why = if step { "unknown command" } else { "not available after an error" };
                    eprintln!("{cmd}: {why}");
                }
                eprintln!("{HELP}");
            }
        }
    }
}

/// Start evaluating `src` in `env` for `p`: park the machine in a
/// `Kont::DebugPrint` frame and make the expression the control. Tracing is
/// off while it runs. False, after reporting why, if `src` doesn't parse.
fn start_print(state: &mut CEKState, rt: &mut RunTime, src: &str, env: EnvRef) -> bool {
    let mut port = crate::io::new_string_port_input(src);
    let expr = match crate::parser::parse(rt.heap, &mut port) {
        Ok(expr) => expr,
        Err(crate::parser::ParseError::Syntax(e)) => {
            eprintln!("p: {e}");
            return false;
        }
        Err(crate::parser::ParseError::Eof) => {
            eprintln!("p: incomplete expression");
            return false;
        }
    };
    let (resume, resume_value) = match state.control {
        Control::Expr(e) => (e, false),
        Control::Value(v) => (v, true),
        Control::Empty => return false,
    };
    let saved = DebugSaved {
        resume,
        resume_value,
        env: std::mem::replace(&mut state.env, env),
        tail: state.tail,
        handlers: *rt.handlers,
        arg_top: rt.arg_stack.len(),
        dw_len: rt.dynamic_wind.len(),
    };
    state.kont = Rc::new(Kont::DebugPrint {
        saved: Box::new(saved),
        next: Rc::clone(&state.kont),
    });
    state.control = Control::Expr(expr);
    state.tail = false;
    *rt.handlers = rt.heap.nil_s();
    set_mode(state, rt.debug, TraceMode::Off);
    true
}

/// `Kont::DebugPrint`: `p`'s expression returned `val`. Print it and put the
/// machine back at the prompt.
pub fn handle_debug_print(
    state: &mut CEKState,
    rt: &mut RunTime,
    val: GcRef,
    saved: DebugSaved,
    next: KontRef,
) {
    io::stdout().flush().ok();
    match print_value(&val) {
        s if s.is_empty() => eprintln!("#<void>"),
        s => eprintln!("{s}"),
    }
    resume(state, rt, saved, next);
}

/// If an uncaught error was raised under a `p` expression, report it and go
/// back to the prompt instead of abandoning the form being debugged. Any
/// dynamic-wind extents the expression entered are dropped without running
/// their `after` thunks.
pub fn error_in_print(state: &mut CEKState, rt: &mut RunTime, message: &str) -> bool {
    let mut k = Some(&state.kont);
    while let Some(frame) = k {
        if let Kont::DebugPrint { saved, next } = &**frame {
            let (saved, next) = ((**saved).clone(), Rc::clone(next));
            eprintln!("p: Error: {message}");
            resume(state, rt, saved, next);
            return true;
        }
        k = frame.next();
    }
    false
}

/// Put the machine back as `start_print` found it, stepping.
fn resume(state: &mut CEKState, rt: &mut RunTime, saved: DebugSaved, next: KontRef) {
    state.control = if saved.resume_value {
        Control::Value(saved.resume)
    } else {
        Control::Expr(saved.resume)
    };
    state.env = saved.env;
    state.tail = saved.tail;
    state.kont = next;
    *rt.handlers = saved.handlers;
    rt.arg_stack.truncate(saved.arg_top);
    rt.dynamic_wind.truncate(saved.dw_len);
    set_mode(state, rt.debug, TraceMode::Step);
}
