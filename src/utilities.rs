//! Internal utilities: error reporting from Rust code (`post_error`), and
//! debug dumps of the machine state, continuations and environments (used
//! by the debugger, `debug-stack` and `trace-env`). Dumps go to standard
//! error, apart from the program's output.

use crate::env::{EnvOps, EnvRef};
use crate::eval::{AndOrKind, CEKState, Control, Kont, KontRef};
use crate::eval::RunTime;
use crate::gc_value;
use crate::printer::print_value_limited;

/// The most characters of one value a dump shows.
const LIMIT: usize = 120;

fn print_value(obj: &crate::gc::GcRef) -> String {
    print_value_limited(obj, LIMIT)
}
use std::rc::Rc;

/// Report a failure (of a built-in procedure, special form or the machine
/// itself) by raising an error object with `error` as its message. A handler
/// may catch it; if none does, it is printed and the top-level form is
/// abandoned (see `eval::exceptions`).
pub fn post_error(state: &mut CEKState, ec: &mut RunTime, error: &str) {
    let nil = ec.heap.nil_s();
    crate::eval::exceptions::raise_error(state, ec, crate::gc::ErrorKind::General, error, nil);
}

/// Dump a summary of the CEK machine state.
pub fn dbg_cek(loc: &str, state: &CEKState) {
    std::io::Write::flush(&mut std::io::stdout()).ok();
    eprintln!("{}: ", loc);

    match &state.control {
        Control::Expr(obj) => {
            eprintln!(
                "Expr   {};      Kont = {}; Tail={}",
                print_value(obj),
                dbg_one_kont("", &state.kont),
                state.tail
            );
        }
        Control::Value(obj) => {
            eprintln!(
                "Value  {};      Kont = {}",
                print_value(obj),
                dbg_one_kont("", &state.kont)
            );
        }
        Control::Empty => {
            eprintln!("Halt      Kont = {}", dbg_one_kont("", &state.kont));
        }
    }
}

/// A one-line description of a continuation frame.
pub fn dbg_one_kont(loc: &str, frame: &Kont) -> String {
    let mut result = format!("{} ", loc);
    match frame {
        Kont::Halt => result.push_str("Halt"),
        Kont::AndOr { .. } => result.push_str("AndOr"),
        Kont::CallWithValues { .. } => result.push_str("CallWithValues"),
        Kont::Cond { .. } => result.push_str("Cond"),
        Kont::CondClause { .. } => result.push_str("CondClause"),
        Kont::EvalArg {
            have_proc,
            remaining_exprs,
            args_base,
            original_call,
            ..
        } => {
            let mut remaining_count = 0usize;
            let mut cursor = *remaining_exprs;
            while let crate::gc::SchemeValue::Pair(_, cdr) = gc_value!(cursor) {
                remaining_count += 1;
                cursor = *cdr;
            }
            result.push_str(
                format!(
                    "EvalArg{{proc={}, remaining={}, args_base={}, orig={}}}",
                    if *have_proc { "Some" } else { "None" },
                    remaining_count,
                    args_base,
                    print_value(original_call),
                )
                .as_str(),
            )
        }
        Kont::ApplyProc {
            proc,
            evaluated_args,
            ..
        } => result.push_str(
            format!(
                "ApplyProc{{proc={:?}, args={}}}",
                proc,
                evaluated_args.len()
            )
            .as_str(),
        ),
        Kont::ApplySpecial {
            proc,
            original_call,
            next,
        } => result.push_str(
            format!(
                "ApplySpecial{{proc={}, orig={}, next={:?}}}",
                print_value(proc),
                print_value(original_call),
                next
            )
            .as_str(),
        ),
        Kont::Bind { symbol, next, .. } => result
            .push_str(format!("Bind{{symbol={}, next={:?}}}", print_value(symbol), next).as_str()),
        Kont::DynamicWind { procs, next, .. } => result.push_str(
            format!(
                "DynamicWind{{after={},next={:?}}}",
                print_value(&procs.after),
                next
            )
            .as_str(),
        ),
        Kont::If {
            then_branch,
            else_branch,
            next, ..
        } => result.push_str(
            format!(
                "If{{then={}, else={}, next={:?}}}",
                print_value(then_branch),
                print_value(else_branch),
                next
            )
            .as_str(),
        ),
        Kont::RestoreEnv { old_env, .. } => {
            result.push_str("RestoreEnv:");
            result.push_str(&dbg_env_short(old_env));
        }
        Kont::Escape { .. } => result.push_str("Escape"),
        Kont::Seq { .. } => result.push_str("Seq"),
        Kont::MacroExpand { mode, .. } => {
            result.push_str(format!("MacroExpand{{mode={:?}}}", mode).as_str())
        }
        Kont::ExpandArg { .. } => result.push_str("ExpandArg"),
        Kont::EvalSeq { forms, .. } => result.push_str(
            format!(
                "EvalSeq{{remaining={}, results={}}}",
                forms.remaining.len(),
                forms.results.len()
            )
            .as_str(),
        ),
        Kont::Timer { .. } => result.push_str("Timer"),
        Kont::Exit { code } => result.push_str(format!("Exit{{code={}}}", code).as_str()),
        Kont::RestoreHandlers { .. } => result.push_str("RestoreHandlers"),
        Kont::RaiseReturn { .. } => result.push_str("RaiseReturn"),
        Kont::DebugPrint { .. } => result.push_str("DebugPrint"),
    }
    result
}

/// Print the continuation chain from `kont` down.
pub fn dbg_kont(loc: &str, kont: &KontRef) {
    std::io::Write::flush(&mut std::io::stdout()).ok();
    eprintln!("{}Stack:", loc);
    let mut kr = Rc::clone(kont);
    loop {
        eprintln!(" {}", dbg_one_kont("", &kr).trim());
        let Some(k) = kr.next() else { break };
        let k = Rc::clone(k);
        kr = k;
    }
}

/// Print the name of a frame's kind.
pub fn _dbg_short_kont(kont: &KontRef) {
    match **kont {
        Kont::Halt => println!("    Halt"),
        Kont::AndOr { kind, .. } => match &kind {
            AndOrKind::And => print!("AndOr {{kind=And}} "),
            AndOrKind::Or => print!("AndOr {{kind=Or}} "),
        },
        Kont::ApplyProc { .. } => print!("ApplyProc "),
        Kont::ApplySpecial { .. } => print!("ApplySpecial "),
        Kont::Bind { .. } => print!("Bind "),
        Kont::CallWithValues { .. } => print!("CallWithValues "),
        Kont::Cond { .. } => print!("Cond "),
        Kont::CondClause { .. } => print!("CondClause "),
        Kont::DynamicWind { .. } => print!("DynamicWind "),
        Kont::EvalArg { .. } => print!("EvalArg "),
        Kont::If { .. } => print!("If "),
        Kont::RestoreEnv { .. } => print!("RestoreEnv "),
        Kont::Escape { .. } => print!("Escape "),
        Kont::Seq { .. } => print!("Seq "),
        Kont::MacroExpand { .. } => print!("MacroExpand "),
        Kont::ExpandArg { .. } => print!("ExpandArg "),
        Kont::EvalSeq { .. } => print!("EvalSeq "),
        Kont::Timer { .. } => print!("Timer "),
        Kont::Exit { .. } => print!("Exit "),
        Kont::RestoreHandlers { .. } => print!("RestoreHandlers "),
        Kont::RaiseReturn { .. } => print!("RaiseReturn "),
        Kont::DebugPrint { .. } => print!("DebugPrint "),
    }
}

/// A one-line summary of a frame's bindings.
pub fn dbg_env_short(frame: &EnvRef) -> String {
    use std::cmp::min;
    let frame = frame.borrow();
    let mut bindings = Vec::new();
    for (k, v) in frame.bindings.iter() {
        match gc_value!(k) {
            crate::gc::SchemeValue::Symbol(s) => {
                bindings.push((s, v));
            }
            _ => {}
        }
    }
    bindings.sort_by(|(k1, _v1), (k2, _v2)| k1.cmp(k2));
    let more = bindings.len().saturating_sub(6);
    bindings.truncate(min(bindings.len(), 6));
    let mut result = String::new();
    for (k, v) in bindings {
        result.push_str(&format!("{}=>{} ", &k, print_value(&v)));
    }
    if more > 0 {
        result.push_str(&format!("... ({more} more)"));
    }
    result
}

/// Print one frame's bindings, sorted by name.
pub fn dbg_one_env(frame: &EnvRef, depth: usize) {
    std::io::Write::flush(&mut std::io::stdout()).ok();
    let frame = frame.borrow();
    let mut bindings: Vec<(String, _)> = frame
        .bindings
        .iter()
        .map(|(k, v)| match gc_value!(k) {
            crate::gc::SchemeValue::Symbol(s) => (s.to_string(), v),
            // Not a plain symbol (a renamed identifier, say): show it as
            // the printer would.
            _ => (print_value(&k), v),
        })
        .collect();
    eprintln!("Env frame {depth}:");
    bindings.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));
    for (k, v) in bindings {
        eprintln!("  {:20} => {}", k, print_value(&v));
    }
}

/// Debug print the environment, sorted alphabetically within each frame.
/// If no argument, print from the top of the environment chain
pub fn dbg_env(loc: &str, frame: EnvRef, global: bool) {
    std::io::Write::flush(&mut std::io::stdout()).ok();
    if !loc.is_empty() {
        eprintln!("{}", loc);
    }
    let mut depth = 0;
    let mut current = frame;
    loop {
        let next_frame = current.parent();
        match next_frame {
            Some(fr) => {
                dbg_one_env(&current, depth);
                depth += 1;
                current = fr;
            }
            None => {
                if global {
                    dbg_one_env(&current, depth);
                }
                return;
            }
        }
    }
}

use std::io;
use std::process::Command;

/// Run `cmd` with `sh -c` and return its standard output.
pub fn run_command(cmd: &str) -> io::Result<String> {
    let output = Command::new("sh").arg("-c").arg(cmd).output()?;
    // Convert stdout bytes to String, trimming trailing newlines if you like
    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}
