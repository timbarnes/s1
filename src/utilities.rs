/// Internal utility functions
///
use crate::env::{EnvOps, EnvRef};
use crate::eval::{RunTime, TraceType};
use crate::gc_value;
use crate::kont::{AndOrKind, CEKState, Control, Kont, KontRef};
use crate::printer::print_value;
use std::rc::Rc;

/// Push an error into the existing CEKState.
pub fn post_error(state: &CEKState, ec: &mut RunTime, error: &str) {
    eprintln!("Error: {}", error);
    *ec.trace = TraceType::Step;
    debugger(state, ec);
}

/// Trace / debug function called from within the CEK machine and on error
///
pub fn debugger(state: &CEKState, ec: &mut RunTime) {
    // simple indentation
    match ec.trace {
        TraceType::Off => return,
        TraceType::Control => {
            indent(*ec.depth, true);
            println!(" {}", dump_control(&state.control));
        }
        TraceType::Full => {
            indent(*ec.depth, true);
            println!(" {}", dump_control(&state.control));
            indent(*ec.depth + 1, false);
            dbg_kont(&state.kont);
            //dump_cek("", &state);
        }
        TraceType::Step => {
            indent(*ec.depth, true);
            debug_interactive(state, ec);
        }
    }
}

fn indent(n: i32, v: bool) {
    for i in 0..n {
        if v {
            if i % 10 == 0 {
                print!("{}", i / 10);
            } else if i % 2 == 0 {
                print!(".");
            } else {
                print!(" ");
            }
        } else {
            print!(" ");
        }
    }
}

fn debug_interactive(state: &CEKState, ec: &mut RunTime) {
    use std::io::{self, Write};
    loop {
        print!("debug> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        if io::stdin().read_line(&mut input).is_err() {
            println!("error reading input");
            continue;
        }
        let cmd = input.trim();
        //println!("Command: {}", cmd);
        match cmd {
            "" | "n" | "next" => {
                // step one more and come back
                println!("control expr = {}", dump_control(&state.control));
                break;
            }
            "c" | "continue" => {
                // run freely until next breakpoint / error
                *ec.trace = TraceType::Off;
                break;
            }
            "e" | "env" => {
                let frame = state.env.clone();
                dbg_env(frame, false);
            }
            "k" | "kont" => {
                dbg_kont(&state.kont);
            }
            "l" | "locals" => {
                dbg_one_env(&state.env, 0);
            }
            "x" | "expr" => {
                println!("control expr = {}", dump_control(&state.control));
            }
            "s" | "state" => {
                dump_cek("state: ", &state);
            }
            // "q" | "quit" => {
            //     println!("Exiting...");
            //     state.control = Control::Empty;
            //     state.kont = Rc::new(Kont::Halt);
            // }
            _ => {
                println!(
                    "commands: n(ext), c(ontinue), e(nv), k(ont), l(ocals), x or expr, s(tate)"
                );
            }
        }
    }
}

fn dump_control(control: &Control) -> String {
    match control {
        Control::Expr(obj) => format!("Expr  = {}", print_value(obj)),
        Control::Value(obj) => format!("Value = {}", print_value(obj)),
        Control::Values(vals) => format!("Values[0]={:20}", print_value(&vals[0])),
        Control::Escape(val, k) => format!("Escape = {}, {:?}", print_value(val), k),
        Control::Empty => format!("Empty"),
    }
}

// Dump a summary of the CEK machine state
///
pub fn dump_cek(_loc: &str, state: &CEKState) {
    match &state.control {
        Control::Expr(obj) => {
            eprintln!(
                "Expr   {};      Kont = {}; Tail={}",
                print_value(obj),
                dbg_one_kont(&state.kont),
                state.tail
            );
        }
        Control::Value(obj) => {
            eprintln!(
                "Value  {};      Kont = {}",
                print_value(obj),
                dbg_one_kont(&state.kont)
            );
        }
        Control::Values(vals) => {
            eprintln!(
                "Values[0] {};      Kont = {}",
                print_value(&vals[0]),
                dbg_one_kont(&state.kont)
            );
        }
        Control::Escape(val, kont) => {
            eprintln!(
                "Escape; Val {};      Kont = {}",
                print_value(val),
                dbg_one_kont(&kont)
            );
        }
        Control::Empty => {
            eprintln!("Halt      Kont = {}", dbg_one_kont(&state.kont));
        }
    }
}

fn dbg_one_kont(frame: &Kont) -> String {
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
                print_value(proc),
                print_value(original_call),
                next
            )
        }
        Kont::If {
            then_branch,
            else_branch,
            next,
        } => {
            format!(
                "If{{then={}, else={}, next={:?}}}",
                print_value(then_branch),
                print_value(else_branch),
                next
            )
        }
        Kont::Bind {
            symbol,
            env: _,
            next,
        } => {
            format!("Bind{{symbol={}, next={:?}}}", print_value(symbol), next)
        }
        other => format!("{:?}", other),
    }
}

pub fn dbg_kont(kont: &KontRef) {
    print!("  Stack:");
    let mut kr = Rc::clone(&kont);
    dbg_one_kont(&kr);
    let k_next = kr.next();
    match k_next {
        Some(k) => kr = Rc::clone(k),
        None => return,
    };
    while let Some(k) = kr.next() {
        dbg_short_kont(&kr);
        kr = Rc::clone(k);
    }
    println!("");
}

pub fn dbg_short_kont(kont: &KontRef) {
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
        Kont::Eval { .. } => print!("Eval "),
        Kont::EvalArg { .. } => print!("EvalArg "),
        Kont::If { .. } => print!("If "),
        Kont::RestoreEnv { .. } => print!("RestoreEnv "),
        Kont::Seq { .. } => print!("Seq "),
    }
}

pub fn dbg_one_env(frame: &EnvRef, depth: usize) {
    let frame = frame.borrow();
    let mut bindings = Vec::new();
    println!("Env frame {depth}:");
    for (k, v) in frame.bindings.iter() {
        match gc_value!(*k) {
            crate::gc::SchemeValue::Symbol(s) => {
                bindings.push((s, v));
            }
            _ => unreachable!(),
        }
    }
    bindings.sort_by(|(k1, _v1), (k2, _v2)| k1.cmp(k2));
    for (k, v) in bindings {
        println!("  {:20} => {}", &k, print_value(&v));
    }
}

/// Debug print the environment, sorted alphabetically within each frame.
/// If no argument, print from the top of the environment chain
pub fn dbg_env(frame: EnvRef, global: bool) {
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
