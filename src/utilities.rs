/// Internal utility functions
///
use crate::env::Frame;
use crate::eval::EvalContext;
use crate::kont::{AndOrKind, CEKState, Control, Kont};
use crate::printer::print_value;
use std::cell::RefCell;
use std::rc::Rc;

/// Push an error into the existing CEKState.
pub fn post_error(state: &mut CEKState, ec: &mut EvalContext, error: String) {
    eprintln!("Error: {}", error);
    debug_cek(state, ec);
}

/// Trace / debug function called from within the CEK machine and on error
///
pub fn debug_cek(state: &mut CEKState, ec: &mut EvalContext) {
    // simple indentation
    for i in 0..*ec.depth {
        if i % 10 == 0 {
            print!("{}", i / 10);
        } else {
            print!(".");
        }
    }

    println!("{}", dump_control(&state.control));

    if *ec.step {
        debug_interactive(state, ec);
    }
}

fn debug_interactive(state: &mut CEKState, ec: &mut EvalContext) {
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
        println!("Command: {}", cmd);
        match cmd {
            "" | "n" | "next" => {
                // step one more and come back
                break;
            }
            "c" | "continue" => {
                // run freely until next breakpoint / error
                *ec.step = false;
                break;
            }
            "e" | "env" => {
                let frame = ec.env.current_frame.clone();
                dump_env(Some(frame));
            }
            "l" | "locals" => {
                dump_locals(&state.env);
            }
            "ctrl" => {
                println!("control = {}", dump_control(&state.control));
            }
            "s" | "state" => {
                dump_cek("", &state);
            }
            "q" | "quit" => {
                println!("Exiting...");
                state.control = Control::Empty;
                state.kont = Rc::new(Kont::Halt);
            }
            _ => {
                println!("commands: n(ext), c(ontinue), e(nv), l(ocals), ctrl, s(tate)");
            }
        }
    }
}

fn dump_control(control: &Control) -> String {
    match control {
        Control::Expr(obj) => format!("Expr  = {}", print_value(obj)),
        Control::Value(obj) => format!("Value = {}", print_value(obj)),
        Control::Error(e) => format!("Error = {}", e),
        Control::Empty => format!("Empty"),
    }
}

// Dump a summary of the CEK machine state
///
pub fn dump_cek(loc: &str, state: &CEKState) {
    if false {
        match &state.control {
            Control::Expr(obj) => {
                eprintln!(
                    "{loc:18} Control: Expr={}; Kont={}; Tail={}",
                    print_value(obj),
                    frame_debug_short(&state.kont),
                    state.tail
                );
            }
            Control::Value(obj) => {
                eprintln!(
                    "{loc:18} Control: Value={:20}; Kont={}",
                    print_value(obj),
                    frame_debug_short(&state.kont)
                );
            }
            Control::Error(e) => {
                eprintln!(
                    "{loc:18} Control: Error={}; Kont={}",
                    e,
                    frame_debug_short(&state.kont)
                );
            }
            Control::Empty => {
                eprintln!(
                    "{loc:18} Control: Halt; Kont={}",
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

pub fn dump_kont(state: &CEKState) {
    println!(" CEK State:");
    match &state.control {
        Control::Expr(e) => print!("  Control::Expr: {:?} ", print_value(e)),
        Control::Value(v) => print!("  Control::Value: {:?} ", print_value(v)),
        Control::Error(e) => print!("  Control::Error: {} ", e),
        Control::Empty => print!("  Control::Empty "),
    }
    println!("tail={}", &state.tail);
    println!("  Stack:");
    let mut kont = state.kont.clone();
    loop {
        match &*kont {
            Kont::Halt => {
                println!("    Halt");
                return;
            }
            Kont::AndOr {
                kind,
                rest: _,
                next,
            } => {
                match &kind {
                    AndOrKind::And => println!("    AndOr {{kind=And}}"),
                    AndOrKind::Or => println!("    AndOr {{kind=Or}}"),
                }
                kont = next.clone();
            }
            Kont::ApplyProc {
                proc,
                evaluated_args,
                next,
            } => {
                println!(
                    "    ApplyProc {{proc={:?}, args={}}}",
                    proc,
                    evaluated_args.len()
                );
                kont = next.clone();
            }
            Kont::ApplySpecial {
                proc,
                original_call,
                next,
            } => {
                println!(
                    "    ApplySpecial {{proc={}, orig={}}}",
                    print_value(&proc),
                    print_value(&original_call),
                );
                kont = next.clone();
            }
            Kont::Bind {
                symbol,
                env: _,
                next,
            } => {
                println!("    Bind {{symbol={}}}", print_value(&symbol),);
                kont = next.clone();
            } // other => format!("{:?}", other),
            Kont::Cond { remaining, next } => {
                println!("    Cond {{remaining={}}}", remaining.len());
                kont = next.clone();
            }
            Kont::CondClause { clause: _, next } => {
                println!("    CondClause");
                kont = next.clone();
            }
            Kont::Eval {
                expr,
                env: _,
                phase: _,
                next,
            } => {
                println!("    Eval {{expr={}}}", print_value(&expr));
                kont = next.clone();
            }
            Kont::EvalArg {
                proc,
                remaining,
                evaluated,
                original_call,
                tail: _,
                env: _,
                next,
            } => {
                println!(
                    "    EvalArg {{proc={}, rem={}, eval={}, orig={:?}}}",
                    if proc.is_some() { "Some" } else { "None" },
                    remaining.len(),
                    evaluated.len(),
                    original_call,
                );
                kont = next.clone();
            }
            Kont::Handler {
                handler_expr,
                handler_env: _,
                next,
            } => {
                println!("    Handler {{expr={}}}", print_value(&handler_expr),);
                kont = next.clone();
            }
            Kont::If {
                then_branch,
                else_branch,
                next,
            } => {
                println!(
                    "    If {{then={}, else={}}}",
                    print_value(&then_branch),
                    print_value(&else_branch),
                );
                kont = next.clone();
            }
            Kont::RestoreEnv { old_env: _, next } => {
                println!("    RestoreEnv");
                kont = next.clone();
            }
            Kont::Seq { rest, next } => {
                println!("    Seq {{rest={}}}", rest.len());
                kont = next.clone();
            }
        }
    }
}

pub fn dump_locals(env: &Rc<RefCell<Frame>>) {
    let frame = env.borrow();
    println!("Local env: ");
    for (k, v) in frame.bindings.iter() {
        // Replace with your actual printer for GcRef
        println!("  {:20} => {}", print_value(&k), print_value(&v));
    }
}

pub fn dump_env(env: Option<Rc<RefCell<Frame>>>) {
    let mut depth = 0;
    let mut current = env;
    while let Some(frame_rc) = current {
        let frame = frame_rc.borrow();
        println!("Env frame {depth}:");
        for (k, v) in frame.bindings.iter() {
            // Replace with your actual printer for GcRef
            println!("  {:20} => {}", print_value(&k), print_value(&v));
        }
        current = frame.parent.clone();
        depth += 1;
    }
}
