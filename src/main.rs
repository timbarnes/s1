//! s1: a Scheme interpreter in Rust, aiming at R7RS-small.
//!
//! These are the internal docs, for working on the interpreter. Read the user
//! documentation in `docs/s1-docs.md` and the design notes in `design/*.md`
//! (both relative to the repository root) alongside them.
//!
//! # How a form is evaluated
//!
//! 1. [`tokenizer`] turns a port's characters into tokens, and [`parser`]
//!    builds a datum from them on the garbage-collected heap ([`gc`]).
//! 2. [`eval`] runs the datum on a CEK machine: a control (expression or
//!    value), an environment ([`env`](mod@env)), and a continuation of linked frames
//!    (`eval::kont`). The step loop is `eval::cek`. It never recurses on the
//!    Rust stack for Scheme calls, which gives proper tail calls and
//!    first-class continuations.
//! 3. Operators are procedures or syntax ([`gc::Callable`]). Builtins
//!    ([`builtin`]) take evaluated arguments and return a value.
//!    Sys-builtins ([`sys_builtins`], [`ports`], `eval::exceptions`) also get
//!    the machine. Special forms ([`special_forms`]) get the unevaluated form.
//!    `syntax-rules` macros ([`syntax_rules`]) expand hygienically, by
//!    renaming (`eval::identifiers`).
//! 4. [`printer`] gives values their external representation.
//!
//! [`main`] loads `scheme/s1-core.scm` (the parts of the language written in
//! Scheme) into the system environment, registers the standard libraries
//! ([`libraries`]), then runs files, a script, or the REPL in the
//! interaction environment.
//!
//! # Modules
//!
//! - [`tokenizer`], [`parser`], [`number_syntax`]: reading
//! - [`gc`]: the heap, value representation and collector
//! - [`env`](mod@env): environments and top-level binding cells
//! - [`eval`]: the CEK machine, exceptions and hygiene
//! - [`special_forms`]: core syntax implemented in Rust
//! - [`syntax_rules`]: `syntax-rules` transformers
//! - [`builtin`], [`sys_builtins`]: built-in procedures
//! - [`ports`], [`io`]: ports and I/O
//! - [`libraries`]: `define-library`, `import` and the standard libraries
//! - [`printer`]: `write` and `display`
//! - [`utilities`]: error reporting, tracing and debug dumps
//!
//! Build these docs with `cargo doc --open`; `.cargo/config.toml` turns on
//! private items, since s1 is a binary crate and nearly everything in it is
//! private.

mod builtin;
mod env;
mod eval;
mod gc;
mod io;
mod libraries;
mod number_syntax;
mod parser;
mod ports;
mod printer;
mod special_forms;
mod syntax_rules;
mod sys_builtins;
mod tokenizer;
mod utilities;

use crate::env::{EnvRef, Frame};
use crate::eval::{
    CEKState, RunTime, RunTimeStruct, eval_main, eval_string, initialize_scheme_globals,
};
use crate::gc::SchemeValue;
use crate::parser::parse;
use crate::printer::print_value;
use std::cell::RefCell;
use std::rc::Rc;

use argh::FromArgs;

// Measured ~15% on the GC-heavy regression suite, where sweep frees objects
// en masse; roughly neutral on the call-heavy micro benchmarks. See
// design/performance.md, F11.
/// The global allocator: mimalloc.
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[derive(FromArgs)]
/// A simple Scheme interpreter
struct Args {
    /// do not load scheme/s1-core.scm
    #[argh(switch, short = 'n')]
    no_core: bool,
    /// files to load after core (can be repeated)
    #[argh(option, short = 'f')]
    file: Vec<String>,
    /// exit after file loading, do not enter REPL
    #[argh(switch, short = 'q')]
    quit: bool,
    /// run regression tests
    #[argh(switch, short = 'r')]
    regression: bool,
    /// a program to run after the -f files, then exit, and the arguments
    /// `command-line` gives it
    #[argh(positional, greedy)]
    script: Vec<String>,
}

/// Parse the command line, initialise the runtime, load s1-core.scm and
/// the standard libraries, then run the files, the script or the REPL.
fn main() {
    // Process command-line arguments
    let args: Args = argh::from_env();
    let script = args.script.first().cloned();
    let command_line = if script.is_some() { args.script.clone() } else { vec!["s1".to_string()] };
    crate::builtin::system::set_command_line(command_line, script.is_some());
    // Initialize the system environment and runtime
    let system = Rc::new(RefCell::new(Frame::new(None)));
    let mut runtime = RunTimeStruct::new();
    let mut rt = RunTime::from_eval(&mut runtime);
    // Set up ports and builtin functions and variables
    match initialize_scheme_globals(&mut rt, system.clone()) {
        Ok(_val) => {} // Ignore success value
        Err(msg) => {
            println!("Runtime initialization failed: {}", msg);
            std::process::exit(1);
        }
    }

    let mut state = CEKState::new(system.clone());
    // A script's output is its own: no banner.
    let banner = script.is_none();
    if banner {
        println!("Welcome to the s1 Scheme REPL");
    }

    // s1-core.scm is part of the system: it loads, to completion, into the
    // system environment, before the interaction environment is made.
    if !args.no_core {
        run_startup_command("(push-port! (open-input-file \"scheme/s1-core.scm\"))", &mut state, &mut rt);
        repl(&mut rt, &mut state, true, system.clone());
        if banner {
            println!("s1-core loaded");
        }
    }

    // The standard libraries are views of the system environment, and the
    // interaction environment, where everything else runs, imports all of it
    // (design/libraries-design.md).
    crate::libraries::register_standard_libraries(rt.heap, &system);
    let env = crate::libraries::make_interaction_env(&system);
    rt.heap.set_interaction_env(env.clone());
    state.env = env.clone();

    let mut startup_commands = Vec::new();

    // Run regression tests if --regression is specified
    if args.regression {
        startup_commands.push(format!(
            "(push-port! (open-input-file \"scheme/regression.scm\"))"
        ));
    }

    // Load each file in order, then the script
    for filename in args.file.iter().chain(script.iter()) {
        startup_commands.push(format!("(push-port! (open-input-file \"{}\"))", filename));
    }

    // Execute startup commands in reverse to build the port stack correctly
    for command in startup_commands.into_iter().rev() {
        run_startup_command(&command, &mut state, &mut rt);
    }

    // Drop into the REPL
    repl(&mut rt, &mut state, args.quit || script.is_some(), env);
}

/// Evaluate a command needed at startup; exit if it fails.
fn run_startup_command(command: &str, state: &mut CEKState, rt: &mut RunTime) {
    if let Err(e) = eval_string(command, state, rt) {
        eprintln!("Error executing startup command '{}': {}", command, e);
        std::process::exit(1);
    }
}

/// Read and evaluate forms from the port stack in `global` until it is
/// empty, or, with `quit_after_load`, until only standard input is left.
fn repl(rt: &mut RunTime, state: &mut CEKState, quit_after_load: bool, global: EnvRef) {
    use crate::io::PortKind;
    use std::io as stdio;
    use stdio::Write;

    let mut interactive;

    loop {
        *rt.depth = 0;
        // Check the port. Each parse-eval needs to be sure the port hasn't changed.
        let current_port_ref = match rt.port_stack.last() {
            Some(port_ref) => *port_ref,
            None => break, // No more ports, exit repl
        };

        // Check if interactive before parsing
        interactive = {
            let port_kind = rt.heap.get_value(current_port_ref);
            if let SchemeValue::Port(port_kind) = port_kind {
                matches!(**port_kind, PortKind::Stdin)
            } else {
                false
            }
        };

        if interactive {
            print!("s1> ");
            stdio::stdout().flush().unwrap();
        }

        let expr = {
            if let SchemeValue::Port(port_kind) = gc_value_mut!(current_port_ref) {
                parse(rt.heap, port_kind)
            } else {
                Err(crate::parser::ParseError::Syntax(
                    "Expected port on port stack".to_string(),
                ))
            }
        };
        match expr {
            Ok(expr) => {
                // Every form read from a port is a top-level form and must be
                // evaluated in the global environment. Without this, definitions
                // made by a file that `load` pulled in land in `load`'s own frame,
                // and the chain grows by a frame per load and never unwinds.
                state.env = global.clone();
                let returned = eval_main(expr, state, rt);
                match returned {
                    Ok(result) => {
                        if interactive {
                            for v in result.iter() {
                                println!("=> {}", print_value(&v));
                                rt.heap.collect_garbage(
                                    &state,
                                    &rt.current_ports[..],
                                    &rt.port_stack,
                                    &rt.dynamic_wind,
                                    &rt.arg_stack,
                                    *rt.handlers,
                                );
                            }
                        }
                    }
                    Err(e) => {
                        println!("Error: {}", e);
                        if crate::builtin::system::script_mode() {
                            crate::sys_builtins::exit_now(crate::builtin::system::SCRIPT_ERROR_STATUS);
                        }
                    }
                }
            }
            Err(crate::parser::ParseError::Eof) => {
                // Pop the port stack on EOF
                rt.port_stack.pop();
                if quit_after_load && rt.port_stack.len() == 1 {
                    break;
                }
                if rt.port_stack.is_empty() {
                    break;
                }
                continue;
            }
            Err(crate::parser::ParseError::Syntax(e)) => {
                println!("Parse error: {}", e);
                if crate::builtin::system::script_mode() {
                    crate::sys_builtins::exit_now(crate::builtin::system::SCRIPT_ERROR_STATUS);
                }
                continue;
            }
        }
    }
    // Output is line-buffered now; emit any trailing partial line.
    stdio::stdout().flush().ok();
}
