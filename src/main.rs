mod builtin;
mod cek;
mod env;
mod eval;
mod gc;
mod io;
mod kont;
mod macros;
mod parser;
mod printer;
mod special_forms;
mod sys_builtins;
mod tokenizer;
mod utilities;

use crate::cek::eval_main;
use crate::env::EnvRef;
use crate::eval::{Evaluator, eval_string};
use crate::printer::print_value;
use std::cell::RefCell;
use std::rc::Rc;

use argh::FromArgs;
use eval::RunTime;
use std::io as stdio;

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
}

fn main() {
    let args: Args = argh::from_env();
    let env = Rc::new(RefCell::new(crate::env::Frame::new(None)));
    let mut evaluator = Evaluator::new(env.clone());
    let mut ec = RunTime::from_eval(&mut evaluator);
    // match initialize_scheme_io_globals(env.clone()) {
    //     Ok(_val) => {}
    //     Err(msg) => {
    //         println!("IO initialization failed: {}", msg);
    //         std::process::exit(1);
    //     }
    // }

    // Execute startup commands as Scheme code
    let mut startup_commands = Vec::new();

    if args.quit {
        startup_commands.push(format!("(eval-string \"(exit))\"))"));
    }
    // Load each file in order
    for filename in &args.file {
        startup_commands.push(format!("(push-port! (open-input-file \"{}\"))", filename));
    }

    // Run regression tests if --regression is specified
    if args.regression {
        startup_commands.push(format!(
            "(push-port! (open-input-file \"scheme/regression.scm\"))"
        ));
    }

    // Load core file unless --no-core
    if !args.no_core {
        startup_commands.push(format!(
            "(push-port! (open-input-file \"scheme/s1-core.scm\"))"
        ));
    }

    // Execute startup commands
    for command in startup_commands {
        if let Err(e) = eval_string(&mut ec, &command, env.clone()) {
            eprintln!("Error executing startup command '{}': {}", command, e);
            std::process::exit(1);
        }
    }

    // Drop into the REPL
    repl(&mut ec, env.clone());
}

fn repl(ev: &mut RunTime, env: EnvRef) {
    use crate::io::PortKind;
    use crate::parser::parse;
    use std::io as stdio;
    use stdio::Write;

    let mut interactive;
    println!("Welcome to the s1 Scheme REPL");

    loop {
        *ev.depth = 0;
        // Check the port. Each parse-eval needs to be sure the port hasn't changed.
        let current_port_val: &mut PortKind;
        let current_port = ev.port_stack.last_mut();
        match current_port {
            Some(port_kind) => current_port_val = port_kind,
            None => continue,
        }

        interactive = matches!(current_port_val, PortKind::Stdin);
        if interactive {
            print!("s1> ");
            stdio::stdout().flush().unwrap();
        }
        let expr = parse(ev.heap, current_port_val);
        match expr {
            Ok(expr) => match eval_main(expr, env.clone(), ev) {
                Ok(result) => {
                    if interactive {
                        for v in result.iter() {
                            println!("=> {}", print_value(&v));
                        }
                    }
                }
                Err(e) => println!("Error: {}", e),
            },
            Err(crate::parser::ParseError::Eof) => {
                // Pop the port stack on EOF
                let port = ev.port_stack.pop(); // Pop and set new port
                match &port {
                    Some(_) => {}
                    None => println!("Error popping port stack"),
                }
                continue;
            }
            Err(crate::parser::ParseError::Syntax(e)) => {
                println!("Parse error: {}", e);
                continue;
            } // Err(crate::parser::ParseError::Other(e)) => {
              //     println!("Other error: {}", e);
              //     continue;
              // }
        }
    }
}
