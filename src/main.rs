#![allow(dead_code)]
mod gc;
mod io;
mod parser;
mod tokenizer;

mod builtin;
mod env;
mod eval;
mod printer;

//use crate::parser::Parser;
//use crate::io::{Port, PortKind};
use crate::eval::{Evaluator, eval_logic, parse_and_deduplicate};
use argh::FromArgs;
use std::io as stdio;
//use stdio::Write;
use crate::printer::print_scheme_value;

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
    /// trace mode
    #[argh(switch, short = 't')]
    trace: bool,
}

fn main() {
    let args: Args = argh::from_env();
    let mut evaluator = Evaluator::new();

    match evaluator.initialize_scheme_io_globals() {
        Ok(_val) => {}
        Err(msg) => {
            println!("IO initialization failed: {}", msg);
            std::process::exit(1);
        }
    }

    // Execute startup commands as Scheme code
    let mut startup_commands = Vec::new();

    // Load core file unless --no-core
    if !args.no_core {
        startup_commands.push(format!(
            "(push-port! (open-input-file \"scheme/s1-core.scm\"))"
        ));
    }

    // Load each file in order
    for filename in &args.file {
        startup_commands.push(format!("(push-port! (open-input-file \"{}\"))", filename));
    }

    // Execute startup commands
    for command in startup_commands {
        if let Err(e) = evaluator.eval_string(&command) {
            eprintln!("Error executing startup command '{}': {}", command, e);
            std::process::exit(1);
        }
    }

    if args.trace {
        evaluator.trace = true;
    }

    if args.quit {
        return;
    }

    // Drop into the REPL
    repl(&mut evaluator);
}

fn repl(evaluator: &mut Evaluator) {
    use crate::gc::SchemeValue;
    use crate::io::PortKind;
    // use crate::tokenizer::Tokenizer;
    use crate::parser::Parser;
    use std::io as stdio;
    use stdio::Write;

    let mut parser = Parser::new();
    let port_stack_sym = evaluator.heap.intern_symbol("**port-stack**");
    let mut interactive = false;
    println!("Welcome to the s1 Scheme REPL");

    // Initialize the current port from the top of the stack
    let mut current_port = {
        let port_stack_val = evaluator
            .env()
            .get_symbol(port_stack_sym)
            .expect("**port-stack** is unbound at REPL start");
        match crate::io::extract_port_from_stack_val(&port_stack_val.value) {
            Ok(p) => p,
            Err(e) => {
                println!("Error: {}", e);
                return;
            }
        }
    };

    loop {
        // Check the port. Each parse-eval needs to be sure the port hasn't changed.
        if evaluator.new_port {
            evaluator.new_port = false;
            current_port = {
                let port_stack_val = evaluator
                    .env()
                    .get_symbol(port_stack_sym)
                    .expect("**port-stack** is unbound at REPL start");
                match crate::io::extract_port_from_stack_val(&port_stack_val.value) {
                    Ok(p) => p,
                    Err(e) => {
                        println!("Error: {}", e);
                        return;
                    }
                }
            };
        }

        interactive = matches!(current_port.kind, PortKind::Stdin);
        if interactive {
            print!("s1> ");
            stdio::stdout().flush().unwrap();
        }
        match parse_and_deduplicate(&mut parser, &mut current_port, evaluator.heap_mut()) {
            Ok(expr) => match eval_logic(expr, evaluator) {
                Ok(result) => {
                    if interactive {
                        println!("=> {}", print_scheme_value(&result.value));
                    }
                }
                Err(e) => println!("Evaluation error: {}", e),
            },
            Err(crate::parser::ParseError::Eof) => {
                // Pop the port stack on EOF
                let port_stack_val = evaluator
                    .env()
                    .get_symbol(port_stack_sym)
                    .expect("**port-stack** is unbound");
                match &port_stack_val.value {
                    SchemeValue::Pair(_car, cdr) => {
                        evaluator.env_mut().set_symbol(port_stack_sym, *cdr);
                        // Update current_port to the new top of the stack
                        let new_stack_val = evaluator
                            .env()
                            .get_symbol(port_stack_sym)
                            .expect("**port-stack** is unbound after pop");
                        match crate::io::extract_port_from_stack_val(&new_stack_val.value) {
                            Ok(p) => current_port = p,
                            Err(e) => {
                                println!("Error: {}", e);
                                break;
                            }
                        }
                    }
                    SchemeValue::Nil => {
                        // Port stack is empty: exit cleanly
                        break;
                    }
                    _ => {
                        println!("Error: **port-stack** is not a proper list");
                        break;
                    }
                }
                continue;
            }
            Err(crate::parser::ParseError::Syntax(e)) => {
                println!("Parse error: {}", e);
                continue;
            }
        }
    }
}
