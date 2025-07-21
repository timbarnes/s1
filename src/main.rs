#![allow(dead_code)]
mod gc;
mod io;
mod tokenizer;
mod parser;

mod eval;
mod printer;
mod builtin;
mod env;

use crate::parser::Parser;
use crate::io::{Port, PortKind};
use crate::eval::{Evaluator, eval_logic, parse_and_deduplicate};
use argh::FromArgs;
use std::io as stdio;
use stdio::Write;
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
}

fn main() {
    let args: Args = argh::from_env();
    let mut evaluator = Evaluator::new();

    match evaluator.initialize_scheme_io_globals() {
        Ok(_val) => {},
        Err(msg) => {
            println!("IO initialization failed: {}", msg);
            std::process::exit(1);
        }
    }

    // Execute startup commands as Scheme code
    // let mut startup_commands = Vec::new();
    
    // Load core file unless --no-core
    // if !args.no_core {
    //     startup_commands.push(format!("(load \"scheme/s1-core.scm\")"));
    // }
    
    // Load each file in order
    // for filename in &args.file {
    //     startup_commands.push(format!("(load \"{}\")", filename));
    // }
    
    // Execute startup commands
    // for command in startup_commands {
    //     if let Err(e) = evaluator.eval_string(&command) {
    //         eprintln!("Error executing startup command '{}': {}", command, e);
    //         std::process::exit(1);
    //     }
    // }
    
    if args.quit {
        return;
    }
    
    // Drop into the REPL
    repl(&mut evaluator);
}



fn repl(evaluator: &mut Evaluator) {
    use crate::gc::SchemeValue;
    use crate::io::{Port, PortKind};
    // use crate::tokenizer::Tokenizer;
    use crate::parser::Parser;
    use std::io as stdio;
    use stdio::Write;

    let stdin = stdio::stdin();
    let mut parser = Parser::new();
    // Cache the interned symbol for **port-stack**
    let port_stack_sym = evaluator.heap.intern_symbol("**port-stack**");
    let mut interactive = false;
    println!("Welcome to the s1 Scheme REPL");
    loop {
        // Look up **port-stack** in the environment
        let port_stack_val = evaluator.env().get_symbol(port_stack_sym);
        let port_stack_val = match port_stack_val {
            Some(v) => v,
            None => {
                println!("Error: **port-stack** is unbound");
                break;
            }
        };
        match &port_stack_val.value {
            SchemeValue::Nil => {
                // Port stack is empty: exit cleanly
                break;
            }
            SchemeValue::Pair(car, _cdr) => {
                // car should be a port object
                let port_val = &car.value;
                let mut port = match port_val {
                    SchemeValue::Port { kind } => Port { kind: kind.clone() },
                    _ => {
                        println!("Error: car of **port-stack** is not a port");
                        break;
                    }
                };
                
                // Store the original port kind to update it later
                let original_kind = match port_val {
                    SchemeValue::Port { kind } => kind.clone(),
                    _ => unreachable!(),
                };
                interactive = matches!(port.kind, PortKind::Stdin);
                if interactive {
                    print!("s1> ");
                    stdio::stdout().flush().unwrap();
                }
                let parse_result = parse_and_deduplicate(&mut parser, &mut port, evaluator.heap_mut());
                // Update the original port with the new position if it's a string port
                if let SchemeValue::Port { kind } = &car.value {
                    if let PortKind::StringPortInput { pos: _, .. } = kind {
                        if let PortKind::StringPortInput { pos: new_pos, .. } = &port.kind {
                            let current_pos = crate::io::get_string_port_pos(&port).unwrap();
                            evaluator.heap_mut().update_string_port_pos(*car, current_pos);
                        }
                    }
                }
                match parse_result {
                    Ok(expr) => {
                        println!("Parsed: {}", print_scheme_value(&expr.value));
                        match eval_logic(expr, evaluator) {
                            Ok(result) => {
                                if interactive {
                                    println!("=> {}", print_scheme_value(&result.value));
                                }
                            }
                            Err(e) => println!("Evaluation error: {}", e),
                        }
                    }
                    Err(e) => {
                        // For stdin, read input and create a new port
                        if matches!(port.kind, PortKind::Stdin) {
                            let mut input = String::new();
                            if stdin.read_line(&mut input).unwrap() == 0 {
                                // EOF on stdin
                                // Pop the port from **port-stack**
                                match &port_stack_val.value {
                                    SchemeValue::Pair(_car, cdr) => {
                                        evaluator.env_mut().set_symbol(port_stack_sym, *cdr);
                                    }
                                    _ => {
                                        println!("Error: **port-stack** is not a proper list");
                                        break;
                                    }
                                }
                                continue;
                            }
                            if input.trim().is_empty() { continue; }
                            // Replace the current port with the new input
                            port.kind = PortKind::StringPortInput {
                                content: input,
                                pos: std::cell::UnsafeCell::new(0),
                            };
                        } else {
                            // For non-stdin ports, pop on any parse error and continue
                            println!("Parse error: {}", e);
                            match &port_stack_val.value {
                                SchemeValue::Pair(_car, cdr) => {
                                    evaluator.env_mut().set_symbol(port_stack_sym, *cdr);
                                    continue;
                                }
                                _ => {
                                    println!("Error: **port-stack** is not a proper list");
                                    break;
                                }
                            }
                        }
                    }
                }
            }
            _ => {
                println!("Error: **port-stack** is not a list");
                break;
            }
        }
    }
}
