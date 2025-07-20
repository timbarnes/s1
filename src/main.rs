#![allow(dead_code)]
mod gc;
mod io;
mod tokenizer;
mod parser;

mod evalsimple;
mod builtin;
mod env;

use crate::parser::Parser;
use crate::io::{Port, PortKind, PortStack};
use crate::evalsimple::{Evaluator, eval_logic, parse_and_deduplicate};
use argh::FromArgs;
use std::io as stdio;
use stdio::Write;

fn print_scheme_value(val: &crate::gc::SchemeValueSimple) -> String {
    use crate::gc::SchemeValueSimple;
    match val {
        SchemeValueSimple::Pair(_, _) => {
            let mut s = String::from("(");
            let mut first = true;
            let mut current = val;
            loop {
                match current {
                    SchemeValueSimple::Pair(car, cdr) => {
                        if !first { s.push(' '); }
                        s.push_str(&print_scheme_value(&car.value));
                        current = &cdr.value;
                        first = false;
                    }
                    SchemeValueSimple::Nil => {
                        s.push(')');
                        break;
                    }
                    _ => {
                        s.push_str(" . ");
                        s.push_str(&print_scheme_value(current));
                        s.push(')');
                        break;
                    }
                }
            }
            s
        }
        SchemeValueSimple::Symbol(s) => s.clone(),
        SchemeValueSimple::Int(i) => i.to_string(),
        SchemeValueSimple::Float(f) => f.to_string(),
        SchemeValueSimple::Str(s) => format!("\"{}\"", s),
        SchemeValueSimple::Bool(true) => "#t".to_string(),
        SchemeValueSimple::Bool(false) => "#f".to_string(),
        SchemeValueSimple::Char(c) => format!("#\\{}", c),
        SchemeValueSimple::Nil => "nil".to_string(),
        _ => format!("{:?}", val),
    }
}

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
    let mut startup_commands = Vec::new();
    
    // Load core file unless --no-core
    if !args.no_core {
        startup_commands.push(format!("(load \"scheme/s1-core.scm\")"));
    }
    
    // Load each file in order
    for filename in &args.file {
        startup_commands.push(format!("(load \"{}\")", filename));
    }
    
    // Execute startup commands
    for command in startup_commands {
        if let Err(e) = evaluator.eval_string(&command) {
            eprintln!("Error executing startup command '{}': {}", command, e);
            std::process::exit(1);
        }
    }
    
    if args.quit {
        return;
    }
    
    // Drop into the REPL
    repl(&mut evaluator);
}



fn repl(evaluator: &mut Evaluator) {
    use crate::gc::SchemeValueSimple;
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
            SchemeValueSimple::Nil => {
                // Port stack is empty: exit cleanly
                break;
            }
            SchemeValueSimple::Pair(car, _cdr) => {
                // car should be a port object
                let port_val = &car.value;
                let mut port = match port_val {
                    SchemeValueSimple::Port { kind } => Port { kind: kind.clone() },
                    _ => {
                        println!("Error: car of **port-stack** is not a port");
                        break;
                    }
                };
                interactive = matches!(port.kind, PortKind::Stdin);
                if interactive {
                    print!("s1> ");
                    stdio::stdout().flush().unwrap();
                }
                let parse_result = parse_and_deduplicate(&mut parser, &mut port, evaluator.heap_mut());
                match parse_result {
                    Ok(expr) => {
                        match eval_logic(expr, evaluator) {
                            Ok(result) => {
                                if interactive {
                                    println!("=> {}", print_scheme_value(&result.value));
                                }
                            }
                            Err(e) => println!("Evaluation error: {}", e),
                        }
                    }
                    Err(e) if e.contains("end of input") => {
                        // Pop the port from **port-stack**
                        // (Scheme code should provide a pop-port! function, but for now, we can do it here)
                        // Get cdr of port-stack and set it
                        match &port_stack_val.value {
                            SchemeValueSimple::Pair(_car, cdr) => {
                                evaluator.env_mut().set_symbol(port_stack_sym, *cdr);
                            }
                            _ => {
                                println!("Error: **port-stack** is not a proper list");
                                break;
                            }
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
                                    SchemeValueSimple::Pair(_car, cdr) => {
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
                                pos: 0,
                            };
                        } else {
                            println!("Parse error: {}", e);
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
