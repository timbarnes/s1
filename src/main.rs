#![allow(dead_code)]
mod gc;
mod io;
mod tokenizer;
mod parser;
mod eval;
mod evalsimple;
mod builtin;
mod env;

use crate::parser::ParserSimple;
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
    let mut port_stack = PortStack::new(Port {
        kind: PortKind::Stdin,
    });
    let mut parser = ParserSimple::new();
    repl(&mut evaluator, &mut port_stack, &mut parser);
}



fn repl(
    evaluator: &mut Evaluator,
    port_stack: &mut PortStack,
    parser: &mut ParserSimple,
) {
    let stdin = stdio::stdin();
    let mut interactive = matches!(port_stack.current().kind, PortKind::Stdin);
    if interactive {
        println!("Welcome to the Scheme REPL (EvaluatorSimple, new GC)");
    }
    loop {
        // Prompt if we're on stdin and about to read
        if matches!(port_stack.current().kind, PortKind::Stdin) {
            print!("s1> ");
            stdio::stdout().flush().unwrap();
        }
        
        let parse_result = parse_and_deduplicate(parser, port_stack.current_mut(), evaluator.heap_mut());
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
                if !port_stack.pop() {
                    break;
                }
                // If we pop to stdin, set interactive mode
                interactive = matches!(port_stack.current().kind, PortKind::Stdin);
            }
            Err(e) => {
                // For stdin, read input and create a new port
                if matches!(port_stack.current().kind, PortKind::Stdin) {
                    let mut input = String::new();
                    if stdin.read_line(&mut input).unwrap() == 0 {
                        // EOF on stdin
                        break;
                    }
                    if input.trim().is_empty() { continue; }
                    // Replace the current port with the new input
                    port_stack.current_mut().kind = PortKind::StringPortInput {
                        content: input,
                        pos: 0,
                    };
                } else {
                    println!("Parse error: {}", e);
                }
            }
        }
    }
}
