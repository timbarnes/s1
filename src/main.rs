mod gc;
mod io;
mod tokenizer;
mod parser;
mod eval;
mod evalsimple;
mod builtin;

use crate::gc::GcHeap;
use crate::parser::ParserSimple;
use crate::eval::EvaluatorSimple;
use crate::builtin::register_all_simple;
use crate::io::{Port, PortKind};
use crate::evalsimple::{Evaluator, eval_logic};
use argh::FromArgs;
use num_bigint::BigInt;
use std::io as stdio;
use stdio::Write;
use std::fs::File;
use std::io::BufReader;
use std::io::Read;
use crate::io::PortStack;

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
    let mut heap = GcHeap::new();
    let mut evaluator = Evaluator::new();
    register_all_simple(&mut heap, evaluator.env_mut().bindings_mut());

    // File loading uses a port stack
    let mut port_stack = PortStack::new(Port {
        kind: PortKind::Stdin,
    });
    let mut parser = ParserSimple::new();

    // Load core file unless --no-core
    if !args.no_core {
        if let Err(e) = load_scheme_file("scheme/s1-core.scm", &mut heap, &mut port_stack) {
            eprintln!("Error loading core: {}", e);
            std::process::exit(1);
        }
    }
    // Load each file in order
    for filename in &args.file {
        if let Err(e) = load_scheme_file(filename, &mut heap, &mut port_stack) {
            eprintln!("Error loading {}: {}", filename, e);
            std::process::exit(1);
        }
    }
    if args.quit {
        return;
    }
    repl(&mut heap, &mut evaluator, &mut port_stack, &mut parser);
}

fn load_scheme_file(
    filename: &str,
    heap: &mut GcHeap,
    port_stack: &mut PortStack,
) -> Result<(), String> {
    let file = File::open(filename).map_err(|e| format!("could not open {}: {}", filename, e))?;
    let mut content = String::new();
    BufReader::new(file).read_to_string(&mut content).map_err(|e| format!("could not read {}: {}", filename, e))?;
    port_stack.push(Port {
        kind: PortKind::StringPortInput {
            content,
            pos: 0,
        },
    });
    Ok(())
}

fn repl(
    heap: &mut GcHeap,
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
        if matches!(port_stack.current().kind, PortKind::Stdin) {
            print!("s1> ");
            stdio::stdout().flush().unwrap();
            let mut input = String::new();
            if stdin.read_line(&mut input).unwrap() == 0 {
                // EOF on stdin
                break;
            }
            if input.trim().is_empty() { continue; }
            port_stack.current_mut().kind = PortKind::StringPortInput {
                content: input.clone(),
                pos: 0,
            };
        }
        let parse_result = parser.parse(heap, port_stack.current_mut());
        match parse_result {
            Ok(expr) => {
                match eval_logic(expr, evaluator) {
                    Ok(result) => {
                        println!("=> {}", print_scheme_value(&result.value));
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
            Err(e) => println!("Parse error: {}", e),
        }
    }
}
