mod gc;
mod io;
mod tokenizer;
mod parser;
mod eval;
mod printer;
mod builtin;

use crate::io::{Port, PortKind, PortStack};
use parser::Parser;
use parser::ParserSimple;
use argh::FromArgs;
use crate::eval::Evaluator;
use crate::eval::EvaluatorSimple;

/// Scheme interpreter command-line options
#[derive(FromArgs, Debug)]
struct Cli {
    /// do not enter the REPL after loading files
    #[argh(switch)]
    no_repl: bool,

    /// use the simple reference-based GC system
    #[argh(switch)]
    simple: bool,

    /// path to core Scheme file to load at startup
    #[argh(option, short = 'c', default = "String::from(\"scheme/s1-core.scm\")")]
    core: String,

    /// additional Scheme files to load (can be repeated)
    #[argh(option, short = 'l')]
    load: Vec<String>,

    /// script file to run (positional, optional)
    #[argh(positional)]
    script: Option<String>,
}

fn main() {
    let cli: Cli = argh::from_env();
    
    // Choose between regular and simple modes
    if cli.simple {
        main_simple();
        return;
    }
    
    println!("Regular mode - Parsed command-line options: {:?}", cli);

    // If --no-repl is given and no core or load file is provided, quit with a message
    let no_files = (cli.core == "core.scm" || cli.core.is_empty()) && cli.load.is_empty() && cli.script.is_none();
    if cli.no_repl && no_files {
        println!("Nothing to do: --no-repl given and no core, load, or script file specified.");
        std::process::exit(0);
    }
    
    // Set up stdin port, port stack, file table
    let stdin_port = Port { kind: PortKind::Stdin };
    let mut port_stack = PortStack::new(stdin_port);
    // let mut file_table = FileTable::new();
    let mut parser = Parser::new();

    // Set up evaluator with builtins registered
    let mut evaluator = Evaluator::new();
    builtin::register_all(&mut evaluator.heap, &mut evaluator.env);

    // Load core file if it exists
    // Always try to load the core file, then fall through to REPL
    match eval::load_file(&cli.core, &mut port_stack, &mut parser, &mut evaluator) {
        Ok(()) => println!("Loaded core file: {}", cli.core),
        Err(e) => {
            eprintln!("Error loading core file '{}': {}", cli.core, e);
            if cli.no_repl {
                std::process::exit(1);
            }
            // If --no-repl is not specified, continue to REPL even if core file fails
        }
    }

    // Load additional files specified with -l
    for filename in &cli.load {
        match eval::load_file(filename, &mut port_stack, &mut parser, &mut evaluator) {
            Ok(()) => println!("Loaded file: {}", filename),
            Err(e) => {
                eprintln!("Error loading file '{}': {}", filename, e);
                if cli.no_repl {
                    std::process::exit(1);
                }
            }
        }
    }

    // Handle script file if provided
    if let Some(script) = cli.script {
        match eval::load_file(&script, &mut port_stack, &mut parser, &mut evaluator) {
            Ok(()) => println!("Executed script: {}", script),
            Err(e) => {
                eprintln!("Error executing script '{}': {}", script, e);
                std::process::exit(1);
            }
        }
        
        // If --no-repl is specified, exit after running the script
        if cli.no_repl {
            return;
        }
    }

    // Call the REPL loop in eval.rs
    eval::repl(&mut port_stack, &mut parser, &mut evaluator);
}

/// Simple main function that uses the reference-based GC system
/// This is an alternative implementation that demonstrates the simple components
fn main_simple() {
    let cli: Cli = argh::from_env();
    println!("Simple mode - Parsed command-line options: {:?}", cli);

    // If --no-repl is given and no core or load file is provided, quit with a message
    let no_files = (cli.core == "core.scm" || cli.core.is_empty()) && cli.load.is_empty() && cli.script.is_none();
    if cli.no_repl && no_files {
        println!("Nothing to do: --no-repl given and no core, load, or script file specified.");
        std::process::exit(0);
    }
    
    // Set up stdin port, port stack
    let stdin_port = Port { kind: PortKind::Stdin };
    let mut port_stack = PortStack::new(stdin_port);
    let mut parser = ParserSimple::new();

    // Set up simple evaluator with builtins registered
    let mut evaluator = EvaluatorSimple::new();
    // TODO: Add simple builtin registration when available
    // builtin::register_all_simple(&mut evaluator.heap, &mut evaluator.env);

    // Load core file if it exists
    // Always try to load the core file, then fall through to REPL
    match load_file_simple(&cli.core, &mut port_stack, &mut parser, &mut evaluator) {
        Ok(()) => println!("Loaded core file (simple): {}", cli.core),
        Err(e) => {
            eprintln!("Error loading core file '{}': {}", cli.core, e);
            if cli.no_repl {
                std::process::exit(1);
            }
            // If --no-repl is not specified, continue to REPL even if core file fails
        }
    }

    // Load additional files specified with -l
    for filename in &cli.load {
        match load_file_simple(filename, &mut port_stack, &mut parser, &mut evaluator) {
            Ok(()) => println!("Loaded file (simple): {}", filename),
            Err(e) => {
                eprintln!("Error loading file '{}': {}", filename, e);
                if cli.no_repl {
                    std::process::exit(1);
                }
            }
        }
    }

    // Handle script file if provided
    if let Some(script) = cli.script {
        match load_file_simple(&script, &mut port_stack, &mut parser, &mut evaluator) {
            Ok(()) => println!("Executed script (simple): {}", script),
            Err(e) => {
                eprintln!("Error executing script '{}': {}", script, e);
                std::process::exit(1);
            }
        }
        
        // If --no-repl is specified, exit after running the script
        if cli.no_repl {
            return;
        }
    }

    // Call the simple REPL loop
    repl_simple(&mut port_stack, &mut parser, &mut evaluator);
}

/// Simple file loading function using the reference-based GC system
fn load_file_simple(filename: &str, port_stack: &mut PortStack, parser: &mut ParserSimple, evaluator: &mut EvaluatorSimple) -> Result<(), String> {
    // Create a file port for the given filename
    let file_port = Port { kind: PortKind::File { name: filename.to_string(), write: false, file_id: None } };
    port_stack.push(file_port);
    
    // Parse and evaluate expressions from the file
    loop {
        let parse_result = parser.parse(&mut evaluator.heap, port_stack.current_mut());
        match parse_result {
            Ok(expr) => {
                let eval_result = evaluator.evaluate(expr);
                match eval_result {
                    Ok(_) => {
                        // Successfully evaluated, continue to next expression
                    }
                    Err(e) => {
                        eprintln!("Evaluation error: {}", e);
                        // Continue to next expression on error
                    }
                }
            }
            Err(e) => {
                if e.contains("EOF") {
                    // End of file reached
                    break;
                } else {
                    return Err(e);
                }
            }
        }
    }
    
    port_stack.pop();
    Ok(())
}

/// Simple REPL function using the reference-based GC system
fn repl_simple(port_stack: &mut PortStack, parser: &mut ParserSimple, evaluator: &mut EvaluatorSimple) {
    println!("Simple s1 REPL (reference-based GC)");
    println!("Type expressions to evaluate, or Ctrl+C to exit.");
    
    // For now, just demonstrate that the simple components work
    // by evaluating a simple expression directly
    println!("Simple mode active - demonstrating reference-based GC components");
    
    // Create a simple test expression: (+ 1 2)
    let arg1 = crate::gc::new_int_simple(&mut evaluator.heap, num_bigint::BigInt::from(1));
    let arg2 = crate::gc::new_int_simple(&mut evaluator.heap, num_bigint::BigInt::from(2));
    let args = vec![arg1, arg2];
    
    // Test the simple plus builtin
    match crate::builtin::number::plus_builtin_simple(&mut evaluator.heap, &args) {
        Ok(result) => {
            println!("Simple plus test: (+ 1 2) => {}", crate::printer::scheme_display(&result.value));
        }
        Err(e) => {
            eprintln!("Simple plus test error: {}", e);
        }
    }
    
    // Test the simple number predicate
    let test_arg = vec![arg1];
    match crate::builtin::predicate::number_q_simple(&mut evaluator.heap, &test_arg) {
        Ok(result) => {
            println!("Simple number? test: (number? 1) => {}", crate::printer::scheme_display(&result.value));
        }
        Err(e) => {
            eprintln!("Simple number? test error: {}", e);
        }
    }
    
    println!("Simple mode demonstration complete");
}
