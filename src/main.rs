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
    
    // Register some basic builtins for testing
    register_simple_builtins(evaluator);
    
    loop {
        print!("s1> ");
        use std::io::Write;
        std::io::stdout().flush().unwrap();
        
        // Read a line from stdin
        let mut input = String::new();
        match std::io::stdin().read_line(&mut input) {
            Ok(0) => {
                // EOF (Ctrl+D)
                println!();
                break;
            }
            Ok(_) => {
                // Process the input
                let trimmed = input.trim();
                if trimmed.is_empty() {
                    continue;
                }
                
                // For now, handle simple expressions manually
                match handle_simple_expression(trimmed, evaluator) {
                    Ok(result) => {
                        println!("=> {}", crate::printer::scheme_display(&result.value));
                    }
                    Err(e) => {
                        eprintln!("Error: {}", e);
                    }
                }
            }
            Err(e) => {
                eprintln!("Error reading input: {}", e);
                break;
            }
        }
    }
}

/// Register basic builtins in the simple evaluator environment
fn register_simple_builtins(evaluator: &mut EvaluatorSimple) {
    // For now, we'll create simple wrapper functions that can be called
    // This is a temporary solution until we have proper primitive objects
    
    // Create all the string values first
    let plus_str = crate::gc::new_string_simple(&mut evaluator.heap, "+");
    let minus_str = crate::gc::new_string_simple(&mut evaluator.heap, "-");
    let times_str = crate::gc::new_string_simple(&mut evaluator.heap, "*");
    let div_str = crate::gc::new_string_simple(&mut evaluator.heap, "/");
    let number_q_str = crate::gc::new_string_simple(&mut evaluator.heap, "number?");
    let type_of_str = crate::gc::new_string_simple(&mut evaluator.heap, "type-of");
    let display_str = crate::gc::new_string_simple(&mut evaluator.heap, "display");
    let newline_str = crate::gc::new_string_simple(&mut evaluator.heap, "newline");
    let quit_str = crate::gc::new_string_simple(&mut evaluator.heap, "quit");
    
    // Then register them all
    evaluator.insert_global_binding("+".to_string(), plus_str);
    evaluator.insert_global_binding("-".to_string(), minus_str);
    evaluator.insert_global_binding("*".to_string(), times_str);
    evaluator.insert_global_binding("/".to_string(), div_str);
    evaluator.insert_global_binding("number?".to_string(), number_q_str);
    evaluator.insert_global_binding("type-of".to_string(), type_of_str);
    evaluator.insert_global_binding("display".to_string(), display_str);
    evaluator.insert_global_binding("newline".to_string(), newline_str);
    evaluator.insert_global_binding("quit".to_string(), quit_str);
}

/// Handle simple expressions in the simple REPL
/// This is a basic implementation that can handle simple builtin calls
fn handle_simple_expression(input: &str, evaluator: &mut EvaluatorSimple) -> Result<crate::gc::GcRefSimple, String> {
    let input = input.trim();
    
    // Handle simple values first (numbers, strings, etc.)
    if let Ok(n) = input.parse::<i64>() {
        return Ok(crate::gc::new_int_simple(&mut evaluator.heap, num_bigint::BigInt::from(n)));
    }
    
    // Handle string literals (quoted strings)
    if input.starts_with('"') && input.ends_with('"') {
        let content = &input[1..input.len()-1];
        return Ok(crate::gc::new_string_simple(&mut evaluator.heap, content));
    }
    
    // Handle booleans
    if input == "#t" {
        return Ok(crate::gc::new_bool_simple(&mut evaluator.heap, true));
    }
    if input == "#f" {
        return Ok(crate::gc::new_bool_simple(&mut evaluator.heap, false));
    }
    
    // Handle function calls (parenthesized expressions)
    if input.starts_with('(') && input.ends_with(')') {
        let content = &input[1..input.len()-1];
        let parts: Vec<&str> = content.split_whitespace().collect();
        
        if parts.is_empty() {
            return Err("Empty expression".to_string());
        }
        
        let func_name = parts[0];
        let args = &parts[1..];
        
        // Parse arguments based on the function
        match func_name {
            "+" => {
                let mut numbers = Vec::new();
                for arg in args {
                    match arg.parse::<i64>() {
                        Ok(n) => {
                            numbers.push(crate::gc::new_int_simple(&mut evaluator.heap, num_bigint::BigInt::from(n)));
                        }
                        Err(_) => return Err(format!("Invalid number: {}", arg)),
                    }
                }
                crate::builtin::number::plus_builtin_simple(&mut evaluator.heap, &numbers)
            }
            "-" => {
                let mut numbers = Vec::new();
                for arg in args {
                    match arg.parse::<i64>() {
                        Ok(n) => {
                            numbers.push(crate::gc::new_int_simple(&mut evaluator.heap, num_bigint::BigInt::from(n)));
                        }
                        Err(_) => return Err(format!("Invalid number: {}", arg)),
                    }
                }
                crate::builtin::number::minus_builtin_simple(&mut evaluator.heap, &numbers)
            }
            "*" => {
                let mut numbers = Vec::new();
                for arg in args {
                    match arg.parse::<i64>() {
                        Ok(n) => {
                            numbers.push(crate::gc::new_int_simple(&mut evaluator.heap, num_bigint::BigInt::from(n)));
                        }
                        Err(_) => return Err(format!("Invalid number: {}", arg)),
                    }
                }
                crate::builtin::number::times_builtin_simple(&mut evaluator.heap, &numbers)
            }
            "/" => {
                let mut numbers = Vec::new();
                for arg in args {
                    match arg.parse::<i64>() {
                        Ok(n) => {
                            numbers.push(crate::gc::new_int_simple(&mut evaluator.heap, num_bigint::BigInt::from(n)));
                        }
                        Err(_) => return Err(format!("Invalid number: {}", arg)),
                    }
                }
                crate::builtin::number::div_builtin_simple(&mut evaluator.heap, &numbers)
            }
            "number?" => {
                if args.len() != 1 {
                    return Err("number? expects exactly 1 argument".to_string());
                }
                let arg = args[0];
                match arg.parse::<i64>() {
                    Ok(_) => {
                        let num = crate::gc::new_int_simple(&mut evaluator.heap, num_bigint::BigInt::from(1));
                        let test_args = vec![num];
                        crate::builtin::predicate::number_q_simple(&mut evaluator.heap, &test_args)
                    }
                    Err(_) => {
                        let str_val = crate::gc::new_string_simple(&mut evaluator.heap, arg);
                        let test_args = vec![str_val];
                        crate::builtin::predicate::number_q_simple(&mut evaluator.heap, &test_args)
                    }
                }
            }
            "type-of" => {
                if args.len() != 1 {
                    return Err("type-of expects exactly 1 argument".to_string());
                }
                let arg = args[0];
                // Try to parse as number first
                if let Ok(_) = arg.parse::<i64>() {
                    let num = crate::gc::new_int_simple(&mut evaluator.heap, num_bigint::BigInt::from(1));
                    let test_args = vec![num];
                    crate::builtin::predicate::type_of_simple(&mut evaluator.heap, &test_args)
                } else {
                    let str_val = crate::gc::new_string_simple(&mut evaluator.heap, arg);
                    let test_args = vec![str_val];
                    crate::builtin::predicate::type_of_simple(&mut evaluator.heap, &test_args)
                }
            }
            "display" => {
                if args.is_empty() {
                    return Err("display expects at least 1 argument".to_string());
                }
                let arg = args[0];
                let str_val = crate::gc::new_string_simple(&mut evaluator.heap, arg);
                let display_args = vec![str_val];
                crate::builtin::display_builtin_simple(&mut evaluator.heap, &display_args)
            }
            "newline" => {
                let empty_args: Vec<crate::gc::GcRefSimple> = vec![];
                crate::builtin::newline_builtin_simple(&mut evaluator.heap, &empty_args)
            }
            "quit" => {
                let empty_args: Vec<crate::gc::GcRefSimple> = vec![];
                crate::builtin::quit_builtin_simple(&mut evaluator.heap, &empty_args)
            }
            _ => {
                Err(format!("Unknown function: {}", func_name))
            }
        }
    } else {
        // Try to look up as a symbol in the environment
        if let Some(value) = evaluator.lookup_global_binding(input) {
            Ok(value)
        } else {
            Err(format!("Unknown expression: {}", input))
        }
    }
}
