mod gc;
mod io;
mod tokenizer;
mod parser;
mod eval;
mod printer;
mod builtin;

use gc::GcHeap;
use crate::io::{Port, PortKind, PortStack, FileTable};
use parser::Parser;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use builtin::BuiltinKind;
use argh::FromArgs;

/// Scheme interpreter command-line options
#[derive(FromArgs, Debug)]
struct Cli {
    /// do not enter the REPL after loading files
    #[argh(switch)]
    no_repl: bool,

    /// path to core Scheme file to load at startup
    #[argh(option, short = 'c', default = "String::from(\"core.scm\")")]
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
    println!("Parsed command-line options: {:?}", cli);

    // If --no-repl is given and no core or load file is provided, quit with a message
    let no_files = (cli.core == "core.scm" || cli.core.is_empty()) && cli.load.is_empty() && cli.script.is_none();
    if cli.no_repl && no_files {
        println!("Nothing to do: --no-repl given and no core, load, or script file specified.");
        std::process::exit(0);
    }
    
    // Set up GC heap, stdin port, port stack, file table
    let heap = Rc::new(RefCell::new(GcHeap::new()));
    let stdin_port = Port { kind: PortKind::Stdin };
    let port_stack = Rc::new(RefCell::new(PortStack::new(stdin_port)));
    let file_table = Rc::new(RefCell::new(FileTable::new()));

    // Set up parser with current port (tokenizer is now internal to parser)
    let current_port = Rc::new(RefCell::new(port_stack.borrow().current().clone()));
    let parser = Rc::new(RefCell::new(Parser::new(heap.clone(), current_port, file_table.clone())));

    // Set up global environment (for now, just a single HashMap)
    let mut env = HashMap::<String, BuiltinKind>::new();
    builtin::register_all(&mut heap.borrow_mut(), &mut env);

    // Load core file if it exists
    if cli.core != "core.scm" || std::path::Path::new(&cli.core).exists() {
        match eval::load_file(&cli.core, &heap, &port_stack, &file_table, &parser, &mut env) {
            Ok(()) => println!("Loaded core file: {}", cli.core),
            Err(e) => {
                eprintln!("Error loading core file '{}': {}", cli.core, e);
                if cli.no_repl {
                    std::process::exit(1);
                }
            }
        }
    }

    // Load additional files specified with -l
    for filename in &cli.load {
        match eval::load_file(filename, &heap, &port_stack, &file_table, &parser, &mut env) {
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
        match eval::load_file(&script, &heap, &port_stack, &file_table, &parser, &mut env) {
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
    eval::repl(heap, port_stack, file_table, parser, env);
}
