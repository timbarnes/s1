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

    // Set up parser (tokenizer is now internal to parser)
    let parser = Rc::new(RefCell::new(Parser::new(heap.clone(), port_stack.clone(), file_table.clone())));

    // Set up global environment (for now, just a single HashMap)
    let mut env = HashMap::<String, BuiltinKind>::new();
    builtin::register_all(&mut heap.borrow_mut(), &mut env);

    // Call the REPL loop in eval.rs
    eval::repl(heap, port_stack, file_table, parser, env);
}
