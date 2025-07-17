mod gc;
mod io;
mod tokenizer;
mod parser;
mod eval;
mod printer;
mod builtin;

use gc::GcHeap;
use crate::io::{Port, PortKind, PortStack, FileTable};
use tokenizer::Tokenizer;
use parser::Parser;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use builtin::BuiltinKind;

fn main() {
    // Set up GC heap, stdin port, port stack, file table
    let heap = Rc::new(RefCell::new(GcHeap::new()));
    let stdin_port = Port { kind: PortKind::Stdin };
    let port_stack = Rc::new(RefCell::new(PortStack::new(stdin_port)));
    let file_table = Rc::new(RefCell::new(FileTable::new()));

    // Set up tokenizer and parser
    let tokenizer = Tokenizer::new(
        port_stack.clone(),
        file_table.clone(),
    );
    let parser = Rc::new(RefCell::new(Parser::new(heap.clone(), tokenizer)));

    // Set up global environment (for now, just a single HashMap)
    let mut env = HashMap::<String, BuiltinKind>::new();
    builtin::register_all(&mut heap.borrow_mut(), &mut env);

    // Call the REPL loop in eval.rs
    eval::repl(heap, port_stack, file_table, parser, env);
}
