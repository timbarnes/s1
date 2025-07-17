use crate::gc::GcHeap;
use crate::io::{PortStack, FileTable};
use crate::parser::Parser;
use std::rc::Rc;
use std::cell::RefCell;

pub fn repl(
    heap: Rc<RefCell<GcHeap>>,
    port_stack: Rc<RefCell<PortStack>>,
    file_table: Rc<RefCell<FileTable>>,
    parser: Rc<RefCell<Parser>>,
) {
    loop {
        print!("s1> ");
        use std::io::Write;
        std::io::stdout().flush().unwrap();
        let result = parser.borrow_mut().parse();
        match result {
            Ok(expr) => println!("{:?}", expr), // Replace with pretty-printer later
            Err(e) if e.contains("end of input") => break, // EOF: exit
            Err(e) => println!("Error: {}", e),
        }
    }
} 