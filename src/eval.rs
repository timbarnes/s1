use crate::gc::{GcHeap, SchemeValue};
use crate::io::{PortStack, FileTable};
use crate::parser::Parser;
use crate::printer::scheme_display;
use std::rc::Rc;
use std::cell::RefCell;

fn is_self_evaluating(expr: &SchemeValue) -> bool {
    matches!(expr,
        SchemeValue::Int(_)
        | SchemeValue::Float(_)
        | SchemeValue::Bool(_)
        | SchemeValue::Char(_)
        | SchemeValue::Str(_)
        | SchemeValue::Nil
        | SchemeValue::Vector(_)
    )
}

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
            Ok(expr) => {
                let value = expr.borrow().value.clone();
                if is_self_evaluating(&value) {
                    println!("=> {}", scheme_display(&value));
                } else {
                    println!("Error: cannot evaluate non-self-evaluating form: {}", scheme_display(&value));
                }
            }
            Err(e) if e.contains("end of input") => break, // EOF: exit
            Err(e) => println!("Error: {}", e),
        }
    }
} 