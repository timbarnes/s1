use crate::gc::{GcHeap, SchemeValue, GcRef};
use crate::io::{PortStack, FileTable};
use crate::parser::Parser;
use crate::printer::scheme_display;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

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
    mut env: HashMap<String, GcRef>,
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
                } else if let SchemeValue::Symbol(ref name) = value {
                    // Variable lookup
                    if let Some(val) = env.get(name) {
                        println!("=> {}", scheme_display(&val.borrow().value));
                    } else {
                        println!("Error: unbound variable: {}", name);
                    }
                } else if let SchemeValue::Pair(car, cdr) = &value {
                    // Procedure call: (symbol ...)
                    if let SchemeValue::Symbol(ref name) = car.borrow().value {
                        if let Some(proc_ref) = env.get(name) {
                            if let SchemeValue::Primitive { func, .. } = &proc_ref.borrow().value {
                                // Evaluate arguments
                                let mut args = Vec::new();
                                let mut cur = cdr.clone();
                                loop {
                                    let next = {
                                        let cur_borrow = cur.borrow();
                                        match &cur_borrow.value {
                                            SchemeValue::Pair(arg, next) => {
                                                args.push(arg.clone()); // For now, don't recursively eval args
                                                Some(next.clone())
                                            }
                                            SchemeValue::Nil => None,
                                            _ => {
                                                println!("Error: malformed argument list");
                                                return;
                                            }
                                        }
                                    };
                                    if let Some(next_cdr) = next {
                                        cur = next_cdr;
                                    } else {
                                        break;
                                    }
                                }
                                if !matches!(&cur.borrow().value, SchemeValue::Nil) {
                                    println!("Error: malformed argument list");
                                    continue;
                                }
                                match func(&mut *heap.borrow_mut(), &args) {
                                    Ok(result) => println!("=> {}", scheme_display(&result.borrow().value)),
                                    Err(e) => println!("Error: {}", e),
                                }
                                continue;
                            }
                        }
                        println!("Error: not a builtin: {}", name);
                    } else {
                        println!("Error: cannot evaluate non-symbol operator");
                    }
                } else {
                    println!("Error: cannot evaluate form: {}", scheme_display(&value));
                }
            }
            Err(e) if e.contains("end of input") => break, // EOF: exit
            Err(e) => println!("Error: {}", e),
        }
    }
} 