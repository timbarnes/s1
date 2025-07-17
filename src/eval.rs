use crate::gc::{GcHeap, SchemeValue, GcRef};
use crate::io::{PortStack, FileTable};
use crate::parser::Parser;
use crate::printer::scheme_display;
use crate::builtin::BuiltinKind;
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
    mut env: HashMap<String, BuiltinKind>,
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
                    println!("Error: variable lookup not implemented for BuiltinKind env: {}", name);
                } else if let SchemeValue::Pair(car, cdr) = &value {
                    // Procedure call: (symbol ...)
                    if let SchemeValue::Symbol(ref name) = car.borrow().value {
                        if let Some(builtin) = env.get(name).cloned() {
                            match builtin {
                                BuiltinKind::SpecialForm(f) => {
                                    // Pass unevaluated args and env
                                    // Collect args as a slice
                                    let mut args = Vec::new();
                                    let mut cur = cdr.clone();
                                    loop {
                                        let next = {
                                            let cur_borrow = cur.borrow();
                                            match &cur_borrow.value {
                                                SchemeValue::Pair(arg, next) => {
                                                    args.push(arg.clone());
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
                                    match f(&mut *heap.borrow_mut(), &args, &mut env) {
                                        Ok(result) => println!("=> {}", scheme_display(&result.borrow().value)),
                                        Err(e) => println!("Error: {}", e),
                                    }
                                    continue;
                                }
                                BuiltinKind::Normal(f) => {
                                    // Evaluate arguments
                                    let mut evaled_args = Vec::new();
                                    let mut cur = cdr.clone();
                                    loop {
                                        let next = {
                                            let cur_borrow = cur.borrow();
                                            match &cur_borrow.value {
                                                SchemeValue::Pair(arg, next) => {
                                                    // Recursively evaluate each argument
                                                    let evaled = {
                                                        // Evaluate the argument
                                                        // For now, only self-evaluating and symbols are supported
                                                        let arg_val = arg.borrow().value.clone();
                                                        if is_self_evaluating(&arg_val) {
                                                            arg.clone()
                                                        } else {
                                                            println!("Error: argument evaluation not fully implemented");
                                                            arg.clone()
                                                        }
                                                    };
                                                    evaled_args.push(evaled);
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
                                    match f(&mut *heap.borrow_mut(), &evaled_args) {
                                        Ok(result) => println!("=> {}", scheme_display(&result.borrow().value)),
                                        Err(e) => println!("Error: {}", e),
                                    }
                                    continue;
                                }
                            }
                        } else {
                            println!("Error: not a builtin: {}", name);
                        }
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