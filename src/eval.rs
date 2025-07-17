use crate::gc::{GcHeap, SchemeValue, GcRef};
use crate::io::{PortStack, FileTable, Port, PortKind};
use crate::parser::Parser;
use crate::printer::scheme_display;
use crate::builtin::BuiltinKind;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;

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

/// Load and evaluate a Scheme file.
///
/// This function reads the file, parses all expressions, and evaluates them
/// in the given environment. If any expression fails to parse or evaluate,
/// an error is returned.
///
/// # Arguments
///
/// * `filename` - The path to the Scheme file to load
/// * `heap` - The garbage-collected heap
/// * `port_stack` - The port stack for I/O operations
/// * `file_table` - The file table for managing file handles
/// * `parser` - The parser to use for parsing expressions
/// * `env` - The environment containing builtins and user bindings
///
/// # Returns
///
/// Returns `Ok(())` if the file was loaded successfully, or `Err(String)` if
/// an error occurred during loading, parsing, or evaluation.
pub fn load_file(
    filename: &str,
    heap: &Rc<RefCell<GcHeap>>,
    port_stack: &Rc<RefCell<PortStack>>,
    file_table: &Rc<RefCell<FileTable>>,
    parser: &Rc<RefCell<Parser>>,
    env: &mut HashMap<String, BuiltinKind>,
) -> Result<(), String> {
    // Read the file content
    let content = fs::read_to_string(filename)
        .map_err(|e| format!("Failed to read file '{}': {}", filename, e))?;
    
    // Create a string port for the file content
    let file_port = Port {
        kind: PortKind::StringPortInput {
            content,
            pos: 0,
        }
    };
    
    // Push the file port onto the port stack
    port_stack.borrow_mut().push(file_port);
    
    // Parse and evaluate all expressions in the file using the existing parser
    loop {
        let parse_result = parser.borrow_mut().parse();
        match parse_result {
            Ok(expr) => {
                // Evaluate the expression
                let eval_result = eval_expr(expr, heap, env);
                match eval_result {
                    Ok(_) => {
                        // Expression evaluated successfully, continue to next
                    }
                    Err(e) => {
                        // Pop the file port and return the error
                        port_stack.borrow_mut().pop();
                        return Err(format!("Evaluation error in '{}': {}", filename, e));
                    }
                }
            }
            Err(e) if e.contains("end of input") => {
                // End of file reached, success
                break;
            }
            Err(e) => {
                // Parse error
                port_stack.borrow_mut().pop();
                return Err(format!("Parse error in '{}': {}", filename, e));
            }
        }
    }
    
    // Pop the file port from the stack
    port_stack.borrow_mut().pop();
    
    Ok(())
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
                                    // Evaluate arguments recursively
                                    let mut evaled_args = Vec::new();
                                    let mut cur = cdr.clone();
                                    loop {
                                        let next = {
                                            let cur_borrow = cur.borrow();
                                            match &cur_borrow.value {
                                                SchemeValue::Pair(arg, next) => {
                                                    // Recursively evaluate each argument
                                                    let evaled = {
                                                        // Evaluate the argument recursively
                                                        eval_expr(arg.clone(), &heap, &mut env)
                                                    };
                                                    match evaled {
                                                        Ok(val) => evaled_args.push(val),
                                                        Err(e) => {
                                                            println!("Error: {}", e);
                                                            return;
                                                        }
                                                    }
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

fn eval_expr(expr: GcRef, heap: &Rc<RefCell<GcHeap>>, env: &mut HashMap<String, BuiltinKind>) -> Result<GcRef, String> {
    let value = expr.borrow().value.clone();
    if is_self_evaluating(&value) {
        Ok(expr)
    } else if let SchemeValue::Pair(car, cdr) = &value {
        if let SchemeValue::Symbol(ref name) = car.borrow().value {
            if let Some(builtin) = env.get(name).cloned() {
                match builtin {
                    BuiltinKind::SpecialForm(f) => {
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
                                    _ => return Err("Malformed argument list".to_string()),
                                }
                            };
                            if let Some(next_cdr) = next {
                                cur = next_cdr;
                            } else {
                                break;
                            }
                        }
                        if !matches!(&cur.borrow().value, SchemeValue::Nil) {
                            return Err("Malformed argument list".to_string());
                        }
                        f(&mut *heap.borrow_mut(), &args, env)
                    }
                    BuiltinKind::Normal(f) => {
                        // Recursively evaluate arguments
                        let mut evaled_args = Vec::new();
                        let mut cur = cdr.clone();
                        loop {
                            let next = {
                                let cur_borrow = cur.borrow();
                                match &cur_borrow.value {
                                    SchemeValue::Pair(arg, next) => {
                                        let evaled = eval_expr(arg.clone(), heap, env)?;
                                        evaled_args.push(evaled);
                                        Some(next.clone())
                                    }
                                    SchemeValue::Nil => None,
                                    _ => return Err("Malformed argument list".to_string()),
                                }
                            };
                            if let Some(next_cdr) = next {
                                cur = next_cdr;
                            } else {
                                break;
                            }
                        }
                        if !matches!(&cur.borrow().value, SchemeValue::Nil) {
                            return Err("Malformed argument list".to_string());
                        }
                        f(&mut *heap.borrow_mut(), &evaled_args)
                    }
                }
            } else {
                Err(format!("not a builtin: {}", name))
            }
        } else {
            Err("cannot evaluate non-symbol operator".to_string())
        }
    } else {
        Err("cannot evaluate form".to_string())
    }
} 