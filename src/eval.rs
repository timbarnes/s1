use crate::gc::{GcHeap, GcRef, SchemeValue, is_nil, as_pair, as_symbol};
use crate::io::{PortStack, FileTable, Port, PortKind};
use crate::parser::Parser;
use crate::printer::scheme_display;
use crate::builtin::BuiltinKind;
// use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;

/// Represents the result of evaluation - either continue with another expression
/// or return a final value.
#[derive(Debug)]
pub enum EvalResult {
    /// Continue evaluation with the given expression
    Continue(GcRef),
    /// Evaluation is complete with the given value
    Done(GcRef),
}

/// Check if a Scheme value is self-evaluating (doesn't need evaluation).
///
/// Self-evaluating expressions include:
/// - Numbers (integers and floats)
/// - Strings
/// - Booleans
/// - Characters
/// - Vectors
///
/// # Examples
///
/// ```rust
/// use s1::gc::{GcHeap, new_int, new_symbol, new_string};
/// use s1::eval::is_self_evaluating;
///
/// let mut heap = GcHeap::new();
/// let num = new_int(&mut heap, 42);
/// let sym = new_symbol(&mut heap, "x");
/// let str_val = new_string(&mut heap, "hello");
///
/// assert!(is_self_evaluating(&num.borrow().value));
/// assert!(!is_self_evaluating(&sym.borrow().value));
/// assert!(is_self_evaluating(&str_val.borrow().value));
/// ```
fn is_self_evaluating(expr: &SchemeValue) -> bool {
    matches!(expr, 
        SchemeValue::Int(_) | 
        SchemeValue::Float(_) | 
        SchemeValue::Str(_) | 
        SchemeValue::Bool(_) | 
        SchemeValue::Char(_) | 
        SchemeValue::Vector(_) |
        SchemeValue::Nil
    )
}

/// Load and evaluate a Scheme file.
///
/// This function reads the specified file, parses all expressions in it,
/// and evaluates them in sequence. The file is loaded in the context of
/// the provided environment.
///
/// # Arguments
///
/// * `filename` - The path to the Scheme file to load
/// * `heap` - The garbage-collected heap for allocating values
/// * `port_stack` - The port stack for managing input sources
/// * `file_table` - The file table for managing open files
/// * `parser` - The parser for parsing expressions
/// * `env` - The environment containing variable bindings
///
/// # Returns
///
/// Returns `Ok(())` if the file was loaded successfully, or `Err(String)` if
/// an error occurred during loading, parsing, or evaluation.
pub fn load_file(
    filename: &str,
    heap: &mut GcHeap,
    port_stack: &mut PortStack,
    file_table: &mut FileTable,
    parser: &mut Parser,
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
    port_stack.push(file_port);
    
    // No longer needed: parser.update_port
    
    loop {
        let parse_result = {
            let current_port = port_stack.current_mut();
            parser.parse(heap, current_port)
        };
        match parse_result {
            Ok(expr) => {
                let eval_result = eval_trampoline(expr, heap, env);
                match eval_result {
                    Ok(_) => {}
                    Err(e) => {
                        port_stack.pop();
                        return Err(format!("Evaluation error in '{}': {}", filename, e));
                    }
                }
            }
            Err(e) if e.contains("end of input") => break,
            Err(e) => {
                port_stack.pop();
                return Err(format!("Parse error in '{}': {}", filename, e));
            }
        }
    }
    
    // Pop the file port from the stack
    port_stack.pop();   
    Ok(())
}

pub fn repl(
    heap: &mut GcHeap,
    port_stack: &mut PortStack,
    parser: &mut Parser,
    mut env: HashMap<String, BuiltinKind>,
) {
    loop {
        print!("s1> ");
        use std::io::Write;
        std::io::stdout().flush().unwrap();
        let result = {
            let current_port = port_stack.current_mut();
            parser.parse(heap, current_port)
        };
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
                                    match f(heap, &args, &mut env) {
                                        Ok(result) => println!("=> {}", scheme_display(&result.borrow().value)),
                                        Err(e) => println!("Error: {}", e),
                                    }
                                    continue;
                                }
                                BuiltinKind::SpecialFormWithEval(f) => {
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
                                    match f(heap, &args, &mut env) {
                                        Ok(result) => println!("=> {}", scheme_display(&result.borrow().value)),
                                        Err(e) => println!("Error: {}", e),
                                    }
                                    continue;
                                }
                                BuiltinKind::Normal(f) => {
                                    // Evaluate arguments recursively using trampoline
                                    let mut evaled_args = Vec::new();
                                    let mut cur = cdr.clone();
                                    loop {
                                        let next = {
                                            let cur_borrow = cur.borrow();
                                            match &cur_borrow.value {
                                                SchemeValue::Pair(arg, next) => {
                                                    // Recursively evaluate each argument using trampoline
                                                    let evaled = eval_trampoline(arg.clone(), heap, &mut env);
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
                                    match f(heap, &evaled_args) {
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

/// Evaluate an expression using the trampoline pattern for tail recursion optimization.
///
/// This function implements tail recursion optimization by using a trampoline pattern.
/// Instead of making recursive calls that grow the stack, it returns `EvalResult::Continue`
/// for expressions that need further evaluation, and `EvalResult::Done` for final results.
/// The trampoline loop processes these results until completion.
///
/// # Arguments
///
/// * `expr` - The expression to evaluate
/// * `heap` - The garbage-collected heap
/// * `env` - The environment containing variable bindings
///
/// # Returns
///
/// Returns `Ok(GcRef)` for the final value, or `Err(String)` for evaluation errors.
///
/// # Examples
///
/// ```rust
/// use s1::eval::eval_trampoline;
/// use s1::gc::{GcHeap, new_int};
/// use std::rc::Rc;
/// use std::cell::RefCell;
/// use std::collections::HashMap;
///
/// let heap = Rc::new(RefCell::new(GcHeap::new()));
/// let mut env = HashMap::new();
/// let expr = new_int(&mut heap.borrow_mut(), 42);
///
/// let result = eval_trampoline(expr, &heap, &mut env);
/// assert!(result.is_ok());
/// ```
pub fn eval_trampoline(expr: GcRef, heap: &mut GcHeap, env: &mut HashMap<String, BuiltinKind>) -> Result<GcRef, String> {
    let mut current_expr = expr;
    
    loop {
        let result = eval_step(current_expr, heap, env)?;
        match result {
            EvalResult::Continue(next_expr) => {
                current_expr = next_expr;
            }
            EvalResult::Done(value) => {
                return Ok(value);
            }
        }
    }
}

/// Evaluate a single step of an expression.
///
/// This function evaluates one step of an expression and returns either:
/// - `Continue(expr)` if further evaluation is needed
/// - `Done(value)` if evaluation is complete
///
/// This is the core of the trampoline pattern for tail recursion optimization.
fn eval_step(expr: GcRef, heap: &mut GcHeap, env: &mut HashMap<String, BuiltinKind>) -> Result<EvalResult, String> {
    let value = expr.borrow().value.clone();
    
    if is_self_evaluating(&value) {
        Ok(EvalResult::Done(expr))
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
                        
                        // For special forms, we evaluate them directly
                        let result = f(heap, &args, env)?;
                        Ok(EvalResult::Done(result))
                    }
                    BuiltinKind::SpecialFormWithEval(f) => {
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
                        let result = f(heap, &args, env)?;
                        Ok(EvalResult::Done(result))
                    }
                    BuiltinKind::Normal(f) => {
                        // For normal functions, we need to evaluate all arguments first
                        let mut evaled_args = Vec::new();
                        let mut cur = cdr.clone();
                        loop {
                            let next = {
                                let cur_borrow = cur.borrow();
                                match &cur_borrow.value {
                                    SchemeValue::Pair(arg, next) => {
                                        // Evaluate each argument using the trampoline
                                        let evaled = eval_trampoline(arg.clone(), heap, env)?;
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
                        
                        // Apply the function
                        let result = f(heap, &evaled_args)?;
                        Ok(EvalResult::Done(result))
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

/// Legacy eval_expr function for backward compatibility.
/// 
/// This function is kept for compatibility but now uses the trampoline internally.
fn eval_expr(expr: GcRef, heap: &mut GcHeap, env: &mut HashMap<String, BuiltinKind>) -> Result<GcRef, String> {
    eval_trampoline(expr, heap, env)
} 

thread_local! {
    static GLOBAL_ENV: RefCell<HashMap<String, GcRef>> = RefCell::new(HashMap::new());
}

pub fn insert_global_binding(name: String, value: GcRef) {
    GLOBAL_ENV.with(|env| {
        env.borrow_mut().insert(name, value);
    });
}

pub fn lookup_global_binding(name: &str) -> Option<GcRef> {
    GLOBAL_ENV.with(|env| {
        env.borrow().get(name).cloned()
    })
}

pub fn evaluate_expression(expr: GcRef, heap: &mut GcHeap, env: &mut HashMap<String, BuiltinKind>) -> Result<GcRef, String> {
    eval_trampoline(expr, heap, env)
} 