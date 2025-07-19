//! Simple evaluator using the new GC system and two-layer architecture.
//!
//! This module implements a clean separation between evaluation logic and function application.
//! The logic layer handles self-evaluating forms, special forms, and argument evaluation,
//! while the apply layer handles function calls with pre-evaluated arguments.

use crate::gc::{GcHeap, GcRefSimple, SchemeValueSimple, new_pair_simple, new_vector_simple, new_closure_simple};
use crate::parser::ParserSimple;
use crate::io::Port;
use crate::env::Environment;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

/// Evaluator that owns both heap and environment
pub struct Evaluator {
    pub heap: GcHeap,
    env: Environment,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            heap: GcHeap::new(),
            env: Environment::new(),
        }
    }

    /// Get a reference to the heap for allocation
    pub fn heap_mut(&mut self) -> &mut GcHeap {
        &mut self.heap
    }

    /// Get a reference to the environment (read-only)
    pub fn env(&self) -> &Environment {
        &self.env
    }

    /// Get a mutable reference to the environment (for eval_apply only)
    pub fn env_mut(&mut self) -> &mut Environment {
        &mut self.env
    }
}

/// Apply function to pre-evaluated arguments
/// Handles environment access for symbol lookup and function application
pub fn eval_apply(func: GcRefSimple, args: &[GcRefSimple], evaluator: &mut Evaluator) -> Result<GcRefSimple, String> {
    match &func.value {
        SchemeValueSimple::Symbol(name) => {
            // Symbol lookup - check environment
            evaluator.env().get(name)
                .ok_or_else(|| format!("Unbound variable: {}", name))
        }
        SchemeValueSimple::Primitive { func: primitive_func, .. } => {
            // Apply primitive function
            primitive_func(&mut evaluator.heap_mut(), args)
        }
        SchemeValueSimple::Closure { params, body, env } => {
            // Apply closure - handle environment and evaluation
            eval_closure_logic(params, body, env, args, evaluator)
        }
        _ => {
            Err("eval_apply: function is not a symbol, primitive, or closure".to_string())
        }
    }
}

/// Evaluate a closure by creating a new environment frame and evaluating the body
fn eval_closure_logic(
    params: &[String],
    body: GcRefSimple,
    env: &Rc<RefCell<crate::env::Frame>>,
    args: &[GcRefSimple],
    evaluator: &mut Evaluator
) -> Result<GcRefSimple, String> {
    // Check argument count
    if args.len() != params.len() {
        return Err(format!(
            "Closure expects {} arguments, got {}",
            params.len(),
            args.len()
        ));
    }

    // Create a new environment extending the captured environment
    let captured_env = Environment::from_frame(env.clone());
    let mut new_env = captured_env.extend();

    // Bind parameters to arguments in the new environment
    for (param, arg) in params.iter().zip(args.iter()) {
        new_env.set(param.clone(), *arg);
    }

    // Temporarily switch the evaluator's environment to the new one
    let original_env = evaluator.env_mut().current_frame();
    evaluator.env_mut().set_current_frame(new_env.current_frame());

    // Evaluate the body in the new environment
    let result = eval_logic(body, evaluator);

    // Restore the original environment
    evaluator.env_mut().set_current_frame(original_env);

    result
}

/// Main evaluation walker - handles self-evaluating forms, symbol resolution, and nested calls
pub fn eval_logic(expr: GcRefSimple, evaluator: &mut Evaluator) -> Result<GcRefSimple, String> {
    // 1. Self-evaluating forms
    if is_self_evaluating(expr) {
        return Ok(expr);
    }

    match &expr.value {
        // 2. Symbol: resolve to function object (or variable) via eval_apply
        SchemeValueSimple::Symbol(_) => {
            eval_apply(expr, &[], evaluator)
        }
        // 3. Pair: function call or special form
        SchemeValueSimple::Pair(car, cdr) => {
            // Special form dispatch using match
            match &car.value {
                SchemeValueSimple::Symbol(name) => match name.as_str() {
                    "quote" => return quote_logic(expr, evaluator),
                    "begin" => return begin_logic(expr, evaluator),
                    "define" => return define_logic(expr, evaluator),
                    "if" => return if_logic(expr, evaluator),
                    "and" => return and_logic(expr, evaluator),
                    "or" => return or_logic(expr, evaluator),
                    "set!" => return set_logic(expr, evaluator),
                    "lambda" => return lambda_logic(expr, evaluator),
                    _ => {}
                },
                _ => {}
            }
            // Recursively evaluate all arguments (cdr)
            let mut evaluated_args = Vec::new();
            let mut current = *cdr;
            loop {
                match &current.value {
                    SchemeValueSimple::Nil => break,
                    SchemeValueSimple::Pair(arg, next) => {
                        evaluated_args.push(eval_logic(*arg, evaluator)?);
                        current = *next;
                    }
                    _ => return Err("Improper list in function call".to_string()),
                }
            }
            // Recursively evaluate the function position (car)
            let func = eval_logic(*car, evaluator)?;
            // Apply the function to the evaluated arguments
            eval_apply(func, &evaluated_args, evaluator)
        }
        _ => Err("eval_logic: unsupported expression type".to_string()),
    }
}

/// Quote logic: return first argument unevaluated
pub fn quote_logic(expr: GcRefSimple, _evaluator: &mut Evaluator) -> Result<GcRefSimple, String> {
    // (quote x) => return x unevaluated
    match &expr.value {
        SchemeValueSimple::Pair(_, cdr) => {
            match &cdr.value {
                SchemeValueSimple::Pair(arg, _) => Ok(*arg),
                _ => Err("Malformed quote: missing argument".to_string()),
            }
        }
        _ => Err("Malformed quote: not a pair".to_string()),
    }
}

// Stubs for special forms
pub fn begin_logic(expr: GcRefSimple, evaluator: &mut Evaluator) -> Result<GcRefSimple, String> {
    // (begin expr1 expr2 ... exprN) => evaluate each in sequence, return last
    match &expr.value {
        SchemeValueSimple::Pair(_, cdr) => {
            let mut current = *cdr;
            let mut last_result = None;
            loop {
                match &current.value {
                    SchemeValueSimple::Nil => break,
                    SchemeValueSimple::Pair(car, next) => {
                        last_result = Some(eval_logic(*car, evaluator)?);
                        current = *next;
                    }
                    _ => return Err("Malformed begin: improper list".to_string()),
                }
            }
            last_result.ok_or_else(|| "Malformed begin: no expressions".to_string())
        }
        _ => Err("Malformed begin: not a pair".to_string()),
    }
}
pub fn define_logic(expr: GcRefSimple, evaluator: &mut Evaluator) -> Result<GcRefSimple, String> {
    // (define symbol expr)
    match &expr.value {
        SchemeValueSimple::Pair(_, cdr) => {
            // cdr should be (symbol . rest)
            match &cdr.value {
                SchemeValueSimple::Pair(sym, rest) => {
                    // sym should be a symbol
                    let name = match &sym.value {
                        SchemeValueSimple::Symbol(s) => s.clone(),
                        _ => return Err("define: first argument must be a symbol".to_string()),
                    };
                    // rest should be (expr . nil)
                    match &rest.value {
                        SchemeValueSimple::Pair(expr_val, tail) => {
                            match &tail.value {
                                SchemeValueSimple::Nil => {
                                    // Evaluate expr_val
                                    let value = eval_logic(*expr_val, evaluator)?;
                                    evaluator.env_mut().set(name, value);
                                    Ok(value)
                                }
                                _ => Err("define: too many arguments".to_string()),
                            }
                        }
                        _ => Err("define: missing value expression".to_string()),
                    }
                }
                _ => Err("define: malformed arguments".to_string()),
            }
        }
        _ => Err("define: not a pair".to_string()),
    }
}
pub fn if_logic(expr: GcRefSimple, evaluator: &mut Evaluator) -> Result<GcRefSimple, String> {
    // (if test consequent [alternate])
    match &expr.value {
        SchemeValueSimple::Pair(_, cdr) => {
            // cdr should be (test . rest)
            match &cdr.value {
                SchemeValueSimple::Pair(test_expr, rest) => {
                    let test_val = eval_logic(*test_expr, evaluator)?;
                    // rest should be (consequent . rest2)
                    match &rest.value {
                        SchemeValueSimple::Pair(consequent_expr, rest2) => {
                            let test_is_true = match &test_val.value {
                                SchemeValueSimple::Bool(false) => false,
                                _ => true,
                            };
                            if test_is_true {
                                eval_logic(*consequent_expr, evaluator)
                            } else {
                                // rest2 may be (alternate . nil) or nil
                                match &rest2.value {
                                    SchemeValueSimple::Pair(alternate_expr, tail) => {
                                        match &tail.value {
                                            SchemeValueSimple::Nil => eval_logic(*alternate_expr, evaluator),
                                            _ => Err("if: too many arguments".to_string()),
                                        }
                                    }
                                    SchemeValueSimple::Nil => Ok(evaluator.heap.nil_simple()),
                                    _ => Err("if: malformed alternate".to_string()),
                                }
                            }
                        }
                        _ => Err("if: missing consequent expression".to_string()),
                    }
                }
                _ => Err("if: missing test expression".to_string()),
            }
        }
        _ => Err("if: not a pair".to_string()),
    }
}
pub fn and_logic(expr: GcRefSimple, evaluator: &mut Evaluator) -> Result<GcRefSimple, String> {
    // (and expr1 expr2 ... exprN)
    match &expr.value {
        SchemeValueSimple::Pair(_, cdr) => {
            let mut current = *cdr;
            let mut last = evaluator.heap.true_simple();
            loop {
                match &current.value {
                    SchemeValueSimple::Nil => break,
                    SchemeValueSimple::Pair(car, next) => {
                        let val = eval_logic(*car, evaluator)?;
                        match &val.value {
                            SchemeValueSimple::Bool(false) => return Ok(val),
                            _ => last = val,
                        }
                        current = *next;
                    }
                    _ => return Err("and: improper list".to_string()),
                }
            }
            Ok(last)
        }
        _ => Err("and: not a pair".to_string()),
    }
}

pub fn or_logic(expr: GcRefSimple, evaluator: &mut Evaluator) -> Result<GcRefSimple, String> {
    // (or expr1 expr2 ... exprN)
    match &expr.value {
        SchemeValueSimple::Pair(_, cdr) => {
            let mut current = *cdr;
            loop {
                match &current.value {
                    SchemeValueSimple::Nil => break,
                    SchemeValueSimple::Pair(car, next) => {
                        let val = eval_logic(*car, evaluator)?;
                        match &val.value {
                            SchemeValueSimple::Bool(false) => current = *next,
                            _ => return Ok(val),
                        }
                    }
                    _ => return Err("or: improper list".to_string()),
                }
            }
            Ok(evaluator.heap.false_simple())
        }
        _ => Err("or: not a pair".to_string()),
    }
}

pub fn set_logic(expr: GcRefSimple, evaluator: &mut Evaluator) -> Result<GcRefSimple, String> {
    // (set! symbol expr)
    match &expr.value {
        SchemeValueSimple::Pair(_, cdr) => {
            // cdr should be (symbol . rest)
            match &cdr.value {
                SchemeValueSimple::Pair(sym, rest) => {
                    // sym should be a symbol
                    let name = match &sym.value {
                        SchemeValueSimple::Symbol(s) => s.clone(),
                        _ => return Err("set!: first argument must be a symbol".to_string()),
                    };
                    // rest should be (expr . nil)
                    match &rest.value {
                        SchemeValueSimple::Pair(expr_val, tail) => {
                            match &tail.value {
                                SchemeValueSimple::Nil => {
                                    let value = eval_logic(*expr_val, evaluator)?;
                                    if evaluator.env().get(&name).is_some() {
                                        evaluator.env_mut().set(name, value);
                                        Ok(value)
                                    } else {
                                        Err("set!: unbound variable".to_string())
                                    }
                                }
                                _ => Err("set!: too many arguments".to_string()),
                            }
                        }
                        _ => Err("set!: missing value expression".to_string()),
                    }
                }
                _ => Err("set!: missing symbol argument".to_string()),
            }
        }
        _ => Err("set!: not a pair".to_string()),
    }
}

/// Lambda logic: create a closure with captured environment
/// (lambda (params...) body) => return closure
pub fn lambda_logic(expr: GcRefSimple, evaluator: &mut Evaluator) -> Result<GcRefSimple, String> {
    use crate::gc::new_closure_simple;
    use std::rc::Rc;
    use std::cell::RefCell;
    
    // Extract the arguments: (lambda (params...) body)
    match &expr.value {
        SchemeValueSimple::Pair(_, cdr) => {
            // cdr should be ((params...) . (body . nil))
            match &cdr.value {
                SchemeValueSimple::Pair(params_list, body_rest) => {
                    // Extract parameter names from params_list
                    let mut params = Vec::new();
                    let mut current = *params_list;
                    loop {
                        match &current.value {
                            SchemeValueSimple::Nil => break,
                            SchemeValueSimple::Pair(param, next) => {
                                match &param.value {
                                    SchemeValueSimple::Symbol(name) => {
                                        params.push(name.clone());
                                    }
                                    _ => return Err("lambda: parameter must be a symbol".to_string()),
                                }
                                current = *next;
                            }
                            _ => return Err("lambda: parameter list must be a proper list".to_string()),
                        }
                    }
                    
                    // Extract body from body_rest
                    match &body_rest.value {
                        SchemeValueSimple::Pair(body, tail) => {
                            match &tail.value {
                                SchemeValueSimple::Nil => {
                                    // Create a new environment that extends the current one
                                    // This captures the current environment for the closure
                                    let captured_env = evaluator.env().extend();
                                    
                                    // Create and return the closure
                                    Ok(new_closure_simple(
                                        evaluator.heap_mut(),
                                        params,
                                        *body,
                                        captured_env.current_frame(),
                                    ))
                                }
                                _ => Err("lambda: body must be a single expression".to_string()),
                            }
                        }
                        _ => Err("lambda: missing body expression".to_string()),
                    }
                }
                _ => Err("lambda: malformed arguments".to_string()),
            }
        }
        _ => Err("lambda: not a pair".to_string()),
    }
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/// Extract arguments from a list (cdr of a function call)
pub fn extract_args(expr: GcRefSimple) -> Result<Vec<GcRefSimple>, String> {
    let mut args = Vec::new();
    let mut current = expr;
    
    loop {
        match &current.value {
            SchemeValueSimple::Nil => {
                // End of list
                break;
            }
            SchemeValueSimple::Pair(car, cdr) => {
                // Add the car to our arguments
                args.push(*car);
                // Move to the cdr
                current = *cdr;
            }
            _ => {
                // Improper list - the cdr is not nil or a pair
                return Err("Improper list in function call".to_string());
            }
        }
    }
    
    Ok(args)
}

/// Check if a function is a special form
pub fn is_special_form(func: GcRefSimple) -> Option<&'static str> {
    match &func.value {
        SchemeValueSimple::Symbol(name) => {
            match name.as_str() {
                "quote" | "if" | "define" | "begin" | "and" | "or" | "set!" | "lambda" => Some(name),
                _ => None,
            }
        }
        _ => None,
    }
}

/// Check if an expression is self-evaluating (doesn't need evaluation)
pub fn is_self_evaluating(expr: GcRefSimple) -> bool {
    match &expr.value {
        SchemeValueSimple::Int(_) => true,
        SchemeValueSimple::Float(_) => true,
        SchemeValueSimple::Str(_) => true,
        SchemeValueSimple::Bool(_) => true,
        SchemeValueSimple::Char(_) => true,
        SchemeValueSimple::Nil => true,
        SchemeValueSimple::Closure { .. } => true,
        _ => false,
    }
}

/// Recursively deduplicate symbols in an expression tree.
///
/// This function walks through the expression and replaces all string-based symbols
/// with interned symbols from the symbol table. This ensures that symbols with the
/// same name are the same object, enabling fast comparison and proper Lisp semantics.
///
/// # Examples
///
/// ```rust
/// use s1::gc::GcHeap;
/// use s1::evalsimple::deduplicate_symbols;
///
/// let mut heap = GcHeap::new();
/// let expr = heap.intern_symbol("foo"); // Already interned
/// let deduplicated = deduplicate_symbols(expr, &mut heap);
/// assert!(std::ptr::eq(expr, deduplicated)); // Same object
/// ```
pub fn deduplicate_symbols(expr: GcRefSimple, heap: &mut GcHeap) -> GcRefSimple {
    match &expr.value {
        SchemeValueSimple::Symbol(name) => {
            // Replace with interned symbol
            heap.intern_symbol(name)
        }
        SchemeValueSimple::Pair(car, cdr) => {
            // Recursively deduplicate car and cdr
            let new_car = deduplicate_symbols(*car, heap);
            let new_cdr = deduplicate_symbols(*cdr, heap);
            
            // Only create new pair if something changed
            if std::ptr::eq(*car, new_car) && std::ptr::eq(*cdr, new_cdr) {
                expr // No change, return original
            } else {
                new_pair_simple(heap, new_car, new_cdr)
            }
        }
        SchemeValueSimple::Vector(elements) => {
            // Recursively deduplicate all vector elements
            let mut new_elements = Vec::new();
            let mut changed = false;
            
            for element in elements {
                let new_element = deduplicate_symbols(*element, heap);
                new_elements.push(new_element);
                if !std::ptr::eq(*element, new_element) {
                    changed = true;
                }
            }
            
            if changed {
                new_vector_simple(heap, new_elements)
            } else {
                expr // No change, return original
            }
        }
        SchemeValueSimple::Closure { params, body, env } => {
            // Deduplicate the body, but params are strings (will be handled later)
            let new_body = deduplicate_symbols(*body, heap);
            
            if std::ptr::eq(*body, new_body) {
                expr // No change, return original
            } else {
                new_closure_simple(heap, params.clone(), new_body, env.clone())
            }
        }
        // Self-evaluating forms and other types return unchanged
        SchemeValueSimple::Int(_) |
        SchemeValueSimple::Float(_) |
        SchemeValueSimple::Str(_) |
        SchemeValueSimple::Bool(_) |
        SchemeValueSimple::Char(_) |
        SchemeValueSimple::Nil |
        SchemeValueSimple::Primitive { .. } => expr,
    }
}

/// Parse an expression and deduplicate symbols before evaluation.
///
/// This wrapper function calls the parser and then runs symbol deduplication
/// to ensure all symbols are interned. The deduplication is transparent to
/// eval_logic, which continues to work with string comparisons.
///
/// # Examples
///
/// ```rust
/// use s1::gc::GcHeap;
/// use s1::evalsimple::{parse_and_deduplicate, Evaluator};
/// use s1::parser::ParserSimple;
/// use s1::io::Port;
///
/// let mut evaluator = Evaluator::new();
/// let mut parser = ParserSimple::new();
/// let mut port = Port::new_string_input("(define x 42)");
///
/// let expr = parse_and_deduplicate(&mut parser, &mut port, &mut evaluator.heap_mut()).unwrap();
/// // expr now has interned symbols, but eval_logic doesn't need to know
/// ```
pub fn parse_and_deduplicate(
    parser: &mut ParserSimple,
    port: &mut Port,
    heap: &mut GcHeap,
) -> Result<GcRefSimple, String> {
    // Parse the expression
    let parsed_expr = parser.parse(heap, port)?;
    
    // Deduplicate symbols in the parsed expression
    let deduplicated_expr = deduplicate_symbols(parsed_expr, heap);
    
    Ok(deduplicated_expr)
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gc::{new_int_simple, new_float_simple, new_string_simple, new_symbol_simple, new_pair_simple, new_primitive_simple, new_closure_simple};

    #[test]
    fn test_eval_logic_self_evaluating() {
        let mut evaluator = Evaluator::new();
        let int_val;
        {
            let heap = evaluator.heap_mut();
            int_val = new_int_simple(heap, num_bigint::BigInt::from(42));
        }
        let result = eval_logic(int_val, &mut evaluator).unwrap();
        assert_eq!(result.value, int_val.value);
    }

    #[test]
    fn test_eval_logic_variable_lookup() {
        let mut evaluator = Evaluator::new();
        let value;
        let symbol;
        {
            let heap = evaluator.heap_mut();
            value = new_int_simple(heap, num_bigint::BigInt::from(99));
            symbol = new_symbol_simple(heap, "x");
        }
        evaluator.env_mut().set("x".to_string(), value);
        let result = eval_logic(symbol, &mut evaluator).unwrap();
        assert_eq!(result.value, value.value);
    }

    #[test]
    fn test_eval_logic_non_nested_call() {
        use std::rc::Rc;
        let mut evaluator = Evaluator::new();
        let plus;
        let a;
        let b;
        let plus_sym;
        let args;
        let expr;
        {
            let heap = evaluator.heap_mut();
            plus = new_primitive_simple(
                heap,
                Rc::new(|heap, args| {
                    let a = match &args[0].value { SchemeValueSimple::Int(i) => i.clone(), _ => return Err("not int".to_string()) };
                    let b = match &args[1].value { SchemeValueSimple::Int(i) => i.clone(), _ => return Err("not int".to_string()) };
                    Ok(new_int_simple(heap, a + b))
                }),
                "plus".to_string(),
                false,
            );
            a = new_int_simple(heap, num_bigint::BigInt::from(2));
            b = new_int_simple(heap, num_bigint::BigInt::from(3));
            plus_sym = new_symbol_simple(heap, "+");
            let nil = heap.nil_simple();
            let b_pair = new_pair_simple(heap, b, nil);
            args = new_pair_simple(heap, a, b_pair);
            expr = new_pair_simple(heap, plus_sym, args);
        }
        evaluator.env_mut().set("+".to_string(), plus);
        let result = eval_logic(expr, &mut evaluator).unwrap();
        match &result.value {
            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "5"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_eval_logic_nested_call() {
        use std::rc::Rc;
        use crate::builtin::number::{plus_builtin_simple, times_builtin_simple};
        let mut evaluator = Evaluator::new();
        let plus;
        let times;
        let two;
        let three;
        let four;
        let five;
        let plus_sym;
        let plus_args;
        let plus_expr;
        let star_sym;
        let star_args;
        let expr;
        {
            let heap = evaluator.heap_mut();
            plus = new_primitive_simple(
                heap,
                Rc::new(plus_builtin_simple),
                "plus".to_string(),
                false,
            );
            times = new_primitive_simple(
                heap,
                Rc::new(times_builtin_simple),
                "times".to_string(),
                false,
            );
            two = new_int_simple(heap, num_bigint::BigInt::from(2));
            three = new_int_simple(heap, num_bigint::BigInt::from(3));
            four = new_int_simple(heap, num_bigint::BigInt::from(4));
            five = new_int_simple(heap, num_bigint::BigInt::from(5));
            plus_sym = new_symbol_simple(heap, "+");
            let nil1 = heap.nil_simple();
            let five_pair = new_pair_simple(heap, five, nil1);
            plus_args = new_pair_simple(heap, four, five_pair);
            plus_expr = new_pair_simple(heap, plus_sym, plus_args);
            star_sym = new_symbol_simple(heap, "*");
            let nil2 = heap.nil_simple();
            let plus_expr_pair = new_pair_simple(heap, plus_expr, nil2);
            let three_pair = new_pair_simple(heap, three, plus_expr_pair);
            star_args = new_pair_simple(heap, two, three_pair);
            expr = new_pair_simple(heap, star_sym, star_args);
        }
        evaluator.env_mut().set("+".to_string(), plus);
        evaluator.env_mut().set("*".to_string(), times);
        let result = eval_logic(expr, &mut evaluator).unwrap();
        match &result.value {
            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "54"),
            _ => panic!("Expected integer result"),
        }
    }
    
    #[test]
    fn test_extract_args() {
        let mut heap = crate::gc::GcHeap::new();
        
        // Create a list (1 2 3)
        let arg1 = new_int_simple(&mut heap, num_bigint::BigInt::from(1));
        let arg2 = new_int_simple(&mut heap, num_bigint::BigInt::from(2));
        let arg3 = new_int_simple(&mut heap, num_bigint::BigInt::from(3));
        let nil = heap.nil_simple();
        
        // Build the list: (1 . (2 . (3 . nil)))
        let list_3 = new_pair_simple(&mut heap, arg3, nil);
        let list_2_3 = new_pair_simple(&mut heap, arg2, list_3);
        let list_1_2_3 = new_pair_simple(&mut heap, arg1, list_2_3);
        
        // Extract arguments
        let args = extract_args(list_1_2_3).unwrap();
        assert_eq!(args.len(), 3);
        
        // Check the arguments
        match &args[0].value {
            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "1"),
            _ => panic!("Expected integer 1"),
        }
        match &args[1].value {
            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "2"),
            _ => panic!("Expected integer 2"),
        }
        match &args[2].value {
            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "3"),
            _ => panic!("Expected integer 3"),
        }
    }

    #[test]
    fn test_eval_logic_simple_nested_call() {
        use std::rc::Rc;
        let mut evaluator = Evaluator::new();
        let times;
        let plus;
        let two;
        let two2;
        let three;
        let plus_sym;
        let plus_args;
        let plus_expr;
        let star_sym;
        let star_args;
        let expr;
        {
            let heap = evaluator.heap_mut();
            times = new_primitive_simple(
                heap,
                Rc::new(|heap, args| {
                    let a = match &args[0].value { SchemeValueSimple::Int(i) => i.clone(), _ => return Err("not int".to_string()) };
                    let b = match &args[1].value { SchemeValueSimple::Int(i) => i.clone(), _ => return Err("not int".to_string()) };
                    Ok(new_int_simple(heap, a * b))
                }),
                "times".to_string(),
                false,
            );
            plus = new_primitive_simple(
                heap,
                Rc::new(|heap, args| {
                    let a = match &args[0].value { SchemeValueSimple::Int(i) => i.clone(), _ => return Err("not int".to_string()) };
                    let b = match &args[1].value { SchemeValueSimple::Int(i) => i.clone(), _ => return Err("not int".to_string()) };
                    Ok(new_int_simple(heap, a + b))
                }),
                "plus".to_string(),
                false,
            );
            two = new_int_simple(heap, num_bigint::BigInt::from(2));
            two2 = new_int_simple(heap, num_bigint::BigInt::from(2));
            three = new_int_simple(heap, num_bigint::BigInt::from(3));
            plus_sym = new_symbol_simple(heap, "+");
            let nil1 = heap.nil_simple();
            let three_pair = new_pair_simple(heap, three, nil1);
            plus_args = new_pair_simple(heap, two2, three_pair);
            plus_expr = new_pair_simple(heap, plus_sym, plus_args);
            star_sym = new_symbol_simple(heap, "*");
            let nil2 = heap.nil_simple();
            let plus_expr_pair = new_pair_simple(heap, plus_expr, nil2);
            star_args = new_pair_simple(heap, two, plus_expr_pair);
            expr = new_pair_simple(heap, star_sym, star_args);
        }
        evaluator.env_mut().set("*".to_string(), times);
        evaluator.env_mut().set("+".to_string(), plus);
        let result = eval_logic(expr, &mut evaluator).unwrap();
        match &result.value {
            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "10"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_eval_logic_nested_mixed_call() {
        use std::rc::Rc;
        let mut evaluator = Evaluator::new();
        let plus;
        let times;
        let minus;
        let two;
        let three;
        let four;
        let five;
        let minus_sym;
        let minus_args;
        let minus_expr;
        let times_sym;
        let times_args;
        let times_expr;
        let plus_sym;
        let plus_args;
        let expr;
        {
            let heap = evaluator.heap_mut();
            plus = new_primitive_simple(
                heap,
                Rc::new(|heap, args| {
                    let a = match &args[0].value { SchemeValueSimple::Int(i) => i.clone(), _ => return Err("not int".to_string()) };
                    let b = match &args[1].value { SchemeValueSimple::Int(i) => i.clone(), _ => return Err("not int".to_string()) };
                    Ok(new_int_simple(heap, a + b))
                }),
                "plus".to_string(),
                false,
            );
            times = new_primitive_simple(
                heap,
                Rc::new(|heap, args| {
                    let a = match &args[0].value { SchemeValueSimple::Int(i) => i.clone(), _ => return Err("not int".to_string()) };
                    let b = match &args[1].value { SchemeValueSimple::Int(i) => i.clone(), _ => return Err("not int".to_string()) };
                    Ok(new_int_simple(heap, a * b))
                }),
                "times".to_string(),
                false,
            );
            minus = new_primitive_simple(
                heap,
                Rc::new(|heap, args| {
                    let a = match &args[0].value { SchemeValueSimple::Int(i) => i.clone(), _ => return Err("not int".to_string()) };
                    let b = match &args[1].value { SchemeValueSimple::Int(i) => i.clone(), _ => return Err("not int".to_string()) };
                    Ok(new_int_simple(heap, a - b))
                }),
                "minus".to_string(),
                false,
            );
            two = new_int_simple(heap, num_bigint::BigInt::from(2));
            three = new_int_simple(heap, num_bigint::BigInt::from(3));
            four = new_int_simple(heap, num_bigint::BigInt::from(4));
            five = new_int_simple(heap, num_bigint::BigInt::from(5));
            minus_sym = new_symbol_simple(heap, "-");
            let nil1 = heap.nil_simple();
            let five_pair = new_pair_simple(heap, five, nil1);
            minus_args = new_pair_simple(heap, four, five_pair);
            minus_expr = new_pair_simple(heap, minus_sym, minus_args);
            times_sym = new_symbol_simple(heap, "*");
            let nil2 = heap.nil_simple();
            let minus_expr_pair = new_pair_simple(heap, minus_expr, nil2);
            times_args = new_pair_simple(heap, three, minus_expr_pair);
            times_expr = new_pair_simple(heap, times_sym, times_args);
            plus_sym = new_symbol_simple(heap, "+");
            let nil3 = heap.nil_simple();
            let times_expr_pair = new_pair_simple(heap, times_expr, nil3);
            plus_args = new_pair_simple(heap, two, times_expr_pair);
            expr = new_pair_simple(heap, plus_sym, plus_args);
        }
        evaluator.env_mut().set("+".to_string(), plus);
        evaluator.env_mut().set("*".to_string(), times);
        evaluator.env_mut().set("-".to_string(), minus);
        let result = eval_logic(expr, &mut evaluator).unwrap();
        match &result.value {
            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "-1"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_eval_logic_quote() {
        let mut evaluator = Evaluator::new();
        let quoted;
        let expr;
        {
            let heap = evaluator.heap_mut();
            let sym = new_symbol_simple(heap, "foo");
            let nil = heap.nil_simple();
            let sym_list = new_pair_simple(heap, sym, nil);
            let quote_sym = new_symbol_simple(heap, "quote");
            expr = new_pair_simple(heap, quote_sym, sym_list);
            quoted = sym;
        }
        let result = eval_logic(expr, &mut evaluator).unwrap();
        match &result.value {
            SchemeValueSimple::Symbol(s) => assert_eq!(s, "foo"),
            _ => panic!("Expected quoted symbol"),
        }
        // Test quoting a list: '(foo bar)
        let quoted_list;
        let expr2;
        {
            let heap = evaluator.heap_mut();
            let foo = new_symbol_simple(heap, "foo");
            let bar = new_symbol_simple(heap, "bar");
            let nil = heap.nil_simple();
            let bar_pair = new_pair_simple(heap, bar, nil);
            let foo_bar_list = new_pair_simple(heap, foo, bar_pair);
            let quote_sym = new_symbol_simple(heap, "quote");
            let foo_bar_list_pair = new_pair_simple(heap, foo_bar_list, nil);
            expr2 = new_pair_simple(heap, quote_sym, foo_bar_list_pair);
            quoted_list = foo_bar_list;
        }
        let result2 = eval_logic(expr2, &mut evaluator).unwrap();
        match &result2.value {
            SchemeValueSimple::Pair(_, _) => (),
            _ => panic!("Expected quoted list"),
        }
    }

    #[test]
    fn test_eval_logic_begin() {
        use std::rc::Rc;
        let mut evaluator = Evaluator::new();
        let plus;
        let a;
        let b;
        let c;
        let plus_sym;
        let plus_args;
        let plus_expr;
        let begin_sym;
        let begin_args;
        let expr;
        {
            let heap = evaluator.heap_mut();
            plus = new_primitive_simple(
                heap,
                Rc::new(|heap, args| {
                    let a = match &args[0].value { SchemeValueSimple::Int(i) => i.clone(), _ => return Err("not int".to_string()) };
                    let b = match &args[1].value { SchemeValueSimple::Int(i) => i.clone(), _ => return Err("not int".to_string()) };
                    Ok(new_int_simple(heap, a + b))
                }),
                "plus".to_string(),
                false,
            );
            a = new_int_simple(heap, num_bigint::BigInt::from(1));
            b = new_int_simple(heap, num_bigint::BigInt::from(2));
            c = new_int_simple(heap, num_bigint::BigInt::from(3));
            plus_sym = new_symbol_simple(heap, "+");
            let nil = heap.nil_simple();
            let b_pair = new_pair_simple(heap, b, nil);
            plus_args = new_pair_simple(heap, a, b_pair);
            plus_expr = new_pair_simple(heap, plus_sym, plus_args);
            begin_sym = new_symbol_simple(heap, "begin");
            let c_pair = new_pair_simple(heap, c, nil);
            let plus_expr_pair = new_pair_simple(heap, plus_expr, c_pair);
            begin_args = plus_expr_pair;
            expr = new_pair_simple(heap, begin_sym, begin_args);
        }
        evaluator.env_mut().set("+".to_string(), plus);
        let result = eval_logic(expr, &mut evaluator).unwrap();
        match &result.value {
            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "3"),
            _ => panic!("Expected integer result from begin"),
        }
    }

    #[test]
    fn test_eval_logic_define() {
        let mut evaluator = Evaluator::new();
        let symbol;
        let expr;
        {
            let heap = evaluator.heap_mut();
            symbol = new_symbol_simple(heap, "y");
            let val = new_int_simple(heap, num_bigint::BigInt::from(42));
            let nil = heap.nil_simple();
            let val_pair = new_pair_simple(heap, val, nil);
            let args = new_pair_simple(heap, symbol, val_pair);
            let define_sym = new_symbol_simple(heap, "define");
            expr = new_pair_simple(heap, define_sym, args);
        }
        let result = eval_logic(expr, &mut evaluator).unwrap();
        match &result.value {
            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "42"),
            _ => panic!("Expected integer result"),
        }
        // Check that the variable is now bound
        let bound = evaluator.env().get("y").unwrap();
        match &bound.value {
            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "42"),
            _ => panic!("Expected integer result in env"),
        }
    }

    #[test]
    fn test_eval_logic_if() {
        let mut evaluator = Evaluator::new();
        let expr_true;
        let expr_false;
        {
            let heap = evaluator.heap_mut();
            let if_sym = new_symbol_simple(heap, "if");
            let t = heap.true_simple();
            let f = heap.false_simple();
            let one = new_int_simple(heap, num_bigint::BigInt::from(1));
            let two = new_int_simple(heap, num_bigint::BigInt::from(2));
            let nil = heap.nil_simple();
            // (if #t 1 2)
            let two_pair = new_pair_simple(heap, two, nil);
            let one_pair = new_pair_simple(heap, one, two_pair);
            let t_pair = new_pair_simple(heap, t, one_pair);
            expr_true = new_pair_simple(heap, if_sym, t_pair);
            // (if #f 1 2)
            let two_pair2 = new_pair_simple(heap, two, nil);
            let one_pair2 = new_pair_simple(heap, one, two_pair2);
            let f_pair = new_pair_simple(heap, f, one_pair2);
            expr_false = new_pair_simple(heap, if_sym, f_pair);
        }
        let result_true = eval_logic(expr_true, &mut evaluator).unwrap();
        match &result_true.value {
            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "1"),
            _ => panic!("Expected integer result for true branch"),
        }
        let result_false = eval_logic(expr_false, &mut evaluator).unwrap();
        match &result_false.value {
            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "2"),
            _ => panic!("Expected integer result for false branch"),
        }
    }

    #[test]
    fn test_eval_logic_and_or() {
        // (and)
        {
            let mut evaluator = Evaluator::new();
            let heap = evaluator.heap_mut();
            let and_sym = new_symbol_simple(heap, "and");
            let nil = heap.nil_simple();
            let and_empty = new_pair_simple(heap, and_sym, nil);
            let result = eval_logic(and_empty, &mut evaluator).unwrap();
            assert!(matches!(&result.value, SchemeValueSimple::Bool(true)));
        }
        // (and #t 1 2)
        {
            let mut evaluator = Evaluator::new();
            let heap = evaluator.heap_mut();
            let and_sym = new_symbol_simple(heap, "and");
            let t = heap.true_simple();
            let one = new_int_simple(heap, num_bigint::BigInt::from(1));
            let two = new_int_simple(heap, num_bigint::BigInt::from(2));
            let nil = heap.nil_simple();
            let two_pair = new_pair_simple(heap, two, nil);
            let one_pair = new_pair_simple(heap, one, two_pair);
            let t_pair = new_pair_simple(heap, t, one_pair);
            let and_expr = new_pair_simple(heap, and_sym, t_pair);
            let result = eval_logic(and_expr, &mut evaluator).unwrap();
            match &result.value {
                SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "2"),
                _ => panic!("Expected integer result for and"),
            }
        }
        // (and #t #f 2)
        {
            let mut evaluator = Evaluator::new();
            let heap = evaluator.heap_mut();
            let and_sym = new_symbol_simple(heap, "and");
            let t = heap.true_simple();
            let f = heap.false_simple();
            let two = new_int_simple(heap, num_bigint::BigInt::from(2));
            let nil = heap.nil_simple();
            let two_pair = new_pair_simple(heap, two, nil);
            let f_pair = new_pair_simple(heap, f, two_pair);
            let t_pair2 = new_pair_simple(heap, t, f_pair);
            let and_expr2 = new_pair_simple(heap, and_sym, t_pair2);
            let result = eval_logic(and_expr2, &mut evaluator).unwrap();
            assert!(matches!(&result.value, SchemeValueSimple::Bool(false)));
        }
        // (or)
        {
            let mut evaluator = Evaluator::new();
            let heap = evaluator.heap_mut();
            let or_sym = new_symbol_simple(heap, "or");
            let nil = heap.nil_simple();
            let or_empty = new_pair_simple(heap, or_sym, nil);
            let result = eval_logic(or_empty, &mut evaluator).unwrap();
            assert!(matches!(&result.value, SchemeValueSimple::Bool(false)));
        }
        // (or #f 1 2)
        {
            let mut evaluator = Evaluator::new();
            let heap = evaluator.heap_mut();
            let or_sym = new_symbol_simple(heap, "or");
            let f = heap.false_simple();
            let one = new_int_simple(heap, num_bigint::BigInt::from(1));
            let two = new_int_simple(heap, num_bigint::BigInt::from(2));
            let nil = heap.nil_simple();
            let two_pair = new_pair_simple(heap, two, nil);
            let one_pair = new_pair_simple(heap, one, two_pair);
            let f_pair = new_pair_simple(heap, f, one_pair);
            let or_expr = new_pair_simple(heap, or_sym, f_pair);
            let result = eval_logic(or_expr, &mut evaluator).unwrap();
            match &result.value {
                SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "1"),
                _ => panic!("Expected integer result for or"),
            }
        }
        // (or #f #f 2)
        {
            let mut evaluator = Evaluator::new();
            let heap = evaluator.heap_mut();
            let or_sym = new_symbol_simple(heap, "or");
            let f = heap.false_simple();
            let two = new_int_simple(heap, num_bigint::BigInt::from(2));
            let nil = heap.nil_simple();
            let two_pair = new_pair_simple(heap, two, nil);
            let f_pair2 = new_pair_simple(heap, f, two_pair);
            let f_pair3 = new_pair_simple(heap, f, f_pair2);
            let or_expr2 = new_pair_simple(heap, or_sym, f_pair3);
            let result = eval_logic(or_expr2, &mut evaluator).unwrap();
            match &result.value {
                SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "2"),
                _ => panic!("Expected integer result for or"),
            }
        }
        // (or #f #f #f)
        {
            let mut evaluator = Evaluator::new();
            let heap = evaluator.heap_mut();
            let or_sym = new_symbol_simple(heap, "or");
            let f = heap.false_simple();
            let nil = heap.nil_simple();
            let f_pair = new_pair_simple(heap, f, nil);
            let f_pair2 = new_pair_simple(heap, f, f_pair);
            let f_pair3 = new_pair_simple(heap, f, f_pair2);
            let or_expr3 = new_pair_simple(heap, or_sym, f_pair3);
            let result = eval_logic(or_expr3, &mut evaluator).unwrap();
            assert!(matches!(&result.value, SchemeValueSimple::Bool(false)));
        }
    }

    #[test]
    fn test_eval_logic_lambda() {
        let mut evaluator = Evaluator::new();
        
        // Set up the + function in the environment
        {
            let heap = evaluator.heap_mut();
            let plus = new_primitive_simple(
                heap,
                Rc::new(|heap, args| {
                    let a = match &args[0].value { SchemeValueSimple::Int(i) => i.clone(), _ => return Err("not int".to_string()) };
                    let b = match &args[1].value { SchemeValueSimple::Int(i) => i.clone(), _ => return Err("not int".to_string()) };
                    Ok(new_int_simple(heap, a + b))
                }),
                "plus".to_string(),
                false,
            );
            evaluator.env_mut().set("+".to_string(), plus);
        }
        
        let lambda_expr;
        let add1;
        let result;
        {
            let heap = evaluator.heap_mut();
            // Create (lambda (x) (+ x 1))
            let x_param = new_symbol_simple(heap, "x");
            let one = new_int_simple(heap, num_bigint::BigInt::from(1));
            let plus_sym = new_symbol_simple(heap, "+");
            let nil = heap.nil_simple();
            
            // Build (+ x 1)
            let one_pair = new_pair_simple(heap, one, nil);
            let x_one_pair = new_pair_simple(heap, x_param, one_pair);
            let plus_expr = new_pair_simple(heap, plus_sym, x_one_pair);
            
            // Build (lambda (x) (+ x 1))
            let lambda_sym = new_symbol_simple(heap, "lambda");
            let params_list = new_pair_simple(heap, x_param, nil);
            let body_nil = new_pair_simple(heap, plus_expr, nil);
            let lambda_args = new_pair_simple(heap, params_list, body_nil);
            lambda_expr = new_pair_simple(heap, lambda_sym, lambda_args);
        }
        
        // Evaluate the lambda to create a closure
        add1 = eval_logic(lambda_expr, &mut evaluator).unwrap();
        
        // Verify it's a closure
        match &add1.value {
            SchemeValueSimple::Closure { params, body, .. } => {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0], "x");
                // The body should be the (+ x 1) expression
                match &body.value {
                    SchemeValueSimple::Pair(car, _cdr) => {
                        match &car.value {
                            SchemeValueSimple::Symbol(s) => assert_eq!(s, "+"),
                            _ => panic!("Expected + symbol"),
                        }
                    }
                    _ => panic!("Expected pair as body"),
                }
            }
            _ => panic!("Expected closure"),
        }
        
        // Now apply the closure: (add1 5)
        let five;
        let apply_expr;
        {
            let heap = evaluator.heap_mut();
            five = new_int_simple(heap, num_bigint::BigInt::from(5));
            
            // Build (add1 5)
            let nil = heap.nil_simple();
            let five_pair = new_pair_simple(heap, five, nil);
            apply_expr = new_pair_simple(heap, add1, five_pair);
        }
        
        // Evaluate the application
        result = eval_logic(apply_expr, &mut evaluator).unwrap();
        
        // Should return 6
        match &result.value {
            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "6"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_eval_logic_closure_application() {
        let mut evaluator = Evaluator::new();
        
        // Set up the + function in the environment
        {
            let heap = evaluator.heap_mut();
            let plus = new_primitive_simple(
                heap,
                Rc::new(|heap, args| {
                    let a = match &args[0].value { SchemeValueSimple::Int(i) => i.clone(), _ => return Err("not int".to_string()) };
                    let b = match &args[1].value { SchemeValueSimple::Int(i) => i.clone(), _ => return Err("not int".to_string()) };
                    Ok(new_int_simple(heap, a + b))
                }),
                "plus".to_string(),
                false,
            );
            evaluator.env_mut().set("+".to_string(), plus);
        }
        
        // Create a closure manually: (lambda (x y) (+ x y))
        let closure;
        let captured_env = evaluator.env().current_frame();
        {
            let heap = evaluator.heap_mut();
            let x_param = new_symbol_simple(heap, "x");
            let y_param = new_symbol_simple(heap, "y");
            let plus_sym = new_symbol_simple(heap, "+");
            let nil = heap.nil_simple();
            
            // Build (+ x y)
            let y_pair = new_pair_simple(heap, y_param, nil);
            let x_y_pair = new_pair_simple(heap, x_param, y_pair);
            let plus_expr = new_pair_simple(heap, plus_sym, x_y_pair);
            
            // Create closure with captured environment
            closure = new_closure_simple(heap, vec!["x".to_string(), "y".to_string()], plus_expr, captured_env);
        }
        
        // Apply the closure: (closure 3 4)
        let three;
        let four;
        let apply_expr;
        let result;
        {
            let heap = evaluator.heap_mut();
            three = new_int_simple(heap, num_bigint::BigInt::from(3));
            four = new_int_simple(heap, num_bigint::BigInt::from(4));
            
            // Build (closure 3 4)
            let nil = heap.nil_simple();
            let four_pair = new_pair_simple(heap, four, nil);
            let three_four_pair = new_pair_simple(heap, three, four_pair);
            apply_expr = new_pair_simple(heap, closure, three_four_pair);
        }
        
        // Evaluate the application
        result = eval_logic(apply_expr, &mut evaluator).unwrap();
        
        // Should return 7
        match &result.value {
            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "7"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_eval_logic_set() {
        let mut evaluator = Evaluator::new();
        let symbol;
        let value;
        let set_sym;
        let x_sym;
        let val_123;
        let arg_pair;
        let args;
        let set_expr;
        {
            let heap = evaluator.heap_mut();
            symbol = new_symbol_simple(heap, "x");
            value = new_int_simple(heap, num_bigint::BigInt::from(123));
            set_sym = new_symbol_simple(heap, "set!");
            x_sym = new_symbol_simple(heap, "x");
            val_123 = new_int_simple(heap, num_bigint::BigInt::from(123));
            let nil = heap.nil_simple();
            arg_pair = new_pair_simple(heap, val_123, nil);
            args = new_pair_simple(heap, x_sym, arg_pair);
            set_expr = new_pair_simple(heap, set_sym, args);
        }
        
        // First define x
        evaluator.env_mut().set("x".to_string(), value);
        
        // Then set! it
        let result = eval_logic(set_expr, &mut evaluator).unwrap();
        
        // Verify the value was set
        let new_value = evaluator.env().get("x").unwrap();
        match &new_value.value {
            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "123"),
            _ => panic!("Expected integer"),
        }
    }

    #[test]
    fn test_deduplicate_symbols() {
        let mut heap = GcHeap::new();
        
        // Create a simple expression with symbols: (foo bar)
        let foo_sym = new_symbol_simple(&mut heap, "foo");
        let bar_sym = new_symbol_simple(&mut heap, "bar");
        let nil = heap.nil_simple();
        let bar_nil = new_pair_simple(&mut heap, bar_sym, nil);
        let expr = new_pair_simple(&mut heap, foo_sym, bar_nil);
        
        // Deduplicate the expression
        let deduplicated = deduplicate_symbols(expr, &mut heap);
        
        // Verify that symbols are now interned
        match &deduplicated.value {
            SchemeValueSimple::Pair(car, cdr) => {
                // Check that car is an interned symbol
                match &car.value {
                    SchemeValueSimple::Symbol(name) => assert_eq!(name, "foo"),
                    _ => panic!("Expected symbol"),
                }
                
                // Check that cdr is a pair with an interned symbol
                match &cdr.value {
                    SchemeValueSimple::Pair(car2, cdr2) => {
                        match &car2.value {
                            SchemeValueSimple::Symbol(name) => assert_eq!(name, "bar"),
                            _ => panic!("Expected symbol"),
                        }
                        match &cdr2.value {
                            SchemeValueSimple::Nil => {},
                            _ => panic!("Expected nil"),
                        }
                    }
                    _ => panic!("Expected pair"),
                }
            }
            _ => panic!("Expected pair"),
        }
        
        // Verify that re-deduplicating returns the same object
        let deduplicated2 = deduplicate_symbols(deduplicated, &mut heap);
        assert!(std::ptr::eq(deduplicated, deduplicated2), "Re-deduplication should return same object");
        
        // Verify that symbols with same name are the same object
        let foo1 = heap.intern_symbol("foo");
        let foo2 = heap.intern_symbol("foo");
        assert!(std::ptr::eq(foo1, foo2), "Same symbol name should be same object");
        
        // Test with a more complex expression: ((lambda (x) (+ x 1)) 5)
        let lambda_sym = new_symbol_simple(&mut heap, "lambda");
        let x_sym = new_symbol_simple(&mut heap, "x");
        let plus_sym = new_symbol_simple(&mut heap, "+");
        let one = new_int_simple(&mut heap, num_bigint::BigInt::from(1));
        let five = new_int_simple(&mut heap, num_bigint::BigInt::from(5));
        
        // Build (+ x 1)
        let one_nil = new_pair_simple(&mut heap, one, nil);
        let x_one = new_pair_simple(&mut heap, x_sym, one_nil);
        let plus_expr = new_pair_simple(&mut heap, plus_sym, x_one);
        
        // Build (lambda (x) (+ x 1))
        let x_nil = new_pair_simple(&mut heap, x_sym, nil);
        let plus_nil = new_pair_simple(&mut heap, plus_expr, nil);
        let lambda_args = new_pair_simple(&mut heap, x_nil, plus_nil);
        let lambda_expr = new_pair_simple(&mut heap, lambda_sym, lambda_args);
        
        // Build ((lambda (x) (+ x 1)) 5)
        let five_nil = new_pair_simple(&mut heap, five, nil);
        let complex_expr = new_pair_simple(&mut heap, lambda_expr, five_nil);
        
        // Deduplicate the complex expression
        let deduplicated_complex = deduplicate_symbols(complex_expr, &mut heap);
        
        // Verify that all symbols in the complex expression are interned
        // (We can't easily check all of them, but we can verify the structure is preserved)
        match &deduplicated_complex.value {
            SchemeValueSimple::Pair(car, cdr) => {
                // Should be a pair structure
                match &car.value {
                    SchemeValueSimple::Pair(lambda_car, lambda_cdr) => {
                        // Should be (lambda ...)
                        match &lambda_car.value {
                            SchemeValueSimple::Symbol(name) => assert_eq!(name, "lambda"),
                            _ => panic!("Expected lambda symbol"),
                        }
                    }
                    _ => panic!("Expected lambda expression"),
                }
                match &cdr.value {
                    SchemeValueSimple::Pair(arg_car, arg_cdr) => {
                        // Should be (5)
                        match &arg_car.value {
                            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "5"),
                            _ => panic!("Expected integer 5"),
                        }
                        match &arg_cdr.value {
                            SchemeValueSimple::Nil => {},
                            _ => panic!("Expected nil"),
                        }
                    }
                    _ => panic!("Expected argument list"),
                }
            }
            _ => panic!("Expected function call structure"),
        }
    }

    #[test]
    fn test_parse_and_deduplicate() {
        let mut evaluator = Evaluator::new();
        let mut parser = ParserSimple::new();
        
        // Test parsing and deduplicating a simple expression
        let mut port = Port {
            kind: crate::io::PortKind::StringPortInput {
                content: "(define x 42)".to_string(),
                pos: 0,
            },
        };
        
        let expr = parse_and_deduplicate(&mut parser, &mut port, &mut evaluator.heap_mut()).unwrap();
        
        // Verify the expression structure is preserved
        match &expr.value {
            SchemeValueSimple::Pair(car, cdr) => {
                // Should be (define x 42)
                match &car.value {
                    SchemeValueSimple::Symbol(name) => assert_eq!(name, "define"),
                    _ => panic!("Expected define symbol"),
                }
                match &cdr.value {
                    SchemeValueSimple::Pair(sym, rest) => {
                        match &sym.value {
                            SchemeValueSimple::Symbol(name) => assert_eq!(name, "x"),
                            _ => panic!("Expected x symbol"),
                        }
                        match &rest.value {
                            SchemeValueSimple::Pair(val, nil) => {
                                match &val.value {
                                    SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "42"),
                                    _ => panic!("Expected integer 42"),
                                }
                                match &nil.value {
                                    SchemeValueSimple::Nil => {},
                                    _ => panic!("Expected nil"),
                                }
                            }
                            _ => panic!("Expected value pair"),
                        }
                    }
                    _ => panic!("Expected symbol-value pair"),
                }
            }
            _ => panic!("Expected define expression"),
        }
        
        // Test that symbols are interned by parsing the same expression again
        let mut port2 = Port {
            kind: crate::io::PortKind::StringPortInput {
                content: "(define x 42)".to_string(),
                pos: 0,
            },
        };
        
        let expr2 = parse_and_deduplicate(&mut parser, &mut port2, &mut evaluator.heap_mut()).unwrap();
        
        // Extract the define symbols from both expressions
        let define1 = match &expr.value {
            SchemeValueSimple::Pair(car, _) => car,
            _ => panic!("Expected pair"),
        };
        let define2 = match &expr2.value {
            SchemeValueSimple::Pair(car, _) => car,
            _ => panic!("Expected pair"),
        };
        
        // Verify they are the same interned symbol
        assert!(std::ptr::eq(*define1, *define2), "Define symbols should be interned");
        
        // Test symbol table statistics
        assert!(evaluator.heap_mut().symbol_table_stats() > 0, "Symbol table should contain symbols");
    }
} 