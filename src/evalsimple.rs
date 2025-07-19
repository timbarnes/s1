//! Simple evaluator using the new GC system and two-layer architecture.
//!
//! This module implements a clean separation between evaluation logic and function application.
//! The logic layer handles self-evaluating forms, special forms, and argument evaluation,
//! while the apply layer handles function calls with pre-evaluated arguments.

use crate::gc::{GcHeap, GcRefSimple, SchemeValueSimple};
use crate::parser::ParserSimple;
use crate::io::Port;
use crate::env::Environment;
use std::collections::HashMap;

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
        _ => {
            Err("eval_apply: function is not a symbol or primitive".to_string())
        }
    }
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
                "quote" | "if" | "define" | "begin" | "and" | "or" | "set!" => Some(name),
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
        _ => false,
    }
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gc::{new_int_simple, new_float_simple, new_string_simple, new_symbol_simple, new_pair_simple, new_primitive_simple};

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
    fn test_eval_logic_set() {
        let mut evaluator = Evaluator::new();
        let (symbol, value, set_sym, x_sym, val_123, arg_pair, args, set_expr);
        {
            let heap = evaluator.heap_mut();
            symbol = new_symbol_simple(heap, "x");
            value = new_int_simple(heap, num_bigint::BigInt::from(22));
            set_sym = new_symbol_simple(heap, "set!");
            x_sym = new_symbol_simple(heap, "x");
            val_123 = new_int_simple(heap, num_bigint::BigInt::from(123));
            arg_pair = new_pair_simple(heap, val_123, heap.nil_simple());
            args = new_pair_simple(heap, x_sym, arg_pair);
            set_expr = new_pair_simple(heap, set_sym, args);
        }
        evaluator.env_mut().set("x".to_string(), value);
        let result = eval_logic(set_expr, &mut evaluator).unwrap();
        assert_eq!(result.value, SchemeValueSimple::Int(num_bigint::BigInt::from(123)));
        // Check that the environment was updated
        let updated = evaluator.env().get("x").unwrap();
        assert_eq!(updated.value, SchemeValueSimple::Int(num_bigint::BigInt::from(123)));
    }
} 