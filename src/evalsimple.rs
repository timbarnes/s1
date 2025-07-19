//! Simple evaluator using the new GC system and two-layer architecture.
//!
//! This module implements a clean separation between evaluation logic and function application.
//! The logic layer handles self-evaluating forms, special forms, and argument evaluation,
//! while the apply layer handles function calls with pre-evaluated arguments.

use crate::gc::{GcHeap, GcRefSimple, SchemeValueSimple};
use crate::parser::ParserSimple;
use crate::io::Port;
use std::collections::HashMap;

/// Simple environment for variable bindings
pub struct Environment {
    bindings: HashMap<String, GcRefSimple>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<GcRefSimple> {
        self.bindings.get(name).copied()
    }

    pub fn set(&mut self, name: String, value: GcRefSimple) {
        self.bindings.insert(name, value);
    }

    pub fn bindings_mut(&mut self) -> &mut HashMap<String, GcRefSimple> {
        &mut self.bindings
    }
}

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
pub fn define_logic(_expr: GcRefSimple, _evaluator: &mut Evaluator) -> Result<GcRefSimple, String> {
    Err("define not implemented yet".to_string())
}
pub fn if_logic(_expr: GcRefSimple, _evaluator: &mut Evaluator) -> Result<GcRefSimple, String> {
    Err("if not implemented yet".to_string())
}
pub fn and_logic(_expr: GcRefSimple, _evaluator: &mut Evaluator) -> Result<GcRefSimple, String> {
    Err("and not implemented yet".to_string())
}
pub fn or_logic(_expr: GcRefSimple, _evaluator: &mut Evaluator) -> Result<GcRefSimple, String> {
    Err("or not implemented yet".to_string())
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
                "quote" | "if" | "define" | "begin" | "and" | "or" => Some(name),
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
} 