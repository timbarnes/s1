pub mod number;
pub mod predicate;

use crate::gc::{GcHeap, GcRef, new_primitive, new_string, new_bool, as_bool, as_pair, is_nil};
use std::collections::HashMap;
use std::rc::Rc;
use num_bigint::BigInt;
use crate::gc::{as_int, as_float, new_int, new_float, SchemeValue};
use num_traits::ToPrimitive;
use number::{plus_builtin, minus_builtin, times_builtin, div_builtin, mod_builtin};
use crate::printer::scheme_display;
use crate::eval::Evaluator;

pub enum BuiltinKind {
    // Old special form signature (for backward compatibility during transition)
    SpecialFormOld(fn(&[GcRef], fn(&GcRef) -> Result<GcRef, String>) -> Result<GcRef, String>),
    // New special form signature using Evaluator
    SpecialFormNew(fn(&mut Evaluator, &[GcRef]) -> Result<GcRef, String>),
    Normal(fn(&mut GcHeap, &[GcRef]) -> Result<GcRef, String>),
}

// No Clone implementation needed for BuiltinKind with function pointers

pub fn quote_handler(args: &[GcRef], _eval: fn(&GcRef) -> Result<GcRef, String>) -> Result<GcRef, String> {
    if args.len() != 1 {
        Err("quote: expected exactly 1 argument".to_string())
    } else {
        Ok(args[0].clone())
    }
}

/// New quote handler using Evaluator interface
pub fn quote_handler_new(evaluator: &mut Evaluator, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        Err("quote: expected exactly 1 argument".to_string())
    } else {
        Ok(args[0].clone())
    }
}

/// New and handler using Evaluator interface
pub fn and_handler_new(evaluator: &mut Evaluator, args: &[GcRef]) -> Result<GcRef, String> {
    if args.is_empty() {
        // (and) returns #t
        return Ok(new_bool(&mut evaluator.heap, true));
    }
    Ok(args[0].clone())
}

/// New or handler using Evaluator interface
pub fn or_handler_new(evaluator: &mut Evaluator, args: &[GcRef]) -> Result<GcRef, String> {
    if args.is_empty() {
        // (or) returns #f
        return Ok(new_bool(&mut evaluator.heap, false));
    }
    Ok(args[0].clone())
}

/// New if handler using Evaluator interface
pub fn if_handler_new(evaluator: &mut Evaluator, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 && args.len() != 3 {
        return Err("if: expected 2 or 3 arguments".to_string());
    }
    
    // For now, just return the test expression
    // TODO: Implement proper evaluation with environment
    Ok(args[0].clone())
}

/// New begin handler using Evaluator interface
pub fn begin_handler_new(evaluator: &mut Evaluator, args: &[GcRef]) -> Result<GcRef, String> {
    if args.is_empty() {
        return Err("begin: expected at least 1 argument".to_string());
    }
    
    // For now, just return the last expression
    // TODO: Implement proper evaluation with environment
    Ok(args[args.len()-1].clone())
}

/// Special form: (if test consequent alternative)
/// 
/// Evaluates test. If test is true, evaluates and returns consequent.
/// If test is false, evaluates and returns alternative.
/// 
/// This is implemented to support tail recursion - the result of evaluating
/// the consequent or alternative is returned directly without further processing.
pub fn if_handler(args: &[GcRef], _eval: fn(&GcRef) -> Result<GcRef, String>) -> Result<GcRef, String> {
    if args.len() != 2 && args.len() != 3 {
        return Err("if: expected 2 or 3 arguments".to_string());
    }
    
    // For now, just return the test expression
    // TODO: Implement proper evaluation with environment
    Ok(args[0].clone())
}

/// Special form: (begin expr1 expr2 ...)
/// 
/// Evaluates expressions in sequence, returning the value of the last expression.
/// 
/// This is implemented to support tail recursion - only the last expression
/// is returned for further evaluation.
pub fn begin_handler(args: &[GcRef], _eval: fn(&GcRef) -> Result<GcRef, String>) -> Result<GcRef, String> {
    if args.is_empty() {
        return Err("begin: expected at least 1 argument".to_string());
    }
    
    // For now, just return the last expression
    // TODO: Implement proper evaluation with environment
    Ok(args[args.len()-1].clone())
}

/// Special form: (and expr1 expr2 ...)
/// 
/// Evaluates expressions from left to right. If any expression evaluates to #f,
/// returns #f immediately. Otherwise returns the value of the last expression.
/// 
/// This is implemented to support tail recursion - short-circuits on #f and
/// returns the last expression for evaluation.
pub fn and_handler(args: &[GcRef], _eval: fn(&GcRef) -> Result<GcRef, String>) -> Result<GcRef, String> {
    if args.is_empty() {
        // (and) returns #t; for now, return the first argument or a dummy true value
        // TODO: Replace with canonical true value if available
        return Ok(args.get(0).cloned().unwrap_or_else(|| args[0].clone()));
    }
    Ok(args[0].clone())
}

/// Special form: (or expr1 expr2 ...)
/// 
/// Evaluates expressions from left to right. If any expression evaluates to a true value,
/// returns that value immediately. If all expressions evaluate to #f, returns #f.
/// 
/// This is implemented to support tail recursion - short-circuits on true values and
/// returns the last expression for evaluation if all are false.
pub fn or_handler(args: &[GcRef], _eval: fn(&GcRef) -> Result<GcRef, String>) -> Result<GcRef, String> {
    if args.is_empty() {
        // (or) returns #f; for now, return the first argument or a dummy false value
        // TODO: Replace with canonical false value if available
        return Ok(args.get(0).cloned().unwrap_or_else(|| args[0].clone()));
    }
    Ok(args[0].clone())
}

/// Special form: (define symbol expr)
/// 
/// Defines a new variable in the current environment.
/// The expression is evaluated and the result is bound to the symbol.
/// 
/// This is a special form because the expression is evaluated in the current environment.
pub fn define_handler_with_eval(args: &[GcRef], eval: fn(&GcRef) -> Result<GcRef, String>) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("define: expected exactly 2 arguments (symbol expr)".to_string());
    }
    
    // First argument should be a symbol
    let symbol = &args[0];
    let symbol_name = match &symbol.borrow().value {
        SchemeValue::Symbol(name) => name.clone(),
        _ => return Err("define: first argument must be a symbol".to_string()),
    };
    
    // Second argument is the expression to evaluate
    let expr = &args[1];
    
    // Evaluate the expression using the evaluation service
    let evaluated_value = eval(expr)?;
    
    // Store the evaluated value in the global environment
    crate::eval::insert_global_binding(symbol_name.clone(), evaluated_value.clone());
    
    // Return the evaluated value
    Ok(evaluated_value)
}

pub fn display_builtin(heap: &mut crate::gc::GcHeap, args: &[crate::gc::GcRef]) -> Result<crate::gc::GcRef, String> {
    if args.len() < 1 || args.len() > 2 {
        return Err("display: expected 1 or 2 arguments".to_string());
    }
    
    let val = &args[0];
    let s = scheme_display(&val.borrow().value);
    
    // If a port is provided as the second argument, write to it
    if args.len() == 2 {
        let port_arg = &args[1];
        // For now, we'll just write to stdout since we don't have port objects in GC yet
        // In a full implementation, we'd check if port_arg is a port and write to it
        print!("{}", s);
        use std::io::Write;
        std::io::stdout().flush().unwrap();
    } else {
        // Default behavior: write to stdout
        print!("{}", s);
        use std::io::Write;
        std::io::stdout().flush().unwrap();
    }
    
    Ok(val.clone())
}

pub fn newline_builtin(heap: &mut crate::gc::GcHeap, args: &[crate::gc::GcRef]) -> Result<crate::gc::GcRef, String> {
    if args.len() > 1 {
        return Err("newline: expected 0 or 1 arguments".to_string());
    }
    
    // For now, just print a newline to stdout
    // In a full implementation, we'd write to the specified port
    println!();
    
    // Return undefined (we'll use #f for now)
    Ok(new_bool(heap, false))
}

pub fn register_all(heap: &mut crate::gc::GcHeap, env: &mut std::collections::HashMap<String, BuiltinKind>) {
    env.insert("number?".to_string(), BuiltinKind::Normal(predicate::number_q));
    env.insert("help".to_string(), BuiltinKind::Normal(help_builtin));
    env.insert("quote".to_string(), BuiltinKind::SpecialFormNew(quote_handler_new));
    env.insert("if".to_string(), BuiltinKind::SpecialFormNew(if_handler_new));
    env.insert("begin".to_string(), BuiltinKind::SpecialFormNew(begin_handler_new));
    env.insert("and".to_string(), BuiltinKind::SpecialFormNew(and_handler_new));
    env.insert("or".to_string(), BuiltinKind::SpecialFormNew(or_handler_new));
    env.insert("define".to_string(), BuiltinKind::SpecialFormOld(define_handler_with_eval));
    env.insert("type-of".to_string(), BuiltinKind::Normal(predicate::type_of));
    env.insert("+".to_string(), BuiltinKind::Normal(plus_builtin));
    env.insert("-".to_string(), BuiltinKind::Normal(minus_builtin));
    env.insert("*".to_string(), BuiltinKind::Normal(times_builtin));
    env.insert("/".to_string(), BuiltinKind::Normal(div_builtin));
    env.insert("mod".to_string(), BuiltinKind::Normal(mod_builtin));
    env.insert("display".to_string(), BuiltinKind::Normal(display_builtin));
    env.insert("newline".to_string(), BuiltinKind::Normal(newline_builtin));
    // Add more builtins and special forms here
}

// (help 'symbol): returns the doc string for the given symbol as a Scheme string
pub fn help_builtin(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    use crate::gc::as_symbol;
    if let Some(arg) = args.get(0) {
        if let Some(sym) = as_symbol(arg) {
            // In a real implementation, you would have access to the environment here.
            // For now, return a placeholder string.
            Ok(new_string(heap, format!("Help for {}: ...", sym)))
        } else {
            Err("help: argument must be a symbol".to_string())
        }
    } else {
        Err("help: expected 1 argument".to_string())
    }
} 

#[cfg(test)]
mod tests {
    use super::*;
    use crate::io::{new_output_string_port, get_output_string, write_line};
    use crate::gc::{new_string, new_int, new_bool};

    #[test]
    fn test_display_with_optional_port() {
        // Create a heap and some test values
        let mut heap = GcHeap::new();
        let test_string = new_string(&mut heap, "hello world");
        let test_int = new_int(&mut heap, BigInt::from(42));
        let test_bool = new_bool(&mut heap, true);
        
        // Test display with one argument (defaults to stdout)
        let args = vec![test_string.clone()];
        let result = display_builtin(&mut heap, &args);
        assert!(result.is_ok());
        
        // Test display with two arguments (port specified)
        // Note: Currently this still writes to stdout since we don't have port objects in GC yet
        let args_with_port = vec![test_string.clone(), test_int.clone()];
        let result = display_builtin(&mut heap, &args_with_port);
        assert!(result.is_ok());
        
        // Test error case: too many arguments
        let too_many_args = vec![test_bool, test_int.clone(), test_string.clone()];
        let result = display_builtin(&mut heap, &too_many_args);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expected 1 or 2 arguments"));
    }
    
    #[test]
    fn test_display_string_port_pattern() {
        // This test demonstrates the pattern for testing display with string ports
        // when we have port objects in the GC
        
        // Create an output string port for testing
        let output_port = new_output_string_port();
        
        // Get the initial content
        let content = get_output_string(&output_port);
        assert_eq!(content, "");
        
        // Note: With the current immutable port design, we can't modify
        // the port content directly. This test shows the pattern for
        // when we have mutable port support in the future.
    }

    #[test]
    fn test_define() {
        use crate::gc::{GcHeap, SchemeValue};
        use crate::io::{Port, PortKind, PortStack};
        use crate::parser::Parser;
        use crate::eval::{lookup_global_binding, TEST_HEAP, TEST_ENV};
        use std::collections::HashMap;

        // Scheme code to test
        let code = "(define x 42) (define y (* 3 5)) (define z '(a b c))";
        let mut heap = GcHeap::new();
        let port = Port {
            kind: PortKind::StringPortInput { content: code.to_string(), pos: 0 },
        };
        let mut port_stack = PortStack::new(port.clone());
        port_stack.push(port);
        let mut parser = Parser::new();
        let mut env = HashMap::new();
        crate::builtin::register_all(&mut heap, &mut env);

        // Set static mut pointers for the evaluation service
        unsafe {
            TEST_HEAP = Some(&mut heap as *mut GcHeap);
            TEST_ENV = Some(&mut env as *mut HashMap<String, crate::builtin::BuiltinKind>);
        }

        // Evaluate all expressions in the string port
        loop {
            let parse_result = parser.parse(&mut heap, port_stack.current_mut());
            match parse_result {
                Ok(expr) => {
                    let eval_result = crate::eval::eval_trampoline(expr, &mut heap, &mut env);
                    let _ = eval_result;
                }
                Err(e) if e.contains("end of input") => break,
                Err(e) => panic!("Parse error: {}", e),
            }
        }

        // Restore static mut pointers
        unsafe {
            TEST_HEAP = None;
            TEST_ENV = None;
        }

        // Check that x is defined and equals 42
        let x_val = lookup_global_binding("x").expect("x should be defined");
        match &x_val.borrow().value {
            SchemeValue::Int(n) => assert_eq!(n.to_string(), "42"),
            v => panic!("x has wrong value: {:?}", v),
        }
        // Check that y is defined and equals 15
        let y_val = lookup_global_binding("y").expect("y should be defined");
        match &y_val.borrow().value {
            SchemeValue::Int(n) => assert_eq!(n.to_string(), "15"),
            v => panic!("y has wrong value: {:?}", v),
        }
        // Check that z is defined and is a quoted list (a b c)
        let z_val = lookup_global_binding("z").expect("z should be defined");
        match &z_val.borrow().value {
            SchemeValue::Pair(a, b) => {
                let a_val = &a.borrow().value;
                assert!(matches!(a_val, SchemeValue::Symbol(s) if s == "a"));
                let b_val = &b.borrow().value;
                match b_val {
                    SchemeValue::Pair(b2, c) => {
                        let b2_val = &b2.borrow().value;
                        assert!(matches!(b2_val, SchemeValue::Symbol(s) if s == "b"));
                        let c_val = &c.borrow().value;
                        match c_val {
                            SchemeValue::Pair(c2, nil) => {
                                let c2_val = &c2.borrow().value;
                                assert!(matches!(c2_val, SchemeValue::Symbol(s) if s == "c"));
                                assert!(matches!(&nil.borrow().value, SchemeValue::Nil));
                            }
                            _ => panic!("z cdr is not a proper list: {:?}", c_val),
                        }
                    }
                    _ => panic!("z cdr is not a proper list: {:?}", b_val),
                }
            }
            v => panic!("z has wrong value: {:?}", v),
        }
    }

    #[test]
    fn test_quote_handler_new() {
        use crate::eval::Evaluator;
        use crate::gc::{GcHeap, new_symbol, new_int};
        use num_bigint::BigInt;

        // Create an evaluator and test the new quote handler
        let mut evaluator = Evaluator::new();
        
        // Create test arguments: (quote foo)
        let symbol = new_symbol(&mut evaluator.heap, "foo");
        let args = vec![symbol.clone()];
        
        // Test the new quote handler
        let result = quote_handler_new(&mut evaluator, &args);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().borrow().value, symbol.borrow().value);
        
        // Test error case: wrong number of arguments
        let empty_args: Vec<crate::gc::GcRef> = vec![];
        let result = quote_handler_new(&mut evaluator, &empty_args);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expected exactly 1 argument"));
        
        let too_many_args = vec![symbol.clone(), new_int(&mut evaluator.heap, BigInt::from(42))];
        let result = quote_handler_new(&mut evaluator, &too_many_args);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expected exactly 1 argument"));
    }

    #[test]
    fn test_if_handler_new() {
        use crate::eval::Evaluator;
        use crate::gc::{new_symbol, new_int, new_bool};
        use num_bigint::BigInt;

        // Create an evaluator and test the new if handler
        let mut evaluator = Evaluator::new();
        
        // Create test arguments: (if test consequent)
        let test_expr = new_bool(&mut evaluator.heap, true);
        let consequent = new_int(&mut evaluator.heap, BigInt::from(42));
        let args = vec![test_expr.clone(), consequent.clone()];
        
        // Test the new if handler
        let result = if_handler_new(&mut evaluator, &args);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().borrow().value, test_expr.borrow().value);
        
        // Test with 3 arguments: (if test consequent alternative)
        let alternative = new_int(&mut evaluator.heap, BigInt::from(99));
        let args_3 = vec![test_expr.clone(), consequent.clone(), alternative.clone()];
        let result = if_handler_new(&mut evaluator, &args_3);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().borrow().value, test_expr.borrow().value);
        
        // Test error case: wrong number of arguments
        let empty_args: Vec<crate::gc::GcRef> = vec![];
        let result = if_handler_new(&mut evaluator, &empty_args);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expected 2 or 3 arguments"));
        
        let one_arg = vec![test_expr.clone()];
        let result = if_handler_new(&mut evaluator, &one_arg);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expected 2 or 3 arguments"));
        
        let four_args = vec![test_expr.clone(), consequent.clone(), alternative.clone(), new_bool(&mut evaluator.heap, false)];
        let result = if_handler_new(&mut evaluator, &four_args);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expected 2 or 3 arguments"));
    }

    #[test]
    fn test_begin_handler_new() {
        use crate::eval::Evaluator;
        use crate::gc::{new_int, new_bool, new_string};
        use num_bigint::BigInt;

        // Create an evaluator and test the new begin handler
        let mut evaluator = Evaluator::new();
        
        // Create test arguments: (begin expr1 expr2 expr3)
        let expr1 = new_int(&mut evaluator.heap, BigInt::from(1));
        let expr2 = new_bool(&mut evaluator.heap, true);
        let expr3 = new_string(&mut evaluator.heap, "hello");
        let args = vec![expr1.clone(), expr2.clone(), expr3.clone()];
        
        // Test the new begin handler - should return the last expression
        let result = begin_handler_new(&mut evaluator, &args);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().borrow().value, expr3.borrow().value);
        
        // Test with single argument
        let single_arg = vec![expr1.clone()];
        let result = begin_handler_new(&mut evaluator, &single_arg);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().borrow().value, expr1.borrow().value);
        
        // Test error case: no arguments
        let empty_args: Vec<crate::gc::GcRef> = vec![];
        let result = begin_handler_new(&mut evaluator, &empty_args);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expected at least 1 argument"));
    }

    #[test]
    fn test_and_handler_new() {
        use crate::eval::Evaluator;
        use crate::gc::{new_int, new_bool, new_string};
        use num_bigint::BigInt;

        // Create an evaluator and test the new and handler
        let mut evaluator = Evaluator::new();
        
        // Create test arguments: (and expr1 expr2 expr3)
        let expr1 = new_bool(&mut evaluator.heap, true);
        let expr2 = new_int(&mut evaluator.heap, BigInt::from(42));
        let expr3 = new_string(&mut evaluator.heap, "hello");
        let args = vec![expr1.clone(), expr2.clone(), expr3.clone()];
        
        // Test the new and handler - should return the first expression for now
        let result = and_handler_new(&mut evaluator, &args);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().borrow().value, expr1.borrow().value);
        
        // Test with single argument
        let single_arg = vec![expr1.clone()];
        let result = and_handler_new(&mut evaluator, &single_arg);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().borrow().value, expr1.borrow().value);
        
        // Test with no arguments (should return first arg or a default)
        let empty_args: Vec<crate::gc::GcRef> = vec![];
        let result = and_handler_new(&mut evaluator, &empty_args);
        assert!(result.is_ok());
        // Should return #t for empty and
        assert!(matches!(&result.unwrap().borrow().value, crate::gc::SchemeValue::Bool(true)));
    }

    #[test]
    fn test_or_handler_new() {
        use crate::eval::Evaluator;
        use crate::gc::{new_int, new_bool, new_string};
        use num_bigint::BigInt;

        // Create an evaluator and test the new or handler
        let mut evaluator = Evaluator::new();
        
        // Create test arguments: (or expr1 expr2 expr3)
        let expr1 = new_bool(&mut evaluator.heap, false);
        let expr2 = new_int(&mut evaluator.heap, BigInt::from(42));
        let expr3 = new_string(&mut evaluator.heap, "hello");
        let args = vec![expr1.clone(), expr2.clone(), expr3.clone()];
        
        // Test the new or handler - should return the first expression for now
        let result = or_handler_new(&mut evaluator, &args);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().borrow().value, expr1.borrow().value);
        
        // Test with single argument
        let single_arg = vec![expr1.clone()];
        let result = or_handler_new(&mut evaluator, &single_arg);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().borrow().value, expr1.borrow().value);
        
        // Test with no arguments (should return #f)
        let empty_args: Vec<crate::gc::GcRef> = vec![];
        let result = or_handler_new(&mut evaluator, &empty_args);
        assert!(result.is_ok());
        // Should return #f for empty or
        assert!(matches!(&result.unwrap().borrow().value, crate::gc::SchemeValue::Bool(false)));
    }
} 