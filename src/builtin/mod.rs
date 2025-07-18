pub mod number;
pub mod predicate;

use crate::gc::{GcHeap, GcRef, new_string, new_bool};
use num_bigint::BigInt;
use crate::gc::SchemeValue;
use number::{plus_builtin, minus_builtin, times_builtin, div_builtin, mod_builtin};
use crate::printer::scheme_display;
use crate::eval::Evaluator;

pub enum BuiltinKind {
    // New special form signature using Evaluator
    SpecialFormNew(fn(&mut Evaluator, &[GcRef]) -> Result<GcRef, String>),
    Normal(fn(&mut GcHeap, &[GcRef]) -> Result<GcRef, String>),
}

// No Clone implementation needed for BuiltinKind with function pointers

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

/// New define handler using Evaluator interface
pub fn define_handler_new(evaluator: &mut Evaluator, args: &[GcRef]) -> Result<GcRef, String> {
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
    
    // Evaluate the expression using the evaluator's eval_service
    let evaluated_value = evaluator.eval_service(expr)?;
    
    // Store the evaluated value in the evaluator's environment
    evaluator.insert_global_binding(symbol_name.clone(), evaluated_value.clone());
    
    // Return the evaluated value
    Ok(evaluated_value)
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
    env.insert("define".to_string(), BuiltinKind::SpecialFormNew(define_handler_new));
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

    #[test]
    fn test_define_handler_new() {
        use crate::eval::Evaluator;
        use crate::gc::{new_symbol, new_int, new_string, new_pair};
        use crate::builtin;
        use num_bigint::BigInt;

        // Create an evaluator with builtins registered
        let mut evaluator = Evaluator::new();
        builtin::register_all(&mut evaluator.heap, &mut evaluator.env);
        
        // Test defining a simple value: (define x 42)
        let symbol = new_symbol(&mut evaluator.heap, "x");
        let value = new_int(&mut evaluator.heap, BigInt::from(42));
        let args = vec![symbol.clone(), value.clone()];
        
        // Test the new define handler
        let result = define_handler_new(&mut evaluator, &args);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().borrow().value, value.borrow().value);
        
        // Verify the binding was created
        let found = evaluator.lookup_global_binding("x");
        assert!(found.is_some());
        assert_eq!(found.unwrap().borrow().value, value.borrow().value);
        
        // Test defining a computed value: (define y (+ 3 4))
        let symbol_y = new_symbol(&mut evaluator.heap, "y");
        let plus_symbol = new_symbol(&mut evaluator.heap, "+");
        let arg1 = new_int(&mut evaluator.heap, BigInt::from(3));
        let arg2 = new_int(&mut evaluator.heap, BigInt::from(4));
        
        // Create the expression (+ 3 4)
        let nil = evaluator.heap.nil();
        let arg2_pair = new_pair(&mut evaluator.heap, arg2.clone(), nil);
        let arg_list = new_pair(&mut evaluator.heap, arg1.clone(), arg2_pair);
        let plus_expr = new_pair(&mut evaluator.heap, plus_symbol, arg_list);
        
        let args_computed = vec![symbol_y.clone(), plus_expr];
        let result = define_handler_new(&mut evaluator, &args_computed);
        assert!(result.is_ok());
        
        // The result should be 7
        match &result.unwrap().borrow().value {
            crate::gc::SchemeValue::Int(n) => assert_eq!(n.to_string(), "7"),
            v => panic!("Expected integer 7, got {:?}", v),
        }
        
        // Verify the binding was created
        let found = evaluator.lookup_global_binding("y");
        assert!(found.is_some());
        match &found.unwrap().borrow().value {
            crate::gc::SchemeValue::Int(n) => assert_eq!(n.to_string(), "7"),
            v => panic!("Expected integer 7, got {:?}", v),
        }
        
        // Test error cases
        let empty_args: Vec<crate::gc::GcRef> = vec![];
        let result = define_handler_new(&mut evaluator, &empty_args);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expected exactly 2 arguments"));
        
        let one_arg = vec![symbol.clone()];
        let result = define_handler_new(&mut evaluator, &one_arg);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expected exactly 2 arguments"));
        
        let three_args = vec![symbol.clone(), value.clone(), new_string(&mut evaluator.heap, "extra")];
        let result = define_handler_new(&mut evaluator, &three_args);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expected exactly 2 arguments"));
        
        // Test error: first argument not a symbol
        let bad_args = vec![value.clone(), symbol.clone()];
        let result = define_handler_new(&mut evaluator, &bad_args);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("first argument must be a symbol"));
    }
} 