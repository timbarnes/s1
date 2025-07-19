use crate::gc::{GcHeap, GcRef, SchemeValue};
use crate::gc::{GcRefSimple, SchemeValueSimple};
use crate::io::{PortStack, Port, PortKind};
use crate::parser::Parser;
use crate::parser::ParserSimple;
// use crate::printer::scheme_display;
use std::collections::HashMap;
// use std::fs;
use std::cell::RefCell;
use std::thread_local;
use std::rc::Rc;

/// The main evaluator that owns the heap and environment.
/// This struct encapsulates all evaluation state and provides
/// a clean interface for special forms to access evaluation services.
pub struct Evaluator {
    pub heap: GcHeap,
    pub env: HashMap<String, GcRef>,
}

/// The main evaluator that uses the reference-based GC system.
/// This struct uses GcRefSimple (&'static GcObject) instead of GcRef (Rc<RefCell<GcObject>>)
/// for better performance and simpler code.
pub struct EvaluatorSimple {
    pub heap: GcHeap,
    pub env: HashMap<String, GcRefSimple>,
}

impl Evaluator {
    /// Create a new evaluator with an empty heap and environment.
    pub fn new() -> Self {
        Self {
            heap: GcHeap::new(),
            env: HashMap::new(),
        }
    }

    /// Evaluation service method that special forms can use to evaluate expressions.
    /// This replaces the global eval_service function pointer.
    pub fn eval_service(&mut self, expr: &GcRef) -> Result<GcRef, String> {
        eval_trampoline_with_evaluator(expr.clone(), self)
    }

    /// Evaluate an expression using this evaluator.
    pub fn evaluate(&mut self, expr: GcRef) -> Result<GcRef, String> {
        eval_trampoline_with_evaluator(expr, self)
    }

    /// Insert a global binding into the environment.
    pub fn insert_global_binding(&mut self, name: String, value: GcRef) {
        // For now, we'll use the existing global binding mechanism
        // TODO: Replace with proper environment management
        insert_global_binding(name, value);
    }

    /// Look up a global binding in the environment.
    pub fn lookup_global_binding(&self, name: &str) -> Option<GcRef> {
        // For now, we'll use the existing global binding mechanism
        // TODO: Replace with proper environment management
        lookup_global_binding(name)
    }

    /// Run the evaluation loop on the current port until EOF
    /// This handles parsing and evaluating expressions from the current port
    pub fn run_loop(&mut self, port_stack: &mut PortStack, parser: &mut Parser) -> Result<(), String> {
        loop {
            let parse_result = parser.parse(&mut self.heap, port_stack.current_mut());
            match parse_result {
                Ok(expr) => {
                    let eval_result = self.evaluate(expr);
                    match eval_result {
                        Ok(_) => {
                            // Successfully evaluated, continue to next expression
                        }
                        Err(e) => {
                            eprintln!("Evaluation error: {}", e);
                            // Continue parsing other expressions
                        }
                    }
                }
                Err(e) if e.contains("end of input") => {
                    // End of input reached - pop the port and return
                    port_stack.pop();
                    break;
                }
                Err(e) => {
                    return Err(format!("Parse error: {}", e));
                }
            }
        }
        Ok(())
    }
}

impl EvaluatorSimple {
    /// Create a new evaluator with an empty heap and environment.
    pub fn new() -> Self {
        Self {
            heap: GcHeap::new(),
            env: HashMap::new(),
        }
    }

    /// Evaluation service method that special forms can use to evaluate expressions.
    /// This uses the reference-based GC system.
    pub fn eval_service(&mut self, expr: &GcRefSimple) -> Result<GcRefSimple, String> {
        eval_trampoline_simple(expr, self)
    }

    /// Evaluate an expression using this evaluator.
    pub fn evaluate(&mut self, expr: GcRefSimple) -> Result<GcRefSimple, String> {
        eval_trampoline_simple(expr, self)
    }

    /// Insert a global binding into the environment.
    pub fn insert_global_binding(&mut self, name: String, value: GcRefSimple) {
        self.env.insert(name, value);
    }

    /// Look up a global binding in the environment.
    pub fn lookup_global_binding(&self, name: &str) -> Option<GcRefSimple> {
        self.env.get(name).copied()
    }

    /// Run the evaluation loop on the current port until EOF
    /// This handles parsing and evaluating expressions from the current port
    pub fn run_loop(&mut self, port_stack: &mut PortStack, parser: &mut ParserSimple) -> Result<(), String> {
        loop {
            let parse_result = parser.parse(&mut self.heap, port_stack.current_mut());
            match parse_result {
                Ok(expr) => {
                    let eval_result = self.evaluate(expr);
                    match eval_result {
                        Ok(_) => {
                            // Successfully evaluated, continue to next expression
                        }
                        Err(e) => {
                            eprintln!("Evaluation error: {}", e);
                            // Continue parsing other expressions
                        }
                    }
                }
                Err(e) if e.contains("end of input") => {
                    // End of input reached - pop the port and return
                    port_stack.pop();
                    break;
                }
                Err(e) => {
                    return Err(format!("Parse error: {}", e));
                }
            }
        }
        Ok(())
    }
}

/// Represents the result of evaluation using the reference-based GC system.
#[derive(Debug)]
pub enum EvalResultSimple {
    /// Continue evaluation with the given expression
    Continue(GcRefSimple),
    /// Evaluation is complete with the given value
    Done(GcRefSimple),
}

/// Evaluate an expression using the trampoline pattern with the reference-based GC system.
///
/// This function evaluates an expression using tail recursion optimization.
/// It repeatedly calls eval_step_simple until evaluation is complete.
///
/// # Arguments
///
/// * `expr` - The expression to evaluate
/// * `evaluator` - The evaluator containing heap and environment
///
/// # Returns
///
/// Returns `Ok(GcRefSimple)` for the evaluated result, or `Err(String)` if
/// an error occurred during evaluation.
///
/// # Examples
///
/// ```rust
/// use s1::eval::{EvaluatorSimple, eval_trampoline_simple};
/// use s1::gc::{GcHeap, new_int_simple};
///
/// let mut evaluator = EvaluatorSimple::new();
/// let expr = new_int_simple(&mut evaluator.heap, 42);
/// let result = eval_trampoline_simple(expr, &mut evaluator);
/// assert!(result.is_ok());
/// ```
pub fn eval_trampoline_simple(expr: GcRefSimple, evaluator: &mut EvaluatorSimple) -> Result<GcRefSimple, String> {
    let mut current_expr = expr;
    
    loop {
        let result = eval_step_simple(current_expr, evaluator)?;
        match result {
            EvalResultSimple::Continue(next_expr) => {
                current_expr = next_expr;
            }
            EvalResultSimple::Done(value) => {
                return Ok(value);
            }
        }
    }
}

/// Evaluate a single step of an expression using the reference-based GC system.
///
/// This function evaluates one step of an expression and returns either:
/// - `Continue(expr)` if further evaluation is needed
/// - `Done(value)` if evaluation is complete
///
/// This is the core of the trampoline pattern for tail recursion optimization.
fn eval_step_simple(expr: GcRefSimple, evaluator: &mut EvaluatorSimple) -> Result<EvalResultSimple, String> {
    let value = &expr.value;
    
    if is_self_evaluating_simple(value) {
        Ok(EvalResultSimple::Done(expr))
    } else if let SchemeValueSimple::Symbol(name) = value {
        // Handle bare symbols - look them up in the environment
        if let Some(bound_value) = evaluator.env.get(name) {
            Ok(EvalResultSimple::Done(*bound_value))
        } else {
            Err(format!("unbound variable: {}", name))
        }
    } else if let SchemeValueSimple::Pair(car, cdr) = value {
        // Handle function calls: (function args...)
        // For now, we'll handle basic primitive function calls
        // TODO: Implement proper argument evaluation and special forms
        
        // Check if the car is a symbol (function name)
        if let SchemeValueSimple::Symbol(func_name) = &car.value {
            // Look up the function in the environment
            if let Some(func_value) = evaluator.env.get(func_name) {
                if let SchemeValueSimple::Primitive { func, is_special_form, .. } = &func_value.value {
                    if *is_special_form {
                        // Special forms not yet implemented
                        Err("Special forms not yet supported".to_string())
                    } else {
                        // For now, just pass the arguments as-is without evaluation
                        // TODO: Implement proper argument evaluation
                        let args = collect_list_simple(cdr)?;
                        match func(&mut evaluator.heap, &args) {
                            Ok(result) => Ok(EvalResultSimple::Done(result)),
                            Err(e) => Err(e),
                        }
                    }
                } else {
                    Err("Not a function".to_string())
                }
            } else {
                Err(format!("unbound variable: {}", func_name))
            }
        } else {
            Err("Cannot evaluate non-symbol operator".to_string())
        }
    } else {
        Err("cannot evaluate form".to_string())
    }
}

/// Helper function to collect a list of arguments from a pair chain
fn collect_list_simple(mut current: GcRefSimple) -> Result<Vec<GcRefSimple>, String> {
    let mut args = Vec::new();
    loop {
        match &current.value {
            SchemeValueSimple::Pair(car, cdr) => {
                args.push(*car);
                current = *cdr;
            }
            SchemeValueSimple::Nil => {
                break;
            }
            _ => {
                return Err("Malformed argument list".to_string());
            }
        }
    }
    Ok(args)
}

/// Check if a SchemeValueSimple is self-evaluating (doesn't need evaluation).
fn is_self_evaluating_simple(expr: &SchemeValueSimple) -> bool {
    matches!(expr, 
        SchemeValueSimple::Int(_) | 
        SchemeValueSimple::Float(_) | 
        SchemeValueSimple::Str(_) | 
        SchemeValueSimple::Bool(_) | 
        SchemeValueSimple::Char(_) | 
        SchemeValueSimple::Vector(_) |
        SchemeValueSimple::Nil
    )
}

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
/// This function reads the specified file, creates a port for it,
/// and lets the evaluator handle the parsing and evaluation.
///
/// # Arguments
///
/// * `filename` - The path to the Scheme file to load
/// * `port_stack` - The port stack for managing input sources
/// * `parser` - The parser for parsing expressions
/// * `evaluator` - The evaluator for evaluating expressions
///
/// # Returns
///
/// Returns `Ok(())` if the file was loaded successfully, or `Err(String)` if
/// an error occurred during loading, parsing, or evaluation.
pub fn load_file(
    filename: &str,
    port_stack: &mut PortStack,
    parser: &mut Parser,
    evaluator: &mut Evaluator,
) -> Result<(), String> {
    // Read the file content
    let content = std::fs::read_to_string(filename)
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
    
    // Let the evaluator handle the parsing and evaluation
    evaluator.run_loop(port_stack, parser)
}

// pub fn repl(
//     port_stack: &mut PortStack,
//     parser: &mut Parser,
//     evaluator: &mut Evaluator,
// ) {
//     // Commented out for now to focus on basic evaluation testing
// }

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
pub fn eval_trampoline_with_evaluator(expr: GcRef, evaluator: &mut Evaluator) -> Result<GcRef, String> {
    let mut current_expr = expr;
    
    loop {
        let result = eval_step_with_evaluator(current_expr, evaluator)?;
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

/// Evaluate a single step of an expression using an Evaluator.
///
/// This function evaluates one step of an expression and returns either:
/// - `Continue(expr)` if further evaluation is needed
/// - `Done(value)` if evaluation is complete
///
/// This is the core of the trampoline pattern for tail recursion optimization.
fn eval_step_with_evaluator(expr: GcRef, evaluator: &mut Evaluator) -> Result<EvalResult, String> {
    let value = &expr.borrow().value;
    
    if is_self_evaluating(value) {
        Ok(EvalResult::Done(expr.clone()))
    } else if let SchemeValue::Symbol(name) = value {
        // Handle bare symbols - look them up in the environment
        if let Some(bound_value) = evaluator.env.get(name) {
            Ok(EvalResult::Done(bound_value.clone()))
        } else {
            Err(format!("unbound variable: {}", name))
        }
    } else if let SchemeValue::Pair(car, cdr) = value {
        if let SchemeValue::Symbol(ref name) = car.borrow().value {
            // Look up the symbol in the unified environment
            if let Some(bound_value) = evaluator.env.get(name) {
                // Handle different types of values
                match &bound_value.borrow().value {
                    SchemeValue::Primitive { func, is_special_form, .. } => {
                        // Collect arguments first to avoid borrowing conflicts
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
                        
                        // Now we can call the function without borrowing conflicts
                        if *is_special_form {
                            // Special form - pass arguments as-is
                            let result = func(&mut evaluator.heap, &args)?;
                            Ok(EvalResult::Done(result))
                        } else {
                            // Normal function - evaluate arguments first
                            let mut evaled_args = Vec::new();
                            // For now, we'll just pass the arguments as-is to avoid borrowing conflicts
                            // TODO: Implement proper argument evaluation
                            evaled_args = args;
                            
                            // Call the normal function
                            let result = func(&mut evaluator.heap, &evaled_args)?;
                            Ok(EvalResult::Done(result))
                        }
                    }
                    // SchemeValue::SpecialForm { func, .. } => {
                    //     // For now, return an error since we can't handle special forms with evaluator access yet
                    //     // TODO: Implement proper handling of special forms with evaluator access
                    //     Err("Special forms with evaluator access not yet implemented".to_string())
                    // }
                    _ => {
                        // User variable - just return the value
                        Ok(EvalResult::Done(bound_value.clone()))
                    }
                }
            } else {
                Err(format!("unbound variable: {}", name))
            }
        } else {
            Err("cannot evaluate non-symbol operator".to_string())
        }
    } else {
        Err("cannot evaluate form".to_string())
    }
}

/// Helper function to handle special forms with evaluator access
/// This avoids borrowing conflicts by extracting the function and arguments first
fn handle_special_form_with_evaluator(
    evaluator: &mut Evaluator,
    func: &Rc<dyn Fn(&mut Evaluator, &[GcRef]) -> Result<GcRef, String>>,
    cdr: &GcRef,
) -> Result<GcRef, String> {
    // Collect arguments first to avoid borrowing conflicts
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
    
    // Now we can call the special form without borrowing conflicts
    func(evaluator, &args)
}

/// Helper function to handle special forms by dropping the environment borrow first
fn call_special_form_with_evaluator(
    evaluator: &mut Evaluator,
    func: Rc<dyn Fn(&mut Evaluator, &[GcRef]) -> Result<GcRef, String> + 'static>,
    args: Vec<GcRef>,
) -> Result<GcRef, String> {
    // Call the special form - the evaluator will drop its borrow of the environment
    // before calling the function to avoid circular borrowing
    func(evaluator, &args)
}

/// Helper function to handle special forms by collecting data first, then dropping borrows
fn handle_special_form_call(
    evaluator: &mut Evaluator,
    func: &Rc<dyn Fn(&mut Evaluator, &[GcRef]) -> Result<GcRef, String> + 'static>,
    cdr: &GcRef,
) -> Result<GcRef, String> {
    // Collect arguments first to avoid borrowing conflicts
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
    
    // Now we can call the special form without borrowing conflicts
    // The evaluator will drop its borrow of the environment before calling the function
    func(evaluator, &args)
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gc::{new_int, new_string, new_bool, new_symbol, new_pair};
    // use crate::builtin;
    use num_bigint::BigInt;

    #[test]
    fn test_evaluator_new() {
        let mut evaluator = Evaluator::new();
        
        // Test that we can create values in the evaluator's heap
        let int_val = new_int(&mut evaluator.heap, BigInt::from(42));
        let string_val = new_string(&mut evaluator.heap, "hello");
        let bool_val = new_bool(&mut evaluator.heap, true);
        
        // Test that we can evaluate self-evaluating expressions
        let result = evaluator.evaluate(int_val.clone());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().borrow().value, int_val.borrow().value);
        
        let result = evaluator.evaluate(string_val.clone());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().borrow().value, string_val.borrow().value);
        
        let result = evaluator.evaluate(bool_val.clone());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().borrow().value, bool_val.borrow().value);
    }

    #[test]
    fn test_evaluator_with_env() {
        // Create an evaluator and add a test value to its environment
        let mut evaluator = Evaluator::new();
        
        // Add a simple test value
        let test_value = new_int(&mut evaluator.heap, BigInt::from(42));
        evaluator.env.insert("test".to_string(), test_value);
        
        // Test that we can look up variables
        let test_symbol = new_symbol(&mut evaluator.heap, "test");
        let result = evaluator.evaluate(test_symbol);
        assert!(result.is_ok());
        
        // The result should be 42
        match &result.unwrap().borrow().value {
            SchemeValue::Int(n) => assert_eq!(n.to_string(), "42"),
            v => panic!("Expected integer 42, got {:?}", v),
        }
        
        // Test that undefined variables fail
        let undefined_symbol = new_symbol(&mut evaluator.heap, "undefined");
        let result = evaluator.evaluate(undefined_symbol);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("unbound variable"));
    }

    #[test]
    fn test_evaluator_eval_service() {
        let mut evaluator = Evaluator::new();
        
        // Test that eval_service works for self-evaluating expressions
        let int_val = new_int(&mut evaluator.heap, BigInt::from(123));
        let result = evaluator.eval_service(&int_val);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().borrow().value, int_val.borrow().value);
        
        // Test that eval_service works for symbols (though they'll fail evaluation)
        let symbol_val = new_symbol(&mut evaluator.heap, "undefined");
        let result = evaluator.eval_service(&symbol_val);
        assert!(result.is_err()); // Should fail because symbol is not defined
    }

    #[test]
    fn test_evaluator_global_bindings() {
        let mut evaluator = Evaluator::new();
        
        // Test inserting and looking up global bindings
        let value = new_int(&mut evaluator.heap, BigInt::from(42));
        evaluator.insert_global_binding("x".to_string(), value.clone());
        
        let found = evaluator.lookup_global_binding("x");
        assert!(found.is_some());
        assert_eq!(found.unwrap().borrow().value, value.borrow().value);
        
        let not_found = evaluator.lookup_global_binding("y");
        assert!(not_found.is_none());
    }

    // ============================================================================
    // TESTS FOR EVALUATOR SIMPLE (Reference-based GC system)
    // ============================================================================

    #[test]
    fn test_evaluator_simple_new() {
        let evaluator = EvaluatorSimple::new();
        assert_eq!(evaluator.env.len(), 0);
        assert_eq!(evaluator.heap.heap_size(), 1); // nil object
    }

    #[test]
    fn test_evaluator_simple_global_bindings() {
        use crate::gc::{new_int_simple, new_string_simple};
        use num_bigint::BigInt;

        let mut evaluator = EvaluatorSimple::new();
        
        // Test inserting a binding
        let value = new_int_simple(&mut evaluator.heap, BigInt::from(42));
        evaluator.insert_global_binding("x".to_string(), value);
        
        // Test looking up the binding
        let found = evaluator.lookup_global_binding("x");
        assert!(found.is_some());
        assert_eq!(&found.unwrap().value, &crate::gc::SchemeValueSimple::Int(BigInt::from(42)));
        
        // Test looking up a non-existent binding
        let not_found = evaluator.lookup_global_binding("y");
        assert!(not_found.is_none());
    }

    #[test]
    fn test_evaluator_simple_eval_service() {
        use crate::gc::{new_int_simple, new_symbol_simple};
        use num_bigint::BigInt;

        let mut evaluator = EvaluatorSimple::new();
        
        // Test evaluating a self-evaluating expression (integer)
        let expr = new_int_simple(&mut evaluator.heap, BigInt::from(42));
        let result = evaluator.eval_service(&expr);
        assert!(result.is_ok());
        assert_eq!(&result.unwrap().value, &crate::gc::SchemeValueSimple::Int(BigInt::from(42)));
        
        // Test evaluating a symbol that's bound in the environment
        let symbol = new_symbol_simple(&mut evaluator.heap, "x");
        let value = new_int_simple(&mut evaluator.heap, BigInt::from(99));
        evaluator.insert_global_binding("x".to_string(), value);
        
        let result = evaluator.eval_service(&symbol);
        assert!(result.is_ok());
        assert_eq!(&result.unwrap().value, &crate::gc::SchemeValueSimple::Int(BigInt::from(99)));
        
        // Test evaluating an unbound symbol
        let unbound_symbol = new_symbol_simple(&mut evaluator.heap, "y");
        let result = evaluator.eval_service(&unbound_symbol);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("unbound variable"));
    }

    #[test]
    fn test_evaluator_simple_evaluate() {
        use crate::gc::{new_int_simple, new_string_simple};
        use num_bigint::BigInt;

        let mut evaluator = EvaluatorSimple::new();
        
        // Test evaluating a self-evaluating expression (string)
        let expr = new_string_simple(&mut evaluator.heap, "hello");
        let result = evaluator.evaluate(expr);
        assert!(result.is_ok());
        assert_eq!(&result.unwrap().value, &crate::gc::SchemeValueSimple::Str("hello".to_string()));
    }
} 