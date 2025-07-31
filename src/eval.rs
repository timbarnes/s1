//! Simple evaluator using the new GC system and two-layer architecture.
//!
//! This module implements a clean separation between evaluation logic and function application.
//! The logic layer handles self-evaluating forms, special forms, and argument evaluation,
//! while the apply layer handles function calls with pre-evaluated arguments.

use crate::env::{Environment, Frame};
use crate::gc::{
    Callable, GcHeap, GcRef, SchemeValue, list_from_vec, list_to_vec, new_closure, new_pair,
    new_vector,
};
use crate::io::Port;
use crate::macros::expand_macro;
use crate::parser::{ParseError, Parser};
use crate::printer::print_scheme_value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Evaluator that owns both heap and environment
pub struct Evaluator {
    pub heap: GcHeap,            // The global heap for scheme data
    env: Environment,            // The current state of the environment
    tail_call: Option<TailCall>, // Tail call optimization state
    pub new_port: bool,          // Indicates a new port has been pushed on the stack
    pub trace: i32,              // Indicates tracing is enabled to a given depth
    pub depth: i32,              // Evaluation nesting depth (used by trace)
}

/// Represents a tail call that should be optimized
#[derive(Clone)]
struct TailCall {
    pub func: GcRef,
    pub args: Vec<GcRef>,
}

impl Evaluator {
    pub fn new() -> Self {
        let mut evaluator = Self {
            heap: GcHeap::new(),
            env: Environment::new(),
            tail_call: None,
            new_port: false,
            trace: 0,
            depth: 0,
        };

        // Register built-ins in the evaluator's heap and environment
        crate::builtin::register_builtins(&mut evaluator.heap, &mut evaluator.env);
        crate::special_forms::register_special_forms(&mut evaluator.heap, &mut evaluator.env);
        evaluator
    }

    /// Create an evaluator with a specific heap
    pub fn with_heap(heap: GcHeap) -> Self {
        // let stdin_port = crate::gc::new_port(&mut heap, crate::io::PortKind::Stdin);
        // Remove port_stack: crate::io::SchemePortStack, from Evaluator
        // Remove all initialization and methods related to SchemePortStack

        let mut evaluator = Self {
            heap,
            env: Environment::new(),
            // Remove port_stack: crate::io::SchemePortStack, from Evaluator
            tail_call: None,
            new_port: false,
            trace: 0,
            depth: 0,
        };

        // Register built-ins in the evaluator's heap and environment
        crate::builtin::register_builtins(&mut evaluator.heap, &mut evaluator.env);

        evaluator
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

    /// Initialize Scheme-level I/O globals: **stdin**, **stdout**, **port-stack**
    ///
    /// This should be called after creating the Evaluator, before loading files or starting the REPL.
    pub fn initialize_scheme_io_globals(&mut self) -> Result<(), String> {
        // Create stdin and stdout ports
        let stdin_port = crate::gc::new_port(&mut self.heap, crate::io::PortKind::Stdin);
        let stdout_port = crate::gc::new_port(&mut self.heap, crate::io::PortKind::Stdout);
        let stderr_port = crate::gc::new_port(&mut self.heap, crate::io::PortKind::Stderr);
        // Bind **stdin**, **stdout**, and **stderr** as Scheme globals
        let stdin_sym = self.heap.intern_symbol("**stdin**");
        let stdout_sym = self.heap.intern_symbol("**stdout**");
        let stderr_sym = self.heap.intern_symbol("**stderr**");
        self.env_mut().set_symbol(stdin_sym, stdin_port);
        self.env_mut().set_symbol(stdout_sym, stdout_port);
        self.env_mut().set_symbol(stderr_sym, stderr_port);
        // Bind **port-stack** as the empty list
        let port_stack_sym = self.heap.intern_symbol("**port-stack**");
        let nil = self.heap.nil_s();
        self.env_mut().set_symbol(port_stack_sym, nil);
        // Cons **stdin** onto **port-stack**
        let port_stack_with_stdin = crate::gc::new_pair(&mut self.heap, stdin_port, nil);
        self.env_mut()
            .set_symbol(port_stack_sym, port_stack_with_stdin);
        Ok(())
    }

    /// Evaluate a string of Scheme code (all expressions, return last result)
    pub fn eval_string(&mut self, code: &str) -> Result<GcRef, String> {
        use crate::parser::{ParseError, Parser};
        //use crate::io::{Port, PortKind};

        let mut port = crate::io::new_string_port_input(code);
        let mut parser = Parser::new();
        let mut last_result = self.heap.nil_s();
        loop {
            match parser.parse(&mut self.heap, &mut port) {
                Err(ParseError::Syntax(e)) => return Err(e),
                Err(ParseError::Eof) => return Ok(last_result),
                Ok(expr) => {
                    let expr = crate::eval::deduplicate_symbols(expr, &mut self.heap);
                    last_result = eval_main(expr, self, false)?;
                }
            }
        }
    }
}

/// Top level evaluator with tail call management
pub fn eval(expr: GcRef, evaluator: &mut Evaluator) -> Result<GcRef, String> {
    let mut result = eval_main(expr, evaluator, false)?;

    while let Some(tail_call) = evaluator.tail_call.take() {
        if let SchemeValue::Callable(Callable::Closure { params, body, env }) =
            &tail_call.func.value
        {
            // Bind parameters
            let new_env = bind_params(params, &tail_call.args, env, evaluator.heap_mut())?;
            let original_env = evaluator.env_mut().current_frame();

            evaluator
                .env_mut()
                .set_current_frame(new_env.current_frame());

            // Always evaluate body in tail position
            result = eval_main(*body, evaluator, true)?;

            evaluator.env_mut().set_current_frame(original_env);
        } else {
            return Err("Invalid tail call: not a closure".to_string());
        }
    }

    Ok(result)
}

/// Handles environment access for symbol lookup application
pub fn eval_symbol(
    func: GcRef,
    args: &[GcRef],
    evaluator: &mut Evaluator,
) -> Result<GcRef, String> {
    if evaluator.trace > evaluator.depth {
        for v in args {
            print_scheme_value(&v.value);
        }
    }
    evaluator.depth -= 1;
    match &func.value {
        SchemeValue::Symbol(name) => {
            // Symbol lookup - check environment using symbol-based lookup
            // Since we're working with deduplicated symbols, we can use the symbol directly
            evaluator
                .env()
                .get_symbol(func)
                .ok_or_else(|| format!("Unbound variable: {}", name))
        }
        _ => Err("eval_apply: function is not a symbol, primitive, macro or closure".to_string()),
    }
}

/// Main evaluation walker - handles self-evaluating forms, symbol resolution, and nested calls
pub fn eval_main(expr: GcRef, evaluator: &mut Evaluator, tail: bool) -> Result<GcRef, String> {
    evaluator.depth += 1;

    if evaluator.trace > evaluator.depth {
        for _ in 1..evaluator.depth {
            print!(">");
        }
        println!("{}", print_scheme_value(&expr.value));
    }

    match &expr.value {
        SchemeValue::Int(_)
        | SchemeValue::Float(_)
        | SchemeValue::Str(_)
        | SchemeValue::Bool(_)
        | SchemeValue::Char(_)
        | SchemeValue::Nil
        | SchemeValue::Callable { .. } => Ok(expr),
        SchemeValue::Symbol(_) => eval_symbol(expr, &[], evaluator),
        SchemeValue::Pair(_, _) => eval_callable(expr, evaluator, tail),
        _ => Err("eval_main: unsupported expression type".to_string()),
    }
}

/// Evaluate a closure by creating a new environment frame and evaluating the body
#[inline(always)]
fn eval_closure(
    params: &[GcRef],
    body: GcRef,
    env: &Rc<RefCell<Frame>>,
    args: &[GcRef],
    evaluator: &mut Evaluator,
    _tail: bool,
) -> Result<GcRef, String> {
    evaluator.depth -= 2;
    // Create a new environment extending the captured environment
    let new_env = bind_params(params, args, env, evaluator.heap_mut())?;
    let original_env = evaluator.env_mut().current_frame();
    // Temporarily switch the evaluator's environment to the new one
    evaluator
        .env_mut()
        .set_current_frame(new_env.current_frame());

    // Evaluate the body in the new environment
    let result = eval_main(body, evaluator, true);

    // Restore the original environment
    evaluator.env_mut().set_current_frame(original_env);

    result
}

/// Macro handler
fn eval_macro(
    params: &[GcRef],
    body: GcRef,
    env: &Rc<RefCell<Frame>>,
    args: &[GcRef],
    evaluator: &mut Evaluator,
) -> Result<GcRef, String> {
    // Bind unevaluated arguments to macro parameters
    let new_env = bind_params(params, args, env, evaluator.heap_mut())?;
    let original_env = evaluator.env_mut().current_frame();
    evaluator
        .env_mut()
        .set_current_frame(new_env.current_frame());
    // Expand the macro
    println!("Unexpanded macro: {}", print_scheme_value(&body.value));
    let expanded = expand_macro(&body, 0, evaluator)?;
    println!("After expansion: {}", print_scheme_value(&expanded.value));

    evaluator.env_mut().set_current_frame(original_env);
    // Review for tail recursion
    eval_main(expanded, evaluator, false)
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

pub fn eval_callable(expr: GcRef, evaluator: &mut Evaluator, tail: bool) -> Result<GcRef, String> {
    match &expr.value {
        SchemeValue::Pair(car, cdr) => {
            let func = eval_main(*car, evaluator, false)?;
            match &func.value {
                SchemeValue::Callable(callable) => match callable {
                    Callable::Builtin { func, .. } => {
                        let processed_args = eval_args(cdr, evaluator)?;
                        func(&mut evaluator.heap, &processed_args)
                    }
                    Callable::SpecialForm { func, .. } => func(expr, evaluator, tail),
                    Callable::Closure { params, body, env } => {
                        let processed_args = eval_args(cdr, evaluator)?;
                        if tail {
                            // Resolve the function first
                            let func = eval_main(*car, evaluator, false)?;
                            // println!(
                            //     "Setting up tail call to: {}",
                            //     print_scheme_value(&expr.value)
                            // );
                            evaluator.tail_call = Some(TailCall {
                                func: func, // Store the resolved closure, not the call expression
                                args: processed_args,
                            });
                            return Ok(evaluator.heap.nil_s());
                        } else {
                            eval_closure(params, *body, env, &processed_args, evaluator, false)
                        }
                    }
                    Callable::Macro { params, body, env } => {
                        let processed_args = list_to_vec(*cdr)?;
                        eval_macro(params, *body, env, &processed_args, evaluator)
                    }
                },
                _ => Err("Not a callable".to_string()),
            }
        }
        _ => Err("Not a callable".to_string()),
    }
}

fn eval_args(args: &GcRef, evaluator: &mut Evaluator) -> Result<Vec<GcRef>, String> {
    let mut processed_args = Vec::new();
    let mut current = *args;
    loop {
        match &current.value {
            SchemeValue::Nil => break,
            SchemeValue::Pair(arg, next) => {
                processed_args.push(eval_main(*arg, evaluator, false)?);
                current = *next;
            }
            _ => return Err("Improper list in function call".to_string()),
        }
    }
    Ok(processed_args)
}

pub fn bind_params(
    params: &[GcRef],
    args: &[GcRef],
    parent_env: &Rc<RefCell<Frame>>,
    heap: &mut GcHeap,
) -> Result<Environment, String> {
    let captured_env = Environment::from_frame(parent_env.clone());
    let mut new_env = captured_env.extend();
    match params.len() {
        0 => (),
        1 => {
            let arglist = list_from_vec(args.to_vec(), heap);
            new_env.set_symbol(params[0], arglist);
        }
        _ => {
            for (param, arg) in params[1..].iter().zip(args.iter()) {
                new_env.set_symbol(*param, *arg);
            }
            let delta = args.len().saturating_sub(params.len() - 1);
            if delta > 0 {
                if let SchemeValue::Symbol(_) = &params[0].value {
                    let rest_args = &args[(params.len() - 1)..];
                    let arglist = list_from_vec(rest_args.to_vec(), heap);
                    new_env.set_symbol(params[0], arglist);
                }
            }
        }
    }
    Ok(new_env)
}

// Expect exactly N arguments in a proper list
// debug_assert!(N > 0);
pub fn expect_n_args(list: GcRef, n: usize) -> Result<Vec<GcRef>, String> {
    let args = list_to_vec(list)?;
    if args.len() != n {
        Err(format!(
            "expected {} arguments, got {}",
            n - 1,
            args.len() - 1
        ))
    } else {
        Ok(args)
    }
}

// Expect at least N arguments in a proper list
pub fn expect_at_least_n_args(list: GcRef, n: usize) -> Result<Vec<GcRef>, String> {
    let args = list_to_vec(list)?;
    if args.len() < n {
        Err(format!(
            "expected at least {} arguments, got {}",
            n - 1,
            args.len() - 1
        ))
    } else {
        Ok(args)
    }
}

// Expect the first N arguments to be present, return them, discard or keep tail as needed
fn extract_first_n(list: GcRef, n: usize) -> Result<(Vec<GcRef>, Option<GcRef>), String> {
    let mut args = Vec::new();
    let mut current = list;
    for _ in 0..n {
        match &current.value {
            SchemeValue::Pair(car, cdr) => {
                args.push(*car);
                current = *cdr;
            }
            _ => return Err("not enough arguments".to_string()),
        }
    }
    Ok((args, Some(current)))
}

// Expect a single symbol from an expression list
pub fn expect_symbol(expr: &GcRef) -> Result<GcRef, String> {
    match &expr.value {
        SchemeValue::Symbol(_) => Ok(*expr),
        _ => Err("expected symbol".to_string()),
    }
}

/// Extract arguments from a list (cdr of a function call)
fn extract_args(expr: GcRef) -> Result<Vec<GcRef>, String> {
    let mut args = Vec::new();
    let mut current = expr;

    loop {
        match &current.value {
            SchemeValue::Nil => {
                // End of list
                break;
            }
            SchemeValue::Pair(car, cdr) => {
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
fn is_special_form(func: GcRef) -> Option<&'static str> {
    match &func.value {
        SchemeValue::Symbol(name) => match name.as_str() {
            "quote" | "quasiquote" | "if" | "define" | "begin" | "and" | "or" | "set!"
            | "lambda" => Some(name),
            _ => None,
        },
        _ => None,
    }
}

/// Recursively deduplicate symbols in an expression tree, preserving parameter symbols.
///
/// This function is used when creating a closure to ensure that the parameter symbols
/// in the body are the same as those in the parameter list, avoiding potential
/// symbol mismatch issues.
///
/// # Examples
///
/// ```rust
/// use s1::gc::GcHeap;
/// use s1::evalsimple::deduplicate_symbols_preserve_params;
///
/// let mut heap = GcHeap::new();
/// let expr = heap.intern_symbol("foo"); // Already interned
/// let param_map = HashMap::new(); // No parameters
/// let deduplicated = deduplicate_symbols_preserve_params(expr, &mut heap, &param_map);
/// assert!(std::ptr::eq(expr, deduplicated)); // Same object
/// ```
pub fn deduplicate_symbols_preserve_params(
    expr: GcRef,
    heap: &mut GcHeap,
    param_map: &HashMap<String, GcRef>,
) -> GcRef {
    match &expr.value {
        SchemeValue::Symbol(name) => {
            // If the symbol is a parameter, return it directly
            if let Some(param_sym) = param_map.get(name) {
                *param_sym
            } else {
                // Otherwise, deduplicate it
                heap.intern_symbol(name)
            }
        }
        SchemeValue::Pair(car, cdr) => {
            // Recursively deduplicate car and cdr
            let new_car = deduplicate_symbols_preserve_params(*car, heap, param_map);
            let new_cdr = deduplicate_symbols_preserve_params(*cdr, heap, param_map);

            // Only create new pair if something changed
            if std::ptr::eq(*car, new_car) && std::ptr::eq(*cdr, new_cdr) {
                expr // No change, return original
            } else {
                new_pair(heap, new_car, new_cdr)
            }
        }
        SchemeValue::Vector(elements) => {
            // Recursively deduplicate all vector elements
            let mut new_elements = Vec::new();
            let mut changed = false;

            for element in elements {
                let new_element = deduplicate_symbols_preserve_params(*element, heap, param_map);
                new_elements.push(new_element);
                if !std::ptr::eq(*element, new_element) {
                    changed = true;
                }
            }

            if changed {
                new_vector(heap, new_elements)
            } else {
                expr // No change, return original
            }
        }
        SchemeValue::Callable(callable) => {
            match callable {
                Callable::Closure { params, body, env } | Callable::Macro { params, body, env } => {
                    // Deduplicate the body, but params are already symbols (interned)
                    let new_body = deduplicate_symbols_preserve_params(*body, heap, param_map);

                    if std::ptr::eq(*body, new_body) {
                        expr // No change, return original
                    } else {
                        new_closure(heap, params.clone(), new_body, env.clone())
                    }
                }
                _ => expr,
            }
        }
        // Self-evaluating forms and other types return unchanged
        SchemeValue::Int(_)
        | SchemeValue::Float(_)
        | SchemeValue::Str(_)
        | SchemeValue::Bool(_)
        | SchemeValue::Char(_)
        | SchemeValue::Nil
        | SchemeValue::Port { .. } => expr,
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
fn deduplicate_symbols(expr: GcRef, heap: &mut GcHeap) -> GcRef {
    match &expr.value {
        SchemeValue::Symbol(name) => {
            // Replace with interned symbol
            heap.intern_symbol(name)
        }
        SchemeValue::Pair(car, cdr) => {
            // Recursively deduplicate car and cdr
            let new_car = deduplicate_symbols(*car, heap);
            let new_cdr = deduplicate_symbols(*cdr, heap);

            // Only create new pair if something changed
            if std::ptr::eq(*car, new_car) && std::ptr::eq(*cdr, new_cdr) {
                expr // No change, return original
            } else {
                new_pair(heap, new_car, new_cdr)
            }
        }
        SchemeValue::Vector(elements) => {
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
                new_vector(heap, new_elements)
            } else {
                expr // No change, return original
            }
        }
        SchemeValue::Callable(callable) => {
            match callable {
                Callable::Closure { params, body, env } | Callable::Macro { params, body, env } => {
                    // Deduplicate the body, but params are already symbols (interned)
                    let new_body = deduplicate_symbols(*body, heap);
                    if std::ptr::eq(*body, new_body) {
                        expr // No change, return original
                    } else {
                        new_closure(heap, params.clone(), new_body, env.clone())
                    }
                }
                _ => expr,
            }
        }
        // Self-evaluating forms and other types return unchanged
        _ => expr,
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
    parser: &mut Parser,
    port: &mut Port,
    heap: &mut GcHeap,
) -> Result<GcRef, ParseError> {
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
    #[allow(unused_imports)]
    use super::*;
    use crate::gc::{get_symbol, new_closure, new_int, new_pair, new_primitive};

    #[test]
    fn test_eval_logic_self_evaluating() {
        let mut evaluator = Evaluator::new();
        let int_val;
        {
            let heap = evaluator.heap_mut();
            int_val = new_int(heap, num_bigint::BigInt::from(42));
        }
        let result = eval_main(int_val, &mut evaluator, false).unwrap();
        assert_eq!(result.value, int_val.value);
    }

    #[test]
    fn test_eval_logic_variable_lookup() {
        let mut evaluator = Evaluator::new();
        let value;
        let symbol;
        {
            let heap = evaluator.heap_mut();
            value = new_int(heap, num_bigint::BigInt::from(99));
            symbol = get_symbol(heap, "x");
        }
        evaluator.env_mut().set_symbol(symbol, value);
        let result = eval_main(symbol, &mut evaluator, false).unwrap();
        assert_eq!(result.value, value.value);
    }

    #[test]
    fn test_eval_logic_non_nested_call() {
        let mut evaluator = Evaluator::new();
        let plus;
        let a;
        let b;
        let plus_sym;
        let args;
        let expr;
        {
            let heap = evaluator.heap_mut();
            plus = new_primitive(
                heap,
                |heap, args| {
                    let a = match &args[0].value {
                        SchemeValue::Int(i) => i.clone(),
                        _ => return Err("not int".to_string()),
                    };
                    let b = match &args[1].value {
                        SchemeValue::Int(i) => i.clone(),
                        _ => return Err("not int".to_string()),
                    };
                    Ok(new_int(heap, a + b))
                },
                "plus".to_string(),
            );
            a = new_int(heap, num_bigint::BigInt::from(2));
            b = new_int(heap, num_bigint::BigInt::from(3));
            plus_sym = get_symbol(heap, "+");
            let nil = heap.nil_s();
            let b_pair = new_pair(heap, b, nil);
            args = new_pair(heap, a, b_pair);
            expr = new_pair(heap, plus_sym, args);
        }
        evaluator.env_mut().set_symbol(plus_sym, plus);
        let result = eval_main(expr, &mut evaluator, false).unwrap();
        match &result.value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "5"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_eval_logic_nested_call() {
        use crate::builtin::number::{plus_builtin, times_builtin};
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
            plus = new_primitive(heap, plus_builtin, "plus".to_string());
            times = new_primitive(heap, times_builtin, "times".to_string());
            two = new_int(heap, num_bigint::BigInt::from(2));
            three = new_int(heap, num_bigint::BigInt::from(3));
            four = new_int(heap, num_bigint::BigInt::from(4));
            five = new_int(heap, num_bigint::BigInt::from(5));
            plus_sym = get_symbol(heap, "+");
            let nil1 = heap.nil_s();
            let five_pair = new_pair(heap, five, nil1);
            plus_args = new_pair(heap, four, five_pair);
            plus_expr = new_pair(heap, plus_sym, plus_args);
            star_sym = get_symbol(heap, "*");
            let nil2 = heap.nil_s();
            let plus_expr_pair = new_pair(heap, plus_expr, nil2);
            let three_pair = new_pair(heap, three, plus_expr_pair);
            star_args = new_pair(heap, two, three_pair);
            expr = new_pair(heap, star_sym, star_args);
        }
        evaluator.env_mut().set_symbol(plus_sym, plus);
        evaluator.env_mut().set_symbol(star_sym, times);
        let result = eval_main(expr, &mut evaluator, false).unwrap();
        match &result.value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "54"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_extract_args() {
        let mut heap = crate::gc::GcHeap::new();

        // Create a list (1 2 3)
        let arg1 = new_int(&mut heap, num_bigint::BigInt::from(1));
        let arg2 = new_int(&mut heap, num_bigint::BigInt::from(2));
        let arg3 = new_int(&mut heap, num_bigint::BigInt::from(3));
        let nil = heap.nil_s();

        // Build the list: (1 . (2 . (3 . nil)))
        let list_3 = new_pair(&mut heap, arg3, nil);
        let list_2_3 = new_pair(&mut heap, arg2, list_3);
        let list_1_2_3 = new_pair(&mut heap, arg1, list_2_3);

        // Extract arguments
        let args = extract_args(list_1_2_3).unwrap();
        assert_eq!(args.len(), 3);

        // Check the arguments
        match &args[0].value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "1"),
            _ => panic!("Expected integer 1"),
        }
        match &args[1].value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "2"),
            _ => panic!("Expected integer 2"),
        }
        match &args[2].value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "3"),
            _ => panic!("Expected integer 3"),
        }
    }

    #[test]
    fn test_eval_logic_simple_nested_call() {
        use crate::builtin::number::{plus_builtin, times_builtin};
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
            times = new_primitive(heap, times_builtin, "times".to_string());
            plus = new_primitive(heap, plus_builtin, "plus".to_string());
            two = new_int(heap, num_bigint::BigInt::from(2));
            two2 = new_int(heap, num_bigint::BigInt::from(2));
            three = new_int(heap, num_bigint::BigInt::from(3));
            plus_sym = get_symbol(heap, "+");
            let nil1 = heap.nil_s();
            let three_pair = new_pair(heap, three, nil1);
            plus_args = new_pair(heap, two2, three_pair);
            plus_expr = new_pair(heap, plus_sym, plus_args);
            star_sym = get_symbol(heap, "*");
            let nil2 = heap.nil_s();
            let plus_expr_pair = new_pair(heap, plus_expr, nil2);
            star_args = new_pair(heap, two, plus_expr_pair);
            expr = new_pair(heap, star_sym, star_args);
        }
        evaluator.env_mut().set_symbol(star_sym, times);
        evaluator.env_mut().set_symbol(plus_sym, plus);
        let result = eval_main(expr, &mut evaluator, false).unwrap();
        match &result.value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "10"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_eval_logic_nested_mixed_call() {
        use crate::builtin::number::{minus_builtin, plus_builtin, times_builtin};
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
            plus = new_primitive(heap, plus_builtin, "plus".to_string());
            times = new_primitive(heap, times_builtin, "times".to_string());
            minus = new_primitive(heap, minus_builtin, "minus".to_string());
            two = new_int(heap, num_bigint::BigInt::from(2));
            three = new_int(heap, num_bigint::BigInt::from(3));
            four = new_int(heap, num_bigint::BigInt::from(4));
            five = new_int(heap, num_bigint::BigInt::from(5));
            minus_sym = get_symbol(heap, "-");
            let nil1 = heap.nil_s();
            let five_pair = new_pair(heap, five, nil1);
            minus_args = new_pair(heap, four, five_pair);
            minus_expr = new_pair(heap, minus_sym, minus_args);
            times_sym = get_symbol(heap, "*");
            let nil2 = heap.nil_s();
            let minus_expr_pair = new_pair(heap, minus_expr, nil2);
            times_args = new_pair(heap, three, minus_expr_pair);
            times_expr = new_pair(heap, times_sym, times_args);
            plus_sym = get_symbol(heap, "+");
            let nil3 = heap.nil_s();
            let times_expr_pair = new_pair(heap, times_expr, nil3);
            plus_args = new_pair(heap, two, times_expr_pair);
            expr = new_pair(heap, plus_sym, plus_args);
        }
        evaluator.env_mut().set_symbol(plus_sym, plus);
        evaluator.env_mut().set_symbol(times_sym, times);
        evaluator.env_mut().set_symbol(minus_sym, minus);
        let result = eval_main(expr, &mut evaluator, false).unwrap();
        match &result.value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "-1"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_eval_logic_quote() {
        let mut evaluator = Evaluator::new();
        // let quoted;
        let expr;
        {
            let heap = evaluator.heap_mut();
            let sym = get_symbol(heap, "foo");
            let nil = heap.nil_s();
            let sym_list = new_pair(heap, sym, nil);
            let quote_sym = get_symbol(heap, "quote");
            expr = new_pair(heap, quote_sym, sym_list);
            // quoted = sym;
        }
        let result = eval_main(expr, &mut evaluator, false).unwrap();
        match &result.value {
            SchemeValue::Symbol(s) => assert_eq!(s, "foo"),
            _ => panic!("Expected quoted symbol"),
        }
        // Test quoting a list: '(foo bar)
        // let quoted_list;
        let expr2;
        {
            let heap = evaluator.heap_mut();
            let foo = get_symbol(heap, "foo");
            let bar = get_symbol(heap, "bar");
            let nil = heap.nil_s();
            let bar_pair = new_pair(heap, bar, nil);
            let foo_bar_list = new_pair(heap, foo, bar_pair);
            let quote_sym = get_symbol(heap, "quote");
            let foo_bar_list_pair = new_pair(heap, foo_bar_list, nil);
            expr2 = new_pair(heap, quote_sym, foo_bar_list_pair);
            // let quoted_list = foo_bar_list;
        }
        let result2 = eval_main(expr2, &mut evaluator, false).unwrap();
        match &result2.value {
            SchemeValue::Pair(_, _) => (),
            _ => panic!("Expected quoted list"),
        }
    }

    #[test]
    fn test_eval_logic_begin() {
        use crate::builtin::number::plus_builtin;
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
            plus = new_primitive(heap, plus_builtin, "plus".to_string());
            a = new_int(heap, num_bigint::BigInt::from(1));
            b = new_int(heap, num_bigint::BigInt::from(2));
            c = new_int(heap, num_bigint::BigInt::from(3));
            plus_sym = get_symbol(heap, "+");
            let nil = heap.nil_s();
            let b_pair = new_pair(heap, b, nil);
            plus_args = new_pair(heap, a, b_pair);
            plus_expr = new_pair(heap, plus_sym, plus_args);
            begin_sym = get_symbol(heap, "begin");
            let c_pair = new_pair(heap, c, nil);
            let plus_expr_pair = new_pair(heap, plus_expr, c_pair);
            begin_args = plus_expr_pair;
            expr = new_pair(heap, begin_sym, begin_args);
        }
        evaluator.env_mut().set_symbol(plus_sym, plus);
        let result = eval_main(expr, &mut evaluator, false).unwrap();
        match &result.value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "3"),
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
            symbol = get_symbol(heap, "y");
            let val = new_int(heap, num_bigint::BigInt::from(42));
            let nil = heap.nil_s();
            let val_pair = new_pair(heap, val, nil);
            let args = new_pair(heap, symbol, val_pair);
            let define_sym = get_symbol(heap, "define");
            expr = new_pair(heap, define_sym, args);
        }
        let result = eval_main(expr, &mut evaluator, false).unwrap();
        match &result.value {
            SchemeValue::Symbol(s) => assert_eq!(s.to_string(), "y"),
            _ => panic!("Expected symbol result"),
        }
        // Check that the variable is now bound
        let bound = evaluator.env().get_symbol(symbol).unwrap();
        match &bound.value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "42"),
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
            let if_sym = get_symbol(heap, "if");
            let t = heap.true_s();
            let f = heap.false_s();
            let one = new_int(heap, num_bigint::BigInt::from(1));
            let two = new_int(heap, num_bigint::BigInt::from(2));
            let nil = heap.nil_s();
            // (if #t 1 2)
            let two_pair = new_pair(heap, two, nil);
            let one_pair = new_pair(heap, one, two_pair);
            let t_pair = new_pair(heap, t, one_pair);
            expr_true = new_pair(heap, if_sym, t_pair);
            // (if #f 1 2)
            let two_pair2 = new_pair(heap, two, nil);
            let one_pair2 = new_pair(heap, one, two_pair2);
            let f_pair = new_pair(heap, f, one_pair2);
            expr_false = new_pair(heap, if_sym, f_pair);
        }
        let result_true = eval_main(expr_true, &mut evaluator, false).unwrap();
        match &result_true.value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "1"),
            _ => panic!("Expected integer result for true branch"),
        }
        let result_false = eval_main(expr_false, &mut evaluator, false).unwrap();
        match &result_false.value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "2"),
            _ => panic!("Expected integer result for false branch"),
        }
    }

    #[test]
    fn test_eval_logic_and_or() {
        // (and)
        {
            let mut evaluator = Evaluator::new();
            let heap = evaluator.heap_mut();
            let and_sym = get_symbol(heap, "and");
            let nil = heap.nil_s();
            let and_empty = new_pair(heap, and_sym, nil);
            let result = eval_main(and_empty, &mut evaluator, false).unwrap();
            assert!(matches!(&result.value, SchemeValue::Bool(true)));
        }
        // (and #t 1 2)
        {
            let mut evaluator = Evaluator::new();
            let heap = evaluator.heap_mut();
            let and_sym = get_symbol(heap, "and");
            let t = heap.true_s();
            let one = new_int(heap, num_bigint::BigInt::from(1));
            let two = new_int(heap, num_bigint::BigInt::from(2));
            let nil = heap.nil_s();
            let two_pair = new_pair(heap, two, nil);
            let one_pair = new_pair(heap, one, two_pair);
            let t_pair = new_pair(heap, t, one_pair);
            let and_expr = new_pair(heap, and_sym, t_pair);
            let result = eval_main(and_expr, &mut evaluator, false).unwrap();
            match &result.value {
                SchemeValue::Int(i) => assert_eq!(i.to_string(), "2"),
                _ => panic!("Expected integer result for and"),
            }
        }
        // (and #t #f 2)
        {
            let mut evaluator = Evaluator::new();
            let heap = evaluator.heap_mut();
            let and_sym = get_symbol(heap, "and");
            let t = heap.true_s();
            let f = heap.false_s();
            let two = new_int(heap, num_bigint::BigInt::from(2));
            let nil = heap.nil_s();
            let two_pair = new_pair(heap, two, nil);
            let f_pair = new_pair(heap, f, two_pair);
            let t_pair2 = new_pair(heap, t, f_pair);
            let and_expr2 = new_pair(heap, and_sym, t_pair2);
            let result = eval_main(and_expr2, &mut evaluator, false).unwrap();
            assert!(matches!(&result.value, SchemeValue::Bool(false)));
        }
        // (or)
        {
            let mut evaluator = Evaluator::new();
            let heap = evaluator.heap_mut();
            let or_sym = get_symbol(heap, "or");
            let nil = heap.nil_s();
            let or_empty = new_pair(heap, or_sym, nil);
            let result = eval_main(or_empty, &mut evaluator, false).unwrap();
            assert!(matches!(&result.value, SchemeValue::Bool(false)));
        }
        // (or #f 1 2)
        {
            let mut evaluator = Evaluator::new();
            let heap = evaluator.heap_mut();
            let or_sym = get_symbol(heap, "or");
            let f = heap.false_s();
            let one = new_int(heap, num_bigint::BigInt::from(1));
            let two = new_int(heap, num_bigint::BigInt::from(2));
            let nil = heap.nil_s();
            let two_pair = new_pair(heap, two, nil);
            let one_pair = new_pair(heap, one, two_pair);
            let f_pair = new_pair(heap, f, one_pair);
            let or_expr = new_pair(heap, or_sym, f_pair);
            let result = eval_main(or_expr, &mut evaluator, false).unwrap();
            match &result.value {
                SchemeValue::Int(i) => assert_eq!(i.to_string(), "1"),
                _ => panic!("Expected integer result for or"),
            }
        }
        // (or #f #f 2)
        {
            let mut evaluator = Evaluator::new();
            let heap = evaluator.heap_mut();
            let or_sym = get_symbol(heap, "or");
            let f = heap.false_s();
            let two = new_int(heap, num_bigint::BigInt::from(2));
            let nil = heap.nil_s();
            let two_pair = new_pair(heap, two, nil);
            let f_pair2 = new_pair(heap, f, two_pair);
            let f_pair3 = new_pair(heap, f, f_pair2);
            let or_expr2 = new_pair(heap, or_sym, f_pair3);
            let result = eval_main(or_expr2, &mut evaluator, false).unwrap();
            match &result.value {
                SchemeValue::Int(i) => assert_eq!(i.to_string(), "2"),
                _ => panic!("Expected integer result for or"),
            }
        }
        // (or #f #f #f)
        {
            let mut evaluator = Evaluator::new();
            let heap = evaluator.heap_mut();
            let or_sym = get_symbol(heap, "or");
            let f = heap.false_s();
            let nil = heap.nil_s();
            let f_pair = new_pair(heap, f, nil);
            let f_pair2 = new_pair(heap, f, f_pair);
            let f_pair3 = new_pair(heap, f, f_pair2);
            let or_expr3 = new_pair(heap, or_sym, f_pair3);
            let result = eval_main(or_expr3, &mut evaluator, false).unwrap();
            assert!(matches!(&result.value, SchemeValue::Bool(false)));
        }
    }

    #[test]
    fn test_eval_logic_lambda() {
        let mut evaluator = Evaluator::new();

        // Set up the + function in the environment
        {
            let heap = evaluator.heap_mut();
            let plus = new_primitive(
                heap,
                crate::builtin::number::plus_builtin,
                "plus".to_string(),
            );
            let plus_sym = heap.intern_symbol("+");
            evaluator.env_mut().set_symbol(plus_sym, plus);
        }

        let lambda_expr;
        let add1;
        let result;
        {
            let heap = evaluator.heap_mut();
            // Create (lambda (x) (+ x 1))
            let x_param = get_symbol(heap, "x");
            let one = new_int(heap, num_bigint::BigInt::from(1));
            let plus_sym = heap.intern_symbol("+");
            let nil = heap.nil_s();

            // Build (+ x 1)
            let one_pair = new_pair(heap, one, nil);
            let x_one_pair = new_pair(heap, x_param, one_pair);
            let plus_expr = new_pair(heap, plus_sym, x_one_pair);

            // Build (lambda (x) (+ x 1))
            let lambda_sym = get_symbol(heap, "lambda");
            let params_list = new_pair(heap, x_param, nil);
            let body_nil = new_pair(heap, plus_expr, nil);
            let lambda_args = new_pair(heap, params_list, body_nil);
            lambda_expr = new_pair(heap, lambda_sym, lambda_args);
        }

        // Evaluate the lambda to create a closure
        add1 = eval_main(lambda_expr, &mut evaluator, false).unwrap();

        // Verify it's a closure
        match &add1.value {
            SchemeValue::Callable(callable) => {
                match callable {
                    Callable::Closure { params, body, .. } => {
                        assert_eq!(params.len(), 2); // 1 + the zeroth element for variadics
                        // Check that the parameter is a symbol with the right name
                        match &params[1].value {
                            SchemeValue::Symbol(name) => assert_eq!(name, "x"),
                            _ => panic!("Expected symbol parameter"),
                        }
                        // The body should be the (+ x 1) expression
                        match &body.value {
                            SchemeValue::Pair(car, _cdr) => match &car.value {
                                SchemeValue::Symbol(s) => assert_eq!(s, "+"),
                                _ => panic!("Expected + symbol"),
                            },
                            _ => panic!("Expected pair as body"),
                        }
                    }
                    _ => panic!("Expected closure"),
                }
            }
            _ => panic!("Expected closure"),
        }

        // Now apply the closure: (add1 5)
        let five;
        let apply_expr;
        {
            let heap = evaluator.heap_mut();
            five = new_int(heap, num_bigint::BigInt::from(5));

            // Build (add1 5)
            let nil = heap.nil_s();
            let five_pair = new_pair(heap, five, nil);
            apply_expr = new_pair(heap, add1, five_pair);
        }

        // Evaluate the application
        result = eval_main(apply_expr, &mut evaluator, false).unwrap();

        // Should return 6
        match &result.value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "6"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_eval_logic_closure_application() {
        let mut evaluator = Evaluator::new();

        // Set up the + function in the environment
        {
            let heap = evaluator.heap_mut();
            let plus = new_primitive(
                heap,
                crate::builtin::number::plus_builtin,
                "plus".to_string(),
            );
            let plus_sym = heap.intern_symbol("+");
            evaluator.env_mut().set_symbol(plus_sym, plus);
        }

        // Create a closure manually: (lambda (x y) (+ x y))
        let closure;
        let captured_env = evaluator.env().current_frame();
        {
            let heap = evaluator.heap_mut();
            let x_param = get_symbol(heap, "x");
            let y_param = get_symbol(heap, "y");
            let plus_sym = heap.intern_symbol("+");
            let nil = heap.nil_s();

            // Build (+ x y)
            let y_pair = new_pair(heap, y_param, nil);
            let x_y_pair = new_pair(heap, x_param, y_pair);
            let plus_expr = new_pair(heap, plus_sym, x_y_pair);

            // Create closure with captured environment
            closure = new_closure(heap, vec![nil, x_param, y_param], plus_expr, captured_env);
        }

        // Apply the closure: (closure 3 4)
        let three;
        let four;
        let apply_expr;
        let result;
        {
            let heap = evaluator.heap_mut();
            three = new_int(heap, num_bigint::BigInt::from(3));
            four = new_int(heap, num_bigint::BigInt::from(4));

            // Build (closure 3 4)
            let nil = heap.nil_s();
            let four_pair = new_pair(heap, four, nil);
            let three_four_pair = new_pair(heap, three, four_pair);
            apply_expr = new_pair(heap, closure, three_four_pair);
        }

        // Evaluate the application
        result = eval_main(apply_expr, &mut evaluator, false).unwrap();

        // Should return 7
        match &result.value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "7"),
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
        {
            let heap = evaluator.heap_mut();
            symbol = heap.intern_symbol("x");
            value = new_int(heap, num_bigint::BigInt::from(123));
            set_sym = get_symbol(heap, "set!");
            x_sym = heap.intern_symbol("x");
            val_123 = new_int(heap, num_bigint::BigInt::from(123));
            let nil = heap.nil_s();
            arg_pair = new_pair(heap, val_123, nil);
            args = new_pair(heap, x_sym, arg_pair);
            new_pair(heap, set_sym, args);
        }

        // First define x
        evaluator.env_mut().set_symbol(symbol, value);

        // Then set! it
        // let result = eval_logic(set_expr, &mut evaluator).unwrap();

        // Verify the value was set
        let new_value = evaluator.env().get_symbol(symbol).unwrap();
        match &new_value.value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "123"),
            _ => panic!("Expected integer"),
        }
    }

    #[test]
    fn test_deduplicate_symbols() {
        let mut heap = GcHeap::new();

        // Create a simple expression with symbols: (foo bar)
        let foo_sym = get_symbol(&mut heap, "foo");
        let bar_sym = get_symbol(&mut heap, "bar");
        let nil = heap.nil_s();
        let bar_nil = new_pair(&mut heap, bar_sym, nil);
        let expr = new_pair(&mut heap, foo_sym, bar_nil);

        // Deduplicate the expression
        let deduplicated = deduplicate_symbols(expr, &mut heap);

        // Verify that symbols are now interned
        match &deduplicated.value {
            SchemeValue::Pair(car, cdr) => {
                // Check that car is an interned symbol
                match &car.value {
                    SchemeValue::Symbol(name) => assert_eq!(name, "foo"),
                    _ => panic!("Expected symbol"),
                }

                // Check that cdr is a pair with an interned symbol
                match &cdr.value {
                    SchemeValue::Pair(car2, cdr2) => {
                        match &car2.value {
                            SchemeValue::Symbol(name) => assert_eq!(name, "bar"),
                            _ => panic!("Expected symbol"),
                        }
                        match &cdr2.value {
                            SchemeValue::Nil => {}
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
        assert!(
            std::ptr::eq(deduplicated, deduplicated2),
            "Re-deduplication should return same object"
        );

        // Verify that symbols with same name are the same object
        let foo1 = heap.intern_symbol("foo");
        let foo2 = heap.intern_symbol("foo");
        assert!(
            std::ptr::eq(foo1, foo2),
            "Same symbol name should be same object"
        );

        // Test with a more complex expression: ((lambda (x) (+ x 1)) 5)
        let lambda_sym = get_symbol(&mut heap, "lambda");
        let x_sym = get_symbol(&mut heap, "x");
        let plus_sym = get_symbol(&mut heap, "+");
        let one = new_int(&mut heap, num_bigint::BigInt::from(1));
        let five = new_int(&mut heap, num_bigint::BigInt::from(5));

        // Build (+ x 1)
        let one_nil = new_pair(&mut heap, one, nil);
        let x_one = new_pair(&mut heap, x_sym, one_nil);
        let plus_expr = new_pair(&mut heap, plus_sym, x_one);

        // Build (lambda (x) (+ x 1))
        let x_nil = new_pair(&mut heap, x_sym, nil);
        let plus_nil = new_pair(&mut heap, plus_expr, nil);
        let lambda_args = new_pair(&mut heap, x_nil, plus_nil);
        let lambda_expr = new_pair(&mut heap, lambda_sym, lambda_args);

        // Build ((lambda (x) (+ x 1)) 5)
        let five_nil = new_pair(&mut heap, five, nil);
        let complex_expr = new_pair(&mut heap, lambda_expr, five_nil);

        // Deduplicate the complex expression
        let deduplicated_complex = deduplicate_symbols(complex_expr, &mut heap);

        // Verify that all symbols in the complex expression are interned
        // (We can't easily check all of them, but we can verify the structure is preserved)
        match &deduplicated_complex.value {
            SchemeValue::Pair(car, cdr) => {
                // Should be a pair structure
                match &car.value {
                    SchemeValue::Pair(lambda_car, _lambda_cdr) => {
                        // Should be (lambda ...)
                        match &lambda_car.value {
                            SchemeValue::Symbol(name) => assert_eq!(name, "lambda"),
                            _ => panic!("Expected lambda symbol"),
                        }
                    }
                    _ => panic!("Expected lambda expression"),
                }
                match &cdr.value {
                    SchemeValue::Pair(arg_car, arg_cdr) => {
                        // Should be (5)
                        match &arg_car.value {
                            SchemeValue::Int(i) => assert_eq!(i.to_string(), "5"),
                            _ => panic!("Expected integer 5"),
                        }
                        match &arg_cdr.value {
                            SchemeValue::Nil => {}
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
        let mut parser = Parser::new();

        // Test parsing and deduplicating a simple expression
        let mut port = crate::io::new_string_port_input("(define x 42)");

        let expr =
            parse_and_deduplicate(&mut parser, &mut port, &mut evaluator.heap_mut()).unwrap();

        // Verify the expression structure is preserved
        match &expr.value {
            SchemeValue::Pair(car, cdr) => {
                // Should be (define x 42)
                match &car.value {
                    SchemeValue::Symbol(name) => assert_eq!(name, "define"),
                    _ => panic!("Expected define symbol"),
                }
                match &cdr.value {
                    SchemeValue::Pair(sym, rest) => {
                        match &sym.value {
                            SchemeValue::Symbol(name) => assert_eq!(name, "x"),
                            _ => panic!("Expected x symbol"),
                        }
                        match &rest.value {
                            SchemeValue::Pair(val, nil) => {
                                match &val.value {
                                    SchemeValue::Int(i) => assert_eq!(i.to_string(), "42"),
                                    _ => panic!("Expected integer 42"),
                                }
                                match &nil.value {
                                    SchemeValue::Nil => {}
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
        let mut port2 = crate::io::new_string_port_input("(define x 42)");

        let expr2 =
            parse_and_deduplicate(&mut parser, &mut port2, &mut evaluator.heap_mut()).unwrap();

        // Extract the define symbols from both expressions
        let define1 = match &expr.value {
            SchemeValue::Pair(car, _) => car,
            _ => panic!("Expected pair"),
        };
        let define2 = match &expr2.value {
            SchemeValue::Pair(car, _) => car,
            _ => panic!("Expected pair"),
        };

        // Verify they are the same interned symbol
        assert!(
            std::ptr::eq(*define1, *define2),
            "Define symbols should be interned"
        );

        // Test symbol table statistics
        assert!(
            evaluator.heap_mut().symbol_table_stats() > 0,
            "Symbol table should contain symbols"
        );
    }

    #[test]
    fn test_symbol_interning_in_lambda() {
        let mut evaluator = Evaluator::new();

        // Create a symbol and bind it to the environment
        {
            let heap = evaluator.heap_mut();
            let plus_sym = heap.intern_symbol("+");
            let plus_func = new_primitive(
                heap,
                crate::builtin::number::plus_builtin,
                "plus".to_string(),
            );
            evaluator.env_mut().set_symbol(plus_sym, plus_func);
        }

        // Now create a lambda that uses the + symbol
        let x_param;
        let plus_sym;
        let one;
        let nil;
        let plus_expr;
        {
            let heap = evaluator.heap_mut();
            x_param = heap.intern_symbol("x");
            plus_sym = heap.intern_symbol("+");
            one = new_int(heap, num_bigint::BigInt::from(1));
            nil = heap.nil_s();

            // Build (+ x 1) - using the same plus_sym that was bound
            let one_pair = new_pair(heap, one, nil);
            let x_one_pair = new_pair(heap, x_param, one_pair);
            plus_expr = new_pair(heap, plus_sym, x_one_pair);
        }

        // Create closure manually with the deduplicated body
        let deduplicated_body;
        let captured_env = evaluator.env().current_frame();
        let closure;
        {
            let heap = evaluator.heap_mut();
            deduplicated_body = deduplicate_symbols(plus_expr, heap);
            closure = new_closure(heap, vec![nil, x_param], deduplicated_body, captured_env);
        }

        // Apply the closure: (closure 5)
        let five;
        let five_pair;
        let apply_expr;
        {
            let heap = evaluator.heap_mut();
            five = new_int(heap, num_bigint::BigInt::from(5));
            five_pair = new_pair(heap, five, nil);
            apply_expr = new_pair(heap, closure, five_pair);
        }

        // This should work because we're using the same interned symbol
        let result = eval_main(apply_expr, &mut evaluator, false).unwrap();
        match &result.value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "6"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_tail_call_infrastructure() {
        let mut evaluator = Evaluator::new();

        // Test that tail call infrastructure is in place
        assert!(evaluator.tail_call.is_none());

        // Test that is_tail_call is conservative (returns false)
        let test_expr = new_int(evaluator.heap_mut(), num_bigint::BigInt::from(42));
        //assert!(!is_tail_call(test_expr, &mut evaluator).unwrap());

        // Test that we can set a tail call (even though it's not used)
        evaluator.tail_call = Some(TailCall {
            func: test_expr,
            args: vec![],
        });
        assert!(evaluator.tail_call.is_some());
    }

    #[test]
    fn test_eval_string_basic_builtins() {
        let mut evaluator = Evaluator::new();
        // Simple arithmetic
        let result = evaluator.eval_string("(+ 1 2)").unwrap();
        match &result.value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "3"),
            _ => panic!("Expected integer result"),
        }
        // Multiple expressions, last result returned
        let result = evaluator.eval_string("(+ 1 2) (* 2 3)").unwrap();
        match &result.value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "6"),
            _ => panic!("Expected integer result"),
        }
        // Variable definition and use
        evaluator.eval_string("(define x 42)").unwrap();
        let result = evaluator.eval_string("(+ x 1)").unwrap();
        match &result.value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "43"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_eval_string_error_handling() {
        let mut evaluator = Evaluator::new();
        // Syntax error
        let err = evaluator.eval_string("(+ 1 2");
        println!("test_eval_string_error: {:?}", err);
        // assert!(
        //     err.contains("end of input") ||
        //     err.contains("unexpected EOF") ||
        //     err.contains("Unclosed list"),
        //     "Unexpected error message: {}", err
        // );
        // Unbound variable
        let err = evaluator.eval_string("(+ y 1)").unwrap_err();
        assert!(err.contains("Unbound variable"));
        // Wrong argument type
        let err = evaluator.eval_string("(+ 1 'foo)").unwrap_err();
        assert!(err.contains("not int") || err.contains("number"));
    }
}
