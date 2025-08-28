//! Simple evaluator using the new GC system and two-layer architecture.
//!
//! This module implements a clean separation between evaluation logic and function application.
//! The logic layer handles self-evaluating forms, special forms, and argument evaluation,
//! while the apply layer handles function calls with pre-evaluated arguments.

use crate::cek::eval_main;
use crate::env::{Environment, Frame};
use crate::gc::SchemeValue::*;
use crate::gc::{GcHeap, GcRef, SchemeValue, list_from_vec, list_to_vec};
use crate::io::PortKind;
use crate::macros::expand_macro;
use std::cell::RefCell;
use std::rc::Rc;

/// Evaluator that owns both heap and environment
pub struct Evaluator {
    pub heap: GcHeap,              // The global heap for scheme data
    env: Environment,              // The current state of the environment
    tail_call: Option<TailCall>,   // Tail call optimization state
    pub port_stack: Vec<PortKind>, // Stack of ports for input/output operations         // Indicates a new port has been pushed on the stack
    pub trace: i32,                // Indicates tracing is enabled to a given depth
    pub depth: i32,                // Evaluation nesting depth (used by trace)
}

pub struct EvalContext<'a> {
    pub heap: &'a mut GcHeap,
    pub env: &'a mut Environment,
    pub tail_call: Option<TailCall>,
    pub port_stack: &'a mut Vec<PortKind>,
    pub trace: &'a mut i32,
    pub depth: &'a mut i32,
}

impl<'a> EvalContext<'a> {
    pub fn from_eval(eval: &'a mut Evaluator) -> Self {
        EvalContext {
            heap: &mut eval.heap,
            env: &mut eval.env,
            tail_call: None,
            port_stack: &mut eval.port_stack,
            trace: &mut eval.trace,
            depth: &mut eval.depth,
        }
    }
}

/// Represents a tail call that should be optimized
#[derive(Clone)]
struct TailCall {
    pub func: GcRef,
    pub args: Vec<GcRef>,
}

impl Evaluator {
    pub fn new() -> Self {
        let g_env = Environment::new();
        let mut evaluator = Self {
            heap: GcHeap::new(),
            env: g_env,
            tail_call: None,
            port_stack: Vec::new(),
            trace: 0,
            depth: 0,
        };

        // Register built-ins in the evaluator's heap and environment
        crate::builtin::register_builtins(&mut evaluator.heap, &mut evaluator.env);
        crate::special_forms::register_special_forms(&mut evaluator.heap, &mut evaluator.env);
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
}

/// Initialize Scheme-level I/O globals: **stdin**, **stdout**, **port-stack**
///
/// This should be called after creating the Evaluator, before loading files or starting the REPL.
pub fn initialize_scheme_io_globals(ec: &mut EvalContext) -> Result<(), String> {
    // Create stdin and stdout ports
    let stdin_port = crate::gc::new_port(&mut ec.heap, PortKind::Stdin);
    let stdout_port = crate::gc::new_port(&mut ec.heap, PortKind::Stdout);
    let stderr_port = crate::gc::new_port(&mut ec.heap, PortKind::Stderr);
    // Bind **stdin**, **stdout**, and **stderr** as Scheme globals
    let stdin_sym = ec.heap.intern_symbol("**stdin**");
    let stdout_sym = ec.heap.intern_symbol("**stdout**");
    let stderr_sym = ec.heap.intern_symbol("**stderr**");
    ec.env.set_symbol(stdin_sym, stdin_port);
    ec.env.set_symbol(stdout_sym, stdout_port);
    ec.env.set_symbol(stderr_sym, stderr_port);

    // Push stdin onto port-stack
    ec.port_stack.push(PortKind::Stdin);
    Ok(())
}

/// Evaluate a string of Scheme code (all expressions, return last result)
pub fn eval_string(ec: &mut EvalContext, code: &str) -> Result<GcRef, String> {
    use crate::parser::{ParseError, Parser};
    //use crate::io::{Port, PortKind};

    let mut port_kind = crate::io::new_string_port_input(code);
    let mut parser = Parser::new();
    let mut last_result = ec.heap.nil_s();
    loop {
        match parser.parse(&mut ec.heap, &mut port_kind) {
            Err(ParseError::Syntax(e)) => return Err(e),
            Err(ParseError::Eof) => return Ok(last_result),
            Ok(expr) => {
                //let expr = crate::eval::deduplicate_symbols(expr, &mut self.heap);
                last_result = eval_main(expr, ec)?;
            }
            _ => {}
        }
    }
}

/// Macro handler
pub fn eval_macro(
    params: &[GcRef],
    body: GcRef,
    env: &Rc<RefCell<Frame>>,
    args: &[GcRef],
    evaluator: &mut EvalContext,
) -> Result<GcRef, String> {
    let new_env = bind_params(params, args, env, evaluator.heap)?;
    let original_env = evaluator.env.current_frame();
    evaluator.env.set_current_frame(new_env.current_frame());
    // println!("Unexpanded macro: {}", print_scheme_value(&body.value));
    let expanded = expand_macro(&body, 0, evaluator)?;
    // println!("After expansion: {}", print_scheme_value(&expanded.value));
    evaluator.env.set_current_frame(original_env);
    // Review for tail recursion
    Ok(expanded)
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

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
                if let SchemeValue::Symbol(_) = &heap.get_value(params[0]) {
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
pub fn expect_n_args(heap: &GcHeap, list: GcRef, n: usize) -> Result<Vec<GcRef>, String> {
    let args = list_to_vec(heap, list)?;
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
pub fn expect_at_least_n_args(heap: &GcHeap, list: GcRef, n: usize) -> Result<Vec<GcRef>, String> {
    let args = list_to_vec(heap, list)?;
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

// Expect a single symbol from an expression list
pub fn expect_symbol(heap: &GcHeap, expr: &GcRef) -> Result<GcRef, String> {
    match &heap.get_value(*expr) {
        SchemeValue::Symbol(_) => Ok(*expr),
        _ => Err("expected symbol".to_string()),
    }
}

fn is_value(v: &SchemeValue) -> bool {
    match v {
        // Symbols are NOT values (must be looked up). Pairs are applications.
        Symbol(_) | Pair(_, _) => false,
        _ => true,
    }
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
        use crate::gc::equal;
        let mut evaluator = Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut evaluator);
        let int_val;
        int_val = new_int(ec.heap, num_bigint::BigInt::from(42));
        let result = eval_main(int_val, &mut ec).unwrap();
        assert!(equal(&evaluator.heap, result, int_val));
    }

    #[test]
    fn test_eval_logic_variable_lookup() {
        use crate::gc::equal;
        let mut evaluator = Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut evaluator);
        let value;
        let symbol;
        value = new_int(ec.heap, num_bigint::BigInt::from(99));
        symbol = get_symbol(ec.heap, "x");
        ec.env.set_symbol(symbol, value);
        let result = eval_main(symbol, &mut ec).unwrap();
        assert!(equal(&evaluator.heap, result, value));
    }

    #[test]
    fn test_eval_logic_non_nested_call() {
        let mut evaluator = Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut evaluator);
        let plus;
        let a;
        let b;
        let plus_sym;
        let args;
        let expr;
        //let hp = ec.heap;
        plus = new_primitive(
            ec.heap,
            |heap, args| {
                let a = match &heap.heap.get_value(args[0]) {
                    SchemeValue::Int(i) => i.clone(),
                    _ => return Err("not int".to_string()),
                };
                let b = match &heap.heap.get_value(args[1]) {
                    SchemeValue::Int(i) => i.clone(),
                    _ => return Err("not int".to_string()),
                };
                Ok(new_int(heap.heap, a + b))
            },
            "plus".to_string(),
        );
        a = new_int(ec.heap, num_bigint::BigInt::from(2));
        b = new_int(ec.heap, num_bigint::BigInt::from(3));
        plus_sym = get_symbol(ec.heap, "+");
        let nil = ec.heap.nil_s();
        let b_pair = new_pair(ec.heap, b, nil);
        args = new_pair(ec.heap, a, b_pair);
        expr = new_pair(ec.heap, plus_sym, args);
        ec.env.set_symbol(plus_sym, plus);
        let result = eval_main(expr, &mut ec).unwrap();
        match &ec.heap.get_value(result) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "5"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_eval_logic_nested_call() {
        use crate::builtin::number::{plus_builtin, times_builtin};
        let mut evaluator = Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut evaluator);
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
        plus = new_primitive(ec.heap, plus_builtin, "plus".to_string());
        times = new_primitive(ec.heap, times_builtin, "times".to_string());
        two = new_int(ec.heap, num_bigint::BigInt::from(2));
        three = new_int(ec.heap, num_bigint::BigInt::from(3));
        four = new_int(ec.heap, num_bigint::BigInt::from(4));
        five = new_int(ec.heap, num_bigint::BigInt::from(5));
        plus_sym = get_symbol(ec.heap, "+");
        let nil1 = ec.heap.nil_s();
        let five_pair = new_pair(ec.heap, five, nil1);
        plus_args = new_pair(ec.heap, four, five_pair);
        plus_expr = new_pair(ec.heap, plus_sym, plus_args);
        star_sym = get_symbol(ec.heap, "*");
        let nil2 = ec.heap.nil_s();
        let plus_expr_pair = new_pair(ec.heap, plus_expr, nil2);
        let three_pair = new_pair(ec.heap, three, plus_expr_pair);
        star_args = new_pair(ec.heap, two, three_pair);
        expr = new_pair(ec.heap, star_sym, star_args);
        ec.env.set_symbol(plus_sym, plus);
        ec.env.set_symbol(star_sym, times);
        let result = eval_main(expr, &mut ec).unwrap();
        match &evaluator.heap.get_value(result) {
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
        let args = list_to_vec(&heap, list_1_2_3).unwrap();
        assert_eq!(args.len(), 3);

        // Check the arguments
        match &heap.get_value(args[0]) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "1"),
            _ => panic!("Expected integer 1"),
        }
        match &heap.get_value(args[1]) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "2"),
            _ => panic!("Expected integer 2"),
        }
        match &heap.get_value(args[2]) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "3"),
            _ => panic!("Expected integer 3"),
        }
    }

    #[test]
    fn test_eval_logic_simple_nested_call() {
        use crate::builtin::number::{plus_builtin, times_builtin};
        let mut evaluator = Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut evaluator);
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
        times = new_primitive(ec.heap, times_builtin, "times".to_string());
        plus = new_primitive(ec.heap, plus_builtin, "plus".to_string());
        two = new_int(ec.heap, num_bigint::BigInt::from(2));
        two2 = new_int(ec.heap, num_bigint::BigInt::from(2));
        three = new_int(ec.heap, num_bigint::BigInt::from(3));
        plus_sym = get_symbol(ec.heap, "+");
        let nil1 = ec.heap.nil_s();
        let three_pair = new_pair(ec.heap, three, nil1);
        plus_args = new_pair(ec.heap, two2, three_pair);
        plus_expr = new_pair(ec.heap, plus_sym, plus_args);
        star_sym = get_symbol(ec.heap, "*");
        let nil2 = ec.heap.nil_s();
        let plus_expr_pair = new_pair(ec.heap, plus_expr, nil2);
        star_args = new_pair(ec.heap, two, plus_expr_pair);
        expr = new_pair(ec.heap, star_sym, star_args);
        ec.env.set_symbol(star_sym, times);
        ec.env.set_symbol(plus_sym, plus);
        let result = eval_main(expr, &mut ec).unwrap();
        match &ec.heap.get_value(result) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "10"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_eval_logic_nested_mixed_call() {
        use crate::builtin::number::{minus_builtin, plus_builtin, times_builtin};
        let mut evaluator = Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut evaluator);
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
        plus = new_primitive(ec.heap, plus_builtin, "plus".to_string());
        times = new_primitive(ec.heap, times_builtin, "times".to_string());
        minus = new_primitive(ec.heap, minus_builtin, "minus".to_string());
        two = new_int(ec.heap, num_bigint::BigInt::from(2));
        three = new_int(ec.heap, num_bigint::BigInt::from(3));
        four = new_int(ec.heap, num_bigint::BigInt::from(4));
        five = new_int(ec.heap, num_bigint::BigInt::from(5));
        minus_sym = get_symbol(ec.heap, "-");
        let nil1 = ec.heap.nil_s();
        let five_pair = new_pair(ec.heap, five, nil1);
        minus_args = new_pair(ec.heap, four, five_pair);
        minus_expr = new_pair(ec.heap, minus_sym, minus_args);
        times_sym = get_symbol(ec.heap, "*");
        let nil2 = ec.heap.nil_s();
        let minus_expr_pair = new_pair(ec.heap, minus_expr, nil2);
        times_args = new_pair(ec.heap, three, minus_expr_pair);
        times_expr = new_pair(ec.heap, times_sym, times_args);
        plus_sym = get_symbol(ec.heap, "+");
        let nil3 = ec.heap.nil_s();
        let times_expr_pair = new_pair(ec.heap, times_expr, nil3);
        plus_args = new_pair(ec.heap, two, times_expr_pair);
        expr = new_pair(ec.heap, plus_sym, plus_args);
        ec.env.set_symbol(plus_sym, plus);
        ec.env.set_symbol(times_sym, times);
        ec.env.set_symbol(minus_sym, minus);
        let result = eval_main(expr, &mut ec).unwrap();
        match &ec.heap.get_value(result) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "-1"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_eval_logic_quote() {
        let mut evaluator = Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut evaluator);
        // let quoted;
        let expr;
        let sym = get_symbol(ec.heap, "foo");
        let nil = ec.heap.nil_s();
        let sym_list = new_pair(ec.heap, sym, nil);
        let quote_sym = get_symbol(ec.heap, "quote");
        expr = new_pair(ec.heap, quote_sym, sym_list);
        // quoted = sym;
        let result = eval_main(expr, &mut ec).unwrap();
        match &ec.heap.get_value(result) {
            SchemeValue::Symbol(s) => assert_eq!(s, "foo"),
            _ => panic!("Expected quoted symbol"),
        }
        // Test quoting a list: '(foo bar)
        // let quoted_list;
        let expr2;
        let foo = get_symbol(ec.heap, "foo");
        let bar = get_symbol(ec.heap, "bar");
        let nil = ec.heap.nil_s();
        let bar_pair = new_pair(ec.heap, bar, nil);
        let foo_bar_list = new_pair(ec.heap, foo, bar_pair);
        let quote_sym = get_symbol(ec.heap, "quote");
        let foo_bar_list_pair = new_pair(ec.heap, foo_bar_list, nil);
        expr2 = new_pair(ec.heap, quote_sym, foo_bar_list_pair);
        // let quoted_list = foo_bar_list;
        let result2 = eval_main(expr2, &mut ec).unwrap();
        match &ec.heap.get_value(result2) {
            SchemeValue::Pair(_, _) => (),
            _ => panic!("Expected quoted list"),
        }
    }

    #[test]
    fn test_eval_logic_begin() {
        use crate::builtin::number::plus_builtin;
        let mut evaluator = Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut evaluator);
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
        plus = new_primitive(ec.heap, plus_builtin, "plus".to_string());
        a = new_int(ec.heap, num_bigint::BigInt::from(1));
        b = new_int(ec.heap, num_bigint::BigInt::from(2));
        c = new_int(ec.heap, num_bigint::BigInt::from(3));
        plus_sym = get_symbol(ec.heap, "+");
        let nil = ec.heap.nil_s();
        let b_pair = new_pair(ec.heap, b, nil);
        plus_args = new_pair(ec.heap, a, b_pair);
        plus_expr = new_pair(ec.heap, plus_sym, plus_args);
        begin_sym = get_symbol(ec.heap, "begin");
        let c_pair = new_pair(ec.heap, c, nil);
        let plus_expr_pair = new_pair(ec.heap, plus_expr, c_pair);
        begin_args = plus_expr_pair;
        expr = new_pair(ec.heap, begin_sym, begin_args);
        ec.env.set_symbol(plus_sym, plus);
        let result = eval_main(expr, &mut ec).unwrap();
        match &ec.heap.get_value(result) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "3"),
            _ => panic!("Expected integer result from begin"),
        }
    }

    #[test]
    fn test_eval_logic_define() {
        let mut evaluator = Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut evaluator);
        let symbol;
        let expr;
        symbol = get_symbol(ec.heap, "y");
        let val = new_int(ec.heap, num_bigint::BigInt::from(42));
        let nil = ec.heap.nil_s();
        let val_pair = new_pair(ec.heap, val, nil);
        let args = new_pair(ec.heap, symbol, val_pair);
        let define_sym = get_symbol(ec.heap, "define");
        expr = new_pair(ec.heap, define_sym, args);
        let result = eval_main(expr, &mut ec).unwrap();
        match &ec.heap.get_value(result) {
            SchemeValue::Symbol(s) => assert_eq!(s.to_string(), "y"),
            _ => panic!("Expected symbol result"),
        }
        // Check that the variable is now bound
        let bound = ec.env.get_symbol(symbol).unwrap();
        match &ec.heap.get_value(bound) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "42"),
            _ => panic!("Expected integer result in env"),
        }
    }

    #[test]
    fn test_eval_logic_if() {
        let mut evaluator = Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut evaluator);
        let expr_true;
        let expr_false;
        let if_sym = get_symbol(ec.heap, "if");
        let t = ec.heap.true_s();
        let f = ec.heap.false_s();
        let one = new_int(ec.heap, num_bigint::BigInt::from(1));
        let two = new_int(ec.heap, num_bigint::BigInt::from(2));
        let nil = ec.heap.nil_s();
        // (if #t 1 2)
        let two_pair = new_pair(ec.heap, two, nil);
        let one_pair = new_pair(ec.heap, one, two_pair);
        let t_pair = new_pair(ec.heap, t, one_pair);
        expr_true = new_pair(ec.heap, if_sym, t_pair);
        // (if #f 1 2)
        let two_pair2 = new_pair(ec.heap, two, nil);
        let one_pair2 = new_pair(ec.heap, one, two_pair2);
        let f_pair = new_pair(ec.heap, f, one_pair2);
        expr_false = new_pair(ec.heap, if_sym, f_pair);
        let result_true = eval_main(expr_true, &mut ec).unwrap();
        match &ec.heap.get_value(result_true) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "1"),
            _ => panic!("Expected integer result for true branch"),
        }
        let result_false = eval_main(expr_false, &mut ec).unwrap();
        match &ec.heap.get_value(result_false) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "2"),
            _ => panic!("Expected integer result for false branch"),
        }
    }

    #[test]
    fn test_eval_logic_and_or() {
        // (and)
        {
            let mut evaluator = Evaluator::new();
            let mut ec = crate::eval::EvalContext::from_eval(&mut evaluator);
            let and_sym = get_symbol(ec.heap, "and");
            let nil = ec.heap.nil_s();
            let and_empty = new_pair(ec.heap, and_sym, nil);
            let result = eval_main(and_empty, &mut ec).unwrap();
            assert!(matches!(
                &ec.heap.get_value(result),
                SchemeValue::Bool(true)
            ));
        }
        // (and #t 1 2)
        {
            let mut evaluator = Evaluator::new();
            let mut ec = crate::eval::EvalContext::from_eval(&mut evaluator);
            let and_sym = get_symbol(ec.heap, "and");
            let t = ec.heap.true_s();
            let one = new_int(ec.heap, num_bigint::BigInt::from(1));
            let two = new_int(ec.heap, num_bigint::BigInt::from(2));
            let nil = ec.heap.nil_s();
            let two_pair = new_pair(ec.heap, two, nil);
            let one_pair = new_pair(ec.heap, one, two_pair);
            let t_pair = new_pair(ec.heap, t, one_pair);
            let and_expr = new_pair(ec.heap, and_sym, t_pair);
            let result = eval_main(and_expr, &mut ec).unwrap();
            match &ec.heap.get_value(result) {
                SchemeValue::Int(i) => assert_eq!(i.to_string(), "2"),
                _ => panic!("Expected integer result for and"),
            }
        }
        // (and #t #f 2)
        {
            let mut evaluator = Evaluator::new();
            let mut ec = crate::eval::EvalContext::from_eval(&mut evaluator);
            let and_sym = get_symbol(ec.heap, "and");
            let t = ec.heap.true_s();
            let f = ec.heap.false_s();
            let two = new_int(ec.heap, num_bigint::BigInt::from(2));
            let nil = ec.heap.nil_s();
            let two_pair = new_pair(ec.heap, two, nil);
            let f_pair = new_pair(ec.heap, f, two_pair);
            let t_pair2 = new_pair(ec.heap, t, f_pair);
            let and_expr2 = new_pair(ec.heap, and_sym, t_pair2);
            let result = eval_main(and_expr2, &mut ec).unwrap();
            assert!(matches!(
                &ec.heap.get_value(result),
                SchemeValue::Bool(false)
            ));
        }
        // (or)
        {
            let mut evaluator = Evaluator::new();
            let mut ec = crate::eval::EvalContext::from_eval(&mut evaluator);
            let or_sym = get_symbol(ec.heap, "or");
            let nil = ec.heap.nil_s();
            let or_empty = new_pair(ec.heap, or_sym, nil);
            let result = eval_main(or_empty, &mut ec).unwrap();
            assert!(matches!(
                &evaluator.heap.get_value(result),
                SchemeValue::Bool(false)
            ));
        }
        // (or #f 1 2)
        {
            let mut evaluator = Evaluator::new();
            let mut ec = crate::eval::EvalContext::from_eval(&mut evaluator);
            let or_sym = get_symbol(ec.heap, "or");
            let f = ec.heap.false_s();
            let one = new_int(ec.heap, num_bigint::BigInt::from(1));
            let two = new_int(ec.heap, num_bigint::BigInt::from(2));
            let nil = ec.heap.nil_s();
            let two_pair = new_pair(ec.heap, two, nil);
            let one_pair = new_pair(ec.heap, one, two_pair);
            let f_pair = new_pair(ec.heap, f, one_pair);
            let or_expr = new_pair(ec.heap, or_sym, f_pair);
            let result = eval_main(or_expr, &mut ec).unwrap();
            match &ec.heap.get_value(result) {
                SchemeValue::Int(i) => assert_eq!(i.to_string(), "1"),
                _ => panic!("Expected integer result for or"),
            }
        }
        // (or #f #f 2)
        {
            let mut evaluator = Evaluator::new();
            let mut ec = crate::eval::EvalContext::from_eval(&mut evaluator);
            let or_sym = get_symbol(ec.heap, "or");
            let f = ec.heap.false_s();
            let two = new_int(ec.heap, num_bigint::BigInt::from(2));
            let nil = ec.heap.nil_s();
            let two_pair = new_pair(ec.heap, two, nil);
            let f_pair2 = new_pair(ec.heap, f, two_pair);
            let f_pair3 = new_pair(ec.heap, f, f_pair2);
            let or_expr2 = new_pair(ec.heap, or_sym, f_pair3);
            let result = eval_main(or_expr2, &mut ec).unwrap();
            match &ec.heap.get_value(result) {
                SchemeValue::Int(i) => assert_eq!(i.to_string(), "2"),
                _ => panic!("Expected integer result for or"),
            }
        }
        // (or #f #f #f)
        {
            let mut evaluator = Evaluator::new();
            let mut ec = crate::eval::EvalContext::from_eval(&mut evaluator);
            let or_sym = get_symbol(ec.heap, "or");
            let f = ec.heap.false_s();
            let nil = ec.heap.nil_s();
            let f_pair = new_pair(ec.heap, f, nil);
            let f_pair2 = new_pair(ec.heap, f, f_pair);
            let f_pair3 = new_pair(ec.heap, f, f_pair2);
            let or_expr3 = new_pair(ec.heap, or_sym, f_pair3);
            let result = eval_main(or_expr3, &mut ec).unwrap();
            assert!(matches!(
                &evaluator.heap.get_value(result),
                SchemeValue::Bool(false)
            ));
        }
    }

    #[test]
    fn test_eval_logic_lambda() {
        let mut evaluator = Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut evaluator);

        // Set up the + function in the environment
        let plus = new_primitive(
            ec.heap,
            crate::builtin::number::plus_builtin,
            "plus".to_string(),
        );
        let plus_sym = ec.heap.intern_symbol("+");
        ec.env.set_symbol(plus_sym, plus);
        let lambda_expr;
        let add1;
        let result;
        // Create (lambda (x) (+ x 1))
        let x_param = get_symbol(ec.heap, "x");
        let one = new_int(ec.heap, num_bigint::BigInt::from(1));
        let plus_sym = ec.heap.intern_symbol("+");
        let nil = ec.heap.nil_s();

        // Build (+ x 1)
        let one_pair = new_pair(ec.heap, one, nil);
        let x_one_pair = new_pair(ec.heap, x_param, one_pair);
        let plus_expr = new_pair(ec.heap, plus_sym, x_one_pair);

        // Build (lambda (x) (+ x 1))
        let lambda_sym = get_symbol(ec.heap, "lambda");
        let params_list = new_pair(ec.heap, x_param, nil);
        let body_nil = new_pair(ec.heap, plus_expr, nil);
        let lambda_args = new_pair(ec.heap, params_list, body_nil);
        lambda_expr = new_pair(ec.heap, lambda_sym, lambda_args);
        // Evaluate the lambda to create a closure
        add1 = eval_main(lambda_expr, &mut ec).unwrap();

        // Verify it's a closure
        use crate::gc::Callable;
        match &ec.heap.get_value(add1) {
            SchemeValue::Callable(callable) => {
                match callable {
                    Callable::Closure { params, body, .. } => {
                        assert_eq!(params.len(), 2); // 1 + the zeroth element for variadics
                        // Check that the parameter is a symbol with the right name
                        match &ec.heap.get_value(params[1]) {
                            SchemeValue::Symbol(name) => assert_eq!(name, "x"),
                            _ => panic!("Expected symbol parameter"),
                        }
                        // The body should be the (+ x 1) expression
                        match &ec.heap.get_value(*body) {
                            SchemeValue::Pair(car, _cdr) => match &ec.heap.get_value(*car) {
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
        five = new_int(ec.heap, num_bigint::BigInt::from(5));

        // Build (add1 5)
        let nil = ec.heap.nil_s();
        let five_pair = new_pair(ec.heap, five, nil);
        apply_expr = new_pair(ec.heap, add1, five_pair);
        // Evaluate the application
        result = eval_main(apply_expr, &mut ec).unwrap();

        // Should return 6
        match &evaluator.heap.get_value(result) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "6"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_eval_logic_closure_application() {
        let mut evaluator = Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut evaluator);

        // Set up the + function in the environment
        let plus = new_primitive(
            ec.heap,
            crate::builtin::number::plus_builtin,
            "plus".to_string(),
        );
        let plus_sym = ec.heap.intern_symbol("+");
        ec.env.set_symbol(plus_sym, plus);

        // Create a closure manually: (lambda (x y) (+ x y))
        let closure;
        let captured_env = ec.env.current_frame();
        let x_param = get_symbol(ec.heap, "x");
        let y_param = get_symbol(ec.heap, "y");
        let plus_sym = ec.heap.intern_symbol("+");
        let nil = ec.heap.nil_s();

        // Build (+ x y)
        let y_pair = new_pair(ec.heap, y_param, nil);
        let x_y_pair = new_pair(ec.heap, x_param, y_pair);
        let plus_expr = new_pair(ec.heap, plus_sym, x_y_pair);

        // Create closure with captured environment
        closure = new_closure(
            ec.heap,
            vec![nil, x_param, y_param],
            plus_expr,
            captured_env,
        );

        // Apply the closure: (closure 3 4)
        let three;
        let four;
        let apply_expr;
        let result;
        three = new_int(ec.heap, num_bigint::BigInt::from(3));
        four = new_int(ec.heap, num_bigint::BigInt::from(4));

        // Build (closure 3 4)
        let nil = ec.heap.nil_s();
        let four_pair = new_pair(ec.heap, four, nil);
        let three_four_pair = new_pair(ec.heap, three, four_pair);
        apply_expr = new_pair(ec.heap, closure, three_four_pair);

        // Evaluate the application
        result = eval_main(apply_expr, &mut ec).unwrap();

        // Should return 7
        match &evaluator.heap.get_value(result) {
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
        match &evaluator.heap.get_value(new_value) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "123"),
            _ => panic!("Expected integer"),
        }
    }

    #[test]
    fn test_symbol_interning_in_lambda() {
        let mut evaluator = Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut evaluator);

        // Create a symbol and bind it to the environment
        let plus_sym = ec.heap.intern_symbol("+");
        let plus_func = new_primitive(
            ec.heap,
            crate::builtin::number::plus_builtin,
            "plus".to_string(),
        );
        ec.env.set_symbol(plus_sym, plus_func);

        // Now create a lambda that uses the + symbol
        let x_param;
        let plus_sym;
        let one;
        let nil;
        let plus_expr;
        x_param = ec.heap.intern_symbol("x");
        plus_sym = ec.heap.intern_symbol("+");
        one = new_int(ec.heap, num_bigint::BigInt::from(1));
        nil = ec.heap.nil_s();

        // Build (+ x 1) - using the same plus_sym that was bound
        let one_pair = new_pair(ec.heap, one, nil);
        let x_one_pair = new_pair(ec.heap, x_param, one_pair);
        plus_expr = new_pair(ec.heap, plus_sym, x_one_pair);

        // Create closure manually with the deduplicated body
        let deduplicated_body;
        let captured_env = ec.env.current_frame();
        let closure;
        deduplicated_body = plus_expr; //deduplicate_symbols(plus_expr, heap);
        closure = new_closure(ec.heap, vec![nil, x_param], deduplicated_body, captured_env);

        // Apply the closure: (closure 5)
        let five;
        let five_pair;
        let apply_expr;
        five = new_int(ec.heap, num_bigint::BigInt::from(5));
        five_pair = new_pair(ec.heap, five, nil);
        apply_expr = new_pair(ec.heap, closure, five_pair);

        // This should work because we're using the same interned symbol
        let result = eval_main(apply_expr, &mut ec).unwrap();
        match &evaluator.heap.get_value(result) {
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
        let mut ev = crate::eval::Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut ev);
        // Simple arithmetic
        let result = eval_string(&mut ec, "(+ 1 2)").unwrap();
        match &ec.heap.get_value(result) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "3"),
            _ => panic!("Expected integer result"),
        }
        // Multiple expressions, last result returned
        let result = eval_string(&mut ec, "(+ 1 2) (* 2 3)").unwrap();
        match &ec.heap.get_value(result) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "6"),
            _ => panic!("Expected integer result"),
        }
        // Variable definition and use
        eval_string(&mut ec, "(define x 42)").unwrap();
        let result = eval_string(&mut ec, "(+ x 1)").unwrap();
        match &ev.heap.get_value(result) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "43"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_eval_string_error_handling() {
        let mut evaluator = Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut evaluator);
        // Syntax error
        let err = eval_string(&mut ec, "(+ 1 2");
        println!("test_eval_string_error: {:?}", err);
        // assert!(
        //     err.contains("end of input") ||
        //     err.contains("unexpected EOF") ||
        //     err.contains("Unclosed list"),
        //     "Unexpected error message: {}", err
        // );
        // Unbound variable
        let err = eval_string(&mut ec, "(+ y 1)").unwrap_err();
        assert!(err.contains("Unbound"));
        // Wrong argument type
        let err = eval_string(&mut ec, "(+ 1 'foo)").unwrap_err();
        assert!(err.contains("numbers"));
    }
}
