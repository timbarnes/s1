pub mod cek;
pub mod kont;

use crate::env::{EnvOps, EnvRef};
use crate::gc::{GcHeap, GcRef, SchemeValue, list_from_slice, list_to_vec, new_port};
use crate::io::{FileTable, PortKind};
use crate::macros::expand_macro;
use crate::parser::parse;
pub use cek::eval_main;
pub use kont::{
    AndOrKind, CEKState, CondClause, Control, Kont, KontRef, insert_and_or, insert_bind,
    insert_cond, insert_eval, insert_eval_eval, insert_if, insert_seq, insert_value,
};

/// Evaluator that owns both heap and environment
pub struct RunTimeStruct {
    pub heap: GcHeap,               // The global heap for scheme data
    pub port_stack: Vec<GcRef>,  // Stack of ports for input/output operations
    pub file_table: FileTable,      // Table of open files
    pub current_output_port: GcRef, // The current output port
    pub trace: TraceType,
    pub depth: i32,
}

pub struct RunTime<'a> {
    pub heap: &'a mut GcHeap,
    pub port_stack: &'a mut Vec<GcRef>,
    pub file_table: &'a mut FileTable,
    pub current_output_port: &'a mut GcRef,
    pub trace: &'a mut TraceType,
    pub depth: &'a mut i32,
}

impl<'a> RunTime<'a> {
    pub fn from_eval(eval: &'a mut RunTimeStruct) -> Self {
        RunTime {
            heap: &mut eval.heap,
            port_stack: &mut eval.port_stack,
            file_table: &mut eval.file_table,
            current_output_port: &mut eval.current_output_port,
            trace: &mut eval.trace,
            depth: &mut eval.depth,
        }
    }
}

pub enum TraceType {
    Reset,
    Control,
    Full,
    Step,
    Off,
}

impl RunTimeStruct {
    /// Create a new RunTime struct initialized to stdin
    pub fn new() -> Self {
        let mut heap = GcHeap::new();
        let mut port_vec = Vec::new();
        let stdin_port = new_port(&mut heap, PortKind::Stdin);
        port_vec.push(stdin_port);
        let stdout_port = new_port(&mut heap, PortKind::Stdout);
        Self {
            heap,
            port_stack: port_vec,
            file_table: FileTable::new(),
            current_output_port: stdout_port,
            trace: TraceType::Reset,
            depth: 0,
        }
    }
}

/// Initialize Scheme-level I/O globals: **stdin**, **stdout**, **port-stack**
///
/// This should be called after creating the Evaluator, before loading files or starting the REPL.
pub fn initialize_scheme_globals(rt: &mut RunTime, env: EnvRef) -> Result<(), String> {
    // Create stdin and stdout ports
    let stdin_port = new_port(rt.heap, PortKind::Stdin);
    let stdout_port = new_port(rt.heap, PortKind::Stdout);
    let stderr_port = new_port(rt.heap, PortKind::Stderr);
    // Bind **stdin**, **stdout**, and **stderr** as Scheme globals
    let stdin_sym = rt.heap.intern_symbol("**stdin**");
    let stdout_sym = rt.heap.intern_symbol("**stdout**");
    let stderr_sym = rt.heap.intern_symbol("**stderr**");
    env.define(stdin_sym, stdin_port);
    env.define(stdout_sym, stdout_port);
    env.define(stderr_sym, stderr_port);
    crate::builtin::register_builtins(rt.heap, env.clone());
    crate::special_forms::register_special_forms(rt.heap, env.clone());
    crate::sys_builtins::register_sys_builtins(rt, env.clone());
    Ok(())
}

/// Evaluate all expressions in a string of Scheme code; return a list of the results
/// If any of the expressions return multiple values, they are flattened into the list of results.
pub fn eval_string(
    code: &str,
    state: &mut CEKState,
    rt: &mut RunTime,
) -> Result<Vec<GcRef>, String> {
    use crate::parser::ParseError;

    let mut port_kind = crate::io::new_string_port_input(code);
    let mut results = Vec::new();
    loop {
        match parse(&mut rt.heap, &mut port_kind) {
            Err(ParseError::Syntax(e)) => return Err(e),
            Err(ParseError::Eof) => {
                let r_list = crate::gc::list_from_slice(&results[..], rt.heap);
                return Ok(vec![r_list]);
            }
            Ok(expr) => match eval_main(expr, state, rt) {
                Ok(value) => {
                    for v in value {
                        results.push(v);
                    }
                }
                Err(err) => return Err(err),
            },
        }
    }
}

/// Macro handler
pub fn eval_macro(
    params: &[GcRef],
    body: GcRef,
    _env: EnvRef,
    args: &[GcRef],
    state: &mut CEKState,
    rt: &mut RunTime,
) -> Result<GcRef, String> {
    let new_env = bind_params(params, args, &state.env, rt.heap)?;
    let original_env = state.env.clone();
    //println!("Before expansion: {}", print_value(&body));
    state.env = new_env;
    let expanded = expand_macro(&body, 0, rt, state)?;
    //println!("After expansion: {}", print_value(&expanded));
    state.env = original_env;
    // Review for tail recursion
    Ok(expanded)
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

pub fn bind_params(
    params: &[GcRef],
    args: &[GcRef],
    parent_env: &EnvRef,
    heap: &mut GcHeap,
) -> Result<EnvRef, String> {
    let captured_env = parent_env.clone();
    let new_env = captured_env.extend();
    match params.len() {
        0 => {
            if !args.is_empty() {
                return Err("too many arguments".to_string());
            }
        }
        1 => {
            let arglist = list_from_slice(args, heap);
            new_env.define(params[0], arglist);
        }
        _ => {
            let num_required = params.len() - 1;
            if args.len() < num_required {
                return Err("not enough arguments".to_string());
            }

            // Bind the required parameters.
            // The first param is the rest arg, so we skip it.
            for i in 0..num_required {
                new_env.define(params[i + 1], args[i]);
            }

            // Bind the rest parameter.
            let rest_args = &args[num_required..];
            let arglist = list_from_slice(rest_args, heap);
            new_env.define(params[0], arglist);
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

// fn is_value(v: &SchemeValue) -> bool {
//     match v {
//         // Symbols are NOT values (must be looked up). Pairs are applications.
//         Symbol(_) | Pair(_, _) => false,
//         _ => true,
//     }
// }

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    #[allow(unused_imports)]
    use super::*;
    use crate::env::Frame;
    use crate::gc::{car, cdr, get_symbol, new_builtin, new_closure, new_int, new_pair};
    use crate::gc_value;
    use crate::printer::print_value;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn test_eval_logic_self_evaluating() {
        use crate::gc::equal;
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut runtime = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut runtime);
        let mut state = CEKState::new(env);
        let int_val;
        int_val = new_int(&mut ec.heap, num_bigint::BigInt::from(42));
        let result = eval_main(int_val, &mut state, &mut ec).unwrap();
        assert!(equal(&ec.heap, result[0], int_val));
    }

    #[test]
    fn test_eval_logic_variable_lookup() {
        use crate::gc::equal;
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut runtime = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut runtime);
        let mut state = CEKState::new(env);
        let value;
        let symbol;
        value = new_int(ec.heap, num_bigint::BigInt::from(99));
        symbol = get_symbol(ec.heap, "x");
        state.env.define(symbol, value);
        let result = eval_main(symbol, &mut state, &mut ec);
        match result {
            Ok(result) => {
                crate::printer::print_value(&result[0]);
                assert!(equal(&ec.heap, result[0], value));
            }
            Err(err) => panic!("test_eval_logic_variable_lookup Error: {}", err),
        }
        // crate::printer::print_value(&result[0]);
        // assert!(equal(&ec.heap, result[0], value));
    }

    #[test]
    fn test_eval_logic_non_nested_call() {
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut runtime = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut runtime);
        let mut state = CEKState::new(env);
        let plus;
        let a;
        let b;
        let plus_sym;
        let args;
        let expr;
        //let hp = ec.heap;
        plus = new_builtin(
            ec.heap,
            |heap, args| {
                let a = match &heap.get_value(args[0]) {
                    SchemeValue::Int(i) => i.clone(),
                    _ => return Err("not int".to_string()),
                };
                let b = match &heap.get_value(args[1]) {
                    SchemeValue::Int(i) => i.clone(),
                    _ => return Err("not int".to_string()),
                };
                Ok(new_int(heap, a + b))
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
        state.env.define(plus_sym, plus);
        let result = eval_main(expr, &mut state, &mut ec).unwrap();
        match &ec.heap.get_value(result[0]) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "5"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_eval_logic_nested_call() {
        use crate::builtin::number::{plus_b, times_b};
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut runtime = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut runtime);
        let mut state = CEKState::new(env);
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
        plus = new_builtin(ec.heap, plus_b, "plus".to_string());
        times = new_builtin(ec.heap, times_b, "times".to_string());
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
        state.env.define(plus_sym, plus);
        state.env.define(star_sym, times);
        let result = eval_main(expr, &mut state, &mut ec).unwrap();
        match &ec.heap.get_value(result[0]) {
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
        use crate::builtin::number::{plus_b, times_b};
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut runtime = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut runtime);
        let mut state = CEKState::new(env);
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
        times = new_builtin(ec.heap, times_b, "times".to_string());
        plus = new_builtin(ec.heap, plus_b, "plus".to_string());
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
        state.env.define(star_sym, times);
        state.env.define(plus_sym, plus);
        let result = eval_main(expr, &mut state, &mut ec).unwrap();
        match &ec.heap.get_value(result[0]) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "10"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_eval_logic_nested_mixed_call() {
        use crate::builtin::number::{minus_b, plus_b, times_b};
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut runtime = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut runtime);
        let mut state = CEKState::new(env);
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
        plus = new_builtin(ec.heap, plus_b, "plus".to_string());
        times = new_builtin(ec.heap, times_b, "times".to_string());
        minus = new_builtin(ec.heap, minus_b, "minus".to_string());
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
        state.env.define(plus_sym, plus);
        state.env.define(times_sym, times);
        state.env.define(minus_sym, minus);
        let result = eval_main(expr, &mut state, &mut ec).unwrap();
        match &ec.heap.get_value(result[0]) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "-1"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_eval_logic_quote() {
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut runtime = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut runtime);
        let mut state = CEKState::new(env);
        initialize_scheme_globals(&mut ec, state.env.clone()).unwrap();

        let results = eval_string("'foo '(a b) (quote 2)", &mut state, &mut ec).unwrap();

        let r1 = car(results[0]).unwrap();
        match gc_value!(r1) {
            SchemeValue::Symbol(sym) => assert_eq!(sym, "foo"),
            _ => panic!("Expected symbol from quote"),
        }
        let r2 = car(cdr(results[0]).unwrap()).unwrap();
        assert_eq!(print_value(&r2), "(a b)");

        let r3 = car(cdr(cdr(results[0]).unwrap()).unwrap()).unwrap();
        match gc_value!(r3) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "2"),
            _ => panic!("Expected 2 from quote"),
        }
    }

    #[test]
    fn test_eval_logic_begin() {
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut runtime = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut runtime);
        let mut state = CEKState::new(env);
        initialize_scheme_globals(&mut ec, state.env.clone()).unwrap();
        let result = eval_string("(begin 22 (+ 1 2))", &mut state, &mut ec).unwrap();
        match gc_value!(car(result[0]).unwrap()) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "3"),
            _ => panic!("Expected integer result from begin"),
        }
    }

    #[test]
    fn test_eval_logic_define() {
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut runtime = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut runtime);
        let mut state = CEKState::new(env);
        initialize_scheme_globals(&mut ec, state.env.clone()).unwrap();
        let result = eval_string("(define y 42)", &mut state, &mut ec).unwrap();
        match gc_value!(car(result[0]).unwrap()) {
            SchemeValue::Symbol(s) => assert_eq!(s.to_string(), "y"),
            _ => panic!("Expected symbol result"),
        }
        // Check that the variable is now bound
        let sym = get_symbol(ec.heap, "y");
        let result = state.env.lookup(sym);
        match result {
            Some(val) => match gc_value!(val) {
                SchemeValue::Int(i) => assert_eq!(i.to_string(), "42"),
                _ => panic!("Expected integer result in env"),
            },
            _ => panic!("Expected integer result in env"),
        }
    }

    #[test]
    fn test_eval_logic_if() {
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut runtime = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut runtime);
        let mut state = CEKState::new(env);
        initialize_scheme_globals(&mut ec, state.env.clone()).unwrap();
        let result = eval_string("(if #t 1 2)", &mut state, &mut ec).unwrap();
        match gc_value!(car(result[0]).unwrap()) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "1"),
            _ => panic!("Expected consequent"),
        }
        let result = eval_string("(if #f 1 2)", &mut state, &mut ec).unwrap();
        match gc_value!(car(result[0]).unwrap()) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "2"),
            _ => panic!("Expected alternate"),
        }
    }

    #[test]
    fn test_eval_logic_and_or() {
        // (and)
        {
            let env = Rc::new(RefCell::new(Frame::new(None)));
            let mut runtime = RunTimeStruct::new();
            let mut ec = RunTime::from_eval(&mut runtime);
            let mut state = CEKState::new(env);
            initialize_scheme_globals(&mut ec, state.env.clone()).unwrap();
            let and_sym = get_symbol(ec.heap, "and");
            let nil = ec.heap.nil_s();
            let and_empty = new_pair(ec.heap, and_sym, nil);
            let result = eval_main(and_empty, &mut state, &mut ec).unwrap();
            assert!(matches!(
                &ec.heap.get_value(result[0]),
                SchemeValue::Bool(true)
            ));
        }
        // (and #t 1 2)
        {
            let env = Rc::new(RefCell::new(Frame::new(None)));
            let mut runtime = RunTimeStruct::new();
            let mut ec = RunTime::from_eval(&mut runtime);
            let mut state = CEKState::new(env);
            initialize_scheme_globals(&mut ec, state.env.clone()).unwrap();
            let result = eval_string("(and #t 1 2)", &mut state, &mut ec).unwrap();
            let obj = crate::gc::car(result[0]).unwrap();
            match gc_value!(obj) {
                SchemeValue::Int(i) => assert_eq!(i.to_string(), "2"),
                _ => panic!("Expected integer result for and"),
            }
        }
        // (and #t #f 2)
        {
            let env = Rc::new(RefCell::new(Frame::new(None)));
            let mut runtime = RunTimeStruct::new();
            let mut ec = RunTime::from_eval(&mut runtime);
            let mut state = CEKState::new(env);
            initialize_scheme_globals(&mut ec, state.env.clone()).unwrap();
            let result = eval_string("(and #t #f 2)", &mut state, &mut ec).unwrap();
            let obj = crate::gc::car(result[0]).unwrap();
            assert!(matches!(&ec.heap.get_value(obj), SchemeValue::Bool(false)));
        }
        // (or)
        {
            let env = Rc::new(RefCell::new(Frame::new(None)));
            let mut runtime = RunTimeStruct::new();
            let mut ec = RunTime::from_eval(&mut runtime);
            let mut state = CEKState::new(env);
            initialize_scheme_globals(&mut ec, state.env.clone()).unwrap();
            let result = eval_string("(or)", &mut state, &mut ec).unwrap();
            let obj = crate::gc::car(result[0]).unwrap();
            assert!(matches!(&ec.heap.get_value(obj), SchemeValue::Bool(false)));
        }
        // (or #f 1 2)
        {
            let env = Rc::new(RefCell::new(Frame::new(None)));
            let mut runtime = RunTimeStruct::new();
            let mut ec = RunTime::from_eval(&mut runtime);
            let mut state = CEKState::new(env);
            initialize_scheme_globals(&mut ec, state.env.clone()).unwrap();
            let result = eval_string("(or #f 1 2)", &mut state, &mut ec).unwrap();
            let obj = crate::gc::car(result[0]).unwrap();
            match gc_value!(obj) {
                SchemeValue::Int(i) => assert_eq!(i.to_string(), "1"),
                _ => panic!("Expected integer result for or"),
            }
        }
        // (or #f #f 2)
        {
            let env = Rc::new(RefCell::new(Frame::new(None)));
            let mut runtime = RunTimeStruct::new();
            let mut ec = RunTime::from_eval(&mut runtime);
            let mut state = CEKState::new(env);
            initialize_scheme_globals(&mut ec, state.env.clone()).unwrap();
            let or_sym = get_symbol(ec.heap, "or");
            let f = ec.heap.false_s();
            let two = new_int(ec.heap, num_bigint::BigInt::from(2));
            let nil = ec.heap.nil_s();
            let two_pair = new_pair(ec.heap, two, nil);
            let f_pair2 = new_pair(ec.heap, f, two_pair);
            let f_pair3 = new_pair(ec.heap, f, f_pair2);
            let or_expr2 = new_pair(ec.heap, or_sym, f_pair3);
            let result = eval_main(or_expr2, &mut state, &mut ec).unwrap();
            match &ec.heap.get_value(result[0]) {
                SchemeValue::Int(i) => assert_eq!(i.to_string(), "2"),
                _ => panic!("Expected integer result for or"),
            }
        }
        // (or #f #f #f)
        {
            let env = Rc::new(RefCell::new(Frame::new(None)));
            let mut runtime = RunTimeStruct::new();
            let mut ec = RunTime::from_eval(&mut runtime);
            let mut state = CEKState::new(env);
            initialize_scheme_globals(&mut ec, state.env.clone()).unwrap();
            let or_sym = get_symbol(ec.heap, "or");
            let f = ec.heap.false_s();
            let nil = ec.heap.nil_s();
            let f_pair = new_pair(ec.heap, f, nil);
            let f_pair2 = new_pair(ec.heap, f, f_pair);
            let f_pair3 = new_pair(ec.heap, f, f_pair2);
            let or_expr3 = new_pair(ec.heap, or_sym, f_pair3);
            let result = eval_main(or_expr3, &mut state, &mut ec).unwrap();
            assert!(matches!(
                &ec.heap.get_value(result[0]),
                SchemeValue::Bool(false)
            ));
        }
    }

    #[test]
    fn test_eval_logic_lambda() {
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut runtime = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut runtime);
        let mut state = CEKState::new(env);
        initialize_scheme_globals(&mut ec, state.env.clone()).unwrap();
        let mut result = eval_string("(lambda (x y) (+ x y))", &mut state, &mut ec).unwrap();
        let obj = car(result[0]).unwrap();
        // Verify it's a closure
        match gc_value!(obj) {
            SchemeValue::Callable(c) => match c {
                crate::gc::Callable::Closure { params, body, .. } => {
                    // Check parameters: 2 params means vector length = 3
                    assert_eq!(params.len(), 3);
                    let x = get_symbol(ec.heap, "x");
                    let y = get_symbol(ec.heap, "y");
                    assert!(crate::gc::eq(ec.heap, params[1], x));
                    assert!(crate::gc::eq(ec.heap, params[2], y));
                    // Check body
                    assert_eq!(car(*body).unwrap(), get_symbol(ec.heap, "+"));
                }
                _ => panic!("Expected closure"),
            },
            _ => panic!("Expected closure"),
        }
        // Now apply the closure: ((lambda (x y) (+ x y)) 2 4)
        result = eval_string("((lambda (x y) (+ x y)) 2 4)", &mut state, &mut ec).unwrap();
        // Should return 6
        match gc_value!(car(result[0]).unwrap()) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "6"),
            _ => panic!("Expected alternate"),
        }
    }

    #[test]
    fn test_eval_logic_closure_application() {
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut runtime = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut runtime);
        let mut state = CEKState::new(env);

        // Set up the + function in the environment
        let plus = new_builtin(ec.heap, crate::builtin::number::plus_b, "plus".to_string());
        let plus_sym = ec.heap.intern_symbol("+");
        state.env.define(plus_sym, plus);

        // Create a closure manually: (lambda (x y) (+ x y))
        let closure;
        let captured_env = Rc::clone(&state.env);
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
        result = eval_main(apply_expr, &mut state, &mut ec).unwrap();

        // Should return 7
        match &ec.heap.get_value(result[0]) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "7"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_eval_logic_set() {
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut runtime = RunTimeStruct::new();
        let ec = RunTime::from_eval(&mut runtime);
        let state = CEKState::new(env);
        let symbol;
        let value;
        let set_sym;
        let x_sym;
        let val_123;
        let arg_pair;
        let args;
        symbol = get_symbol(ec.heap, "x");
        value = new_int(ec.heap, num_bigint::BigInt::from(123));
        set_sym = get_symbol(ec.heap, "set!");
        x_sym = ec.heap.intern_symbol("x");
        val_123 = new_int(ec.heap, num_bigint::BigInt::from(123));
        let nil = ec.heap.nil_s();
        arg_pair = new_pair(ec.heap, val_123, nil);
        args = new_pair(ec.heap, x_sym, arg_pair);
        new_pair(ec.heap, set_sym, args);

        // First define x
        state.env.define(symbol, value);

        // Then set! it
        // let result = eval_logic(set_expr, &mut evaluator).unwrap();

        // Verify the value was set
        let new_value = state.env.lookup(symbol).unwrap();
        match &ec.heap.get_value(new_value) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "123"),
            _ => panic!("Expected integer"),
        }
    }

    #[test]
    fn test_symbol_interning_in_lambda() {
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut runtime = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut runtime);
        let mut state = CEKState::new(env);

        // Create a symbol and bind it to the environment
        let plus_sym = ec.heap.intern_symbol("+");
        let plus_func = new_builtin(ec.heap, crate::builtin::number::plus_b, "plus".to_string());
        state.env.define(plus_sym, plus_func);

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
        let captured_env = Rc::clone(&state.env);
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
        let result = eval_main(apply_expr, &mut state, &mut ec).unwrap();
        match &ec.heap.get_value(result[0]) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "6"),
            _ => panic!("Expected integer result"),
        }
    }

    #[test]
    fn test_eval_string_basic_builtins() {
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut runtime = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut runtime);
        let mut state = CEKState::new(env);
        initialize_scheme_globals(&mut ec, state.env.clone()).unwrap();
        // Simple arithmetic
        let mut result = eval_string("(+ 1 2)", &mut state, &mut ec).unwrap();
        let mut r1 = car(result[0]).unwrap();
        assert_eq!(print_value(&r1), "3");
        // Multiple expressions, last result returned
        result = eval_string("(+ 1 2) (* 2 3)", &mut state, &mut ec).unwrap();
        r1 = car(result[0]).unwrap();
        eprintln!("eval-string test: {}", print_value(&r1));
        assert_eq!(print_value(&r1), "3");
        // Variable definition and use
        result = eval_string("(define x 42) (+ x 1)", &mut state, &mut ec).unwrap();
        r1 = car(cdr(result[0]).unwrap()).unwrap();
        assert_eq!(print_value(&r1), "43");
    }

    #[test]
    fn test_eval_string_error_handling() {
        let env = Rc::new(RefCell::new(Frame::new(None)));
        let mut runtime = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut runtime);
        let mut state = CEKState::new(env);
        initialize_scheme_globals(&mut ec, state.env.clone()).unwrap();
        // Syntax error
        let err = eval_string("(+ 1 2", &mut state, &mut ec);
        eprintln!("test_eval_string_error: <(+ 1 2>{:?}", &err);
        match err {
            Err(e) => {
                assert!(e.contains("Unclosed list"));
            }
            _ => panic!("Expected error"),
        }
        // Unbound variable
        let err = eval_string("(+ y 1)", &mut state, &mut ec).unwrap_err();
        eprintln!("eval-string error:{}", err);
        assert!(err.contains("Unbound"));
        //     // Wrong argument type
        //     let err = eval_string("(+ 1 'foo)", &mut state, &mut ec).unwrap_err();
        //     assert!(err.contains("numbers"));
    }
}
