mod gc;
mod io;
mod tokenizer;
mod parser;
mod eval;
mod evalsimple;
mod builtin;

use crate::gc::GcHeap;
use crate::parser::ParserSimple;
use crate::eval::EvaluatorSimple;
use crate::builtin::register_all_simple;
use crate::io::{Port, PortKind};
use crate::evalsimple::{Evaluator, eval_logic};
use argh::FromArgs;
use num_bigint::BigInt;

#[derive(FromArgs)]
/// A simple Scheme interpreter
struct Args {
    /// input file to execute (optional)
    #[argh(positional)]
    file: Option<String>,
}

fn main() {
    println!("Testing EvaluatorSimple with new GC system...\n");

    let mut heap = GcHeap::new();
    let mut evaluator = Evaluator::new();

    // Register builtins
    register_all_simple(&mut heap, evaluator.env_mut().bindings_mut());

    // Test cases for self-evaluating forms and symbol lookup
    let test_cases = vec![
        ("42", "Number"),
        ("3.14", "Float"), 
        ("\"hello\"", "String"),
        ("#t", "Boolean true"),
        ("#f", "Boolean false"),
        ("#\\a", "Character"),
        ("nil", "Nil"),
        ("x", "Symbol (unbound)"),
        ("(+ 1 2)", "Basic addition"),
        ("(* 3 4)", "Basic multiplication"),
        ("(number? 42)", "Number predicate"),
        ("(type-of 42)", "Type of function"),
        // Special forms
        ("'foo", "Quote special form (symbol)"),
        ("'(1 2 3)", "Quote special form (list)"),
        ("(begin 1 2 3)", "Begin special form (sequence)"),
        ("(begin (+ 1 2) (* 3 4))", "Begin special form (multiple expressions)"),
        // Define and variable lookup
        ("(define x 22)", "Define x to 22"),
        ("(define y 'z)", "Define y to symbol z"),
        ("x", "Lookup x after define"),
        ("y", "Lookup y after define"),
    ];

    // Add a binding for symbol lookup test
    let x_value = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(99));
    evaluator.env_mut().set("x".to_string(), x_value);

    for (input, description) in test_cases {
        println!("Testing {}: {}", description, input);
        
        // Create a port for the input
        let mut port = Port { 
            kind: PortKind::StringPortInput { 
                content: input.to_string(), 
                pos: 0 
            } 
        };
        
        // Parse the expression
        let mut parser = ParserSimple::new();
        match parser.parse(&mut heap, &mut port) {
            Ok(expr) => {
                // Evaluate the expression using eval_logic from evalsimple
                match eval_logic(expr, &mut evaluator) {
                    Ok(result) => {
                        println!("  Parsed: {:?}", result.value);
                        // Print the value in a readable format
                        match &result.value {
                            crate::gc::SchemeValueSimple::Int(i) => println!("  => {}", i),
                            crate::gc::SchemeValueSimple::Float(f) => println!("  => {}", f),
                            crate::gc::SchemeValueSimple::Str(s) => println!("  => \"{}\"", s),
                            crate::gc::SchemeValueSimple::Bool(b) => println!("  => {}", b),
                            crate::gc::SchemeValueSimple::Char(c) => println!("  => #\\{}", c),
                            crate::gc::SchemeValueSimple::Nil => println!("  => nil"),
                            crate::gc::SchemeValueSimple::Symbol(s) => println!("  => {}", s),
                            _ => println!("  => {:?}", result.value),
                        }
                    }
                    Err(e) => {
                        println!("  Evaluation error: {}", e);
                    }
                }
            }
            Err(e) => {
                println!("  Parse error: {}", e);
            }
        }
        println!();
    }

    println!("Evaluator test complete!");
}
