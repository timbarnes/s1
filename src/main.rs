mod gc;
mod io;
mod tokenizer;
mod parser;
// mod eval;
// mod printer;
// mod builtin;

use crate::gc::GcHeap;
use crate::parser::ParserSimple;
use crate::io::{Port, PortKind};
use argh::FromArgs;

#[derive(FromArgs)]
/// A simple Scheme interpreter
struct Args {
    /// input file to execute (optional)
    #[argh(positional)]
    file: Option<String>,
}

fn main() {
    // Test parser with new GC system
    println!("Testing ParserSimple with new GC system...");
    
    let mut heap = GcHeap::new();
    
    // Test cases for leaf-level functions
    let test_cases = vec![
        "42",           // Number
        "3.14",         // Float
        "hello",        // Symbol
        "\"world\"",    // String
        "#t",           // Boolean true
        "#f",           // Boolean false
        "#\\a",         // Character
        "nil",          // Nil
        "(1 2 3)",      // Simple list
        "'hello",       // Quoted expression
        "#(1 2 3)",     // Vector
        "'(a b c)",     // Quoted list
        "#(1 2)",       // Simple vector
        "'(a)",         // Quoted single element
        "#(1 '(a))",    // Vector with nested quoted expression
    ];
    
    for test_input in test_cases {
        println!("\nParsing: {}", test_input);
        
        // Create a string port
        let mut port = Port {
            kind: PortKind::StringPortInput {
                content: test_input.to_string(),
                pos: 0,
            }
        };
        
        // Create parser and parse
        let mut parser = ParserSimple::new();
        match parser.parse(&mut heap, &mut port) {
            Ok(result) => {
                println!("  Result: {:?}", result);
                // Print the value for debugging
                match &result.value {
                    crate::gc::SchemeValueSimple::Int(i) => println!("  Value: Int({})", i),
                    crate::gc::SchemeValueSimple::Float(f) => println!("  Value: Float({})", f),
                    crate::gc::SchemeValueSimple::Symbol(s) => println!("  Value: Symbol({})", s),
                    crate::gc::SchemeValueSimple::Str(s) => println!("  Value: String({})", s),
                    crate::gc::SchemeValueSimple::Bool(b) => println!("  Value: Bool({})", b),
                    crate::gc::SchemeValueSimple::Char(c) => println!("  Value: Char({})", c),
                    crate::gc::SchemeValueSimple::Nil => println!("  Value: Nil"),
                    _ => println!("  Value: {:?}", result.value),
                }
            }
            Err(e) => println!("  Error: {}", e),
        }
    }
    
    println!("\nParser test complete!");
}
