pub mod number;
pub mod predicate;

use crate::gc::{GcHeap, GcRef, new_primitive, new_string};
use std::collections::HashMap;

pub fn register_all(heap: &mut GcHeap, env: &mut HashMap<String, GcRef>) {
    // Register type predicate as a test
    let is_number = new_primitive(
        heap,
        std::rc::Rc::new(predicate::number_q),
        "(number? x): Returns #t if x is a number, #f otherwise.".to_string(),
    );
    env.insert("number?".to_string(), is_number);
    // Register help builtin
    let help = new_primitive(
        heap,
        std::rc::Rc::new(help_builtin),
        "(help 'symbol): Prints documentation for the given builtin symbol.".to_string(),
    );
    env.insert("help".to_string(), help);
    // Add more builtins here
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