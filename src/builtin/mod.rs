pub mod number;
pub mod predicate;

use crate::gc::{GcHeap, GcRef, new_primitive, new_string};
use std::collections::HashMap;
use std::rc::Rc;
use num_bigint::BigInt;
use crate::gc::{as_int, as_float, new_int, new_float, SchemeValue};
use num_traits::ToPrimitive;
use number::{plus_builtin, minus_builtin, times_builtin, div_builtin, mod_builtin};
use crate::printer::scheme_display;

pub enum BuiltinKind {
    SpecialForm(Rc<dyn Fn(&mut crate::gc::GcHeap, &[crate::gc::GcRef], &mut std::collections::HashMap<String, BuiltinKind>) -> Result<crate::gc::GcRef, String>>),
    Normal(Rc<dyn Fn(&mut crate::gc::GcHeap, &[crate::gc::GcRef]) -> Result<crate::gc::GcRef, String>>),
}

impl Clone for BuiltinKind {
    fn clone(&self) -> Self {
        match self {
            BuiltinKind::SpecialForm(f) => BuiltinKind::SpecialForm(Rc::clone(f)),
            BuiltinKind::Normal(f) => BuiltinKind::Normal(Rc::clone(f)),
        }
    }
}

pub fn quote_handler(_heap: &mut crate::gc::GcHeap, args: &[crate::gc::GcRef], _env: &mut std::collections::HashMap<String, BuiltinKind>) -> Result<crate::gc::GcRef, String> {
    if args.len() != 1 {
        Err("quote: expected exactly 1 argument".to_string())
    } else {
        Ok(args[0].clone())
    }
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
        // For now, we'll just write to stdout since we don't have port objects in the GC yet
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

pub fn register_all(heap: &mut crate::gc::GcHeap, env: &mut std::collections::HashMap<String, BuiltinKind>) {
    env.insert("number?".to_string(), BuiltinKind::Normal(Rc::new(predicate::number_q)));
    env.insert("help".to_string(), BuiltinKind::Normal(Rc::new(help_builtin)));
    env.insert("quote".to_string(), BuiltinKind::SpecialForm(Rc::new(quote_handler)));
    env.insert("type-of".to_string(), BuiltinKind::Normal(Rc::new(predicate::type_of)));
    env.insert("+".to_string(), BuiltinKind::Normal(Rc::new(plus_builtin)));
    env.insert("-".to_string(), BuiltinKind::Normal(Rc::new(minus_builtin)));
    env.insert("*".to_string(), BuiltinKind::Normal(Rc::new(times_builtin)));
    env.insert("/".to_string(), BuiltinKind::Normal(Rc::new(div_builtin)));
    env.insert("mod".to_string(), BuiltinKind::Normal(Rc::new(mod_builtin)));
    env.insert("display".to_string(), BuiltinKind::Normal(Rc::new(display_builtin)));
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
    use crate::io::{new_output_string_port, PortStack, FileTable, get_output_string, write_line};
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
        let mut port_stack = PortStack::new(output_port);
        let mut file_table = FileTable::new();
        
        // Write directly to the string port using our I/O functions
        assert!(write_line(&mut port_stack, &mut file_table, "hello world"));
        
        // Get the accumulated content
        let content = get_output_string(&port_stack.current());
        assert_eq!(content, "hello world");
        
        // This pattern shows how display could work with string ports:
        // 1. Create output string port
        // 2. Pass it as second argument to display
        // 3. Retrieve content for assertions
    }
} 