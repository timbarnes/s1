pub mod number;
pub mod predicate;

use crate::gc::{GcHeap, GcRef, new_primitive, new_string};
use std::collections::HashMap;
use std::rc::Rc;

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

pub fn register_all(heap: &mut crate::gc::GcHeap, env: &mut std::collections::HashMap<String, BuiltinKind>) {
    env.insert("number?".to_string(), BuiltinKind::Normal(Rc::new(predicate::number_q)));
    env.insert("help".to_string(), BuiltinKind::Normal(Rc::new(help_builtin)));
    env.insert("quote".to_string(), BuiltinKind::SpecialForm(Rc::new(quote_handler)));
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