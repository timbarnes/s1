pub mod number;
pub mod predicate;

use crate::gc::{GcHeap, GcRef, new_symbol, new_primitive};
use std::collections::HashMap;

pub fn register_all(heap: &mut GcHeap, env: &mut HashMap<String, GcRef>) {
    // Register type predicate as a test
    let is_number = new_primitive(heap, std::rc::Rc::new(predicate::is_number_predicate));
    env.insert("number?".to_string(), is_number);
    // Add more builtins here
} 