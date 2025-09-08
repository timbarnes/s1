//! Environment and global binding access for the Scheme interpreter.
//!
//! This module provides:
//! - The Environment struct with frame-based lexical scoping
//! - Frame creation and chaining for closures and function calls
//! - Constructors and accessors for environments
//! - Public get/set global binding helpers

use crate::gc::GcRef;
use crate::printer::print_value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type EnvRef = Rc<RefCell<Frame>>;

/// A single environment frame containing variable bindings
#[derive(Debug)]
pub struct Frame {
    pub bindings: HashMap<GcRef, GcRef>,
    pub parent: Option<EnvRef>,
}

impl Frame {
    /// Create a new frame with an optional parent
    pub fn new(parent: Option<EnvRef>) -> Self {
        Self {
            bindings: HashMap::new(),
            parent,
        }
    }
}

pub trait EnvOps {
    fn lookup(&self, symbol: GcRef) -> Option<GcRef>; // Search all frames
    fn lookup_local(&self, symbol: GcRef) -> Option<GcRef>; // Search only this frame
    fn lookup_with_frame(&self, symbol: GcRef) -> Option<(GcRef, EnvRef)>; // Search all frames and return the frame where the binding was found
    fn define(&self, symbol: GcRef, val: GcRef); // Define a binding in this frame
    fn set(&self, symbol: GcRef, val: GcRef) -> Result<(), String>; // Set in any frame where defined
    fn set_local(&self, symbol: GcRef, val: GcRef) -> Result<(), String>; // Set in this frame only
    fn extend(&self) -> EnvRef; // Create a new frame with this frame as parent
}

impl EnvOps for EnvRef {
    /// Get a binding from this frame using a symbol key (doesn't search parent)
    fn lookup(&self, symbol: GcRef) -> Option<GcRef> {
        let frame = self.borrow();
        if let Some(val) = frame.bindings.get(&symbol) {
            Some(*val)
        } else if let Some(ref parent) = frame.parent {
            parent.lookup(symbol)
        } else {
            None
        }
    }

    // Search only this frame
    fn lookup_local(&self, symbol: GcRef) -> Option<GcRef> {
        let frame = self.borrow();
        if let Some(val) = frame.bindings.get(&symbol) {
            Some(*val)
        } else {
            None
        }
    }

    fn lookup_with_frame(&self, symbol: GcRef) -> Option<(GcRef, EnvRef)> {
        let frame = self.borrow();
        if let Some(val) = frame.bindings.get(&symbol) {
            Some((*val, Rc::clone(self)))
        } else if let Some(ref parent) = frame.parent {
            parent.lookup_with_frame(symbol)
        } else {
            None
        }
    }

    // Define a binding in this frame
    fn define(&self, symbol: GcRef, val: GcRef) {
        // Define a binding in this frame
        self.borrow_mut().bindings.insert(symbol, val);
    }

    // Set a binding in this frame using a symbol key
    fn set(&self, symbol: GcRef, val: GcRef) -> Result<(), String> {
        let mut frame = self.borrow_mut();
        if frame.bindings.contains_key(&symbol) {
            frame.bindings.insert(symbol.clone(), val);
            Ok(())
        } else if let Some(ref parent) = frame.parent {
            //drop(frame); // release borrow before recursive call
            parent.set(symbol, val)
        } else {
            Err(format!("Unbound variable: {}", print_value(&symbol)))
        }
    }

    fn set_local(&self, symbol: GcRef, val: GcRef) -> Result<(), String> {
        let mut frame = self.borrow_mut();
        if frame.bindings.contains_key(&symbol) {
            frame.bindings.insert(symbol.clone(), val);
            Ok(())
        } else {
            Err(format!("Unbound variable: {}", print_value(&symbol)))
        }
    }

    // Add a new frame with this frame as parent
    fn extend(&self) -> EnvRef {
        Rc::new(RefCell::new(Frame::new(Some(Rc::clone(self)))))
    }
}

// /// Set a binding in this frame using a symbol key
// pub fn set_local(&mut self, symbol: GcRef, value: GcRef) {
//     self.bindings.insert(symbol, value);
// }

/// Check if this frame has a local binding using a symbol key
//     pub fn has_local(&self, symbol: GcRef) -> bool {
//         self.bindings.contains_key(&symbol)
//     }
// }

/// The environment for variable bindings with frame-based lexical scoping
// pub struct Environment {
//     pub current_frame: Rc<RefCell<Frame>>,
// }

// impl Environment {
//     /// Create a new environment with a single global frame
//     pub fn new() -> Self {
//         Self {
//             current_frame: Rc::new(RefCell::new(Frame::new(None))),
//         }
//     }

//     /// Create an environment from an existing frame
//     pub fn from_frame(frame: Rc<RefCell<Frame>>) -> Self {
//         Self {
//             current_frame: frame,
//         }
//     }

//     /// Create a new frame extending the current environment
//     /// Returns a new Environment with the new frame as current
//     pub fn extend(&self) -> Self {
//         Self {
//             current_frame: Rc::new(RefCell::new(Frame::new(Some(self.current_frame.clone())))),
//         }
//     }

/// Add a binding to the current frame using a symbol key (for builtin registration)
// pub fn add_binding(&mut self, symbol: GcRef, value: GcRef) {
//     self.set_symbol(symbol, value);
// }

/// Check if a binding exists in the current frame only (not parent frames)
// pub fn has_local(&self, symbol: GcRef) -> bool {
//     self.current_frame.borrow().has_local(symbol)
// }

/// Check if a binding exists anywhere in the frame chain
// pub fn has(&self, symbol: GcRef) -> bool {
//     self.get_symbol(symbol).is_some()
// }

/// Get the current frame (for closure creation)
// pub fn current_frame(&self) -> Rc<RefCell<Frame>> {
//     self.current_frame.clone()
// }

// /// Set the current frame (for closure evaluation)
// pub fn set_current_frame(&mut self, frame: Rc<RefCell<Frame>>) {
//     self.current_frame = frame;
// }

// /// Get a binding by symbol, searching through the frame chain
// pub fn get_symbol(&self, symbol: GcRef) -> Option<GcRef> {
//     let mut current = Some(self.current_frame.clone());

//     while let Some(frame_rc) = current {
//         let frame = frame_rc.borrow();
//         if let Some(value) = frame.get_local(symbol) {
//             return Some(value);
//         }
//         current = frame.parent.clone();
//     }
//     None
// }

// /// Retrieve a binding by symbol, returning its value and the frame it was found in
// pub fn get_symbol_and_frame(&self, symbol: GcRef) -> Option<(GcRef, Rc<RefCell<Frame>>)> {
//     let mut current = Some(self.current_frame.clone());

//     while let Some(frame_rc) = current {
//         let frame = frame_rc.borrow();
//         if let Some(value) = frame.get_local(symbol) {
//             return Some((value, frame_rc.clone())); // clone Rc to return
//         }
//         current = frame.parent.clone();
//     }
//     None
// }

// /// Set a binding in the current frame using a symbol key
// pub fn set_symbol(&mut self, symbol: GcRef, value: GcRef) {
//     let mut frame = self.current_frame.borrow_mut();
//     frame.set_local(symbol, value);
// }

/// Set a binding in the global frame (root of the chain) using a symbol key
// pub fn set_global_symbol(&mut self, symbol: GcRef, value: GcRef) {
//     let mut current = self.current_frame.clone();

//     // Find the root frame (the one with no parent)
//     loop {
//         let parent = {
//             let frame = current.borrow();
//             frame.parent.clone()
//         };

//         if parent.is_none() {
//             // This is the global frame
//             let mut frame = current.borrow_mut();
//             frame.set_local(symbol, value);
//             break;
//         }
//         current = parent.unwrap();
//     }
// }

/// Check if a binding exists in the current frame only (not parent frames) using a symbol key
// pub fn has_local_symbol(&self, symbol: GcRef) -> bool {
//     self.current_frame.borrow().has_local(symbol)
// }

/// Check if a binding exists anywhere in the frame chain using a symbol key
//     pub fn has_symbol(&self, symbol: GcRef) -> bool {
//         self.get_symbol(symbol).is_some()
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gc::{new_int, new_string};
    use num_bigint::BigInt;

    // #[test]
    // fn test_frame_based_environment() {
    //     let mut ev = crate::eval::Evaluator::new();
    //     let ec = crate::eval::EvalContext::from_eval(&mut ev);

    //     // Create a new environment
    //     let mut env = Environment::new();

    //     // Create some interned symbols
    //     let global_sym = ec.heap.intern_symbol("global_var");
    //     let local_sym = ec.heap.intern_symbol("local_var");
    //     let extended_sym = ec.heap.intern_symbol("extended_var");

    //     // Set a global binding
    //     let global_val = new_int(ec.heap, BigInt::from(42));
    //     env.set_global_symbol(global_sym, global_val);

    //     // Verify we can get the global binding
    //     assert!(env.get_symbol(global_sym).is_some());

    //     // Set a local binding in current frame
    //     let local_val = new_string(ec.heap, "local");
    //     env.set_symbol(local_sym, local_val);

    //     // Verify we can get the local binding
    //     assert!(env.get_symbol(local_sym).is_some());

    //     // Create an extended environment (new frame)
    //     let mut extended_env = env.extend();

    //     // Set a binding in the new frame
    //     let extended_val = new_int(ec.heap, BigInt::from(99));
    //     extended_env.set_symbol(extended_sym, extended_val);

    //     // Verify we can get the extended binding
    //     assert!(extended_env.get_symbol(extended_sym).is_some());

    //     // Verify we can still get the global binding (lexical scoping)
    //     assert!(extended_env.get_symbol(global_sym).is_some());

    //     // Verify we can still get the local binding from parent frame
    //     assert!(extended_env.get_symbol(local_sym).is_some());

    //     // Verify the original environment doesn't see the extended binding
    //     assert!(env.get_symbol(extended_sym).is_none());

    //     // Test that local bindings shadow global ones
    //     let shadow_val = new_string(ec.heap, "shadowed");
    //     extended_env.set_symbol(global_sym, shadow_val);
    //     assert!(extended_env.get_symbol(global_sym).is_some());
    // }

    // #[test]
    // fn test_symbol_based_environment() {
    //     let mut ev = crate::eval::Evaluator::new();
    //     let ec = crate::eval::EvalContext::from_eval(&mut ev);

    //     // Create a new environment
    //     let mut env = Environment::new();

    //     // Create some interned symbols
    //     let global_sym = ec.heap.intern_symbol("global_var");
    //     let local_sym = ec.heap.intern_symbol("local_var");
    //     let extended_sym = ec.heap.intern_symbol("extended_var");

    //     // Set a global binding using symbol
    //     let global_val = new_int(ec.heap, BigInt::from(42));
    //     env.set_global_symbol(global_sym, global_val);

    //     // Verify we can get the global binding using symbol
    //     assert!(env.get_symbol(global_sym).is_some());

    //     // Set a local binding in current frame using symbol
    //     let local_val = new_string(ec.heap, "local");
    //     env.set_symbol(local_sym, local_val);

    //     // Verify we can get the local binding using symbol
    //     assert!(env.get_symbol(local_sym).is_some());

    //     // Create an extended environment (new frame)
    //     let mut extended_env = env.extend();

    //     // Set a binding in the new frame using symbol
    //     let extended_val = new_int(ec.heap, BigInt::from(99));
    //     extended_env.set_symbol(extended_sym, extended_val);

    //     // Verify we can get the extended binding using symbol
    //     assert!(extended_env.get_symbol(extended_sym).is_some());

    //     // Verify we can still get the global binding (lexical scoping)
    //     assert!(extended_env.get_symbol(global_sym).is_some());

    //     // Verify we can still get the local binding from parent frame
    //     assert!(extended_env.get_symbol(local_sym).is_some());

    //     // Verify the original environment doesn't see the extended binding
    //     assert!(extended_env.get_symbol(extended_sym).is_some());
    //     assert!(env.get_symbol(extended_sym).is_none());

    //     // Test that local bindings shadow global ones using symbols
    //     let shadow_val = new_string(ec.heap, "shadowed");
    //     extended_env.set_symbol(global_sym, shadow_val);
    //     assert!(extended_env.get_symbol(global_sym).is_some());

    //     // Test symbol-based has methods
    //     assert!(env.has_symbol(global_sym));
    //     assert!(env.has_symbol(local_sym));
    //     assert!(!env.has_symbol(extended_sym));
    // }
}
