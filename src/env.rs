//! Environment and global binding access for the Scheme interpreter.
//!
//! This module provides:
//! - The Environment struct with frame-based lexical scoping
//! - Frame creation and chaining for closures and function calls
//! - Constructors and accessors for environments
//! - Public get/set global binding helpers

use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use crate::gc::GcRefSimple;

/// A single environment frame containing variable bindings
#[derive(Debug)]
pub struct Frame {
    bindings: HashMap<String, GcRefSimple>,
    parent: Option<Rc<RefCell<Frame>>>,
}

impl Frame {
    /// Create a new frame with an optional parent
    fn new(parent: Option<Rc<RefCell<Frame>>>) -> Self {
        Self {
            bindings: HashMap::new(),
            parent,
        }
    }

    /// Get a binding from this frame (doesn't search parent)
    fn get_local(&self, name: &str) -> Option<GcRefSimple> {
        self.bindings.get(name).copied()
    }

    /// Set a binding in this frame
    fn set_local(&mut self, name: String, value: GcRefSimple) {
        self.bindings.insert(name, value);
    }

    /// Check if this frame has a local binding
    fn has_local(&self, name: &str) -> bool {
        self.bindings.contains_key(name)
    }
}

/// The environment for variable bindings with frame-based lexical scoping
pub struct Environment {
    current_frame: Rc<RefCell<Frame>>,
}

impl Environment {
    /// Create a new environment with a single global frame
    pub fn new() -> Self {
        Self {
            current_frame: Rc::new(RefCell::new(Frame::new(None))),
        }
    }

    /// Get a binding by name, searching through the frame chain
    pub fn get(&self, name: &str) -> Option<GcRefSimple> {
        let mut current = Some(self.current_frame.clone());
        
        while let Some(frame_rc) = current {
            let frame = frame_rc.borrow();
            if let Some(value) = frame.get_local(name) {
                return Some(value);
            }
            current = frame.parent.clone();
        }
        None
    }

    /// Set a binding in the current frame
    pub fn set(&mut self, name: String, value: GcRefSimple) {
        let mut frame = self.current_frame.borrow_mut();
        frame.set_local(name, value);
    }

    /// Set a binding in the global frame (root of the chain)
    pub fn set_global(&mut self, name: String, value: GcRefSimple) {
        let mut current = self.current_frame.clone();
        
        // Find the root frame (the one with no parent)
        loop {
            let parent = {
                let frame = current.borrow();
                frame.parent.clone()
            };
            
            if parent.is_none() {
                // This is the global frame
                let mut frame = current.borrow_mut();
                frame.set_local(name, value);
                break;
            }
            current = parent.unwrap();
        }
    }

    /// Create a new frame extending the current environment
    /// Returns a new Environment with the new frame as current
    pub fn extend(&self) -> Self {
        Self {
            current_frame: Rc::new(RefCell::new(Frame::new(Some(self.current_frame.clone())))),
        }
    }

    /// Add a binding to the current frame (for builtin registration)
    pub fn add_binding(&mut self, name: String, value: GcRefSimple) {
        self.set(name, value);
    }

    /// Check if a binding exists in the current frame only (not parent frames)
    pub fn has_local(&self, name: &str) -> bool {
        self.current_frame.borrow().has_local(name)
    }

    /// Check if a binding exists anywhere in the frame chain
    pub fn has(&self, name: &str) -> bool {
        self.get(name).is_some()
    }

    /// Get the current frame (for closure creation)
    pub fn current_frame(&self) -> Rc<RefCell<Frame>> {
        self.current_frame.clone()
    }
}

/// Get a global binding from the environment by name
pub fn get_global_binding(env: &Environment, name: &str) -> Option<GcRefSimple> {
    env.get(name)
}

/// Set a global binding in the environment
pub fn set_global_binding(env: &mut Environment, name: &str, value: GcRefSimple) {
    env.set_global(name.to_string(), value);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gc::{GcHeap, new_int_simple, new_string_simple};
    use num_bigint::BigInt;

    #[test]
    fn test_frame_based_environment() {
        let mut heap = GcHeap::new();
        
        // Create a new environment
        let mut env = Environment::new();
        
        // Set a global binding
        let global_val = new_int_simple(&mut heap, BigInt::from(42));
        env.set_global("global_var".to_string(), global_val);
        
        // Verify we can get the global binding
        assert!(env.get("global_var").is_some());
        
        // Set a local binding in current frame
        let local_val = new_string_simple(&mut heap, "local");
        env.set("local_var".to_string(), local_val);
        
        // Verify we can get the local binding
        assert!(env.get("local_var").is_some());
        
        // Create an extended environment (new frame)
        let mut extended_env = env.extend();
        
        // Set a binding in the new frame
        let extended_val = new_int_simple(&mut heap, BigInt::from(99));
        extended_env.set("extended_var".to_string(), extended_val);
        
        // Verify we can get the extended binding
        assert!(extended_env.get("extended_var").is_some());
        
        // Verify we can still get the global binding (lexical scoping)
        assert!(extended_env.get("global_var").is_some());
        
        // Verify we can still get the local binding from parent frame
        assert!(extended_env.get("local_var").is_some());
        
        // Verify the original environment doesn't see the extended binding
        assert!(env.get("extended_var").is_none());
        
        // Test that local bindings shadow global ones
        let shadow_val = new_string_simple(&mut heap, "shadowed");
        extended_env.set("global_var".to_string(), shadow_val);
        assert!(extended_env.get("global_var").is_some());
        
        // But the original environment still sees the original global
        assert!(env.get("global_var").is_some());
    }
} 