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
    // New symbol-based bindings for the future
    symbol_bindings: HashMap<GcRefSimple, GcRefSimple>,
    parent: Option<Rc<RefCell<Frame>>>,
}

impl Frame {
    /// Create a new frame with an optional parent
    fn new(parent: Option<Rc<RefCell<Frame>>>) -> Self {
        Self {
            bindings: HashMap::new(),
            symbol_bindings: HashMap::new(),
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

    // Symbol-based methods for the new system
    /// Get a binding from this frame using a symbol key (doesn't search parent)
    fn get_local_symbol(&self, symbol: GcRefSimple) -> Option<GcRefSimple> {
        self.symbol_bindings.get(&symbol).copied()
    }

    /// Set a binding in this frame using a symbol key
    fn set_local_symbol(&mut self, symbol: GcRefSimple, value: GcRefSimple) {
        self.symbol_bindings.insert(symbol, value);
    }

    /// Check if this frame has a local binding using a symbol key
    fn has_local_symbol(&self, symbol: GcRefSimple) -> bool {
        self.symbol_bindings.contains_key(&symbol)
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

    /// Create an environment from an existing frame
    pub fn from_frame(frame: Rc<RefCell<Frame>>) -> Self {
        Self {
            current_frame: frame,
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

    /// Add a binding to the current frame using a symbol key (for builtin registration)
    pub fn add_binding_symbol(&mut self, symbol: GcRefSimple, value: GcRefSimple) {
        self.set_symbol(symbol, value);
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

    /// Set the current frame (for closure evaluation)
    pub fn set_current_frame(&mut self, frame: Rc<RefCell<Frame>>) {
        self.current_frame = frame;
    }

    // Symbol-based methods for the new system
    /// Get a binding by symbol, searching through the frame chain
    pub fn get_symbol(&self, symbol: GcRefSimple) -> Option<GcRefSimple> {
        let mut current = Some(self.current_frame.clone());
        
        while let Some(frame_rc) = current {
            let frame = frame_rc.borrow();
            if let Some(value) = frame.get_local_symbol(symbol) {
                return Some(value);
            }
            current = frame.parent.clone();
        }
        None
    }

    /// Set a binding in the current frame using a symbol key
    pub fn set_symbol(&mut self, symbol: GcRefSimple, value: GcRefSimple) {
        let mut frame = self.current_frame.borrow_mut();
        frame.set_local_symbol(symbol, value);
    }

    /// Set a binding in the global frame (root of the chain) using a symbol key
    pub fn set_global_symbol(&mut self, symbol: GcRefSimple, value: GcRefSimple) {
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
                frame.set_local_symbol(symbol, value);
                break;
            }
            current = parent.unwrap();
        }
    }

    /// Check if a binding exists in the current frame only (not parent frames) using a symbol key
    pub fn has_local_symbol(&self, symbol: GcRefSimple) -> bool {
        self.current_frame.borrow().has_local_symbol(symbol)
    }

    /// Check if a binding exists anywhere in the frame chain using a symbol key
    pub fn has_symbol(&self, symbol: GcRefSimple) -> bool {
        self.get_symbol(symbol).is_some()
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
    }

    #[test]
    fn test_symbol_based_environment() {
        let mut heap = GcHeap::new();
        
        // Create a new environment
        let mut env = Environment::new();
        
        // Create some interned symbols
        let global_sym = heap.intern_symbol("global_var");
        let local_sym = heap.intern_symbol("local_var");
        let extended_sym = heap.intern_symbol("extended_var");
        
        // Set a global binding using symbol
        let global_val = new_int_simple(&mut heap, BigInt::from(42));
        env.set_global_symbol(global_sym, global_val);
        
        // Verify we can get the global binding using symbol
        assert!(env.get_symbol(global_sym).is_some());
        
        // Set a local binding in current frame using symbol
        let local_val = new_string_simple(&mut heap, "local");
        env.set_symbol(local_sym, local_val);
        
        // Verify we can get the local binding using symbol
        assert!(env.get_symbol(local_sym).is_some());
        
        // Create an extended environment (new frame)
        let mut extended_env = env.extend();
        
        // Set a binding in the new frame using symbol
        let extended_val = new_int_simple(&mut heap, BigInt::from(99));
        extended_env.set_symbol(extended_sym, extended_val);
        
        // Verify we can get the extended binding using symbol
        assert!(extended_env.get_symbol(extended_sym).is_some());
        
        // Verify we can still get the global binding (lexical scoping)
        assert!(extended_env.get_symbol(global_sym).is_some());
        
        // Verify we can still get the local binding from parent frame
        assert!(extended_env.get_symbol(local_sym).is_some());
        
        // Verify the original environment doesn't see the extended binding
        assert!(extended_env.get_symbol(extended_sym).is_some());
        assert!(env.get_symbol(extended_sym).is_none());
        
        // Test that local bindings shadow global ones using symbols
        let shadow_val = new_string_simple(&mut heap, "shadowed");
        extended_env.set_symbol(global_sym, shadow_val);
        assert!(extended_env.get_symbol(global_sym).is_some());
        
        // Test symbol-based has methods
        assert!(env.has_symbol(global_sym));
        assert!(env.has_symbol(local_sym));
        assert!(!env.has_symbol(extended_sym));
        
        // Test that string-based and symbol-based methods work independently
        // (they use different hash maps)
        
        // Set a binding using string-based method
        let string_val = new_string_simple(&mut heap, "string_value");
        env.set("global_var".to_string(), string_val);
        
        // Verify string-based lookup works
        assert!(env.get("global_var").is_some());
        
        // Verify symbol-based lookup works (different binding)
        assert!(env.get_symbol(global_sym).is_some());
        
        // They should return different values since they're different bindings
        let string_binding = env.get("global_var").unwrap();
        let symbol_binding = env.get_symbol(global_sym).unwrap();
        assert!(!std::ptr::eq(string_binding, symbol_binding)); // Different values
    }
} 