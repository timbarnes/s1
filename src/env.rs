//! Environment and global binding access for the Scheme interpreter.
//!
//! This module provides:
//! - The Environment struct (for now, a flat table; will support frames)
//! - Constructors and accessors for environments
//! - Public get/set global binding helpers

use std::collections::HashMap;
use crate::gc::GcRefSimple;

/// The environment for variable bindings (flat for now; will support frames)
pub struct Environment {
    bindings: HashMap<String, GcRefSimple>,
}

impl Environment {
    /// Create a new, empty environment
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    /// Get a binding by name (if present)
    pub fn get(&self, name: &str) -> Option<GcRefSimple> {
        self.bindings.get(name).copied()
    }

    /// Set a binding (add or update)
    pub fn set(&mut self, name: String, value: GcRefSimple) {
        self.bindings.insert(name, value);
    }

    /// Get mutable access to the underlying bindings (for builtin registration)
    pub fn bindings_mut(&mut self) -> &mut HashMap<String, GcRefSimple> {
        &mut self.bindings
    }
}

/// Get a global binding from the environment by name
pub fn get_global_binding(env: &Environment, name: &str) -> Option<GcRefSimple> {
    env.get(name)
}

/// Set a global binding in the environment
pub fn set_global_binding(env: &mut Environment, name: &str, value: GcRefSimple) {
    env.set(name.to_string(), value);
} 