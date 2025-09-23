#![allow(dead_code)]

use super::{Callable, GcObject, GcRef, Mark, SchemeValue};
use crate::io::PortKind;
use std::collections::HashMap;

/// The garbage-collected heap that manages all Scheme objects.
pub struct GcHeap {
    // Singleton values for SchemeValueSimple
    pub nil_obj: Option<GcRef>,
    pub true_obj: Option<GcRef>,
    pub false_obj: Option<GcRef>,
    // pub tail_call_obj: Option<GcRef>,
    pub eof_obj: Option<GcRef>,
    pub undefined_obj: Option<GcRef>,
    pub void_obj: Option<GcRef>,
    // Free list for GcRef objects
    free_list: Vec<GcObject>,
    // All allocated GcRef objects (for potential future GC)
    objects: Vec<GcRef>,
    // For objects allocated since the last GC
    nursery: Vec<GcRef>,
    // Symbol table for interning symbols (name -> symbol object)
    symbol_table: HashMap<String, GcRef>,
    // Number of allocations since last GC
    allocations: usize,
    pub threshold: usize,
}

impl GcHeap {
    /// Create a new empty heap.
    pub fn new() -> Self {
        let mut heap = Self {
            nil_obj: None,
            true_obj: None,
            false_obj: None,
            // tail_call_obj: None,
            eof_obj: None,
            undefined_obj: None,
            void_obj: None,
            free_list: Vec::new(),
            objects: Vec::new(),
            nursery: Vec::new(),
            symbol_table: HashMap::new(),
            allocations: 0,
            threshold: 100000,
        };

        // Pre-allocate singleton objects
        heap.pre_allocate_simple_objects();

        heap
    }

    /// Pre-allocate singleton objects (nil, true, false) for efficiency.
    fn pre_allocate_simple_objects(&mut self) {
        // Allocate nil
        let nil_obj = GcObject {
            value: SchemeValue::Nil,
            marked: false,
        };
        self.nil_obj = Some(self.alloc(nil_obj));

        // Allocate true
        let true_obj = GcObject {
            value: SchemeValue::Bool(true),
            marked: false,
        };
        self.true_obj = Some(self.alloc(true_obj));

        // Allocate false
        let false_obj = GcObject {
            value: SchemeValue::Bool(false),
            marked: false,
        };
        self.false_obj = Some(self.alloc(false_obj));

        let void_obj = GcObject {
            value: SchemeValue::Void,
            marked: false,
        };
        self.void_obj = Some(self.alloc(void_obj));

        // Allocate true
        let eof_obj = GcObject {
            value: SchemeValue::Eof,
            marked: false,
        };
        self.eof_obj = Some(self.alloc(eof_obj));

        // Allocate false
        let undefined_obj = GcObject {
            value: SchemeValue::Undefined,
            marked: false,
        };
        self.undefined_obj = Some(self.alloc(undefined_obj));
    }

    /// Allocate a new object on the heap.
    pub fn alloc(&mut self, obj: GcObject) -> GcRef {
        self.allocations += 1;
        // For now, we'll use a simple approach: allocate on the heap and leak it
        // In a real implementation, you'd want proper memory management
        let boxed = Box::new(obj);
        let raw = Box::into_raw(boxed);
        self.objects.push(raw);
        self.nursery.push(raw);
        raw
    }

    pub fn get(&self, gcref: GcRef) -> &GcObject {
        unsafe { &*gcref }
    }

    pub fn get_mut(&mut self, gcref: GcRef) -> &mut GcObject {
        unsafe { &mut *gcref }
    }

    pub fn get_value(&self, r: GcRef) -> &SchemeValue {
        unsafe { &(*r).value }
    }

    pub fn get_value_mut(&self, r: GcRef) -> &mut SchemeValue {
        unsafe { &mut (*r).value }
    }

    /// Get a mutable reference to PortKind from a GcRef, panicking if not a port
    pub fn get_port_mut(&self, r: GcRef) -> &mut PortKind {
        match self.get_value_mut(r) {
            SchemeValue::Port(port_kind) => port_kind,
            _ => panic!("Expected Port, got different SchemeValue type"),
        }
    }


    /// Get the singleton nil value.
    pub fn nil_s(&self) -> GcRef {
        self.nil_obj.unwrap()
    }

    /// Get the singleton true value.
    pub fn true_s(&self) -> GcRef {
        self.true_obj.unwrap()
    }

    /// Get the singleton false value.
    pub fn false_s(&self) -> GcRef {
        self.false_obj.unwrap()
    }
    /// Get the singleton void value.
    pub fn void(&self) -> GcRef {
        self.void_obj.unwrap()
    }

    /// Get the singleton undefined value.
    pub fn unspecified(&self) -> GcRef {
        self.undefined_obj.unwrap()
    }

    /// Get the singleton eof value.
    pub fn eof(&self) -> GcRef {
        self.eof_obj.unwrap()
    }

    /// Get the tail call object.
    // pub fn tail_call_s(&self) -> GcRef {
    //     self.tail_call_obj.unwrap()
    // }

    /// Get statistics about the simple heap.
    pub fn simple_stats(&self) -> (usize, usize) {
        (self.objects.len(), self.symbol_table.len())
    }

    /// Check if a symbol exists in the symbol table.
    pub fn symbol_exists(&self, name: &str) -> Option<GcRef> {
        if let Some(existing) = self.symbol_table.get(name) {
            Some(*existing)
        } else {
            None
        }
    }

    /// Intern a symbol (ensure only one copy exists for each name).
    pub fn intern_symbol(&mut self, name: &str) -> GcRef {
        if let Some(existing) = self.symbol_table.get(name) {
            return *existing;
        }

        let symbol_obj = GcObject {
            value: SchemeValue::Symbol(name.to_string()),
            marked: false,
        };
        let symbol_ref = self.alloc(symbol_obj);
        self.symbol_table.insert(name.to_string(), symbol_ref);
        symbol_ref
    }

    /// Get statistics about the symbol table.
    pub fn symbol_table_stats(&self) -> usize {
        self.symbol_table.len()
    }

    /// Perform garbage collection.
    pub fn collect_garbage(&mut self, state: &crate::eval::CEKState, current_output_port: GcRef) {
        //println!("gc..");
        self.allocations = 0;
        for obj in &self.objects {
            crate::gc::unmark(*obj);
        }
        self.mark_from(state, current_output_port);
        self.sweep();
        self.nursery.clear();
    }

    fn mark_from(&mut self, state: &crate::eval::CEKState, current_output_port: GcRef) {
        let mut root_set: Vec<GcRef> = Vec::new();
        state.mark(&mut |gcref| root_set.push(gcref));

        // Add roots from the runtime
        root_set.push(current_output_port);

        // Add singleton objects to the root set
        if let Some(nil_obj) = self.nil_obj {
            root_set.push(nil_obj);
        }
        if let Some(true_obj) = self.true_obj {
            root_set.push(true_obj);
        }
        if let Some(false_obj) = self.false_obj {
            root_set.push(false_obj);
        }
        if let Some(void_obj) = self.void_obj {
            root_set.push(void_obj);
        }
        if let Some(eof_obj) = self.eof_obj {
            root_set.push(eof_obj);
        }
        if let Some(undefined_obj) = self.undefined_obj {
            root_set.push(undefined_obj);
        }
        // Add symbols from the symbol table to the root set
        for symbol in self.symbol_table.values() {
            root_set.push(*symbol);
        }
        // Add objects in the nursery to the root set
        for obj in &self.nursery {
            root_set.push(*obj);
        }
        // Now, mark all reachable objects from the collected roots
        for root in root_set {
            self.mark_reachable(root);
        }
    }

    fn mark_reachable(&mut self, start: GcRef) {
        let mut worklist = vec![start];

        while let Some(gcref) = worklist.pop() {
            if unsafe { (*gcref).marked } {
                continue;
            }

            unsafe {
                (*gcref).marked = true;
            }

            match unsafe { &(*gcref).value } {
                SchemeValue::Pair(car, cdr) => {
                    worklist.push(*car);
                    worklist.push(*cdr);
                }
                SchemeValue::Vector(vec) => {
                    for item in vec {
                        worklist.push(*item);
                    }
                }
                SchemeValue::Callable(Callable::Closure { body, env, .. }) => {
                    worklist.push(*body);
                    env.mark(&mut |gcref| worklist.push(gcref));
                }
                _ => {}
            }
        }
    }

    fn sweep(&mut self) {
        self.objects.retain(|obj| {
            let marked = unsafe { (**obj).marked };
            if !marked {
                let _ = unsafe { Box::from_raw(*obj) };
            }
            marked
        });
    }

    pub fn needs_gc(&self) -> bool {
        // Temporarily disabled for testing port_stack changes
        false
        // self.allocations > self.threshold
    }

    /// Update the position of a StringPortInput in a SchemeValue::Port
    pub fn update_string_port_pos(&mut self, port_ref: &mut PortKind, new_pos: usize) -> bool {
        crate::io::update_string_port_pos(port_ref, new_pos)
    }
}

pub struct ResultListIter {
    current: Option<GcRef>,
}

impl ResultListIter {
    pub fn new(start: GcRef) -> Self {
        Self {
            current: Some(start),
        }
    }

    pub fn next(&mut self, heap: &GcHeap) -> Result<Option<GcRef>, String> {
        let current = match self.current {
            Some(gcref) => gcref,
            None => return Ok(None),
        };

        match heap.get_value(current) {
            SchemeValue::Pair(car, cdr) => {
                self.current = Some(*cdr);
                Ok(Some(*car))
            }
            SchemeValue::Nil => {
                self.current = None;
                Ok(None)
            }
            _ => Err("Improper list in function call".to_string()),
        }
    }
}
