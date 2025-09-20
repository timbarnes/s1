#![allow(dead_code)]

use super::{GcObject, GcRef, Mark, SchemeValue};
use crate::io::PortKind;
use std::collections::HashMap;

/// The garbage-collected heap that manages all Scheme objects.
pub struct GcHeap {
    // Singleton values for SchemeValueSimple
    pub nil_obj: Option<GcRef>,
    pub true_obj: Option<GcRef>,
    pub false_obj: Option<GcRef>,
    pub tail_call_obj: Option<GcRef>,
    pub eof_obj: Option<GcRef>,
    pub undefined_obj: Option<GcRef>,
    pub void_obj: Option<GcRef>,
    // Free list for GcRef objects
    free_list: Vec<GcObject>,
    // All allocated GcRef objects (for potential future GC)
    objects: Vec<GcRef>,
    // Symbol table for interning symbols (name -> symbol object)
    symbol_table: HashMap<String, GcRef>,
}

impl GcHeap {
    /// Create a new empty heap.
    pub fn new() -> Self {
        let mut heap = Self {
            nil_obj: None,
            true_obj: None,
            false_obj: None,
            tail_call_obj: None,
            eof_obj: None,
            undefined_obj: None,
            void_obj: None,
            free_list: Vec::new(),
            objects: Vec::new(),
            symbol_table: HashMap::new(),
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

        // Allocate tail_call
        let tail_call_obj = GcObject {
            value: SchemeValue::TailCallScheduled,
            marked: false,
        };
        self.tail_call_obj = Some(self.alloc(tail_call_obj));
    }

    /// Allocate a new object on the heap.
    pub fn alloc(&mut self, obj: GcObject) -> GcRef {
        // For now, we'll use a simple approach: allocate on the heap and leak it
        // In a real implementation, you'd want proper memory management
        let boxed = Box::new(obj);
        let raw = Box::into_raw(boxed);
        self.objects.push(raw);
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
    pub fn tail_call_s(&self) -> GcRef {
        self.tail_call_obj.unwrap()
    }

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

    /// Perform garbage collection (placeholder for future implementation).
    pub fn garbage_collect<T: Mark>(&mut self, roots: &T) {
        //self.mark_from(|f| roots.mark(f));
        //self.sweep();
    }

    fn mark_from<F>(&mut self, mut mark_fn: F)
    where
        F: FnMut(GcRef),
    {
        //mark_fn(self.root);
        //self.marked = true;
    }

    fn sweep(&mut self) {
        //let mut next_free = None;
        //let mut prev = None;
        /*
                for (i, obj) in self.objects.iter_mut().enumerate() {
                    if obj.marked {
                        obj.marked = false;
                        if let Some(prev) = prev {
                            self.objects[prev].next = Some(i);
                        }
                        prev = Some(i);
                    } else {
                        if let Some(next_free) = next_free {
                            self.objects[next_free].next = Some(i);
                        }
                        next_free = Some(i);
                    }
                }

                self.free_list = next_free;
        */
    }

    pub fn needs_gc(&self) -> bool {
        self.free_list.len() < 1000
    }

    /// Update the position of a StringPortInput in a SchemeValue::Port
    pub fn update_string_port_pos(&mut self, port_ref: &PortKind, new_pos: usize) -> bool {
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
