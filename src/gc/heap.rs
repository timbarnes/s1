#![allow(dead_code)]

use super::{Callable, GcObject, GcRef, Mark, SchemeValue};
use crate::eval::DynamicWind;
use crate::io::PortKind;
use rustc_hash::FxHashMap as HashMap;

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
    // All allocated GcRef objects (for potential future GC)
    objects: Vec<GcRef>,
    // Reusable worklist for marking objects during GC
    worklist: Vec<GcRef>,
    // Symbol table for interning symbols (name -> symbol object)
    symbol_table: HashMap<String, GcRef>,
    // Documentation attached to symbols via add-doc, independent of what
    // (if anything) the symbol is currently bound to.
    doc_table: HashMap<GcRef, String>,
    // Number of allocations since last GC
    allocations: usize,
    pub threshold: usize,
    // Bumped at the start of every collection; an object is "marked" for
    // the current cycle when its own `marked` field equals this. Avoids a
    // full-heap pass to reset every object's mark bit before each GC (F7)
    // — see `collect_garbage`.
    current_epoch: u64,
    // When set (via the S1_GC_POISON env var), sweep() overwrites an
    // unmarked object's value with a "<<FREED>>" sentinel and leaks the box
    // instead of freeing it, turning a premature free into a visible marker
    // in printed output instead of silent corruption or a use-after-free.
    // See Docs/nested-evaluation.md's "Tooling worth keeping".
    poison_sweep: bool,
}

impl GcHeap {
    /// Create a new empty heap.
    pub fn new() -> Self {
        // Was 100,000: the shipped regression suite allocates ~55,848
        // objects, so GC never fired during a normal `-r` run and any bug or
        // inefficiency in collection itself stayed invisible (see
        // Docs/performance.md, Docs/nested-evaluation.md staging item 4).
        // Lowered below that so a standard regression run exercises a
        // handful of ordinary collections without resorting to the
        // artificially extreme thresholds (1, 500) already used for
        // targeted stress tests in scheme/gc_stress_tests.scm.
        let gc_threshold = 20000;
        let mut heap = Self {
            nil_obj: None,
            true_obj: None,
            false_obj: None,
            // tail_call_obj: None,
            eof_obj: None,
            undefined_obj: None,
            void_obj: None,
            objects: Vec::new(),
            worklist: Vec::with_capacity(gc_threshold + 1000),
            symbol_table: HashMap::default(),
            doc_table: HashMap::default(),
            allocations: 0,
            threshold: gc_threshold,
            current_epoch: 0,
            poison_sweep: std::env::var("S1_GC_POISON").is_ok(),
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
            marked: 0,
        };
        self.nil_obj = Some(self.alloc(nil_obj));

        // Allocate true
        let true_obj = GcObject {
            value: SchemeValue::Bool(true),
            marked: 0,
        };
        self.true_obj = Some(self.alloc(true_obj));

        // Allocate false
        let false_obj = GcObject {
            value: SchemeValue::Bool(false),
            marked: 0,
        };
        self.false_obj = Some(self.alloc(false_obj));

        let void_obj = GcObject {
            value: SchemeValue::Void,
            marked: 0,
        };
        self.void_obj = Some(self.alloc(void_obj));

        // Allocate true
        let eof_obj = GcObject {
            value: SchemeValue::Eof,
            marked: 0,
        };
        self.eof_obj = Some(self.alloc(eof_obj));

        // Allocate false
        let undefined_obj = GcObject {
            value: SchemeValue::Undefined,
            marked: 0,
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
            marked: 0,
        };
        let symbol_ref = self.alloc(symbol_obj);
        self.symbol_table.insert(name.to_string(), symbol_ref);
        symbol_ref
    }

    /// Get statistics about the symbol table.
    pub fn symbol_table_stats(&self) -> usize {
        self.symbol_table.len()
    }

    /// Attach or replace the documentation for a symbol, independent of
    /// whatever value (if any) it's currently bound to.
    pub fn set_doc(&mut self, symbol: GcRef, doc: String) {
        self.doc_table.insert(symbol, doc);
    }

    /// Look up documentation attached to a symbol via `set_doc`/`add-doc`.
    pub fn get_doc(&self, symbol: GcRef) -> Option<&String> {
        self.doc_table.get(&symbol)
    }

    /// Perform garbage collection.
    pub fn collect_garbage(
        &mut self,
        state: &crate::eval::CEKState,
        current_output_port: GcRef,
        port_stack: &[GcRef],
        dynamic_wind: &[DynamicWind],
        arg_stack: &[GcRef],
    ) {
        // println!("GC: Starting collection, {} objects, {} ports in stack",
        //          self.objects.len(), port_stack.len());
        self.worklist.clear();
        self.worklist.reserve(self.threshold + 1000);
        self.allocations = 0;
        // F7: bumping the epoch makes every object implicitly "unmarked"
        // for this cycle (its `marked` field, from a prior cycle or 0 if
        // never marked, can no longer equal `current_epoch`) — no need to
        // walk `self.objects` resetting a bool on each one first.
        self.current_epoch += 1;
        // Mirror it for env::Frame's own visited-this-epoch tracking (F6);
        // see `crate::gc::GC_EPOCH`.
        crate::gc::GC_EPOCH.store(self.current_epoch, std::sync::atomic::Ordering::Relaxed);
        self.mark_from(state, current_output_port, port_stack, dynamic_wind, arg_stack);
        self.sweep();
    }

    fn mark_from(
        &mut self,
        state: &crate::eval::CEKState,
        current_output_port: GcRef,
        port_stack: &[GcRef],
        dynamic_wind: &[DynamicWind],
        arg_stack: &[GcRef],
    ) {
        // Copied out so the marking closures below don't need to borrow
        // `self` (they already borrow `self.worklist` mutably).
        let epoch = self.current_epoch;

        // CEKState roots
        state.mark(&mut |gcref| mark_reachable(gcref, epoch, &mut self.worklist));

        // Runtime roots
        mark_reachable(current_output_port, epoch, &mut self.worklist);
        for port in port_stack {
            mark_reachable(*port, epoch, &mut self.worklist);
        }

        for dw in dynamic_wind {
            mark_reachable(dw.before, epoch, &mut self.worklist);
            mark_reachable(dw.after, epoch, &mut self.worklist);
        }

        // Evaluated-argument scratch stack: every in-flight `EvalArg`'s
        // already-evaluated arguments live here rather than in the frame
        // itself, so the whole stack is a root (not just the top frame's
        // slice — outer calls with pending sibling arguments still have
        // live entries below the innermost call's `args_base`).
        for arg in arg_stack {
            mark_reachable(*arg, epoch, &mut self.worklist);
        }

        // Singleton objects
        for &obj in [
            self.nil_obj,
            self.true_obj,
            self.false_obj,
            self.void_obj,
            self.eof_obj,
            self.undefined_obj,
        ]
        .iter()
        .flatten()
        {
            mark_reachable(obj, epoch, &mut self.worklist);
        }

        // Symbol table roots
        for &sym in self.symbol_table.values() {
            mark_reachable(sym, epoch, &mut self.worklist);
        }
    }

    fn sweep(&mut self) {
        let poison = self.poison_sweep;
        let epoch = self.current_epoch;
        self.objects.retain(|obj| {
            let marked = unsafe { (**obj).marked } == epoch;
            if !marked {
                if poison {
                    // Leak the box, but overwrite its value so any stray
                    // reference to it reads a visible sentinel instead of
                    // silently-corrupted or freed memory.
                    unsafe {
                        (**obj).value = SchemeValue::Symbol("<<FREED>>".to_string());
                    }
                } else {
                    let _ = unsafe { Box::from_raw(*obj) };
                }
            }
            marked
        });
    }

    pub fn needs_gc(&self) -> bool {
        self.allocations > self.threshold
    }

    /// Update the position of a StringPortInput in a SchemeValue::Port
    pub fn update_string_port_pos(&mut self, port_ref: &mut PortKind, new_pos: usize) -> bool {
        crate::io::update_string_port_pos(port_ref, new_pos)
    }
}

/// Mark-on-push: an object is marked the instant it's queued, not when it's
/// popped, so any other path that reaches it while it's still sitting in the
/// worklist sees it's already marked and doesn't queue it again. (Marking
/// only on pop, as before, meant an object reachable by k paths was pushed —
/// and popped — k times instead of once, since nothing stopped the earlier
/// k-1 duplicates from being queued before the first one was processed.)
#[inline]
fn push_if_unmarked(gcref: GcRef, epoch: u64, worklist: &mut Vec<GcRef>) {
    if *crate::gc_marked!(gcref) == epoch {
        return;
    }
    *crate::gc_marked_mut!(gcref) = epoch;
    worklist.push(gcref);
}

fn mark_reachable(start: GcRef, epoch: u64, worklist: &mut Vec<GcRef>) {
    push_if_unmarked(start, epoch, worklist);

    while let Some(gcref) = worklist.pop() {
        match crate::gc_value!(gcref) {
            SchemeValue::Pair(car, cdr) => {
                push_if_unmarked(*car, epoch, worklist);
                push_if_unmarked(*cdr, epoch, worklist);
            }
            SchemeValue::Vector(vec) => {
                for item in vec {
                    push_if_unmarked(*item, epoch, worklist);
                }
            }
            SchemeValue::Callable(Callable::Closure { body, env, .. }) => {
                push_if_unmarked(*body, epoch, worklist);
                env.mark(&mut |gcref| push_if_unmarked(gcref, epoch, worklist));
            }
            SchemeValue::Callable(Callable::Macro { body, env, .. }) => {
                push_if_unmarked(*body, epoch, worklist);
                env.mark(&mut |gcref| push_if_unmarked(gcref, epoch, worklist));
            }
            SchemeValue::Continuation(kont, dw_stack) => {
                kont.mark(&mut |gcref| push_if_unmarked(gcref, epoch, worklist));
                for dw in dw_stack {
                    push_if_unmarked(dw.before, epoch, worklist);
                    push_if_unmarked(dw.after, epoch, worklist);
                }
            }
            _ => {}
        }
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
