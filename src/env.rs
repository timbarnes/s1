//! Environment and global binding access for the Scheme interpreter.
//!
//! This module provides:
//! - The Environment struct with frame-based lexical scoping
//! - Frame creation and chaining for closures and function calls
//! - Constructors and accessors for environments
//! - Public get/set global binding helpers

use crate::gc::GcRef;
use rustc_hash::FxHashMap as HashMap;
use std::cell::{Cell, RefCell};
use std::rc::Rc;

pub type EnvRef = Rc<RefCell<Frame>>;

/// A top-level variable: the binding itself, with an identity separate from
/// its name and its value (Docs/precompilation-design.md). `define` and
/// `set!` change a cell's contents, never replace the cell, so anything
/// holding the cell sees every later assignment. That is what lets two names
/// share one variable (a library's export and an importer's import) and lets
/// pre-analysed code hold a global reference directly.
///
/// A cell can be unbound: created for a name that has no value yet (a
/// forward reference in analysed code, say), to be filled by a later
/// `define`. Lookups treat an unbound cell as no binding.
#[derive(Debug, PartialEq)]
pub struct BindingCell {
    /// The value, or null while unbound. (A null pointer rather than an
    /// `Option`, which would double the size: raw pointers have no niche.)
    value: Cell<GcRef>,
}

pub type CellRef = Rc<BindingCell>;

impl BindingCell {
    pub fn new(value: GcRef) -> CellRef {
        Rc::new(BindingCell { value: Cell::new(value) })
    }

    #[allow(dead_code)] // for libraries (phase 9) and pre-analysis
    pub fn unbound() -> CellRef {
        Self::new(std::ptr::null_mut())
    }

    pub fn get(&self) -> Option<GcRef> {
        let value = self.value.get();
        if value.is_null() { None } else { Some(value) }
    }

    pub fn set(&self, value: GcRef) {
        self.value.set(value);
    }
}

/// Variable bindings for one frame.
///
/// Non-global frames (closure calls, `let`, ...) almost always hold a
/// handful of bindings, where a linear scan over a `Vec` beats hashing —
/// and skips allocating a hash table's bucket array entirely. The global
/// frame holds hundreds of bindings (every builtin, special form, and
/// top-level `define`), where the hash map earns its keep. Frames are
/// classified once, at creation (`Frame::new`), based on whether they have
/// a parent: `extend()`-created frames are never global.
///
/// A top-level frame maps names to `BindingCell`s; a local frame holds
/// values directly, since nothing needs to share a local binding.
#[derive(Debug, PartialEq)]
pub enum Bindings {
    Small(Vec<(GcRef, GcRef)>),
    Large(HashMap<GcRef, TopBinding>),
}

/// A top-level name's binding: its cell, and whether the name was imported
/// (bound to another environment's cell by `bind_cell`). `define` of an
/// imported name gives it a fresh cell of its own, leaving the exporter's
/// variable alone, and `set!` of one is an error (Docs/libraries-design.md).
#[derive(Debug, PartialEq)]
pub struct TopBinding {
    pub cell: CellRef,
    pub imported: bool,
}

impl Bindings {
    fn get(&self, symbol: GcRef) -> Option<GcRef> {
        match self {
            Bindings::Small(v) => v.iter().find(|(k, _)| *k == symbol).map(|(_, v)| *v),
            Bindings::Large(m) => m.get(&symbol).and_then(|b| b.cell.get()),
        }
    }

    fn insert(&mut self, symbol: GcRef, val: GcRef) {
        match self {
            Bindings::Small(v) => {
                if let Some(slot) = v.iter_mut().find(|(k, _)| *k == symbol) {
                    slot.1 = val;
                } else {
                    v.push((symbol, val));
                }
            }
            Bindings::Large(m) => match m.get(&symbol) {
                Some(b) if !b.imported => b.cell.set(val),
                _ => {
                    m.insert(symbol, TopBinding { cell: BindingCell::new(val), imported: false });
                }
            },
        }
    }

    /// Iterate the bound `(symbol, value)` pairs, for GC marking and debug
    /// dumps. Unbound cells are skipped.
    pub fn iter(&self) -> Box<dyn Iterator<Item = (GcRef, GcRef)> + '_> {
        match self {
            Bindings::Small(v) => Box::new(v.iter().copied()),
            Bindings::Large(m) => Box::new(m.iter().filter_map(|(k, b)| b.cell.get().map(|v| (*k, v)))),
        }
    }
}

/// A single environment frame containing variable bindings
#[derive(Debug, PartialEq)]
pub struct Frame {
    pub bindings: Bindings,
    pub parent: Option<EnvRef>,
    /// The GC epoch (see `crate::gc::GC_EPOCH`) this frame was last visited
    /// in during marking. Since a mark walk always continues to the root,
    /// a frame at this epoch means everything above it was already walked
    /// too this cycle — `Mark for EnvRef` uses this to stop early instead
    /// of re-walking a chain shared by many closures (typically all the
    /// way to the global frame) once per closure.
    gc_mark_epoch: Cell<u64>,
    /// False for the environments `environment` makes, in which `define`
    /// and `set!` are errors.
    pub mutable: bool,
}

impl Frame {
    /// Create a new frame with an optional parent. Only a frame with no
    /// parent (the global frame) gets the hash-map representation; every
    /// frame created via `extend()` is local and gets the small-vector one.
    pub fn new(parent: Option<EnvRef>) -> Self {
        let bindings = if parent.is_none() {
            Bindings::Large(HashMap::default())
        } else {
            Bindings::Small(Vec::new())
        };
        Self {
            bindings,
            parent,
            gc_mark_epoch: Cell::new(0),
            mutable: true,
        }
    }

    /// A new, empty top-level environment.
    pub fn new_top_level(mutable: bool) -> EnvRef {
        let mut frame = Frame::new(None);
        frame.mutable = mutable;
        Rc::new(RefCell::new(frame))
    }
}

pub trait EnvOps {
    fn lookup(&self, symbol: GcRef) -> Option<GcRef>; // Search all frames
    fn lookup_local(&self, symbol: GcRef) -> Option<GcRef>; // Search only this frame
    fn lookup_with_frame(&self, symbol: GcRef) -> Option<(GcRef, EnvRef)>; // Search all frames and return the frame where the binding was found
    fn define(&self, symbol: GcRef, val: GcRef); // Define a binding in this frame
    #[allow(dead_code)] // for define-library (phase 9d) and pre-analysis
    fn cell(&self, symbol: GcRef) -> Option<CellRef>; // The cell for a top-level name, made unbound if new
    fn bind_cell(&self, symbol: GcRef, cell: CellRef); // Import: make a top-level name denote an existing cell
    fn top_level_cells(&self) -> Vec<(GcRef, CellRef)>; // A top-level frame's bound names and their cells
    fn is_imported(&self, symbol: GcRef) -> bool; // Whether this frame's binding of symbol is imported
    fn is_mutable(&self) -> bool;
    fn extend(&self) -> EnvRef; // Create a new frame with this frame as parent
    fn parent(&self) -> Option<EnvRef>;
}

impl EnvOps for EnvRef {
    /// Get a binding from this frame using a symbol key (doesn't search parent)
    fn lookup(&self, symbol: GcRef) -> Option<GcRef> {
        let mut current = Some(self.clone());
        while let Some(env) = current {
            let frame = env.borrow();
            if let Some(val) = frame.bindings.get(symbol) {
                return Some(val);
            }
            current = frame.parent.clone();
        }
        None
    }

    // Search only this frame
    fn lookup_local(&self, symbol: GcRef) -> Option<GcRef> {
        let frame = self.borrow();
        frame.bindings.get(symbol)
    }

    fn lookup_with_frame(&self, symbol: GcRef) -> Option<(GcRef, EnvRef)> {
        let mut current = Some(self.clone());
        while let Some(env) = current {
            let frame = env.borrow();
            if let Some(val) = frame.bindings.get(symbol) {
                return Some((val, env.clone()));
            }
            current = frame.parent.clone();
        }
        None
    }

    // Define a binding in this frame
    fn define(&self, symbol: GcRef, val: GcRef) {
        // Define a binding in this frame
        self.borrow_mut().bindings.insert(symbol, val);
    }

    /// The cell `symbol` denotes in this top-level frame, creating an
    /// unbound one if the name is new, so that a later `define` fills the
    /// same cell. `None` for a local frame, which has no cells.
    fn cell(&self, symbol: GcRef) -> Option<CellRef> {
        match &mut self.borrow_mut().bindings {
            Bindings::Large(m) => Some(Rc::clone(
                &m.entry(symbol)
                    .or_insert_with(|| TopBinding { cell: BindingCell::unbound(), imported: false })
                    .cell,
            )),
            Bindings::Small(_) => None,
        }
    }

    /// Make `symbol` in this top-level frame denote `cell`, replacing any
    /// binding it had, and mark it imported: how an import shares a
    /// library's variable. `cell` must belong to an environment the GC
    /// treats as a root (the system environment or a registered library's),
    /// since marking skips imported bindings.
    fn bind_cell(&self, symbol: GcRef, cell: CellRef) {
        match &mut self.borrow_mut().bindings {
            Bindings::Large(m) => {
                m.insert(symbol, TopBinding { cell, imported: true });
            }
            Bindings::Small(_) => panic!("bind_cell: not a top-level frame"),
        }
    }

    fn top_level_cells(&self) -> Vec<(GcRef, CellRef)> {
        match &self.borrow().bindings {
            Bindings::Large(m) => m
                .iter()
                .filter(|(_, b)| b.cell.get().is_some())
                .map(|(k, b)| (*k, Rc::clone(&b.cell)))
                .collect(),
            Bindings::Small(_) => Vec::new(),
        }
    }

    fn is_imported(&self, symbol: GcRef) -> bool {
        match &self.borrow().bindings {
            Bindings::Large(m) => m.get(&symbol).is_some_and(|b| b.imported),
            Bindings::Small(_) => false,
        }
    }

    fn is_mutable(&self) -> bool {
        self.borrow().mutable
    }

    // Add a new frame with this frame as parent
    fn extend(&self) -> EnvRef {
        Rc::new(RefCell::new(Frame::new(Some(Rc::clone(self)))))
    }

    // Get the parent environment (or None if we're at the global frame)
    fn parent(&self) -> Option<EnvRef> {
        let frame = self.borrow();
        frame.parent.clone()
    }
}

impl crate::gc::Mark for EnvRef {
    fn mark(&self, visit: &mut dyn FnMut(GcRef)) {
        let epoch = crate::gc::GC_EPOCH.load(std::sync::atomic::Ordering::Relaxed);
        let mut current = Some(self.clone());
        while let Some(env) = current {
            let frame = env.borrow();
            if frame.gc_mark_epoch.get() == epoch {
                // Already walked this frame (and, since this walk always
                // continues to the root, everything above it) earlier in
                // the same collection via another closure sharing this
                // chain — stop instead of re-walking it again.
                break;
            }
            frame.gc_mark_epoch.set(epoch);
            match &frame.bindings {
                Bindings::Small(v) => {
                    for (key, val) in v {
                        visit(*key);
                        visit(*val);
                    }
                }
                // Imported bindings are skipped: an imported cell always
                // belongs to an environment that is itself a root (the
                // system environment, or a registered library's), which
                // marks it. Only the import's name needs marking here.
                Bindings::Large(m) => {
                    for (key, b) in m {
                        visit(*key);
                        if !b.imported {
                            if let Some(val) = b.cell.get() {
                                visit(val);
                            }
                        }
                    }
                }
            }
            current = frame.parent.clone();
        }
    }
}
// impl crate::gc::Mark for EnvRef {
//     fn mark(&self, visit: &mut dyn FnMut(GcRef)) {
//         let mut current = Some(self.clone());
//         while let Some(env) = current {
//             let frame = env.borrow();
//             // Mark keys first
//             for &key in frame.bindings.keys() {
//                 visit(key);
//             }
//             // Mark values second
//             for &val in frame.bindings.values() {
//                 visit(val);
//             }
//             current = frame.parent.clone();
//         }
//     }
// }

// impl crate::gc::Mark for EnvRef {
//     fn mark(&self, visit: &mut dyn FnMut(GcRef)) {
//         let mut current = Some(self.clone());
//         while let Some(env) = current {
//             let frame = env.borrow();
//             for &val in frame.bindings.values() {
//                 visit(val);
//             }
//             current = frame.parent.clone();
//         }
//     }
// }

// impl crate::gc::Mark for EnvRef {
//     fn mark(&self, visit: &mut dyn FnMut(GcRef)) {
//         let frame = self.borrow();
//         for val in frame.bindings.values() {
//             visit(*val);
//         }
//         if let Some(parent) = &frame.parent {
//             parent.mark(visit);
//         }
//     }
// }

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

    #[test]
    fn test_frame_based_environment() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let ec = crate::eval::RunTime::from_eval(&mut ev);

        // Create a new environment
        let env = Rc::new(RefCell::new(Frame::new(None)));

        // Create some interned symbols
        let global_sym = ec.heap.intern_symbol("global_var");
        let local_sym = ec.heap.intern_symbol("local_var");
        let extended_sym = ec.heap.intern_symbol("extended_var");

        // Set a global binding
        let global_val = new_int(ec.heap, BigInt::from(42));
        env.define(global_sym, global_val);

        // Verify we can get the global binding
        assert!(env.lookup(global_sym).is_some());

        // Set a local binding in current frame
        let local_val = new_string(ec.heap, "local");
        env.define(local_sym, local_val);

        // Verify we can get the local binding
        assert!(env.lookup(local_sym).is_some());

        // Create an extended environment (new frame)
        let extended_env = env.extend();

        // Set a binding in the new frame
        let extended_val = new_int(ec.heap, BigInt::from(99));
        extended_env.define(extended_sym, extended_val);

        // Verify we can get the extended binding
        assert!(extended_env.lookup(extended_sym).is_some());

        // Verify we can still get the global binding (lexical scoping)
        assert!(extended_env.lookup(global_sym).is_some());

        // Verify we can still get the local binding from parent frame
        assert!(extended_env.lookup(local_sym).is_some());

        // Verify the original environment doesn't see the extended binding
        assert!(env.lookup(extended_sym).is_none());

        // Test that local bindings shadow global ones
        let shadow_val = new_string(ec.heap, "shadowed");
        extended_env.define(global_sym, shadow_val);
        assert!(extended_env.lookup(global_sym).is_some());
    }

    #[test]
    fn test_binding_cells() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let ec = crate::eval::RunTime::from_eval(&mut ev);
        let global = Rc::new(RefCell::new(Frame::new(None)));
        let other = Rc::new(RefCell::new(Frame::new(None)));
        let x = ec.heap.intern_symbol("x");
        let y = ec.heap.intern_symbol("y");
        let one = new_int(ec.heap, BigInt::from(1));
        let two = new_int(ec.heap, BigInt::from(2));

        // A cell asked for before its name is defined is unbound, and the
        // later define fills that same cell.
        let cell = global.cell(x).unwrap();
        assert_eq!(global.lookup(x), None);
        assert_eq!(global.borrow().bindings.iter().count(), 0);
        global.define(x, one);
        assert_eq!(cell.get(), Some(one));

        // Another frame's name bound to the cell shares the variable: it
        // sees the owner's later assignments.
        other.bind_cell(y, Rc::clone(&cell));
        assert!(other.is_imported(y) && !global.is_imported(x));
        assert_eq!(other.lookup(y), Some(one));
        global.define(x, two);
        assert_eq!(other.lookup(y), Some(two));

        // Defining an imported name gives it its own cell, leaving the
        // owner's variable alone.
        other.define(y, one);
        assert!(!other.is_imported(y));
        assert_eq!(other.lookup(y), Some(one));
        assert_eq!(global.lookup(x), Some(two));

        // Local frames have no cells.
        assert!(global.extend().cell(x).is_none());
    }

    #[test]
    fn test_symbol_based_environment() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let ec = crate::eval::RunTime::from_eval(&mut ev);

        // Create a new environment
        let env = Rc::new(RefCell::new(Frame::new(None)));

        // Create some interned symbols
        let global_sym = ec.heap.intern_symbol("global_var");
        let local_sym = ec.heap.intern_symbol("local_var");
        let extended_sym = ec.heap.intern_symbol("extended_var");

        // Set a global binding using symbol
        let global_val = new_int(ec.heap, BigInt::from(42));
        env.define(global_sym, global_val);

        // Verify we can get the global binding using symbol
        assert!(env.lookup(global_sym).is_some());

        // Set a local binding in current frame using symbol
        let local_val = new_string(ec.heap, "local");
        env.define(local_sym, local_val);

        // Verify we can get the local binding using symbol
        assert!(env.lookup_local(local_sym).is_some());

        // Create an extended environment (new frame)
        let extended_env = env.extend();

        // Set a binding in the new frame using symbol
        let extended_val = new_int(ec.heap, BigInt::from(99));
        extended_env.define(extended_sym, extended_val);

        // Verify we can get the extended binding using symbol
        assert!(extended_env.lookup_local(extended_sym).is_some());

        // Verify we can still get the global binding (lexical scoping)
        assert!(extended_env.lookup(global_sym).is_some());

        // Verify we can still get the local binding from parent frame
        assert!(extended_env.lookup(local_sym).is_some());

        // Verify the original environment doesn't see the extended binding
        assert!(extended_env.lookup_local(extended_sym).is_some());
        assert!(env.lookup_local(extended_sym).is_none());

        // Test that local bindings shadow global ones using symbols
        let shadow_val = new_string(ec.heap, "shadowed");
        extended_env.define(global_sym, shadow_val);
        assert!(extended_env.lookup_local(global_sym).is_some());
    }
}
