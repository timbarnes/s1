//! Identifiers and aliases, the basis of hygiene (Docs/hygiene-design.md).
//!
//! `syntax-rules` expansion renames each identifier a template introduces to
//! an *alias*: a fresh uninterned symbol recorded in the heap's alias table
//! with the identifier it renames (`original`) and the macro's definition
//! environment (`env`). Because an alias is an ordinary symbol object,
//! binding forms bind it like any name; what differs is lookup. An alias is
//! first looked up as itself, which finds only bindings the expansion made,
//! and failing that its original is looked up in the definition environment.

use crate::env::{EnvOps, EnvRef};
use crate::gc::{GcHeap, GcRef, SchemeValue, new_pair, new_vector};
use crate::gc_value;
use rustc_hash::FxHashSet as HashSet;
use std::rc::Rc;

/// Where an identifier's binding was found.
pub struct Resolved {
    pub value: GcRef,
    /// The frame holding the binding
    pub frame: EnvRef,
    /// The key it is bound under in that frame: `id` itself, or the
    /// original an alias renames (or that original's original, ...)
    pub key: GcRef,
}

/// Find the binding `id` refers to in `env`, following aliases to their
/// definition environments.
pub fn resolve(heap: &GcHeap, id: GcRef, env: &EnvRef) -> Option<Resolved> {
    let mut id = id;
    let mut env = env.clone();
    loop {
        if let Some((value, frame)) = env.lookup_with_frame(id) {
            return Some(Resolved {
                value,
                frame,
                key: id,
            });
        }
        let alias = heap.alias(id)?;
        id = alias.original;
        env = alias.env.clone();
    }
}

/// The value `id` refers to in `env`: a plain lookup, falling back to alias
/// resolution only when that fails, so ordinary code pays nothing extra.
#[inline]
pub fn lookup(heap: &GcHeap, id: GcRef, env: &EnvRef) -> Option<GcRef> {
    match env.lookup(id) {
        Some(value) => Some(value),
        None if heap.alias(id).is_some() => resolve(heap, id, env).map(|r| r.value),
        None => None,
    }
}

/// The identifier an alias ultimately renames (itself if not an alias).
pub fn strip(heap: &GcHeap, id: GcRef) -> GcRef {
    let mut id = id;
    while let Some(alias) = heap.alias(id) {
        id = alias.original;
    }
    id
}

/// Whether `a` (in `env_a`) and `b` (in `env_b`) refer to the same binding,
/// or are both unbound and strip to the same identifier: R7RS's
/// `free-identifier=?`, used to match `syntax-rules` literals.
pub fn free_identifier_eq(heap: &GcHeap, a: GcRef, env_a: &EnvRef, b: GcRef, env_b: &EnvRef) -> bool {
    match (resolve(heap, a, env_a), resolve(heap, b, env_b)) {
        (Some(x), Some(y)) => Rc::ptr_eq(&x.frame, &y.frame) && x.key == y.key,
        (None, None) => strip(heap, a) == strip(heap, b),
        _ => false,
    }
}

/// `datum` with every alias in it replaced by `strip` of it: what a quoted
/// template must yield. Returns `datum` itself, allocating nothing, when it
/// contains no aliases. Cyclic data (possible only through datum labels in a
/// template) is returned unchanged.
pub fn strip_datum(heap: &mut GcHeap, datum: GcRef) -> GcRef {
    let mut seen = HashSet::default();
    let mut cyclic = false;
    if !contains_alias(heap, datum, &mut seen, &mut cyclic) || cyclic {
        return datum;
    }
    copy_stripped(heap, datum)
}

fn contains_alias(heap: &GcHeap, datum: GcRef, seen: &mut HashSet<GcRef>, cyclic: &mut bool) -> bool {
    match gc_value!(datum) {
        SchemeValue::Symbol(_) => heap.alias(datum).is_some(),
        SchemeValue::Pair(..) | SchemeValue::Vector(_) if !seen.insert(datum) => {
            *cyclic = true;
            false
        }
        SchemeValue::Pair(car, cdr) => {
            // Bitwise `|` so both sides are always walked (for `cyclic`).
            contains_alias(heap, *car, seen, cyclic) | contains_alias(heap, *cdr, seen, cyclic)
        }
        SchemeValue::Vector(items) => {
            let mut any = false;
            for item in items {
                any |= contains_alias(heap, *item, seen, cyclic);
            }
            any
        }
        _ => false,
    }
}

fn copy_stripped(heap: &mut GcHeap, datum: GcRef) -> GcRef {
    match gc_value!(datum) {
        SchemeValue::Symbol(_) => strip(heap, datum),
        SchemeValue::Pair(..) => {
            // Walk the spine iteratively so long lists don't recurse deeply.
            let mut items = Vec::new();
            let mut rest = datum;
            while let SchemeValue::Pair(car, cdr) = gc_value!(rest) {
                items.push(*car);
                rest = *cdr;
            }
            let mut result = copy_stripped(heap, rest);
            for item in items.into_iter().rev() {
                let item = copy_stripped(heap, item);
                result = new_pair(heap, item, result);
            }
            result
        }
        SchemeValue::Vector(items) => {
            let items = items.clone();
            let copied = items.into_iter().map(|i| copy_stripped(heap, i)).collect();
            new_vector(heap, copied)
        }
        _ => datum,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::{CEKState, RunTime, RunTimeStruct};
    use crate::gc::{list_from_slice, new_int};
    use crate::printer::print_value;
    use num_bigint::BigInt;

    fn int(heap: &mut GcHeap, n: i64) -> GcRef {
        new_int(heap, BigInt::from(n))
    }

    fn global() -> EnvRef {
        Rc::new(std::cell::RefCell::new(crate::env::Frame::new(None)))
    }

    #[test]
    fn alias_resolves_in_definition_env() {
        let mut ev = RunTimeStruct::new();
        let heap = &mut ev.heap;
        let g = global();
        let def_env = g.extend();
        let use_env = g.extend();
        let x = heap.intern_symbol("x");
        let outer = int(heap, 1);
        let inner = int(heap, 2);
        def_env.define(x, outer);
        use_env.define(x, inner);

        let alias = heap.make_alias(x, def_env.clone());
        // The use site's own x doesn't capture the alias...
        assert_eq!(lookup(heap, alias, &use_env), Some(outer));
        // ...but a binding of the alias itself (made by the expansion) does.
        let local = use_env.extend();
        let bound = int(heap, 3);
        local.define(alias, bound);
        assert_eq!(lookup(heap, alias, &local), Some(bound));
        // Plain identifiers are unaffected.
        assert_eq!(lookup(heap, x, &use_env), Some(inner));

        let r = resolve(heap, alias, &use_env).unwrap();
        assert!(Rc::ptr_eq(&r.frame, &def_env));
        assert_eq!(r.key, x);
    }

    #[test]
    fn alias_chains_resolve_through_each_environment() {
        let mut ev = RunTimeStruct::new();
        let heap = &mut ev.heap;
        let g = global();
        let y = heap.intern_symbol("y");
        let value = int(heap, 42);
        g.define(y, value);
        let first = heap.make_alias(y, g.clone());
        let second = heap.make_alias(first, g.clone());
        let elsewhere = g.extend();
        assert_eq!(lookup(heap, second, &elsewhere), Some(value));
        assert_eq!(strip(heap, second), y);
        // A binding under the intermediate alias is found first.
        let shadow = int(heap, 7);
        g.define(first, shadow);
        assert_eq!(lookup(heap, second, &elsewhere), Some(shadow));
    }

    #[test]
    fn free_identifier_eq_compares_bindings() {
        let mut ev = RunTimeStruct::new();
        let heap = &mut ev.heap;
        let g = global();
        let else_ = heap.intern_symbol("else");
        let renamed_else = heap.make_alias(else_, g.clone());
        let local = g.extend();
        // Both unbound, same name: equal.
        assert!(free_identifier_eq(heap, renamed_else, &g, else_, &local));
        // Bound at one site only: not equal.
        let v = int(heap, 0);
        local.define(else_, v);
        assert!(!free_identifier_eq(heap, renamed_else, &g, else_, &local));
        // Two names bound to the same frame slot: equal.
        let car = heap.intern_symbol("car");
        g.define(car, v);
        let renamed_car = heap.make_alias(car, g.clone());
        assert!(free_identifier_eq(heap, renamed_car, &local, car, &g));
    }

    #[test]
    fn strip_datum_copies_only_when_needed() {
        let mut ev = RunTimeStruct::new();
        let heap = &mut ev.heap;
        let g = global();
        let a = heap.intern_symbol("a");
        let one = int(heap, 1);
        let plain = list_from_slice(&[a, one], heap);
        assert_eq!(strip_datum(heap, plain), plain, "no aliases: returned as is");

        let renamed = heap.make_alias(a, g.clone());
        let vec = new_vector(heap, vec![renamed]);
        let tail = new_pair(heap, renamed, renamed);
        let datum = list_from_slice(&[renamed, vec, tail], heap);
        let stripped = strip_datum(heap, datum);
        assert_ne!(stripped, datum);
        assert_eq!(print_value(&stripped), "(a #(a) (a . a))");
        // Every symbol in the copy is the interned one.
        assert_eq!(crate::gc::car(stripped).unwrap(), a);
    }

    #[test]
    fn gc_keeps_an_aliased_environment_alive_while_the_alias_lives() {
        let mut ev = RunTimeStruct::new();
        ev.heap.set_poison_sweep(true);
        let g = global();
        let state = CEKState::new(g.clone());
        let mut rt = RunTime::from_eval(&mut ev);

        // `value` is reachable only through def_env, and def_env only
        // through the alias's table entry.
        let def_env = g.extend();
        let z = rt.heap.intern_symbol("z");
        let one = int(rt.heap, 1);
        let value = list_from_slice(&[one], rt.heap);
        def_env.define(z, value);
        let alias = rt.heap.make_alias(z, def_env);

        let collect = |rt: &mut RunTime, roots: &[GcRef]| {
            rt.heap.collect_garbage(&state, *rt.current_output_port, rt.port_stack, &[], roots, *rt.handlers);
        };

        // Alias rooted: the entry, its environment and the value survive.
        collect(&mut rt, &[alias]);
        assert_eq!(rt.heap.alias_count(), 1);
        assert_eq!(print_value(&value), "(1)", "value must not have been freed");
        assert_eq!(lookup(rt.heap, alias, &g), Some(value));

        // Alias unreachable: its entry is dropped.
        collect(&mut rt, &[]);
        assert_eq!(rt.heap.alias_count(), 0);
    }
}
