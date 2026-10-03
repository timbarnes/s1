# Tail-Recursive Loops Never Triggered Automatic GC

Investigated and fixed 2026-09-18, during a general review of the GC
architecture. Related to but distinct from
[nested-evaluation.md](nested-evaluation.md): that document is about GC
*safety* (collecting while state was unrooted); this one is about GC
*liveness* (collecting not happening often enough to bound memory at all).

## Summary

Automatic collection (`heap.needs_gc()`, i.e. `allocations > threshold`) was
checked in exactly one place: `handle_restore_env` in `src/eval/cek.rs`,
which only runs when a **non-tail** call returns (a `Kont::RestoreEnv` frame
is popped). Tail calls take the TCO fast path in `apply_proc`
(`Callable::Closure`'s `state.tail` branch) and reuse the existing
continuation without ever pushing `RestoreEnv`. A purely tail-recursive
Scheme loop that allocates garbage each iteration therefore never triggered
automatic collection, no matter how long it ran or how much garbage it
produced — even though its live working set could be O(1). This is a
straightforward path to unbounded memory growth in idiomatic tail-recursive
Scheme code, e.g.:

```scheme
(define (spin n) (if (= n 0) 'done (begin (cons 1 2) (spin (- n 1)))))
```

## Evidence

Using a temporary `eprintln!` in `collect_garbage` to count collections
(reverted after measuring), running `(gc-threshold 500)` then `(spin 5000)`
via `s1 -f script.scm -q` (non-interactive, so the REPL's own per-form GC
call doesn't confound the count):

| | Automatic collections during the loop |
|---|---|
| Before the fix | **1** (the one non-tail return, at the very end, back to `spin`'s caller) |
| After the fix | **20** (spread throughout the loop, as expected at threshold 500) |

Before the fix, the loop's garbage accumulated for its entire duration and
was only ever collected once it returned — for a longer or non-terminating
loop, this is unbounded growth rather than a one-cycle delay.

## Why the fix is safe

`src/eval/cek.rs`'s `take_kont` documents the actual safety invariant (added
in commit `d2796a4`): the frame currently being dispatched is *not* rooted
while its handler runs, so **collection is only safe once `state.control` /
`state.kont` / `state.env` have been fully committed to their post-transition
values.** `handle_restore_env` collects *after* its two assignments for
exactly this reason; the `(gc)` builtin is safe because it runs after
`apply_proc` has already installed its own frame.

`apply_proc`'s tail branch fully commits state the same way before returning:

```rust
if state.tail {
    state.env = new_env;
    state.kont = next;
    state.control = Control::Expr(*body);
    // <- state fully installed; safe to check needs_gc() here, same as
    //    handle_restore_env's "collect only after the restore".
    Ok(())
}
```

So the fix adds the identical guarded `if ec.heap.needs_gc() { collect_garbage(...) }`
block here, right after those three assignments — structurally the same
checkpoint as `handle_restore_env`, just placed at the symmetric point for
tail calls.

**Alternative considered and rejected:** checking `needs_gc()` unconditionally
at the top of `step()` (i.e. on every CEK transition, not just closure tail
calls) would cover this too, and was in fact the *original* implementation —
commit `337bdda` ("Moved gc call to repl and RestoreEnv to decrease frequency
of checks") deliberately moved away from it because it ran the check on every
symbol lookup, literal, and continuation dispatch, not just on calls. The
fix here restores tail-call coverage without reintroducing that frequency,
and without touching the invariant `take_kont` established afterward.

## Audit: any other unrooted Rust-local re-entrancy?

Before relying on the "collect only once state is fully committed" pattern,
re-checked for the bug class described in `nested-evaluation.md` (state held
in a Rust local across a call that could allocate/collect):

- `grep -rn "eval_main" src/` — the only callers are `main.rs`'s REPL,
  `eval_string` in `src/eval/mod.rs` (used by the REPL and `load`, both
  genuinely top-level, not re-entrant), and unit tests. No other function
  calls `eval_main` re-entrantly; the 5 sites fixed in `4498ecf` remain the
  only ones that ever did, and `src/macros.rs` is gone.
- `grep -rn "collect_garbage(" src/` — exactly 4 sites: the definition, this
  fix's new checkpoint, `handle_restore_env`, the `(gc)`/`(garbage-collect)`
  builtin, and the interactive REPL's post-eval call in `main.rs` (which
  fires after `eval_main` has already fully returned, and unconditionally
  rather than threshold-gated — a minor inefficiency, not a correctness
  issue, and out of scope here).
- Every `SysBuiltin` in `src/sys_builtins.rs` that manipulates `state.env`/
  `state.kont` (`call_cc_sp`, `escape_sp`, `dynamic_wind_sp`, `apply_sp`,
  etc.) either installs a new `Kont` frame before returning or calls a
  synchronous, non-reentrant helper (`create_callable`, `apply_sp` itself) —
  none of them loop back into `run_cek`/`step`/`eval_main` while holding
  state only in a local.
- `src/special_forms.rs`'s `create_callable`/`create_lambda_or_macro`/
  `transform_internal_defines` do pure syntactic construction (interning,
  consing, `new_closure`/`new_macro`) with no re-entrant evaluation.

No additional re-entrant-evaluation sites were found. The codebase is
consistent with `nested-evaluation.md`'s post-fix state: every
"evaluate this, then resume" is a `Kont` frame reachable from `state.kont`,
and every place `collect_garbage` is actually called runs at a point where
`CEKState` is fully self-consistent.

## Verification

- `cargo test`: 98/98 pass.
- `scheme/regression.scm`: 698/698 pass (698th is the new tail-loop test
  added to `scheme/gc_stress_tests.scm`, which runs a bounded tail-recursive
  loop at `gc-threshold 500` and asserts the correct result — an unsafe
  collection point would corrupt state or crash, not just quietly produce
  the wrong number).
- `bench/bench.sh`'s "tail loop 300k" workload (`(count 300000)` at the
  default 100,000 threshold, which now crosses the threshold ~2-3 times
  during the loop instead of never): measured directly on this machine
  (not the darwin/arm64 box `performance.md`'s baseline numbers came from,
  so only the relative before/after comparison is meaningful here), 3 runs
  each, `/usr/bin/time -p`:

  | | real |
  |---|---|
  | Before the fix | 0.57-0.58 s |
  | After the fix | 0.59-0.61 s |

  A small (~3-5%) real increase, from actually performing the 2-3 full
  mark-sweep collections this workload was previously skipping entirely —
  the expected and correct trade for bounding memory instead of leaving it
  unbounded. No other benchmarked workload is affected: none of the others
  are pure tail loops that cross the default threshold.
