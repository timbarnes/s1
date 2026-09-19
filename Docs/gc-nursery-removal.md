# Removing the Nursery's Blanket GC Rooting

Investigated and fixed 2026-09-18, immediately after
[gc-tail-loop.md](gc-tail-loop.md), as the next item from the same GC
architecture review.

## Summary

`GcHeap.nursery` (everything allocated since the last collection) was marked
as an **unconditional root** every collection, in `mark_from`'s "Nursery
roots" loop. This was never generational GC — `mark_from` still walked every
other root every time, so there was no reduced-scan benefit — it only
guaranteed that nothing allocated in the current epoch could be collected
even if it was already garbage by the time a collection ran, delaying
reclamation by at least one cycle.

Per `Docs/nested-evaluation.md`, this wasn't a deliberate design choice: it
was flagged retrospectively as the reason a real rooting bug (nested
`eval_main` calls parking the outer continuation/environment in Rust locals
invisible to `Mark`) was hard to reproduce for a long time — *"recently
allocated objects are accidentally protected... the bug only bites when the
outer continuation references objects that predate the previous collection,
which needs an accumulated heap."* That bug was properly fixed in `4498ecf`
by replacing the five re-entrant sites with proper `Kont` frames, but the
nursery's blanket rooting itself was never revisited afterward.

## Why removing it is safe

`src/eval/cek.rs`'s `take_kont` documents the actual safety invariant
(commit `d2796a4`): collection is only safe once `CEKState` is fully
committed to its post-transition values. Per [gc-tail-loop.md](gc-tail-loop.md)'s
audit, `collect_garbage` is called from exactly three places today —
`handle_restore_env`, `apply_proc`'s tail-call branch, and the manual `(gc)`
builtin — and all three run only after `state.control`/`kont`/`env` are
fully installed. Since collection only ever fires when everything needed is
already reachable via `CEKState::mark`, nothing should depend on "recently
allocated" as a separate rooting category. The change:

- `src/gc/heap.rs`: delete the "Nursery roots" loop in `mark_from`.
- Remove the `nursery: Vec<GcRef>` field entirely (its push in `alloc()` and
  `clear()` in `collect_garbage`), since it has no remaining reader once the
  rooting loop is gone — matching this project's practice of deleting dead
  fields outright rather than leaving them to rot (see `Docs/performance.md`
  F2 on the removed `free_list` field).

## Validation: the poisoning sweep

Reasoning alone wasn't treated as sufficient — `nested-evaluation.md`'s "Tooling
worth keeping" section describes a debug sweep mode that overwrites an
unmarked object's value with a `<<FREED>>` sentinel and leaks the box instead
of freeing it, turning a premature free into a visible marker in printed
output instead of silent corruption or a use-after-free crash. It found
three of the five original defects and was recommended to be made permanent,
but was never actually merged. Added now (`GcHeap::poison_sweep`, enabled via
`S1_GC_POISON=1`), both to validate this change and to fulfill that standing
recommendation for future use.

Ran the full `scheme/regression.scm` suite (which includes
`scheme/gc_stress_tests.scm`, the exact macro/quasiquote stress tests that
originally found defects #1-#3, plus the tail-loop stress test from the
previous fix) with `S1_GC_POISON=1` at thresholds 1, 5, 20, 50, 200, and 500
— the same low thresholds `nested-evaluation.md` used to reproduce the
original bugs (its SIGSEGV reproduced at threshold ≤20):

| Threshold | Result | `<<FREED>>` occurrences |
|---|---|---|
| 1 | 698/698 pass | 0 |
| 5 | 698/698 pass | 0 |
| 20 | 698/698 pass | 0 |
| 50 | 698/698 pass | 0 |
| 200 | 698/698 pass | 0 |
| 500 | 698/698 pass | 0 |

Also re-ran the exact historical repro from `Docs/performance.md` (which used
to `SIGSEGV`, exit 139, before `4498ecf`):

```
printf '(gc-threshold 20)\n(load "scheme/test-harness.scm")\n(load "scheme/macro_tests.scm")\n' > /tmp/mt.scm
S1_GC_POISON=1 ./target/debug/s1 -f /tmp/mt.scm -q < /dev/null
```

Exit 0, all tests pass, 0 `<<FREED>>` occurrences.

No corruption found at any tested threshold. `cargo test`: 98/98 pass.

## Performance

Same-machine before/after (this machine's absolute numbers don't match
`Docs/performance.md`'s darwin/arm64 baseline, so only relative comparison
is meaningful — see `Docs/gc-tail-loop.md`'s note on the same issue),
`/usr/bin/time -p`, 3 runs each:

- `regression 1x` (`s1 -r -q`): no clear difference — both before and after
  vary 3.6-4.9 s across 3 runs on this box, well within the run-to-run noise
  of the environment itself.
- `tail loop 300k`: ~0.38-0.41 s both before and after — statistically
  indistinguishable.

No measurable win or loss. This wasn't expected to be a performance
optimization — the nursery's marking cost was never the bottleneck this
review found (that was the missing tail-call GC checkpoint, fixed
separately) — this change is a correctness/architecture cleanup: removing
an undocumented, never-proven-necessary rooting rule now that the actual
safety invariant it was accidentally masking gaps in has its own explicit,
audited, three-checkpoint discipline.
