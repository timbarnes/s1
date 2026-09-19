[Home](s1-docs.md)

# Regression suite CPU profile at the default `gc-threshold` (20,000)

Measured 2026-09-18 on linux/x86_64, `--release` (`debug = true`), commit
76258c2. Baseline for comparing against future runs — re-profile after any
change to the evaluator's hot path, allocation pattern, or `gc-threshold`
default, and append a new dated section below rather than overwriting this
one.

## Why this exists

`ad59cc1` lowered the default `gc-threshold` 100,000 → 20,000 specifically so
the regression suite exercises real collections instead of GC being
invisible (see `performance.md`'s Phase 3). This profile answers "now that it
collects, what does that actually cost?"

## Method

**No `perf`/`samply` on this machine.** `perf_event_paranoid` is `2` and
raising it needs `sudo` (a persistent, system-wide setting) with no
passwordless credentials available — asked-around rather than done silently.
Used [`pprof-rs`](https://docs.rs/pprof) instead: a signal-based (`SIGPROF`)
sampler that needs no elevated privileges. Same "temporary instrumentation,
reverted after" pattern as the counting/bump allocators in `performance.md`'s
"Allocation budget" section — not committed. To redo it:

```rust
// In main(), gated by an env var so normal builds are unaffected:
let guard = std::env::var("S1_PROFILE").ok().map(|_| {
    pprof::ProfilerGuardBuilder::default().frequency(1000)
        .blocklist(&["libc", "libgcc", "pthread", "vdso"]).build().unwrap()
});
// ... run the program as normal ...
// after repl() returns, before main() exits:
if let Some(guard) = guard {
    let report = guard.report().build().unwrap();
    report.flamegraph(std::fs::File::create("flamegraph.svg").unwrap()).unwrap();
}
```
`Cargo.toml` needs `pprof = { version = "0.14", features = ["flamegraph"] }`.

**The regression suite alone is ~0.03 s** — too short to sample. Amplified by
repeating it in-process, but two pitfalls came up doing that:

1. **`load` does not block.** It's defined in Scheme (`scheme/s1-core.scm`) as
   `(push-port! (open-input-file f))` and returns immediately; the file's
   forms only get read/evaluated later, when the outer REPL loop
   (`src/main.rs::repl`) next drains the top of the port stack. Wrapping
   repeated `(load ...)` calls inside a Scheme `lambda`/loop queues all their
   ports without draining any of them in between, so everything actually
   executes later, in the wrong interleaving, once the wrapping call
   returns. This produced spurious `log`/`exp` failures that had nothing to
   do with GC. The fix (already used by `bench/bench.sh`): **flat, literal
   top-level `(load ...)` forms** in the driver file, never inside a
   function body — each one is read and fully drained by the REPL loop
   before the next is reached.

2. **`gc_stress_tests.scm` dominates if included.** It deliberately sets
   `(gc-threshold 1)` (and `500` for one test) to stress-test collection
   under pressure. Repeated in-process, that one file alone produced 5,899 GC
   cycles in a single pass and would swamp a profile with an artificial
   worst case rather than showing what the *default* threshold costs. It was
   excluded from the driver used for the numbers below; profile it
   separately if the stress-test path itself is ever the thing under study.

Driver: `test-harness.scm` + `basic_tests.scm` + `macro_tests.scm` +
`advanced_tests.scm` + `help_tests.scm`, repeated 400× as flat top-level
`(load ...)` forms, with `**print-successes**` set to `#f` right after each
`test-harness.scm` load (cuts ~2,000 `display` calls/iteration without
touching the tests themselves — failures still print). ~19 s wall,
~12,400 samples at 1 kHz.

## GC cost at the default threshold

A single `-r -q` run fires **exactly 3** real GC collections (traced via a
temporary counter in `collect_garbage`), live set stabilizing at ~20,000-23,000
objects each time. Over the 400x driver the live set plateaus around
19,500-23,500 objects — modest, bounded growth, not a runaway.

## Results — self/inclusive time, 12,398 samples

| Where | Inclusive | Self | Note |
|---|---|---|---|
| `eval_main`/`run_cek`/`step` | 96% | — | everything, as expected |
| `dispatch_kont` | 60% | 26% | core CEK step dispatch |
| `eval_cek` | 32% | 17% | |
| `handle_eval_arg` | 31% | — | |
| `apply_proc` | 26% | 4.3% | |
| `apply_special_direct` | 14% | — | |
| `Rc<RefCell<Frame>>` clone / `inc_strong` / `lookup` (env) | 8.8% each | — | refcounting + variable lookup |
| `alloc::alloc::alloc` et al. | 5.8% | 4.8% | allocation |
| **GC** (`collect_garbage`/`sweep`/`mark_reachable`) | **~5%** | 2.1% (sweep closure) | modest at this threshold |
| BigInt/BigUint `to_f64` | 5.0% each (~10% combined) | — | heavier than expected; math tests |
| `s1::utilities::debugger` | 4.4% | 4.4% | called even with tracing off |
| `core::intrinsics::unlikely` | — | 10.3% | branch-hint scaffolding, not a real callee |

**Headline: GC is real but minor (~5%) at threshold 20,000** — the
interpreter's own dispatch loop and env lookups still dominate, consistent
with the F1/F10 work already described in `performance.md`. Contrast with
`gc_stress_tests.scm`'s deliberate threshold-1 section, which pushed GC to
~91% of samples in an early (excluded) version of this profile — that's a
property of the stress test, not of ordinary use.

## Loose threads noticed, not yet acted on

* `s1::utilities::debugger` costing 4.4% self time while tracing is off
  matches a smaller version of the same finding in `performance.md`'s `fib`
  profile (2.4% there). Worth an early-return check if the no-op path isn't
  already as cheap as it can be.
* BigInt→f64 conversion (`to_f64`) is ~10% combined self+adjacent time in a
  suite that's mostly small-integer arithmetic — surprising, and it's the
  same code path as the `log`/`exp` bug below, so may be entangled with it
  rather than a separate cost.
* A reproducible correctness bug in the in-process-repeat driver itself —
  see next section — means this profile may include some samples in a
  broken code path (`log`/`exp` returning `#<undefined>` from the second
  iteration onward). Not expected to change the headline numbers materially
  since it's 2 of ~550 tests per iteration, but worth knowing.

## Artifact

`flamegraph_regression_gc20k.svg` in the repo root (matches the
`flamegraph*.svg` gitignore pattern used for prior profiling sessions — not
committed, regenerate with the recipe above).

[Home](s1-docs.md)
