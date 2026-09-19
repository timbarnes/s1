# s1 Performance Baseline and Improvement Plan

Measured 2026-09-15 on darwin/arm64, `--release` (`debug = true`).
Re-run the numbers with `bench/bench.sh [reps]`.

**Status, 2026-09-18: Phases 1-3 all done.** Everything below is done except
F3 (skipped by decision) and F10(3) (deferred by decision, full design in
`Docs/kont-flat-stack-design.md`) — see "Suggested order" at the end for the
final per-item status table.

## Results

| Workload | Baseline | Phase 1 | + F10(1) | total |
|---|---|---|---|---|
| `regression 1x` (`s1 -r -q`) | 0.04 s | 0.04 s | 0.03 s | process-start bound |
| `regression 5x` in-process | 0.37 s | 0.21 s | 0.16 s | **2.3×** |
| `regression 20x` in-process | 5.33 s | 0.82 s | **0.65 s** | **8.2×**, now linear |
| `fib 25` (call + arith) | 0.32 s | 0.31 s | **0.24 s** | **1.3×** |
| `list/map` 300 × 200 elems | 0.59 s | 0.59 s | **0.50 s** | **1.2×** |
| `tail loop 300k` | 0.75 s | 0.30 s | **0.24 s** | **3.1×** |

Phase 1 = F1, F4, F8, dead `free_list` (commit 7db8e1e).
F10(1) = consume continuation frames instead of cloning (commit d2796a4).

Test count went 484 → 528: two tests previously aborted their top-level form
and never ran (F1), one was swallowed by a stray paren, and 42 bignum tests
were added (commit dbc6fa7, which also fixed exact comparison, `expt`, and
`even?`/`odd?`).

## OPEN: memory-safety bug under GC pressure

> **Superseded by [nested-evaluation.md](nested-evaluation.md)** (2026-09-16).
> The hypothesis below was confirmed, but it is one of five defects in the
> nested-`eval_main` mechanism, and the save-stack fix suggested here covers
> only two of them. Read that document before acting on this section.


**The interpreter segfaults if the GC actually runs often.** The default
threshold is 100,000 allocations and the regression suite allocates 55,848, so
GC never fires in normal use and this is invisible — but it is a live
use-after-free, not a theoretical one.

Reproduce:

```
printf '(gc-threshold 20)\n(load "scheme/test-harness.scm")\n(load "scheme/macro_tests.scm")\n' > /tmp/mt.scm
./target/release/s1 -f /tmp/mt.scm -q < /dev/null   # exit 139 (SIGSEGV)
```

It crashes right after "my-or returns first truthy", i.e. during macro
expansion. Thresholds ≥50 pass. This is **pre-existing** — it reproduces
identically at threshold ≤10 on commit 7db8e1e, before the dispatch_kont work.
That work shifted the triggering threshold from ≤10 to ≤20 by changing the
allocation pattern, not by adding a hazard (rooting the continuation chain in
`take_kont` made no difference to it).

Likely cause, from reading rather than a minimal repro: `eval_macro`
(`src/eval/mod.rs:153-171`) runs a *nested* `eval_main`, which overwrites
`state.kont` with `Kont::Halt`. The outer continuation and environment survive
only in Rust locals (`saved_kont`, `saved_env`), and `Mark for CEKState` marks
`state.kont` — so the whole outer continuation is unreachable from the GC roots
for the duration of the expansion. A collection during macro expansion sweeps
it, and restoring `state.kont = saved_kont` then yields dangling pointers.

Why it is hard to reproduce minimally: `GcHeap.nursery` — everything allocated
since the last collection — is itself part of the root set, so recently
allocated objects are accidentally protected. The bug only bites when the outer
continuation references objects that predate the previous collection, which
needs an accumulated heap.

This is the same class as the `take_kont` invariant: **saved interpreter state
held in Rust locals is not a GC root.** Suggested fix: a `Vec<(KontRef, EnvRef)>`
save stack in `RunTimeStruct` that `collect_garbage` marks alongside
`dynamic_wind` (which already works this way), pushed and popped by
`eval_macro`. Worth doing before F6/F7, and certainly before lowering the
default threshold.

Counters for one `s1 -r -q` run (temporary instrumentation, since reverted):

| Metric | Value |
|---|---|
| `Frame::lookup` calls | 348,930 |
| frames walked by those lookups | 1,444,736 (**4.14 frames per lookup**) |
| heap objects allocated | 55,848 |
| GC runs | **0** (threshold is 100,000 allocations) |

Note the last row: **GC never runs during the shipped regression suite.** Any GC
work below is invisible on the headline benchmark and only pays off on longer
programs.

## Profile

`flamegraph` needs `sudo` for dtrace on macOS, so these are `/usr/bin/sample`
runs at 1 ms (4,176 samples, `fib 31`). To get the SVG, run:

```
sudo cargo flamegraph --release --bin s1 -o flamegraph.svg -- -q -f bench/fib.scm
```

Breakdown by top-of-stack, `fib` workload:

| Bucket | Share |
|---|---|
| malloc / free / memset / memmove / realloc | 61 % |
| interpreter core (`dispatch_kont`, `eval_main`, `lookup`, `apply_proc`, `list_to_vec`) | 22 % |
| `debugger()` (doing nothing — tracing is off) | 2.4 % |
| BigInt arithmetic proper (`lt_b`, `minus_b`, `plus_b`) | 3.9 % |
| GC (`collect_garbage`, `mark_reachable`, `Rc::drop_slow`) | ~4 % |

> **That 61 % is overstated — do not size work against it.** `sample` attributes
> leaf PCs badly in heavily inlined release code. Measured directly instead (see
> "Allocation budget" below), the *entire* cost of allocation is **~30 %**, and
> Scheme-object allocation is a rounding error within it.

A profile of the *unfixed* `regression 20x` workload instead shows 90 % of
samples in `Frame::lookup` — that is F1 below, not a property of `lookup`.

## Allocation budget (measured, not sampled)

Two instruments, both temporary (patch kept at
`/tmp/phase1-plus-instrumentation.patch` during the session; re-derive if needed):

1. A counting `#[global_allocator]` — ground truth on malloc traffic.
2. A bump `#[global_allocator]` that never frees — the ceiling if allocation
   were free.

Ceiling, with allocation effectively free:

| Workload | system malloc | bump | headroom |
|---|---|---|---|
| `fib 25` | 0.29 s | 0.20 s | **31 %** |
| `list/map` | 0.67 s | 0.51 s | **24 %** |
| `regression 20x` | 0.79 s | 0.53 s | **33 %** |

So ~30 % is the *total* budget for every allocation-reduction idea combined
(F2, F3, F5, F9, F10). The bump allocator still pays a CAS and first-touches
600 MB, so the true ceiling is somewhat better — call it 30-40 %.

Where the malloc traffic actually comes from, `fib 25` (242,785 calls,
**11,057,911 mallocs** — 45 per call):

| Source | Count | Per call | Share of mallocs |
|---|---|---|---|
| `Rc<Kont>` allocations | 3,641,973 | 15 | 33 % |
| `Vec::clone` in `dispatch_kont` | 4,612,934 (2.67 M elems) | 19 | ≤24 % |
| `list_to_vec` per application | 1,092,719 | 4.5 | 10 % |
| arithmetic builtins (BigInt temporaries) | 606,961 | 2.5 | ~8 % |
| **`GcHeap::alloc` (all Scheme objects)** | **366,563** | 1.5 | **3.3 %** |

Runtime share of any one source ≈ (its share of mallocs) × 30 %. That model was
checked against a spike: interning small integers cut `GcHeap::alloc` by 98.8 %
(366,563 → 4,513) — 3.2 % of malloc traffic — and moved `fib` by ~1 %, i.e.
inside noise. Which is exactly what the model predicts.

**The churn is in the evaluator's own bookkeeping — continuation frames and the
vectors inside them — not in Scheme values.**

### Allocation census by value type

`GcHeap::alloc` calls, by `SchemeValue` variant:

| Workload | Int | Pair | everything else |
|---|---|---|---|
| `fib 25` | 99.4 % | 0.5 % | 0.1 % |
| `regression 1x` | 74.7 % | 21.7 % | 3.6 % |

Integers are nearly all tiny: 99.9 % of `fib`'s have |v| < 256, 83 % of
`regression`'s, 100 % of `list/map`'s. Essentially none need a bignum
(3 of 41,747 in the whole regression suite).

## Findings, ranked

Phase 1 (F1, F4, F8, dead `free_list`) is **done** — see the status notes below.

### F1 — stale `env` and `tail` leak between sibling subexpressions — DONE

The original diagnosis ("`eval_main` never resets `state.env`") was the symptom.
The actual defect was three separate places where saved state was discarded:

1. `handle_eval_arg` took the saved environment as `_env` and **never used it**.
   `Kont::EvalArg` saves the call's env precisely because a tail call
   deliberately leaves `state.env` inside the callee; the restore was missing,
   so argument *n+1* was evaluated in argument *n*'s callee frame.
2. `eval_cek` hardcoded `tail: true` into `Kont::EvalArg` instead of the
   application's real tail position, and `handle_eval_arg` never read the field
   back. So `state.tail` also leaked out of argument evaluation — a call whose
   last argument ended in a tail call was itself treated as a tail call and
   skipped its `RestoreEnv` barrier.
3. `Kont::Bind` carried `env: Option<EnvRef>`, and the `None` (define) path
   resolved the target frame from `state.env` *when the frame ran*. After a
   non-local exit through that frame (`call/cc`), that is an unrelated
   environment, so the definition landed in the wrong frame.

Fixes: restore `state.env` and `state.tail` from the `EvalArg` frame; capture
the real tail position in `eval_cek`; change `Bind` to carry the frame captured
where the `define`/`set!` was written, plus an `is_define` flag for the return
value. Top-level forms are then reset to the global frame in `repl`, which is
what makes `load` put definitions in the global environment instead of `load`'s
own frame.

Bugs this fixed, beyond the speedup:

- `(map f (build 2 '()))` inside a lambda body no longer reports
  `Unbound variable`.
- A `define` whose value expression escapes via `call/cc` now actually binds
  (`(define a (call/cc (lambda (k) (k 5))))` previously left `a` unbound, and
  only appeared to work because the leaked frame stayed reachable).
- `scheme/basic_tests.scm:503` had a stray paren that silently swallowed a
  test; fixed, hence 486 rather than 484.

Env chain depth is now flat at 1 across any number of loads, and
`regression 20x` scales linearly.

How it was found: the env chain depth at end-of-file grew `1 → 30 → 45 → 60 →
75`, i.e. +15 frames per `load`, never unwound. `Frame::lookup` walks that chain
with a failed hash probe per level, which is where the quadratic came from — a
profile of the unfixed `regression 20x` put 90 % of samples in `lookup`. After
the fix, depth is 1 at every top-level form and single-run frame-walks dropped
from 1,444,736 to 624,068.

### F2 — one `malloc` per Scheme value — expected payoff **~1 %**

`src/gc/heap.rs:105-114`: `alloc` does `Box::new` per object, plus a push to
both `objects` and `nursery`. `GcObject` is 64 bytes. `fib 25` performs 366,563
of these — which is only **3.3 % of malloc traffic**, hence ~1 % of runtime.
Not worth doing on its own. Its real value is GC pressure (fewer objects → fewer
collections), which the benchmarks here do not exercise.

Fix: slab allocation — `Vec<Box<[GcObject; N]>>` chunks plus a real free list
threaded through dead slots. (A never-read `free_list: Vec<GcObject>` field used
to sit in `GcHeap` as a stub for this; it was removed in Phase 1 rather than
left to rot. Reintroduce it as an actual free list when doing this work.)

### F3 — `SchemeValue::Int(BigInt)` — SKIPPED, expected payoff **~4-6 %**

Decided against, 2026-09-18: of everything left in this doc, F3 has the
smallest expected payoff, and the actual workload mix doesn't favor it
enough to justify touching every arithmetic builtin plus adding the bignum
test coverage this section already flags as missing. The analysis below is
otherwise unchanged and left for reference in case this is revisited.

`src/gc/mod.rs:116`. Every integer heap-allocates a `BigUint` backing `Vec`, so
each number costs *two* mallocs, and arithmetic allocates temporaries on top
(`plus_b` does `sum_int += i` on a `BigInt` accumulator).

Fix: split into `Int(i64)` and `BigInt(BigInt)`. Add fixnum fast paths in
`src/builtin/number.rs` using `checked_add`/`checked_sub`/`checked_mul`,
promoting to `BigInt` only on overflow. **Bignums stay** — they are a separate
variant reached on overflow, not removed. Verified working today and must remain
so: `(fact 30)`, `(* i64::MAX i64::MAX)`, `(+ i64::MAX 1)` all give exact
results. There are currently **no bignum tests** — add some first, since nothing
in the suite would catch a regression (3 of 41,747 integers in the whole suite
exceed i64).

Payoff breakdown: ~8 % of malloc traffic is BigInt temporaries → ~2.5 % of
runtime; plus ~2-3 % of non-malloc CPU now spent in `BigInt` add/compare, which
becomes a register op. Optionally add a small-int cache (intern −1024..1024) for
another ~1 %; a 20-line spike was validated (all 486 tests pass, bignums
unaffected) and measured at ~1 %.

Correction to an earlier claim in this doc: `BigInt` is **not** what sizes
`SchemeValue`. `PortKind` and `Callable` are both 48 bytes, so `SchemeValue`
stays 56 and `GcObject` stays 64 even after this change.

### F4 — `debugger()` called on every CEK step — DONE

`src/eval/cek.rs:63` calls `debugger()` unconditionally; `src/utilities.rs:27-30`
returns immediately when `trace` is `Off`. 2.4 % of samples for a function call
that does nothing in normal operation.

Fixed: hoisted the test to the call site in `step()`. Together with F1 this took
`tail loop 300k` from 0.75 s to 0.30 s.

### F5 — `Rc<RefCell<Frame>>` + `FxHashMap` per call — DONE

`src/env.rs:117-119` and `src/eval/mod.rs:184`. Each call allocates an `Rc`
control block, a `Frame`, and a hash table — for `fib`, a table holding a single
binding.

Fix: make `Frame.bindings` a `Vec<(GcRef, GcRef)>` with linear scan. For ≤8
bindings (essentially every non-global frame) a pointer-compare scan beats
hashing, and it removes the table allocation. Keep the `FxHashMap` only for the
global frame — e.g. an enum, or keep the map but construct it with the exact
capacity.

Fixed (commit f5b9894): `Frame.bindings` is now a `Bindings` enum —
`Small(Vec<(GcRef, GcRef)>)` for every frame created via `extend()`,
`Large(HashMap<GcRef, GcRef>)` kept only for the global frame (identified
structurally by `parent.is_none()`). Bundled with the F9/F10(2) work below;
measured together at 18-22 % across fib/tail-loop/list-map.

### F6 — GC mark re-pushes already-marked objects — DONE

`src/gc/heap.rs:298-336`. The mark bit is checked *after* popping, so an object
reachable by k paths is pushed k times. Measured 4.17 M worklist pushes against
~110 K live objects per GC — about **5× redundant**.

Worse, `Mark for EnvRef` (`src/env.rs:128-141`) walks the *entire parent chain*
for *every* closure, unconditionally. Cost is O(closures × depth × bindings),
which is why `env_mark_frames` hit 167,030 frame-walks over 8 GCs.

Fix: check the mark bit before pushing, and give `Frame` its own visited flag so
a shared chain is traversed once per collection instead of once per closure.

Fixed (commit ad59cc1): `mark_reachable`'s worklist now marks on push
(`push_if_unmarked`) instead of on pop, and `env::Frame` carries its own
`gc_mark_epoch: Cell<u64>` so `Mark for EnvRef`'s walk stops the instant it
hits a frame already visited this cycle — correct because the walk always
continues to the root, so an already-visited frame means everything above
it was too. (`Frame` isn't a `GcObject`, so it reads a small `AtomicU64`
mirror, `crate::gc::GC_EPOCH`, rather than needing an epoch parameter
threaded through the whole `Mark` trait.) Measured on a workload built to
exercise exactly this case (3,000 closures sharing a 200-deep environment
chain, forced through ~150 collections): **2.16 s → 0.34 s, ~6.4×**. The
existing fib/tail-loop/list-map benchmarks barely collect at the current
default threshold, so they don't show this off — see F7's threshold change
below for why that stopped being true for the regression suite specifically.

### F7 — full unmark pass every GC — DONE

`src/gc/heap.rs:221-223` pointer-chases every object in `objects` to clear one
bool (718 K unmarks over 8 GCs).

Fix: replace the boolean with a generation counter compared against the current
GC epoch, so no reset pass is needed; or move mark bits into a side bitmap
adjacent to the F2 slabs so mark and sweep become linear scans of contiguous
memory.

Fixed (commit ad59cc1): `GcObject.marked` is now a `u64` GC epoch (0 = never
marked) instead of a `bool`. `collect_garbage` just bumps
`GcHeap::current_epoch`; sweep keeps whatever has `marked == current_epoch`.
No side bitmap / F2 slabs needed — this works against the existing per-object
`Box` allocation.

Also, separately: the default `gc_threshold` was lowered from 100,000 to
20,000 (same commit) specifically so the regression suite exercises ordinary
collections on its own — previously it allocated only ~55,848 objects total
and never collected outside `gc_stress_tests.scm`'s own deliberately extreme
thresholds, so F6/F7 (and any future GC work) would otherwise be invisible to
the standard `-r` run. Verified clean at `gc-threshold` 1/5/20/200/2000 with
`S1_GC_POISON=1` before and after.

### F8 — stdout flushed on every `display` — DONE

`src/io.rs:281-284` does `print!` then `io::stdout().flush()`. That is one
`write` syscall per `display` call — roughly 2,400 per regression run. Only ~7 %
when output goes to `/dev/null`; considerably worse on a real terminal.

Fixed, but **no measurable wall-clock win** when output is redirected: A/B
measured 1.05 s vs 1.08 s to `/dev/null` and 0.86 s vs 0.83 s through a pipe —
both within noise. Rust's `Stdout` is already a `LineWriter`, so the fix was to
delete the explicit per-call `flush()` rather than add buffering; stdout is now
flushed before any stdin read and at the end of `repl`. The ~2,400 saved
`write(2)` calls per run only matter on a real terminal, which is not measured
here.

### F9 — `list_to_vec` allocates per application — DONE, expected payoff **~3 %**

A fresh `Vec<GcRef>` for every call's argument list: 1,092,719 calls for
`fib 25` (4.5 per Scheme call), 10 % of malloc traffic.

Fix: a reusable argument stack — one `Vec<GcRef>` in `RunTime`; push args, pass
a slice, `truncate` on return.

Fixed, bundled with F10(2) below (commit f5b9894): `Kont::EvalArg` no longer
calls `list_to_vec` at all — `remaining_exprs: GcRef` walks the call's
cons-list directly instead of copying it into a `Vec` up front, and
evaluated arguments accumulate on the new shared `RunTime::arg_stack`
(F10(2)) rather than a fresh per-call `Vec`. Safe under `call/cc` because
`capture_call_site_kont` already strips `EvalArg`/`ApplyProc` frames on
capture — verified by diffing output of the exact "capture mid-argument-
evaluation, store it, invoke repeatedly after intervening calls" pattern
before/after, byte-identical. Measured with F5 at 18-22 % across
fib/tail-loop/list-map.

### F10 — continuation churn — part (1) DONE, measured **16-17 %**

The largest single item, and the original ranking had it last. Two parts, both
in the evaluator's bookkeeping rather than in Scheme values:

**(a) `Rc<Kont>` per continuation frame** — 3,641,973 allocations for `fib 25`,
**15 per Scheme call**, 33 % of all malloc traffic. `Kont` is 96 bytes.

**(b) `dispatch_kont` clones the `Vec`s inside the frame it is dispatching on**
(`src/eval/cek.rs`, the `EvalArg`/`Cond`/`AndOr`/`Escape` arms) — 4,612,934
clones moving 2.67 M elements for `fib 25`, 19 per call. It matches on `&*kont`
so it cannot move the fields out, and clones instead.

Fixes, cheapest first:

1. **DONE** (commit d2796a4, 16-17 % across all workloads — better than the
   5-7 % predicted, because it also removed the `Rc::clone` in `step` and the
   per-top-level-form `Kont::Halt` allocation). `dispatch_kont` now takes the
   frame by value via `Rc::try_unwrap` and moves its vectors into the handler.
   The commented-out `take_kont` draft the author had left in the file was the
   right idea; it is now real, plus the GC rooting it needed.
2. **DONE** (commit f5b9894, bundled with F5/F9, 18-22 % together). `EvalArg`
   no longer carries `remaining`/`evaluated` `Vec`s at all — see F9 above.
3. **DEFERRED**, 2026-09-18. (a) needs a `Vec<Kont>` stack with indices rather
   than an `Rc` chain — full design in `Docs/kont-flat-stack-design.md`
   (written before any code was touched, per this project's usual practice).
   Test gaps that design identified were closed first (commit 8adbd2a: no
   `call/cc`/`dynamic-wind` test ran under GC pressure; nothing captured from
   deep inside a long non-tail chain), and doing that surfaced a correction
   to the design itself: capture is O(1)-ish *today* for ordinary chains
   (`capture_call_site_kont` shares via one `Rc::clone` once it hits the
   first non-skippable frame), not O(depth) as first assumed, so a flat
   `Vec` design's O(depth) capture is a real regression for that case, not a
   wash. Decided not to implement it: parts 1 and 2 above already removed
   two of the three per-call allocations this item was chasing, so what's
   left (removing the `Rc<Kont>` node itself) is now the smallest remaining
   item on this list by expected payoff, while being by a wide margin the
   largest and riskiest to implement — it touches nearly every handler in
   `src/eval/cek.rs` and `src/eval/kont.rs` plus `call/cc`/`dynamic-wind` in
   `src/sys_builtins.rs`, all at once, with no partial-compile checkpoint
   along the way. Worth revisiting only if it becomes load-bearing for a
   future bytecode-VM redesign rather than pursued as a standalone
   optimization — see the design doc's final section.

## Suggested order

**Phase 1 — DONE.** F1, F4, F8, dead `free_list` field removed. `regression 20x`
5.33 s → 0.82 s and now linear; `tail loop 300k` 0.75 s → 0.30 s. 486/486 scheme
tests and 87/87 cargo tests pass. `fib` and `list/map` are unchanged, as
predicted — they are allocator-bound, which is Phase 2.

**Phase 2 — DONE, 2026-09-18.** Was "F3 then F2"; that was wrong. The 61 %
figure it was originally sized against came from `sample`'s leaf attribution
and does not survive direct measurement: total allocation cost is ~30 %, and
Scheme-object allocation is 3.3 % of malloc traffic. Corrected order by
measured payoff, with final status:

| Item | Expected | Effort | Status |
|---|---|---|---|
| ~~F10(1) `Rc::try_unwrap` instead of cloning frame vectors~~ | 16-17 % | — | **DONE** |
| F10(2) + F9 shared argument stack | ~8-10 % | medium | **DONE**, bundled with F5, measured 18-22 % |
| F5 small-vec frames | ~3-4 % | medium | **DONE**, see above |
| F3 fixnums (+ optional small-int cache) | ~4-6 % | medium; needs bignum tests first | **SKIPPED** |
| F10(3) `Vec<Kont>` stack instead of `Rc` chain | rest of the ~30 % | large, touches call/cc | **DEFERRED** — see F10 above, `Docs/kont-flat-stack-design.md` |
| F2 slab allocator | ~1 % | skip unless doing it for GC pressure | not started |

Also fixed along the way, not originally in this table: the redundant
operator-symbol lookup in `eval_cek` (every application looked its operator
up twice — once to check for a special form, again via `EvalArg`'s operator
phase — commit f5b9894).

**Phase 3 — DONE, 2026-09-18.** F6, F7. GC did not fire at all on the
regression suite at the time this phase was written; fixed alongside F6/F7
by lowering the default `gc_threshold` (100,000 → 20,000, commit ad59cc1) so
it now does, without resorting to the artificially extreme thresholds
`gc_stress_tests.scm` already used for targeted defect coverage. F6
specifically was validated against a purpose-built workload (many closures
sharing a deep environment chain) since fib/tail-loop/list-map barely
collect even at the new default — see F6 above.

Re-run `bench/bench.sh` after each phase and record the numbers here.
