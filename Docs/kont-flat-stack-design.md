# F10(3): Replacing the `Rc<Kont>` Chain with a Flat Stack

Designed 2026-09-18, not yet implemented. Written before touching code, per
this project's own practice (see nested-evaluation.md, gc-nursery-removal.md).
Companion question answered at the end: is the current test suite adequate
to validate this change?

## Why this is last on performance.md's list

`Rc<Kont>` allocations are 33% of all malloc traffic for `fib 25` — the
single largest item, ahead of the argument-vector churn F9/F10(2) already
removed. But performance.md flagged it as "large, touches call/cc" and
deferred it, correctly: `call/cc` and `dynamic-wind` depend on properties of
today's representation that a naive flat stack does not have for free. This
document works out exactly what those properties are and what a flat-stack
design needs to do to preserve them.

## What `Kont` gives you today

`Kont` is a persistent, `Rc`-linked list: every frame is `Rc::new(Kont::Foo
{ ..., next: prev })`, where `prev` is an `Rc::clone` of whatever `state.kont`
was. Two things fall out of that for free:

1. **Pushing a frame never invalidates an older reference to the chain
   below it.** Once a frame is built, its `next` pointer and everything
   below it are immutable — the only thing that ever changes is what
   `state.kont` currently points to.
2. **Capturing "the current continuation" is just cloning an `Rc`.** No
   copying, O(1), regardless of depth.

This is exactly what real multi-shot continuations need. This evaluator
doesn't have those, though — `call/cc` is escape-only, implemented by
`capture_call_site_kont` (`src/sys_builtins.rs:1044`), which *filters* while
walking the live chain:

- `EvalArg` / `ApplyProc` frames: skipped entirely, unconditionally.
- `DynamicWind` in `Return` phase: skipped entirely.
- `DynamicWind` in `Thunk` or `After` phase: **kept**, but rebuilt with
  phase forced to `After` — so invoking the captured continuation later
  never re-runs a thunk that was in flight at capture time, it treats the
  thunk as already-completed and proceeds straight to `after`.
- Anything else (`If`, `Seq`, `Bind`, `RestoreEnv`, `Cond`, ...): the walk
  **stops** and shares (`Rc::clone`s) that frame and everything below it,
  unchanged.

So a typical capture is cheap in practice (it stops at the nearest
non-application frame, usually 1-3 hops up), and the result is a *new*,
independent-looking chain that nonetheless mostly aliases the live one via
`Rc::clone` for its unshared-but-unmodified tail.

The critical fact for this design: **invoking a captured continuation can
happen long after the frames it was captured from have been popped off the
live stack and reused for something else entirely.** The regression suite
already exercises this on purpose — `advanced_tests.scm`'s "Re-entrant
dynamic-wind" test captures `k` inside a `dynamic-wind` thunk, lets that
whole top-level form finish normally, runs more code (including a *second*,
unrelated `dynamic-wind`), and only then calls `(k 're-entry-value)`,
asserting both the return value and the exact thunk-execution order
(`before-2 after-2 before-1 after-1`). By the time `k` is invoked, the
frames it closes over are long gone from any "current" stack. Persistent
sharing is what makes that work today at zero extra cost: the old nodes stay
alive because the captured `Rc` keeps them alive, entirely independent of
whatever `state.kont` has moved on to since.

## Why a naive `Vec<Kont>` breaks this

A flat, mutable `Vec` reused as a call stack does not have property (1)
above: once a frame is popped and the slot reused, whatever used to be
there is gone. "Capture the current continuation" as *just an index* into
that `Vec` is not enough — by the time `(k 're-entry-value)` runs in the
test above, that index has long since been overwritten by unrelated calls.
This is exactly the tension performance.md flagged and is the reason this
needs a real design pass rather than a mechanical find-and-replace.

## Proposed design: flat spine, copy-on-capture, copy-on-invoke

This is the standard technique for cheap *escape-only* continuations on a
contiguous stack (as opposed to full re-entrant continuations, which
usually do need either persistent structures or full stack-copying VMs).
Concretely:

- `Kont`'s variants lose their `next: KontRef` field — call the result
  `KontFrame`. "Next" becomes implicit: whatever is one slot lower in the
  array.
- `CEKState.kont: KontRef` becomes `CEKState.kont_stack: Vec<KontFrame>`.
  Pushing a frame is `kont_stack.push(...)` — no `Rc::clone` of "prev," no
  heap allocation in the common case (amortized `Vec` growth instead).
  Popping is `kont_stack.pop()`, which always yields owned data directly.
  This also **removes `take_kont` and its `Rc::try_unwrap`-or-clone
  fallback entirely** — there is no longer anything to share, so there is
  nothing that can fail to unwrap.
- **Capture** (replaces `capture_call_site_kont`): scan `kont_stack` from
  the top downward applying the *exact same filtering rules* listed above,
  building an **owned, independent `Vec<KontFrame>` snapshot** (a real
  copy, bottom-to-top) of the surviving frames. Store that `Vec` (plus the
  existing `Vec<DynamicWind>` snapshot, unchanged) inside
  `SchemeValue::Continuation` instead of a `KontRef`.
- **Invoke** (replaces `handle_escape`'s final step): after running the
  scheduled dynamic-wind transition thunks — that machinery is untouched,
  it already operates on the separate, already-flat
  `RunTime.dynamic_wind: Vec<DynamicWind>` — **replace `kont_stack`
  wholesale** with a fresh copy of the captured snapshot
  (`kont_stack.clear(); kont_stack.extend_from_slice(&snapshot)`), then set
  `Control::Value(result)`. This mirrors today's semantics exactly: `state.kont
  = new_kont` is *already* a full replacement, not a "pop back to a shared
  depth," so there is no shared-prefix cleverness to get right or wrong
  here — always fully replace.
- `Kont::Escape`'s bookkeeping (remaining thunks, result, target snapshot,
  target dynamic-wind stack) moves to a dedicated `CEKState` field rather
  than a pushed frame. It never composed with "next" the way ordinary
  frames do anyway — today's code already assigns `state.kont =
  Rc::new(Kont::Escape{...})`, discarding rather than extending the
  previous continuation — so this is a representational change only, not a
  behavior change.
- GC marking simplifies: `Mark` for the spine is a flat loop over the
  `Vec` (no worklist needed just to walk it — sub-structure inside a frame,
  e.g. a `Pair` chain in an `If`'s branch, still goes through the existing
  `mark_reachable` worklist as it does today). A captured `Continuation`'s
  snapshot is marked the same way, just against its own private `Vec`
  instead of a shared `Rc` chain. The GC-safepoint invariant that
  `take_kont`'s doc comment already calls out — *a handler must install
  the popped frame's `GcRef`s into `state` before anything can
  collect* — is unchanged; it just applies to `Vec::pop()`'s result
  instead of `take_kont`'s.

### Cost model

Ordinary calls (the overwhelming majority of what runs) go from "one
`Rc::clone` + one heap allocation per pushed frame" to "one amortized
`Vec::push`, no allocation most of the time" — this is the entire point,
and it's what removes the 33%-of-malloc-traffic item. `call/cc` capture and
invoke each become O(depth) copies, versus O(depth) capture / O(1) invoke
today — the same total work per capture-then-invoke pair, just reshaped
from the chain-walk-and-filter into an explicit copy. Since ordinary calls
vastly outnumber `call/cc` uses in real programs, this is the correct trade
and matches how fast Scheme implementations handle escape-only
continuations over a native-like stack.

### What does *not* change

- `RunTime.arg_stack` (F9/F10(2)) — independent, already flat, already
  safe for the same underlying reason (nothing captures an `EvalArg`
  frame).
- `env::Frame` / `Bindings` (F5) and the GC epoch / mark-on-push scheme
  (F6/F7) — orthogonal. `Mark for EnvRef`/`GcHeap::mark_from` need their
  call sites updated to the new spine shape, not their own logic.
- `RunTime.dynamic_wind: Vec<DynamicWind>` — already flat today; untouched.

## Risk register

- **Dynamic-wind phase rewriting during capture** is the trickiest single
  piece of logic being ported (`Thunk`/`After` → keep-and-rewrite-to-`After`,
  `Return` → skip). Get this scan-and-copy exactly right against the
  existing recursive filter, frame kind by frame kind.
- **Escape must fully replace, never merge.** Resist any temptation to
  diff the live stack against the captured snapshot and reuse a common
  prefix "for efficiency" — today's semantics are a full replacement, and
  reusing a prefix is both unnecessary (captures are rare) and a good way
  to reintroduce exactly the staleness bug this document is about.
- **Popped-frame GC safepoint invariant** must be preserved verbatim; it's
  no longer phrased in terms of `take_kont`, so the invariant needs to be
  re-documented at `Vec::pop()`'s call site so it isn't lost in the
  rewrite.
- **Surface area**: this touches essentially every handler in
  `src/eval/cek.rs`, every `insert_*` helper in `src/eval/kont.rs`, the
  `Debug`/`Mark` impls there, `capture_call_site_kont`/`escape_sp`/
  `handle_escape`/`dynamic_wind_sp`/`call_cc_sp` in `src/sys_builtins.rs`,
  the debug dumpers in `src/utilities.rs`, and `mark_from` in
  `src/gc/heap.rs`. It cannot land as a partial/incremental compile state —
  `Kont`'s shape changes for every variant at once. Recommended internal
  sequencing while writing it (not necessarily separate commits, since the
  type change doesn't compile halfway):
  1. Mechanical pass: introduce `KontFrame` (drop `next` from every
     variant), `CEKState.kont_stack: Vec<KontFrame>`, rewire every handler
     to push/pop instead of clone/link. Get everything *except* `call/cc`
     itself passing (i.e., temporarily treat capture as unsupported/`todo!`
     if needed to isolate this step while developing, though it must be
     finished before any commit).
  2. Implement capture (the filtering scan) and invoke (the replace) in
     `sys_builtins.rs`.
  3. Delete now-dead code: `take_kont`, the old `Mark for KontRef`
     worklist walk, `Kont::next()`.
  4. Full verification pass (see below) before considering it done.

## Is the current test suite adequate?

Short answer: **not quite — one real gap, one smaller one, both cheap to
close, and both should be closed before or alongside this change,** per
this project's own standing practice of writing the stress test first
(nested-evaluation.md).

**What's already strong**, verified by reading the files rather than
assuming:

- `scheme/advanced_tests.scm` has substantial `call/cc` coverage: nested
  `call/cc`, escaping from loops/conditionals/list-recursion, and —
  importantly — the "Re-entrant dynamic-wind" test described above, which
  is *exactly* the capture-inside-a-thunk, invoke-long-after-from-a-
  different-dynamic-extent case, and it asserts the precise thunk order,
  not just the return value. This is real, non-trivial coverage of the
  hardest part of `capture_call_site_kont`'s logic, and it's wired into
  the automated `-r` run via `regression.scm`.
- `scheme/gc_stress_tests.scm` already establishes the right methodology —
  re-run existing logic-correctness tests (`macro_tests.scm`) under
  `gc-threshold 1` with the poisoning sweep — this project has already
  built the harness this change needs; it just isn't pointed at `call/cc`.

**Gap 1 (real, should be closed first): no `call/cc`/`dynamic-wind` test
runs under GC pressure.** `gc_stress_tests.scm`'s low-threshold section
re-runs `macro_tests.scm` and its own quasiquote/`eval-string` stress
helpers, but never `advanced_tests.scm`. This matters specifically for this
change because the new code being added — the capture scan and the
copy-back-in on invoke — is exactly the kind of GC-rooting-sensitive code
that has bitten this project twice before (see nested-evaluation.md). A
correctness bug in "is the snapshot `Vec<KontFrame>` reachable while it's
being built, and again while it's being spliced back in" would most likely
show up only when a collection actually happens mid-capture or mid-invoke,
which nothing currently forces.

Fix: add a section to `gc_stress_tests.scm` (or a new file loaded from
`regression.scm`) that re-runs `advanced_tests.scm`'s `call/cc` section, or
at minimum its dynamic-wind-interaction tests, at `gc-threshold 1` with
`S1_GC_POISON=1` available as it already is for the macro section. Small,
mechanical, follows the existing pattern exactly.

**Gap 2 (smaller, recommended): every existing capture is shallow.**
Captures in the current suite happen 1-3 frames deep, or a couple of
`dynamic-wind` levels deep — never from inside a long non-tail call chain.
`gc_stress_tests.scm`'s `deep` helper recurses non-tail to depth 50, which
is fine for exercising repeated grow/shrink cycling of the spine, but
nothing captures a continuation *from* deep inside such a chain. That's
precisely the shape that would expose a snapshot-copy bug (wrong order,
off-by-one, truncation) that a shallow capture wouldn't touch.

Fix: add one test that recurses non-tail a few hundred to a thousand
levels deep, captures a continuation partway through (e.g. via a
conditional escape at some depth), lets the original call chain unwind
normally, and separately invokes the saved continuation later — checking
both the returned value and, via a side-effect counter, that none of the
intervening (correctly-discarded) call frames' work re-executed.

**Gap 3 (minor, found opportunistically, not blocking):**
`advanced_tests.scm`'s "Test 3: Multiple escapes from same dynamic-wind"
captures `multi-k` but never actually invokes it a second time — the test
name promises more than it checks. Worth extending while in the area; not
a prerequisite.

**On unit tests specifically**: none of the 98 `cargo test` tests construct
a `Kont` chain or exercise `call/cc` — they're black-box (evaluate a
Scheme expression, check the result), which is actually the *right* tool
for validating that a representation change preserved behavior, not a gap
to fill with new white-box tests. Every prior change in this performance
series (F5, F9/F10(2), F6/F7) was validated the same way — Scheme
regression suite plus the `S1_GC_POISON` stress protocol, no new Rust unit
tests — and there's no reason for F10(3) to be different. It does mean
100% of this change's validation burden sits on the Scheme suite, which is
exactly why gaps 1 and 2 matter more here than they would for a smaller
change.

**Not testable by assertions, and not a suite gap**: performance regressions.
"Still correct" and "still fast" are checked by different means — the
existing regression/unit suites can only confirm the former. Verifying the
latter for this change means re-running `bench/bench.sh` plus a `call/cc`-
heavy microbenchmark before/after, the same way F5/F9/F10(2)/F6/F7 were each
measured directly rather than assumed.

## Recommendation

Close gap 1 (required) and gap 2 (recommended) as a small, self-contained
first step — cheap, low-risk, immediately valuable independent of this
redesign, and it's the same "write the stress test first" move that made
the nested-evaluation.md and gc-nursery-removal.md fixes verifiable. Then
implement F10(3) against that strengthened harness, verifying at each
sequencing step above with the full regression suite, `gc-threshold`
1/5/20/200/2000 with `S1_GC_POISON=1`, and a direct before/after benchmark
comparison — the same protocol used for every change so far in this
series.
