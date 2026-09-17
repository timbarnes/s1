# Replacing Nested Evaluation in the Macro / Quasiquote Path

Investigated 2026-09-16. Supersedes the "OPEN: memory-safety bug under GC
pressure" section of [performance.md](performance.md), which identified one of
the five defects below and proposed a fix that covers two of them.

## Summary

Five places in the interpreter evaluate Scheme code by calling `eval_main`
*re-entrantly*, parking the outer machine state in Rust locals and restoring it
afterwards:

| Site | Saves |
|---|---|
| `eval_macro` (`src/eval/mod.rs:153`) | `kont`, `env`, `tail` |
| `quasiquote_sf` (`src/special_forms.rs:574`) → `expand_macro` | `kont`, `tail` |
| `expand_sf` (`src/special_forms.rs:525`) | `kont`, `tail` |
| `with_timer_sf` (`src/special_forms.rs:638`) | nothing |
| `eval_string_sp` (`src/sys_builtins.rs:64`) | `next` (as an argument) |

`CEKState` *is* the GC root set (`impl Mark for CEKState`,
`src/eval/kont.rs:515`), so anything parked in a Rust local is invisible to the
collector for the duration of the nested evaluation. Four of the five sites are
broken, in three distinct ways, and the discipline has no way to be checked by
the compiler.

**This is not a bug to patch; the mechanism should be replaced.** Every
"evaluate this, then resume" should be a `Kont` frame, which
`impl Mark for KontRef` already traces — the pattern `Kont::RestoreEnv` and
`Kont::Eval` already use.

## Verified defects

All findings below were reproduced. Experiments ran in a throwaway copy of the
tree; the two instruments were a *leaking* sweep (never free) and a *poisoning*
sweep (overwrite the freed object's value with a `<<FREED>>` sentinel and leak
it) — see "Tooling worth keeping" at the end.

### 1. `eval_macro`: unrooted `saved_kont` / `saved_env` — SIGSEGV

```
printf '(gc-threshold 20)\n(load "scheme/test-harness.scm")\n(load "scheme/macro_tests.scm")\n' > /tmp/mt.scm
./target/release/s1 -f /tmp/mt.scm -q < /dev/null   # exit 139, after "my-or returns first truthy"
```

Two controlled deltas establish the mechanism rather than just the symptom:

* Leaking sweep → passes. So it is a premature free, not a logic error.
* A GC-marked save stack rooting *only* `eval_macro`'s `saved_kont` and
  `saved_env` → full `regression.scm` passes at `gc-threshold` **1, 20 and
  200**. The unfixed binary segfaults at 1 and 20.

The hypothesis recorded in performance.md is correct.

### 2. `quasiquote_sf`: unrooted `saved_kont` — silent corruption

Independent of #1. With #1 rooted and the ordinary sweep:

```scheme
(gc-threshold 20)
(define (deep n) (if (= n 0) 0 (+ 1 (deep (- n 1)))))
(define (f1) (deep 200) (list 1 2 3))
(define (run i)
  (if (= i 0) 'done
      (begin (length `(h ,(f1) ,(f1) ,(f1) t)) (run (- i 1)))))
(run 500)
```

→ `Error: -: all arguments must be numbers`. Clean under a leaking sweep.

### 3. Expander intermediates in `src/macros.rs` — not fixed by a save stack

`expand_macro_internal` holds partially-built results in Rust locals
(`Expanded::Single`/`Splice`, `hs`, `elements`, `filtered`) *across* its nested
`eval_main` calls. With the save stacks for #1 and #2 both installed, the
poisoning sweep reports 200/200 corrupted expansions:

```
CORRUPT: (h <<FREED>> <<FREED>> <<FREED>> (x y) t)
```

This is the reason the save-stack approach is a stopgap rather than a fix:
rooting the saved *machine state* does nothing for the expander's own
*intermediate values*.

### 4. `eval_main` forces `tail = true`, clobbering `state.env` — not a GC bug

Reproduces on the stock binary with GC never running:

```scheme
(define (f x) (list x))
(define (h i) `(a ,(f i) b ,i))
(h 7)
;=> Error: Unbound variable: i
;=> (a (7) b )
```

`eval_main` sets `state.tail = true` (`src/eval/cek.rs:25`), so the nested
evaluation of `,(f i)` takes the TCO path in `apply_proc` and never pushes a
`RestoreEnv` barrier — `state.env` is left inside `f`. `quasiquote_sf` restores
`kont` and `tail` but not `env`, so the following unquote is evaluated in the
callee's frame.

### 5. `with_timer_sf` never restores `kont` — continuation truncation

```scheme
(display (+ 100 (with-timer (* 2 3))))   ; prints nothing; the outer form is abandoned
```

The nested machine leaves `state.kont` at `Halt`, so `run_cek` returns as soon
as `with_timer_sf` produces its value and the real continuation is discarded.

### Latent, same class

* `eval_string_sp` holds `next` and its `results` vector in Rust locals across
  every form it evaluates.
* A continuation captured by `call/cc` inside a nested evaluation escapes to
  the *inner* `Halt`, not to the outer continuation. Unsound today; fixed for
  free by the redesign.

## Proposal

### A. `Kont::MacroExpand` — replaces `eval_macro`

In `apply_unevaluated` (`src/eval/cek.rs:802`), for `Callable::Macro`:

```
bind params to the raw argument forms  ->  macro_env
push Kont::MacroExpand { call_env: state.env.clone(), mode, next }
state.env = macro_env; state.control = Expr(body); state.tail = false
```

`handle_macro_expand` restores `state.env = call_env` and then either

* `mode = Evaluate`: `Control::Expr(expansion)`, `tail = true` — a normal macro
  call; or
* `mode = Expand`: `Control::Value(expansion)` — for `(expand form)`.

`eval_macro` and its save/restore disappear. The saved environment now lives in
a frame reachable from `state.kont`, so it is marked with no new root-set
machinery. `expand_sf` becomes a `Kont::ExpandArg { next }` that evaluates its
argument normally and then pushes `MacroExpand { mode: Expand }`.

### B. Lower quasiquote syntactically instead of evaluating inside the walker

This is the load-bearing change. Rather than making the recursive expander
re-entrant, remove the interleaving entirely: transform the template into an
ordinary expression and hand it back to the machine.

```
qq(d, atom)                       ->  (quote atom)
qq(1, (unquote x))                ->  x
qq(d, (unquote x))          d > 1 ->  (list 'unquote qq(d-1, x))
qq(d, (quasiquote x))             ->  (list 'quasiquote qq(d+1, x))
qq(1, ((unquote-splicing x) . r)) ->  (append x qq(1, r))
qq(d, (a . b))                    ->  (cons qq(d, a) qq(d, b))
qq(d, #(e ...))                   ->  (list->vector qq(d, (e ...)))
```

`quasiquote_sf` becomes three lines: build the lowered form, `insert_eval`, and
return. No nested evaluation, nothing to save, nothing to root — the transform
is pure allocation, and the only GC points in the system are
`handle_restore_env` (`src/eval/cek.rs:741`) and the `(gc)` builtin, neither of
which can fire inside it.

`src/macros.rs` — the `Expanded` enum, `expand_macro`, `expand_macro_internal`
— is deleted outright. That removes defect #3 rather than papering over it, and
defects #2 and #4 along with it.

Two details:

* Embed `cons` / `append` / `list->vector` in the lowered form as **literal
  `Callable` `GcRef`s, not symbols**. `eval_cek` already treats `Callable(_)` as
  self-evaluating (`src/eval/cek.rs:103`), and a non-`Symbol` operator falls
  through to the generic application path. This makes expansion immune to a user
  rebinding `append`.
* Vectors inside a quasiquote are currently not walked at all
  (`expand_macro_internal` matches only `Pair` and `Symbol`). The rewrite is the
  natural place to add that; it is a behaviour change and should be called out
  in the commit.

Beyond the memory safety, this gives correct tail position for the expansion and
makes `call/cc` inside an unquote behave.

### C. `Kont::Timer { start: Instant, next }`

`with_timer_sf` pushes the frame and evaluates the body normally; the handler
discards the body's value and returns the elapsed float to `next`. `Instant`
holds no `GcRef`, so its `mark` arm is a no-op. Fixes defect #5.

### D. `eval_string_sp`

Parse all forms up front, then drive them from a frame —
`Kont::EvalSeq { remaining: Vec<GcRef>, results: Vec<GcRef>, next }` — so both
vectors are marked.

`eval_string` as used by the REPL (`src/main.rs:86`) is genuinely top-level and
can keep calling `eval_main`. `load` is defined in Scheme
(`scheme/s1-core.scm:171`) and works by pushing a port that the main loop reads,
so it is not re-entrant and needs no change.

### E. Mechanical checklist for every new frame

Each new `Kont` variant needs an arm in `Kont::next()`, `impl Debug for Kont`,
`impl Mark for KontRef`, and `dispatch_kont`. A missing `Mark` arm reintroduces
exactly this bug class silently, so the matches should be exhaustive rather than
ending in a catch-all — note that `dispatch_kont` currently ends in
`_ => Err(...)` (`src/eval/cek.rs:299`), which hides omissions until runtime.

Keep frame payloads small or behind `Rc`: `take_kont` clones the frame whenever
it is shared, and `Kont`'s size is on the hot path (see performance.md, F10).

## Staging

1. **B** — quasiquote lowering. Deletes `src/macros.rs`; fixes defects #2, #3
   and #4. Largest win and independent of the rest.
2. **A** — `Kont::MacroExpand`. Fixes defect #1, the segfault.
3. **C** and **D** — small and independent.
4. Lower the default `gc_threshold` from 100,000 and add a `gc-threshold 1`
   regression run to CI. Today GC never runs during the shipped suite (55,848
   allocations), which is why all of this stayed invisible.

## On the stopgap proposed in performance.md

The suggested `Vec<(KontRef, EnvRef)>` save stack in `RunTimeStruct`, marked by
`collect_garbage` alongside `dynamic_wind`, does work: it was measured fixing
the segfault at threshold 1. But it covers only defects #1 and #2. The poisoning
sweep proves #3 survives it, and #4 and #5 are not GC bugs at all. It is a
reasonable one-commit tourniquet if the threshold must be lowered before the
rewrite lands, and nothing more.

## Tooling worth keeping

Two debug-only sweep modes, gated behind an env var or a cargo feature, made
these findings possible and should be permanent:

* **leak** — `sweep` retains but never frees. If a crash disappears under it,
  the crash is a premature free. Distinguishes memory bugs from logic bugs in
  one run.
* **poison** — `sweep` overwrites the freed object's value with a
  `Symbol("<<FREED>>")` sentinel and leaks the box. Turns silent corruption into
  a visible marker in printed output. Defect #3 is not findable without it.

Combined with a `gc-threshold 1` run over `regression.scm`, these give a cheap
standing check for the whole "interpreter state held in a Rust local is not a GC
root" class.
