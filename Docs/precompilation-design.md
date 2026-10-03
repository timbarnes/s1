# Precompilation: a design study

Written 2026-10-02, before any implementation, per this project's practice
(see kont-flat-stack-design.md, hygiene-design.md). The question: which
"precompilation" techniques (resolving names ahead of time, pre-expanding
macros, compiling source into a pre-analysed form) would make s1 faster, in
what order, and how to do them **without fighting the full R7RS environment
model** (libraries, `import`, `environment`, `eval` with environments) that
phase 9 will eventually implement.

All numbers below come from the build at commit c83f996 on the workloads in
"Method". Instrumentation was temporary and is not committed.

## Summary

1. **The cheapest large win isn't precompilation.** About half of all
   evaluator steps only produce a constant or a variable's value as an
   argument. Evaluating those inline, instead of with a full CEK step and
   continuation frame each, was prototyped: **10-24% faster** across the
   workloads, all tests passing, no conflict with anything below. Do this
   first.
2. **Name lookup is only 3-15% of run time today.** Caching bindings
   (technique A) can't win more than that on its own, so on its own it isn't
   worth the disruption.
3. **Re-parsing special-form syntax on each evaluation is cheap** (not
   measurable for `if` and `cond`). Lexical addressing of locals (technique
   B) also has little to win: local lookups already walk only 1.0-1.4 frames
   on average.
4. **Shared binding cells for top-level environments are the common
   foundation** for both full R7RS libraries and any later compiled code.
   Introduce them once, in phase 9, and design them so compiled references can
   point at them. Cells give the right import semantics; they give speed only
   when code holds direct references to them, which needs technique D.
5. **Full pre-analysis (technique D) is the only route to a large further
   gain**, and it has to be designed around cells and lazily analysed
   procedural macros. It should be prototyped on a subset and measured before
   committing to it.

Recommended order: inline trivial operands now; cells with phase 9; then a
measured prototype of D. (Status, 2026-10-03: steps 1 and 2 are done, and
phase 9 uses the cells. Before D, three cheaper changes were made, cutting
another 7-42% (see "After phase 11: cheaper steps before D"). D is not
started.)

## Method

Workloads:

| Name | What |
|---|---|
| fib 25 | `bench/fib.scm` at 25: call-heavy arithmetic recursion |
| typical program | `bench/program.scm`: records, internal definitions, closures, `map`/`for-each`, sorting, string ports (also reproduced in the appendix) |
| macro loop | `bench/macro-loop.scm`: a loop whose body uses small `syntax-rules` macros (`inc!`, `unless`) |
| regression | the full `s1 -r -q` suite, including GC stress |

Counters (temporary instrumentation) recorded evaluator steps by expression
kind, variable and operator lookups with frames walked and where they were
satisfied, special-form dispatches, applications by kind, and closure
creation. Times are medians of five interleaved runs.

There is no sampling profiler on this machine, so costs were measured by
**duplication**: doing a piece of work twice and timing the difference, which
approximates what that work costs now.

## What the evaluator spends its effort on

Counts per workload:

| | fib 25 | typical | macro loop | regression |
|---|---|---|---|---|
| evaluator steps | 5.49M | 4.78M | 3.60M | 4.44M |
| ... constant | 1.39M | 0.15M | 0.40M | 0.96M |
| ... variable reference | 1.51M | 2.16M | 1.00M | 1.31M |
| ... form (pair) | 2.59M | 2.47M | 2.20M | 2.17M |
| variable lookups: frames walked per lookup | 1.0 | 1.2 | 3.6 | 1.4 |
| ... satisfied in the global frame | 0% | 20% | 0% | 14% |
| operator lookups: frames walked per lookup | 2.0 | 2.7 | 4.7 | 2.7 |
| ... satisfied in the global frame | 100% | 67% | 36% | 77% |
| ... failing a plain lookup (alias path) | 0% | 27% | 55% | 16% |
| special-form dispatches | 0.54M | 0.59M | 0.80M | 0.67M |
| closure applications | 0.54M | 0.72M | 0.20M | 0.55M |
| closures created at run time | 76 | 236 | 78 | 61K |

Observations:

* **Half the steps are trivial.** Constants and variable references are
  53% of steps for fib and 48% for the typical program. Each costs a CEK
  step, a continuation-frame dispatch and, as an argument, an `EvalArg` frame
  allocation.
* **Operator lookups go global.** Almost every form has a symbol operator,
  and most of those resolve in the global frame after walking every local
  frame on the way, then a hash probe.
* **Identifiers produced by macros pay twice.** Aliases (Phase 5) fail the
  plain lookup and then resolve again in the definition environment. In the
  typical program a quarter of operator lookups take this path, because
  `define-record-type`'s accessors call `%record-get` through an alias.
* **Local variable lookups are already cheap:** about one frame each.

Costs, by duplication (median time, baseline -> with the work done twice):

| Work duplicated | fib 25 | typical | macro loop | regression |
|---|---|---|---|---|
| every plain lookup's frame walk | 0.55 -> 0.57 (+3%) | 0.54 -> 0.58 (+7%) | 0.40 -> 0.46 (+15%) | 1.00 -> 1.03 (+3%) |
| `if` and `cond` re-parsing their syntax | no measurable change | no measurable change | - | no measurable change |

The lookup figure excludes the second resolution of aliases, so it
understates the macro-heavy case. Even so, lookups are a minority cost.

## Prototype: inline trivial operands

When `handle_eval_arg` moves to the next argument, the prototype checks
whether it is a constant (anything but a symbol or pair) or a variable that a
plain lookup finds. If so, it pushes the value straight onto `arg_stack` and
moves on, without setting `control`, returning to the machine, or allocating
an `EvalArg` frame. Anything else (a nested form, an alias, an unbound name)
takes the existing path, so errors and hygiene behave exactly as before.

| | fib 25 | typical | macro loop | regression |
|---|---|---|---|---|
| time | 0.55 -> **0.42 (-24%)** | 0.53 -> **0.43 (-19%)** | 0.43 -> **0.34 (-21%)** | 1.03 -> **0.93 (-10%)** |
| evaluator steps | 5.49M -> 2.71M | 4.78M -> 2.76M | 3.60M -> 2.40M | 4.44M -> 2.50M |

Output was identical and the full regression suite (1,361 tests, including
call/cc re-entry and GC stress) passed. It is about 20 lines in one function.
The same idea applies to other operand positions that are evaluated and then
consumed by a frame: the test of `if`, the value of `define`/`set!`, `and`/
`or` operands.

Caveats: `trace` shows fewer steps; a continuation captured inside a later
argument sees the inlined values already on `arg_stack`, which is exactly
what the existing snapshot mechanism expects.

### Step 1 as implemented

The implementation (`immediate` in `src/eval/cek.rs`) goes a little further
than the prototype. An expression is evaluated directly if it is a constant, a
variable that a plain lookup finds, or a call whose operator is a variable
bound to a built-in procedure and whose arguments are all constants or such
variables. Every argument is checked before the built-in is called, so if the
expression turns out not to qualify nothing has been evaluated and it takes
the normal path. A built-in that fails raises its error exactly as it would
from the machine, so `guard` and handlers see no difference.

It is used for later arguments of a call, the test of `if`, and the value of
`define` and `set!`. Each extension was measured separately (medians, seconds):

| | fib 25 | typical | macro loop | regression |
|---|---|---|---|---|
| before | 0.52 | 0.50 | 0.39 | 0.99 |
| arguments (constants, variables) | 0.45 | | | |
| + built-in calls | 0.33 | | | |
| + `if` tests | 0.29 | | | |
| + `define`/`set!` values | **0.28 (-46%)** | **0.39 (-22%)** | **0.28 (-28%)** | **0.89 (-10%)** |

The regression suite has tests for the cases that could differ: evaluation
order, unbound variables, built-in errors in each position, a failed `define`,
a locally rebound built-in, and `call/cc` among direct arguments.

## The R7RS environment model these techniques must fit

What full R7RS environments (phase 9) require, independent of how they are
implemented:

1. **Each library has its own top level.** A library's definitions are
   visible inside it and, if exported, to importers under possibly renamed
   names.
2. **Imports share bindings, not values.** An importer's name and the
   library's name denote one variable, so the library's later assignments
   are visible (see the phase 9 discussion in todo.md).
3. **Top-level definitions are late-bound.** At the REPL, a procedure
   defined before `g` exists may call `g`, and redefining `g` must affect it.
4. **Environments are values.** `environment` and `interaction-environment`
   return environments that `eval` evaluates in.
5. **Imported bindings can't be assigned by the importer**, and
   `environment`'s bindings are immutable. Compiled code may rely on this.

The common thread is that **a top-level binding needs an identity separate
from its name and its value**: a cell. Names in a top-level environment map
to cells; importing maps a new name to an existing cell; `define` and `set!`
change a cell's contents; compiled code can hold a cell directly.

## Techniques

### A. Cache global bindings

Resolve a global reference once and remember the result.

* **Caching the value is wrong** under requirement 3: redefinition would be
  missed. (s1 already does it where it is safe: `core_form` embeds special
  forms, and quasiquote embeds `cons`/`append`, in code s1 itself generates.)
* **Caching the cell** is right and cheap to keep valid. It needs somewhere to
  put the cache, though. Source code is plain s-expressions, and a symbol
  object is shared by every occurrence of that name, so a per-occurrence
  cache would have to be a side table keyed by the form, and a hash probe per
  evaluation costs about what it saves. **A only pays off together with D**,
  where each reference is a node with its own slot.
* **Fits R7RS** if built on the same cells as phase 9.

Expected gain alone: none worth having. As part of D: most of the 3-15%
lookup cost, plus the alias double-resolution.

### B. Lexical addressing for local variables

Replace a local reference with (frames up, slot).

* Local lookups already average about one frame; the saving is small.
* It needs fixed frame layouts. s1 frames can grow at run time: a definition
  after an expression (allowed since phase 6), a `define` from a procedural
  macro's expansion, `eval`.
* **Neutral to R7RS** (local scope isn't affected by libraries).

Recommendation: skip, or fold into D later only if measurements justify it.

### C. Pre-expanding macros ahead of time

* Phase 5's expansion cache already expands each use once, lazily, keyed by
  the use form; ahead-of-time expansion would add nothing on its own.
* Its value is as the front half of D: a fully expanded body is what an
  analyser needs.
* The obstacle identified in Phase 5 remains: ahead-of-time expansion has to
  know how every special form binds names. D has to solve this anyway.
* **Fits R7RS** if expansion happens in the right environment (the library's,
  for library code).

### D. Pre-analysis into a node tree

Convert each top-level form, and each `lambda` body when its closure is
created, into a tree of pre-resolved nodes: constant, local reference, global
reference (holding a cell), `if`, application, `lambda`, sequence, and so on.
The CEK machine then runs nodes instead of re-reading s-expressions.

What it removes: per-step syntax dispatch (looking up `if` to find it is a
special form), argument-list conversions, the per-evaluation closure creation
of `let` (61K closures in the regression suite), global hash lookups (via A),
and alias re-resolution (aliases are resolved once, at analysis).

How it must be shaped to fit s1 and R7RS:

* **Analysis environment.** Analysis needs to know, for each identifier,
  whether it is local, global, a special form or a macro. Locals come from the
  enclosing `lambda`/`let` nodes being built. Globals are resolved through
  the analysing environment's cells: the REPL's, a library's, or the
  environment given to `eval`. An identifier not yet defined gets a fresh,
  unbound cell in that environment, so a later `define` fills it
  (requirement 3).
* **Special forms become analysers.** Each current special form handler
  turns into a function from syntax to nodes. This is the scoping knowledge
  Phase 5 avoided writing; here it is unavoidable, and it lives with the
  forms themselves rather than in a separate expander.
* **`syntax-rules` expands during analysis** (pure Rust, no Scheme code runs).
* **Procedural `macro` uses become lazy nodes.** Their expansion runs Scheme
  code, which analysis can't do synchronously, so the analyser emits a node
  that, the first time it runs, expands through the existing `MacroExpand`
  frame, analyses the result and replaces itself.
* **Redefining a macro doesn't affect code already analysed.** This is
  standard Scheme behaviour, but it is a change for s1, whose expansion cache
  currently notices a redefined macro.
* **Continuations, `dynamic-wind` and exceptions** keep working as now. Kont
  frames refer to nodes instead of s-expressions; `arg_stack` and the
  continuation snapshots are unchanged.
* **Debugging and errors.** Each node keeps its source form, so error messages,
  `trace` and `expand` can still show source.

Expected gain: this removes most of the remaining interpretive overhead, and
the inline-operands prototype already shows how much per-step overhead there
is. A realistic expectation is a further 1.5-2x on call-heavy code, but this
should be confirmed by a prototype before committing (see "Recommended
plan").

Cost: the largest change since the CEK rewrite. Every special form, `eval`,
`eval-string`, `expand`, macro handling and the trace/debug tooling are
touched.

### Other techniques considered

* **Flat continuation stack** (performance.md F10(3), designed in
  kont-flat-stack-design.md, deferred): complementary to all of the above.
  Continuation frames are about a third of malloc traffic, and D doesn't
  remove them.
* **Constant folding, inlining of builtins:** only safe for bindings that
  can't change, which R7RS libraries provide (requirement 5). That is a later
  refinement of D, not a separate technique.

## Compatibility summary

| Technique | Works with full R7RS environments? | Depends on |
|---|---|---|
| Inline trivial operands | Yes: purely an evaluation-order shortcut | nothing |
| A. cached global bindings | Yes, if cells are phase 9's cells | cells, D |
| B. lexical addressing | Neutral | D |
| C. ahead-of-time expansion | Yes, if expansion uses the defining environment | D |
| D. pre-analysis | Yes, if global references are cells resolved in the analysing environment | cells |

What would fight R7RS environments, and must be avoided:

* caching or inlining **values** of mutable top-level bindings;
* a single global "symbol -> value" table assumed by compiled code (libraries
  need one top level each);
* analysis that ignores the environment it is analysing for (`eval` with an
  environment, library bodies);
* copy-on-import, if compiled code is to share bindings: phase 9 should use
  cells, not the lightweight copying option discussed earlier.

## Recommended plan

1. **Inline trivial operands.** Done: 10-46% measured, including built-in
   calls on trivial arguments, `if` tests and `define`/`set!` values (see
   "Step 1 as implemented").
2. **Cells for top-level environments.** Done, ahead of phase 9: a
   top-level frame maps names to `BindingCell`s (`src/env.rs`); local frames
   are unchanged. `define` and `set!` change a cell's contents, `cell` gets a
   name's cell (creating an unbound one for a forward reference) and
   `bind_cell` makes a name denote an existing cell, which is what `import`
   will do. Cells are `Rc`s, like the frames that hold them, rather than heap
   objects as first proposed: nothing then has to thread the heap through
   `define`, a cell can never leak into Scheme data, and the values they hold
   are marked through the frames (pre-analysed nodes holding cells will mark
   them too). Measured speed-neutral. Phase 9's `import` now binds names to
   cells (tests in `scheme/library_tests.scm`).
3. **Prototype D on a subset**: constants, local and global references,
   `if`, application, `lambda`, `begin`, with everything else falling back to
   the current evaluator (a node that evaluates its source form the old way).
   Measure on the four workloads. Proceed only if the gain over step 1 is
   large (say, at least 1.5x on fib and the typical program).
4. If it pays, **convert the remaining special forms**, procedural macros as
   lazy nodes, and retire the s-expression evaluator path.

## After phase 11: cheaper steps before D

A review of the evaluator after phase 11 found per-evaluation work that
doesn't need D to remove. Each change was measured separately on this
machine (linux/x86-64), as medians of 7-11 interleaved runs, in seconds.
`fib` here is `bench/fib.scm` (fib 31), and "let loop" is a procedure called
100,000 times whose body has a `let`, an internal `define` and a named
`let`.

1. **Direct application from the argument stack.** Closures and built-ins
   are applied straight from `arg_stack` (`apply_from_stack` in
   `src/eval/cek.rs`). This saves an `ApplyProc` frame and an `Rc<Vec>` copy
   of the arguments on every call. `bind_params` no longer binds a junk entry
   for the empty rest slot of a fixed-arity procedure. Also removed: a
   parameter `HashMap` built and thrown away on every closure creation.
2. **A wider `immediate`.** It now covers quoted data (`'()`, `'sym`) and
   built-in calls nested up to two deep (`(car (cdr x))`). Before any
   built-in in a nested expression runs, every remaining argument is
   checked, so falling back to the machine never repeats a call. When a
   call's operator is already resolved, `eval_cek` evaluates the arguments at
   once (`eval_args`). A call whose arguments all qualify, like `(fib (- n
   1))`, needs no `EvalArg` frame at all.
3. **Cached rewrites.** `lambda`, `let`, named `let`, `let*`, `letrec`,
   `do` and `guard` used to rebuild their rewrites, closure templates and
   internal-definition transforms on every evaluation; a named `let` built
   about a dozen forms and two closures each time a loop was entered. From a
   form's second evaluation on, its rewrite is cached (`rewrite_once` in
   `src/special_forms.rs`), in the `syntax-rules` expansion cache, keyed by
   the form and the special form. A cached `lambda` or `let` holds a
   procedure *template*, which `instantiate` copies with the current
   environment. A plain `let` then applies that copy to its inits directly,
   without building a call form.

| | fib | typical | macro loop | regression | let loop |
|---|---|---|---|---|---|
| before | 2.47 | 0.44 | 0.28 | 1.60 | 1.06 |
| 1. direct application | -11% | -17% | ~0 | ~0 | |
| 2. wider `immediate` | -3% | -19% | ~0 | -2% | -27% (1 and 2) |
| 3. cached rewrites | ~0 | ~0 | -4% | +4% | -22% |
| after | **2.08 (-16%)** | **0.29 (-34%)** | **0.26 (-7%)** | **1.53 (-4%)** | **0.62 (-42%)** |

What didn't pay:

* **`immediate` for `cond` tests, `and`/`or` operands and non-final `begin`
  forms** made no measurable difference, even on a microbenchmark written
  for it, once step 2's operator fast path had made those evaluations cheap.
  It was reverted.
* **Caching from the first evaluation** made the regression suite 14%
  slower. That suite is mostly code that runs once, and much of it runs at
  `gc-threshold 1`. `guard`'s rewrite builds about eight fresh `lambda` forms
  each time, every one of which became a single-use cache entry. Each
  collection also marks the cached templates and rewrites. Caching only from
  the second evaluation (`GcHeap::first_sight`) keeps the whole gain on
  repeated code and cuts the suite's cost to the +4% in the table.
* **Revisiting only untraced ephemeron entries** during marking changed
  nothing measurable.

Behaviour changes, all deliberate:

* A rewrite is cached per form, like a `syntax-rules` expansion. A `guard`
  form's rewrite holds the top-level `call/cc`, `apply`, ... it found on its
  second evaluation, not on each evaluation. That only differs if a program
  redefines one of those at top level between evaluations of the same
  `guard` form.
* `trace` shows fewer steps.

Regression tests for these paths are in `scheme/advanced_tests.scm`: nested
calls with side effects, quoted arguments, a rebound `quote`, a fresh
closure per evaluation, `call/cc` in a `let` init, a redefined macro inside
a cached named `let`, and repeated `guard`, `let*`, `letrec` and `do`.

**Where D stands.** These steps remove part of what D was expected to
remove: per-evaluation closure creation and rewrites, and per-step
overhead for simple operands. What D would still remove: special-form
dispatch by lookup, alias re-resolution in macro output, global hash lookups,
and `EvalArg` frames for arguments that are calls to closures. The 1.5x gate
for the step 3 prototype is now measured against these faster numbers. The
largest remaining cost that D doesn't touch is the continuation chain:
`RestoreEnv` and `EvalArg` frames, and a GC that walks the whole chain, which
makes deep recursion quadratic. That is F10(3) (kont-flat-stack-design.md).

## Risks

| Risk | Consequence | Mitigation |
|---|---|---|
| Inline operands bypass a check the normal path makes | A wrong value or a missed error | It only handles constants and successful plain lookups; aliases and failures take the normal path. The regression suite (including call/cc re-entry and GC stress) already passes on the prototype. |
| Cells change top-level semantics subtly (REPL redefinition, `set!` of an import) | Behaviour differences at the REPL | Specify the REPL rules in the phase 9 design; tests for redefinition, forward references and imports |
| D's analysers diverge from the current special forms' behaviour | Programs behave differently once compiled | Build D behind a switch; run the regression and conformance suites under both evaluators until the old one is retired |
| Macro redefinition no longer affects analysed code | A behaviour change some s1 code may notice | Document it; it matches other Schemes |
| D's gain is smaller than expected | Large effort for little | The subset prototype in step 3 is the gate |

## Appendix: the "typical program" workload

```scheme
(define-record-type employee (make-employee name dept salary) employee?
  (name emp-name) (dept emp-dept) (salary emp-salary set-emp-salary!))
(define (make-staff n)
  (let loop ((i 0) (acc '()))
    (if (= i n) acc
        (loop (+ i 1)
              (cons (make-employee (string-append "e" (number->string i))
                                   (vector-ref #(sales eng ops) (modulo i 3))
                                   (+ 1000 (* 7 (modulo (* i 37) 101))))
                    acc)))))
(define (insert-sorted x lst less?)
  (cond ((null? lst) (list x))
        ((less? x (car lst)) (cons x lst))
        (else (cons (car lst) (insert-sorted x (cdr lst) less?)))))
(define (sort lst less?)
  (let loop ((rest lst) (acc '()))
    (if (null? rest) acc (loop (cdr rest) (insert-sorted (car rest) acc less?)))))
(define (summarise staff)
  (define (total-for dept)
    (let loop ((s staff) (sum 0))
      (cond ((null? s) sum)
            ((eq? (emp-dept (car s)) dept) (loop (cdr s) (+ sum (emp-salary (car s)))))
            (else (loop (cdr s) sum)))))
  (map (lambda (d) (cons d (total-for d))) '(sales eng ops)))
(define (raise-all! staff pct)
  (for-each (lambda (e) (set-emp-salary! e (+ (emp-salary e) (quotient (* pct (emp-salary e)) 100)))) staff))
(define (report staff)
  (let ((out (open-output-string)))
    (for-each (lambda (e) (write-string (emp-name e) out) (write-char #\space out)) staff)
    (string-length (get-output-string out))))
(define (run k)
  (let ((staff (make-staff 300)))
    (do ((i 0 (+ i 1))) ((= i k))
      (raise-all! staff 1)
      (summarise staff)
      (report (sort staff (lambda (a b) (< (emp-salary a) (emp-salary b))))))))
(run 6)
```
