# Phase 5: Hygienic Macros (`syntax-rules`)

Designed 2026-10-02, not yet implemented. Written before touching code, per
this project's practice (see kont-flat-stack-design.md, nested-evaluation.md).

## Goal

R7RS section 4.3: `define-syntax`, `let-syntax`, `letrec-syntax` and
`syntax-rules`, with hygiene. Concretely, the 27 tests in section 4.3 of the
conformance suite must pass. Between them they require:

1. **A macro's free identifiers mean what they meant where the macro was
   defined**, whatever the use site binds. `(let ((if #t)) (when if ...))`
   must still use the real `if` inside `when`'s expansion, and a template's
   `x` must find the `x` around the `let-syntax`, not an inner `x` at the use
   site.
2. **Identifiers a macro introduces can't capture the user's.** `my-or`'s
   `(let ((temp e1)) ...)` must not capture a `temp` in the user's arguments.
3. **Macro-defining macros**, including `(... ...)` escapes, custom ellipsis
   identifiers (`(syntax-rules dots () ...)`), and a template whose `...` is
   produced by an escape in an outer template.
4. **Literal matching by binding.** `(let ((=> #f)) (cond (#t => 'ok)))` is
   `ok`: a locally bound `=>` is a variable, not the keyword. In patterns, an
   identifier is a literal only if it *is* one of the listed literals
   (`bound-identifier=?`), so a renamed `k` and the user's `k` differ.
5. **Introduced definitions.** A macro that expands to
   `(begin (define march-hare 42) (define-syntax hatter ...))` must leave
   `hatter`'s template able to find `march-hare`.
6. **Forward references.** A macro defined in a body may expand into a call
   to a procedure defined later in the same body.
7. **Full pattern language**: ellipsis anywhere in a list (`(a b (m n) ... x y)`),
   dotted tails after an ellipsis, vectors, `_`, literals (including `_` and
   `...` as literals, which take priority), nested ellipsis depth.

## How s1 evaluates code today

There is no expansion pass. The CEK machine interprets s-expressions
directly, and special forms are ordinary values in the environment: when
`eval_cek` meets `(op ...)`, it looks `op` up, and if the value is a
`Callable::SpecialForm` it calls the Rust handler with the unevaluated form.
Many handlers work by rewriting the form into others and evaluating the
result: named `let` becomes `letrec` plus `lambda`, internal definitions become
`letrec`, `quasiquote` becomes calls to `cons`/`append`, and `guard` (phase 4)
becomes R7RS's reference expansion. Those rewrites name `lambda` and `letrec`
by interned symbol, so a user's local `letrec` would break a named `let`.

s1's existing `macro` form is the same idea one level up: a `Callable::Macro`
in the environment whose Scheme body runs at each use, producing code that is
then evaluated. It is non-hygienic and stays as an s1 extension.

Environments map symbol objects (`GcRef`s) to values. Lookup is by pointer,
so two distinct symbol objects with the same name are different variables.
Phase 4 already relies on this: `guard`'s temporaries are fresh uninterned
symbols, and `lambda` keeps parameter symbols as written.

## The choice: an expansion pass, or runtime expansion with renaming?

The plan proposed a separate expansion pass over each top-level form. Having
looked at what it would take, this design does **not** do that.

A hygienic pre-expander must know the binding structure of every special form
it walks through. To expand `(let ((if 1)) (m))` correctly it must know that
`let` binds `if`. s1 has about twenty special forms implemented as Rust
rewrites, each of which would need a scoping description kept in step with its
implementation. The procedural `macro` form, which runs Scheme code to
compute its expansion, can't be expanded ahead of evaluation at all. A
pre-expander would effectively be a second evaluator front end.

The alternative fits what s1 already is. **`syntax-rules` transformers are
values in the environment, like special forms and `macro`s, and a use is
expanded when the evaluator meets it.** Hygiene comes from **renaming**: every
identifier the template introduces is replaced by a fresh *alias* that
remembers the original identifier and the environment where the macro was
defined. Looking up an alias first tries the current environment (where only
a binding the expansion itself made can have that alias as its key), and
otherwise looks the original up in the definition environment. This is the
syntactic-closure / explicit-renaming idea, resolved lazily at lookup time,
which a runtime-expanding interpreter can do because it has both environments
in hand when the expansion runs.

Every special form keeps working unchanged: an alias *is* a symbol (see
below), so `let`, `lambda`, `define`, `do` and the rest bind it like any other
name, and their keyword checks by name (`else`, `lambda`, `define`) still
recognise a renamed keyword.

## Design

### 1. Aliases

An alias is a **fresh uninterned symbol object with the same name as the
identifier it renames**, plus an entry in an alias table on the heap:

```text
alias table: alias GcRef  ->  { original: GcRef, env: EnvRef }
```

`original` is the template identifier it renames (an interned symbol, or
itself an alias when a macro-generated macro expands), and `env` is the
transformer's definition environment.

Using a real `SchemeValue::Symbol` rather than a new value type means the 60
or so places that match `SchemeValue::Symbol` (binding names in `lambda`,
`let`, `let*`, `letrec`, `do`, named `let`, `define`, `set!`, `guard`, keyword
checks via `matches_sym`) handle aliases without change. The cost is a side
table that the GC must understand (section 6).

Operations, in a new `src/eval/identifiers.rs`:

* `resolve(id, env) -> Option<(value, frame, key)>`: `env.lookup(id)`; if
  that fails and `id` is an alias, `resolve(original, alias_env)`. Recursion
  follows alias chains from macro-generated macros.
* `strip(id) -> symbol`: follow `original` links to the interned symbol
  (`syntax->datum` for one identifier). `strip_datum` copies a datum,
  replacing aliases with `strip` of them, and returns the input unchanged
  (no allocation) when it contains none.
* `free_identifier_eq(a, env_a, b, env_b)`: both resolve to the same binding
  (same frame and key), or both are unbound and `strip` to the same name.

### 2. Where the evaluator resolves identifiers

* **Variable references** (`eval_cek`, symbol case): `env.lookup` first,
  exactly as now. Only on failure, and only if the symbol is an alias, fall
  back to `resolve`. Ordinary code pays nothing.
* **Operator position** (the fast special-form check in `eval_cek`): same
  fallback, so a template's `if` finds the special form even when the use site
  binds `if`.
* **`set!`**: uses `resolve` to find the frame and key, and assigns there. A
  template's `(set! counter ...)` updates the definition environment's
  `counter`.
* **`define`**: binds the identifier as written in the current frame. A
  template-introduced name is bound under its alias, so it is visible to
  the expansion that introduced it (and to macros that expansion defines, by
  alias resolution) but not to user code. This includes the top level. See
  decision 1.

### 3. The transformer

`(syntax-rules [ellipsis] (literal ...) (pattern template) ...)` is a special
form that evaluates to a transformer object, a new boxed
`Callable::SyntaxRules { ellipsis, literals, rules, env }` capturing the
current environment as the definition environment. Its validity (well-formed
rules, ellipsis placement, pattern variables used at consistent depths) is
checked when the `syntax-rules` form is evaluated.

The matcher and template instantiation are plain Rust in a new
`src/syntax_rules.rs`, with no evaluation and no CEK frames involved:

* **Matching** follows R7RS 4.3.2. The keyword position is ignored; `_`
  matches anything; an identifier is a literal if it is pointer-identical to
  an entry in the literal list, and then matches an input identifier by
  `free_identifier_eq` (literal resolved in the definition environment, input
  in the use environment); other identifiers are pattern variables. A list
  pattern may have one ellipsis with any number of patterns after it and an
  optional dotted tail; vectors likewise. Other data match by `equal?`.
  Bindings are a tree: `Single(GcRef)` or `Seq(Vec<Binding>)` per ellipsis
  level.
* **The ellipsis** is the custom identifier if one is given (compared by
  `bound-identifier=?`, i.e. pointer identity), otherwise any identifier that
  `strip`s to `...`. The latter is what lets a `...` produced by an outer
  `(... ...)` escape (an alias of `...`) act as the inner macro's ellipsis.
  Literals take priority over the ellipsis.
* **Instantiation** substitutes pattern variables, repeating `sub ...`
  subtemplates over their bindings (`x ... ...` flattens), handles
  `(... template)` escapes, and renames every other identifier: one fresh
  alias per distinct template identifier per expansion, so all occurrences of
  `temp` in one expansion are the same alias.
* **Literal data is stripped** in the result: the datum of any `quote` form
  whose keyword resolves to the `quote` special form, the quoted parts of
  `quasiquote` forms (not their unquoted parts), and vector literals. So
  `'(... ...)` yields the symbol `...` and `'#(b)` yields `#(b)`, not
  aliases. This is done once per expansion, so the `quote` special form
  itself is unchanged and costs nothing extra.

### 4. Applying a transformer

When the operator of `(m arg ...)` resolves to a `SyntaxRules` value:

* In the `eval_cek` fast path (operator is an identifier, the common case):
  expand and evaluate the result with `insert_eval(state, expansion,
  state.tail)`, preserving tail position, which recursive macros such as
  `my-or` rely on.
* Via `Kont::ApplySpecial` (an operator expression that evaluates to a
  transformer, rare): same, without tail position.

A use that matches no rule raises an error object ("no syntax rule matches"
plus the form), which `guard` can catch.

### 5. The definition forms

* `(define-syntax name transformer)`: `define` that requires the value to be a
  transformer (`syntax-rules` or a `macro`). Internal `define-syntax` is
  treated like `define` by `transform_internal_defines`, so it becomes part of
  the body's `letrec` and its transformer closes over the body's frame.
* `(let-syntax ((name transformer) ...) body ...)`: transformers evaluated in
  the current environment, then bound in a new frame where the body runs as a
  lambda body (so definitions inside it stay local, as the suite requires).
* `(letrec-syntax ...)`: the same, with the transformers evaluated in the new
  frame so they can refer to each other.

Macro uses in a body that expand into definitions are evaluated as they are
met and `define` binds into the current frame, so they behave as internal
definitions in practice (the suite's `(let () (foo bar x) (bar 1))` case),
though without full `letrec*` ordering guarantees.

### 6. Garbage collection

The alias table must keep an alias's `original` and `env` alive exactly as
long as the alias itself is alive, and forget dead aliases. That is an
ephemeron table:

* After the normal mark phase, repeat until nothing changes: for each entry
  whose alias is marked, mark `original` and `env`. (A newly marked `env` may
  reach further aliases, hence the loop.)
* Sweep removes entries whose alias was not marked, before the alias objects
  themselves are freed.

The new `SyntaxRules` callable marks its literals, rules and environment
like `Closure` does. Expansion allocates but runs no Scheme code, and s1 only
collects at known safe points, so nothing in flight needs extra rooting.

### 7. Expansion cache (measured, then kept or dropped)

Runtime expansion re-expands a use every time it is evaluated, so a macro in a
loop body pays for matching and allocation on each iteration. An expansion
cache keyed by the use's form (its pair) and the transformer object
(`(form, transformer) -> expansion`) avoids that. It is the same ephemeron
shape as the alias table (an entry lives as long as the form), so it reuses
that machinery.

The cache is only correct if expanding the same form with the same transformer
always gives an equivalent result. That holds except when a literal's binding
at the use site changes between evaluations, which needs something like
`(define else ...)` in between. Reusing alias objects across evaluations is
fine: each evaluation still creates fresh frames.

The cache is built after correctness and kept only if `bench/` shows a
measurable gain on a macro-heavy loop.

### 8. Tightening existing rewrites

With aliases available, the special forms that rewrite into other forms
(named `let`, internal definitions, `guard`, `quasiquote`'s already
object-embedded procedures excepted) will name `lambda`, `letrec`, `let`,
`cond` and `else` with aliases whose environment is the global one, instead
of interned symbols. That closes phase 4's documented `guard` limitation and
the same hole in named `let`.

`cond`'s `else` and `=>` checks will also require the identifier to be unbound
at the use site (`resolve` finds nothing), which is what makes
`(let ((=> #f)) (cond (#t => 'ok)))` evaluate to `ok`.

## What does not change

* The `macro` form and its runtime expansion path (`Kont::MacroExpand`).
* `eval_cek`'s behaviour and cost for code that uses no `syntax-rules` macros:
  the alias fallback runs only after a failed lookup.
* The `quote` special form (stripping happens at expansion time).
* Continuations, `dynamic-wind`, exceptions.

## Known limitations

* **Errors in a macro use surface when the use is evaluated**, not when the
  surrounding code is defined. A malformed use in a branch that never runs is
  never reported. (Errors in the `syntax-rules` form itself are reported when
  it is evaluated.)
* **Debug output shows aliases by name**, so `(expand ...)` and trace output
  can show two different `temp`s that print the same.
* **Keywords that special forms check by name** (`else`, `=>`, `define` in a
  body) still match any identifier with that name that is unbound at the use
  site, which is R7RS's `free-identifier=?` for unbound keywords. A user can't
  rebind them globally and expect the special forms to notice.
* **Introduced definitions are hidden** (decision 1). A macro meant to define a
  name chosen by the macro writer, not passed in by the user, won't make that
  name visible.

## Risk register

| Risk | Consequence | Mitigation |
|---|---|---|
| Wrong resolution in an edge case (alias chains, `set!` through an alias, literal comparison) | Wrong variable used, silently | Every behaviour in "Goal" has a dedicated test; the 4.3 suite covers the classic hygiene cases |
| Alias-table ephemeron marking wrong | An alias outlives its `env` or `original`: use-after-free | Unit tests on the table; the new suite re-run at `gc-threshold 1` in `gc_stress_tests.scm`; a test that collects between defining and using a macro |
| Re-expansion cost | Slower loops using macros | Expansion cache (section 7), measured with a macro-heavy benchmark |
| Ellipsis detection by name (`...` via `strip`) | A user identifier named `...` in an unusual position | Literal priority and custom ellipses behave per R7RS; covered by the suite's `elli-lit-1` and `be-like-begin` cases |
| Interaction with `macro` | Aliases inside a `macro` expansion | `macro` output is evaluated in the use environment, where aliases resolve normally; tested |

## Test plan

* **Rust unit tests** in `src/syntax_rules.rs`: pattern matching
  (middle ellipsis, dotted tails, vectors, `_`, literals, custom ellipsis,
  nested depth, `equal?` data), instantiation (escapes, flattening, renaming
  identity within one expansion), and validation errors.
* **`scheme/syntax_rules_tests.scm`** (about 50 tests), in the regression suite and
  re-run under `gc-threshold 1`: the hygiene cases in "Goal", `set!` through
  an alias, recursive and mutually recursive macros, macro-defining macros,
  tail calls through a recursive macro (a 100,000-iteration loop must not
  grow the stack), macros in internal definitions, a use that matches no rule
  caught by `guard`, and coexistence with `macro`.
* **The conformance suite**: all 27 tests in 4.3 Macros use only what this
  phase provides, so all 27 should pass.
