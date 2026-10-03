# Macros

R7RS: [section 4.3, Macros](https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-6.html#TAG:__tex2page_sec_4.3). Syntax definitions are in [section 5.4](https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-7.html#TAG:__tex2page_sec_5.4).

S1 has two macro systems:

* **`syntax-rules`**: the standard system, pattern-based and **hygienic**. Use it for new code.
* **`macro`**, an s1 extension: the macro body is ordinary Scheme code that computes the expansion, usually with quasiquote. It is **not** hygienic.

Both kinds of macro are values bound in the environment, like procedures. A use is expanded when the evaluator reaches it, and the expansion is evaluated in place.

## Defining macros

### `define-syntax`

`(define-syntax keyword transformer)`

Binds `keyword` to a transformer, normally a `syntax-rules` form. Works at top level and inside bodies (an internal `define-syntax` is part of the body's scope, like an internal `define`).

```scheme
(define-syntax swap!
  (syntax-rules ()
    ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))
```

### `let-syntax` and `letrec-syntax`

`(let-syntax ((keyword transformer) ...) body ...)`
`(letrec-syntax ((keyword transformer) ...) body ...)`

Bind macros for the extent of `body`, which runs in a new scope: definitions inside it are local. With `letrec-syntax` the transformers can refer to each other and to themselves.

## `syntax-rules`

`(syntax-rules (literal ...) (pattern template) ...)`
`(syntax-rules ellipsis (literal ...) (pattern template) ...)`

A use of the macro is matched against each pattern in turn. The first that matches is instantiated with its template. A use that matches no rule raises an error (`"no syntax rule matches ..."`), which `guard` can catch.

### Patterns

* The first element of a pattern stands for the keyword and is ignored.
* An identifier is a **pattern variable** and matches anything, unless it is `_` or a literal.
* `_` matches anything without binding it.
* A **literal** (an identifier listed in the literals) matches an identifier only if both refer to the same binding, or both are unbound and have the same name. So `else` matches `else`, but not an `else` that the use site has bound as a variable.
* `p ...` matches zero or more forms matching `p`. The ellipsis can appear anywhere in a list, followed by more patterns, and before a dotted tail: `(a b ... c d . rest)`.
* Vector patterns `#(p ...)` work like list patterns.
* Other data (numbers, strings, characters, booleans) match by `equal?`.

### Templates

* Pattern variables are replaced by what they matched. A variable matched under an ellipsis must be used under at least as many: `(list x ...)`. Several ellipses flatten: `(b ... ...)`.
* `(... template)` escapes the ellipsis: `(... ...)` is a literal `...`, and `'(... (x ...))` quotes the list `(x ...)` with `x` substituted. This is how a macro writes a macro that uses `...`.
* A custom ellipsis identifier, given before the literals, replaces `...` within that `syntax-rules` form: `(syntax-rules dots () ((_ x dots) (list x dots)))`. A literal takes priority over the ellipsis.

Malformed rules (an ellipsis with nothing before it, two ellipses in one list, a pattern variable used twice or under too few ellipses) are reported when the `syntax-rules` form is evaluated.

### Hygiene

Identifiers that a template introduces (not pattern variables) behave as if renamed:

* **They mean what they meant where the macro was defined.** A template's `if`, `list` or `counter` refers to the definition-site binding even if the use site binds its own. `set!` on such an identifier assigns the definition-site variable.
* **They can't capture the user's identifiers.** `swap!`'s `tmp` is a different variable from a `tmp` passed in by the user: `(let ((tmp 1) (y 2)) (swap! tmp y) (list tmp y))` gives `(2 1)`.
* **Definitions they introduce are hidden.** If a template expands to `(define helper ...)`, `helper` is visible to the code the same expansion produced, including macros it defines, but not to user code, even at top level. Names the user passes in (`(define-getter name ...)`) are defined normally.
* **Quoted data is plain.** Symbols inside a quoted part of a template, a quasiquoted part, or a vector literal are ordinary symbols: `'(a b)` in a template yields a list whose `a` is `eq?` to the user's `'a`.

The special forms that s1 implements by rewriting into other forms (named `let`, `do`, `let*`, `letrec`, internal definitions, `guard`, `quasiquote`, ...) are hygienic in the same way: binding `lambda`, `let`, `if` or `loop` locally can't affect them.

### Errors appear when a use is evaluated

Macros are expanded when the evaluator reaches them, so a malformed use in code that never runs is never reported. Errors in the `syntax-rules` form itself are reported when it is evaluated.

`(syntax-error message arg ...)` in a template reports a malformed use: it raises an error whose message is the string `message` and whose irritants are the `arg`s, unevaluated.

```scheme
(define-syntax must-be-pair
  (syntax-rules ()
    ((_ (a . b)) 'pair)
    ((_ x) (syntax-error "must-be-pair: not a pair" x))))
(must-be-pair oops)   ; error: must-be-pair: not a pair oops
```

### Auxiliary syntax

`else`, `=>`, `_` and `...` are bound in `(scheme base)`, so they can be exported, imported and renamed like other syntax: after `(import (rename (only (scheme base) else) (else otherwise)))`, `otherwise` works as `else` in `cond` and `case`. Using one as an expression, as in `(else 1)`, is an error. A local variable named `else` or `=>` is an ordinary variable, and `cond`, `case` and `guard` don't treat it as a keyword.

### Performance

Each use is expanded once and the expansion is cached, keyed by the use and the macro. A macro used inside a loop or a frequently called procedure costs little more than the equivalent hand-written code (about 1.3× in a tight loop). Redefining the macro invalidates the cache. The cache doesn't notice a literal such as `else` being rebound between two evaluations of the same code; that would need something like `(define else ...)` at top level.

## `macro` (s1 extension)

`(macro params body ...)`

Creates a non-hygienic macro. When a use is evaluated, `params` are bound to the **unevaluated** argument forms (as with `lambda`, a symbol or dotted list binds the rest), the body runs as ordinary Scheme code, and its result is evaluated in place of the use.

```scheme
(define my-unless
  (macro (test . body)
    `(if ,test #f (begin ,@body))))
```

Because the expansion is plain code, identifiers in it can capture or be captured by the user's. Choose unusual names for temporaries. The body can compute anything, which `syntax-rules` can't do. s1's core library uses `macro` for `def`, `push!` and `pop!`.

## Inspecting expansions

`(expand form)` evaluates `form` and, if the result is a macro use (of either kind), returns its expansion one level deep without evaluating it:

```scheme
(expand '(swap! x y))   ; => (let ((tmp x)) (set! x y) (set! y tmp))
```

In the output of a `syntax-rules` expansion, renamed identifiers print with their original names, so two different `tmp`s look the same.
