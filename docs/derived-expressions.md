# Derived Expression Types

R7RS section 4.2's derived forms. Most are hygienic `syntax-rules` macros defined in `scheme/s1-core.scm`, following R7RS's own reference definitions (section 7.3); `case-lambda` is built in. See also [Control Features](./control-features.md) for `values` and `dynamic-wind`, and [Exceptions](./exceptions.md) for `guard`.

## Conditionals

### `when`, `unless`

`(when test expr1 expr2 ...)` evaluates the expressions in order if `test` is true; `(unless test expr1 expr2 ...)` if it is false. The result is that of the last expression, or unspecified if they don't run.

### `case`

`(case key clause ...)`

Evaluates `key` once and compares it with `eqv?` against the data in each clause `((datum ...) expr ...)`. The first matching clause's expressions are evaluated. An `else` clause matches anything. A clause of the form `((datum ...) => proc)` or `(else => proc)` calls `proc` with the key.

```scheme
(case (car '(c d))
  ((a e i o u) 'vowel)
  ((w y) 'semivowel)
  (else => (lambda (x) x)))          ; => c
```

## Binding constructs

### `letrec*`

`(letrec* ((var init) ...) body ...)`: like `letrec`, with the inits evaluated left to right, each able to use the variables bound before it. (s1's `letrec` already behaves this way.)

### `let-values`, `let*-values`

`(let-values ((formals init) ...) body ...)`

Each `init` returns as many values as `formals` describes, and they are bound to its variables. `formals` is a list `(a b)`, a dotted list `(a . rest)` or a single variable that receives all values as a list. `let-values` evaluates all the inits before binding anything; `let*-values` binds each in turn, so later inits see earlier variables.

```scheme
(let*-values (((root rem) (exact-integer-sqrt 32))) (* root rem))   ; => 35
```

### `define-values`

`(define-values formals expr)`

Defines the variables in `formals` (as for `let-values`) from the values of `expr`, at top level or in a body. Other definitions may follow it in the same body.

## Procedures

### `case-lambda`

`(case-lambda (formals body ...) ...)`

A procedure that, when called, runs the first clause whose `formals` accept that many arguments. Calling it with a number of arguments no clause accepts is an error.

```scheme
(define range
  (case-lambda
    ((e) (range 0 e))
    ((b e) (do ((r '() (cons e r)) (e (- e 1) (- e 1))) ((< e b) r)))))
(range 3)      ; => (0 1 2)
(range 3 5)    ; => (3 4)
```

## Parameters

### `make-parameter`

`(make-parameter value [converter])`

Returns a parameter object: a procedure that, called with no arguments, returns its current value. With a `converter`, the initial value and every value given by `parameterize` are passed through it first (it can check or normalise them).

### `parameterize`

`(parameterize ((param value) ...) body ...)`

Evaluates the body with each parameter set to its (converted) value, restoring the previous values when control leaves the body by any means: a normal return, an error, or a continuation. Re-entering through a continuation reinstates the parameterized values.

```scheme
(define radix (make-parameter 10))
(define (f n) (number->string n (radix)))
(f 12)                            ; => "12"
(parameterize ((radix 2)) (f 12)) ; => "1100"
```

## Promises

### `delay`, `delay-force`, `force`

`(delay expr)` returns a promise to evaluate `expr`. `(force promise)` evaluates it the first time and returns the same value every time after. Forcing a non-promise returns it unchanged.

`(delay-force expr)`, where `expr` evaluates to a promise, is for iterative lazy algorithms: forcing a long chain of `delay-force` promises runs in constant space.

### `make-promise`, `promise?`

`(make-promise obj)` returns `obj` if it is a promise, otherwise a promise already forced to `obj`. `(promise? obj)` tests for promises.

A promise is currently represented as a tagged vector, so `vector?` is also true of one. That will change when records are added.
