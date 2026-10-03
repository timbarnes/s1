[Home](s1-docs.md)

# Miscellaneous

## `help`

`(help 'symbol)`

Returns the doc string for the given symbol as a Scheme string.

## Printing procedures

R7RS gives procedures no external representation, so `write` and `display` show them as opaque objects:

* `#<procedure car>` for a built-in procedure, and `#<syntax if>` for a special form.
* `#<procedure f>` for a closure (or `case-lambda` procedure) bound by `define`, `set!`, `letrec`, named `let` or an internal definition. A procedure takes the first name it is bound to and keeps it: after `(define (adder n) (lambda (x) (+ x n)))` and `(define add1 (adder 1))`, `add1` prints as `#<procedure add1>`. One that was never bound that way prints as `#<procedure>`.
* `#<macro m>` for a `macro` procedure, `#<syntax-rules>` for a `syntax-rules` transformer, and `#<continuation>`.
* Ports print by kind: `#<input-port string>`, `#<output-port stdout>`, `#<output-port "out.txt">`, `#<binary-input-port bytevector>`, `#<closed-port>`.

## `procedure-source`

`(procedure-source proc)`

Returns the form `proc` was made from, as data: `(lambda formals body ...)` for a closure (also when it was written as `(define (f ...) ...)`), `(macro ...)` for a `macro` procedure, and `(case-lambda clause ...)` for a `case-lambda`. Docstrings and internal definitions appear as written. Identifiers renamed by `syntax-rules` expansion show their original names, so the result is for reading, not for evaluating. Returns `#f` for a built-in procedure.

```scheme
(define (f x) (+ x 1))
f                       ; => #<procedure f>
(procedure-source f)    ; => (lambda (x) (+ x 1))
```

## `void`

`(void)`

Returns the void value.

## `error`

`(error message irritant ...)`

Raises an error object with the given message and irritants. See [Exceptions](./exceptions.md).

## `closure?`

`(closure? obj)`

Returns `#t` if `obj` is a closure, and `#f` otherwise. Implemented via `type-of`.

## `macro?`

`(macro? obj)`

Returns `#t` if `obj` is a macro made with s1's `macro` form, and `#f` otherwise (including for `syntax-rules` transformers, whose `type-of` is `syntax-rules`). Implemented via `type-of`. See [Macros](./macros.md).

## `procedure?`

`(procedure? obj)`

Returns `#t` if `obj` is a procedure, and `#f` otherwise. Implemented in `s1-core.scm`.

[Home](s1-docs.md)