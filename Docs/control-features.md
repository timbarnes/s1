[Home](s1-docs.md)

# Control Features

## Proper tail calls

s1 is properly tail-recursive (R7RS 3.5): a procedure call in tail position reuses the caller's continuation, so a loop written as a tail call runs in constant space however many times it repeats. The tail positions are the last expression of a `lambda` or `case-lambda` body; the branches of `if`, `cond` (including `=>`), `case`, `when` and `unless`; the last expression of `and`, `or` and `begin`; the bodies of every `let` form, named `let`, `let-values`, `let-syntax` and `letrec-syntax`; a `do` loop's result expressions; and macro uses in any of these. `apply` and `call/cc` call their procedure, and `call-with-values` its consumer, in tail position.

Calls inside `guard`, `parameterize`, `dynamic-wind` and `with-exception-handler` bodies aren't tail calls, since those forms must do something after the body returns. Neither is `eval`.

`(%kont-depth)`, an internal procedure, returns the number of continuation frames waiting for its value; the regression suite's `scheme/tail_tests.scm` uses it to check every tail context.

## `procedure?`

`(procedure? obj)`

Returns `#t` if `obj` is a procedure, and `#f` otherwise. Implemented via `type-of`.

## `apply`

`(apply proc arg1 ... args)`

Calls `proc` with the elements of the list `(append (list arg1 ...) args)` as the actual arguments.

## `map`

`(map proc list1 list2 ...)`

The `map` procedure applies `proc` element-wise to the elements of the `list`s and returns a list of the results, in order. Implemented in `s1-core.scm`.

## `for-each`

`(for-each proc list1 list2 ...)`

Similar to `map`, but `for-each` is called for its side effects rather than for its values. Implemented in `s1-core.scm`.

## `force`

`(force promise)`

Forces the value of `promise`.

## `call-with-current-continuation`

`(call-with-current-continuation proc)`

`proc` must be a procedure of one argument. The procedure `call-with-current-continuation` packages up the current continuation (see the following section) as an "escape procedure" and passes it as an argument to `proc`.

## `values`

`(values obj ...)`

Delivers all of its arguments to its continuation.

## `call-with-values`

`(call-with-values producer consumer)`

Calls its `producer` argument with no arguments and a continuation that, when passed some values, calls the `consumer` procedure with those values as arguments.

## `dynamic-wind`

`(dynamic-wind before thunk after)`

Calls `thunk` without arguments, returning the result(s) of this call. `before` and `after` are called just before and just after `thunk` is called.

[Home](s1-docs.md)