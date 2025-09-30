[Home](s1-docs.md)

# Control Features

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