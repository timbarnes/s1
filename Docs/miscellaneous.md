[Home](s1-docs.md)

# Miscellaneous

## `help`

`(help 'symbol)`

Returns the doc string for the given symbol as a Scheme string.

## `void`

`(void)`

Returns the void value.

## `error`

`(error reason ...)`

Signals an error. The `reason` and subsequent arguments are displayed in an error message.

## `closure?`

`(closure? obj)`

Returns `#t` if `obj` is a closure, and `#f` otherwise. Implemented via `type-of`.

## `macro?`

`(macro? obj)`

Returns `#t` if `obj` is a macro, and `#f` otherwise. Implemented via `type-of`.

## `procedure?`

`(procedure? obj)`

Returns `#t` if `obj` is a procedure, and `#f` otherwise. Implemented in `s1-core.scm`.

[Home](s1-docs.md)