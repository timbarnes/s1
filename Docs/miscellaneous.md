[Home](s1-docs.md)

# Miscellaneous

## `help`

`(help 'symbol)`

Returns the doc string for the given symbol as a Scheme string.

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