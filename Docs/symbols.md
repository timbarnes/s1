[Home](s1-docs.md)

# Symbols

## `symbol?`

`(symbol? obj)`

Returns `#t` if `obj` is a symbol, and `#f` otherwise. Implemented via `type-of`.

## `symbol->string`

`(symbol->string symbol)`

Returns the name of `symbol` as a string. Implemented in `s1-core.scm`.

## `string->symbol`

`(string->symbol string)`

Returns the symbol whose name is `string`. Implemented in `s1-core.scm`.

[Home](s1-docs.md)