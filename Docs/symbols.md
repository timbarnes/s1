[Home](s1-docs.md)

# Symbols

Symbols are case sensitive. A symbol whose name wouldn't read back as a symbol is written with bars: `|hello world|`.

* `(symbol? obj)`.
* `(symbol=? s1 s2 s3 ...)`: `#t` if all are the same symbol.
* `(symbol->string sym)`: a new string holding the name.
* `(string->symbol s)`: the symbol with that name.

s1 reads a bare `nil` as the empty list, an extension; `|nil|` is the symbol.

[Home](s1-docs.md)
