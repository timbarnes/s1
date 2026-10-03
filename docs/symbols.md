# Symbols

Symbols are case sensitive. A symbol whose name wouldn't read back as a symbol is written with bars: `|hello world|`.

* `(symbol? obj)`.
* `(symbol=? s1 s2 s3 ...)`: `#t` if all are the same symbol.
* `(symbol->string sym)`: a new string holding the name.
* `(string->symbol s)`: the symbol with that name.

`nil` is an ordinary symbol. s1's core library also defines a variable `nil` bound to the empty list, so `nil` used as a value means `()`; `'nil` is the symbol.
