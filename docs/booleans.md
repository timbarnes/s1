# Booleans

`#t` / `#true` and `#f` / `#false`. Only `#f` counts as false in conditionals.

* `(boolean? obj)`.
* `(not obj)`: `#t` if `obj` is `#f`, otherwise `#f`.
* `(boolean=? b1 b2 b3 ...)`: `#t` if all are the same boolean.
