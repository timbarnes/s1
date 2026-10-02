[Home](s1-docs.md)

# Equivalence Predicates

## `eq?`

`(eq? obj1 obj2)`

The `eq?` procedure returns `#t` if `obj1` and `obj2` are the same object, otherwise it returns `#f`.

## `eqv?`

`(eqv? obj1 obj2)`

The `eqv?` procedure is similar to `eq?`, but it is more discerning. It returns `#t` if `obj1` and `obj2` are the same object, or if they are equivalent values of certain types (booleans, characters, and numbers). Numbers are `eqv?` only when they have the same exactness and value, so `(eqv? 2 2.0)` and `(eqv? 0.0 -0.0)` are `#f`.

## `equal?`

`(equal? obj1 obj2)`

`The `equal?` procedure returns `#t` if `obj1` and `obj2` print the same. In other words, it recursively compares the contents of pairs, vectors, and strings, and returns `#t` if the contents are the same.

[Home](s1-docs.md)
