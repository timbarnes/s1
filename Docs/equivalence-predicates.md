[Home](s1-docs.md)

# Equivalence Predicates

## `eq?`

`(eq? obj1 obj2)`

The `eq?` procedure returns `#t` if `obj1` and `obj2` are the same object, otherwise it returns `#f`.

## `eqv?`

`(eqv? obj1 obj2)`

The `eqv?` procedure is similar to `eq?`, but it is more discerning. In S1 Scheme, `eqv?` is currently implemented using the same logic as `eq?`. It returns `#t` if `obj1` and `obj2` are the same object, otherwise it returns `#f`.

## `equal?`

`(equal? obj1 obj2)`

`The `equal?` procedure returns `#t` if `obj1` and `obj2` print the same. In other words, it recursively compares the contents of pairs, vectors, and strings, and returns `#t` if the contents are the same.

[Home](s1-docs.md)
