[Home](s1-docs.md)

# Vectors

## `vector?`

`(vector? obj)`

Returns `#t` if `obj` is a vector, and `#f` otherwise. Implemented via `type-of`.

## `make-vector`

`(make-vector k [fill])`

Returns a newly allocated vector of `k` elements. If a second argument is given, then each element is initialized to `fill`.

## `vector`

`(vector obj ...)`

Returns a newly allocated vector whose elements are the given arguments.

## `vector-length`

`(vector-length vector)`

Returns the number of elements in `vector` as an exact integer.

## `vector-ref`

`(vector-ref vector k)`

Returns the `k`th element of `vector`.

## `vector-set!`

`(vector-set! vector k obj)`

Stores `obj` in element `k` of `vector`.

## `vector->list`

`(vector->list vector)`

Returns a newly allocated list of the objects contained in the elements of `vector`.

## `list->vector`

`(list->vector list)`

Returns a newly created vector initialized to the elements of `list`.

## `vector-fill!`

`(vector-fill! vector fill)`

Stores `fill` in every element of `vector`.

[Home](s1-docs.md)