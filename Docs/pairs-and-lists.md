[Home](s1-docs.md)

# Pairs and Lists

## `pair?`

`(pair? obj)`

Returns `#t` if `obj` is a pair, and `#f` otherwise. Implemented via `type-of`.

## `cons`

`(cons obj1 obj2)`

Returns a newly allocated pair whose car is `obj1` and whose cdr is `obj2`.

## `car`

`(car pair)`

Returns the car of `pair`.

## `cdr`

`(cdr pair)`

Returns the cdr of `pair`.

## `set-car!`

`(set-car! pair obj)`

Stores `obj` in the car field of `pair`.

## `set-cdr!`

`(set-cdr! pair obj)`

Stores `obj` in the cdr field of `pair`.

## `null?`

`(null? obj)`

Returns `#t` if `obj` is the empty list, and `#f` otherwise. Implemented in `s1-core.scm`.

## `list?`

`(list? obj)`

Returns `#t` if `obj` is a proper list, and `#f` otherwise. Implemented via `type-of`.

## `list`

`(list obj ...)`

Returns a newly allocated list of its arguments.

## `length`

`(length list)`

Returns the length of `list`.

## `append`

`(append list ...)`

Returns a list consisting of the elements of the first `list` followed by the elements of the other `list`s.

## `reverse`

`(reverse list)`

Returns a newly allocated list consisting of the elements of `list` in reverse order.

## `list-tail`

`(list-tail list k)`

Returns the sublist of `list` obtained by omitting the first `k` elements.

## `list-ref`

`(list-ref list k)`

Returns the `k`th element of `list`.

## `memq`

`(memq obj list)`

## `memv`

`(memv obj list)`

## `member`

`(member obj list)`

These procedures return the first sublist of `list` whose car is `obj`, where the sublists of `list` are the non-empty lists returned by `(list-tail list k)` for `k` from 0 to `(- (length list) 1)`. If `obj` does not occur in `list`, then `#f` is returned. `memq` uses `eq?` to compare `obj` with the elements of `list`, while `memv` uses `eqv?` and `member` uses `equal?`. Implemented in `s1-core.scm`.

## `assq`

`(assq obj alist)`

## `assv`

`(assv obj alist)`

## `assoc`

`(assoc obj alist)`

`alist` (for "association list") must be a list of pairs. These procedures find the first pair in `alist` whose car field is `obj`, and returns that pair. If no pair in `alist` has `obj` as its car, then `#f` is returned. `assq` uses `eq?` to compare `obj` with the car fields of the pairs in `alist`, while `assv` uses `eqv?` and `assoc` uses `equal?`. Implemented in `s1-core.scm`.

## List Accessors

These functions provide convenient access to nested list elements. Implemented in `s1-core.scm`.

*   `cadr`
*   `cdar`
*   `caar`
*   `cddr`
*   `caddr`
*   `cadddr`
*   `cadar`
*   `cddar`
*   `caadr`
*   `cdadr`
*   `cdddr`
*   `caaar`
*   `cdaar`
*   `caaadr`
*   `cdaadr`
*   `cadadr`
*   `cddadr`
*   `caaddr`
*   `cdaddr`
*   `cdddr`
## `empty?`

`(empty? obj)`

Returns `#t` if `obj` is the empty list, and `#f` otherwise. An alias for `null?`. Implemented in `s1-core.scm`.

## `top`

`(top list)`

Returns the first element of a list. An alias for `car`. Implemented in `s1-core.scm`.

## `push!`

`(push! obj list)`

A macro that prepends an element to a list. Equivalent to `(set! list (cons obj list))`. Implemented in `s1-core.scm`.

## `pop!`

`(pop! list)`

A macro that removes and returns the first element of a list. Equivalent to `(let ((result (car list))) (set! list (cdr list)) result)`. Implemented in `s1-core.scm`.

## `zip`

`(zip list ...)`

Returns a list of lists, where the i-th list contains the i-th elements of the input lists. Implemented in `s1-core.scm`.

## `map`

`(map proc list ...)`

Applies `proc` to the elements of the `list`s and returns a list of the results. Implemented in `s1-core.scm`.

[Home](s1-docs.md)