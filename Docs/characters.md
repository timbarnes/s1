[Home](s1-docs.md)

# Characters

## `char?`

`(char? obj)`

Returns `#t` if `obj` is a character, and `#f` otherwise.

## `char=?`

`(char=? char1 char2)`

## `char<?`

`(char<? char1 char2)`

## `char>?`

`(char>? char1 char2)`

## `char<=?`

`(char<=? char1 char2)`

## `char>=?`

`(char>=? char1 char2)`

These procedures return `#t` if the results of passing their arguments to `char->integer` are respectively equal, less than, greater than, less than or equal to, or greater than or equal to. Implemented in `s1-core.scm`.

## `char-alphabetic?`

`(char-alphabetic? char)`

## `char-numeric?`

`(char-numeric? char)`

## `char-whitespace?`

`(char-whitespace? char)`

## `char-upper-case?`

`(char-upper-case? char)`

## `char-lower-case?`

`(char-lower-case? char)`

These character predicates return `#t` if the character is alphabetic, numeric, whitespace, upper case, or lower case, respectively. Implemented in `s1-core.scm`.

## `char->integer`

`(char->integer char)`

Returns the integer representation of `char`.

## `integer->char`

`(integer->char n)`

Returns the character whose integer representation is `n`.

## `char-upcase`

`(char-upcase char)`

## `char-downcase`

`(char-downcase char)`

These procedures return a character `char2` such that `(char-downcase char)` returns the lower case version of `char` and `(char-upcase char)` returns the upper case version of `char`. **Not implemented.**

[Home](s1-docs.md)