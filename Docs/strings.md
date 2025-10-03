[Home](s1-docs.md)

# Strings

## `string?`

`(string? obj)`

Returns `#t` if `obj` is a string, and `#f` otherwise. Implemented via `type-of`.

## `make-string`

`(make-string k [char])`

Returns a newly allocated string of length `k`. If `char` is given, then all elements of the string are initialized to `char`, otherwise the contents of the string are unspecified.

## `string`

`(string char ...)`

Returns a newly allocated string composed of the arguments.

## `string-length`

`(string-length string)`

Returns the number of characters in `string`.

## `string-ref`

`(string-ref string k)`

Returns the `k`th character of `string`.

## `string-set!`

`(string-set! string k char)`

Stores `char` in element `k` of `string`.

## `string=?`

`(string=? string1 string2)`

## `string<?`

`(string<? string1 string2)`

## `string>?`

`(string>? string1 string2)`

## `string<=?`

`(string<=? string1 string2)`

## `string>=?`

`(string>=? string1 string2)`

These procedures compare strings in a lexicographical fashion. `string>=?` is **not implemented**.

## `substring`

`(substring string start end)`

Returns a newly allocated string formed from the characters of `string` beginning with index `start` (inclusive) and ending with index `end` (exclusive).

## `string-append`

`(string-append string ...)`

Returns a newly allocated string whose characters form the concatenation of the given strings.

## `string->list`

`(string->list string)`

Returns a newly allocated list of the characters that make up the given string.

## `list->string`

`(list->string list)`

Returns a newly allocated string formed from the characters in `list`.

## `string-copy`

`(string-copy string)`

Returns a newly allocated copy of the given `string`.

## `string-fill!`

`(string-fill! string char)`

Stores `char` in every element of `string`.

[Home](s1-docs.md)