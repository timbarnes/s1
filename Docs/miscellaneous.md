[Home](s1-docs.md)

# Miscellaneous

## `help`

`(help 'symbol)`

Returns the doc string for the given symbol as a Scheme string.

## `exit`

`(exit)`

Exits the Scheme interpreter with exit code 0.

## `void`

`(void)`

Returns the void value.

## `gc-threshold`

`(gc-threshold [n])`

With no arguments, returns the current GC threshold. With one argument, sets the GC threshold to `n`.

## `shell`

`(shell cmd)`

Executes the given command in a subprocess and returns the output as a string.

[Home](s1-docs.md)