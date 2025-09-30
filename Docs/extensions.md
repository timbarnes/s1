[Home](s1-docs.md)

# S1 Scheme Extensions

This document describes functions and features specific to the S1 Scheme interpreter that are not part of the R5RS specification.

## `exit`

`(exit)`

Exits the Scheme interpreter with exit code 0.

## `gc-threshold`

`(gc-threshold [n])`

With no arguments, returns the current GC threshold. With one argument, sets the GC threshold to `n`.

## `shell`

`(shell cmd)`

Executes the given command in a subprocess and returns the output as a string.

## `gc`

`(gc)`

Forces a garbage collection cycle.

## `trace`

`(trace [arg])`

Controls step and tracing options.
*   `(trace)`: Returns the current trace setting.
*   `(trace 'all)`: Prints `state.control` and `state.kont` each time through the evaluator.
*   `(trace 'expr)`: Shows trace when control is an expression, or when a value is returned.
*   `(trace 'step)`: Enables single stepping.
*   `(trace 'off)`: Disables tracing and stepping.

[Home](s1-docs.md)
