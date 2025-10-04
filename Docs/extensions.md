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

## `benchmark`

`(benchmark form count)`

Runs the quoted `form` `count` times and returns the average execution time.

## `def`, `def-fn`, `def-var`

These are macros that provide a more convenient way to create definitions. `def` is a general purpose macro that dispatches to `def-fn` or `def-var` based on the form of the first argument.

`(def (name args...) body ...)` is equivalent to `(define name (lambda (args...) body ...))`.

`(def name value)` is equivalent to `(define name value)`.

[Home](s1-docs.md)
