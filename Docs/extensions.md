[Home](s1-docs.md)

# S1 Scheme Extensions

This document describes functions and features specific to the S1 Scheme interpreter that are not part of the R5RS specification.

## `gc-threshold`

`(gc-threshold [n])`

With no arguments, returns the current GC threshold. With one argument, sets the GC threshold to `n`; 0 turns automatic collection off.

The threshold is the minimum number of allocations between collections; the default is 20,000. A collection also waits until as many objects have been allocated as survived the last one, so collecting takes time in proportion to allocating, however large the live data grows. A threshold below the default is exact, which is how the GC stress tests collect after every allocation: `(gc-threshold 1)`.

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

## `macro` and `expand`

`(macro params body ...)` creates a non-hygienic macro whose body computes its expansion; `(expand form)` shows one level of a macro use's expansion. See [Macros](./macros.md).

## `def`, `def-fn`, `def-var`

These are macros that provide a more convenient way to create definitions. `def` is a general purpose macro that dispatches to `def-fn` or `def-var` based on the form of the first argument.

`(def (name args...) body ...)` is equivalent to `(define name (lambda (args...) body ...))`.

`(def name value)` is equivalent to `(define name value)`.

[Home](s1-docs.md)
