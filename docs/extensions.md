# S1 Extensions

What s1 provides beyond R7RS-small. The extension procedures are all available at the REPL and in programs, and the library `(s1)` exports them, so a library can `(import (s1))` to use them. A portable program should avoid them, or test for s1 with [`cond-expand`](./libraries.md#cond-expand-and-features) (`s1` is one of its features).

## Language

* **Non-hygienic macros.** `(macro params body ...)` makes a macro whose body is ordinary Scheme code that computes the expansion; `(expand form)` shows one level of a macro use's expansion. See [Macros](./macros.md#macro-s1-extension).
* **Docstrings.** A string at the start of a procedure body that has more expressions after it documents the procedure for `help`. See [Primitive Expressions](./primitive-expressions.md#lambda).
* **`eval` without an environment.** `(eval expr)` evaluates `expr` in the caller's scope, so local variables are visible. See [Environments and Evaluation](./environments.md#eval).
* **Square brackets** read as a vector: `[1 2 3]` is `#(1 2 3)`. See [Lexical Syntax](./lexical-syntax.md#s1-differences).
* **`nil`** is a variable bound to `()`.
* **`def`**: `(def (name args ...) body ...)` is `(define (name args ...) body ...)`, and `(def name value)` is `(define name value)`. `def-fn` and `def-var` are the two cases.

## Documentation and inspection

### `help`, `add-doc`

`(help 'name)` returns the documentation string of the procedure or special form bound to `name`. Built-in procedures, s1-core's procedures, and procedures with docstrings all have one:

```scheme
(help 'car)       ; => "(car pair) -> first element of pair"
```

`(add-doc 'name "doc")` attaches documentation to a name, replacing any it had.

### `procedure-source`

`(procedure-source proc)`

Returns the form `proc` was made from, as data: `(lambda formals body ...)` for a closure (also when it was written as `(define (f ...) ...)`), `(macro ...)` for a `macro` procedure, and `(case-lambda clause ...)` for a `case-lambda`. Docstrings and internal definitions appear as written. Identifiers renamed by `syntax-rules` expansion show their original names, so the result is for reading, not for evaluating. Returns `#f` for a built-in procedure.

```scheme
(define (f x) (+ x 1))
f                       ; => #<procedure f>
(procedure-source f)    ; => (lambda (x) (+ x 1))
```

### `type-of`

`(type-of obj)` returns a symbol naming the type of `obj`: `integer`, `rational`, `float`, `symbol`, `string`, `char`, `boolean`, `null`, `pair`, `vector`, `bytevector`, `closure`, `builtin`, `record`, `port`, `environment`, and so on. Promises are records, and parameter objects are closures.

### Type predicates

* `(closure? obj)`: `#t` for a procedure written in Scheme (`lambda`, `define`, `case-lambda`), `#f` for built-ins.
* `(macro? obj)`: `#t` for a macro made with `macro`, `#f` for anything else, including `syntax-rules` transformers.
* `(float? obj)`: `#t` for an inexact number.

### Libraries

`(library-names)` lists the registered libraries, and `(library-exports '(name ...))` the names one exports. See [Libraries](./libraries.md#standard-libraries).

## Output

* `(displayln obj ...)`: `display`s each argument followed by a space, then a newline.
* `(display+ obj)`: `display`s `obj` followed by a space.
* `(writeln obj ...)`: `display`s each argument with no separator and no newline; with no arguments, writes a newline.
* `(>string obj ...)`: a new string holding what `display` would print for each argument, concatenated: `(>string 42 " " 'x)` is `"42 x"`.
* `(void)`: returns the void value, which the REPL doesn't print.
* `flush-output` is an older name for `flush-output-port`.
* `push-port!` and `pop-port!` manage the stack of ports the REPL reads from, which is how `load` works. `**stdin**`, `**stdout**` and `**stderr**` are the standard ports.

## Lists

* `(empty? list)`: the same as `null?`.
* `(top list)`: the first element, or `#f` if the list is empty.
* `(push! obj variable)`: a macro that conses `obj` onto the list in `variable` and stores the result back. `(pop! variable)` removes the first element and returns it.
* `(zip list ...)`: a list of lists, the first holding the first elements of each list, and so on.

```scheme
(define stack '())
(push! 1 stack)
(push! 2 stack)
(pop! stack)        ; => 2
stack               ; => (1)
```

## Evaluation and the system

* `(eval-string string)`: reads every form in `string`, evaluates them in turn, and returns a list of their values.
* `(shell command)`: runs `command` with the system shell and returns its standard output as a string.

## Performance and debugging

### Timing

* `(with-timer expr)`: evaluates `expr` and returns the time it took, in seconds.
* `(benchmark 'form count)`: evaluates `form` `count` times and returns the average time, in seconds.

For timing with standard procedures, use `current-jiffy` (see [System Interface](./system-interface.md#current-jiffy)).

### Garbage collection

`(gc)` collects garbage now and returns the time it took, in seconds. `garbage-collect` is another name for it.

`(gc-threshold [n])` with no arguments returns the current GC threshold; with one, sets it to `n`. 0 turns automatic collection off. The threshold is the minimum number of allocations between collections; the default is 20,000. A collection also waits until as many objects have been allocated as survived the last one, so collecting takes time in proportion to allocating, however large the live data grows. A threshold below the default is exact, which is how the GC stress tests collect after every allocation: `(gc-threshold 1)`.

### `trace`

`(trace mode)` controls tracing of the evaluator, and `(trace)` returns the current mode:

* `(trace 'all)`: prints the machine's control and continuation every step.
* `(trace 'expr)`: prints each expression as it is evaluated, and each value returned.
* `(trace 'step)`: single steps.
* `(trace 'off)`: turns tracing and stepping off, but an uncaught error then enters the stepper, so you can inspect the state where it happened.
* `(trace 'reset)`: back to the initial mode: no tracing, and uncaught errors are reported normally.

The stepper prompts `debug>`; press Enter to take a step, or type `c` to continue, `e` to show the environment, `l` the local variables, `k` the continuation, `x` the current expression, or `s` the machine state.

### `debug-stack`, `trace-env`

`(debug-stack)` prints the continuation frames waiting for its value, innermost first. `(trace-env)` prints the bindings in the caller's local environment frames, innermost first; `(trace-env 'global)` includes the top-level frame too, which is long. Both return the void value.

```scheme
(define (f x) (let ((y 2)) (trace-env) (+ x y)))
(f 1)
; Env frame 0:
;   y                    => 2
; Env frame 1:
;   x                    => 1
```

## Internal procedures

Names beginning with `%` (`%make-record-type`, `%kont-depth`, `%library-unimplemented`, ...) are the internal primitives that s1-core's macros and procedures are built on. They are exported by `(s1)` but may change without notice.
