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

`(trace mode)` traces or single-steps the evaluator, and `(trace)` returns the current mode. Trace output and the debugger prompt go to standard error.

* `(trace 'expr)`: prints each expression the machine evaluates and each value it returns, indented by continuation depth (the depth is shown as a number once it is past 40). A value leaving a procedure is marked `<- return`, with the bindings of the call it leaves, and each top-level form's value is shown as `Result:`.
* `(trace 'all)`: the same, and under a line, the frame its expression or value feeds, with that frame's local bindings, whenever that frame changes.
* `(trace 'step)`: stops at the `debug>` prompt before every step.
* `(trace 'off)`: no tracing, but an uncaught error opens the `debug>` prompt, so you can look at the state where it happened before the form is abandoned.
* `(trace 'reset)`: the initial mode: no tracing, and uncaught errors are just reported.

In the modes that trace or step, an uncaught error also opens the prompt. Arguments that need no machine step of their own, such as variables, constants and calls of built-in procedures on them, are evaluated without a step and so don't appear in the trace.

```scheme
(define (fact n) (if (zero? n) 1 (* (fact (- n 1)) n)))
(trace 'all)
(fact 2)
; Expr:  (fact 2)
; Expr:  (if (zero? n) 1 (* (fact (- n 1)) n))
; Expr:  (* (fact (- n 1)) n)
;  Expr:  (fact (- n 1))
;      | in call  (* (fact (- n 1)) n)  [n=2]
;   Expr:  (if (zero? n) 1 (* (fact (- n 1)) n))
;   Expr:  (* (fact (- n 1)) n)
;    Expr:  (fact (- n 1))
;        | in call  (* (fact (- n 1)) n)  [n=1]
;     Expr:  (if (zero? n) 1 (* (fact (- n 1)) n))
;     Expr:  1
;     Value: 1   <- return [n=0]
;    Value: 1
;   Value: 1   <- return [n=1]
;       | in call  (* (fact (- n 1)) n)  [n=2]
;  Value: 1
; Result: 2
```

### `break`

`(break obj ...)` displays its arguments and stops at the `debug>` prompt, as if stepping had been on. Put it where you want to look around; `c` continues.

### The `debug>` prompt

| Command | |
|---|---|
| Enter, `n` | take one step |
| `o` | step over the current expression: run until its value is returned |
| `f` | finish: run until the current procedure returns |
| `c` | stop stepping and run on |
| `q` | abandon the top-level form |
| `bt` | backtrace: the current expression (frame 0) and the continuation frames waiting for it, each call with its local bindings (`[n=2]`). A procedure return appears only when the value is on its way to it (`-- return value = 30 --`); the numbers skip the returns left out |
| `u [n]`, `d [n]`, `fr n` | select a frame `n` lines up or down the backtrace, or by number |
| `l` | the bindings in the selected frame's innermost environment |
| `e` | all its local bindings |
| `p expr` | evaluate `expr` in the selected frame's environment and print the value; an error in it comes back to the prompt |
| `x`, `s`, `k` | the current expression, the machine state, the raw continuation |
| `h` | list the commands |

After an uncaught error the commands that run the machine (`n`, `o`, `f`, `p`) are not available; `c`, `q` or the end of input abandons the form. At the end of input while stepping, stepping stops and the program runs on.

```scheme
(define (f x) (let ((y (+ x 1))) (break) (* x y)))
(f 3)
; break
;    Value: #<void>
; debug> p (list x y)
; (3 4)
; debug> c
```

### `trace-procedure`, `untrace-procedure`

`(trace-procedure name)` replaces the procedure bound to the variable `name` with one that writes each call and its result to the current error port, indented by how many traced calls enclose it. `(untrace-procedure name)` puts the original back. Unlike `trace`, it costs nothing in the rest of the program, and it sees every call. Calls of a traced procedure are not tail calls while it is traced. Imported bindings, such as the built-in procedures, can't be traced, because they can't be assigned.

```scheme
(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(trace-procedure fib)
(fib 2)
; > (fib 2)
; | > (fib 1)
; | < 1
; | > (fib 0)
; | < 0
; < 1
```

### `debug-stack`, `trace-env`

`(debug-stack)` prints the continuation frames waiting for its value, innermost first. `(trace-env)` prints the bindings in the caller's local environment frames, innermost first; `(trace-env 'global)` includes the top-level frame too, which is long. Both print to standard error and return the void value.

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
