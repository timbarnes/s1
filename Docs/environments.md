[Home](s1-docs.md)

# Environments and Evaluation

R7RS section 6.12. An environment is a set of top-level bindings that an expression can be evaluated in. Libraries (`define-library`, `import`) are being added in phase 9; see the [design](./libraries-design.md).

## The system and interaction environments

s1's built-in procedures and special forms, and everything `s1-core.scm` defines, live in the *system environment*. Programs, the REPL and loaded files run in the *interaction environment*, which imports every system binding. An imported name shares the system's variable, so:

* `(define car ...)` at the REPL makes a new `car` for the interaction environment only. Procedures defined in `s1-core.scm` keep using the system's `car`.
* `(set! car ...)` is an error: an imported variable can only be assigned by its owner. Names the program defines itself can be `set!` as usual.

## Standard libraries

The R7RS standard libraries exist as export lists over the system environment: `(scheme base)`, `(scheme case-lambda)`, `(scheme char)`, `(scheme complex)`, `(scheme cxr)`, `(scheme eval)`, `(scheme file)`, `(scheme inexact)`, `(scheme lazy)`, `(scheme load)`, `(scheme process-context)`, `(scheme read)`, `(scheme repl)`, `(scheme time)`, `(scheme write)` and `(scheme r5rs)`. Names a library should export but s1 doesn't define yet are left out (the complex-number procedures, for example). `(s1)` exports everything else in the system environment: s1's extensions.

* `(library-names)`: the registered libraries' names, such as `(scheme base)`. An s1 extension.
* `(library-exports library-name)`: the names a library exports, as a list of symbols. An s1 extension.

## `import`

`(import import-set ...)`

Binds the names the import sets denote in the current top-level environment. An import set is:

* a library name, such as `(scheme base)`: all its exports;
* `(only set identifier ...)`: just those names;
* `(except set identifier ...)`: all but those names;
* `(prefix set prefix)`: every name with `prefix` in front;
* `(rename set (from to) ...)`: those names renamed.

Naming an identifier the set doesn't contain is an error. Every set is checked before anything is bound, so an import that fails binds nothing.

```scheme
(import (prefix (scheme char) c:))
(c:char-upcase #\a)                                    ; => #\A
(import (rename (only (scheme base) car) (car first)))
(first '(1 2))                                         ; => 1
```

An imported name shares the library's variable, and can't be `set!`. `import` is allowed only at the top level of an environment (the REPL, a loaded file, or `eval` in a mutable environment), not inside a body. At the REPL, importing a name replaces any binding it had, and a later `define` of it replaces the import.

## `environment`

`(environment import-set ...)`

Returns a new environment containing exactly the bindings of the import sets. It is immutable: `define`, `set!` and `import` at its top level are errors (local definitions inside expressions are fine).

```scheme
(eval '(+ 1 2) (environment '(scheme base)))              ; => 3
(eval '(char-upcase #\a) (environment '(scheme base)))   ; error: char-upcase is unbound
```

## `scheme-report-environment` and `null-environment`

`(scheme-report-environment 5)` returns an immutable environment of `(scheme r5rs)`, and `(null-environment 5)` one with only its syntactic keywords (`if`, `define`, `let`, ...). R5RS procedures; 5 is the only version supported.

## `interaction-environment`

`(interaction-environment)`

Returns the environment the REPL and loaded files run in: everything the system environment holds, imported, and the program's own top-level definitions. Environments print as `#<environment>`, and `type-of` gives `environment`.

## `eval`

`(eval expr environment)`

Evaluates `expr`, a datum, in `environment`, and returns its value. Definitions in `expr` bind in that environment. The caller's local variables are not visible:

```scheme
(eval '(* 7 3) (interaction-environment))       ; => 21
(let ((x 1)) (eval 'x (interaction-environment))) ; error: x is unbound
```

`(eval expr)`, without an environment, is an s1 extension: it evaluates `expr` where `eval` is called, so the caller's local variables are visible.

## `load`

`(load filename [environment])`

With an environment, reads the forms in `filename` and evaluates them in turn in that environment, and returns when the file is finished. Without one, see [System Interface](./system-interface.md).

[Home](s1-docs.md)
