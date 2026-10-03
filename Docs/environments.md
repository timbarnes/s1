[Home](s1-docs.md)

# Environments and Evaluation

R7RS section 6.12. An environment is a set of top-level bindings that an expression can be evaluated in. Libraries (`define-library`, `import`) and `environment` are being added in phase 9; see the [design](./libraries-design.md).

## `interaction-environment`

`(interaction-environment)`

Returns the environment the REPL and loaded files run in: every built-in procedure and special form, everything `s1-core.scm` defines, and the program's own top-level definitions. Environments print as `#<environment>`, and `type-of` gives `environment`.

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
