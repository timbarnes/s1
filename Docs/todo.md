[Home](s1-docs.md)

# Not Yet Implemented

Standard features s1 doesn't provide yet, and known issues. The R7RS conformance suite (`tests/r7rs/`) tracks progress section by section.

## Numbers

*   Complex numbers (`make-rectangular`, `make-polar`, `real-part`, `imag-part`, `magnitude`, `angle`): not planned

## Input and Output

*   `transcript-on`, `transcript-off` (R5RS only; removed in R7RS).

## Libraries (R7RS phase 9, done)

*   Done: see [Libraries](./libraries.md) and [Environments and Evaluation](./environments.md). `interaction-environment`, and `eval` and `load` with an environment argument (9a). The system and interaction environments, the standard libraries as export lists, and `environment` with library names (9b). `import` with `only`, `except`, `prefix` and `rename`; import sets in `environment`; `scheme-report-environment` and `null-environment` (9c). `define-library`, `include`, `include-ci`, `include-library-declarations` (9d). Library files on a search path (9e). `cond-expand` and `features` (9f). The conformance suite uses the real `import`, and its 6.12 tests pass (9g). Design: [libraries-design.md](./libraries-design.md).
*   Not provided: `syntax-error`, and bindings for the auxiliary keywords `else`, `=>`, `_` and `...` (recognised by name), so they can't be imported or renamed.

## Pairs and lists

*   Eight of the four-level `c...r` procedures are missing: `caaaar`, `caadar`, `cadaar`, `caddar`, `cdaaar`, `cdadar`, `cddaar`, `cdddar` (found by the `(scheme cxr)` export list; see `%library-unimplemented`).

## Development tools

*   A pretty printer for source code and other data (indentation, line breaking, `'x` for `(quote x)`), for use with `procedure-source`. Wanted, not a priority.

## Known issues

None outstanding. (Explicit-port reads now share the port, and `write` labels cycles, since phase 8.)

[Home](s1-docs.md)