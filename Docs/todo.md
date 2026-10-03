[Home](s1-docs.md)

# Not Yet Implemented

Standard features s1 doesn't provide yet, and known issues. The R7RS conformance suite (`tests/r7rs/`) tracks progress section by section.

## Numbers

*   Complex numbers (`make-rectangular`, `make-polar`, `real-part`, `imag-part`, `magnitude`, `angle`): not planned

## Input and Output

*   `transcript-on`, `transcript-off` (R5RS only; removed in R7RS).

## Libraries (R7RS phase 9, done)

*   Done: see [Libraries](./libraries.md) and [Environments and Evaluation](./environments.md). `interaction-environment`, and `eval` and `load` with an environment argument (9a). The system and interaction environments, the standard libraries as export lists, and `environment` with library names (9b). `import` with `only`, `except`, `prefix` and `rename`; import sets in `environment`; `scheme-report-environment` and `null-environment` (9c). `define-library`, `include`, `include-ci`, `include-library-declarations` (9d). Library files on a search path (9e). `cond-expand` and `features` (9f). The conformance suite uses the real `import`, and its 6.12 tests pass (9g). Design: [libraries-design.md](./libraries-design.md).

## System interface

*   Done in phase 10: `command-line`, `exit` with a status and `dynamic-wind` unwinding, `emergency-exit`, `get-environment-variable(s)`, `current-second`, `current-jiffy`, `jiffies-per-second`, and running scripts (`s1 script arg ...`). See [System Interface](./system-interface.md).
*   s1 finds `scheme/s1-core.scm` relative to the current directory, so a `#!/usr/bin/env s1` script only works when run from the s1 directory. A configured or compiled-in location would fix it.

## Audit (R7RS phase 11, done)

*   Proper tail calls in the last expression of `and` and `or`, and in `apply` and `call/cc`; `scheme/tail_tests.scm` checks all 31 tail contexts (see [Control Features](./control-features.md#proper-tail-calls)). Nested `guard`s no longer cubic. O(1) indexing of ASCII strings. `syntax-error`; `else`, `=>`, `_` and `...` bound, so they can be imported and renamed; the last eight `c...r` procedures; the `full-unicode` feature.
*   Every standard library now exports all its names except the complex-number procedures, and the conformance suite fails only on complex numbers. A sweep of about 90 R7RS behaviours the suite doesn't test found nothing else.

## Development tools

*   A pretty printer for source code and other data (indentation, line breaking, `'x` for `(quote x)`), for use with `procedure-source`. Wanted, not a priority.

## Known issues

*   Indexing a string that contains non-ASCII characters (`string-ref`, `string-set!`, `substring`) scans from the start; ASCII strings are O(1).

[Home](s1-docs.md)