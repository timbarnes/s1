[Home](s1-docs.md)

# Not Yet Implemented

Standard features s1 doesn't provide yet, and known issues. The R7RS conformance suite (`tests/r7rs/`) tracks progress section by section.

## Numbers

*   Complex numbers (`make-rectangular`, `make-polar`, `real-part`, `imag-part`, `magnitude`, `angle`): not planned

## Input and Output

*   `transcript-on`, `transcript-off` (R5RS only; removed in R7RS).

## Libraries (R7RS phase 9, tabled)

*   `define-library`, `import` (with `only`, `except`, `prefix`, `rename`), `export`, `include`, `cond-expand`, `features`, `environment`, `interaction-environment`, and `eval` with an environment argument. Discussed but not started: the lightweight option is to copy exported values on import (exported variables that the library later `set!`s would not update in importers); the full option is shared binding cells. The conformance shim stubs `import` meanwhile.

## Development tools

*   A pretty printer for source code and other data (indentation, line breaking, `'x` for `(quote x)`), for use with `procedure-source`. Wanted, not a priority.

## Known issues

None outstanding. (Explicit-port reads now share the port, and `write` labels cycles, since phase 8.)

[Home](s1-docs.md)