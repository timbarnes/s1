[Home](s1-docs.md)

# Not Yet Implemented

Standard features s1 doesn't provide yet, and known issues. The R7RS conformance suite (`tests/r7rs/`) tracks progress section by section.

## Numbers

*   Complex numbers (`make-rectangular`, `make-polar`, `real-part`, `imag-part`, `magnitude`, `angle`): not planned

## Input and Output

*   `transcript-on`, `transcript-off` (R5RS only; removed in R7RS).

## Libraries (R7RS phase 9, in progress)

*   Done: `interaction-environment`, and `eval` and `load` with an environment argument (9a). The system and interaction environments, the standard libraries as export lists, and `environment` with library names (9b).
*   To do: `define-library`, `import` (with `only`, `except`, `prefix`, `rename`), `export`, `include`, `cond-expand`, `features`, import sets in `environment`, `scheme-report-environment`, `null-environment`. Design: [libraries-design.md](./libraries-design.md). The conformance shim stubs `import` meanwhile.

## Pairs and lists

*   Eight of the four-level `c...r` procedures are missing: `caaaar`, `caadar`, `cadaar`, `caddar`, `cdaaar`, `cdadar`, `cddaar`, `cdddar` (found by the `(scheme cxr)` export list; see `%library-unimplemented`).

## Development tools

*   A pretty printer for source code and other data (indentation, line breaking, `'x` for `(quote x)`), for use with `procedure-source`. Wanted, not a priority.

## Known issues

None outstanding. (Explicit-port reads now share the port, and `write` labels cycles, since phase 8.)

[Home](s1-docs.md)