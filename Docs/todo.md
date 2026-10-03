[Home](s1-docs.md)

# Not Yet Implemented

Standard features s1 doesn't provide yet, and known issues. The R7RS conformance suite (`tests/r7rs/`) tracks progress section by section.

## Numbers

*   Complex numbers (`make-rectangular`, `make-polar`, `real-part`, `imag-part`, `magnitude`, `angle`): not planned

## Input and Output

*   `transcript-on`, `transcript-off` (R5RS only; removed in R7RS).

## Known issues

None outstanding. (Explicit-port reads now share the port, and `write` labels cycles, since phase 8.)

[Home](s1-docs.md)