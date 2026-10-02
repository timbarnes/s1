[Home](s1-docs.md)

# Not Yet Implemented

Standard features s1 doesn't provide yet, and known issues. The R7RS conformance suite (`tests/r7rs/`) tracks progress section by section.

## Numbers

*   Complex numbers (`make-rectangular`, `make-polar`, `real-part`, `imag-part`, `magnitude`, `angle`): not planned

## Input and Output

*   String ports and the rest of R7RS 6.13 (phase 8).
*   `transcript-on`, `transcript-off` (R5RS only; removed in R7RS).

## Known issues, scheduled in the R7RS plan

*   `(read port)` with an explicit port reads from a copy of the port, so repeated reads return the same datum (phase 8, ports get identity).
*   `write` loops forever on cyclic data, which datum labels can now create (phase 8, `write` with datum labels). (`equal?`, `length` and `list?` handle cycles since phase 7.)

[Home](s1-docs.md)