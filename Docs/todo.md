[Home](s1-docs.md)

# R5RS Functions To Implement

This document lists R5RS (Revised^5 Report on the Algorithmic Language Scheme) functions that are not yet implemented in the S1 Scheme interpreter.

## Core Syntax/Forms

*   `define-record-type` (phase 7)

## Equivalence Predicates


## Numbers

*   Complex numbers (`make-rectangular`, `make-polar`, `real-part`, `imag-part`, `magnitude`, `angle`): not planned

## Characters


## Strings

*   `string>=?`

## Input and Output

*   `transcript-on`, `transcript-off`

## Known issues, scheduled in the R7RS plan

*   `(read port)` with an explicit port reads from a copy of the port, so repeated reads return the same datum (phase 8, ports get identity).
*   `write` loops forever on cyclic data, which datum labels can now create (phase 8, `write` with datum labels).

[Home](s1-docs.md)