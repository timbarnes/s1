[Home](s1-docs.md)

# R5RS Functions To Implement

This document lists R5RS (Revised^5 Report on the Algorithmic Language Scheme) functions that are not yet implemented in the S1 Scheme interpreter.

## Core Syntax/Forms

*   `case`
*   `delay` (core form, `force` is implemented)
*   `quasiquote`, `unquote`, `unquote-splicing`

## Equivalence Predicates

*   `eqv?` (currently implemented as `eq?`)

## Numbers

*   `complex?`, `real?`, `rational?`, `integer?` (some predicates are implemented, but not all)
*   `exact?`, `inexact?`
*   `exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`
*   `make-rectangular`, `make-polar`, `real-part`, `imag-part`, `magnitude`, `angle`

## Characters

*   `char-upcase`, `char-downcase`

## Strings

*   `string` (from chars)
*   `string<=?`, `string>=?`
*   `list->string`
*   `string-fill!`

## Input and Output

*   `transcript-on`, `transcript-off`

[Home](s1-docs.md)