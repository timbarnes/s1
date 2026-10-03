[Home](s1-docs.md)

# System Interface

## `load`

`(load filename [environment])`

`filename` must be a string. The `load` procedure reads expressions and definitions from the file and evaluates them sequentially. Without an environment, the file is pushed onto the stack of ports the REPL reads from, so its forms are evaluated in the interaction environment after the current top-level form finishes. With an environment, they are evaluated in it before `load` returns (see [Environments and Evaluation](./environments.md)). Implemented in `s1-core.scm`.

## `transcript-on`

`(transcript-on filename)`

`filename` must be a string. Starts a transcript of interaction with the user, saving it to the file. **Not implemented.**

## `transcript-off`

`(transcript-off)`

Ends the transcript. **Not implemented.**

[Home](s1-docs.md)