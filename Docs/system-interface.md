[Home](s1-docs.md)

# System Interface

## `load`

`(load filename)`

`filename` must be a string. The `load` procedure reads expressions and definitions from the file and evaluates them sequentially. Implemented in `s1-core.scm`.

## `transcript-on`

`(transcript-on filename)`

`filename` must be a string. Starts a transcript of interaction with the user, saving it to the file. **Not implemented.**

## `transcript-off`

`(transcript-off)`

Ends the transcript. **Not implemented.**

[Home](s1-docs.md)