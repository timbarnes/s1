[Home](s1-docs.md)

# Input and Output

## `call-with-input-file`

`(call-with-input-file string proc)`

## `call-with-output-file`

`(call-with-output-file string proc)`

These procedures call `proc` with one argument: the port obtained by opening the named file for input or output. If the file cannot be opened, an error is signalled. The port is closed whether `proc` returns normally or returns by escaping from its continuation. Implemented in `s1-core.scm`.

## `input-port?`

`(input-port? obj)`

Returns `#t` if `obj` is an input port, and `#f` otherwise.

## `output-port?`

`(output-port? obj)`

Returns `#t` if `obj` is an output port, and `#f` otherwise.

## `port?`

`(port? obj)`

Returns `#t` if `obj` is a port, and `#f` otherwise. Implemented via `type-of`.

## `current-input-port`

`(current-input-port)`

Returns the current default input port.

## `current-output-port`

`(current-output-port)`

Returns the current default output port.

## `open-input-file`

`(open-input-file filename)`

Takes a string for `filename` and returns an input port that can deliver characters from the file.

## `open-output-file`

`(open-output-file filename)`

Takes a string for `filename` and returns an output port that can write characters to the file.

## `close-input-port`

`(close-input-port port)`

Closes the `port`.

## `close-output-port`

`(close-output-port port)`

Closes the `port`.

## `read`

`(read [port])`

Converts external representations of Scheme objects into the objects themselves.

## `write`

`(write obj [port])`

Writes a written representation of `obj` to the given `port`.

## `display`

`(display obj [port])`

Writes a representation of `obj` to the given `port`.

## `newline`

`(newline [port])`

Writes an end of line to `port`.

## `displayln`

`(displayln obj ...)`

Like `display`, but adds a newline character at the end. Implemented in `s1-core.scm`.

## `writeln`

`(writeln obj ...)`

Like `write`, but adds a newline character at the end. Implemented in `s1-core.scm`.

## `read-char`

`(read-char [port])`

Returns the next character available from the input `port`.

## `peek-char`

`(peek-char [port])`

Returns the next character available from the input `port`, but without consuming it.

## `eof-object?`

`(eof-object? obj)`

Returns `#t` if `obj` is an end-of-file object, and `#f` otherwise.

## `char-ready?`

`(char-ready? [port])`

Returns `#t` if a character is ready on the input `port` and `#f` otherwise.

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