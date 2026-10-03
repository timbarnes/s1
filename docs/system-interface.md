# System Interface

R7RS: [section 6.14, System interface](https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html#TAG:__tex2page_sec_6.14). It covers `(scheme process-context)`, `(scheme time)`, and `load` from `(scheme load)`. `file-exists?` and `delete-file` are in [Input and Output](./input-and-output.md), and `features` in [Libraries](./libraries.md#cond-expand-and-features).

## Running s1

`s1 [-n] [--core file] [-f file]... [-q] [-r] [script [arg ...]]`

* With no script, s1 loads the `-f` files in order and then starts the REPL, or exits if `-q` is given.
* With a script, s1 loads the `-f` files, runs the script, and exits. Everything after the script name, including words that start with `-`, is passed to the program, and `command-line` returns it. A script gets no startup banner, so its output is its own.
* A script's first line may be `#!/usr/bin/env s1` (the reader treats `#!/...` and `#! ...` as comments to the end of the line).
* s1's core library, `scheme/s1-core.scm`, is built into the binary, so s1 runs from any directory. `-n` skips it; `--core file` loads `file` in its place, for working on the core without rebuilding.

Exit status: 0 when the program finishes, or whatever `exit` gives. While a script is running, an uncaught error or a syntax error ends it with status 70 (`EX_SOFTWARE`), after the `after` thunks of any `dynamic-wind` it is inside have run. Without a script, an error is reported and s1 carries on with the next form.

## `command-line`

`(command-line)`

Returns the command line as a list of strings: the script's name as given, followed by its arguments. Without a script it is `("s1")`.

```scheme
;; s1 count.scm a b
(command-line)          ; => ("count.scm" "a" "b")
```

## `exit`

`(exit [obj])`

Runs the `after` thunks of every `dynamic-wind` the call is inside, innermost first, flushes standard output, and ends the process. The exit status is 0 if `obj` is absent or `#t`, 1 if it is `#f`, and `obj` itself if it is an exact integer. Any other `obj` is an error. If an `after` thunk raises an error, the error is reported as usual and the program carries on. Works at the REPL too.

## `emergency-exit`

`(emergency-exit [obj])`

Ends the process at once, with the same exit status as `exit`, without running any `after` thunks. Standard output is flushed.

## `get-environment-variable`

`(get-environment-variable name)`

Returns the value of the environment variable `name` (a string) as a string, or `#f` if it isn't set. A value that isn't valid UTF-8 is converted with replacement characters.

```scheme
(get-environment-variable "HOME")       ; => "/home/user"
(get-environment-variable "NO_SUCH")    ; => #f
```

## `get-environment-variables`

`(get-environment-variables)`

Returns all environment variables as a list of `(name . value)` pairs of strings.

## `current-second`

`(current-second)`

Returns the current time as an inexact number of seconds since the Unix epoch (1970-01-01). R7RS specifies TAI; s1 uses the system clock, which is UTC, so it differs from TAI by the leap seconds (37 in 2026).

## `current-jiffy`

`(current-jiffy)`

Returns the number of jiffies since s1 started, as an exact integer, from a clock that never goes backwards. Use it to time things:

```scheme
(define start (current-jiffy))
(do-something)
(/ (- (current-jiffy) start) (jiffies-per-second))   ; elapsed seconds, exact
```

## `jiffies-per-second`

`(jiffies-per-second)`

Returns 1000000000: a jiffy is a nanosecond.

## `load`

`(load filename [environment])`

`filename` must be a string. The `load` procedure reads expressions and definitions from the file and evaluates them sequentially. Without an environment, the file is pushed onto the stack of ports the REPL reads from, so its forms are evaluated in the interaction environment after the current top-level form finishes. With an environment, they are evaluated in it before `load` returns (see [Environments and Evaluation](./environments.md)). Implemented in `s1-core.scm`.
