# A Scheme interpreter, written in Rust.
This is the beginning of a Scheme interpreter, that tries to be faithful to the r7rs specification. It is incomplete, and under development.
It is a single-threaded implementation, with support for lexical scoping.

Currently it does not provide full support for tail recursion, but that is in the roadmap and a priority. I'm also considering the possibility of a byte-code interpreter using a simple stack machine, but this is not part of the current codebase.

Development is on a Mac Studio, but so far as I know it should be straightforward to target for a different architecture as it's a standard cargo project.
