# S1 Scheme Interpreter

A Scheme interpreter written in Rust that implements [R7RS-small](https://standards.scheme.org/corrected-r7rs/r7rs.html), except for complex numbers. S1 features a modern garbage-collected runtime, lexical scoping, macro support, and an extensible I/O system.

Documentation: the [user reference](https://timbarnes.github.io/s1/) (source in `docs/`) and the [internal docs](https://timbarnes.github.io/s1/api/s1/) for working on the interpreter.

## Features

### Core Language Support
- **R7RS-small**: passes chibi-scheme's R7RS test suite except for complex numbers; see [Standards Conformance](https://timbarnes.github.io/s1/conformance/)
- **Proper tail calls**, first-class re-entrant continuations, `dynamic-wind`, exceptions and parameters
- **Libraries**: `import`, `define-library`, `.sld` library files and `cond-expand`
- **Lexical Scoping**: Full lexical scoping with proper closure capture
- **Garbage Collection**: Mark-and-sweep garbage collector with cycle detection
- **Macro System**: Hygienic `syntax-rules` macros (`define-syntax`, `let-syntax`, `letrec-syntax`), plus s1's procedural `macro` form
- **Interactive REPL**: Full read-eval-print loop with command history
- **CEK Evaluator**: Basis for call/cc, exceptions, and continuations

### Data Types
- **Numbers**: Arbitrary precision integers (BigInt), exact rationals, and IEEE 754 floating-point
- **Symbols**: Interned identifiers for efficient comparison
- **Strings**: UTF-8 string literals with proper escaping
- **Characters**: Individual character values
- **Booleans**: `#t` and `#f` values
- **Lists**: Proper and improper lists built from cons cells
- **Vectors**: Fixed-size heterogeneous arrays
- **Bytevectors**: Byte arrays, written `#u8(...)`
- **Records**: `define-record-type`
- **Closures**: First-class functions with lexical environment capture
- **Ports**: I/O abstraction supporting files, strings, and standard streams

### Special Forms
- `quote` - Prevent evaluation
- `lambda` - Function definition
- `define-syntax`, `let-syntax`, `letrec-syntax`, `syntax-rules` - Hygienic macros
- `when`, `unless`, `case`, `do`, named `let`, `let*`, `letrec`, `letrec*`, `let-values`, `let*-values`, `define-values`, `case-lambda`, `parameterize`, `delay`, `delay-force`, `guard` - R7RS derived forms (see [docs/derived-expressions.md](docs/derived-expressions.md))
- `macro` - Procedural (non-hygienic) macro definition, an s1 extension
- `define` - Variable and function binding
- `set!` - Variable assignment
- `if` - Conditional evaluation
- `cond` - Multi-way conditional
- `begin` - Sequential evaluation
- `and` / `or` - Short-circuiting logical operators
- `eval` / `apply` - Meta-evaluation functions

### Built-in Functions

#### Arithmetic Operations
- `+`, `-`, `*`, `/` - Arithmetic; exact operands give exact results (`(/ 1 2)` is `1/2`)
- `quotient`, `remainder`, `modulo`, `floor/`, `truncate/` - Integer division
- `=`, `<`, `>`, `<=`, `>=` - Numeric comparison operators
- See [docs/numbers.md](docs/numbers.md) for the full numeric library

#### List Operations
- `car`, `cdr` - List accessors
- `cons` - Pair construction
- `list` - List construction from arguments
- `append` - List concatenation
- Extended car/cdr combinations: `cadr`, `caddr`, `cadddr`, etc.

#### Type Predicates
- `number?`, `symbol?`, `pair?`, `string?`, `vector?`
- `boolean?`, `char?`, `procedure?`, `closure?`, `macro?`
- `null?`, `eq?` - Value testing

#### String Operations
- `string-append` - Concatenate strings
- `string-length` - Length of a string
- `substring` - Extract substring
- `string-ref` - Character at index
- `string->list` - Convert string to list of characters
- `list->string` - Convert list of characters to string
- `string-copy` - Create a copy of a string
- `string=?` - Compare strings for equality
- `string<?` - Compare strings lexicographically
- `string>?` - Compare strings lexicographically
- `string-upcase` - Convert string to uppercase
- `string-downcase` - Convert string to lowercase

#### I/O Operations
- `display` - Output values in human-readable format
- `write` - Output values in Scheme-readable format
- `displayln` - Output a sequence of values followed by a newline character
- `newline` - Output newline character
- `open-input-file` - Open files for reading

#### Utilities
- `type-of` - Runtime type inspection
- `help` - Documentation lookup
- `exit` - Exit interpreter

### Advanced Features

#### Garbage Collection
The interpreter uses a mark-and-sweep garbage collector that automatically manages memory for all Scheme objects. The GC handles cycles correctly and provides predictable memory management without manual intervention.

#### Environment Model
Implements proper lexical scoping using environment frames. Each closure captures its defining environment, enabling proper lexical variable access and supporting advanced patterns like currying and partial application.

#### Port System
Extensible I/O system supporting:
- Standard input/output streams
- File-based input/output
- String-based I/O ports
- Port stack management for nested file loading

#### Macro Expansion
R7RS `syntax-rules` macros are hygienic: identifiers a macro introduces can't capture, or be captured by, the user's. s1's own `macro` form instead runs Scheme code on the unevaluated arguments to compute the expansion. See [docs/macros.md](docs/macros.md).

#### Debug Support
- `trace` function for debugging evaluation
- Limited error messages with context - more work required in the evaluator
- Interactive debugging in REPL mode

## Architecture

### Two-Layer Evaluation
The interpreter uses a clean separation between evaluation logic and function application:
- **Logic Layer**: Handles self-evaluating forms, special forms, and argument evaluation
- **Apply Layer**: Handles function calls with pre-evaluated arguments

### Memory Management
All Scheme values are allocated on a garbage-collected heap using `GcRef` references. The heap automatically manages memory and handles circular references correctly.

### Module Structure
- `gc.rs` - Garbage collection and object allocation
- `eval.rs` - Core evaluation interface
- `cek.rs` - Continuation-passing evaluator
- `parser.rs` - S-expression parsing
- `tokenizer.rs` - Lexical analysis
- `env.rs` - Environment and scoping
- `builtin/` - Built-in function implementations
- `io.rs` - I/O port system
- `macros.rs` - Macro expansion
- `builtin/mod.rs` - orchestrates builtin functions

## Usage

### Command Line Interface

```bash
# Start interactive REPL
cargo run

# Load files and start REPL
cargo run -- -f file1.scm -f file2.scm

# Execute files and exit (batch mode)
cargo run -- -f script.scm -q

# Run a script with arguments, then exit; (command-line) gives the arguments
cargo run -- script.scm arg1 arg2

# Skip loading core library
cargo run -- -n

# Run the regression suite
cargo run --release -- -r -q
```

### Command Line Options
- `-f <file>` - Load Scheme file (can be repeated)
- `-q` - Quit after loading files (batch mode)
- `-n` - Skip loading `scheme/s1-core.scm`
- `-r` - Run the regression suite
- `script [arg ...]` - Run a script after the `-f` files, then exit; everything after it is passed to the program (see [docs/system-interface.md](docs/system-interface.md))

### REPL Usage

```scheme
s1> (+ 1 2 3)
=> 6

s1> (define factorial
      (lambda (n)
        (if (= n 0)
            1
            (* n (factorial (- n 1))))))
=> factorial

s1> (factorial 5)
=> 120

s1> (help 'car)
=> "(car pair) -> first element of pair"

s1> (exit)
```

### File Loading

The interpreter automatically loads `scheme/s1-core.scm` on startup (unless `-n` is specified), which provides additional standard library functions and utilities.

## Building

### Prerequisites
- Rust 2024 edition or later
- Cargo package manager

### Dependencies
- `argh` - Command line argument parsing
- `num-bigint` - Arbitrary precision integer arithmetic
- `num-traits` - Numeric trait abstractions
- `tempfile` - Temporary file handling

### Build Commands

```bash
# Build debug version
cargo build

# Build optimized release version
cargo build --release

# Run tests
cargo test

# Run the Scheme regression suite
cargo run --release -- -r -q

# Run the R7RS conformance suite against its baseline (see tests/r7rs/README.md)
tests/r7rs/run.sh

# Run with specific file
cargo run -- -f examples/test.scm
```

## Standard Library

The `scheme/s1-core.scm` file provides additional Scheme functions:
- Extended list accessors (`cadr`, `caddr`, etc.)
- Additional type predicates
- Utility functions and common patterns
- Higher-order functions like `map`

## Development Status

s1 implements R7RS-small apart from complex numbers, which are not planned. The [Standards Conformance](https://timbarnes.github.io/s1/conformance/) page lists where it departs from the report, and `design/todo.md` tracks open work, including:

- Faster evaluation by pre-analysing code (see `design/precompilation-design.md`)
- Error reporting with source locations
- Finding `scheme/s1-core.scm` without having to run s1 from its own directory

## Examples

### Basic Arithmetic and Lists
```scheme
(define numbers (list 1 2 3 4 5))
(define sum (lambda (lst)
              (if (null? lst)
                  0
                  (+ (car lst) (sum (cdr lst))))))
(sum numbers)  ; => 15
```

### Higher-Order Functions
```scheme
(define my-map (lambda (f lst)
              (if (null? lst)
                  '()
                  (cons (f (car lst))
                        (my-map f (cdr lst))))))

(my-map (lambda (x) (* x x)) (list 1 2 3 4))  ; => (1 4 9 16)
```

### Macros
```scheme
(define-syntax my-unless
  (syntax-rules ()
    ((_ test body ...) (if test #f (begin body ...)))))

(my-unless (> 3 5) (display "3 is not greater than 5"))
```

## Contributing

S1 is under active development. Contributions are welcome, particularly in areas of:
- Performance improvements
- Documentation and examples
- Test coverage

## License

This project follows standard open source practices. See the repository for specific licensing terms.
