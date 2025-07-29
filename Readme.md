# S1 Scheme Interpreter

A comprehensive Scheme interpreter written in Rust that aims to closely follow the R7RS specification. S1 features a modern garbage-collected runtime, lexical scoping, macro support, and an extensible I/O system.

## Features

### Core Language Support
- **R7RS Compliance**: Implements core Scheme language features following the R7RS specification (incomplete)
- **Lexical Scoping**: Full lexical scoping with proper closure capture
- **Garbage Collection**: Mark-and-sweep garbage collector with cycle detection
- **Macro System**: Scheme-style macros with proper expansion
- **Interactive REPL**: Full read-eval-print loop with command history

### Data Types
- **Numbers**: Arbitrary precision integers (BigInt) and IEEE 754 floating-point
- **Symbols**: Interned identifiers for efficient comparison
- **Strings**: UTF-8 string literals with proper escaping
- **Characters**: Individual character values
- **Booleans**: `#t` and `#f` values
- **Lists**: Proper and improper lists built from cons cells
- **Vectors**: Fixed-size heterogeneous arrays
- **Closures**: First-class functions with lexical environment capture
- **Ports**: I/O abstraction supporting files, strings, and standard streams

### Special Forms
- `quote` - Prevent evaluation
- `lambda` - Function definition
- `macro` - Macro definition
- `define` - Variable and function binding
- `set!` - Variable assignment
- `if` - Conditional evaluation
- `cond` - Multi-way conditional
- `begin` - Sequential evaluation
- `and` / `or` - Short-circuiting logical operators
- `eval` / `apply` - Meta-evaluation functions

### Built-in Functions

#### Arithmetic Operations
- `+`, `-`, `*`, `/` - Basic arithmetic with mixed integer/float support
- `mod` - Integer modulo operation
- `=`, `<`, `>` - Numeric comparison operators

#### List Operations
- `car`, `cdr` - List accessors
- `cons` - Pair construction
- `list` - List construction from arguments
- `append` - List concatenation
- Extended car/cdr combinations: `cadr`, `caddr`, `cadddr`, etc.

#### Type Predicates
- `number?`, `symbol?`, `pair?`, `string?`, `vector?`
- `boolean?`, `char?`, `closure?`, `macro?`, `primitive?`
- `nil?`, `eq?` - Value testing

#### I/O Operations
- `display` - Output values to ports
- `newline` - Output newline character
- `open-input-file` - Open files for reading

#### Utilities
- `type-of` - Runtime type inspection
- `help` - Documentation lookup
- `quit` - Exit interpreter

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
Full macro system allowing definition of new syntactic forms. Macros receive unevaluated arguments and can perform arbitrary computation to generate code.

#### Debug Support
- `trace` function for debugging evaluation
- Comprehensive error messages with context
- Interactive debugging in REPL mode

## Architecture

### Two-Layer Evaluation
The interpreter uses a clean separation between evaluation logic and function application:
- **Logic Layer**: Handles self-evaluating forms, special forms, and argument evaluation
- **Apply Layer**: Handles function calls with pre-evaluated arguments

### Memory Management
All Scheme values are allocated on a garbage-collected heap using `GcRef` references. The heap automatically manages memory and handles circular references correctly.

### Modular Design
- `gc.rs` - Garbage collection and object allocation
- `eval.rs` - Core evaluation engine
- `parser.rs` - S-expression parsing
- `tokenizer.rs` - Lexical analysis
- `env.rs` - Environment and scoping
- `builtin/` - Built-in function implementations
- `io.rs` - I/O port system
- `macros.rs` - Macro expansion

## Usage

### Command Line Interface

```bash
# Start interactive REPL
cargo run

# Load files and start REPL
cargo run -- -f file1.scm -f file2.scm

# Execute files and exit (batch mode)
cargo run -- -f script.scm -q

# Skip loading core library
cargo run -- -n

# Enable evaluation tracing
cargo run -- -t
```

### Command Line Options
- `-f <file>` - Load Scheme file (can be repeated)
- `-q` - Quit after loading files (batch mode)
- `-n` - Skip loading `scheme/s1-core.scm`
- `-t` - Enable trace mode for debugging

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
=> "Help for car: ..."

s1> (quit)
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

### Current Capabilities
- ✅ Core Scheme evaluation
- ✅ Lexical scoping and closures
- ✅ Garbage collection
- ✅ Macro system
- ✅ File I/O
- ✅ Interactive REPL
- ✅ Comprehensive built-ins

### Planned Features
- 🔄 **Tail call optimization** (highest priority)
- 📋 Full R7RS compliance
- 📋 Additional standard library functions
- 📋 Improved error reporting with source locations
- 📋 Module system
- 📋 Additional numeric types (rationals, complex)

### Known Limitations
- No tail call optimization (currently being implemented)
- Limited standard library compared to full R7RS
- No module system yet
- Basic error messages without source location tracking

## Examples

### Basic Arithmetic and Lists
```scheme
(define numbers (list 1 2 3 4 5))
(define sum (lambda (lst)
              (if (nil? lst)
                  0
                  (+ (car lst) (sum (cdr lst))))))
(sum numbers)  ; => 15
```

### Higher-Order Functions
```scheme
(define map (lambda (f lst)
              (if (nil? lst)
                  '()
                  (cons (f (car lst))
                        (map f (cdr lst))))))

(map (lambda (x) (* x x)) (list 1 2 3 4))  ; => (1 4 9 16)
```

### Macros
```scheme
(define unless (macro (test body)
                 (list 'if test #f body)))

(unless (> 3 5) (display "3 is not greater than 5"))
```

## Contributing

S1 is under active development. Contributions are welcome, particularly in areas of:
- Tail call optimization implementation
- Standard library expansion
- Performance improvements
- Documentation and examples
- Test coverage

## License

This project follows standard open source practices. See the repository for specific licensing terms.