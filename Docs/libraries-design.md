[Home](s1-docs.md)

# Libraries and environments: design (phase 9)

Status: implemented (9a to 9g). User documentation: [libraries.md](./libraries.md) and [environments.md](./environments.md).
Built on the binding cells of commit 7300841 (`BindingCell` in `src/env.rs`).

## Goal

R7RS-small sections 5.2 (`import`), 5.6 (libraries), 6.12 (environments and
evaluation) and 4.2.1's `cond-expand`:

* `define-library` with `export`, `import`, `begin`, `include`, `include-ci`,
  `include-library-declarations` and `cond-expand` declarations.
* `import` with `only`, `except`, `prefix` and `rename`.
* The standard libraries: `(scheme base)`, `(scheme char)`, `(scheme write)`,
  ... as importable libraries.
* `environment`, `interaction-environment`, and `eval` (and `load`) with an
  environment argument. `scheme-report-environment` and `null-environment`
  from `(scheme r5rs)`.
* `include` and `include-ci` as ordinary syntax, `cond-expand` and `features`.
* Library files: `(import (foo bar))` finds and loads `foo/bar.sld`.

Existing programs and the REPL keep working without any `import`.

## Decisions (agreed)

1. **Imports share cells.** An imported name denotes the library's own
   variable: if the library later assigns it, importers see the new value.
2. **Bindings are marked imported or local.** `define` of an imported name
   gives it a fresh local cell, shadowing the import without touching the
   library. `set!` of an imported name is an error. A library's own `set!` of
   its variable is allowed and importers see it.
3. **The REPL starts with everything**, as now. `import` there adds or
   replaces names.
4. **`environment` returns immutable environments**: `define` and `set!` in
   them are errors.
5. **Library files** are `<name parts joined by />.sld`, searched for in
   the directories in `S1_LIBRARY_PATH` (colon separated), then `.`, then
   `scheme/lib`.

## The environments

Three kinds of top-level environment, all ordinary top-level frames
(`Frame::new(None)`, whose names map to cells):

* **The system environment** holds every built-in procedure and special form
  and everything `s1-core.scm` defines. It is never the environment user code
  runs in. The standard libraries are views of it (below), and `core_id` /
  `core_form` resolve against it (`GcHeap::set_global_env`), so the code s1's
  own rewrites generate always finds the real `lambda`, `let`, `begin`.
* **The interaction environment** is where the REPL, loaded files and the
  regression suite run. At startup, after `s1-core.scm` has loaded, it is
  created by importing every system binding: each name is bound to the system
  cell and marked imported.
* **Library environments**, one per `define-library`, start empty and contain
  only what the library imports and defines.

The interaction environment is a flat frame of imported cells, not a child
of the system environment. A child frame would make `define` shadow
correctly, but `set!` would find and change the system's binding, and every
global lookup would search two hash tables instead of one. With a flat frame,
lookups cost what they cost today: one hash probe and a cell read.

### Consequences for existing behaviour

* Redefining a built-in at the REPL, `(define (map f l) ...)`, no longer
  changes the `map` that `s1-core.scm` procedures use. They keep the system's.
  This is the library semantics R7RS intends; today a REPL redefinition can
  break core procedures.
* `(set! car ...)` at the REPL becomes an error ("set!: car is imported").
  `(define car ...)` still works.
* Startup creates one cell reference per system binding (a few hundred
  `Rc` clones), which is not measurable.

Both changes will be checked against the regression and conformance suites
in step 9b. If a test depends on the old behaviour, it is the test that's
reconsidered, not the decision, unless it reveals something unexpected.

## Representation

* **Top-level bindings.** `Bindings::Large` maps a name to
  `{ cell: CellRef, imported: bool }`. `define` on a local binding sets the
  cell; on an imported one it replaces it with a fresh local cell. `set!`
  checks the flag (`set_sf` and its immediate path). Local frames are
  unchanged.
* **Immutable frames.** `Frame` gains a `mutable` flag, false for frames made
  by `environment` (and `scheme-report-environment`, `null-environment`).
  `define_sf` and `set_sf` check it.
* **Environment values.** A new `SchemeValue::Environment(EnvRef)` holds a
  top-level frame. It prints as `#<environment>`, `type-of` gives
  `environment`, and the GC marks its frame.
* **Libraries.** A registry (`src/libraries.rs`), keyed by library name (the
  name list as a key such as `["scheme", "base"]`; numbers allowed as
  parts), holds for each library its export table (external name to cell)
  and, for a `define-library`, its environment. A standard library has no
  environment of its own: its export table holds system cells. The registry
  lives in the heap, beside the system environment, so built-in procedures
  such as `environment` can reach it.
* **GC.** The system environment, the interaction environment and each
  library environment are roots. Marking skips imported bindings (only
  their names are marked), because an imported cell always belongs to one of
  those root environments, which marks it. Without this, every collection
  would walk the interaction environment's several hundred imports a second
  time, which cost about 6% on the regression suite.

## Standard libraries

A Rust table (`src/libraries.rs`) lists the names each R7RS standard library
exports, taken from R7RS appendix A: `(scheme base)`, `case-lambda`, `char`,
`complex`, `cxr`, `eval`, `file`, `inexact`, `lazy`, `load`,
`process-context`, `read`, `repl`, `time`, `write`, `r5rs`. At startup each
becomes an export table of the system cells for those names.

* A name s1 doesn't implement (`make-rectangular`, `exit` until phase 10) is
  left out of the table rather than failing the import. A regression test
  lists the missing names, which is also the checklist for the phase 11 audit.
* `(s1)` exports every system binding that no standard library exports: s1's
  extensions (`macro`, `help`, `push!`, `type-of`, ...).
* Syntax is exported the same way as procedures. In s1, special forms and
  `syntax-rules` transformers are values in the environment, so `if` and
  `when` are just cells in `(scheme base)`'s table.

## `import`

`(import import-set ...)` is a special form, valid at top level (the REPL, a
program, a library's declarations). Each import set is resolved to a list of
(name, cell) pairs:

* `(library name)`: the library's export table.
* `(only set id ...)`, `(except set id ...)`, `(prefix set prefix)`,
  `(rename set (from to) ...)`: filters and renames, applied in order.
  Naming an identifier the set doesn't contain is an error.

Then each name is bound to its cell with `bind_cell`, marked imported. At the
REPL, importing a name that's already bound replaces it. Inside a library,
importing one name with two different cells is an error, as R7RS requires.

An unknown library is looked up on disk (see "Library files"). The search
happens before any binding is made, so a failed import binds nothing.

## `define-library`

`(define-library name declaration ...)` is a special form, valid at top
level. It:

1. Processes the declarations in order. `cond-expand` declarations are
   resolved in place, and `include-library-declarations` reads its files as
   further declarations. `import` declarations import into a new library
   environment. `begin`, `include` and `include-ci` collect body forms.
   `export` collects `name` and `(rename internal external)` specs.
2. Gets each export's cell from the library environment with `cell`. A name
   not yet defined gets an unbound cell, which the body's `define` fills.
3. Evaluates the body forms in order in the library environment, through the
   machine, with the caller's environment restored afterwards, as `eval` does.
4. After the body, a final step checks that every export is bound ("define-
   library: (foo) exports x, which it doesn't define") and only then adds
   the library to the registry. A library whose body fails is not
   registered, so it can be fixed and reloaded.

Step 4 runs as a last internal form appended to the body (a call to a
system procedure), so no new continuation frame type is needed. As built,
step 2 moved into step 4: the export cells are looked up when the body has
finished, which finds the same cells and needs no unbound ones.

### Macros and hygiene

A `syntax-rules` macro defined in a library captures the library environment,
as macros capture their definition environment today. Its expansion's
aliases resolve there. So an exported macro may expand into uses of the
library's unexported helpers, and the importer doesn't need to import what
the expansion uses. This needs no new mechanism, only tests.

`macro` (s1's non-hygienic form) is different by nature: its expansion's
names resolve where it is used. A library that uses `push!` from `(s1)` needs
`set!` and `cons` in scope, normally from `(scheme base)`. This will be
documented, not changed.

## Environments and `eval`

* `(interaction-environment)` returns the interaction environment.
* `(environment import-set ...)` returns a new immutable environment
  containing exactly those imports.
* `(eval expr env)` evaluates `expr` in `env` (a top-level frame), restoring
  the caller's environment afterwards. Today's `eval` evaluates its second
  argument a second time, as an expression, and otherwise ignores it. That
  will be fixed. One-argument `eval` stays as an s1 extension and evaluates
  in the current environment.
* `(load filename [env])`: the optional environment argument, defaulting to
  the interaction environment.
* `(scheme-report-environment 5)` and `(null-environment 5)` are
  `environment` of `(scheme r5rs)` and of its syntax only.

## `include`, `cond-expand`, `features`

* `(include file ...)` and `(include-ci file ...)` read the files' forms
  (case-folded for `include-ci`) and evaluate them as a `begin` where the
  `include` appears. In a library, relative file names are relative to the
  library file's directory.
* `(cond-expand clause ...)` at top level and in libraries. Requirements are
  feature identifiers, `(library name)` (true if the library is registered
  or can be found on disk), `(and ...)`, `(or ...)`, `(not ...)` and `else`.
* `(features)` returns s1's feature list: `r7rs`, `exact-closed`,
  `ieee-float`, `ratios`, `s1`, and the OS, OS family (plus `posix` on
  unix), architecture and byte order (`linux`, `unix`, `posix`, `x86-64`,
  `little-endian`). Not `exact-complex`, since s1 has no complex numbers;
  `full-unicode` waits for the phase 11 audit to confirm it.

## Library files

`(import (foo bar))` for a library not in the registry searches the path for
`foo/bar.sld`. If found, the file is evaluated in the interaction environment
(it normally contains one `define-library`), and the import retries. If the
library still isn't registered, the import fails ("import: foo/bar.sld does
not define (foo bar)"). A file is loaded at most once per run.

Because `import` runs inside the machine, loading the file before binding is
done by rewriting the import: `(begin (%load-library "path") (import ...))`.
`define-library`'s own imports are handled the same way. A file is marked
loaded before it is evaluated, so a library that imports itself can't loop.
As built, `include` file names in a library file's `define-library` are made
relative to the file's directory by rewriting them as the file is read, which
needs no "current file" state. `environment`, a plain procedure, doesn't
search for files.

## Steps

Each is tested and committed separately. The regression and conformance
suites must pass after each.

| Step | Contents | Size |
|---|---|---|
| 9a | `Environment` values; `interaction-environment`; `eval` with an environment (and its double-evaluation fix); `load` with an environment | small |
| 9b | System and interaction environments; imported flag; `set!`/`define` rules; standard library tables and `(s1)`; registry; `environment` with plain library names; immutable frames | medium |
| 9c | `import` and full import sets; `environment` with import sets; `scheme-report-environment`, `null-environment` | medium |
| 9d | `define-library`; `include`, `include-ci`, `include-library-declarations`; export checking | largest |
| 9e | Library files and the search path | small |
| 9f | `cond-expand` and `features` | small |
| 9g | Conformance: drop the `import` stub, enable 6.12; `Docs/libraries.md`; `todo.md` | small |

### Tests

Regression tests cover, among others:

* An imported variable that the library later `set!`s is seen updated by the
  importer, and a REPL `define` of an imported name doesn't change the library.
* `set!` of an imported name, and `define` in an `environment` result, are
  errors.
* Import set combinations, and errors for names a set doesn't contain.
* A library that sees only its imports: using an unimported name is an
  unbound-variable error.
* An exported `syntax-rules` macro using an unexported helper.
* A library whose body fails isn't registered; an export that is never
  defined is reported.
* `eval` in a library's or an `environment`'s scope, and in the interaction
  environment.
* A library file found on the path and loaded once.
* `cond-expand` with each requirement form.

## Risks

* **Code that relied on REPL redefinition reaching core procedures.** This
  is the one behaviour change. It will be found in 9b if the suites rely on it.
* **Names the R7RS tables list but s1 binds under different semantics.**
  These show up in the missing-names test or in conformance, and go to the
  phase 11 audit.
* **Continuations captured in a library body** and resumed later run in the
  library environment, since frames already carry their environment. This is
  covered by tests rather than design.
* **`core_id` aliases** resolve in the system environment. Library code that
  shadows `lambda` locally is already handled by hygiene, but it gets a test.

[Home](s1-docs.md)
