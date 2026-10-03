# R7RS conformance suite

The yardstick for s1's R7RS-small work.

| File | What it is |
| --- | --- |
| `r7rs-tests.scm` | chibi-scheme's R7RS test suite, vendored **unmodified** from [ashinn/chibi-scheme](https://github.com/ashinn/chibi-scheme) `tests/r7rs-tests.scm` at commit `c4e7367e867428889d8fe898a0b39f42e418b3f1`. BSD licence: `LICENSE-chibi`. |
| `shim.scm` | Stand-in for `(chibi test)`: `test`, `test-assert`, `test-values`, `test-error`, `test-begin`, `test-end`. Written with s1's `macro` form. |
| `run.sh` | Runner. Splits the suite into its 20 sections, runs each in a fresh s1 process after the suite's own `import` header (less `(chibi test)`, which the shim replaces), prints a table and compares it with the baseline. |
| `baseline.txt` | Pass counts per section from the last `--update`. |
| `last-run.log` | Full output of the last run (git-ignored). Search it for `FAIL:` and `ERROR:` lines. |

## Running

```bash
tests/r7rs/run.sh            # exits 1 if any section passes fewer tests than baseline.txt
tests/r7rs/run.sh --update   # accept the current results as the new baseline
```

Run `--update` in the same commit as the work that changes the counts, so the
baseline stays in step with the code. `S1_BIN` selects a prebuilt binary and
`S1_TIMEOUT` the per-section timeout in seconds (default 15).

## Reading the table

- **pass / fail**: the test ran and its result did or did not match. Inexact
  numbers match to a relative 1e-5, as in `(chibi test)`.
- **error**: the test's expression raised an exception. The shim catches it
  with `guard`, so the next test runs normally; the `ERROR:` line in the log
  shows what was raised. (For `test-error`, raising is a pass.)
- **unrch** (unreached): tests in the section that never started, usually
  because something outside any test failed first (a `define-syntax`, or a
  definition in an enclosing `let`). It is `~total - attempted`.
- **~total**: a static count of test forms per section. It is an estimate
  (helpers defined inside `define-syntax` templates are counted too), so trust
  `pass` as the metric to drive up.

Each section runs in its own process, so a reader desync or crash in one
section cannot swallow the next ones.

## Known gaps (1154 passing after phase 10)

- **Complex numbers** are not supported and are reported as parse errors. They
  account for nearly all unreached tests (in 6.2 Numbers and Numeric syntax).
