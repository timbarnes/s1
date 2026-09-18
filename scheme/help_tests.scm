;; help_tests.scm
;;
;; Regression coverage for the (help 'sym) documentation system:
;;   - the doc string baked into Builtin/SysBuiltin/SpecialForm callables
;;   - add-doc, a symbol-keyed side table independent of what (if anything)
;;     a symbol is bound to, which always takes priority over a value's own
;;     doc when both exist
;;   - the lambda/define/macro leading-docstring convention (a string
;;     literal right after the parameter list, only when at least one more
;;     body form follows it)

(display "          === Testing help: builtin doc strings ===")
(newline)

(test-equal "(car pair) -> first element of pair" (help 'car)
    "help returns car's built-in doc string")
(test-equal "(gc-threshold [new-threshold]) Get or set the GC threshold" (help 'gc-threshold)
    "help returns a plain-Builtin's doc string")

(display "          === Testing add-doc on unbound and bound symbols ===")
(newline)

(test-true (eq? (void) (add-doc 'my-documented-global "a global counter"))
    "add-doc returns void")
(test-equal "a global counter" (help 'my-documented-global)
    "help finds a doc added to a symbol that was never bound")

(add-doc 'car "overridden car doc")
(test-equal "overridden car doc" (help 'car)
    "add-doc overrides a builtin's own doc string")
(add-doc 'car "(car pair) -> first element of pair") ;; restore for later tests

(display "          === Testing lambda/define leading docstrings ===")
(newline)

(define (doc-square x)
  "squares x"
  (* x x))
(test-equal "squares x" (help 'doc-square)
    "define with a docstring: doc is extracted")
(test-equal 49 (doc-square 7)
    "define with a docstring: body still evaluates correctly, docstring is not part of it")

(define doc-lambda-direct
  (lambda (x) "adds one to x" (+ x 1)))
(test-equal "adds one to x" (help 'doc-lambda-direct)
    "lambda with a docstring bound via define: doc is extracted")
(test-equal 6 (doc-lambda-direct 5)
    "lambda with a docstring: calling it still runs the real body")

;; A body that is *only* a string literal is the return value, not a
;; docstring - there'd be no body left if it were treated as one.
(define (doc-single-string)
  "just a string")
(test-equal "just a string" (doc-single-string)
    "single-string body is the return value, not mistaken for a docstring")
(test-equal "doc-single-string: no documentation available" (help 'doc-single-string)
    "a closure with no docstring reports no documentation available")

(display "          === Testing add-doc overriding a closure's own docstring ===")
(newline)

(define (doc-override-target x)
  "the closure's own docstring"
  x)
(test-equal "the closure's own docstring" (help 'doc-override-target)
    "closure doc before any add-doc override")
(add-doc 'doc-override-target "manually overridden doc")
(test-equal "manually overridden doc" (help 'doc-override-target)
    "add-doc overrides a closure's own leading docstring")

(display "          === Testing macro leading docstrings ===")
(newline)

(define doc-macro-example
  (macro (x) "a trivial identity macro" x))
(test-equal "a trivial identity macro" (help 'doc-macro-example)
    "macro with a docstring: doc is extracted")
(test-equal 99 (doc-macro-example 99)
    "macro with a docstring: it still expands/evaluates correctly")

(display "          === Testing documentation survives garbage collection ===")
(newline)

(gc)
(test-equal "a global counter" (help 'my-documented-global)
    "add-doc'd documentation for an unbound symbol survives gc")
(test-equal "squares x" (help 'doc-square)
    "a closure's leading-docstring doc survives gc")
(test-equal "manually overridden doc" (help 'doc-override-target)
    "an add-doc override survives gc")
