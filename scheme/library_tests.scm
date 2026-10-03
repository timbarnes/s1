;; library_tests.scm: environments, eval, and (from phase 9b on) libraries.
;; See Docs/libraries-design.md.

(display "          === Testing environments and eval ===")
(newline)

(test-equal 21 (eval '(* 7 3) (interaction-environment)) "eval in the interaction environment")
(test-equal 'a (eval ''a (interaction-environment)) "eval evaluates its expression once")
(test-equal '(x "s") (eval '(list 'x "s") (interaction-environment)) "eval of a call")
(test-equal 3 (eval '(+ 1 2)) "eval without an environment (s1 extension)")
(test-equal 3 (let ((local-q 3)) (eval 'local-q)) "one-argument eval sees the caller's locals")
(test-equal 'unbound
    (let ((local-x 1)) (guard (e (#t 'unbound)) (eval 'local-x (interaction-environment))))
    "the interaction environment doesn't see the caller's locals")
(test-equal "eval: the second argument must be an environment"
    (guard (e (#t (error-object-message e))) (eval '(* 7 3) 'not-an-environment))
    "eval rejects a non-environment")
(test-equal '("#<environment>" environment)
    (let ((out (open-output-string)))
      (write (interaction-environment) out)
      (list (get-output-string out) (type-of (interaction-environment))))
    "environments print opaquely")

;; define through eval binds in that environment, and the caller's
;; environment is restored afterwards, also after an error.
(test-equal '(5 42)
    (let ((y 5))
      (eval '(define eval-defined 42) (interaction-environment))
      (list y eval-defined))
    "eval's define binds at top level and the caller's environment is restored")
(test-equal 6
    (let ((y 6)) (guard (e (#t y)) (eval '(car 1) (interaction-environment))))
    "an error inside eval leaves the caller's environment intact")
(test-equal 7
    (let ((k-result (call/cc (lambda (k) (eval `(,k 7) (interaction-environment)))))) k-result)
    "a continuation called from inside eval")

;; load with an environment runs the file before returning
(define library-test-file "tests/library-test.tmp")
(call-with-output-file library-test-file
  (lambda (p) (write '(define loaded-value (* 6 7)) p) (write '(define loaded-twice (* 2 loaded-value)) p)))
(test-equal '(42 84)
    (let ((unused 0))
      (load library-test-file (interaction-environment))
      (list loaded-value loaded-twice))
    "load with an environment evaluates the file before returning")
(delete-file library-test-file)

(display "          === Testing standard libraries and environment ===")
(newline)

(test-equal 3 (eval '(+ 1 2) (environment '(scheme base))) "eval in an environment of (scheme base)")
(test-equal #\A (eval '(char-upcase #\a) (environment '(scheme base) '(scheme char)))
    "an environment of two libraries")
(test-equal 'unbound
    (guard (e (#t 'unbound)) (eval '(char-upcase #\a) (environment '(scheme base))))
    "an environment holds only what its libraries export")
(test-equal 'unbound
    (guard (e (#t 'unbound)) (eval '(type-of 1) (environment '(scheme base))))
    "s1's extensions aren't in (scheme base)")
(test-equal 'integer (eval '(type-of 1) (environment '(s1))) "(s1) exports s1's extensions")
(test-equal "define: this environment is immutable"
    (guard (e (#t (error-object-message e))) (eval '(define x 1) (environment '(scheme base))))
    "an environment from environment is immutable")
(test-equal "set!: car is imported and can't be assigned"
    (guard (e (#t (error-object-message e))) (eval '(set! car cdr) (environment '(scheme base))))
    "set! in an environment from environment")
(test-equal 6 (eval '(let () (define x 6) x) (environment '(scheme base)))
    "local definitions in an immutable environment")
(test-equal "environment: unknown library (no such library)"
    (guard (e (#t (error-object-message e))) (environment '(no such library)))
    "an unknown library")
(test-equal '(case-lambda) (library-exports '(scheme case-lambda)) "library-exports")
(test-equal '(#t #t) (list (and (member '(scheme base) (library-names)) #t) (and (member '(s1) (library-names)) #t))
    "library-names")

;; What R7RS assigns the standard libraries that s1 doesn't define yet: the
;; checklist for the rest of phase 9 and the phase 11 audit.
(test-equal '(((scheme base) ("..." "=>" "_" "cond-expand" "else" "features" "include" "include-ci" "syntax-error"))
              ((scheme complex) ("angle" "imag-part" "magnitude" "make-polar" "make-rectangular" "real-part"))
              ((scheme cxr) ("caaaar" "caadar" "cadaar" "caddar" "cdaaar" "cdadar" "cddaar" "cdddar"))
              ((scheme process-context) ("command-line" "emergency-exit" "get-environment-variable" "get-environment-variables"))
              ((scheme r5rs) ("angle" "caaaar" "caadar" "cadaar" "caddar" "cdaaar" "cdadar" "cddaar" "cdddar" "imag-part" "magnitude" "make-polar" "make-rectangular" "real-part"))
              ((scheme time) ("current-jiffy" "current-second" "jiffies-per-second")))
    (let loop ((names (library-names)) (acc '()))
      (cond ((null? names) (reverse acc))
            ((null? (%library-unimplemented (car names))) (loop (cdr names) acc))
            (else (loop (cdr names) (cons (list (car names) (%library-unimplemented (car names))) acc)))))
    "standard names s1 doesn't define yet")

(display "          === Testing the interaction environment's imports ===")
(newline)

;; The REPL's names are imported from the system: defining one shadows it
;; without changing the system's, which s1-core's procedures keep using.
(define saved-empty? empty?)
(define (empty? s) 'shadowed)
(test-equal '(shadowed 1) (list (empty? '(1)) (top '(1)))
    "a REPL define shadows a built-in without affecting s1-core")
(define empty? saved-empty?)
(test-equal "set!: car is imported and can't be assigned"
    (guard (e (#t (error-object-message e))) (set! car cdr))
    "set! of an imported name is an error")
(test-equal 1 (car '(1 2)) "and leaves it unchanged")
(define repl-defined 1)
(set! repl-defined 2)
(test-equal 2 repl-defined "set! of a REPL definition")
(test-equal 'outer
    (let ((car (lambda (x) 'outer))) (set! car (lambda (x) 'outer)) (car 1))
    "set! of a local that shadows an import")

(display "          === Testing import ===")
(newline)

(import (prefix (scheme char) c:))
(test-equal #\A (c:char-upcase #\a) "import with prefix")
(import (rename (only (scheme base) car cdr) (car first) (cdr rest-of)))
(test-equal '(1 (2)) (list (first '(1 2)) (rest-of '(1 2))) "import with only and rename")
(test-equal "set!: first is imported and can't be assigned"
    (guard (e (#t (error-object-message e))) (set! first cdr))
    "a name bound by import can't be assigned")
(test-equal 'unbound
    (guard (e (#t 'unbound)) (eval '(car '(1)) (environment '(except (scheme base) car))))
    "environment with except")
(test-equal '(1 #\B)
    (eval '(list (head '(1)) (up #\b))
          (environment '(rename (only (scheme base) car list) (car head))
                       '(prefix (only (scheme char) char-upcase) x-)
                       '(rename (prefix (only (scheme char) char-upcase) x-) (x-char-upcase up))))
    "environment with combined import sets")
(test-equal "environment: no-such is not in the import set (scheme base)"
    (guard (e (#t (error-object-message e))) (environment '(only (scheme base) car no-such)))
    "only of a missing name is an error")
(test-equal "import: unknown library (no such)"
    (guard (e (#t (error-object-message e)))
      (eval '(import (scheme base) (no such)) (interaction-environment)))
    "importing an unknown library is an error")
(test-equal "import: only allowed at top level"
    (guard (e (#t (error-object-message e))) (let () (import (scheme base)) 1))
    "import isn't allowed in a body")
(test-equal "import: this environment is immutable"
    (guard (e (#t (error-object-message e))) (eval '(import (scheme char)) (environment '(scheme base))))
    "import into an immutable environment")
;; A REPL definition can be replaced by an import, and vice versa.
(define local-then-imported 'local)
(import (rename (only (scheme base) car) (car local-then-imported)))
(test-equal 1 (local-then-imported '(1)) "import replaces a definition")
(define local-then-imported 'local-again)
(test-equal 'local-again local-then-imported "a definition replaces an import")

(test-equal 21 (eval '(* 7 3) (scheme-report-environment 5)) "scheme-report-environment")
(test-equal '(1 unbound)
    (list (eval '(if #t 1 2) (null-environment 5))
          (guard (e (#t 'unbound)) (eval '(car '(1)) (null-environment 5))))
    "null-environment has only syntax")
(test-equal "null-environment: the only version supported is 5"
    (guard (e (#t (error-object-message e))) (null-environment 7))
    "R5RS environments of other versions")
