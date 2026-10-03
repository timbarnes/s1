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
