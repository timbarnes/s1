;; exception_tests.scm: raise, with-exception-handler, guard and error
;; objects (R7RS 6.11). Also re-run under gc-threshold 1 by gc_stress_tests.scm.

(display "          === Testing exceptions ===")
(newline)

;; --- raise-continuable and with-exception-handler
(test-equal 65
    (with-exception-handler (lambda (c) 42) (lambda () (+ (raise-continuable 'oops) 23)))
    "raise-continuable returns the handler's value")
(test-equal '(caught oops)
    (call/cc (lambda (k)
      (with-exception-handler (lambda (c) (k (list 'caught c))) (lambda () (raise 'oops)))))
    "handler escapes with a continuation")
(test-equal 'outer
    (call/cc (lambda (k)
      (with-exception-handler
        (lambda (c) (k 'outer))
        (lambda ()
          (with-exception-handler
            (lambda (c) (raise 'from-inner-handler))
            (lambda () (raise 'x)))))))
    "an error inside a handler goes to the outer handler")
(test-equal 'not-called
    (call/cc (lambda (k)
      (with-exception-handler
        (lambda (c) (k 'not-called))
        (lambda ()
          (with-exception-handler (lambda (c) (k 'stale-handler)) (lambda () 'returned))
          (raise 'later)))))
    "a handler is uninstalled when its thunk returns")
(test-equal 'outer-only
    (call/cc (lambda (k)
      (with-exception-handler
        (lambda (c) (k 'outer-only))
        (lambda ()
          (call/cc (lambda (escape)
            (with-exception-handler (lambda (c) (k 'stale)) (lambda () (escape 'out)))))
          (raise 'after-escape)))))
    "escaping out of a handler's extent uninstalls it")

;; --- built-in failures are error objects
(test-equal "car: argument must be a pair"
    (guard (e ((error-object? e) (error-object-message e))) (car 1))
    "built-in error is an error object with a message")
(test-equal '() (guard (e (#t (error-object-irritants e))) (car 1)) "built-in error has no irritants")
(test-equal 'arity (guard (e (#t 'arity)) ((lambda (x) x))) "wrong argument count is catchable")
(test-equal 'non-procedure (guard (e (#t 'non-procedure)) (1 2)) "applying a non-procedure is catchable")
(test-equal 'unbound (guard (e (#t 'unbound)) undefined-variable-xyz) "unbound variable is catchable")

;; --- error
(test-equal "boom" (guard (e (#t (error-object-message e))) (error "boom" 1 2)) "error message")
(test-equal '(1 2) (guard (e (#t (error-object-irritants e))) (error "boom" 1 2)) "error irritants")
(test-equal '(#t #f #f) (guard (e (#t (list (error-object? e) (file-error? e) (read-error? e)))) (error "x"))
    "error makes a plain error object")
(test-equal #f (error-object? 'sym) "error-object? of a non-error")

;; --- file and read errors
(test-equal '(#t ("/no/such/file"))
    (guard (e ((file-error? e) (list #t (error-object-irritants e)))) (open-input-file "/no/such/file"))
    "open-input-file failure is a file error naming the file")
(define read-error-file "tests/read-error-test.tmp")
(let ((out (open-output-file read-error-file)))
  (display ")" out)
  (close-output-port out))
(test-equal #t
    (guard (e ((read-error? e) #t)) (read (open-input-file read-error-file)))
    "read of malformed input is a read error")

;; --- non-continuable raise
(test-equal "exception handler returned from a non-continuable raise of"
    (guard (e ((error-object? e) (error-object-message e)))
      (with-exception-handler (lambda (c) 'returned) (lambda () (raise 'x))))
    "returning from a raise handler is a secondary error")

;; --- guard
(test-equal '(caught boom) (guard (e (#t (list 'caught e))) (raise 'boom)) "guard catches")
(test-equal 'str (guard (e ((symbol? e) 'sym) ((string? e) 'str)) (raise "x")) "guard picks the matching clause")
(test-equal 42 (guard (e ((assq 'a e) => cdr) ((assq 'b e))) (raise (list (cons 'a 42)))) "guard => clause")
(test-equal '(b . 23) (guard (e ((assq 'a e) => cdr) ((assq 'b e))) (raise (list (cons 'b 23)))) "guard test-only clause")
(test-equal 'else (guard (e ((string? e) 'str) (else 'else)) (raise 1)) "guard else clause")
(test-equal 'outer (guard (e (#t 'outer)) (guard (e ((string? e) 'inner)) (raise 'not-a-string)))
    "unmatched guard re-raises to the outer guard")
(test-equal 11
    (with-exception-handler (lambda (c) 10)
      (lambda () (+ 1 (guard (e ((string? e) 'no)) (raise-continuable 5)))))
    "unmatched guard re-raises continuably in the original context")
(test-equal '(1 2) (call-with-values (lambda () (guard (e (#t 0)) (values 1 2))) list) "guard body returns multiple values")
(test-equal 7 (guard (e (#t 'no)) (+ 3 4)) "guard with no raise returns the body's value")
(test-equal '(mine mine2 r)
    (let ((condition 'mine) (args 'mine2)) (guard (e (#t (list condition args e))) (raise 'r)))
    "guard's temporaries don't capture user variables")
(define guard-log '())
(test-equal '(g (before after handled))
    (let ((v (guard (e (#t (set! guard-log (cons 'handled guard-log)) 'g))
               (dynamic-wind (lambda () (set! guard-log (cons 'before guard-log)))
                             (lambda () (raise 'x))
                             (lambda () (set! guard-log (cons 'after guard-log)))))))
      (list v (reverse guard-log)))
    "guard unwinds dynamic-wind before running its clauses")
(define (reraise-test v)
  (call/cc (lambda (k)
    (with-exception-handler
      (lambda (x) (k (list 'reraised x)))
      (lambda ()
        (guard (condition ((positive? condition) 'positive) ((negative? condition) 'negative))
          (raise v)))))))
(test-equal '(positive negative (reraised 0)) (map reraise-test '(1 -1 0)) "SRFI-34 re-raise examples")

;; --- cond => passes the test value, not a re-evaluation of it
(test-equal 2 (cond ((assq 'b '((a 1) (b 2))) => cadr) (else 'no)) "cond => with a list value")

;; --- an uncaught error unwinds dynamic-wind before abandoning the form
(define uncaught-after-ran #f)
(display "          (the next error is expected)")
(newline)
(dynamic-wind (lambda () #f)
              (lambda () (raise 'deliberately-uncaught))
              (lambda () (set! uncaught-after-ran #t)))
(test-equal #t uncaught-after-ran "uncaught error runs dynamic-wind after thunks")
