;; regression.scm: Simple test framework for s1 Scheme interpreter
;; This can be run directly in the REPL without file loading

;; Simple test framework - run each test manually
;; Example usage:
;; (define test-result (test-equal 42 42 "Integer equality"))
;; (define test-result (test-true (number? 42) "Number predicate"))

;(gc-threshold 10000)

;; Test helper functions
(define **failed-tests** '()) ; keep track of test failures
(define **counter** 0)
(define **print-successes** #t)

(define fails
  (lambda (message)
      (set! **failed-tests** (cons (cons **counter** message) **failed-tests**))))

(define success
  (lambda (message)
    (if **print-successes**
        (begin
            (display **counter**)
            (display " PASS: ")
            (display message)
            (newline)
            #t)
        #f)))

(define failure
    (lambda (message expected actual)
        (fails message)
        (newline)
        (display **counter**)
        (display "    ***    FAIL: ")
        (display message)
        (display " - expected ")
        (display expected)
        (display ", received ")
        (display actual)
        (newline)
        (newline)
        #f))

(define test-equal
  (lambda (expected actual message)
    (set! **counter** (+ **counter** 1))
    (if (equal? expected actual)
        (success message)
        (failure message expected actual))))

(define test-true
  (lambda (value message)
    (test-equal #t value message)))

(define test-false
  (lambda (value message)
    (test-equal #f value message)))

(define test-number
  (lambda (value message)
    (set! **counter** (+ **counter** 1))
    (if (number? value)
        (success message)
        (failure message "a number" value))))

(define test-symbol
  (lambda (value message)
    (set! **counter** (+ **counter** 1))
    (if (symbol? value)
        (success message)
        (failure message "a symbol" value))))

;; Simple nil test using eq? directly
(define test-nil
  (lambda (value message)
    (set! **counter** (+ **counter** 1))
    (if (eq? value '())
        (success message)
        (failure message "nil" value))))

;; Run all tests individually
(display "          Starting regression tests...")
(newline)

(displayln "         =====Basic Tests=====")
(load "scheme/basic_tests.scm")

(displayln "         =====Advanced Tests=====")
(load "scheme/advanced_tests.scm")

(display "          === All tests completed ===")
(newline)
(if (null? **failed-tests**)
    (displayln "===  All" **counter** "tests passed!  ===")
    (begin
        (displayln "Failing tests: ")
        (displayln "==============")
        (map displayln **failed-tests**)))
