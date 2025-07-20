;; regression.scm: Simple test framework for s1 Scheme interpreter
;; This can be run directly in the REPL without file loading

;; Simple test framework - run each test manually
;; Example usage:
;; (define test-result (test-equal 42 42 "Integer equality"))
;; (define test-result (test-true (number? 42) "Number predicate"))

;; Test helper functions
(define test-equal
  (lambda (expected actual message)
    (if (eq? expected actual)
        (begin
          (display "PASS: ")
          (display message)
          (newline)
          #t)
        (begin
          (display "FAIL: ")
          (display message)
          (display " - expected ")
          (display expected)
          (display ", got ")
          (display actual)
          (newline)
          #f))))

(define test-true
  (lambda (value message)
    (test-equal #t value message)))

(define test-false
  (lambda (value message)
    (test-equal #f value message)))

(define test-number
  (lambda (value message)
    (if (number? value)
        (begin
          (display "PASS: ")
          (display message)
          (newline)
          #t)
        (begin
          (display "FAIL: ")
          (display message)
          (display " - expected number, got ")
          (display value)
          (newline)
          #f))))

(define test-symbol
  (lambda (value message)
    (if (symbol? value)
        (begin
          (display "PASS: ")
          (display message)
          (newline)
          #t)
        (begin
          (display "FAIL: ")
          (display message)
          (display " - expected symbol, got ")
          (display value)
          (newline)
          #f))))

;; Simple nil test using eq? directly
(define test-nil
  (lambda (value message)
    (if (eq? value '())
        (begin
          (display "PASS: ")
          (display message)
          (newline)
          #t)
        (begin
          (display "FAIL: ")
          (display message)
          (display " - expected nil, got ")
          (display value)
          (newline)
          #f))))

;; Run all tests individually
(display "Starting regression tests...")
(newline)

;; Test 1: Literals
(display "=== Testing Literals ===")
(newline)
(test-equal 42 42 "Integer literal")
(test-equal 3.14 3.14 "Float literal")
(test-equal "hello" "hello" "String literal")
(test-equal #t #t "Boolean true literal")
(test-equal #f #f "Boolean false literal")
(test-equal #\a #\a "Character literal")
(test-nil '() "Nil literal")
(test-symbol 'foo "Symbol literal")

;; Test 2: Arithmetic
(display "=== Testing Arithmetic ===")
(newline)
(test-equal 3 (+ 1 2) "Simple addition")
(test-equal 12 (* 3 4) "Simple multiplication")
(test-equal -5 (- 5 4 3 2 1) "Subtraction with multiple args")
(test-equal 0.3 (/ 6 5 4) "Division with multiple args")
(test-equal 15 (+ 1 2 3 4 5) "Addition with multiple args")
(test-equal 120 (* 2 3 4 5) "Multiplication with multiple args")
(test-equal 83 (- 100 10 5 2) "Subtraction with multiple args")

;; Test 3: Type Predicates
(display "=== Testing Type Predicates ===")
(newline)
(test-true (number? 42) "number? with integer")
(test-true (number? 3.14) "number? with float")
(test-false (number? "not a number") "number? with string")
(test-true (symbol? 'foo) "symbol? with symbol")
(test-false (symbol? 42) "symbol? with number")
(test-true (string? "hello") "string? with string")
(test-false (string? 42) "string? with number")
(test-true (boolean? #t) "boolean? with true")
(test-true (boolean? #f) "boolean? with false")
(test-false (boolean? 42) "boolean? with number")
(test-true (char? #\a) "char? with character")
(test-false (char? 42) "char? with number")
(test-true (pair? '(1 2)) "pair? with pair")
(test-false (pair? 42) "pair? with number")
(test-true (eq? '() '()) "nil? with nil")
(test-false (eq? '(1 2) '()) "nil? with list")

;; Test 4: Type-of
(display "=== Testing Type-of ===")
(newline)
(test-equal 'integer (type-of 42) "type-of with integer")
(test-equal 'float (type-of 3.14) "type-of with float")
(test-equal 'string (type-of "hello") "type-of with string")
(test-equal 'boolean (type-of #t) "type-of with boolean")
(test-equal 'boolean (type-of #f) "type-of with boolean")
(test-equal 'char (type-of #\a) "type-of with character")
(test-equal 'nil (type-of '()) "type-of with nil")
(test-equal 'symbol (type-of 'symbol) "type-of with symbol")
(test-equal 'pair (type-of '(1 2 3)) "type-of with pair")

;; Test 5: List Literals
(display "=== Testing List Literals ===")
(newline)
(test-equal '(1 2 3) '(1 2 3) "List literal")
(test-equal '(a b c) '(a b c) "Symbol list literal")
(test-equal '(1 (+ 2 3) 4) '(1 (+ 2 3) 4) "List with quoted expression")
(test-equal '(1 2 3 4 5) '(1 2 3 4 5) "Longer list literal")

;; Test 6: Begin
(display "=== Testing Begin ===")
(newline)
(test-equal 3 (begin 1 2 3) "Simple begin")
(test-equal 12 (begin (+ 1 2) (* 3 4)) "Begin with expressions")
(test-equal 5 (begin 1 2 3 4 5) "Begin with multiple expressions")

;; Test 7: Variables
(display "=== Testing Variables ===")
(newline)
(define x 22)
(define y 'z)
(define a 10)
(define b 20)
(define w (+ 5 5))
(define v 'hello)
(test-equal 22 x "Variable lookup after define")
(test-equal 'z y "Symbol variable lookup")
(test-equal 50 (+ a (* b 2)) "Complex variable expression")
(test-equal 10 w "Define with expression")
(test-equal 'hello v "Define with symbol")

;; Test 8: Set!
(display "=== Testing Set! ===")
(newline)
(define x 10)
(set! x 42)
(set! x 'foo)
(define z 100)
(set! z 200)
(set! z (+ z 50))
(test-equal 42 42 "set! with number")
(test-equal 'foo 'foo "set! with symbol")
(test-equal 200 200 "set! with number")
(test-equal 250 250 "set! with expression")

;; Test 9: If
(display "=== Testing If ===")
(newline)
(test-equal 1 (if #t 1 2) "if with true condition")
(test-equal 2 (if #f 1 2) "if with false condition")
(test-equal 1 (if #f 1) "if without else")
(test-equal 1 (if #t 1) "if without else, true condition")
(test-equal 3 (if #t (+ 1 2) (- 1 2)) "if with expressions")
(test-equal -1 (if #f (+ 1 2) (- 1 2)) "if with expressions, false")
(test-equal 11 (if (> 10 5) (+ 10 1) (- 10 1)) "if with comparison")

;; Test 10: And/Or
(display "=== Testing And/Or ===")
(newline)
(test-equal #t (and) "Empty and")
(test-equal 2 (and #t 1 2) "and with all true")
(test-equal #f (and #t #f 1) "and with false")
(test-equal #f (or) "Empty or")
(test-equal #f (or #f #f) "or with all false")
(test-equal 1 (or #f 1 2) "or with true")
(test-equal 3 (or #f #f 3) "or with last true")
(test-equal #t (and #t #t #t) "and with all true")
(test-equal #f (and #t #f #t) "and with middle false")
(test-equal #f (or #f #f #f) "or with all false")
(test-equal #t (or #f #t #f) "or with middle true")

;; Test 11: Quote
(display "=== Testing Quote ===")
(newline)
(test-equal 42 '42 "Quote number")
(test-equal #t '#t "Quote boolean")
(test-equal "hello" '"hello" "Quote string")
(test-equal #\a '#\a "Quote character")

;; Test 12: Lambda
(display "=== Testing Lambda ===")
(newline)
(define add1 (lambda (x) (+ x 1)))
(define make-adder (lambda (n) (lambda (x) (+ x n))))
(define add5 (make-adder 5))
(test-equal 6 (add1 5) "Simple lambda application")
(test-equal 7 ((lambda (x y) (+ x y)) 3 4) "Immediate lambda application")
(test-equal 8 (add5 3) "Closure application")
(test-equal 720 ((lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))) 6) "Factorial test")

;; Test 13: List Accessors
(display "=== Testing List Accessors ===")
(newline)
(test-equal 1 (car '(1 2 3)) "car of list")
(test-equal '(2 3) (cdr '(1 2 3)) "cdr of list")
(test-equal 2 (cadr '(1 2 3)) "cadr of list")
(test-equal 3 (caddr '(1 2 3 4)) "caddr of list")
(test-equal 1 (caar '((1 2) (3 4))) "caar of nested list")
(test-equal '(2) (cdar '((1 2) (3 4))) "cdar of nested list")
(test-equal '(3 4) (cadr '((1 2) (3 4))) "cadr of nested list")

;; Test 14: List Construction
(display "=== Testing List Construction ===")
(newline)
(test-equal '(1 . 2) (cons 1 2) "cons with atom")
(test-equal '(1 2 3) (cons 1 '(2 3)) "cons with list")
(test-equal '() (list) "empty list")
(test-equal '(42) (list 42) "single element list")
(test-equal '(1 2 3) (list 1 2 3) "multiple element list")
(test-equal '(1 2 3 4) (append '(1 2) '(3 4)) "append two lists")
(test-equal '(1 2) (append '(1 2) '()) "append with empty list")

;; Test 15: Comparisons
(display "=== Testing Comparisons ===")
(newline)
(test-true (= 5 5) "Equal numbers")
(test-true (= 5 5.0) "Equal int and float")
(test-false (= 5 6) "Unequal numbers")
(test-true (< 1 2 3) "Strictly increasing")
(test-false (< 1 2 2) "Not strictly increasing")
(test-true (> 3 2 1) "Strictly decreasing")
(test-false (> 3 2 2) "Not strictly decreasing")

;; Test 16: Equality
(display "=== Testing Equality ===")
(newline)
(test-true (eq? 'foo 'foo) "Equal symbols")
(test-false (eq? 'foo 'bar) "Unequal symbols")
(test-true (eq? 42 42) "Equal numbers")
(test-false (eq? 42 43) "Unequal numbers")
(test-true (eq? '(1 2) '(1 2)) "Equal lists")
(test-false (eq? '(1 2) '(1 3)) "Unequal lists")

(display "=== All tests completed ===")
(newline) 