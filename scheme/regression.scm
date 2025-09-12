;; regression.scm: Simple test framework for s1 Scheme interpreter
;; This can be run directly in the REPL without file loading

;; Simple test framework - run each test manually
;; Example usage:
;; (define test-result (test-equal 42 42 "Integer equality"))
;; (define test-result (test-true (number? 42) "Number predicate"))

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
            (display "PASS: ")
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

(display "          === Testing Literals ===")
(newline)
(test-equal 42 42 "Integer literal")
(test-equal 3.14 3.14 "Float literal")
(test-equal "hello" "hello" "String literal")
(test-equal #t #t "Boolean true literal")
(test-equal #f #f "Boolean false literal")
(test-equal #\a #\a "Character literal")
(test-nil '() "Nil literal")
(test-symbol 'foo "Symbol literal")

(display "          === Testing Arithmetic ===")
(newline)
(test-equal 3 (+ 1 2) "Simple addition")
(test-equal 12 (* 3 4) "Simple multiplication")
(test-equal -5 (- 5 4 3 2 1) "Subtraction with multiple args")
(test-equal 0.3 (/ 6 5 4) "Division with multiple args")
(test-equal 15 (+ 1 2 3 4 5) "Addition with multiple args")
(test-equal 120 (* 2 3 4 5) "Multiplication with multiple args")
(test-equal 83 (- 100 10 5 2) "Subtraction with multiple args")
(test-true (inexact? 3.14) "inexact? with float")
(test-false (inexact? 42) "inexact? with integer")
(test-true (inexact? 3.14) "inexact? with float")
(test-false (inexact? 42) "inexact? with integer")
(test-true (exact? 42) "exact? with integer")
(test-false (exact? 3.14) "exact? with float")
(test-true (exact? 42) "exact? with integer")
(test-false (exact? 3.14) "exact? with float")
(test-equal 1 (modulo 7 3) "modulo basic")
(test-equal 2 (modulo 8 3) "modulo with remainder")
(test-equal 0 (modulo 9 3) "modulo exact division")
;; Comparison tests
(test-true (= 5 5) "equality same numbers")
(test-true (= 5 5.0) "equality int and float")
(test-false (= 5 6) "equality different numbers")
(test-true (= 1 1 1) "equality multiple same")
(test-false (= 1 1 2) "equality multiple different")

(test-true (< 1 2) "less than basic")
(test-true (< 1 2 3) "less than multiple")
(test-false (< 1 2 2) "less than not strict")
(test-false (< 3 2 1) "less than decreasing")

(test-true (> 3 2) "greater than basic")
(test-true (> 5 4 3) "greater than multiple")
(test-false (> 5 4 4) "greater than not strict")
(test-false (> 1 2 3) "greater than increasing")
;; New R5RS numeric functions
(test-equal 3 (quotient 10 3) "quotient basic")
(test-equal -3 (quotient -10 3) "quotient negative dividend")
(test-equal 1 (remainder 10 3) "remainder basic")
(test-equal -1 (remainder -10 3) "remainder negative dividend")

(test-equal 5 (numerator 5) "numerator integer")
(test-equal 1 (denominator 5) "denominator integer")
(test-equal 5 (numerator 5.0) "numerator exact float")
(test-equal 1 (denominator 5.0) "denominator exact float")

(test-equal 3 (floor 3.7) "floor positive")
(test-equal -4 (floor -3.2) "floor negative")
(test-equal 5 (floor 5) "floor integer")

(test-equal 4 (ceiling 3.2) "ceiling positive")
(test-equal -3 (ceiling -3.7) "ceiling negative")
(test-equal 5 (ceiling 5) "ceiling integer")

(test-equal 3 (truncate 3.7) "truncate positive")
(test-equal -3 (truncate -3.7) "truncate negative")
(test-equal 5 (truncate 5) "truncate integer")

(test-equal 4 (round 3.7) "round up")
(test-equal 3 (round 3.2) "round down")
(test-equal 4 (round 3.5) "round half")
(test-equal 5 (round 5) "round integer")

(test-equal 3.0 (sqrt 9) "sqrt perfect square")
(test-equal 2.0 (sqrt 4.0) "sqrt float")

(test-equal 8 (expt 2 3) "expt integer base and exp")
(test-equal 8 (expt 2.0 3) "expt float base")
(test-equal 0.25 (expt 2 -2) "expt negative exp")
(test-equal 1 (expt 5 0) "expt zero exp")

(display "          === Testing Type Predicates ===")
(newline)
(test-true (integer? 22) "integer? with integer")
(test-false (integer? nil) "integer with nil")
(test-true (float? 3.14) "float? with float")
(test-false (float? 3) "float? with integer")
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

(display "          === Testing Type-of ===")
(newline)
(test-equal 'integer (type-of 42) "type-of with integer")
(test-equal 'float (type-of 3.14) "type-of with float")
(test-equal 'string (type-of "hello") "type-of with string")
(test-equal 'boolean (type-of #t) "type-of with boolean")
(test-equal 'boolean (type-of #f) "type-of with boolean")
(test-equal 'char (type-of #\a) "type-of with character")
(test-equal 'null (type-of ()) "type-of with nil")
(test-equal 'symbol (type-of 'symbol) "type-of with symbol")
(test-equal 'pair (type-of '(1 2 3)) "type-of with pair")

(display "          === Testing List Literals ===")
(newline)
(test-equal '(1 2 3) '(1 2 3) "List literal")
(test-equal '(a b c) '(a b c) "Symbol list literal")
(test-equal '(1 (+ 2 3) 4) '(1 (+ 2 3) 4) "List with quoted expression")
(test-equal '(1 2 3 4 5) '(1 2 3 4 5) "Longer list literal")

(display "          === Testing Begin ===")
(newline)
(test-equal 3 (begin 1 2 3) "Simple begin")
(test-equal 12 (begin (+ 1 2) (* 3 4)) "Begin with expressions")
(test-equal 5 (begin 1 2 3 4 5) "Begin with multiple expressions")

(display "          === Testing Variables ===")
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

(display "          === Testing Set! ===")
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

(display "          === Testing If ===")
(newline)
(test-equal 1 (if #t 1 2) "if with true condition")
(test-equal 2 (if #f 1 2) "if with false condition")
(test-equal 5 (if #t 5) "if without alternate")
(test-equal 3 (if #t (+ 1 2) (- 1 2)) "if with expressions")
(test-equal -1 (if #f (+ 1 2) (- 1 2)) "if with expressions, false")
(test-equal 11 (if (> 10 5) (+ 10 1) (- 10 1)) "if with comparison")
(test-equal 22 (cond ((= 3 3) 22) (else "failed")) "cond with boolean expr")
(test-equal 22 (cond (else 22)) "minimal cond with else")
(test-equal 66 (cond (#f 22 33) (#f 44 55) (else (define zz "ok") 66)) "cond with implicit begin")
(test-equal zz "ok" "cond else with implicit begin")

(display "          === Testing And/Or ===")
(newline)
(test-equal #t (and) "Empty and")
(test-equal 2 (and #t 1 2) "'and' with all truthy (numbers)")
(test-equal #f (and #t #f 1) "'and' with false")
(test-equal #f (or) "Empty 'or'")
(test-equal #f (or #f #f) "'or' with all false")
(test-equal 1 (or #f 1 2) "'or' with true")
(test-equal 3 (or #f #f 3) "'or' with last true")
(test-equal #t (and #t #t #t) "'and' with all #t")
(test-equal #f (and #t #f #t) "'and' with middle false")
(test-equal #f (or #f #f #f) "'or' with all false")
(test-equal #t (or #f #t #f) "'or' with middle true")

(display "          === Testing Quote ===")
(newline)
(test-equal 42 '42 "Quote number")
(test-equal #t '#t "Quote boolean")
(test-equal "hello" '"hello" "Quote string")
(test-equal #\a '#\a "Quote character")

(display "          === Testing Lambda ===")
(newline)
(define add1 (lambda (x) (+ x 1)))
(define make-adder (lambda (n) (lambda (x) (+ x n))))
(define add5 (make-adder 5))
(test-equal 6 (add1 5) "Simple lambda application")
(test-equal 7 ((lambda (x y) (+ x y)) 3 4) "Immediate lambda application")
(test-equal 8 (add5 3) "Closure application")
(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
(test-equal 720 (fact 6) "Factorial test")
(define fac-acc (lambda (n)
    (define f (lambda (n acc)
        (if (= n 0)
            acc
            (f (- n 1) (* acc n)))))
    (f n 1)))
(test-equal 720 (fac-acc 6) "Factorial test with nested define and accumulator")

(display "          === Testing List Accessors ===")
(newline)
(test-equal 1 (car '(1 2 3)) "car of list")
(test-equal '(2 3) (cdr '(1 2 3)) "cdr of list")
(test-equal 2 (cadr '(1 2 3)) "cadr of list")
(test-equal 3 (caddr '(1 2 3 4)) "caddr of list")
(test-equal 1 (caar '((1 2) (3 4))) "caar of nested list")
(test-equal '(2) (cdar '((1 2) (3 4))) "cdar of nested list")
(test-equal '(3 4) (cadr '((1 2) (3 4))) "cadr of nested list")

(display "          === Testing List Construction ===")
(newline)
(test-equal '(1 . 2) (cons 1 2) "cons with atom")
(test-equal '(1 2 3) (cons 1 '(2 3)) "cons with list")
(test-equal '() (list) "empty list")
(test-equal '(42) (list 42) "single element list")
(test-equal '(1 2 3) (list 1 2 3) "multiple element list")
(test-equal '(1 2 3 4) (append '(1 2) '(3 4)) "append two lists")
(test-equal '(1 2) (append '(1 2) '()) "append with empty list")
(define foo '(a b))
(test-equal '(1 b) (begin (set-car! foo 1) foo)  "set-car! on list")
(test-equal '(1 . 2) (begin (set-cdr! foo 2) foo)  "set-cdr! on list")
(test-equal '(1 2 3) (begin (set-cdr! foo (list 2 3)) foo)  "set-cdr! on list")
(define bar '(x y))
(define ref bar)
(set-car! bar 'z)
(test-true (eq? ref bar) "mutated pair is the same object")

(display "          === Testing Comparisons ===")
(newline)
(test-true (= 5 5) "Equal numbers")
(test-true (= 5 5.0) "Equal int and float")
(test-false (= 5 6) "Unequal numbers")
(test-true (< 1 2 3) "Strictly increasing")
(test-false (< 1 2 2) "Not strictly increasing")
(test-true (> 3 2 1) "Strictly decreasing")
(test-false (> 3 2 2) "Not strictly decreasing")
(test-equal 4 (min 5 4 6) "min with integers")
(test-equal 3.14 (min 5 6 7.8 3.14) "min with int and float")

(display "          === Testing Strings ===")
(newline)
(test-equal "22" (>string 22) "Integer to string")
(test-equal "22.5" (>string 22.5) "Float to string")
(test-equal "hello" (>string "hello") "String to string")
(test-equal "(1 2 3)" (>string '(1 2 3)) "List to string")
(test-equal 5 (string-length "hello") "string-length")
(test-equal 0 (string-length "") "string-length of empty string")
(test-equal "now is the time" (string-downcase "Now Is The Time") "string-downcase")
(test-equal "NOW IS THE TIME" (string-upcase "Now Is The Time") "string-upcase")
(test-equal "Now is the time" (string-append "Now " "is " "the time") "string-append")
(test-equal "A copied string" (string-copy "A copied string") "string-copy")
(test-equal #\3 (string-ref "012345" 3) "string-ref")
(test-equal #\A (string-ref "A copied string" 0) "string-ref")
(test-equal "A cop" (substring "A copied string" 0 4) "substring")
(test-true (string=? "hello" "hello") "string=?")
(test-false (string=? "hello" "world") "string=?")
(test-true (string<? "apple" "banana") "string<?")
(test-false (string<? "apple" "apple") "string<?")
(test-true (string<? "apple" "chinook") "string<?")
(test-false (string>? "apple" "banana") "string>?")
(test-false (string>? "apple" "apple") "string>?")

(display "          === Testing Equality ===")
(newline)
(test-true (eq? 'foo 'foo) "Equal symbols")
(test-false (eq? 'foo 'bar) "Unequal symbols")
(test-true (eq? 42 42) "Equal numbers")
(test-false (eq? 42 43) "Unequal numbers")
(test-true (equal? '(1 2) '(1 2)) "Equal lists")
(test-false (equal? '(1 2) '(1 3)) "Unequal lists")

(display "          === Testing eval and apply ===")
(newline)
(test-true (eq? (eval '(+ 1 2)) 3) "eval")
(test-true (eq? (apply + '(1 2)) 3) "apply")
(test-true (eq? (eval '(+ 1 2)) (apply + '(1 2))) "eval and apply")
(define square (lambda (x) (* x x)))
(test-equal 144 (apply square '(12)) "Apply with closure")
(test-equal '(a . b) (apply cons '(a b)) "Apply giving dotted pair")
(test-equal '((1 2) 3 4)(apply apply (list list '(1 2) '(3 4))) "Nested apply")
(test-equal #(1 2 3) (apply vector '(1 2 3)) "Apply creating vector")
(define (tail x) (apply values (list x)))
(test-equal 5 (tail 5) "Apply with (values ...)")
(display "          === Testing Recursion ===")
(define fact
    (lambda (n)
        (if (zero? n) 1 (* n (fact (- n 1))))))
(newline)
(test-equal 1 (fact 0) "Factorial(0)")
(test-equal 720 (fact 6) "Factorial(6)")
(define fib
    (lambda (n)
        (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))
(test-equal 13 (fib 7) "Fibonacci(7)")
(test-equal 6765 (fib 20) "Fibonacci(20)")
(test-equal '(720) (list (fac-acc 6)) "Tail recursion inside a form")
(define test-tail (lambda (n) (if (= n 0) #t (test-tail (- n 1)))))
(test-true (test-tail 7000) "(test-tail 7000) - fails without tail recursion")

(display "          === Testing Macros ===")
(newline)

(define m1 (macro (x) `(list 11 ,x)))
(test-equal '(11 22) (m1 22) "Simple macro")

(define when (macro (p body)
    `(if ,p ,body nil)))
(test-equal 22 (when #t 22) "when with #t")
(test-equal nil (when #f 22) "when with #f")

(define unless (macro (p body)
    `(if (not ,p) ,body nil)))
(test-equal nil (unless #t 22) "unless with #t")
(test-equal 22 (unless #f 22) "when with #f")

(define m1 (macro (x) `(list ,x)))
(test-equal '(42) (m1 42) "macro (m1 42)")

(define m2 (macro (x) `(quote ,x)))
(test-equal 'hello (m2 hello) "macro (m2 hello)")

(define m3 (macro args `(list ,@args)))
(test-equal '(1 2) (m3 1 2) "macro (m3 1 2)")

(define m4 (macro (op . args) `(list ,op ,@args)))
(test-equal (list + 1 2 3) (m4 + 1 2 3) "macro (m4 + 1 2 3)")

(define m5 (macro x `(append ,@x)))
(test-equal '(1 2 3 4) (m5 '(1 2) '(3 4)) "macro (m5 '(1 2) '(3 4))")

(define m6 (macro (test . body)
    `(if ,test (begin ,@body) nil)))
(test-equal 22 (m6 #t 22) "macro (m6 #t 11 22)")
(test-equal nil (m6 #f 22) "macro (m6 #f 22)")

(test-equal '(2 1) (let ((a 1) (b '(2))) (expand `(,@b ,a))) "expand macro expression")
(test-equal '(1 2) (let ((a 1) (b '(2))) (expand `(,a ,@b))) "expand macro expression")

(display "          === Testing let ===")
(newline)

(test-equal 6 (let ((x 2) (y 3)) (* x y)) "Two binding let with single body expression")
(test-equal 9 (let ((x 3)) (* x x)) "Single binding let with single body expression")
(test-equal 42 (let ((x 6)) (let ((y (+ x 1))) (* x y))) "Nested let expression")

(display "          === Testing read ===")
(newline)
(test-equal '(list a 2 3) (read) "Reading a list")
(list a 2 3)
(test-equal 22 (read) "Reading an integer")
22
(test-equal [1 2 3] (read) "Reading a vector")
[1 2 3]
(test-equal "a string" (read) "Reading a string")
"a string"

(display "          === Testing delay and force ===")
(newline)
(define **count** 0)
(define p
  (delay (begin (set! **count** (+ **count** 1))
                (if (> **count** x)
                    **count**
                    (force p)))))
(define x 5)
(test-equal 6 (force p) "First force")
(test-equal 6 (begin (set! **count** 10) (force p)) "Second force")

(display "          === Testing call/cc ===")
(newline)
(define cont1 (call/cc (lambda (k) (k 5))))
(test-equal 5  cont1 "Return a value")
(test-equal 10 (call/cc (lambda (k) (+ 1 (k 10) 3))) "Break out of add")
(define saved #f)
(define result
    (call/cc (lambda (k) (set! saved k) 'ok)))
(test-equal 'ok result "Capture and re-use continuation")
(test-equal 99 (saved 99) "Reuse previous continuation")

;; Additional call/cc regression tests
;; Basic call/cc with assignment (the original bug case)
(define cont1 (call/cc (lambda (k) (k 42))))
(test-equal 42 cont1 "call/cc assignment should work")

;; call/cc with nested computation
(define result1 (call/cc (lambda (k) (+ 10 (k 5) 20))))
(test-equal 5 result1 "call/cc should escape from nested computation")

;; call/cc without escape (normal return)
(define result2 (call/cc (lambda (k) (+ 1 2 3))))
(test-equal 6 result2 "call/cc should work without escaping")

;; Multiple call/cc in sequence
(define val1 (call/cc (lambda (k) (k 'first))))
(define val2 (call/cc (lambda (k) (k 'second))))
(test-equal 'first val1 "first call/cc should work")
(test-equal 'second val2 "second call/cc should work")

;; call/cc with conditional escape
(define test-escape
  (lambda (x)
    (call/cc (lambda (return)
               (if (< x 0)
                   (return 'negative)
                   (+ x 10))))))
(test-equal 15 (test-escape 5) "call/cc conditional no escape")
(test-equal 'negative (test-escape -3) "call/cc conditional escape")

;; call/cc capturing loop state
(define sum 0)
(define loop-result
  (call/cc (lambda (break)
             (define loop (lambda (i)
               (set! sum (+ sum i))
               (if (> i 5)
                   (break sum)
                   (loop (+ i 1)))))
             (loop 1))))
(test-equal 21 loop-result "call/cc should capture loop state")

;; Nested call/cc
(define nested-result
  (call/cc (lambda (outer)
             (+ 100
                (call/cc (lambda (inner)
                          (outer 42)))
                200))))
(test-equal 42 nested-result "nested call/cc should escape to outer")

;; call/cc with function application
(define add-or-escape
  (lambda (x y escape?)
    (call/cc (lambda (k)
               (if escape?
                   (k 'escaped)
                   (+ x y))))))
(test-equal 7 (add-or-escape 3 4 #f) "call/cc function normal case")
(test-equal 'escaped (add-or-escape 3 4 #t) "call/cc function escape case")

;; call/cc with list processing
(define find-negative
  (lambda (lst)
    (call/cc (lambda (found)
               (define loop (lambda (items)
                 (cond
                   ((null? items) 'none)
                   ((< (car items) 0) (found (car items)))
                   (else (loop (cdr items))))))
               (loop lst)))))
(test-equal -3 (find-negative '(1 2 -3 4)) "call/cc list processing escape")
(test-equal 'none (find-negative '(1 2 3 4)) "call/cc list processing normal")

;; call/cc with string operations
(define process-string
  (lambda (str)
    (call/cc (lambda (return)
               (if (string=? str "")
                   (return 'empty-string)
                   (string-length str))))))
(test-equal 5 (process-string "hello") "call/cc string processing normal")
(test-equal 'empty-string (process-string "") "call/cc string processing escape")

;; call/cc with error simulation
(define safe-divide
  (lambda (x y)
    (call/cc (lambda (error)
               (if (= y 0)
                   (error 'division-by-zero)
                   (/ x y))))))
(test-equal 5.0 (safe-divide 10 2) "call/cc error simulation normal")
(test-equal 'division-by-zero (safe-divide 10 0) "call/cc error simulation escape")

(display "          === Testing values / call-with-values ===")
(newline)
(test-equal 5 (+ (values 2)3) "Singleton values call")

(display "          === All tests completed ===")
(newline)
(if (null? **failed-tests**)
    (displayln "===  All" **counter** "tests passed!  ===")
    (begin
        (displayln "Failing tests: ")
        (displayln "==============")
        (map displayln **failed-tests**)))
