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

(display "          === Testing let, let* and letrec ===")
(newline)

(test-equal 6 (let ((x 2) (y 3)) (* x y)) "Two binding let with single body expression")
(test-equal 9 (let ((x 3)) (* x x)) "Single binding let with single body expression")
(test-equal 42 (let ((x 6)) (let ((y (+ x 1))) (* x y))) "Nested let expression")
(test-equal 3 (let* ((x 1) (y (+ x 1))) (+ x y)) "let* with sequential dependency")
(test-equal 3  (let ((x 10)) (let* ((x 1) (y (+ x 1))) (+ x y))) "let* with parameter shadowing")
(test-equal 42 (let* () 42) "let* with no bindings")
(test-equal 25 (let* ((x 5)) (* x x)) "let* with single binding")
(test-equal 25 (let* ((x 2) (y 3)) (define z (+ x y)) (* z z)) "Multiple body expressions")
(test-equal 4 (let* ((x 1) (y (let* ((a x) (b (+ a 1))) (+ a b)))) (+ x y)) "Nested let* expressions")
(test-equal '(3 (2 4 6))
    (let* ((lst '(1 2 3)) (len (length lst)) (doubled (map (lambda (x) (* x 2)) lst))) (list len doubled))
    "let* using previous bindings in complex expression")
(test-equal 120
 (letrec ((fact (lambda (n)
                  (if (= n 0)
                      1
                      (* n (fact (- n 1)))))))
   (fact 5)) "letrec factorial")

(test-equal '(#t #f #f #t)
 (letrec ((even? (lambda (n)
                   (if (= n 0)
                       #t
                       (odd? (- n 1)))))
          (odd? (lambda (n)
                  (if (= n 0)
                      #f
                      (even? (- n 1))))))
   (list (even? 4) (odd? 4) (even? 3) (odd? 3)))
   "Mutually recursive functions")

 (test-equal 42 (letrec () 42) "letrec with no bindings")

 (test-equal 49 (letrec ((square (lambda (x) (* x x))))
   (square 7)) "letrec with single binding")
 (test-equal 10
    (letrec ((double (lambda (x) (* x 2))))
    (define result (double 5))
    result)
    "letrec with multiple body expressions")

 ;; Self-referencing non-function (should work but xyz should be undefined initially)
 (test-equal 42 (letrec ((xyz (if #f xyz 42))) xyz) "Self-referencing non-function")

 (test-equal 4 (letrec ((length (lambda (lst)
                    (if (null? lst)
                        0
                        (+ 1 (length (cdr lst)))))))
                (length '(a b c d)))
    "Recursive list processing")

(test-equal 8 (letrec ((outer (lambda (n)
                                (letrec ((inner (lambda (m) (+ n m))))
                                    (inner 5)))))
                (outer 3))
    "Nested letrec")

(test-equal 3 (let ((x 100))
                (letrec ((x 1)
                            (f (lambda () (+ x 2))))
                    (f)))
        "Variable shadowing with recursion")

(test-equal 55 (let loop ((x 10) (acc 0))
                    (if (= x 0)
                        acc
                        (loop (- x 1) (+ acc x))))
    "Named let")

(display "          === Testing do ===")
(newline)

;; Basic counting loop
(test-equal 55
    (do ((i 1 (+ i 1))
         (sum 0 (+ sum i)))
        ((> i 10) sum))
    "do: sum 1 to 10")

;; Simple countdown
(test-equal 0
    (do ((n 5 (- n 1)))
        ((= n 0) n))
    "do: countdown to zero")

;; Factorial calculation
(test-equal 120
    (do ((n 5 (- n 1))
         (fact 1 (* fact n)))
        ((= n 0) fact))
    "do: factorial of 5")

;; List length calculation
(test-equal 4
    (do ((lst '(a b c d) (cdr lst))
         (len 0 (+ len 1)))
        ((null? lst) len))
    "do: calculate list length")

;; List reversal
(test-equal '(d c b a)
    (do ((lst '(a b c d) (cdr lst))
         (rev '() (cons (car lst) rev)))
        ((null? lst) rev))
    "do: reverse list")

;; Empty bindings
(test-equal 42
    (do () (#t 42))
    "do: empty bindings")

;; Single variable, no step (variable unchanged)
(test-equal 10
    (do ((x 10))
        (#t x))
    "do: single variable no step")

;; Multiple result expressions
(test-equal 3
    (do ((i 0 (+ i 1)))
        ((= i 3) (+ i 1) (- i 1) i))
    "do: multiple result expressions returns last")

;; Commands executed during iteration
(test-equal 6
    (let ((result 0))
      (do ((i 1 (+ i 1)))
          ((> i 3) result)
        (set! result (+ result i))))
    "do: commands executed each iteration")

;; No result expressions (returns unspecified, test for not crashing)
(test-equal #t
    (let ((x #f))
      (do ((i 0 (+ i 1)))
          ((= i 1) (set! x #t)))
      x)
    "do: no result expressions")

;; Nested do loops
(test-equal 10
    (do ((i 1 (+ i 1))
         (total 0))
        ((> i 3) total)
      (do ((j 1 (+ j 1)))
          ((> j i))
        (set! total (+ total j))))
    "do: nested do loops")

;; Variable shadowing
(test-equal 10
    (let ((x 100))
      (do ((x 0 (+ x 1))
           (sum 0 (+ sum x)))
          ((= x 5) sum)))
    "do: variable shadowing")

;; Complex step expressions
(test-equal '(1 2 4 8 16)
    (do ((n 16 (quotient n 2))
         (powers '() (cons n powers)))
        ((= n 0) powers))
    "do: complex step expressions")

;; Zero iterations (test immediately true)
(test-equal 42
    (do ((i 0 (+ i 1)))
        (#t 42)
      (error "should not execute"))
    "do: zero iterations")

;; Multiple commands in body
(test-equal '(3 2 1)
    (let ((result '()))
      (do ((i 1 (+ i 1)))
          ((> i 3) result)
        (set! result (cons i result))
        (set! result (reverse result))
        (set! result (reverse result))))
    "do: multiple commands in body")

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
(define cont2 (call/cc (lambda (k) (k 42))))
(test-equal 42 cont2 "call/cc assignment should work")

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

(display "          === Testing dyamic-wind ===")
(newline)
(test-equal "thunk"
    (dynamic-wind
        (lambda () (display "Before "))
        (lambda () (display "Thunk ") "thunk")
        (lambda () (displayln "After ")))
    "dynamic-wind with local exit")

(define x '())
(define result
  (call/cc
   (lambda (k)
     (dynamic-wind
      (lambda () (set! x (cons 'before x)))
      (lambda () (k 'escaped))
      (lambda () (set! x (cons 'after x)))))))
(test-equal 'escaped result "dynamic-wind: non-local exit returns correct value")
(test-equal '(after before) x "dynamic-wind: non-local exit runs 'after' thunk")

(display "          === Testing Nested dynamic-wind ===")
(newline)
(define nested-dw-log '())
(define (log . items)
  (set! nested-dw-log (append nested-dw-log items)))

(define nested-dw-result
  (call/cc
   (lambda (exit)
     (dynamic-wind
       (lambda () (log 'outer-before))
       (lambda ()
         (dynamic-wind
           (lambda () (log 'inner-before))
           (lambda () (exit 'escaped))
           (lambda () (log 'inner-after))))
       (lambda () (log 'outer-after))))))

(test-equal 'escaped nested-dw-result "nested dynamic-wind: correct return value")
(test-equal '(outer-before inner-before inner-after outer-after) nested-dw-log "nested dynamic-wind: correct thunk order")

(display "          === Testing Re-entrant dynamic-wind ===")
(newline)
(define re-entrant-log '())
(define (log-re . items)
  (set! re-entrant-log (append re-entrant-log items)))

(define k #f)
(define re-entrant-result
  (dynamic-wind
    (lambda () (log-re 'before-1))
    (lambda ()
      (call/cc
       (lambda (exit)
         (set! k exit)
         'initial-run)))
    (lambda () (log-re 'after-1))))

(test-equal 'initial-run re-entrant-result "re-entrant: initial result")
(test-equal '(before-1 after-1) re-entrant-log "re-entrant: initial run order")

;; Part 2: Re-entry
(set! re-entrant-log '())
(set! re-entrant-result #f) ; Reset the result from part 1

; When (k 're-entry-value) is called, it will escape to where k was captured
; and bind 're-entry-value to re-entrant-result, executing thunks as needed
(call/cc
 (lambda (top-exit)
   (dynamic-wind
     (lambda () (log-re 'before-2))
     (lambda () (top-exit (k 're-entry-value)))
     (lambda () (log-re 'after-2)))))
; The above call/cc will never complete because k escapes to the first dynamic-wind
; So we test that re-entrant-result got the correct value from the escape
(test-equal 're-entry-value re-entrant-result "re-entrant: re-entry result")
(test-equal '(before-2 after-2 before-1 after-1) re-entrant-log "re-entrant: re-entry run order")

;; Additional dynamic-wind edge case tests
(display "          === Testing Dynamic-wind Edge Cases ===")
(newline)

;; Test 1: Error handling with continuations (simulated error in before thunk)
(define error-test-1-result #f)
(define error-test-1-caught #f)
(call/cc
  (lambda (outer-escape)
    (call/cc
      (lambda (error-escape)
        (set! error-test-1-result
          (dynamic-wind
            (lambda () (error-escape "before-error-caught"))
            (lambda () 'should-not-reach)
            (lambda () 'cleanup-not-reached)))
        (set! error-test-1-result 'should-not-reach)))
    (set! error-test-1-caught #t)))
(test-equal #t error-test-1-caught "error simulation: before thunk escape caught")

;; Test 2: Simple escape from dynamic-wind thunk
(define simple-escape-result #f)
(set! simple-escape-result
  (call/cc
    (lambda (escape)
      (dynamic-wind
        (lambda () 'setup)
        (lambda () (escape 'escaped-early))
        (lambda () 'cleanup)))))
(test-equal 'escaped-early simple-escape-result "simple escape: early exit works")

;; Test 3: Multiple escapes from same dynamic-wind
(define multi-escape-log '())
(define multi-k #f)
(define multi-result
  (dynamic-wind
    (lambda () (set! multi-escape-log (cons 'before multi-escape-log)))
    (lambda ()
      (call/cc (lambda (k) (set! multi-k k) 'first)))
    (lambda () (set! multi-escape-log (cons 'after multi-escape-log)))))

(test-equal 'first multi-result "multiple escapes: initial result")
(test-equal '(after before) multi-escape-log "multiple escapes: initial log")

;; Test 4: Dynamic-wind with minimal thunk
(define empty-log '())
(define empty-thunk-result
  (dynamic-wind
    (lambda () (set! empty-log (cons 'before empty-log)))
    (lambda () #f)
    (lambda () (set! empty-log (cons 'after empty-log)))))
(test-equal #f empty-thunk-result "empty thunk: returns #f")
(test-equal '(before after) (reverse empty-log) "empty thunk: thunks execute")

;; Test 5: Deep nesting (simplified)
(define deep-simple-result
  (dynamic-wind
    (lambda () 'outer-setup)
    (lambda ()
      (dynamic-wind
        (lambda () 'inner-setup)
        (lambda () 'inner-completed)
        (lambda () 'inner-cleanup)))
    (lambda () 'outer-cleanup)))

(test-equal 'inner-completed deep-simple-result "deep nesting: simple nested completion")

;; Test 6: Dynamic-wind with side effects
(define side-effect-counter 0)
(define side-effect-result
  (call/cc
    (lambda (escape)
      (dynamic-wind
        (lambda () (set! side-effect-counter (+ side-effect-counter 1)))
        (lambda () (escape 'escaped))
        (lambda () (set! side-effect-counter (+ side-effect-counter 10)))))))

(test-equal 'escaped side-effect-result "side effects: escape result")
(test-equal 11 side-effect-counter "side effects: counter value")

;; Test 7: Nested dynamic-wind (simplified)
(define nested-simple-log '())
(define nested-simple-result
  (dynamic-wind
    (lambda () (set! nested-simple-log (cons 'outer-before nested-simple-log)))
    (lambda ()
      (dynamic-wind
        (lambda () (set! nested-simple-log (cons 'inner-before nested-simple-log)))
        (lambda () 'nested-normal)
        (lambda () (set! nested-simple-log (cons 'inner-after nested-simple-log))))
      'outer-normal)
    (lambda () (set! nested-simple-log (cons 'outer-after nested-simple-log)))))

(test-equal 'outer-normal nested-simple-result "nested simple: normal completion")
(test-equal '(outer-before inner-before inner-after outer-after) (reverse nested-simple-log) "nested simple: normal log")

;; Test 8: Simplified chain test
(define simple-chain-log '())
(define simple-chain-result
  (dynamic-wind
    (lambda () (set! simple-chain-log (cons 'dw1-before simple-chain-log)))
    (lambda ()
      (dynamic-wind
        (lambda () (set! simple-chain-log (cons 'dw2-before simple-chain-log)))
        (lambda () 'chain-normal)
        (lambda () (set! simple-chain-log (cons 'dw2-after simple-chain-log)))))
    (lambda () (set! simple-chain-log (cons 'dw1-after simple-chain-log)))))

(test-equal 'chain-normal simple-chain-result "simple chain: normal completion")
(test-equal '(dw1-before dw2-before dw2-after dw1-after) (reverse simple-chain-log) "simple chain: normal log")

(display "          === Testing values / call-with-values ===")
(newline)
(test-equal 5 (+ (values 2)3) "Singleton values call")
