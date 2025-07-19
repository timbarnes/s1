42
3.14
"hello"
#t
#f
#\a
nil
(+ 1 2)
(* 3 4)
(- 5 4 3 2 1)
(/ 6 5 4)
(+ 1 2 3 (* (/ 6 3) 7) (- 4))
(number? 42)
(type-of 42)
'foo
'(1 2 3)
(begin 1 2 3)
(begin (+ 1 2) (* 3 4))
(define x 22)
(define y 'z)
x
y
(if #t 1 2)
(if #f 1 2)
(if #f 1)
(if #t 1)
(and)
(and #t 1 2)
(and #t #f 1)
(or)
(or #f #f)
(or #f 1 2)
(or #f #f 3)
(define x 10)
(set! x 42)
x
(set! x 'foo)
x
(set! yy 99)

; Additional regression tests for available functionality

; More complex arithmetic expressions
(+ 1 2 3 4 5)
(* 2 3 4 5)
(- 100 10 5 2)
(/ 100 2 2 5)

; More type checking
(number? 3.14)
(number? "not a number")
(type-of 3.14)
(type-of "hello")
(type-of #t)
(type-of #f)
(type-of #\a)
(type-of nil)
(type-of 'symbol)
(type-of '(1 2 3))

; More complex nested expressions
(define a 10)
(define b 20)
(+ a (* b 2))
(if (> a 5) (+ a 1) (- a 1))

; More list operations
'(a b c)
'(1 (+ 2 3) 4)
'(1 2 3 4 5)

; More complex begin expressions
(begin 1 2 3 4 5)
(begin (define x 1) (define y 2) (+ x y))

; More complex if expressions
(if #t (+ 1 2) (- 1 2))
(if #f (+ 1 2) (- 1 2))
(if #t (+ 1 2))

; More complex and/or expressions
(and #t #t #t)
(and #t #f #t)
(or #f #f #f)
(or #f #t #f)

; More complex set! expressions
(define z 100)
(set! z 200)
z
(set! z (+ z 50))
z

; More complex define expressions
(define w (+ 5 5))
w
(define v 'hello)
v

; More complex quote expressions
'42
'#t
'"hello"
'#\a

; ============================================================================
; COMMENTED OUT TESTS - NOT YET IMPLEMENTED
; ============================================================================

; Lambda functions - not yet implemented
; (lambda (x) (+ x 1))
; (define add1 (lambda (x) (+ x 1)))
; (add1 5)
; ((lambda (x y) (+ x y)) 3 4)

; Vector operations - not yet implemented
; #(1 2 3)
; #(1 (+ 2 3) 4)

; Character literals - not yet implemented
; #\a
; #\space
; #\newline

; Additional predicates - not yet implemented
; (symbol? 'foo)
; (string? "hello")
; (boolean? #t)
; (char? #\a)
; (pair? '(1 2))
; (vector? #(1 2 3))

; Environment and scoping - not yet implemented
; (define x 10)
; (define f (lambda (y) (+ x y)))
; (f 5)

; Output functions - not yet implemented
; (display "hello")
; (newline)
; (display 42)

; Error cases - not yet implemented
; (+ 1 "hello")  ; type error
; (define)        ; wrong number of args
; (if)           ; wrong number of args
; (set! x 5)     ; undefined variable

; More complex lambda applications - not yet implemented
; (define make-adder (lambda (x) (lambda (y) (+ x y))))
; (define add5 (make-adder 5))
; (add5 3)

; List manipulation - not yet implemented
; (car '(1 2 3))
; (cdr '(1 2 3))
; (cons 1 '(2 3)) 