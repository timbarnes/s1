11
;; Dummy
(display "Dummy file loaded")

(define fib
    (lambda (n)
        (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))

(fib 30)

(define def (macro x
    (cond ((symbol? (car x)) `(define ,(car x), ,(cadr x)))
        ((pair? (car x)) `(define ,(caar x) (lambda ,(cadar x) ,(cadr x))))
    (else (display "Invalid syntax")))))
