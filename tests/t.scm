11
;; Dummy

(define fib
    (lambda (n)
        (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))

(define fib-30 (fib 30))

(define def (macro (front . back)
    (cond ((symbol? front) `(define ,front ,back))
        ((pair? front) `(define ,(car front) (lambda ,(cdr front) ,@back)))
    (else (display "Invalid syntax")))))

(define fac-acc (lambda (n)
    (define f (lambda (n acc)
        (if (zero? n)
            acc
            (f (- n 1) (* acc n)))))
    (f n 1)))

(define f3000 (fac-acc 10000))

(display "Test file processed")
(quit)
