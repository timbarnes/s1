11
;; Dummy
(display "Dummy file loaded")
(define x 22)

(define def
    (lambda (args body)
        (if (symbol? args)
            (define args body)
            (define (car args) (lambda (cdr args) body)))))
