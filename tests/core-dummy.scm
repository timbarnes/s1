;; Dummy core Scheme file for file loading test
;; This file does nothing but should be loaded at startup.
22
(display "Core dummy file loaded\n")

(define int_expand (lambda (expr)
    (if (eq? (car expr) 'comma)
        (list (eval (cdr expr)))
        (if (eq! (car expr) 'comma-at)
            (eval (cdr expr))
            (list expr)))))

(define expander (lambda (expr)
    (if (eq? (car expr) 'backquote)
        ; We have something to expand
        (append (int_expand (car expr) (int_expand (cdr expression))))
    ; Nothing to expand
        expr)))
