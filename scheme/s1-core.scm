;; s1-core.scm: Scheme-level core predicates and utilities

;; Type predicates using type-of function
;; (define number? (lambda (x) (or (eq? (type-of x) 'integer) (eq? (type-of x) 'float))))
(define symbol? (lambda (x) (eq? (type-of x) 'symbol)))
(define pair? (lambda (x) (eq? (type-of x) 'pair)))
(define string? (lambda (x) (eq? (type-of x) 'string)))
(define vector? (lambda (x) (eq? (type-of x) 'vector)))
(define closure? (lambda (x) (eq? (type-of x) 'closure)))
(define boolean? (lambda (x) (eq? (type-of x) 'boolean)))
(define char? (lambda (x) (eq? (type-of x) 'char)))
(define primitive? (lambda (x) (eq? (type-of x) 'primitive)))
(define env-frame? (lambda (x) (eq? (type-of x) 'env-frame)))
(define nil? (lambda (x) (eq? x '())))

;; List accessor functions (compositions of car and cdr)
;; These provide convenient access to nested list elements

;; Basic 2-level accessors
(define cadr (lambda (l) (car (cdr l))))      ;; Second element of a list
(define cdar (lambda (l) (cdr (car l))))      ;; Cdr of the first element
(define caar (lambda (l) (car (car l))))      ;; Car of the first element
(define cddr (lambda (l) (cdr (cdr l))))      ;; Cdr of the cdr of a list

;; 3-level accessors
(define caddr (lambda (l) (car (cdr (cdr l)))))     ;; Third element of a list
(define cadddr (lambda (l) (car (cdr (cdr (cdr l)))))) ;; Fourth element of a list
(define cadar (lambda (l) (car (cdr (car l)))))     ;; Car of cdr of car
(define cddar (lambda (l) (cdr (cdr (car l)))))     ;; Cdr of cdr of car
(define caadr (lambda (l) (car (car (cdr l)))))     ;; Car of car of cdr
(define cdadr (lambda (l) (cdr (car (cdr l)))))     ;; Cdr of car of cdr
(define cdddr (lambda (l) (cdr (cdr (cdr l)))))     ;; Cdr of cdr of cdr

;; 4-level accessors (less common but useful)
(define caaar (lambda (l) (car (car (car l)))))     ;; Car of car of car
(define cdaar (lambda (l) (cdr (car (car l)))))     ;; Cdr of car of car
(define caaadr (lambda (l) (car (car (car (cdr l)))))) ;; Car of car of car of cdr
(define cdaadr (lambda (l) (cdr (car (car (cdr l)))))) ;; Cdr of car of car of cdr
(define cadadr (lambda (l) (car (cdr (car (cdr l)))))) ;; Car of cdr of car of cdr
(define cddadr (lambda (l) (cdr (cdr (car (cdr l)))))) ;; Cdr of cdr of car of cdr
(define caaddr (lambda (l) (car (car (cdr (cdr l)))))) ;; Car of car of cdr of cdr
(define cdaddr (lambda (l) (cdr (car (cdr (cdr l)))))) ;; Cdr of car of cdr of cdr
(define cdddr (lambda (l) (cdr (cdr (cdr l)))))      ;; Cdr of cdr of cdr
(define cddddr (lambda (l) (cdr (cdr (cdr (cdr l)))))) ;; Cdr of cdr of cdr of cdr

(display "s1-core loaded")
(newline)

(define load (lambda (f)
    (begin (define inp (open-input-file f))
        (display inp)
        (push-port! inp))))
;; (load "scheme/regression.scm")

(define not (lambda (v) (if v #f #t)))
(define abs (lambda (n) (if (< n 0) (- n) n)))
(define <= (lambda (m n) (not (> m n))))
(define >= (lambda (m n) (not (< m n))))
(define zero? (lambda (n) (= n 0)))
(define positive? (lambda (n) (>= n 0)))
(define negative? (lambda (n) (< n 0)))
(define even? (lambda (n) (zero? (mod n 2))))
