;; s1-core.scm: Scheme-level core predicates and utilities

(define error (lambda (msg) (displayln "Error:"msg)))
;; Type predicates using type-of function
(define number? (lambda (x) (or (eq? (type-of x) 'integer) (eq? (type-of x) 'float))))
(define integer? (lambda (x) (eq? (type-of x) 'integer)))
(define float? (lambda (x) (eq? (type-of x) 'float)))
(define symbol? (lambda (x) (eq? (type-of x) 'symbol)))
(define pair? (lambda (x) (eq? (type-of x) 'pair)))
(define string? (lambda (x) (eq? (type-of x) 'string)))
(define vector? (lambda (x) (eq? (type-of x) 'vector)))
(define closure? (lambda (x) (eq? (type-of x) 'closure)))
(define macro? (lambda (x) (eq? (type-of x) 'macro)))
(define boolean? (lambda (x) (eq? (type-of x) 'boolean)))
(define char? (lambda (x) (eq? (type-of x) 'char)))
(define primitive? (lambda (x) (eq? (type-of x) 'primitive)))
(define env-frame? (lambda (x) (eq? (type-of x) 'env-frame)))
(define null? (lambda (x) (eq? x '())))

(define procedure? (lambda (x)
    (let ((type (type-of x)))
        (or (eq? type 'builtin)
            (eq? type 'closure)
            (eq? type 'special-form)
            (eq? type 'macro)))))

(define equal? (lambda (x y)
    (cond
        ((eqv? x y) #t)
        ((and (pair? x) (pair? y)) (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y))))
        ((and (string? x) (string? y)) (string=? x y))
        ((and (vector? x) (vector? y)) (vector=? x y))
        (else #f))))

(define eqv? (lambda (x y)
    (cond ((eq? x y) #t)
        ((and (closure? x) (closure? y)) (eq? x y))
        ((and (macro? x) (macro? y)) (eq? x y))
        ((and (boolean? x) (boolean? y)) (eq? x y))
        ((and (char? x) (char? y)) (eq? x y))
        ((and (primitive? x) (primitive? y)) (eq? x y))
        ((and (env-frame? x) (env-frame? y)) (eq? x y))
        (else #f))))

(define exact? (lambda (n)
    (integer? n)))

(define inexact? (lambda (n)
    (not (integer? n))))

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

(define map (lambda (f l)
    (if (null? l)
        '()
        (cons (f (car l)) (map f (cdr l))))))

(define for-each (lambda (f args)
    (if (null? args)
        #t
        (begin
            (f (car args))
            (for-each f (cdr args))))))

(define displayln (lambda s
    (for-each display+ s)
    (newline)))

(define display+ (lambda (s)
    (display s)
    (display " ")))

(define writeln (lambda args
    (if (null? args)
        (newline)
        (for-each display args))))

(display "s1-core loaded")
(newline)

(define load (lambda (f)
    (begin (define inp (open-input-file f))
        (display inp)
        (push-port! inp))))

(define not (lambda (v) (if v #f #t)))
(define abs (lambda (n) (if (< n 0) (- n) n)))
(define <= (lambda (m n) (not (> m n))))
(define >= (lambda (m n) (not (< m n))))
(define zero? (lambda (n) (= n 0)))
(define positive? (lambda (n) (>= n 0)))
(define negative? (lambda (n) (< n 0)))
(define even? (lambda (n) (zero? (mod n 2))))
(define odd? (lambda (n) (not (even? n))))

(define max (lambda x
    (define _max (lambda (acc l)
        (if (null? l) acc
            (if (> (car l) acc)
                (_max (car l) (cdr l))
                (_max acc (cdr l))))))
    (_max (car x) (cdr x))))

(define min (lambda x
    (define _min (lambda (acc l)
        (if (null? l) acc
            (if (< (car l) acc)
                (_min (car l) (cdr l))
                (_min acc (cdr l))))))
    (_min (car x) (cdr x))))

(define number>string (lambda (n)
    (if (number? n)
        (>string n)
        (error "number>string: not a string"))))

;; Stack support
(define empty? (lambda (s) (null? s)))
(define top (lambda (s) (if (empty? s) #f (car s))))
(define push! (macro (val var) `(set! ,var (cons ,val ,var))))
(define pop! (macro (var) `(let ((result (car ,var))) (set! ,var (cdr ,var)) result)))

; (define map (lambda (f . lists)
;     (display "Fn:") (displayln f)
;     (display "Lists:") (displayln lists)
;   (define any-null? (lambda lsts
;     (cond ((null? lsts) #f)
;           ((null? (car lsts)) #t)
;           (else (any-null? (cdr lsts))))))

;   (define loop (lambda lists
;     (if (any-null? lists)
;         '()
;         (cons (apply f (map car lists))
;               (loop (map cdr lists))))))

;   (loop lists)))

;; Pass in a quoted form and number of times to run it
;; e.g. (benchmark '(fac-acc 1000) 20)
(define benchmark
  (lambda (form count)
    (define total-count count)
    (define b
      (lambda (form count sum)
        (if (= 0 count)
            (/ sum total-count)
            (b form (- count 1) (+ sum (with-timer (eval form)))))))
    (b form count 0.0)))
