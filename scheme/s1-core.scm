;; s1-core.scm: Scheme-level core predicates and utilities

(define error (lambda msg
    (apply displayln msg)))
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
(define env-frame? (lambda (x) (eq? (type-of x) 'env-frame)))
(define port? (lambda (x) (eq? (type-of x) 'port)))

(define (procedure? x)
    (if (memq (type-of x) '(builtin closure sys-builtin))
        #t #f))

;; Character comparisons and predicates
(define char<=?
  (lambda (c1 c2)
    (or (char<? c1 c2)
        (char=? c1 c2))))

(define char>=?
  (lambda (c1 c2)
    (or (char>? c1 c2)
        (char=? c1 c2))))

(define char-alphabetic?
  (lambda (c)
    (or (and (char>=? c #\a) (char<=? c #\z))
        (and (char>=? c #\A) (char<=? c #\Z)))))

(define char-numeric?
  (lambda (c)
    (and (char>=? c #\0) (char<=? c #\9))))

(define char-whitespace?
  (lambda (c)
    (or (char=? c #\space)
        (char=? c #\newline)
        (char=? c #\tab))))

(define char-upper-case?
  (lambda (c)
    (and (char>=? c #\A) (char<=? c #\Z))))

(define char-lower-case?
  (lambda (c)
    (and (char>=? c #\a) (char<=? c #\z))))

(define null? (lambda (x) (eq? x '())))

;; Membership and association functions
(define memq
  (lambda (v l)
    (cond ((null? l) #f)
	  ((eq? v (car l)) l)
	  (else (memq v (cdr l))))))

(define memv
  (lambda (v l)
    (cond ((null? l) #f)
	  ((eqv? v (car l)) l)
	  (else (memv v (cdr l))))))

(define member
  (lambda (v l)
    (cond ((null? l) #f)
	  ((equal? v (car l)) l)
	  (else (member v (cdr l))))))

(define assoc
    (lambda (key alist)
        (cond ((null? alist) #f)
              ((equal? key (caar alist)) (car alist))
              (else (assoc key (cdr alist))))))

(define assv
    (lambda (key alist)
        (cond ((null? alist) #f)
              ((eqv? key (caar alist)) (car alist))
              (else (assv key (cdr alist))))))

(define assq
    (lambda (key alist)
        (cond ((null? alist) #f)
              ((eq? key (caar alist)) (car alist))
              (else (assq key (cdr alist))))))

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

(define cadr (lambda (l) (car (cdr l))))      ;; Second element of a list
(define cdar (lambda (l) (cdr (car l))))      ;; Cdr of the first element
(define caar (lambda (l) (car (car l))))      ;; Car of the first element
(define cddr (lambda (l) (cdr (cdr l))))      ;; Cdr of the cdr of a list

(define caddr (lambda (l) (car (cdr (cdr l)))))     ;; Third element of a list
(define cadddr (lambda (l) (car (cdr (cdr (cdr l)))))) ;; Fourth element of a list
(define cadar (lambda (l) (car (cdr (car l)))))     ;; Car of cdr of car
(define cddar (lambda (l) (cdr (cdr (car l)))))     ;; Cdr of cdr of car
(define caadr (lambda (l) (car (car (cdr l)))))     ;; Car of car of cdr
(define cdadr (lambda (l) (cdr (car (cdr l)))))     ;; Cdr of car of cdr
(define cdddr (lambda (l) (cdr (cdr (cdr l)))))     ;; Cdr of cdr of cdr

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

;; Simplified (single argument) version of map
; (define (map f l)
;     (if (null? l)
;         '()
;         (cons (f (car l)) (map f (cdr l)))))

;; Simplified (single argument) version of for-each
(define (for-each f args)
    (if (null? args)
        #t
        (begin
            (f (car args))
            (for-each f (cdr args)))))

;; Multi-element print function; needs to take an optional port
(define (displayln . s)
    (for-each display+ s)
    (newline))

(define (display+ s)
    (display s)
    (display " "))

(define (writeln . args)
    (if (null? args)
        (newline)
        (for-each display args)))

(display "s1-core loaded")
(newline)

(define load (lambda (f)
    (begin (define inp (open-input-file f))
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

(define number->string (lambda (n)
    (if (number? n)
        (>string n)
        (error "number>string: not a number"))))

(define (string->number str)
    (if (string? str)
        (car (eval-string str))
        (error "string>number: not a string")))

;; Stack support
(define empty? (lambda (s) (null? s)))
(define top (lambda (s) (if (empty? s) #f (car s))))
(define push! (macro (val var) `(set! ,var (cons ,val ,var))))
(define pop! (macro (var) `(let ((result (car ,var))) (set! ,var (cdr ,var)) result)))

(define (zip . lists)
    (if (null? lists)
        '()
        (cons (map car lists)
              (apply zip (map cdr lists)))))

;; Portable R5RS multi-list map
(define (map f . lists)
  ;; helpers to extract first elements and tails
  (define (cars ls)
    (if (null? ls) '()
        (cons (caar ls) (cars (cdr ls)))))
  (define (cdrs ls)
    (if (null? ls) '()
        (cons (cdar ls) (cdrs (cdr ls)))))
  ;; main loop
  (define (loop ls)
    (if (or (null? ls) (null? (car ls))) ; stop when shortest list ends
        '()
        (cons (apply f (cars ls))
              (loop (cdrs ls)))))
  (loop lists))

; (define (map f . lists)
;    (letrec ((cars (lambda (ls)
;                     (if (null? ls) '()
;                         (cons (caar ls) (cars (cdr ls))))))
;             (cdrs (lambda (ls)
;                     (if (null? ls) '()
;                         (cons (cdar ls) (cdrs (cdr ls))))))
;             (loop (lambda (ls)
;                     (if (or (null? ls) (null? (car ls)))
;                         '()
;                         (cons (apply f (cars ls))
;                               (loop (cdrs ls)))))))
;      (loop lists)))

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

(define symbol->string (lambda (sym)
    (if (symbol? sym)
        (>string sym)
        (error "symbol>string: not a symbol"))))

;; (delay expr) and (force p)
(define delay (macro (expr) `(make-promise (lambda () ,expr))))

(define force (lambda (p) (p)))

(define make-promise
    (lambda (proc)
        (let ((result-ready? #f)
              (result #f))
          (lambda ()
            (if result-ready?
                result
                (let ((x (proc)))
                  (if result-ready?
                      result
                      (begin (set! result-ready? #t)
                             (set! result x)
                             result))))))))


(define def
  (macro (sig . body)
      (cond
          ((pair? sig) `(def-fn ,sig ,body))
          ((symbol? sig) `(def-var ,sig ,body))
          (else (error "define: bad syntax" sig body)))))

(define def-fn
  (macro (s . b)
     '(define ,(car s)
       (lambda ,(cdr s)
         (begin ,@b)))))

(define def-var
  (macro (s . b)
    (cond
      ((and (pair? b) (null? (cdr b)))
       `(define ,s ,(car b)))
      (else
       (error "define: expected exactly one value expression")))))

;; File I/O convenience functions

(define call-with-input-file
  (lambda (filename proc)
    (let ((port (open-input-file filename)))
      (let ((result (proc port)))
        (close-input-port port)
        result))))

(define call-with-output-file
  (lambda (filename proc)
    (let ((port (open-output-file filename)))
      (let ((result (proc port)))
        (close-output-port port)
        result))))
