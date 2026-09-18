;; s1-core.scm: Scheme-level core predicates and utilities

(define error (lambda msg
    "(error msg ...) displays msg and any additional arguments followed by a newline; used to report a condition since there is no error/condition system"
    (apply displayln msg)))
;; Type predicates using type-of function
(define number? (lambda (x)
    "(number? x) returns #t if x is an integer or float, otherwise #f"
    (or (eq? (type-of x) 'integer) (eq? (type-of x) 'float))))
(define integer? (lambda (x)
    "(integer? x) returns #t if x is an integer, otherwise #f"
    (eq? (type-of x) 'integer)))
(define float? (lambda (x)
    "(float? x) returns #t if x is a float, otherwise #f"
    (eq? (type-of x) 'float)))
(define symbol? (lambda (x)
    "(symbol? x) returns #t if x is a symbol, otherwise #f"
    (eq? (type-of x) 'symbol)))
(define pair? (lambda (x)
    "(pair? x) returns #t if x is a pair, otherwise #f"
    (eq? (type-of x) 'pair)))
(define string? (lambda (x)
    "(string? x) returns #t if x is a string, otherwise #f"
    (eq? (type-of x) 'string)))
(define vector? (lambda (x)
    "(vector? x) returns #t if x is a vector, otherwise #f"
    (eq? (type-of x) 'vector)))
(define closure? (lambda (x)
    "(closure? x) returns #t if x is a Scheme-defined procedure (a closure), otherwise #f"
    (eq? (type-of x) 'closure)))
(define macro? (lambda (x)
    "(macro? x) returns #t if x is a macro, otherwise #f"
    (eq? (type-of x) 'macro)))
(define boolean? (lambda (x)
    "(boolean? x) returns #t if x is a boolean, otherwise #f"
    (eq? (type-of x) 'boolean)))
(define char? (lambda (x)
    "(char? x) returns #t if x is a character, otherwise #f"
    (eq? (type-of x) 'char)))
(define env-frame? (lambda (x)
    "(env-frame? x) returns #t if x is an environment frame, otherwise #f"
    (eq? (type-of x) 'env-frame)))
(define port? (lambda (x)
    "(port? x) returns #t if x is a port, otherwise #f"
    (eq? (type-of x) 'port)))

(define (procedure? x)
    "(procedure? x) returns #t if x is callable (a builtin, closure, or sys-builtin), otherwise #f"
    (if (memq (type-of x) '(builtin closure sys-builtin))
        #t #f))

;; Character comparisons and predicates
(define char<=?
  (lambda (c1 c2)
    "(char<=? c1 c2) returns #t if c1 is less than or equal to c2, otherwise #f"
    (or (char<? c1 c2)
        (char=? c1 c2))))

(define char>=?
  (lambda (c1 c2)
    "(char>=? c1 c2) returns #t if c1 is greater than or equal to c2, otherwise #f"
    (or (char>? c1 c2)
        (char=? c1 c2))))

(define char-alphabetic?
  (lambda (c)
    "(char-alphabetic? c) returns #t if c is an ASCII letter (a-z or A-Z), otherwise #f"
    (or (and (char>=? c #\a) (char<=? c #\z))
        (and (char>=? c #\A) (char<=? c #\Z)))))

(define char-numeric?
  (lambda (c)
    "(char-numeric? c) returns #t if c is an ASCII digit (0-9), otherwise #f"
    (and (char>=? c #\0) (char<=? c #\9))))

(define char-whitespace?
  (lambda (c)
    "(char-whitespace? c) returns #t if c is a space, newline, or tab, otherwise #f"
    (or (char=? c #\space)
        (char=? c #\newline)
        (char=? c #\tab))))

(define char-upper-case?
  (lambda (c)
    "(char-upper-case? c) returns #t if c is an ASCII uppercase letter (A-Z), otherwise #f"
    (and (char>=? c #\A) (char<=? c #\Z))))

(define char-lower-case?
  (lambda (c)
    "(char-lower-case? c) returns #t if c is an ASCII lowercase letter (a-z), otherwise #f"
    (and (char>=? c #\a) (char<=? c #\z))))

(define null? (lambda (x)
    "(null? x) returns #t if x is the empty list, otherwise #f"
    (eq? x '())))

;; Membership and association functions
(define memq
  (lambda (v l)
    "(memq v l) returns the sublist of l starting with the first element eq? to v, or #f if none is found"
    (cond ((null? l) #f)
	  ((eq? v (car l)) l)
	  (else (memq v (cdr l))))))

(define memv
  (lambda (v l)
    "(memv v l) returns the sublist of l starting with the first element eqv? to v, or #f if none is found"
    (cond ((null? l) #f)
	  ((eqv? v (car l)) l)
	  (else (memv v (cdr l))))))

(define member
  (lambda (v l)
    "(member v l) returns the sublist of l starting with the first element equal? to v, or #f if none is found"
    (cond ((null? l) #f)
	  ((equal? v (car l)) l)
	  (else (member v (cdr l))))))

(define assoc
    (lambda (key alist)
        "(assoc key alist) returns the first pair in alist whose car is equal? to key, or #f if none is found"
        (cond ((null? alist) #f)
              ((equal? key (caar alist)) (car alist))
              (else (assoc key (cdr alist))))))

(define assv
    (lambda (key alist)
        "(assv key alist) returns the first pair in alist whose car is eqv? to key, or #f if none is found"
        (cond ((null? alist) #f)
              ((eqv? key (caar alist)) (car alist))
              (else (assv key (cdr alist))))))

(define assq
    (lambda (key alist)
        "(assq key alist) returns the first pair in alist whose car is eq? to key, or #f if none is found"
        (cond ((null? alist) #f)
              ((eq? key (caar alist)) (car alist))
              (else (assq key (cdr alist))))))

(define eqv? (lambda (x y)
    "(eqv? x y) returns #t if x and y are the same object, or equal atoms of the same primitive type, otherwise #f"
    (cond ((eq? x y) #t)
        ((and (closure? x) (closure? y)) (eq? x y))
        ((and (macro? x) (macro? y)) (eq? x y))
        ((and (boolean? x) (boolean? y)) (eq? x y))
        ((and (char? x) (char? y)) (eq? x y))
        ((and (primitive? x) (primitive? y)) (eq? x y))
        ((and (env-frame? x) (env-frame? y)) (eq? x y))
        (else #f))))

(define exact? (lambda (n)
    "(exact? n) returns #t if n is an integer, otherwise #f"
    (integer? n)))

(define inexact? (lambda (n)
    "(inexact? n) returns #t if n is not an integer, otherwise #f"
    (not (integer? n))))

;; List accessor functions (compositions of car and cdr)
;; These provide convenient access to nested list elements

(define cadr (lambda (l) "(cadr list) -> second element of list" (car (cdr l))))
(define cdar (lambda (l) "(cdar list) -> cdr of the first element of list" (cdr (car l))))
(define caar (lambda (l) "(caar list) -> car of the first element of list" (car (car l))))
(define cddr (lambda (l) "(cddr list) -> cdr of the cdr of list" (cdr (cdr l))))

(define caddr (lambda (l) "(caddr list) -> third element of list" (car (cdr (cdr l)))))
(define cadddr (lambda (l) "(cadddr list) -> fourth element of list" (car (cdr (cdr (cdr l))))))
(define cadar (lambda (l) "(cadar list) -> car of cdr of car of list" (car (cdr (car l)))))
(define cddar (lambda (l) "(cddar list) -> cdr of cdr of car of list" (cdr (cdr (car l)))))
(define caadr (lambda (l) "(caadr list) -> car of car of cdr of list" (car (car (cdr l)))))
(define cdadr (lambda (l) "(cdadr list) -> cdr of car of cdr of list" (cdr (car (cdr l)))))
(define cdddr (lambda (l) "(cdddr list) -> cdr of cdr of cdr of list" (cdr (cdr (cdr l)))))

(define caaar (lambda (l) "(caaar list) -> car of car of car of list" (car (car (car l)))))
(define cdaar (lambda (l) "(cdaar list) -> cdr of car of car of list" (cdr (car (car l)))))
(define caaadr (lambda (l) "(caaadr list) -> car of car of car of cdr of list" (car (car (car (cdr l))))))
(define cdaadr (lambda (l) "(cdaadr list) -> cdr of car of car of cdr of list" (cdr (car (car (cdr l))))))
(define cadadr (lambda (l) "(cadadr list) -> car of cdr of car of cdr of list" (car (cdr (car (cdr l))))))
(define cddadr (lambda (l) "(cddadr list) -> cdr of cdr of car of cdr of list" (cdr (cdr (car (cdr l))))))
(define caaddr (lambda (l) "(caaddr list) -> car of car of cdr of cdr of list" (car (car (cdr (cdr l))))))
(define cdaddr (lambda (l) "(cdaddr list) -> cdr of car of cdr of cdr of list" (cdr (car (cdr (cdr l))))))
(define cdddr (lambda (l) "(cdddr list) -> cdr of cdr of cdr of list" (cdr (cdr (cdr l)))))
(define cddddr (lambda (l) "(cddddr list) -> cdr of cdr of cdr of cdr of list" (cdr (cdr (cdr (cdr l))))))

;; Simplified (single argument) version of map
; (define (map f l)
;     (if (null? l)
;         '()
;         (cons (f (car l)) (map f (cdr l)))))

;; Simplified (single argument) version of for-each
(define (for-each f args)
    "(for-each f list) calls f on each element of list, in order, for effect, and returns #t"
    (if (null? args)
        #t
        (begin
            (f (car args))
            (for-each f (cdr args)))))

;; Multi-element print function; needs to take an optional port
(define (displayln . s)
    "(displayln arg ...) displays each argument separated by spaces, followed by a newline"
    (for-each display+ s)
    (newline))

(define (display+ s)
    "(display+ s) displays s followed by a space"
    (display s)
    (display " "))

(define (writeln . args)
    "(writeln arg ...) displays each argument with no separators, or just a newline if there are no arguments"
    (if (null? args)
        (newline)
        (for-each display args)))

(display "s1-core loaded")
(newline)

(define load (lambda (f)
    "(load filename) opens filename and pushes it onto the port stack so the interpreter reads and evaluates its contents next"
    (begin (define inp (open-input-file f))
        (push-port! inp))))

(define not (lambda (v)
    "(not v) returns #t if v is #f, otherwise #f"
    (if v #f #t)))
(define abs (lambda (n)
    "(abs n) returns the absolute value of n"
    (if (< n 0) (- n) n)))
(define <= (lambda (m n)
    "(<= m n) returns #t if m is less than or equal to n, otherwise #f"
    (not (> m n))))
(define >= (lambda (m n)
    "(>= m n) returns #t if m is greater than or equal to n, otherwise #f"
    (not (< m n))))
(define zero? (lambda (n)
    "(zero? n) returns #t if n is 0, otherwise #f"
    (= n 0)))
(define positive? (lambda (n)
    "(positive? n) returns #t if n is greater than or equal to 0, otherwise #f"
    (>= n 0)))
(define negative? (lambda (n)
    "(negative? n) returns #t if n is less than 0, otherwise #f"
    (< n 0)))
(define even? (lambda (n)
    "(even? n) returns #t if n is evenly divisible by 2, otherwise #f"
    (zero? (modulo n 2))))
(define odd? (lambda (n)
    "(odd? n) returns #t if n is not evenly divisible by 2, otherwise #f"
    (not (even? n))))

(define max (lambda x
    "(max n1 n2 ...) returns the largest of its arguments"
    (define _max (lambda (acc l)
        (if (null? l) acc
            (if (> (car l) acc)
                (_max (car l) (cdr l))
                (_max acc (cdr l))))))
    (_max (car x) (cdr x))))

(define min (lambda x
    "(min n1 n2 ...) returns the smallest of its arguments"
    (define _min (lambda (acc l)
        (if (null? l) acc
            (if (< (car l) acc)
                (_min (car l) (cdr l))
                (_min acc (cdr l))))))
    (_min (car x) (cdr x))))

(define number->string (lambda (n)
    "(number->string n) converts the number n to its string representation and returns the string"
    (if (number? n)
        (>string n)
        (error "number>string: not a number"))))

(define (string->number str)
    "(string->number str) parses str as a number and returns the number"
    (if (string? str)
        (car (eval-string str))
        (error "string>number: not a string")))

;; Stack support
(define empty? (lambda (s)
    "(empty? s) returns #t if the stack/list s is empty, otherwise #f"
    (null? s)))
(define top (lambda (s)
    "(top s) returns the top element of stack s, or #f if s is empty"
    (if (empty? s) #f (car s))))
(define push! (macro (val var)
    "(push! val var) pushes val onto the front of the list bound to var, mutating var"
    `(set! ,var (cons ,val ,var))))
(define pop! (macro (var)
    "(pop! var) removes and returns the first element of the list bound to var, mutating var"
    `(let ((result (car ,var))) (set! ,var (cdr ,var)) result)))

(define (zip . lists)
    "(zip list ...) returns a list of lists, pairing up the i-th elements of each input list"
    (if (null? lists)
        '()
        (cons (map car lists)
              (apply zip (map cdr lists)))))

;; Portable R5RS multi-list map
(define (map f . lists)
  "(map f list ...) applies f to corresponding elements of each list and returns a list of the results, stopping at the shortest list"
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
    "(benchmark form count) evaluates form count times and returns the average elapsed time in seconds"
    (define total-count count)
    (define b
      (lambda (form count sum)
        (if (= 0 count)
            (/ sum total-count)
            (b form (- count 1) (+ sum (with-timer (eval form)))))))
    (b form count 0.0)))

(define symbol->string (lambda (sym)
    "(symbol->string sym) converts the symbol sym to a string and returns the string"
    (if (symbol? sym)
        (>string sym)
        (error "symbol>string: not a symbol"))))

;; (delay expr) and (force p)
(define delay (macro (expr)
    "(delay expr) returns a promise that, when forced, evaluates expr at most once and caches the result"
    `(make-promise (lambda () ,expr))))

(define force (lambda (p)
    "(force p) evaluates the promise p (if not already forced) and returns its value"
    (p)))

(define make-promise
    (lambda (proc)
        "(make-promise proc) wraps thunk proc in a promise that calls proc and caches its result the first time it's invoked, and returns the cached result on later calls"
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
      "(def sig . body) defines a function if sig is (name . args), or a variable if sig is a plain symbol"
      (cond
          ((pair? sig) `(def-fn ,sig ,body))
          ((symbol? sig) `(def-var ,sig ,body))
          (else (error "define: bad syntax" sig body)))))

(define def-fn
  (macro (s . b)
     "(def-fn (name . args) body) expands to a (define name (lambda args body...)) function definition"
     '(define ,(car s)
       (lambda ,(cdr s)
         (begin ,@b)))))

(define def-var
  (macro (s . b)
    "(def-var name (value)) expands to a (define name value) variable definition; body must be exactly one expression"
    (cond
      ((and (pair? b) (null? (cdr b)))
       `(define ,s ,(car b)))
      (else
       (error "define: expected exactly one value expression")))))

;; File I/O convenience functions

(define call-with-input-file
  (lambda (filename proc)
    "(call-with-input-file filename proc) opens filename for input, calls proc with the port, closes the port, and returns proc's result"
    (let ((port (open-input-file filename)))
      (let ((result (proc port)))
        (close-input-port port)
        result))))

(define call-with-output-file
  (lambda (filename proc)
    "(call-with-output-file filename proc) opens filename for output, calls proc with the port, closes the port, and returns proc's result"
    (let ((port (open-output-file filename)))
      (let ((result (proc port)))
        (close-output-port port)
        result))))
