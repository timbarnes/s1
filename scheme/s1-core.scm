;; s1-core.scm: Scheme-level core predicates and utilities

;; nil is an ordinary symbol, as R7RS requires; as a variable it is bound to
;; the empty list, so code that uses nil as a value keeps working.
(define nil '())

;; Type predicates using type-of function
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

;; member and assoc take an optional equality predicate (R7RS 6.4); memq,
;; memv, assq and assv are built in.
(define (member x lst . compare)
  "(member x list [compare]) returns the first sublist of list whose car is equal? (or compare) to x, or #f"
  (let ((same? (if (null? compare) equal? (car compare))))
    (let loop ((l lst))
      (cond ((null? l) #f)
            ((same? x (car l)) l)
            (else (loop (cdr l)))))))

(define (assoc x alist . compare)
  "(assoc x alist [compare]) returns the first pair in alist whose car is equal? (or compare) to x, or #f"
  (let ((same? (if (null? compare) equal? (car compare))))
    (let loop ((l alist))
      (cond ((null? l) #f)
            ((same? x (caar l)) (car l))
            (else (loop (cdr l)))))))

(define (procedure? x)
    "(procedure? x) returns #t if x is callable (a builtin, closure, sys-builtin or case-lambda), otherwise #f"
    (if (memq (type-of x) '(builtin closure sys-builtin case-lambda))
        #t #f))

;; Character procedures are built in (src/builtin/char.rs).
(define null? (lambda (x)
    "(null? x) returns #t if x is the empty list, otherwise #f"
    (eq? x '())))

;; Membership and association functions
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
(define caaaar (lambda (l) "(caaaar list) -> car of car of car of car of list" (car (car (car (car l))))))
(define caadar (lambda (l) "(caadar list) -> car of car of cdr of car of list" (car (car (cdr (car l))))))
(define cadaar (lambda (l) "(cadaar list) -> car of cdr of car of car of list" (car (cdr (car (car l))))))
(define caddar (lambda (l) "(caddar list) -> car of cdr of cdr of car of list" (car (cdr (cdr (car l))))))
(define cdaaar (lambda (l) "(cdaaar list) -> cdr of car of car of car of list" (cdr (car (car (car l))))))
(define cdadar (lambda (l) "(cdadar list) -> cdr of car of cdr of car of list" (cdr (car (cdr (car l))))))
(define cddaar (lambda (l) "(cddaar list) -> cdr of cdr of car of car of list" (cdr (cdr (car (car l))))))
(define cdddar (lambda (l) "(cdddar list) -> cdr of cdr of cdr of car of list" (cdr (cdr (cdr (car l))))))
(define cddddr (lambda (l) "(cddddr list) -> cdr of cdr of cdr of cdr of list" (cdr (cdr (cdr (cdr l))))))

;; Simplified (single argument) version of map
; (define (map f l)
;     (if (null? l)
;         '()
;         (cons (f (car l)) (map f (cdr l)))))

;; Simplified (single argument) version of for-each
(define (%any-null? lists)
    (cond ((null? lists) #f)
          ((null? (car lists)) #t)
          (else (%any-null? (cdr lists)))))

(define (for-each f lst . lists)
    "(for-each f list1 list2 ...) calls f on corresponding elements of the lists, in order, for effect, stopping at the shortest list, and returns #t"
    (if (null? lists)
        (let loop ((l lst))
          (if (pair? l) (begin (f (car l)) (loop (cdr l))) #t))
        (let loop ((ls (cons lst lists)))
          (if (%any-null? ls)
              #t
              (begin (apply f (map car ls)) (loop (map cdr ls)))))))

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

(define load (lambda (f . env)
    "(load filename [env]) evaluates the forms in filename. Without env, the file is pushed onto the port stack so the interpreter reads and evaluates its contents next, in the interaction environment; with env, its forms are read and evaluated in env before load returns"
    (if (null? env)
        (push-port! (open-input-file f))
        (let ((port (open-input-file f)))
          (let loop ((form (read port)))
            (if (eof-object? form)
                (close-port port)
                (begin (eval form (car env)) (loop (read port)))))))))

(define not (lambda (v)
    "(not v) returns #t if v is #f, otherwise #f"
    (if v #f #t)))
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
    (if (or (null? lists) (%any-null? lists))
        '()
        (cons (map car lists)
              (apply zip (map cdr lists)))))

;; Portable R5RS multi-list map
(define (map f . lists)
  "(map f list ...) applies f to corresponding elements of each list and returns a list of the results, stopping at the shortest list"
  (if (and (pair? lists) (null? (cdr lists)))
      ;; One list, the common case: walk it directly, about five times
      ;; faster than the general loop below.
      (let loop ((l (car lists)))
        (if (null? l) '() (cons (f (car l)) (loop (cdr l)))))
      (%map-lists f lists)))

(define (%map-lists f lists)
  "(%map-lists f lists) is map for any number of lists"
  ;; helpers to extract first elements and tails
  (define (cars ls)
    (if (null? ls) '()
        (cons (caar ls) (cars (cdr ls)))))
  (define (cdrs ls)
    (if (null? ls) '()
        (cons (cdar ls) (cdrs (cdr ls)))))
  (define (any-null? ls)
    (cond ((null? ls) #f)
          ((null? (car ls)) #t)
          (else (any-null? (cdr ls)))))
  ;; main loop
  (define (loop ls)
    (if (or (null? ls) (any-null? ls)) ; stop when the shortest list ends
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

(define def
  (macro (sig . body)
      "(def sig . body) defines a function if sig is (name . args), or a variable if sig is a plain symbol"
      (cond
          ((pair? sig) `(def-fn ,sig ,@body))
          ((symbol? sig) `(def-var ,sig ,@body))
          (else (error "define: bad syntax" sig body)))))

(define def-fn
  (macro (s . b)
     "(def-fn (name . args) body) expands to a (define name (lambda args body...)) function definition"
     `(define ,(car s)
       (lambda ,(cdr s)
         ,@b))))

(define def-var
  (macro (s . b)
    "(def-var name (value)) expands to a (define name value) variable definition; body must be exactly one expression"
    (cond
      ((and (pair? b) (null? (cdr b)))
       `(define ,s ,(car b)))
      (else
       (error "define: expected exactly one value expression")))))

;; File and port conveniences (R7RS 6.13.1)

(define (call-with-port port proc)
  "(call-with-port port proc) calls proc with port, closes the port, and returns proc's result"
  (let ((result (proc port)))
    (close-port port)
    result))

(define (call-with-input-file filename proc)
  "(call-with-input-file filename proc) opens filename for input, calls proc with the port, closes it, and returns proc's result"
  (call-with-port (open-input-file filename) proc))

(define (call-with-output-file filename proc)
  "(call-with-output-file filename proc) opens filename for output, calls proc with the port, closes it, and returns proc's result"
  (call-with-port (open-output-file filename) proc))

;;; ---------------------------------------------------------------------------
;;; R7RS derived expression types (section 4.2) and related procedures.
;;; Most definitions follow R7RS's own reference implementations (7.3), which
;;; rely on syntax-rules hygiene: the `tmp`, `x` and `args` they introduce
;;; can't capture user variables. case-lambda and guard are built in.
;;; ---------------------------------------------------------------------------

(define-syntax when
  (syntax-rules ()
    ((_ test result1 result2 ...) (if test (begin result1 result2 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((_ test result1 result2 ...) (if (not test) (begin result1 result2 ...)))))

(define-syntax case
  (syntax-rules (else =>)
    ((_ (key ...) clauses ...)
     (let ((atom-key (key ...))) (case atom-key clauses ...)))
    ((_ key (else => result)) (result key))
    ((_ key (else result1 result2 ...)) (begin result1 result2 ...))
    ((_ key ((atoms ...) => result))
     (if (memv key '(atoms ...)) (result key)))
    ((_ key ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...)) (begin result1 result2 ...)))
    ((_ key ((atoms ...) => result) clause clauses ...)
     (if (memv key '(atoms ...)) (result key) (case key clause clauses ...)))
    ((_ key ((atoms ...) result1 result2 ...) clause clauses ...)
     (if (memv key '(atoms ...)) (begin result1 result2 ...) (case key clause clauses ...)))))

;; s1's letrec already initialises its bindings left to right, which is
;; letrec*'s guarantee.
(define-syntax letrec*
  (syntax-rules ()
    ((_ bindings body1 body2 ...) (letrec bindings body1 body2 ...))))

(define-syntax let*-values
  (syntax-rules ()
    ((_ () body1 body2 ...) (let () body1 body2 ...))
    ((_ ((formals init) binding ...) body1 body2 ...)
     (call-with-values (lambda () init)
       (lambda formals (let*-values (binding ...) body1 body2 ...))))))

;; All inits are evaluated before any variable is bound: each formal is first
;; bound to a fresh temporary, and the real names are bound together at the end.
(define-syntax let-values
  (syntax-rules ()
    ((_ (binding ...) body0 body1 ...)
     (let-values "bind" (binding ...) () (let () body0 body1 ...)))
    ((_ "bind" () tmps body)
     (let tmps body))
    ((_ "bind" ((b0 e0) binding ...) tmps body)
     (let-values "mktmp" b0 e0 () (binding ...) tmps body))
    ((_ "mktmp" () e0 args bindings tmps body)
     (call-with-values (lambda () e0)
       (lambda args (let-values "bind" bindings tmps body))))
    ((_ "mktmp" (a . b) e0 (arg ...) bindings (tmp ...) body)
     (let-values "mktmp" b e0 (arg ... x) bindings (tmp ... (a x)) body))
    ((_ "mktmp" a e0 (arg ...) bindings (tmp ...) body)
     (call-with-values (lambda () e0)
       (lambda (arg ... . x) (let-values "bind" bindings (tmp ... (a x)) body))))))

(define-syntax define-values
  (syntax-rules ()
    ((_ () expr)
     (define dummy (call-with-values (lambda () expr) (lambda args #f))))
    ((_ (var) expr)
     (define var expr))
    ((_ (var0 var1 ... varn) expr)
     (begin
       (define var0 (call-with-values (lambda () expr) list))
       (define var1 (let ((v (cadr var0))) (set-cdr! var0 (cddr var0)) v)) ...
       (define varn (let ((v (cadr var0))) (set! var0 (car var0)) v))))
    ((_ (var0 var1 ... . varn) expr)
     (begin
       (define var0 (call-with-values (lambda () expr) list))
       (define var1 (let ((v (cadr var0))) (set-cdr! var0 (cddr var0)) v)) ...
       (define varn (let ((v (cdr var0))) (set! var0 (car var0)) v))))
    ((_ var expr)
     (define var (call-with-values (lambda () expr) list)))))

;; --- Parameters (R7RS 4.2.6)
;; A parameter object is a procedure: called with no arguments it returns the
;; current value. parameterize talks to it through two private markers,
;; which the built-in current-input-port and friends also understand.
(define %param-set '%param-set)
(define %param-converter '%param-converter)

(define (make-parameter value . converter)
  "(make-parameter value [converter]) returns a parameter object whose value is (converter value)"
  (let* ((convert (if (null? converter) (lambda (x) x) (car converter)))
         (current (convert value)))
    (lambda args
      (cond ((null? args) current)
            ((eq? (car args) %param-set) (set! current (cadr args)))
            ((eq? (car args) %param-converter) convert)
            (else (error "parameter object called with arguments" args))))))

;; Each binding swaps the converted value in on entry to its dynamic extent
;; and back out on exit, including exits and re-entries through continuations.
(define-syntax parameterize
  (syntax-rules ()
    ((_ () body ...) (let () body ...))
    ((_ ((param value) rest ...) body ...)
     (let* ((p param)
            (new ((p %param-converter) value))
            (old #f))
       (dynamic-wind
         (lambda () (set! old (p)) (p %param-set new))
         (lambda () (parameterize (rest ...) body ...))
         (lambda () (set! new (p)) (p %param-set old)))))))

(define-syntax define-record-type
  (syntax-rules ()
    ((_ type (constructor field ...) predicate fieldspec ...)
     (begin
       (define type (%make-record-type 'type '(fieldspec ...)))
       (define (constructor field ...) (%record-make type '(field ...) (list field ...)))
       (define (predicate obj) (%record? obj type))
       (%define-record-field type fieldspec) ...))))

;; One field spec: (field), (field accessor) or (field accessor modifier).
(define-syntax %define-record-field
  (syntax-rules ()
    ((_ type (field)) 'field)
    ((_ type (field accessor))
     (define (accessor obj) (%record-get obj type 'field)))
    ((_ type (field accessor modifier))
     (begin
       (define (accessor obj) (%record-get obj type 'field))
       (define (modifier obj value) (%record-set! obj type 'field value))))))

;; --- Promises (R7RS 4.2.5), following the reference implementation, which
;; forces chains of delay-force iteratively (in constant space).
;; A promise holds a box (done? . value-or-thunk); promises that share a box
;; were merged by force.
(define-record-type %promise
  (%make-promise-record box)
  promise?
  (box %promise-box %promise-set-box!))
(define (%make-promise done? value) (%make-promise-record (cons done? value)))
(define (%promise-done? p) (car (%promise-box p)))
(define (%promise-value p) (cdr (%promise-box p)))
(define (%promise-update! new old)
  (set-car! (%promise-box old) (%promise-done? new))
  (set-cdr! (%promise-box old) (%promise-value new))
  (%promise-set-box! new (%promise-box old)))

(define (force promise)
  "(force promise) returns the value of promise, computing it the first time; a non-promise is returned as is"
  (if (promise? promise)
      (if (%promise-done? promise)
          (%promise-value promise)
          (%force-step promise ((%promise-value promise))))
      promise))
(define (%force-step promise promise*)
  (if (not (%promise-done? promise)) (%promise-update! promise* promise))
  (force promise))

(define (make-promise obj)
  "(make-promise obj) returns obj if it is a promise, otherwise a promise already forced to obj"
  (if (promise? obj) obj (%make-promise #t obj)))

(define-syntax delay-force
  (syntax-rules () ((_ expr) (%make-promise #f (lambda () expr)))))
(define-syntax delay
  (syntax-rules () ((_ expr) (delay-force (%make-promise #t expr)))))

;; --- Mapping over strings and vectors (R7RS 6.10), via lists. Like map and
;; for-each, they stop at the shortest argument.
(define (string-map f s . strings)
  "(string-map f string1 string2 ...) returns a string of the results of f on corresponding characters"
  (list->string (apply map f (map string->list (cons s strings)))))
(define (string-for-each f s . strings)
  "(string-for-each f string1 string2 ...) calls f on corresponding characters, in order, for effect"
  (apply for-each f (map string->list (cons s strings))))
(define (vector-map f v . vectors)
  "(vector-map f vector1 vector2 ...) returns a vector of the results of f on corresponding elements"
  (list->vector (apply map f (map vector->list (cons v vectors)))))
(define (vector-for-each f v . vectors)
  "(vector-for-each f vector1 vector2 ...) calls f on corresponding elements, in order, for effect"
  (apply for-each f (map vector->list (cons v vectors))))

;; Defined after parameterize: the current ports are parameter-like.
(define (with-input-from-file filename thunk)
  "(with-input-from-file filename thunk) calls thunk with the current input port reading filename"
  (let ((port (open-input-file filename)))
    (let ((result (parameterize ((current-input-port port)) (thunk))))
      (close-port port)
      result)))

(define (with-output-to-file filename thunk)
  "(with-output-to-file filename thunk) calls thunk with the current output port writing to filename"
  (let ((port (open-output-file filename)))
    (let ((result (parameterize ((current-output-port port)) (thunk))))
      (close-port port)
      result)))

;; --- Procedure tracing (an s1 extension; see docs/extensions.md).
;; (trace-procedure f) rebinds the variable f to a wrapper that writes each
;; call and its result to the current error port, indented by how many
;; traced calls enclose it; (untrace-procedure f) puts the original back.
;; Untraced code pays nothing. A traced procedure's calls are not tail
;; calls, since the wrapper waits for the result.
(define %traced '())              ; (wrapper . original) pairs
(define %trace-level (make-parameter 0))

(define (%trace-line level mark items)
  (let ((port (current-error-port)))
    (let indent ((i 0))
      (when (< i level) (write-string "| " port) (indent (+ i 1))))
    (write-string mark port)
    (let loop ((items items) (sep ""))
      (unless (null? items)
        (write-string sep port)
        (write (car items) port)
        (loop (cdr items) " ")))
    (newline port)))

(define (%trace-wrap name proc)
  (cond ((assq proc %traced) proc)
        ((not (procedure? proc)) (error "trace-procedure: not a procedure:" name))
        (else
         (let ((wrapper
                (lambda args
                  (let ((level (%trace-level)))
                    (%trace-line level "> " (list (cons name args)))
                    (call-with-values
                      (lambda ()
                        (parameterize ((%trace-level (+ level 1))) (apply proc args)))
                      (lambda results
                        (%trace-line level "< " results)
                        (apply values results)))))))
           (set! %traced (cons (cons wrapper proc) %traced))
           wrapper))))

(define (%trace-unwrap proc)
  (let ((entry (assq proc %traced)))
    (if entry (cdr entry) proc)))

(define-syntax trace-procedure
  (syntax-rules ()
    ((_ name) (begin (set! name (%trace-wrap 'name name)) 'name))))

(define-syntax untrace-procedure
  (syntax-rules ()
    ((_ name) (begin (set! name (%trace-unwrap name)) 'name))))
