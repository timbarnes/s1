;; shim.scm: a minimal stand-in for chibi's (chibi test) library, so that the
;; vendored R7RS conformance suite (r7rs-tests.scm) can run on s1.
;;
;; Written with s1's non-hygienic `macro` form because s1 has no syntax-rules
;; yet (plan phase 5). Load it before r7rs-tests.scm; run.sh does this.
;;
;; Counting: every test bumps %r7rs-attempted before its expression is
;; evaluated, inside a `guard`. A test whose expression raises is counted as
;; an error (or a pass, for test-error) and the next test runs normally. If
;; something still escapes the guard, the test never reports; it is counted
;; as an error when the next test (or test-end) notices the pending one.
;; Tests in a top-level form that failed before reaching them (say, a
;; definition the form needs raised) are never attempted at all; run.sh
;; reports those as "not reached".

(define %r7rs-pass 0)
(define %r7rs-fail 0)
(define %r7rs-error 0)
(define %r7rs-attempted 0)
(define %r7rs-pending #f)           ; quoted expr of the test now running
(define %r7rs-sections '())         ; stack of (name pass fail error)
(define %r7rs-epsilon 1e-5)

;; Close out a test that started but never reported back.
(define (%r7rs-settle-pending)
  (if %r7rs-pending
      (begin
        (set! %r7rs-error (+ %r7rs-error 1))
        (display "ERROR: ")
        (write %r7rs-pending)
        (newline)
        (set! %r7rs-pending #f))))

(define (%r7rs-section-name)
  (if (null? %r7rs-sections) "" (car (car %r7rs-sections))))

;; equal?, but inexact numbers only need to agree to a relative epsilon, as in
;; (chibi test).
(define (%r7rs-equal? expected actual)
  (or (equal? expected actual)
      (and (number? expected) (number? actual)
           (or (inexact? expected) (inexact? actual))
           (< (abs (- expected actual))
              (* %r7rs-epsilon (max 1 (abs expected)))))))

(define (%r7rs-start expr)
  (%r7rs-settle-pending)
  (set! %r7rs-attempted (+ %r7rs-attempted 1))
  (set! %r7rs-pending expr))

(define (%r7rs-report ok name expr expected actual)
  (set! %r7rs-pending #f)
  (if ok
      (set! %r7rs-pass (+ %r7rs-pass 1))
      (begin
        (set! %r7rs-fail (+ %r7rs-fail 1))
        (display "FAIL: [")
        (display (%r7rs-section-name))
        (display "] ")
        (if name (begin (write name) (display " ")))
        (write expr)
        (display " expected ")
        (write expected)
        (display " but got ")
        (write actual)
        (newline))))

(define (%r7rs-check name expr expected thunk)
  (%r7rs-start expr)
  (guard (e (#t (%r7rs-raised name expr e)))
    (let ((actual (thunk)))
      (%r7rs-report (%r7rs-equal? expected actual) name expr expected actual))))

;; A test's expression raised `e`.
(define (%r7rs-raised name expr e)
  (set! %r7rs-pending #f)
  (set! %r7rs-error (+ %r7rs-error 1))
  (display "ERROR: [")
  (display (%r7rs-section-name))
  (display "] ")
  (if name (begin (write name) (display " ")))
  (write expr)
  (display " raised ")
  (if (error-object? e)
      (begin (display (error-object-message e))
             (for-each (lambda (i) (display " ") (write i)) (error-object-irritants e)))
      (write e))
  (newline))

;; (test [name] expected expr)
(define test
  (macro args
    (if (= (length args) 3)
        `(%r7rs-check ,(car args) ',(caddr args) ,(cadr args)
                      (lambda () ,(caddr args)))
        `(%r7rs-check #f ',(cadr args) ,(car args)
                      (lambda () ,(cadr args))))))

;; (test-assert [name] expr): passes when expr is true.
(define test-assert
  (macro args
    (let ((name (if (= (length args) 2) (car args) #f))
          (expr (if (= (length args) 2) (cadr args) (car args))))
      `(begin
         (%r7rs-start ',expr)
         (guard (e (#t (%r7rs-raised ,name ',expr e)))
           (let ((actual ,expr))
             (%r7rs-report (if actual #t #f) ,name ',expr #t actual)))))))

;; (test-values expected-expr expr): compares all returned values.
(define test-values
  (macro (expected expr)
    `(%r7rs-check #f ',expr
                  (call-with-values (lambda () ,expected) list)
                  (lambda () (call-with-values (lambda () ,expr) list)))))

;; (test-error expr): passes when expr raises.
(define test-error
  (macro (expr)
    `(begin
       (%r7rs-start ',expr)
       (guard (e (#t (%r7rs-report #t #f ',expr 'an-error e)))
         (let ((actual ,expr))
           (%r7rs-report #f #f ',expr 'an-error actual))))))

(define (test-begin . name)
  (%r7rs-settle-pending)
  (set! %r7rs-sections
        (cons (list (if (null? name) "" (car name))
                    %r7rs-pass %r7rs-fail %r7rs-error)
              %r7rs-sections))
  (display "== ")
  (display (%r7rs-section-name))
  (newline))

;; Prints "SECTION pass=P fail=F error=E name" for the section just closed;
;; run.sh collects these lines.
(define (test-end . name)
  (%r7rs-settle-pending)
  (if (pair? %r7rs-sections)
      (let ((s (car %r7rs-sections)))
        (display "SECTION pass=")
        (display (- %r7rs-pass (cadr s)))
        (display " fail=")
        (display (- %r7rs-fail (caddr s)))
        (display " error=")
        (display (- %r7rs-error (cadddr s)))
        (display " ")
        (display (car s))
        (newline)
        (set! %r7rs-sections (cdr %r7rs-sections)))))

;; Called by run.sh after the suite: the line run.sh parses for totals.
(define (%r7rs-summary)
  (%r7rs-settle-pending)
  (display "SUMMARY attempted=")
  (display %r7rs-attempted)
  (display " pass=")
  (display %r7rs-pass)
  (display " fail=")
  (display %r7rs-fail)
  (display " error=")
  (display %r7rs-error)
  (newline))
