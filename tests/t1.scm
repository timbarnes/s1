; nested lambda binding bug
; (define **global** 0)

; (define fails
;   (lambda (x)
;   (set! **global** x)))

; (define bug
;     (lambda ()
;         (fails 1)
;         (display "**CORRECT**")))


; (define test-equal
;   (lambda (expected actual)
;     (cond ((eq? expected actual) #t)
;         (else  #f))))

(define test-equal
  (lambda (expected actual)
    (if (eq? expected actual)
        #t #f)))

(test-equal 42 42)
(debug-env)
;(define x 22)
;(define y 'z)
;(test-equal 22 x)
;(debug-env)
;(test-equal 'z y)
