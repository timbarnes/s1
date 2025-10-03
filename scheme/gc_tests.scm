(load "scheme/test-harness.scm")

(display "          === GC Tests === ")
(newline)
(gc-threshold 1)

(define gc-test-side-effect-counter 0)
(define gc-test-side-effect-result #f)
(set! gc-test-side-effect-result
  (call/cc
    (lambda (escape)
      (dynamic-wind
        (lambda () (set! gc-test-side-effect-counter (+ gc-test-side-effect-counter 1)))
        (lambda () (escape 'escaped))
        (lambda () (set! gc-test-side-effect-counter (+ gc-test-side-effect-counter 10)))))))

(test-equal 'escaped gc-test-side-effect-result "side effects: escape result")
(test-equal 11 gc-test-side-effect-counter "side effects: counter value")

(gc-threshold 100000)
