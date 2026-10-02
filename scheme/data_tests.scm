;; data_tests.scm: records, bytevectors, and the list, symbol, character,
;; string and vector procedures of R7RS 6.1-6.10. Also re-run under
;; gc-threshold 1 by gc_stress_tests.scm.

(display "          === Testing records ===")
(newline)
(define-record-type <pare> (kons x y) pare? (x kar set-kar!) (y kdr))
(test-equal '(#t #f) (list (pare? (kons 1 2)) (pare? (cons 1 2))) "record predicate")
(test-equal '(1 2) (list (kar (kons 1 2)) (kdr (kons 1 2))) "record accessors")
(test-equal 3 (let ((k (kons 1 2))) (set-kar! k 3) (kar k)) "record modifier")
(test-equal "record accessor: expected a <pare> record, got (1 . 2)"
    (guard (e (#t (error-object-message e))) (kar (cons 1 2)))
    "an accessor checks the record type")
(define-record-type <other> (make-other x) other? (x other-x))
(test-equal #f (pare? (make-other 1)) "record types are distinct")
(test-equal '(3 5 #t #f)
    (let ()
      (define-record-type point (make-point x y) point? (x px) (y py set-py!))
      (define p (make-point 3 4))
      (set-py! p 5)
      (list (px p) (py p) (point? p) (point? 5)))
    "define-record-type in a body")
(define-record-type node (make-node val) node? (val node-val) (next node-next set-node-next!))
(test-equal 'tail
    (let ((n (make-node 1))) (set-node-next! n 'tail) (node-next n))
    "a field not set by the constructor can be set later")
(test-equal '(#t #f) (list (promise? (delay 1)) (vector? (delay 1))) "promises are records, not vectors")
