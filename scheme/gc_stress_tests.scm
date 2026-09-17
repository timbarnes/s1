;; gc_stress_tests.scm
;;
;; Regression coverage for Docs/nested-evaluation.md: five places used to
;; evaluate Scheme code by re-entering eval_main and parking the outer
;; machine state (kont/env) in Rust locals, which are invisible to the GC's
;; mark phase. At a low gc-threshold, a collection during that nested
;; evaluation could free objects still referenced only from those locals.
;;
;; Running macro/quasiquote-heavy code at gc-threshold 1 (a collection
;; before nearly every allocation) is a cheap standing check for the whole
;; "interpreter state held in a Rust local is not a GC root" class described
;; there.

(display "          === GC stress: macro/quasiquote under gc-threshold 1 ===")
(newline)

(define **saved-gc-threshold** (gc-threshold))
(gc-threshold 1)

;; Defect 1 (eval_macro's unrooted saved_kont/saved_env): re-run the macro
;; suite -- every macro call goes through the same expansion path -- under
;; a threshold low enough to guarantee a collection mid-expansion.
(load "scheme/macro_tests.scm")

;; Defect 2 (quasiquote_sf's unrooted saved_kont, and defect 3, the
;; macro-expander's own Rust-local intermediates): deeply recursive calls
;; inside nested unquotes, repeated enough times to force many collections
;; while quasiquote templates are mid-expansion.
(define (deep n) (if (= n 0) 0 (+ 1 (deep (- n 1)))))
(define (qq-probe) (deep 50) (list 1 2 3))
(define (qq-stress i)
  (if (= i 0)
      'done
      (begin
        (test-equal '(h (1 2 3) (1 2 3) (1 2 3) t)
            `(h ,(qq-probe) ,(qq-probe) ,(qq-probe) t)
            "quasiquote survives gc pressure mid-expansion")
        (qq-stress (- i 1)))))
(qq-stress 50)

;; eval-string's old implementation held its per-form results (and the
;; caller's continuation) across nested eval_main calls the same way
;; eval_macro did; doc/nested-evaluation.md lists this as latent rather than
;; independently verified. Run it under the same gc-threshold 1 pressure as
;; a standing check now that it is driven from a Kont::EvalSeq frame instead.
(define (es-stress i)
  (if (= i 0)
      'done
      (begin
        (test-equal '(3 es-m (11 5))
            (eval-string "(+ 1 2) (define es-m (macro (x) `(list 11 ,x))) (es-m 5)")
            "eval-string sequences forms without losing GC roots")
        (es-stress (- i 1)))))
(es-stress 20)

(gc-threshold **saved-gc-threshold**)

(display "          === Defect regression: env/tail/continuation bugs ===")
(newline)

;; Defect 4 (eval_main forced tail=true, clobbering state.env): an unquoted
;; call must not leave state.env pointed inside the callee for the unquotes
;; that follow it.
(define (defect4-f x) (list x))
(define (defect4-h i) `(a ,(defect4-f i) b ,i))
(test-equal '(a (7) b 7) (defect4-h 7) "quasiquote: unquote does not clobber env for later unquotes")

;; Defect 5 (with_timer_sf never restored kont): the continuation around
;; with-timer must survive, and with-timer's own result is the elapsed time,
;; not the body's value.
(define **wt-side** 0)
(define **wt-result**
    (+ 100 (with-timer (begin (set! **wt-side** (+ **wt-side** 6)) (* 2 3)))))
(test-equal 6 **wt-side** "with-timer still evaluates its body for effect")
(test-true (and (number? **wt-result**) (> **wt-result** 100))
    "with-timer preserves the outer continuation (+ 100 ...)")

(display "          === Behaviour change: vectors are now walked in quasiquote ===")
(newline)

;; Proposal B calls out that vector literals inside a quasiquote template
;; were not walked at all; the lowering adds that support.
(define **vq-x** 5)
(test-equal (vector 1 5 3) `#(1 ,**vq-x** 3) "quasiquote walks into vector literals")
(test-equal (vector 1 2 3 4) `#(1 ,@'(2 3) 4) "quasiquote splices into vector literals")
