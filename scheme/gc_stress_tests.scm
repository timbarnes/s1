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
;;
;; Also covers Docs/gc-tail-loop.md: a purely tail-recursive loop never
;; pushed a RestoreEnv frame, which used to be the only place automatic GC
;; was checked, so such a loop never triggered a collection no matter how
;; much garbage it produced.
;;
;; Also covers a gap identified while designing Docs/kont-flat-stack-design.md
;; (F10(3), replacing the Rc<Kont> chain with a flat stack, not yet
;; implemented): advanced_tests.scm has substantial call/cc and dynamic-wind
;; coverage, but none of it used to run under GC pressure, unlike the macro
;; suite below. That mattered specifically because F10(3)'s capture/invoke
;; mechanism is exactly the class of GC-rooting-sensitive code that has
;; caused real bugs here before (see nested-evaluation.md) - so it gets the
;; same treatment now, plus a deep (500-frame) non-tail capture case that
;; nothing previously exercised.

(display "          === GC stress: macro/quasiquote under gc-threshold 1 ===")
(newline)

(define **saved-gc-threshold** (gc-threshold))
(gc-threshold 1)

;; Defect 1 (eval_macro's unrooted saved_kont/saved_env): re-run the macro
;; suite -- every macro call goes through the same expansion path -- under
;; a threshold low enough to guarantee a collection mid-expansion.
(load "scheme/macro_tests.scm")

;; F10(3) prep: re-run the call/cc and dynamic-wind suite the same way.
;; Its "Re-entrant dynamic-wind" test in particular captures a continuation
;; inside a dynamic-wind thunk and invokes it long after that whole
;; top-level form (and others) already ran -- precisely the shape a flat,
;; reused Kont stack has to get right when it replaces today's persistent
;; Rc chain. See Docs/kont-flat-stack-design.md.
(load "scheme/advanced_tests.scm")

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

;; F10(3) prep: every call/cc capture above happens 1-3 frames deep, or a
;; couple of dynamic-wind levels deep. None captures from far down a long
;; non-tail call chain -- the shape most likely to expose a snapshot-copy
;; bug (wrong order, truncation, off-by-one) in a flat-stack capture/invoke
;; implementation, as opposed to today's O(1) Rc::clone share. See
;; Docs/kont-flat-stack-design.md.
;;
;; deep-capture recurses non-tail, so at the point call/cc fires (n = 0),
;; none of the 500 pending "increment then return" frames above it have run
;; yet -- they are the captured continuation, not already-finished work.
;; Invoking deep-k later must therefore replay all 500 of them again, in
;; full and in order, landing back on the original `first-result` binding
;; (this evaluator's continuations are escape-only: invoking one resumes at
;; its capture site, not at the new call site -- see the "re-entrant" test
;; above and Docs/kont-flat-stack-design.md).
(define deep-depth 500)
(define deep-exit-count 0)
(define deep-k #f)

(define (deep-capture n)
  (if (= n 0)
      (call/cc (lambda (k) (set! deep-k k) 'base-value))
      (let ((r (deep-capture (- n 1))))
        (set! deep-exit-count (+ deep-exit-count 1))
        r)))

(define deep-first-result (deep-capture deep-depth))
(test-equal 'base-value deep-first-result "deep capture: initial unwind returns base value")
(test-equal deep-depth deep-exit-count
    "deep capture: initial unwind runs every one of the 500 pending frames exactly once")

;; Unrelated work between capture and re-invocation, so nothing from the
;; original chain is "current" by coincidence.
(define (deep-noise n) (if (= n 0) 'done (deep-noise (- n 1))))
(deep-noise 1000)

(deep-k 'escaped-value)
(test-equal 'escaped-value deep-first-result
    "deep capture: re-invocation replays the captured chain back to the original binding")
(test-equal (* 2 deep-depth) deep-exit-count
    "deep capture: re-invocation replays all 500 pending frames exactly once more")

;; And once more, to check this isn't a two-shots-then-corrupts fluke.
(deep-k 'escaped-again)
(test-equal 'escaped-again deep-first-result
    "deep capture: third invocation still reaches the original binding")
(test-equal (* 3 deep-depth) deep-exit-count
    "deep capture: third invocation replays all 500 pending frames exactly once more")

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

(display "          === GC stress: tail-recursive loop under gc pressure ===")
(newline)

;; A tail-recursive loop's self-calls take the TCO path in apply_proc, which
;; never pushes Kont::RestoreEnv - previously the only place needs_gc() was
;; checked. Running one with garbage-producing steps at a threshold low
;; enough to force many collections mid-loop checks two things at once: that
;; collections actually happen during the loop rather than only after it
;; returns, and that collecting from the tail-call checkpoint is safe (an
;; unsafe collection point would corrupt state or crash, not just quietly
;; return the wrong number).
(define **tail-gc-threshold** (gc-threshold))
(gc-threshold 500)

(define (tail-spin n acc)
  (if (= n 0)
      acc
      (begin
        (cons n n)          ;; garbage: never referenced again
        (list "garbage" n)  ;; more garbage, a different shape
        (tail-spin (- n 1) (+ acc 1)))))

(test-equal 20000 (tail-spin 20000 0)
    "tail-recursive loop produces the correct result across many collections mid-loop")

(gc-threshold **tail-gc-threshold**)
