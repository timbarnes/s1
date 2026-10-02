;; syntax_rules_tests.scm: define-syntax, let-syntax, letrec-syntax and
;; syntax-rules, especially hygiene (Docs/hygiene-design.md). Also re-run under
;; gc-threshold 1 by gc_stress_tests.scm.

(display "          === Testing syntax-rules ===")
(newline)

;; --- basics
(define-syntax swap!
  (syntax-rules () ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))
(test-equal '(2 1) (let ((x 1) (y 2)) (swap! x y) (list x y)) "swap!")
(test-equal '(2 1) (let ((tmp 1) (other 2)) (swap! tmp other) (list tmp other))
    "introduced tmp doesn't capture the user's tmp")

(define-syntax my-if
  (syntax-rules () ((_ c t e) (cond (c t) (else e)))))
(test-equal 'yes (let ((else #f) (cond 'shadowed)) (my-if #t 'yes 'no))
    "template keywords resolve where the macro was defined")

(test-equal 'outer
    (let ((x 'outer))
      (let-syntax ((m (syntax-rules () ((m) x))))
        (let ((x 'inner)) (m))))
    "free template identifiers use the definition environment")

(test-equal 'now
    (let-syntax ((when (syntax-rules () ((_ test s1 s2 ...) (if test (begin s1 s2 ...))))))
      (let ((if #t)) (when if (set! if 'now)) if))
    "a use site's local if doesn't affect the template's if")

;; --- recursion and tail calls
;; gc_stress_tests.scm defines **syntax-loop-n** before reloading this file,
;; since a collection per allocation makes long loops far too slow.
(define syntax-loop-n (guard (e (#t 10000)) **syntax-loop-n**))
(define-syntax my-or
  (syntax-rules ()
    ((_) #f)
    ((_ e) e)
    ((_ e1 e2 ...) (let ((temp e1)) (if temp temp (my-or e2 ...))))))
(test-equal 7
    (let ((x #f) (y 7) (temp 8) (let odd?) (if even?)) (my-or x (let temp) (if y) y))
    "recursive macro with shadowed let, if and temp")
(define-syntax count-down
  (syntax-rules () ((_ n) (let lp ((i n)) (if (> i 0) (lp (- i 1)) 'done)))))
(test-equal 'done (count-down syntax-loop-n) "a long loop built by a macro")
(define (tail-through-macro n) (my-or #f (if (= n 0) 'bottom (tail-through-macro (- n 1)))))
(test-equal 'bottom (tail-through-macro syntax-loop-n) "a macro use in tail position stays a tail call")

(test-equal '(#t #f)
    (letrec-syntax ((ev? (syntax-rules () ((_ n) (if (= n 0) #t (od? (- n 1))))))
                    (od? (syntax-rules () ((_ n) (if (= n 0) #f (#t-or-ev? n)))))
                    (#t-or-ev? (syntax-rules () ((_ n) (not (= (modulo n 2) 1))))))
      (list (ev? 4) (od? 4)))
    "letrec-syntax transformers can refer to each other")

;; --- patterns
(define-syntax part-2
  (syntax-rules ()
    ((_ a b (m n) ... x y) (vector (list a b) (list m ...) (list n ...) (list x y)))
    ((_ . rest) 'error)))
(test-equal '#((10 43) (31 41 51) (32 42 52) (63 77))
    (part-2 10 (+ 21 22) (31 32) (41 42) (51 52) (+ 61 2) 77)
    "ellipsis in the middle of a list")
(define-syntax tail-of
  (syntax-rules () ((_ (a x ... . r)) '(a (x ...) r))))
(test-equal '(1 (2 3) 4) (tail-of (1 2 3 . 4)) "dotted tail after an ellipsis")
(test-equal '(1 (2 3) ()) (tail-of (1 2 3)) "dotted tail matching ()")
(define-syntax vec-rest (syntax-rules () ((_ #(a b ...)) '(a (b ...)))))
(test-equal '(1 (2 3)) (vec-rest #(1 2 3)) "vector pattern")
(define-syntax flatten (syntax-rules () ((_ (a b ...) ...) '(b ... ...))))
(test-equal '(2 3 5) (flatten (1 2 3) (4 5)) "nested ellipsis flattened")
(define-syntax count-to-2
  (syntax-rules () ((_) 0) ((_ _) 1) ((_ _ _) 2) ((_ . _) 'many)))
(test-equal '(2 0 many) (list (count-to-2 a b) (count-to-2) (count-to-2 a b c d)) "underscore wildcard")
(define-syntax arrow-or-not (syntax-rules (=>) ((_ a => b) 'arrow) ((_ a b c) 'plain)))
(test-equal 'arrow (arrow-or-not 1 => 2) "literal matches")
(test-equal 'plain (let ((=> 0)) (arrow-or-not 1 => 2)) "literal bound at the use site doesn't match")
(test-equal 'ok (let ((=> #f)) (cond (#t => 'ok))) "cond's => is a variable when bound")

;; --- ellipsis escapes and custom ellipses
(define-syntax elli-esc
  (syntax-rules () ((_) '(... ...)) ((_ x) '(... (x ...)))))
(test-equal '... (elli-esc) "(... ...) is a literal ellipsis")
(test-equal '(100 ...) (elli-esc 100) "(... template) escapes the whole template")
(define-syntax custom-dots (syntax-rules dots () ((_ x dots) (list x dots))))
(test-equal '(1 2 3) (custom-dots 1 2 3) "custom ellipsis identifier")
(define-syntax lit-dots (syntax-rules ... (...) ((_ x) '(x ...))))
(test-equal '(100 ...) (lit-dots 100) "a literal takes priority over the ellipsis")

;; --- macro-defining macros
(define-syntax be-like-begin
  (syntax-rules ()
    ((_ name) (define-syntax name (syntax-rules () ((name expr (... ...)) (begin expr (... ...))))))))
(be-like-begin sequence)
(test-equal 3 (sequence 0 1 2 3) "macro-defining macro with an escaped ellipsis")
(define-syntax be-like-begin-dots
  (syntax-rules ()
    ((_ name) (define-syntax name (syntax-rules dots () ((name expr dots) (begin expr dots)))))))
(be-like-begin-dots sequence-dots)
(test-equal 5 (sequence-dots 2 3 4 5) "macro-defining macro with a custom ellipsis")
(define-syntax jabberwocky
  (syntax-rules ()
    ((_ hatter) (begin (define march-hare 42)
                       (define-syntax hatter (syntax-rules () ((_) march-hare)))))))
(jabberwocky mad-hatter)
(test-equal 42 (mad-hatter) "an introduced definition is visible to the macro it defines")
(test-equal 'hidden (guard (e (#t 'hidden)) march-hare) "an introduced top-level definition is hidden")

;; --- definitions and scope
(test-equal '(2 1)
    (let ((x 1) (y 2))
      (define-syntax local-swap! (syntax-rules () ((_ a b) (let ((t a)) (set! a b) (set! b t)))))
      (local-swap! x y)
      (list x y))
    "internal define-syntax")
(test-equal 1 (let () (define x 1) (let-syntax () (define x 2) #f) x) "let-syntax body is a new scope")
(test-equal 42
    (let ()
      (define-syntax fwd (syntax-rules () ((_) (later))))
      (define (use) (fwd))
      (define (later) 42)
      (use))
    "forward reference from a macro")
(define counter 0)
(define-syntax bump! (syntax-rules () ((_) (set! counter (+ counter 1)))))
(let ((counter 'shadowed)) (bump!) (bump!))
(test-equal 2 counter "set! through an alias assigns the definition environment's variable")

;; --- quoted data in templates
(define-syntax quoted (syntax-rules () ((_ x) '(a x #(b c)))))
(test-equal '(a 1 #(b c)) (quoted 1) "quoted template data is plain symbols")
(test-equal #t (eq? (car (quoted 1)) 'a) "quoted template symbols are eq? to the user's")
(define-syntax qq (syntax-rules () ((_ e) `(tag ,e ,(list 'x e)))))
(test-equal '(tag 5 (x 5)) (qq 5) "quasiquote in a template")

;; --- errors
(test-equal "no syntax rule matches (swap! 1)"
    (guard (e (#t (error-object-message e))) (swap! 1))
    "a use that matches no rule raises an error")
(test-equal 'bad
    (guard (e (#t 'bad)) (define-syntax broken (syntax-rules () ((_ ... x) 1))))
    "a malformed syntax-rules raises an error")

;; --- coexistence with s1's non-hygienic macro form
(define old-style (macro (x) `(list ,x ,x)))
(define-syntax new-style (syntax-rules () ((_ x) (old-style (+ x 1)))))
(test-equal '(3 3) (new-style 2) "a syntax-rules template can use a macro")
(define old-wraps-new (macro (x) `(new-style ,x)))
(test-equal '(6 6) (old-wraps-new 5) "a macro expansion can use a syntax-rules macro")

;; --- the transformer survives collection between definition and use
(define-syntax survives-gc (syntax-rules () ((_ x) (let ((v x)) (list v v)))))
(gc)
(test-equal '(9 9) (survives-gc 9) "a transformer and its environment survive a collection")

;; --- special forms that rewrite themselves use the global core forms, so
;; local bindings of core names can't break them (and do's loop can't
;; capture a user variable)
(test-equal 1 (let ((lambda #f)) (define (f) 1) (f)) "internal define with a local lambda")
(test-equal 2 (let ((lambda #f)) (define (g x) x) (g 2)) "procedure define with a local lambda")
(test-equal 3 (let ((letrec #f)) (let lp ((i 0)) (if (< i 3) (lp (+ i 1)) i))) "named let with a local letrec")
(test-equal 3 (let ((lambda #f)) (let lp ((i 0)) (if (< i 3) (lp (+ i 1)) i))) "named let with a local lambda")
(test-equal '(mine mine)
    (let ((loop 'mine)) (do ((i 0 (+ i 1)) (acc '() (cons loop acc))) ((= i 2) acc)))
    "do doesn't capture a user variable named loop")
(test-equal 'done (let ((if #f)) (do ((i 0 (+ i 1))) ((= i 2) 'done))) "do with a local if")
(test-equal 'caught (let ((lambda #f)) (guard (e (#t 'caught)) (raise 'x))) "guard with a local lambda")
(test-equal 'caught (let ((cond #f)) (guard (e (#t 'caught)) (raise 'x))) "guard with a local cond")
(test-equal 3 (let ((let 1)) (let* ((a 1) (b 2)) (+ a b))) "let* with a local let")
(test-equal 1 (let ((set! #f)) (letrec ((f (lambda () 1))) (f))) "letrec with a local set!")
(test-equal '(x y) (let ((quote list)) `(x y)) "quasiquote with a local quote")

;; --- expansion cache: a use is expanded once per transformer
(define-syntax which-version (syntax-rules () ((_) 'first)))
(define (call-which) (which-version))
(test-equal 'first (call-which) "cached expansion of a use")
(test-equal 'first (call-which) "the same use again")
(define-syntax which-version (syntax-rules () ((_) 'second)))
(test-equal 'second (call-which) "redefining the macro invalidates the cached expansion")
