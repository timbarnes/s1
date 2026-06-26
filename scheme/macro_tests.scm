(display "          === Testing Macros ===")
(newline)

(define m1 (macro (x) `(list 11 ,x)))
(test-equal '(11 22) (m1 22) "Simple macro")

(define when (macro (p body)
    `(if ,p ,body nil)))
(test-equal 22 (when #t 22) "when with #t")
(test-equal nil (when #f 22) "when with #f")

(define unless (macro (p body)
    `(if (not ,p) ,body nil)))
(test-equal nil (unless #t 22) "unless with #t")
(test-equal 22 (unless #f 22) "when with #f")

(define m1 (macro (x) `(list ,x)))
(test-equal '(42) (m1 42) "macro (m1 42)")

(define m2 (macro (x) `(quote ,x)))
(test-equal 'hello (m2 hello) "macro (m2 hello)")

(define m3 (macro args `(list ,@args)))
(test-equal '(1 2) (m3 1 2) "macro (m3 1 2)")

(define m4 (macro (op . args) `(list ,op ,@args)))
(test-equal (list + 1 2 3) (m4 + 1 2 3) "macro (m4 + 1 2 3)")

(define m5 (macro x `(append ,@x)))
(test-equal '(1 2 3 4) (m5 '(1 2) '(3 4)) "macro (m5 '(1 2) '(3 4))")

(define m6 (macro (test . body)
    `(if ,test (begin ,@body) nil)))
(test-equal 22 (m6 #t 22) "macro (m6 #t 11 22)")
(test-equal nil (m6 #f 22) "macro (m6 #f 22)")

(test-equal '(2 1) (let ((a 1) (b '(2))) (expand `(,@b ,a))) "expand macro expression")
(test-equal '(1 2) (let ((a 1) (b '(2))) (expand `(,a ,@b))) "expand macro expression")

;; --- Additional macro coverage ---

;; Zero-argument macro
(define greet (macro () `(list 'hello 'world)))
(test-equal '(hello world) (greet) "zero-argument macro")

;; Compute at expansion time inside unquote
(define m-precomp (macro (x) `(list ,(+ 100 1) ,x)))
(test-equal '(101 5) (m-precomp 5) "compute inside unquote")

;; Use length of variadic args at expansion time
(define m-count (macro args `(list ,(length args) ,@args)))
(test-equal '(3 a b c) (m-count 'a 'b 'c) "length of args at expansion")

;; Conditional generation: produce a literal symbol
(define m-pick (macro (flag) `(quote ,(if flag 'first 'second))))
(test-equal 'first (m-pick #t) "compile-time conditional generation t")
(test-equal 'second (m-pick #f) "compile-time conditional generation f")

;; Macro generating set! (inc!)
(define inc! (macro (v) `(set! ,v (+ ,v 1))))
(define **inc-counter** 5)
(inc! **inc-counter**)
(test-equal 6 **inc-counter** "inc! generates set!")

;; Macro generating set! (swap!)
(define swap! (macro (a b) `(let ((tmp ,a)) (set! ,a ,b) (set! ,b tmp))))
(define **sp** 1)
(define **sq** 2)
(swap! **sp** **sq**)
(test-equal 2 **sp** "swap! first var")
(test-equal 1 **sq** "swap! second var")

;; Macro generating push! (cons-onto-list)
(define push! (macro (v lst) `(set! ,lst (cons ,v ,lst))))
(define **stk** '())
(push! 1 **stk**)
(push! 2 **stk**)
(push! 3 **stk**)
(test-equal '(3 2 1) **stk** "push! generates set!/cons")

;; Macro generating a lambda
(define thunk (macro (body) `(lambda () ,body)))
(define **t1** (thunk (+ 1 2)))
(test-equal 3 (**t1**) "thunk macro generates lambda")

;; Macro generating let
(define with-x (macro (val body) `(let ((x ,val)) ,body)))
(test-equal 49 (with-x 7 (* x x)) "macro generates let binding")

;; Macro generating define
(define defconst (macro (name val) `(define ,name ,val)))
(defconst **PI** 3.14)
(test-equal 3.14 **PI** "macro generates define")

;; Macro generating cond
(define case3 (macro (a b c)
    `(cond (,a 'first) (,b 'second) (,c 'third) (else 'none))))
(test-equal 'first (case3 #t #f #f) "macro generates cond - first")
(test-equal 'second (case3 #f #t #f) "macro generates cond - second")
(test-equal 'third (case3 #f #f #t) "macro generates cond - third")
(test-equal 'none (case3 #f #f #f) "macro generates cond - else")

;; Macro composition (one macro expands into a call to another macro)
(define dbl (macro (x) `(* 2 ,x)))
(define quad (macro (x) `(dbl (dbl ,x))))
(test-equal 12 (quad 3) "macro composition (quad via dbl)")

;; Nested macro: unless built on top of when (with separate names)
(define when2 (macro (test . body) `(if ,test (begin ,@body) #f)))
(define unless2 (macro (test . body) `(when2 (not ,test) ,@body)))
(test-equal #f (unless2 #t 'no) "macro built on macro (true case)")
(test-equal 'yes (unless2 #f 'yes) "macro built on macro (false case)")

;; while loop macro
(define while (macro (test . body)
    `(let loop () (if ,test (begin ,@body (loop)) nil))))
(define **wc** 0)
(while (< **wc** 3) (set! **wc** (+ **wc** 1)))
(test-equal 3 **wc** "while macro reaches loop bound")

;; for loop macro
(define for (macro (var start end . body)
    `(let loop ((,var ,start))
       (if (< ,var ,end)
           (begin ,@body (loop (+ ,var 1)))
           nil))))
(define **for-total** 0)
(for i 1 5 (set! **for-total** (+ **for-total** i)))
(test-equal 10 **for-total** "for macro sums 1..4")

;; progn alias macro (variadic via rest arg)
(define progn (macro args `(begin ,@args)))
(test-equal 3 (progn 1 2 3) "progn macro returns last")

;; Side effects via begin generation
(define **prog-side** 0)
(progn (set! **prog-side** 1) (set! **prog-side** (+ **prog-side** 10)))
(test-equal 11 **prog-side** "progn macro runs side effects in order")

;; Empty splice
(define m-empty (macro () `(list 1 ,@'() 2)))
(test-equal '(1 2) (m-empty) "empty splice disappears")

;; Macro that quotes its argument (raw symbol to value)
(define quote-it (macro (name) `(quote ,name)))
(test-equal 'foo (quote-it foo) "macro generating quote form")

;; Macro with a vector template
(define mvec (macro (x) `(vector ,x ,x)))
(test-equal (vector 9 9) (mvec 9) "macro with vector template")

;; Macro with a string in template
(define mstr (macro (s) `(string-length ,s)))
(test-equal 5 (mstr "hello") "macro with string argument")

;; Macro redefinition: later definition wins
(define rm (macro (x) `(list 'a ,x)))
(test-equal '(a 1) (rm 1) "macro before redefinition")
(define rm (macro (x) `(list 'b ,x)))
(test-equal '(b 1) (rm 1) "macro after redefinition")

;; Macro that shadows a built-in name inside the expansion
(define m-shadow (macro (x) `(let ((+ -)) (+ ,x 1))))
(test-equal 4 (m-shadow 5) "macro expansion can shadow builtin")

;; Macro that references its parameter multiple times
(define repeat3 (macro (body) `(begin ,body ,body ,body)))
(define **rc** 0)
(repeat3 (set! **rc** (+ **rc** 1)))
(test-equal 3 **rc** "macro references parameter multiple times")

;; Macro mixing several unquoted positions
(define m-build (macro (x y z)
    `(list 'before ,x ,(* 10 0) ,y ,z 'after)))
(test-equal '(before 1 0 2 3 after) (m-build 1 2 3) "macro mixes literals and unquotes")

;; Variadic macro with one fixed param and a rest, splicing rest
(define m-prefix (macro (head . rest) `(cons ,head (list ,@rest))))
(test-equal '(0 1 2 3) (m-prefix 0 1 2 3) "fixed + rest splice")

;; Splice with a single element rest list
(define m-one-rest (macro args `(list 'tag ,@args)))
(test-equal '(tag x) (m-one-rest 'x) "splice single-element rest")
(test-equal '(tag) (m-one-rest) "splice empty rest list")

;; Macro generating quoted data (literal list)
(define m-data (macro (a b) `'(,a ,b)))
(test-equal '(1 2) (m-data 1 2) "macro generates quoted list literal")

(display "          === Testing Quasiquote at top level ===")
(newline)

;; Quasiquote works outside any macro context
(define **qq-a** 10)
(test-equal '(1 10 3) `(1 ,**qq-a** 3) "top-level unquote")
(test-equal '(1 2 3 4) `(1 ,@'(2 3) 4) "top-level splice")
(test-equal '(3 12) `(,(+ 1 2) ,(* 3 4)) "top-level computed unquotes")
(test-equal '(a b c) `(a b c) "top-level qq without unquotes")
(test-equal '(1 2 3) `(,@'(1 2 3)) "top-level all-splice")
(test-equal '(5 6 7 8) (let ((x 5) (y '(6 7))) `(,x ,@y 8)) "qq inside let")
(test-equal '() `() "qq of empty list")

(display "          === Testing defmacro-style macro bodies ===")
(newline)

;; Macro body is ordinary Scheme code, evaluated at expansion time.
;; The returned value is the expansion.

;; if at expansion time selects which template to return
(define m-if (macro (x) (if x `(list ,x) `(list 22))))
(test-equal '(7) (m-if 7) "if at expansion - truthy")
(test-equal '(22) (m-if #f) "if at expansion - falsy")

;; let at expansion time computes a value before quasiquoting
(define m-let (macro (x) (let ((y (+ 1 2))) `(+ ,x ,y))))
(test-equal 13 (m-let 10) "let at expansion time")

;; cond at expansion time dispatches on literal arg
(define m-cond (macro (n)
    (cond ((eq? n 0) `'zero)
          ((eq? n 1) `'one)
          (else `'other))))
(test-equal 'zero (m-cond 0) "cond at expansion - 0")
(test-equal 'one (m-cond 1) "cond at expansion - 1")
(test-equal 'other (m-cond 5) "cond at expansion - other")

;; Direct list construction with cons, no quasiquote
(define m-direct (macro args (cons 'list args)))
(test-equal '(1 2 3) (m-direct 1 2 3) "direct list construction in body")

;; Helper function called at expansion time builds the template
(define (gen-add-tree n)
  (if (eq? n 1) 'x `(+ x ,(gen-add-tree (- n 1)))))
(define triple-add (macro () (gen-add-tree 3)))
(define **ta-x** 10)
(test-equal 30 (let ((x **ta-x**)) (triple-add)) "helper function called at expansion")

(display "          === Testing recursive macros ===")
(newline)

;; my-and: expands to nested ifs
(define my-and (macro args
    (cond ((null? args) #t)
          ((null? (cdr args)) (car args))
          (else `(if ,(car args) (my-and ,@(cdr args)) #f)))))
(test-equal #t (my-and) "my-and (no args)")
(test-equal 3 (my-and 1 2 3) "my-and all truthy returns last")
(test-equal #f (my-and 1 #f 3) "my-and short-circuits on #f")
(test-equal 5 (my-and 5) "my-and single arg")

;; my-or: expands to nested lets so each arg evaluates once
(define my-or (macro args
    (cond ((null? args) #f)
          ((null? (cdr args)) (car args))
          (else `(let ((tmp ,(car args)))
                   (if tmp tmp (my-or ,@(cdr args))))))))
(test-equal #f (my-or) "my-or (no args)")
(test-equal 3 (my-or #f #f 3) "my-or returns first truthy")
(test-equal #f (my-or #f #f #f) "my-or all falsy returns #f")

(display "          === Testing expand debugging form ===")
(newline)

;; expand evaluates its argument, then macro-expands one level.

;; Atom: returned unchanged
(test-equal 42 (expand 42) "expand on atom")

;; Non-macro list: returned unchanged
(test-equal '(+ 1 2) (expand '(+ 1 2)) "expand on non-macro form")

;; Macro call: one level of expansion
(define when4 (macro (p . body) `(if ,p (begin ,@body) nil)))
(test-equal '(if #t (begin 42 43) ()) (expand '(when4 #t 42 43)) "expand on macro call")

;; Recursive macro: one level only
(test-equal '(if 1 (my-and 2 3) #f) (expand '(my-and 1 2 3)) "expand shows one level for recursive macro")
