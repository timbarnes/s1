;; Dummy core Scheme file for file loading test
;; This file does nothing but should be loaded at startup.

(define macro-expand
  (lambda (x)
    (expand-aux x 0)))

(define expand-aux
  (lambda (expr depth)
    (cond
      ;; (comma x) at current backquote depth: evaluate x
      ((and (pair? expr)
            (eq? (car expr) 'unquote)
            (= depth 1))
       (eval (cadr expr)))

      ;; (comma x) nested deeper: leave it as-is, decrease depth
      ((and (pair? expr)
            (eq? (car expr) 'unquote))
       (list 'unquote (expand-aux (cadr expr) (- depth 1))))

      ;; Case 3: (comma-at ...)
      ((and (pair? x) (eq? (car x) 'comma-at))
       (if (= depth 0)
           (error "unquote-splicing not in list context")
           (list 'unquote-splicing (expand-aux (cadr x) (- depth 1)))))

      ;; Case 4: beginning of list is ,@ (splicing)
      ((and (pair? x)
            (pair? (car x))
            (eq? (car (car x)) 'unquote-splicing))
       (if (= depth 1)
           (list 'append
                 (cadr (car x))
                 (expand-aux (cdr x) depth))
           (list 'cons
                 (expand-aux (car x) depth)
                 (expand-aux (cdr x) depth))))

      ;; (backquote x): increase depth, expand inner
      ((and (pair? expr)
            (eq? (car expr) 'quasiquote))
       (if (= depth 0)
           (expand-aux (cadr expr) (+ depth 1))                           ;; fully unwrapped
           (list 'quasiquote (expand-aux (cadr expr) (+ depth 1)))))       ;; nested quoting

      ;; Pairs: recursively expand car and cdr
      ((pair? expr)
       (cons (expand-aux (car expr) depth)
             (expand-aux (cdr expr) depth)))

      ;; Atoms: leave as-is
      (else expr))))



(define foo (macro (x) `(list 'bar ,x)))

(display "Expander file loaded\n")
