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
            (eq? (car expr) 'comma)
            (= depth 1))
       (eval (cadr expr)))

      ;; (comma x) nested deeper: leave it as-is, decrease depth
      ((and (pair? expr)
            (eq? (car expr) 'comma))
       (list 'comma (expand-aux (cadr expr) (- depth 1))))

      ;; (backquote x): increase depth, expand inner
      ((and (pair? expr)
            (eq? (car expr) 'backquote))
       (if (= depth 0)
           (expand-aux (cadr expr) (+ depth 1))                           ;; fully unwrapped
           (list 'backquote (expand-aux (cadr expr) (+ depth 1)))))       ;; nested quoting

      ;; Pairs: recursively expand car and cdr
      ((pair? expr)
       (cons (expand-aux (car expr) depth)
             (expand-aux (cdr expr) depth)))

      ;; Atoms: leave as-is
      (else expr))))



(define foo (macro (x) `(list 'bar ,x)))

(display "Expander file loaded\n")
