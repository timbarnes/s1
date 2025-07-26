;; Dummy core Scheme file for file loading test
;; This file does nothing but should be loaded at startup.
22
(display "Core dummy file loaded\n")

(define expander
  (lambda (x)
    (expand-aux x 0)))

(define expand-aux
  (lambda (x depth)
      ; (display depth) (display ": ")(display x) (newline)
    (if (pair? x)
        (if (and (pair? x) (eq? (car x) 'backquote))
            ;; (backquote ...)
            (expand-aux (cadr x) (+ depth 1))
            (if (and (pair? x) (eq? (car x) 'comma))
                ;; (comma ...)
                (if (= depth 1)
                    (cadr x)
                    (list 'backquote (expand-aux (cadr x) (- depth 1))))
                (if (and (pair? x) (eq? (car x) 'comma-at))
                    ;; (comma-at ...)
                    (if (= depth 1)
                        (error "unquote-splicing not in list context")
                        (list 'comma-at (expand-aux (cadr x) (- depth 1))))
                    (if (and (pair? (car x)) (eq? (car (car x)) 'comma-at))
                        ;; List starting with ,@x
                        (if (= depth 1)
                            (list 'append
                                  (cadr (car x))
                                  (expand-aux (cdr x) depth))
                            (list 'cons
                                  (expand-aux (car x) depth)
                                  (expand-aux (cdr x) depth)))
                        ;; General list
                        (list 'cons
                              (expand-aux (car x) depth)
                              (expand-aux (cdr x) depth))))))
        ;; Not a pair
        (if (and (symbol? x)
                 (not (eq? x 'comma))
                 (not (eq? x 'comma-at))
                 (not (eq? x 'backquote)))
            (if (= depth 1)
                (list 'quote x)
                x)
            x))))

(define define-macro (lambda (name . body)
    (expand `(lambda ,params ,@body))))
