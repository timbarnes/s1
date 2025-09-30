((lambda (n f x)
   (n f x))
 (lambda (f x)
   (let loop ((i 0) (g x))
     (if (< i 100000) (loop (+ i 1) (f g)) g)))
 (lambda (y) y) 0)

;; simple loop to stress eval and env lookup
(let loop ((i 0) (acc 0))
  (if (< i 1000000)
      (loop (+ i 1) (+ acc i))
      acc))

(define (deep n)
  (if (zero? n) 1 (+ 1 (deep (- n 1)))))
(deep 200000)

(exit)
