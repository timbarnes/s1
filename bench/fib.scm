;; Long-running workload for flamegraph: sudo cargo flamegraph --release --bin s1 -o flamegraph.svg -- -q -f bench/fib.scm
(define fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))
(display (fib 31))
(newline)
