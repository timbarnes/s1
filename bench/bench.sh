#!/usr/bin/env bash
# Performance benchmark for s1. Usage: bench/bench.sh [reps]
# Reports median wall-clock for the regression suite plus three micro workloads.
# ponytail: shell + /usr/bin/time, no criterion. Add a real harness when we
# need per-function numbers rather than whole-program wall clock.
set -uo pipefail
cd "$(dirname "$0")/.."
REPS=${1:-5}
BIN=target/release/s1
W=$(mktemp -d)
trap 'rm -rf "$W"' EXIT

cargo build --release >/dev/null 2>&1 || { echo "build failed"; exit 1; }

median() { sort -n | awk '{a[NR]=$1} END{if(NR==0){print "n/a";exit} print (NR%2)?a[(NR+1)/2]:(a[NR/2]+a[NR/2+1])/2}'; }

run() { # name, cmd...
  local name=$1; shift
  for _ in $(seq "$REPS"); do
    /usr/bin/time -p "$@" >/dev/null 2>"$W/t" </dev/null
    awk '/^real/{print $2}' "$W/t"
  done | median | xargs printf '%-30s %ss\n' "$name"
}

# 1x: the suite as shipped (`cargo run -- -r`). Dominated by process start + core load.
run "regression 1x" $BIN -r -q

# In-process repetition. Exposes per-top-level-form state that is never reset
# between forms (env chain depth, heap growth). Should scale LINEARLY with n;
# if 20x is more than ~4x the 5x time, something is accumulating.
for n in 5 20; do
  for _ in $(seq "$n"); do echo '(load "scheme/regression.scm")'; done > "$W/reg$n.scm"
  run "regression ${n}x in-proc" $BIN -f "$W/reg$n.scm" -q
done

cat > "$W/fib.scm" <<'EOF'
(define fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))
(display (fib 25)) (newline)
EOF
run "fib 25 (call + arith)" $BIN -f "$W/fib.scm" -q

# NOTE: (map f (build ...)) currently clobbers the caller's env
# ("Unbound variable"), so the list is built once at top level.
cat > "$W/list.scm" <<'EOF'
(define build (lambda (n acc) (if (= n 0) acc (build (- n 1) (cons n acc)))))
(define data (build 200 '()))
(define dbl (lambda (x) (* x 2)))
(define lp (lambda (i) (if (= i 0) 'done (begin (map dbl data) (lp (- i 1))))))
(display (lp 300)) (newline)
EOF
run "list/map (alloc churn)" $BIN -f "$W/list.scm" -q

cat > "$W/deep.scm" <<'EOF'
(define count (lambda (n) (if (= n 0) 'done (count (- n 1)))))
(display (count 300000)) (newline)
EOF
run "tail loop 300k (dispatch)" $BIN -f "$W/deep.scm" -q
