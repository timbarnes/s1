//! The tracer and stepper (src/debugger.rs) and `trace-procedure`, tested
//! by running the s1 binary with debugger commands on standard input.

use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};

/// Write `source` to a temporary file named after `name`, and return its path.
fn script(name: &str, source: &str) -> PathBuf {
    let path = std::env::temp_dir().join(format!("s1-debugger-test-{}-{}.scm", std::process::id(), name));
    std::fs::write(&path, source).unwrap();
    path
}

/// Load `source` with -f and quit, with `input` as standard input: the
/// exit status, stdout less s1's two startup lines, and stderr.
fn run(name: &str, source: &str, input: &str) -> (i32, String, String) {
    let path = script(name, source);
    let mut child = Command::new(env!("CARGO_BIN_EXE_s1"))
        .args(["-q", "-f", path.to_str().unwrap()])
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    child.stdin.take().unwrap().write_all(input.as_bytes()).unwrap();
    let out = child.wait_with_output().unwrap();
    std::fs::remove_file(&path).ok();
    let stdout = String::from_utf8_lossy(&out.stdout);
    let body: Vec<&str> = stdout.lines().skip(2).collect();
    (out.status.code().unwrap(), body.join("\n"), String::from_utf8_lossy(&out.stderr).into_owned())
}

const PROGRAM: &str = r#"
(define (sq x) (* x x))
(define (f x) (let ((y (+ x 1))) (break 'in-f y) (+ (sq y) (sq x))))
(display (f 3)) (newline)
(display "after") (newline)
"#;

#[test]
fn break_stops_and_continue_runs_on() {
    let (code, out, err) = run("break", PROGRAM, "c\n");
    assert_eq!((code, out.as_str()), (0, "25\nafter"));
    assert!(err.contains("break in-f 4"), "{err}");
    assert!(err.contains("debug> "), "{err}");
}

#[test]
fn print_evaluates_in_the_paused_environment() {
    let (_, out, err) = run("print", PROGRAM, "p (list x y (sq y))\nc\n");
    assert_eq!(out, "25\nafter");
    assert!(err.contains("(3 4 16)"), "{err}");
}

#[test]
fn an_error_in_print_returns_to_the_prompt() {
    let (_, out, err) = run("print-error", PROGRAM, "p (car 5)\np y\nc\n");
    assert_eq!(out, "25\nafter");
    assert!(err.contains("p: Error: car"), "{err}");
    assert!(err.lines().any(|l| l.ends_with("debug> 4")), "{err}");
}

#[test]
fn backtrace_and_frame_selection() {
    let source = r#"
(define (inner z) (break) (* z 2))
(define (outer a) (+ 1 (inner (+ a 10))))
(display (outer 5))
"#;
    // Two frames up from the break (past the hidden return from inner) is
    // the call (+ 1 (inner ...)), in outer's environment.
    let (_, out, err) = run("bt", source, "bt\nup 2\np (list a)\nd 2\np z\nc\n");
    assert_eq!(out, "31");
    assert!(err.contains("in call  (+ 1 (inner (+ a 10)))"), "{err}");
    // Returns still waiting for their values are left out.
    assert!(!err.contains("-- return"), "{err}");
    assert!(err.lines().any(|l| l.ends_with("debug> (5)")), "{err}");
    assert!(err.lines().any(|l| l.ends_with("debug> 15")), "{err}");
}

#[test]
fn step_over_and_finish() {
    // After the break: n to the body's next form, n into (sq y), o over it,
    // then f to the end of f's body.
    let (_, out, err) = run("over", PROGRAM, "n\nn\no\nf\nc\n");
    assert_eq!(out, "25\nafter");
    assert!(err.contains("Expr:  (sq y)"), "{err}");
    assert!(err.contains("Value: 16"), "{err}");
    assert!(err.contains("Value: 25"), "{err}");
}

#[test]
fn backtrace_shows_the_value_being_returned() {
    // Two steps after the break, (* z 2) has its value, 30, on its way to
    // the frame that returns it from inner.
    let source = r#"
(define (inner z) (break) (* z 2))
(define (outer a) (+ 1 (inner (+ a 10))))
(display (outer 5))
"#;
    let (_, _, err) = run("bt-value", source, "n\nn\nbt\nc\n");
    assert!(err.contains("-- return value = 30 --  [z=15]"), "{err}");
    assert!(err.contains("in call  (+ 1 (inner (+ a 10)))  [a=5]"), "{err}");
}

#[test]
fn quit_abandons_the_form() {
    // The (newline) after (display (f 3)) is a form of its own, so it runs.
    let (code, out, _) = run("quit", PROGRAM, "q\n");
    assert_eq!((code, out.as_str()), (0, "\nafter"));
}

#[test]
fn end_of_input_while_stepping_runs_on() {
    let (code, out, _) = run("eof", "(trace 'step) (display (+ 1 2))", "");
    assert_eq!((code, out.as_str()), (0, "3"));
}

#[test]
fn post_mortem_then_the_form_unwinds() {
    let source = r#"
(trace 'off)
(define (g x)
  (dynamic-wind (lambda () #f)
                (lambda () (car x))
                (lambda () (display "after-thunk") (newline))))
(g 5)
(display (trace)) (newline)
"#;
    let (code, out, err) = run("post-mortem", source, "l\nc\n");
    assert_eq!((code, out.as_str()), (0, "after-thunk\noff"));
    assert!(err.contains("Error: car"), "{err}");
    assert!(err.contains("Entering the debugger"), "{err}");
    // The next form is not stepped.
    assert_eq!(err.matches("debug> ").count(), 2, "{err}");
}

#[test]
fn reset_reports_errors_without_the_prompt() {
    let (_, _, err) = run("reset", "(car 5)", "");
    assert!(err.contains("Error: car"), "{err}");
    assert!(!err.contains("debug>"), "{err}");
}

#[test]
fn trace_modes() {
    let source = r#"
(display (list (trace) (trace 'off) (trace) (trace 'reset) (trace)))
(define (sq x) (* x x))
(trace 'expr)
(+ 1 (sq 2))
(trace 'reset)
"#;
    let (_, out, err) = run("modes", source, "");
    assert_eq!(out, "(reset off off reset reset)");
    assert!(err.contains("Expr:  (* x x)"), "{err}");
    // sq's value is marked as a return, with the call's bindings, and the
    // form's value, which reaches the top level without a step, is shown.
    assert!(err.contains("Value: 4   <- return [x=2]"), "{err}");
    assert!(err.contains("Result: 5"), "{err}");
}

#[test]
fn trace_all_shows_a_frame_when_it_changes() {
    let source = r#"
(define (fact n) (if (zero? n) 1 (* (fact (- n 1)) n)))
(trace 'all)
(fact 2)
(trace 'reset)
"#;
    let (_, _, err) = run("trace-all", source, "");
    // The pending (* ...) call, with its level's n: n=2 on the way down and
    // again when n=1's value comes back to it; n=1 only on the way down,
    // since nothing changes between its frame and the return to it.
    assert_eq!(err.matches("| in call  (* (fact (- n 1)) n)  [n=2]").count(), 2, "{err}");
    assert_eq!(err.matches("| in call  (* (fact (- n 1)) n)  [n=1]").count(), 1, "{err}");
    assert!(!err.contains("top level"), "{err}");
    assert!(err.contains("Value: 1   <- return [n=0]"), "{err}");
    assert!(err.contains("Result: 2"), "{err}");
}

#[test]
fn trace_procedure_shows_calls_and_results() {
    let source = r#"
(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(define original fib)
(trace-procedure fib)
(display (fib 2))
(untrace-procedure fib)
(display (eq? fib original))
(fib 5)
"#;
    let (_, out, err) = run("trace-procedure", source, "");
    assert_eq!(out, "1#t");
    assert_eq!(err, "> (fib 2)\n| > (fib 1)\n| < 1\n| > (fib 0)\n| < 0\n< 1\n");
}
