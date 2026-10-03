//! R7RS 6.14 procedures that end or depend on the process (exit,
//! emergency-exit), tested by running the s1 binary.

use std::path::PathBuf;
use std::process::{Command, Output, Stdio};

/// Write `source` to a temporary file named after `name`, and return its path.
fn script(name: &str, source: &str) -> PathBuf {
    let path = std::env::temp_dir().join(format!("s1-process-test-{}-{}.scm", std::process::id(), name));
    std::fs::write(&path, source).unwrap();
    path
}

/// Run s1 with `args`, from the package root (s1 loads scheme/s1-core.scm
/// relative to the current directory), with standard input closed.
fn s1(args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_s1"))
        .args(args)
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .stdin(Stdio::null())
        .output()
        .unwrap()
}

/// Load `source` as a file with -f, then quit: the exit status and stdout,
/// less s1's two startup lines.
fn run_file(name: &str, source: &str) -> (i32, String) {
    let path = script(name, source);
    let out = s1(&["-q", "-f", path.to_str().unwrap()]);
    std::fs::remove_file(&path).ok();
    let stdout = String::from_utf8_lossy(&out.stdout);
    let body: Vec<&str> = stdout.lines().skip(2).collect();
    (out.status.code().unwrap(), body.join("\n"))
}

#[test]
fn exit_statuses() {
    assert_eq!(run_file("exit-none", "(exit)").0, 0);
    assert_eq!(run_file("exit-true", "(exit #t)").0, 0);
    assert_eq!(run_file("exit-false", "(exit #f)").0, 1);
    assert_eq!(run_file("exit-int", "(exit 7)").0, 7);
    assert_eq!(run_file("emergency-int", "(emergency-exit 4)").0, 4);
    assert_eq!(run_file("emergency-false", "(emergency-exit #f)").0, 1);
}

#[test]
fn exit_stops_the_program() {
    let (code, out) = run_file("exit-stops", "(display \"a\") (exit 2) (display \"b\")");
    assert_eq!((code, out.as_str()), (2, "a"));
}

#[test]
fn exit_runs_after_thunks_innermost_first() {
    let (code, out) = run_file(
        "exit-winds",
        r#"(dynamic-wind
             (lambda () (display "in "))
             (lambda ()
               (dynamic-wind (lambda () #f)
                             (lambda () (exit 3))
                             (lambda () (display "inner "))))
             (lambda () (display "outer")))"#,
    );
    assert_eq!((code, out.as_str()), (3, "in inner outer"));
}

#[test]
fn emergency_exit_skips_after_thunks() {
    let (code, out) = run_file(
        "emergency-winds",
        r#"(display "start ")
           (dynamic-wind (lambda () #f)
                         (lambda () (emergency-exit 5))
                         (lambda () (display "after")))"#,
    );
    assert_eq!((code, out.as_str()), (5, "start "));
}

#[test]
fn exit_rejects_other_values() {
    let (code, out) = run_file(
        "exit-bad",
        r#"(display (guard (e (#t (error-object-message e))) (exit 'x)))"#,
    );
    assert_eq!((code, out.as_str()), (0, "exit: expected a boolean or an exact integer"));
}
