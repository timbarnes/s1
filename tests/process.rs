//! R7RS 6.14 procedures that end or depend on the process (exit,
//! emergency-exit, command-line), and running scripts, tested by running
//! the s1 binary.

use std::path::PathBuf;
use std::process::{Command, Output, Stdio};

/// Write `source` to a temporary file named after `name`, and return its path.
fn script(name: &str, source: &str) -> PathBuf {
    let path = std::env::temp_dir().join(format!("s1-process-test-{}-{}.scm", std::process::id(), name));
    std::fs::write(&path, source).unwrap();
    path
}

/// Run s1 with `args`, from the package root, with standard input closed.
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

/// Run `source` as a script, `s1 script args...`: the exit status, stdout
/// and stderr. Scripts get no startup banner.
fn run_script(name: &str, source: &str, args: &[&str]) -> (i32, String, String, String) {
    let path = script(name, source);
    let path_str = path.to_str().unwrap().to_string();
    let mut all = vec![path_str.as_str()];
    all.extend_from_slice(args);
    let out = s1(&all);
    std::fs::remove_file(&path).ok();
    (
        out.status.code().unwrap(),
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        path_str,
    )
}

#[test]
fn command_line_without_a_script() {
    let (code, out) = run_file("cl-none", "(write (command-line))");
    assert_eq!((code, out.as_str()), (0, "(\"s1\")"));
}

#[test]
fn script_gets_its_arguments() {
    // Everything after the script goes to it, options included.
    let (code, out, _, path) =
        run_script("cl-args", "(write (command-line))", &["a", "-q", "--x", "two words"]);
    assert_eq!(code, 0);
    assert_eq!(out, format!("(\"{}\" \"a\" \"-q\" \"--x\" \"two words\")", path));
}

#[test]
fn script_runs_after_files_then_exits() {
    let lib = script("cl-lib", "(define from-file 'loaded)");
    let main = script("cl-main", "(write from-file)");
    let out = s1(&["-f", lib.to_str().unwrap(), main.to_str().unwrap()]);
    std::fs::remove_file(&lib).ok();
    std::fs::remove_file(&main).ok();
    assert_eq!(out.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.ends_with("loaded"), "{:?}", stdout);
    assert!(!stdout.contains("s1>"), "a script doesn't start the REPL: {:?}", stdout);
}

#[test]
fn script_skips_a_shebang_line() {
    let (code, out, _, _) = run_script("shebang", "#!/usr/bin/env s1\n(display \"ran\")\n", &[]);
    assert_eq!((code, out.as_str()), (0, "ran"));
}

#[test]
fn uncaught_error_ends_a_script_with_status_70() {
    let (code, out, err, _) = run_script(
        "uncaught",
        r#"(dynamic-wind (lambda () #f)
                         (lambda () (car 1))
                         (lambda () (display "after")))
           (display "never")"#,
        &[],
    );
    assert_eq!((code, out.as_str()), (70, "after"));
    assert!(err.contains("car"), "{:?}", err);
}

#[test]
fn syntax_error_ends_a_script_with_status_70() {
    let (code, out, _, _) = run_script("syntax", "(display \"x\")\n#bogus\n(display \"never\")", &[]);
    assert_eq!(code, 70);
    assert!(out.starts_with("x") && !out.contains("never"), "{:?}", out);
}

#[test]
fn script_exit_status() {
    assert_eq!(run_script("script-exit", "(exit 9)", &[]).0, 9);
    assert_eq!(run_script("script-ok", "(display 1)", &[]).0, 0);
}
