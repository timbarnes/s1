use crate::eval::EvalContext;
use crate::gc::{GcHeap, GcRef, SchemeValue, new_port};
use crate::io::{PortKind, port_kind_from_scheme_port};
use crate::parser::{ParseError, parse};

/// (read [port])
/// Reads an s-expression from a port. Port defaults to the current input port (normally stdin).
fn read_builtin(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() > 1 {
        return Err("read: expected at most 1 argument".to_string());
    }
    let mut port_kind = ec.port_stack.last_mut().unwrap().clone();
    if args.len() == 1 {
        match args.get(0) {
            Some(port) => port_kind = port_kind_from_scheme_port(ec, *port),
            None => {
                return Err("read: invalid port argument".to_string());
            }
        }
    }

    match port_kind {
        PortKind::Stdin | PortKind::StringPortInput { .. } /* | PortKind::File { .. } */ => {
            let result = parse(ec.heap, &mut port_kind);
            match result {
                Ok(expr) => Ok(expr),
                Err(err) => match err {
                    ParseError::Eof => Ok(ec.heap.eof()),
                    ParseError::Syntax(err) => Err(format!("read: syntax error:{}", err)),
                    //ParseError::Other(err) => Err(format!("read: other error:{}", err)),
                },
            }
        }
        _ => Err("read: port must be a string port or file port".to_string()),
    }
}

/// (eof-object? obj) -> #t or #f
/// Tests the result of a read operation to determine if it reached the end of the file.
fn eof_object_q(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("eof-object: expected exactly 0 arguments".to_string());
    }
    let result;
    if crate::gc::eq(&ec.heap, ec.heap.eof(), args[0]) {
        result = ec.heap.true_s();
    } else {
        result = ec.heap.false_s();
    }
    Ok(result)
}

/// (open-input-file filename) -> port
fn open_input_file(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("open-input-file: expected exactly 1 argument".to_string());
    }
    let filename = match &ec.heap.get_value(args[0]) {
        SchemeValue::Str(s) => s,
        _ => return Err("open-input-file: argument must be a string".to_string()),
    };
    // Try to open the file
    match std::fs::File::open(filename) {
        Ok(file) => {
            // Read the file into a string (for now, use StringPortInput for simplicity)
            use std::io::Read;
            let mut content = String::new();
            let mut reader = std::io::BufReader::new(file);
            if let Err(e) = reader.read_to_string(&mut content) {
                return Err(format!(
                    "open-input-file: could not read file '{}': {}",
                    filename, e
                ));
            }
            let port = new_port(ec.heap, crate::io::new_string_port_input(&content));
            Ok(port)
        }
        Err(e) => Err(format!(
            "open-input-file: could not open file '{}': {}",
            filename, e
        )),
    }
}

/// Builtin function: (load filename) - evaluator-aware version
///
/// Loads and evaluates a Scheme file using the evaluator's port stack.

/// (push-port! port)
/// Pushes a port onto the port stack, causing the evaluator to load scheme code from it.
/// At EOF, the evaluator will pop the port and continue evaluating code from the next port on the stack.
///
/// # Examples
///
/// ```
/// let mut evaluator = Evaluator::new();
/// evaluator.push_port("example.txt");
/// ```
pub fn push_port(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    // (push-port! port)
    if args.len() != 1 {
        return Err("push_port!: expected exactly 1 argument".to_string());
    }
    let port_kind = port_kind_from_scheme_port(ec, args[0]);
    ec.port_stack.push(port_kind);
    Ok(ec.heap.nil_s())
}

pub fn pop_port(ec: &mut EvalContext, _args: &[GcRef]) -> Result<GcRef, String> {
    // (pop-port!)
    let port_kind = ec.port_stack.pop();
    match port_kind {
        Some(port_kind) => {
            let port = new_port(&mut ec.heap, port_kind);
            Ok(port)
        }
        None => Err("Port Stack is empty".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::Evaluator;
    use crate::gc::{SchemeValue, new_int, new_string};
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_open_input_file_success() {
        let mut ev = Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut ev);
        // Create a temp file with some content
        let mut tmpfile = NamedTempFile::new().unwrap();
        write!(tmpfile, "hello world").unwrap();
        let path = tmpfile.path().to_str().unwrap().to_string();
        let filename = new_string(&mut ec.heap, &path);
        let result = open_input_file(&mut ec, &[filename]);
        assert!(result.is_ok());
        let port = result.unwrap();
        match &ec.heap.get_value(port) {
            SchemeValue::Port(kind) => match kind {
                crate::io::PortKind::StringPortInput { content, .. } => {
                    assert_eq!(content, "hello world");
                }
                _ => panic!("Expected StringPortInput"),
            },
            _ => panic!("Expected Port"),
        }
    }

    #[test]
    fn test_open_input_file_nonexistent() {
        let mut ev = Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut ev);
        let filename = new_string(&mut ec.heap, "/no/such/file/hopefully.txt");
        let result = open_input_file(&mut ec, &[filename]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("could not open file"));
    }

    #[test]
    fn test_open_input_file_nonstring_arg() {
        let mut ev = Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut ev);
        let not_a_string = new_int(&mut ec.heap, 42.into());
        let result = open_input_file(&mut ec, &[not_a_string]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("argument must be a string"));
    }
}

macro_rules! register_builtin_family {
    ($heap:expr, $env:expr, $($name:expr => $func:expr),* $(,)?) => {
        $(
            $env.set_symbol($heap.intern_symbol($name),
                crate::gc::new_primitive($heap, $func,
                    concat!($name, ": builtin function").to_string()));
        )*
    };
}

pub fn register_fileio_builtins(heap: &mut GcHeap, env: &mut crate::env::Environment) {
    register_builtin_family!(heap, env,
        "read" => read_builtin,
        "open-input-file" => open_input_file,
        "push-port!" => push_port,
        "pop-port!" => pop_port,
        "eof-object?" => eof_object_q,
    );
}
