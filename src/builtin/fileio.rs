use crate::env::{EnvOps, EnvRef};
use crate::gc::{GcHeap, GcRef, SchemeValue, new_bool, new_port};
use std::io::Write;

macro_rules! register_builtin_family {
    ($heap:expr, $env:expr, $($name:expr => $func:expr),* $(,)?) => {
        $(
            $env.define($heap.intern_symbol($name),
                crate::gc::new_builtin($heap, $func,
                    concat!($name, ": builtin function").to_string()));
        )*
    };
}

pub fn register_fileio_builtins(heap: &mut GcHeap, env: EnvRef) {
    register_builtin_family!(heap, env,
        "open-input-file" => open_input_file,
        "open-output-file" => open_output_file,
        "eof-object?" => eof_object_q,
        "input-port?" => input_port_q,
        "output-port?" => output_port_q,
        "close-input-port" => close_input_port,
        "flush-output" => flush_output,
        "close-output-port" => close_output_port,
    );
}

fn output_port_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("output-port?: expected 1 argument".to_string());
    }
    let is_output_port = match &heap.get_value(args[0]) {
        SchemeValue::Port(kind) => match kind {
            crate::io::PortKind::Stdout | crate::io::PortKind::Stderr | crate::io::PortKind::StringPortOutput { .. } => true,
            crate::io::PortKind::File { write, .. } => *write,
            _ => false,
        },
        _ => false,
    };
    Ok(new_bool(heap, is_output_port))
}

/// (eof-object? obj) -> #t or #f
/// Tests the result of a read operation to determine if it reached the end of the file.
fn eof_object_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("eof-object?: expected exactly 1 argument".to_string());
    }
    let is_eof = matches!(&heap.get_value(args[0]), SchemeValue::Eof);
    Ok(new_bool(heap, is_eof))
}

fn input_port_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("input-port?: expected 1 argument".to_string());
    }
    let is_input_port = match &heap.get_value(args[0]) {
        SchemeValue::Port(kind) => match kind {
            crate::io::PortKind::Stdin | crate::io::PortKind::StringPortInput { .. } => true,
            crate::io::PortKind::File { write, .. } => !*write,
            _ => false,
        },
        _ => false,
    };
    Ok(new_bool(heap, is_input_port))
}

/// (open-input-file filename) -> port
fn open_input_file(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("open-input-file: expected exactly 1 argument".to_string());
    }
    let filename = match &heap.get_value(args[0]) {
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
            let port = new_port(heap, crate::io::new_string_port_input(&content));
            Ok(port)
        }
        Err(e) => Err(format!(
            "open-input-file: could not open file '{}': {}",
            filename, e
        )),
    }
}

/// (open-output-file filename) -> port
fn open_output_file(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("open-output-file: expected exactly 1 argument".to_string());
    }
    let filename = match &heap.get_value(args[0]) {
        SchemeValue::Str(s) => s,
        _ => return Err("open-output-file: argument must be a string".to_string()),
    };
    // Try to open the file
    match crate::io::FileTable::new().open_file(filename, true) {
        Ok(id) => {
            let port = new_port(
                heap,
                crate::io::PortKind::File {
                    name: filename.to_string(),
                    id,
                    write: true,
                    pos: std::cell::Cell::new(0),
                },
            );
            Ok(port)
        }
        Err(e) => Err(format!(
            "open-output-file: could not open file '{}': {}",
            filename, e
        )),
    }
}

fn close_input_port(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("close-input-port: expected exactly 1 argument".to_string());
    }
    let port = args[0];
    match heap.get_value(port) {
        SchemeValue::Port(kind) => {
            if let crate::io::PortKind::File { id, write: false, .. } = kind {
                crate::io::FileTable::new().close_file(*id);
                Ok(heap.void())
            } else {
                // Closing non-file input ports is a no-op.
                Ok(heap.void())
            }
        }
        _ => Err("close-input-port: argument must be a port".to_string()),
    }
}

/// (close-output-port port)
fn close_output_port(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("close-output-port: expected exactly 1 argument".to_string());
    }
    let port = args[0];
    match heap.get_value(port) {
        SchemeValue::Port(kind) => {
            if let crate::io::PortKind::File { id, write: true, .. } = kind {
                crate::io::FileTable::new().close_file(*id);
                Ok(heap.void())
            } else {
                Err("close-output-port: argument must be an output file port".to_string())
            }
        }
        _ => Err("close-output-port: argument must be a port".to_string()),
    }
}

fn flush_output(heap: &mut GcHeap, _args: &[GcRef]) -> Result<GcRef, String> {
    std::io::stdout().flush().ok();
    Ok(heap.void())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::RunTimeStruct;
    use crate::gc::{SchemeValue, new_int, new_string};
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_open_input_file_success() {
        let mut ev = RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);
        // Create a temp file with some content
        let mut tmpfile = NamedTempFile::new().unwrap();
        write!(tmpfile, "hello world").unwrap();
        let path = tmpfile.path().to_str().unwrap().to_string();
        let filename = new_string(&mut ec.heap, &path);
        let result = open_input_file(&mut ec.heap, &[filename]);
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
        let mut ev = RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);
        let filename = new_string(&mut ec.heap, "/no/such/file/hopefully.txt");
        let result = open_input_file(&mut ec.heap, &[filename]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("could not open file"));
    }

    #[test]
    fn test_open_input_file_nonstring_arg() {
        let mut ev = RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);
        let not_a_string = new_int(&mut ec.heap, 42.into());
        let result = open_input_file(&mut ec.heap, &[not_a_string]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("argument must be a string"));
    }
}