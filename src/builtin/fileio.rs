use crate::env::{EnvOps, EnvRef};
use crate::gc::{GcHeap, GcRef, SchemeValue, new_port};
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
        "eof-object?" => eof_object_q,
        "flush-output" => flush_output,
    );
}

/// (eof-object? obj) -> #t or #f
/// Tests the result of a read operation to determine if it reached the end of the file.
fn eof_object_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("eof-object: expected exactly 0 arguments".to_string());
    }
    let result;
    if crate::gc::eq(&heap, heap.eof(), args[0]) {
        result = heap.true_s();
    } else {
        result = heap.false_s();
    }
    Ok(result)
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
