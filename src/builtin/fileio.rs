use crate::gc::{GcHeap, GcRef, SchemeValue, new_port};

/// (open-input-file filename) -> port
pub fn open_input_file_builtin(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("open-input-file: expected exactly 1 argument".to_string());
    }
    let filename = match &args[0].value {
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
                return Err(format!("open-input-file: could not read file '{}': {}", filename, e));
            }
            let port = new_port(heap, crate::io::PortKind::StringPortInput {
                content,
                pos: std::cell::UnsafeCell::new(0),
            });
            Ok(port)
        }
        Err(e) => Err(format!("open-input-file: could not open file '{}': {}", filename, e)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gc::{GcHeap, new_string, new_int, SchemeValue};
    use std::fs::File;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_open_input_file_success() {
        let mut heap = GcHeap::new();
        // Create a temp file with some content
        let mut tmpfile = NamedTempFile::new().unwrap();
        write!(tmpfile, "hello world").unwrap();
        let path = tmpfile.path().to_str().unwrap().to_string();
        let filename = new_string(&mut heap, &path);
        let result = open_input_file_builtin(&mut heap, &[filename]);
        assert!(result.is_ok());
        let port = result.unwrap();
        match &port.value {
            SchemeValue::Port { kind } => match kind {
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
        let mut heap = GcHeap::new();
        let filename = new_string(&mut heap, "/no/such/file/hopefully.txt");
        let result = open_input_file_builtin(&mut heap, &[filename]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("could not open file"));
    }

    #[test]
    fn test_open_input_file_nonstring_arg() {
        let mut heap = GcHeap::new();
        let not_a_string = new_int(&mut heap, 42.into());
        let result = open_input_file_builtin(&mut heap, &[not_a_string]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("argument must be a string"));
    }
} 