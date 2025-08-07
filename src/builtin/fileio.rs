use crate::eval::EvalContext;
use crate::gc::{GcHeap, GcRef, SchemeValue, new_port};

/// (open-input-file filename) -> port
pub fn open_input_file_builtin(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
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
pub fn load_builtin_evaluator(
    ec: &mut crate::eval::EvalContext,
    args: &[GcRef],
) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("load: expected exactly 1 argument".to_string());
    }

    let filename = match &ec.heap.get_value(args[0]) {
        crate::gc::SchemeValue::Str(filename) => filename.clone(),
        _ => return Err("load: argument must be a string".to_string()),
    };

    // For now, just return a success message
    // TODO: Implement actual file loading with proper port stack integration
    Ok(crate::gc::new_string(
        ec.heap,
        &format!("Loaded file: {}", filename),
    ))
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
        let result = open_input_file_builtin(&mut ec, &[filename]);
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
        let result = open_input_file_builtin(&mut ec, &[filename]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("could not open file"));
    }

    #[test]
    fn test_open_input_file_nonstring_arg() {
        let mut ev = Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut ev);
        let not_a_string = new_int(&mut ec.heap, 42.into());
        let result = open_input_file_builtin(&mut ec, &[not_a_string]);
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
    register_builtin_family!(heap, env, "open-input-file" => open_input_file_builtin,);
}
