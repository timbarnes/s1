use rustc_hash::FxHashMap as HashMap;
use std::fs::File;
use std::io::{self, BufRead, Read, Write};

// use std::io::BufReader as StdBufReader;
use crate::eval::RunTime;
use crate::gc::GcRef;
use std::cell::Cell;

/// The different types of ports supported by the I/O system.
///
/// Ports are used for input/output operations and can be:
/// - Standard input/output streams
/// - File-based ports for reading/writing files
/// - String ports for in-memory string operations
#[derive(Debug)]
pub enum PortKind {
    /// Standard input stream
    Stdin,
    /// Standard output stream
    Stdout,
    /// Standard error stream
    Stderr,
    /// File-based port with read/write mode and optional file ID
    File {
        name: String,
        id: usize,        // ID from FileTable
        write: bool,      // true if output, false if input
        pos: Cell<usize>, // Current position in the file
    },
    /// In-memory string port for input with content and current position
    StringPortInput {
        content: String,
        pos: Cell<usize>,
    },
    // In-memory string port for output with accumulating content
    StringPortOutput {
        content: String,
    },
}

impl Clone for PortKind {
    fn clone(&self) -> Self {
        match self {
            PortKind::Stdin => PortKind::Stdin,
            PortKind::Stdout => PortKind::Stdout,
            PortKind::Stderr => PortKind::Stderr,
            PortKind::File {
                name,
                id,
                write,
                pos,
            } => PortKind::File {
                name: name.clone(),
                id: *id,
                write: *write,
                pos: Cell::new(pos.get()),
            },
            PortKind::StringPortInput { content, pos } => PortKind::StringPortInput {
                content: content.clone(),
                pos: Cell::new(pos.get()),
            },
            PortKind::StringPortOutput { content } => PortKind::StringPortOutput {
                content: content.clone(),
            },
        }
    }
}

impl PartialEq for PortKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (PortKind::Stdin, PortKind::Stdin) => true,
            (PortKind::Stdout, PortKind::Stdout) => true,
            (PortKind::Stderr, PortKind::Stderr) => true,
            (
                PortKind::File {
                    name: n1,
                    id: i1,
                    write: w1,
                    pos: p1,
                },
                PortKind::File {
                    name: n2,
                    id: i2,
                    write: w2,
                    pos: p2,
                },
            ) => n1 == n2 && i1 == i2 && w1 == w2 && p1.get() == p2.get(),
            (
                PortKind::StringPortInput {
                    content: c1,
                    pos: p1,
                },
                PortKind::StringPortInput {
                    content: c2,
                    pos: p2,
                },
            ) => c1 == c2 && p1.get() == p2.get(),
            (
                PortKind::StringPortOutput { content: c1 },
                PortKind::StringPortOutput { content: c2 },
            ) => c1 == c2,
            _ => false,
        }
    }
}

impl PortKind {
    pub fn next_char_utf8(&mut self) -> Option<char> {
        match self {
            PortKind::StringPortInput { content, pos } => {
                let mut p = pos.get();
                if p >= content.len() {
                    return None;
                }
                let rest = &content[p..];
                let mut iter = rest.chars();
                let ch = iter.next()?;
                p += ch.len_utf8();
                pos.set(p);
                Some(ch)
            }
            PortKind::Stdin => {
                let stdin = io::stdin();
                let mut handle = stdin.lock();
                let mut buf = [0u8; 4]; // max size of UTF-8 char
                let mut first = [0u8; 1];
                if handle.read_exact(&mut first).is_err() {
                    return None;
                }

                let needed = utf8_char_width(first[0]);
                buf[0] = first[0];
                if needed > 1 {
                    if handle.read_exact(&mut buf[1..needed]).is_err() {
                        return None;
                    }
                }

                std::str::from_utf8(&buf[..needed])
                    .ok()
                    .and_then(|s| s.chars().next())
            }
            _ => todo!("next_char_utf8 PortKind types"),
        }
    }
}

/// UTF-8 width helper
fn utf8_char_width(first: u8) -> usize {
    match first {
        0x00..=0x7F => 1,
        0xC0..=0xDF => 2,
        0xE0..=0xEF => 3,
        0xF0..=0xF7 => 4,
        _ => 1,
    }
}

/// Manages open file handles for file ports.
///
/// The file table maintains a mapping from file IDs to actual file handles,
/// allowing multiple file ports to reference the same underlying file
/// and ensuring proper cleanup when files are closed.
///
pub struct FileTable {
    /// Next available file ID
    next_id: usize,
    /// Mapping from file IDs to file handles
    files: HashMap<usize, File>,
}

impl FileTable {
    /// Create a new empty file table.
    pub fn new() -> Self {
        Self {
            next_id: 1,
            files: HashMap::default(),
        }
    }

    /// Open a file and return its ID.
    ///
    /// The file ID can be used to create file ports and access the file handle.
    /// Files are opened in read mode if `write` is `false`, or write mode if `write` is `true`.
    ///
    pub fn open_file(&mut self, name: &str, write: bool) -> io::Result<usize> {
        let file = if write {
            File::create(name)?
        } else {
            File::open(name)?
        };
        let id = self.next_id;
        self.next_id += 1;
        self.files.insert(id, file);
        Ok(id)
    }

    /// Close a file by its ID.
    ///
    /// The file handle is removed from the table and the underlying file is closed.
    ///
    pub fn close_file(&mut self, id: usize) {
        self.files.remove(&id);
    }

    /// Get a mutable reference to a file handle by ID.
    ///
    /// Returns `None` if the file ID is not found in the table.
    ///
    pub fn get(&mut self, id: usize) -> Option<&mut File> {
        self.files.get_mut(&id)
    }
}

/// Helper function to read a line from a string port and update its position
fn read_line_from_string_port(
    port_kind: &mut PortKind,
    file_table: &mut FileTable,
) -> Option<(String, PortKind)> {
    if let PortKind::StringPortInput { content, pos } = port_kind {
        let current_pos = pos.get();
        let mut lines = content[current_pos..].lines();
        if let Some(line) = lines.next() {
            let new_pos = current_pos + line.len() + 1; // +1 for the newline character
            pos.set(new_pos);
            Some((line.to_string() + "\n", port_kind.clone()))
        } else {
            None
        }
    } else if let PortKind::File { id, pos, .. } = port_kind {
        if let Some(file) = file_table.get(*id) {
            let mut reader = std::io::BufReader::new(file);
            let mut buf = String::new();
            let n = reader.read_line(&mut buf).ok()?;
            if n == 0 {
                None
            } else {
                pos.set(pos.get() + n);
                Some((buf, port_kind.clone()))
            }
        } else {
            None
        }
    } else {
        None
    }
}

/// Read a line from the current input port.
///
/// This function reads from the current port. If the current
/// port is exhausted (EOF), it returns None.
///
pub fn read_line(port_kind: &mut PortKind, file_table: &mut FileTable) -> Option<String> {
    // Handle StringPort case separately to avoid borrow checker issues
    if let Some((line, _)) = read_line_from_string_port(port_kind, file_table) {
        return Some(line);
    }

    // Handle other port types
    match port_kind {
        PortKind::Stdin => {
            let mut buf = String::new();
            let n = io::stdin().read_line(&mut buf).ok()?;
            if n == 0 { None } else { Some(buf) }
        }
        _ => None,
    }
}

/// Write a line to the current output port.
///
/// This function writes to the current port. For stdout ports,
/// the line is printed to the console. For file ports, the line is written to
/// the file.
///
pub fn write_line(port_kind: &mut PortKind, file_table: &mut FileTable, line: &str) -> bool {
    match port_kind {
        PortKind::Stdout => {
            print!("{}", line);
            io::stdout().flush().ok();
            true
        }
        PortKind::File { id, .. } => {
            if let Some(file) = file_table.get(*id) {
                let mut writer = std::io::BufWriter::new(file);
                writer.write_all(line.as_bytes()).is_ok()
            } else {
                eprintln!("write: failed to write to port");
                false
            }
        }
        PortKind::StringPortOutput { content } => {
            content.push_str(line);
            true
        }
        _ => false,
    }
}

/// Read a single character from the current input port.
///
/// This function reads one character from the current port.
/// For string ports, it advances the position pointer after reading.
///
pub fn read_char(port_kind: &PortKind, file_table: &mut FileTable) -> Option<char> {
    match port_kind {
        PortKind::Stdin => {
            let mut buf = [0u8; 1];
            match std::io::stdin().read_exact(&mut buf) {
                Ok(_) => Some(buf[0] as char),
                Err(_) => None,
            }
        }
        PortKind::Stdout => None,
        PortKind::Stderr => None,
        PortKind::File { id, pos, .. } => {
            if let Some(file) = file_table.get(*id) {
                let mut reader = std::io::BufReader::new(file);
                let mut buf = [0u8; 1];
                match reader.read_exact(&mut buf) {
                    Ok(_) => {
                        pos.set(pos.get() + 1);
                        Some(buf[0] as char)
                    }
                    Err(_) => None,
                }
            } else {
                None
            }
        }
        PortKind::StringPortInput { content, pos } => {
            let current_pos = pos.get();
            if current_pos < content.len() {
                let ch = content[current_pos..].chars().next().unwrap();
                pos.set(current_pos + ch.len_utf8());
                Some(ch)
            } else {
                None
            }
        }
        PortKind::StringPortOutput { .. } => None,
    }
}

/// Write a single character to the current output port.
///
/// This function writes one character to the current port.
/// For stdout ports, the character is printed to the console.
/// For file ports, the character is written to the file.
///
pub fn write_char(port_kind: &PortKind, file_table: &mut FileTable, ch: char) -> bool {
    match port_kind {
        PortKind::Stdout => {
            print!("{}", ch);
            io::stdout().flush().ok();
            true
        }
        PortKind::File { id, .. } => {
            if let Some(file) = file_table.get(*id) {
                let mut writer = std::io::BufWriter::new(file);
                writer.write_all(ch.to_string().as_bytes()).is_ok()
            } else {
                false
            }
        }
        PortKind::StringPortOutput { .. } => {
            // This will be fixed later by using RefCell
            todo!()
        }
        _ => false,
    }
}

/// Peek at the next character from the input port without consuming it.
///
/// This function returns the next character that would be read by `read_char`,
/// but it does not advance the port's position.
///
/// Currently only supported for String ports.
pub fn peek_char(port_kind: &PortKind, _file_table: &mut FileTable) -> Option<char> {
    match port_kind {
        PortKind::StringPortInput { content, pos } => {
            let p = pos.get();
            if p >= content.len() {
                None
            } else {
                content[p..].chars().next()
            }
        }
        _ => None, // Not supported for other types yet
    }
}

/// Check if a character is ready on the input port.
///
/// Returns `true` if a character is available for reading without blocking,
/// and `false` otherwise.
pub fn char_ready(port_kind: &PortKind) -> bool {
    match port_kind {
        PortKind::StringPortInput { content, pos } => pos.get() < content.len(),
        PortKind::File { .. } => true, // Assume file is always ready until EOF
        PortKind::Stdin => false,      // Stdin blocking check not supported
        _ => false,
    }
}

/// Create a new string port for in-memory string I/O.
///
/// String ports allow reading from a string as if it were a file, with
/// an internal position pointer that advances as characters are read.
///
pub fn new_string_port_input(content: &str) -> PortKind {
    PortKind::StringPortInput {
        content: content.to_string(),
        pos: Cell::new(0),
    }
}

/// Create a new output string port for in-memory string output.
///
/// Output string ports allow writing to a string as if it were a file,
/// accumulating content that can be retrieved later.
///
pub fn new_output_string_port() -> PortKind {
    PortKind::StringPortOutput {
        content: String::new(),
    }
}

/// Get the content from an output string port.
///
/// This function retrieves the accumulated content from a string output port.
/// It should only be called on ports that are output string ports.
///
pub fn get_output_string(port_kind: &mut PortKind) -> String {
    match port_kind {
        PortKind::StringPortOutput { content } => content.clone(),
        _ => String::new(),
    }
}

/// Get the current position of a string port safely.
/// This function should be called through the GC heap accessor.
pub fn get_string_port_pos(port_kind: &mut PortKind) -> Option<usize> {
    match port_kind {
        PortKind::StringPortInput { pos, .. } => Some(pos.get()),
        _ => None,
    }
}

/// Update the position of a string port safely.
/// This function should be called through the GC heap accessor.
pub fn update_string_port_pos(port_kind: &mut PortKind, new_pos: usize) -> bool {
    match port_kind {
        PortKind::StringPortInput { pos, .. } => {
            pos.set(new_pos);
            true
        }
        _ => false,
    }
}

/// Convert a Rust Port to a Scheme port object.
pub fn port_to_scheme_port(rt: &mut RunTime, port_kind: PortKind) -> GcRef {
    let heap = &mut rt.heap;
    crate::gc::new_port(heap, port_kind)
}

/// Extract a PortKind from aScheme port
pub fn port_kind_from_scheme_port(rt: &mut RunTime, scheme_port: GcRef) -> PortKind {
    let s_p = rt.heap.get_value(scheme_port);
    match s_p {
        crate::gc::SchemeValue::Port(kind) => kind.clone(),
        _ => panic!("Expected port object"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_port_conversion() {
        let mut runtime = crate::eval::RunTimeStruct::new();
        let mut rt = RunTime::from_eval(&mut runtime);

        let orig_port_kind = PortKind::StringPortInput {
            content: "hello".to_string(),
            pos: Cell::new(0),
        };
        //let original_port = crate::gc::new_port(&mut heap, port_kind);

        let scheme_port = port_to_scheme_port(&mut rt, orig_port_kind.clone());
        let converted_port = port_kind_from_scheme_port(&mut rt, scheme_port);

        assert_eq!(&orig_port_kind, &converted_port);
    }
}
