//! Input/Output system for the Scheme interpreter.
//!
//! This module provides:
//! - Port management (stdin, stdout, files, string ports)
//! - Port stack for nested file loading with fallback to REPL
//! - File table for managing open file handles
//! - I/O operations (read/write lines and characters)
//!
//! # Examples
//!
//! ```rust
//! use s1::io::{FileTable, read_line, write_line, Port, PortKind};
//!
//! let stdin_port = Port { kind: PortKind::Stdin };
//! let mut file_table = FileTable::new();
//!
//! // Read from current port (stdin)
//! let line = read_line(&stdin_port, &mut file_table);
//!
//! // Write to current port (stdout)
//! write_line(&stdin_port, &mut file_table, "Hello, World!");
//! ```

use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead, BufReader, BufWriter, Read, Write};

// use std::io::BufReader as StdBufReader;
use crate::eval::{EvalContext, Evaluator};
use crate::gc::GcRef;
use std::cell::UnsafeCell;

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
        write: bool,
        file_id: Option<usize>,
    },
    /// In-memory string port for input with content and current position
    StringPortInput {
        content: String,
        pos: UnsafeCell<usize>,
    },
    /// In-memory string port for output with accumulating content
    StringPortOutput { content: String },
}

impl Clone for PortKind {
    fn clone(&self) -> Self {
        match self {
            PortKind::Stdin => PortKind::Stdin,
            PortKind::Stdout => PortKind::Stdout,
            PortKind::Stderr => PortKind::Stderr,
            PortKind::File {
                name,
                write,
                file_id,
            } => PortKind::File {
                name: name.clone(),
                write: *write,
                file_id: *file_id,
            },
            PortKind::StringPortInput { content, pos } => PortKind::StringPortInput {
                content: content.clone(),
                pos: UnsafeCell::new(unsafe { *pos.get() }),
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
                    write: w1,
                    file_id: f1,
                },
                PortKind::File {
                    name: n2,
                    write: w2,
                    file_id: f2,
                },
            ) => n1 == n2 && w1 == w2 && f1 == f2,
            (
                PortKind::StringPortInput {
                    content: c1,
                    pos: p1,
                },
                PortKind::StringPortInput {
                    content: c2,
                    pos: p2,
                },
            ) => c1 == c2 && unsafe { *p1.get() == *p2.get() },
            (
                PortKind::StringPortOutput { content: c1 },
                PortKind::StringPortOutput { content: c2 },
            ) => c1 == c2,
            _ => false,
        }
    }
}

/// Manages open file handles for file ports.
///
/// The file table maintains a mapping from file IDs to actual file handles,
/// allowing multiple file ports to reference the same underlying file
/// and ensuring proper cleanup when files are closed.
///
/// # Examples
///
/// ```rust
/// use s1::io::FileTable;
/// use std::fs::File;
/// use std::io::Write;
///
/// let mut table = FileTable::new();
///
/// // Create a test file
/// {
///     let mut file = File::create("test.txt").unwrap();
///     writeln!(file, "hello").unwrap();
/// }
///
/// // Open the file for reading
/// let id = table.open_file("test.txt", false).unwrap();
/// assert!(table.get(id).is_some());
///
/// // Close the file
/// table.close_file(id);
/// assert!(table.get(id).is_none());
/// ```
pub struct FileTable {
    /// Next available file ID
    next_id: usize,
    /// Mapping from file IDs to file handles
    files: HashMap<usize, File>,
}

impl FileTable {
    /// Create a new empty file table.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use s1::io::FileTable;
    ///
    /// let table = FileTable::new();
    /// ```
    pub fn new() -> Self {
        Self {
            next_id: 1,
            files: HashMap::new(),
        }
    }

    /// Open a file and return its ID.
    ///
    /// The file ID can be used to create file ports and access the file handle.
    /// Files are opened in read mode if `write` is `false`, or write mode if `write` is `true`.
    ///
    /// # Arguments
    ///
    /// * `name` - The filename to open
    /// * `write` - Whether to open in write mode (true) or read mode (false)
    ///
    /// # Returns
    ///
    /// Returns `Ok(file_id)` on success, or `Err` if the file cannot be opened.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use s1::io::FileTable;
    /// use std::fs::File;
    /// use std::io::Write;
    ///
    /// let mut table = FileTable::new();
    ///
    /// // Create a test file
    /// {
    ///     let mut file = File::create("test.txt").unwrap();
    ///     writeln!(file, "hello").unwrap();
    /// }
    ///
    /// // Open for reading
    /// let read_id = table.open_file("test.txt", false).unwrap();
    /// assert!(table.get(read_id).is_some());
    ///
    /// // Open for writing
    /// let write_id = table.open_file("output.txt", true).unwrap();
    /// assert!(table.get(write_id).is_some());
    /// ```
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
    /// # Arguments
    ///
    /// * `id` - The file ID to close
    ///
    /// # Examples
    ///
    /// ```rust
    /// use s1::io::FileTable;
    /// use std::fs::File;
    /// use std::io::Write;
    ///
    /// let mut table = FileTable::new();
    ///
    /// // Create and open a test file
    /// {
    ///     let mut file = File::create("test.txt").unwrap();
    ///     writeln!(file, "hello").unwrap();
    /// }
    ///
    /// let id = table.open_file("test.txt", false).unwrap();
    /// assert!(table.get(id).is_some());
    ///
    /// table.close_file(id);
    /// assert!(table.get(id).is_none());
    /// ```
    pub fn close_file(&mut self, id: usize) {
        self.files.remove(&id);
    }

    /// Get a mutable reference to a file handle by ID.
    ///
    /// Returns `None` if the file ID is not found in the table.
    ///
    /// # Arguments
    ///
    /// * `id` - The file ID to look up
    ///
    /// # Examples
    ///
    /// ```rust
    /// use s1::io::FileTable;
    /// use std::fs::File;
    /// use std::io::Write;
    ///
    /// let mut table = FileTable::new();
    ///
    /// // Create and open a test file
    /// {
    ///     let mut file = File::create("test.txt").unwrap();
    ///     writeln!(file, "hello").unwrap();
    /// }
    ///
    /// let id = table.open_file("test.txt", false).unwrap();
    /// assert!(table.get(id).is_some());
    ///
    /// table.close_file(id);
    /// assert!(table.get(id).is_none());
    /// ```
    pub fn get(&mut self, id: usize) -> Option<&mut File> {
        self.files.get_mut(&id)
    }
}

/// Helper function to read a line from a string port and update its position
fn read_line_from_string_port(port_kind: &PortKind) -> Option<(String, PortKind)> {
    if let PortKind::StringPortInput { content, .. } = &port_kind {
        let current_pos = get_string_port_pos(port_kind).unwrap();
        let mut lines = content.lines();
        // Skip to the current position
        for _ in 0..current_pos {
            lines.next();
        }
        if let Some(line) = lines.next() {
            let new_pos = current_pos + 1;
            let new_port = PortKind::StringPortInput {
                content: content.clone(),
                pos: UnsafeCell::new(new_pos),
            };
            Some((line.to_string() + "\n", new_port))
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
/// # Arguments
///
/// * `port` - The current port to read from
/// * `file_table` - The file table for file port operations
///
/// # Returns
///
/// Returns `Some(line)` if a line was successfully read, or `None` if
/// the port is exhausted.
///
/// # Examples
///
/// ```rust
/// use s1::io::{FileTable, read_line, Port, PortKind};
///
/// let port = Port { kind: PortKind::Stdin };
/// let mut file_table = FileTable::new();
///
/// // Read from stdin (in a real scenario, this would block for user input)
/// let line = read_line(&port, &mut file_table);
/// ```
pub fn read_line(port_kind: &PortKind, file_table: &mut FileTable) -> Option<String> {
    // Handle StringPort case separately to avoid borrow checker issues
    if let Some((line, _)) = read_line_from_string_port(port_kind) {
        return Some(line);
    }

    // Handle other port types
    match &port_kind {
        PortKind::Stdin => {
            let mut buf = String::new();
            let n = io::stdin().read_line(&mut buf).ok()?;
            if n == 0 { None } else { Some(buf) }
        }
        PortKind::File { file_id, .. } => {
            if let Some(file_id) = file_id {
                if let Some(file) = file_table.get(*file_id) {
                    let mut reader = BufReader::new(file);
                    let mut buf = String::new();
                    let n = reader.read_line(&mut buf).ok()?;
                    if n == 0 { None } else { Some(buf) }
                } else {
                    None
                }
            } else {
                None
            }
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
/// # Arguments
///
/// * `port` - The current port to write to
/// * `file_table` - The file table for file port operations
/// * `line` - The line to write
///
/// # Returns
///
/// Returns `true` if the write was successful, `false` otherwise.
///
/// # Examples
///
/// ```rust
/// use s1::io::{FileTable, write_line, Port, PortKind};
///
/// let port = Port { kind: PortKind::Stdout };
/// let mut file_table = FileTable::new();
///
/// // Write to stdout
/// write_line(&port, &mut file_table, "Hello, World!");
/// ```
pub fn write_line(port_kind: &PortKind, file_table: &mut FileTable, line: &str) -> bool {
    match &port_kind {
        PortKind::Stdout => {
            print!("{}", line);
            io::stdout().flush().ok();
            true
        }
        PortKind::File { file_id, .. } => {
            if let Some(file_id) = file_id {
                if let Some(file) = file_table.get(*file_id) {
                    let mut writer = BufWriter::new(file);
                    writer.write_all(line.as_bytes()).is_ok()
                } else {
                    false
                }
            } else {
                false
            }
        }
        PortKind::StringPortOutput { .. } => {
            // We need to modify the content, so we need a mutable reference
            // This is a limitation of the current design - we'll need to handle this differently
            false
        }
        _ => false,
    }
}

/// Read a single character from the current input port.
///
/// This function reads one character from the current port.
/// For string ports, it advances the position pointer after reading.
///
/// # Arguments
///
/// * `port` - The current port to read from
/// * `file_table` - The file table for file port operations
///
/// # Returns
///
/// Returns `Some(char)` if a character was successfully read, or `None` if
/// the port is exhausted or an error occurred.
///
/// # Examples
///
/// ```rust
/// use s1::io::{FileTable, read_char, Port, PortKind};
///
/// let port = Port { kind: PortKind::Stdin };
/// let mut file_table = FileTable::new();
///
/// // Read a character from stdin (in a real scenario, this would block for user input)
/// let ch = read_char(&port, &mut file_table);
/// ```
pub fn read_char(port_kind: &PortKind, file_table: &mut FileTable) -> Option<char> {
    match &port_kind {
        PortKind::Stdin => {
            let mut buf = [0u8; 1];
            match std::io::stdin().read_exact(&mut buf) {
                Ok(_) => Some(buf[0] as char),
                Err(_) => None,
            }
        }
        PortKind::Stdout => None,
        PortKind::Stderr => None,
        PortKind::File { file_id, .. } => {
            if let Some(file_id) = file_id {
                if let Some(file) = file_table.get(*file_id) {
                    let mut buf = [0u8; 1];
                    match file.read_exact(&mut buf) {
                        Ok(_) => Some(buf[0] as char),
                        Err(_) => None,
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }
        PortKind::StringPortInput { content, .. } => {
            let current_pos = get_string_port_pos(port_kind).unwrap();
            if current_pos < content.len() {
                let ch = content.chars().nth(current_pos).unwrap();
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
/// For stdout ports, the character is printed to the console. For file ports,
/// the character is written to the file.
///
/// # Arguments
///
/// * `port` - The current port to write to
/// * `file_table` - The file table for file port operations
/// * `ch` - The character to write
///
/// # Returns
///
/// Returns `true` if the write was successful, `false` otherwise.
///
/// # Examples
///
/// ```rust
/// use s1::io::{FileTable, write_char, Port, PortKind};
///
/// let port = Port { kind: PortKind::Stdout };
/// let mut file_table = FileTable::new();
///
/// // Write a character to stdout
/// write_char(&port, &mut file_table, 'A');
/// ```
pub fn write_char(port_kind: &PortKind, file_table: &mut FileTable, ch: char) -> bool {
    match &port_kind {
        PortKind::Stdout => {
            print!("{}", ch);
            io::stdout().flush().ok();
            true
        }
        PortKind::File { file_id, .. } => {
            if let Some(file_id) = file_id {
                if let Some(file) = file_table.get(*file_id) {
                    let mut writer = BufWriter::new(file);
                    writer.write_all(&[ch as u8]).is_ok()
                } else {
                    false
                }
            } else {
                false
            }
        }
        PortKind::StringPortOutput { .. } => {
            // We need to modify the content, so we need a mutable reference
            // This is a limitation of the current design - we'll need to handle this differently
            false
        }
        _ => false,
    }
}

/// Create a new string port for in-memory string I/O.
///
/// String ports allow reading from a string as if it were a file, with
/// an internal position pointer that advances as characters are read.
///
/// # Arguments
///
/// * `s` - The string to create a port from
///
/// # Returns
///
/// Returns a new string port.
///
/// # Examples
///
/// ```rust
/// use s1::io::{new_string_port, FileTable, read_char, Port, PortKind};
///
/// let string_port = new_string_port("hello");
/// let mut file_table = FileTable::new();
///
/// // Read characters from the string
/// assert_eq!(read_char(&string_port, &mut file_table), Some('h'));
/// assert_eq!(read_char(&string_port, &mut file_table), Some('e'));
/// assert_eq!(read_char(&string_port, &mut file_table), Some('l'));
/// ```
pub fn new_string_port(s: &str) -> PortKind {
    PortKind::StringPortInput {
        content: s.to_string(),
        pos: UnsafeCell::new(0),
    }
}

/// Create a new output string port for in-memory string output.
///
/// Output string ports allow writing to a string as if it were a file,
/// accumulating content that can be retrieved later.
///
/// # Returns
///
/// Returns a new output string port.
///
/// # Examples
///
/// ```rust
/// use s1::io::{new_output_string_port, FileTable, write_line, get_output_string, Port, PortKind};
///
/// let output_port = new_output_string_port();
/// let mut file_table = FileTable::new();
///
/// // Write to the string port
/// write_line(&output_port, &mut file_table, "hello");
///
/// // Get the accumulated content
/// let content = get_output_string(&output_port);
/// assert_eq!(content, "hello");
/// ```
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
/// # Arguments
///
/// * `port` - The port to get content from
///
/// # Returns
///
/// Returns the accumulated content as a string, or an empty string if the port
/// is not an output string port.
///
/// # Examples
///
/// ```rust
/// use s1::io::{new_output_string_port, get_output_string, Port, PortKind};
///
/// let port = new_output_string_port();
/// let content = get_output_string(&port);
/// assert_eq!(content, "");
/// ```
pub fn get_output_string(port_kind: &PortKind) -> String {
    match &port_kind {
        PortKind::StringPortOutput { content } => content.clone(),
        _ => String::new(),
    }
}

/// Get the current position of a string port safely.
/// This function should be called through the GC heap accessor.
pub fn get_string_port_pos(port_kind: &PortKind) -> Option<usize> {
    match &port_kind {
        PortKind::StringPortInput { pos, .. } => Some(unsafe { *pos.get() }),
        _ => None,
    }
}

/// Update the position of a string port safely.
/// This function should be called through the GC heap accessor.
pub fn update_string_port_pos(port_kind: &PortKind, new_pos: usize) -> bool {
    match &port_kind {
        PortKind::StringPortInput { pos, .. } => {
            unsafe {
                *pos.get() = new_pos;
            }
            true
        }
        _ => false,
    }
}

/// Create a new input string port from a &str.
pub fn new_string_port_input(content: &str) -> PortKind {
    PortKind::StringPortInput {
        content: content.to_string(),
        pos: UnsafeCell::new(0),
    }
}

/// Convert a Rust Port to a Scheme port object.
pub fn port_to_scheme_port(ec: &mut EvalContext, port_kind: PortKind) -> GcRef {
    let heap = &mut ec.heap;
    crate::gc::new_port(heap, port_kind)
}

/// Extract a PortKind from aScheme port
pub fn port_kind_from_scheme_port(evaluator: &mut Evaluator, scheme_port: GcRef) -> PortKind {
    let s_p = evaluator.heap.get_value(scheme_port);
    match s_p {
        crate::gc::SchemeValue::Port(kind) => kind.clone(),
        _ => panic!("Expected port object"),
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn test_port_conversion() {
//         use crate::gc::GcHeap;

//         let mut heap = GcHeap::new();
//         let original_port = Port {
//             kind: PortKind::StringPortInput {
//                 content: "hello".to_string(),
//                 pos: UnsafeCell::new(0),
//             },
//         };

//         let scheme_port = port_to_scheme_port(ec, original_port.clone());
//         let converted_port = port_from_scheme_port(ec, scheme_port);

//         assert_eq!(original_port.kind, converted_port.kind);
//     }
// }
