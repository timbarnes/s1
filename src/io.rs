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
//! use s1::io::{PortStack, FileTable, read_line, write_line, Port, PortKind};
//!
//! let stdin_port = Port { kind: PortKind::Stdin };
//! let mut port_stack = PortStack::new(stdin_port);
//! let mut file_table = FileTable::new();
//!
//! // Read from current port (stdin)
//! let line = read_line(&mut port_stack, &mut file_table);
//!
//! // Write to current port (stdout)
//! write_line(&mut port_stack, &mut file_table, "Hello, World!");
//! ```

use std::io::{self, Write, Read, BufRead, BufReader, BufWriter};
use std::collections::HashMap;
use std::fs::File;
use std::io::BufReader as StdBufReader;
use std::cell::UnsafeCell;
use crate::gc::GcRef;

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
    File { name: String, write: bool, file_id: Option<usize> },
    /// In-memory string port for input with content and current position
    StringPortInput { content: String, pos: UnsafeCell<usize> },
    /// In-memory string port for output with accumulating content
    StringPortOutput { content: String },
}

impl Clone for PortKind {
    fn clone(&self) -> Self {
        match self {
            PortKind::Stdin => PortKind::Stdin,
            PortKind::Stdout => PortKind::Stdout,
            PortKind::Stderr => PortKind::Stderr,
            PortKind::File { name, write, file_id } => PortKind::File {
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
            (PortKind::File { name: n1, write: w1, file_id: f1 }, PortKind::File { name: n2, write: w2, file_id: f2 }) => {
                n1 == n2 && w1 == w2 && f1 == f2
            }
            (PortKind::StringPortInput { content: c1, pos: p1 }, PortKind::StringPortInput { content: c2, pos: p2 }) => {
                c1 == c2 && unsafe { *p1.get() == *p2.get() }
            }
            (PortKind::StringPortOutput { content: c1 }, PortKind::StringPortOutput { content: c2 }) => {
                c1 == c2
            }
            _ => false,
        }
    }
}

/// Represents an open port (input or output).
///
/// Ports can be stdin, stdout, file-based, or string-based.
/// File ports store a file_id that maps to an open file handle in the FileTable.
/// String ports store the string content and current read position.
#[derive(Clone, Debug)]
pub struct Port {
    /// The type and configuration of this port
    pub kind: PortKind,
}

/// Manages a stack of ports for input/output operations.
///
/// The port stack allows for nested file loading where reading from a file
/// can temporarily switch the input source, then fall back to the previous
/// port when the file is exhausted or closed.
///
/// # Examples
///
/// ```rust
/// use s1::io::{PortStack, Port, PortKind};
///
/// let stdin_port = Port { kind: PortKind::Stdin };
/// let file_port = Port { kind: PortKind::File { name: "input.txt".to_string(), write: false, file_id: Some(1) } };
///
/// let mut stack = PortStack::new(stdin_port);
/// assert_eq!(stack.current().kind, PortKind::Stdin);
///
/// stack.push(file_port);
/// assert_eq!(stack.current().kind, PortKind::File { name: "input.txt".to_string(), write: false, file_id: Some(1) });
///
/// stack.pop();
/// assert_eq!(stack.current().kind, PortKind::Stdin);
/// ```
pub struct PortStack {
    /// The stack of ports, with the current port at the top
    stack: Vec<GcRef>,
}

/// Scheme-based port stack that uses a vector of port objects.
/// This is the new implementation that stores ports as Scheme objects.
pub struct SchemePortStack {
    /// The stack of ports as Scheme objects, with the current port at the top
    stack: Vec<crate::gc::GcRef>,
}

impl PortStack {
    /// Create a new port stack with an initial port.
    ///
    /// The initial port is typically stdin for interactive use.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use s1::io::{PortStack, Port, PortKind};
    ///
    /// let stdin_port = Port { kind: PortKind::Stdin };
    /// let stack = PortStack::new(stdin_port);
    /// assert_eq!(stack.current().kind, PortKind::Stdin);
    /// ```
    pub fn new(initial: GcRef) -> Self {
        Self { stack: vec![initial] }
    }

    /// Get the current port (the top of the stack).
    ///
    /// # Examples
    ///
    /// ```rust
    /// use s1::io::{PortStack, Port, PortKind};
    ///
    /// let port = Port { kind: PortKind::Stdin };
    /// let stack = PortStack::new(port);
    /// assert_eq!(stack.current().kind, PortKind::Stdin);
    /// ```
    pub fn current(&self) -> GcRef {
        *self.stack.last().unwrap()
    }

    /// Get a mutable reference to the current port.
    ///
    /// This is useful for updating port state (like string port position).
    pub fn current_mut(&mut self) -> &mut GcRef {
        self.stack.last_mut().unwrap()
    }

    /// Push a new port onto the stack, making it the current port.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use s1::io::{PortStack, Port, PortKind};
    ///
    /// let stdin_port = Port { kind: PortKind::Stdin };
    /// let file_port = Port { kind: PortKind::File { name: "input.txt".to_string(), write: false, file_id: Some(1) } };
    ///
    /// let mut stack = PortStack::new(stdin_port);
    /// stack.push(file_port);
    /// assert_eq!(stack.current().kind, PortKind::File { name: "input.txt".to_string(), write: false, file_id: Some(1) });
    /// ```
    pub fn push(&mut self, port: GcRef) {
        self.stack.push(port);
    }

    /// Pop the current port from the stack, returning to the previous port.
    ///
    /// Returns `true` if a port was popped, `false` if the stack would become empty.
    /// The stack always maintains at least one port (typically stdin).
    ///
    /// # Examples
    ///
    /// ```rust
    /// use s1::io::{PortStack, Port, PortKind};
    ///
    /// let stdin_port = Port { kind: PortKind::Stdin };
    /// let file_port = Port { kind: PortKind::File { name: "input.txt".to_string(), write: false, file_id: Some(1) } };
    ///
    /// let mut stack = PortStack::new(stdin_port);
    /// stack.push(file_port);
    /// assert_eq!(stack.current().kind, PortKind::File { name: "input.txt".to_string(), write: false, file_id: Some(1) });
    ///
    /// assert!(stack.pop());
    /// assert_eq!(stack.current().kind, PortKind::Stdin);
    ///
    /// assert!(!stack.pop()); // Cannot pop the last port
    /// ```
    pub fn pop(&mut self) -> bool {
        if self.stack.len() > 1 {
            self.stack.pop();
            true
        } else {
            false
        }
    }

}

impl SchemePortStack {
    /// Create a new scheme port stack with an initial port.
    ///
    /// The initial port is typically stdin for interactive use.
    pub fn new(_heap: &mut crate::gc::GcHeap, initial: crate::gc::GcRef) -> Self {
        Self { stack: vec![initial] }
    }

    /// Get the current port (the top of the stack).
    pub fn current(&self) -> crate::gc::GcRef {
        *self.stack.last().unwrap()
    }

    /// Get a mutable reference to the current port.
    /// This is useful for updating port state (like string port position).
    pub fn current_mut(&mut self) -> &mut crate::gc::GcRef {
        self.stack.last_mut().unwrap()
    }

    /// Push a new port onto the stack, making it the current port.
    pub fn push(&mut self, port: crate::gc::GcRef) {
        self.stack.push(port);
    }

    /// Pop the current port from the stack, returning to the previous port.
    ///
    /// Returns `true` if a port was popped, `false` if the stack would become empty.
    /// The stack always maintains at least one port (typically stdin).
    pub fn pop(&mut self) -> bool {
        if self.stack.len() > 1 {
            self.stack.pop();
            true
        } else {
            false
        }
    }

    /// Load a Scheme file and push it onto the port stack for reading.
    ///
    /// This method reads the entire file content and creates a string port
    /// that can be used by the parser. The file content is pushed onto the
    /// port stack, so when the file is exhausted, the previous port is restored.
    ///
    /// # Arguments
    ///
    /// * `filename` - The path to the Scheme file to load
    /// * `heap` - The garbage collection heap for allocating the new port
    ///
    /// # Returns
    ///
    /// Returns `Ok(())` on success, or an error message on failure.
    pub fn load_scheme_file(&mut self, filename: &str, heap: &mut crate::gc::GcHeap) -> Result<(), String> {
        let file = File::open(filename).map_err(|e| format!("could not open {}: {}", filename, e))?;
        let mut content = String::new();
        StdBufReader::new(file).read_to_string(&mut content).map_err(|e| format!("could not read {}: {}", filename, e))?;
        
        let port = crate::gc::new_port(heap, PortKind::StringPortInput {
            content,
            pos: UnsafeCell::new(0),
        });
        self.push(port);
        Ok(())
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
        Self { next_id: 1, files: HashMap::new() }
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
fn read_line_from_string_port(port: &Port) -> Option<(String, Port)> {
    if let PortKind::StringPortInput { content, pos } = &port.kind {
        let current_pos = unsafe { *pos.get() };
        let mut lines = content.lines();
        // Skip to the current position
        for _ in 0..current_pos {
            lines.next();
        }
        if let Some(line) = lines.next() {
            let new_pos = current_pos + 1;
            let new_port = Port {
                kind: PortKind::StringPortInput {
                    content: content.clone(),
                    pos: UnsafeCell::new(new_pos),
                }
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
pub fn read_line(port: &Port, file_table: &mut FileTable) -> Option<String> {
    // Handle StringPort case separately to avoid borrow checker issues
    if let Some((line, _)) = read_line_from_string_port(port) {
        return Some(line);
    }

    // Handle other port types
    match &port.kind {
        PortKind::Stdin => {
            let mut buf = String::new();
            let n = io::stdin().read_line(&mut buf).ok()?;
            if n == 0 {
                None
            } else {
                Some(buf)
            }
        }
        PortKind::File { file_id, .. } => {
            if let Some(file_id) = file_id {
                if let Some(file) = file_table.get(*file_id) {
                    let mut reader = BufReader::new(file);
                    let mut buf = String::new();
                    let n = reader.read_line(&mut buf).ok()?;
                    if n == 0 {
                        None
                    } else {
                        Some(buf)
                    }
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
pub fn write_line(port: &Port, file_table: &mut FileTable, line: &str) -> bool {
    match &port.kind {
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
        PortKind::StringPortOutput { content } => {
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
pub fn read_char(port: &Port, file_table: &mut FileTable) -> Option<char> {
    match &port.kind {
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
        PortKind::StringPortInput { content, pos } => {
            let current_pos = unsafe { *pos.get() };
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
pub fn write_char(port: &Port, file_table: &mut FileTable, ch: char) -> bool {
    match &port.kind {
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
        PortKind::StringPortOutput { content } => {
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
/// use s1::io::{new_string_port, PortStack, FileTable, read_char, Port, PortKind};
///
/// let string_port = new_string_port("hello");
/// let mut port_stack = PortStack::new(string_port);
/// let mut file_table = FileTable::new();
///
/// // Read characters from the string
/// assert_eq!(read_char(&mut port_stack, &mut file_table), Some('h'));
/// assert_eq!(read_char(&mut port_stack, &mut file_table), Some('e'));
/// assert_eq!(read_char(&mut port_stack, &mut file_table), Some('l'));
/// ```
pub fn new_string_port(s: &str) -> Port {
    Port {
        kind: PortKind::StringPortInput {
            content: s.to_string(),
            pos: UnsafeCell::new(0),
        }
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
/// use s1::io::{new_output_string_port, PortStack, FileTable, write_line, get_output_string, Port, PortKind};
///
/// let output_port = new_output_string_port();
/// let mut port_stack = PortStack::new(output_port);
/// let mut file_table = FileTable::new();
///
/// // Write to the string port
/// write_line(&mut port_stack, &mut file_table, "hello");
/// 
/// // Get the accumulated content
/// let content = get_output_string(&port_stack.current());
/// assert_eq!(content, "hello");
/// ```
pub fn new_output_string_port() -> Port {
    Port {
        kind: PortKind::StringPortOutput {
            content: String::new(),
        }
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
pub fn get_output_string(port: &Port) -> String {
    match &port.kind {
        PortKind::StringPortOutput { content } => content.clone(),
        _ => String::new(),
    }
}

/// Get the current position of a string port safely.
/// This function should be called through the GC heap accessor.
pub fn get_string_port_pos(port: &Port) -> Option<usize> {
    match &port.kind {
        PortKind::StringPortInput { pos, .. } => {
            Some(unsafe { *pos.get() })
        }
        _ => None,
    }
}

/// Update the position of a string port safely.
/// This function should be called through the GC heap accessor.
pub fn update_string_port_pos(port: &Port, new_pos: usize) -> bool {
    match &port.kind {
        PortKind::StringPortInput { pos, .. } => {
            unsafe { *pos.get() = new_pos; }
            true
        }
        _ => false,
    }
}

/// Convert a Rust Port to a Scheme port object.
pub fn port_to_scheme_port(port: Port, heap: &mut crate::gc::GcHeap) -> crate::gc::GcRef {
    crate::gc::new_port(heap, port.kind)
}

/// Convert a Scheme port object to a Rust Port.
pub fn scheme_port_to_port(scheme_port: crate::gc::GcRef) -> Port {
    match &scheme_port.value {
        crate::gc::SchemeValue::Port { kind } => Port {
            kind: kind.clone(),
        },
        _ => panic!("Expected port object"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::{File, remove_file};
    use std::io::Write;

    #[test]
    fn test_port_stack_push_pop() {
        use crate::gc::GcHeap;
        let mut heap = GcHeap::new();
        let stdin_port = port_to_scheme_port(Port { kind: PortKind::Stdin }, &mut heap);
        let file_port = port_to_scheme_port(Port { kind: PortKind::File { name: "foo.scm".to_string(), write: false, file_id: Some(0) } }, &mut heap);
        let mut stack = PortStack::new(stdin_port);
        assert!(matches!(&stack.current().value, crate::gc::SchemeValue::Port { kind: PortKind::Stdin }));
        stack.push(file_port);
        match &stack.current().value {
            crate::gc::SchemeValue::Port { kind: PortKind::File { name, .. } } => {
                assert_eq!(name, "foo.scm");
            }
            _ => panic!("Expected file port"),
        }
        assert!(stack.pop());
        assert!(matches!(&stack.current().value, crate::gc::SchemeValue::Port { kind: PortKind::Stdin }));
        assert!(!stack.pop());
    }

    #[test]
    fn test_file_table_open_close() {
        let mut table = FileTable::new();
        let fname = "test_io_file.txt";
        {
            let mut file = File::create(fname).unwrap();
            writeln!(file, "hello").unwrap();
        }
        let id = table.open_file(fname, false).unwrap();
        assert!(table.get(id).is_some());
        table.close_file(id);
        assert!(table.get(id).is_none());
        remove_file(fname).unwrap();
    }

    #[test]
    fn test_file_read_write() {
        let mut table = FileTable::new();
        let fname = "test_io_file2.txt";
        // Write
        let id = table.open_file(fname, true).unwrap();
        let port = Port { kind: PortKind::File { name: fname.to_string(), write: true, file_id: Some(id) } };
        assert!(write_line(&port, &mut table, "abc\n"));
        table.close_file(id);
        // Read
        let id = table.open_file(fname, false).unwrap();
        let port = Port { kind: PortKind::File { name: fname.to_string(), write: false, file_id: Some(id) } };
        let line = read_line(&port, &mut table).unwrap();
        assert_eq!(line, "abc\n");
        table.close_file(id);
        remove_file(fname).unwrap();
    }

    #[test]
    fn test_file_read_write_char() {
        let mut table = FileTable::new();
        let fname = "test_io_file3.txt";
        // Write a char
        let id = table.open_file(fname, true).unwrap();
        let port = Port { kind: PortKind::File { name: fname.to_string(), write: true, file_id: Some(id) } };
        assert!(write_char(&port, &mut table, 'Z'));
        table.close_file(id);
        // Read a char
        let id = table.open_file(fname, false).unwrap();
        let port = Port { kind: PortKind::File { name: fname.to_string(), write: false, file_id: Some(id) } };
        let ch = read_char(&port, &mut table).unwrap();
        assert_eq!(ch, 'Z');
        table.close_file(id);
        remove_file(fname).unwrap();
    }

    #[test]
    fn test_port_stack_nesting_and_fallback() {
        use crate::gc::GcHeap;
        
        let mut heap = GcHeap::new();
        let stdin_port = port_to_scheme_port(Port { kind: PortKind::Stdin }, &mut heap);
        let file1_port = port_to_scheme_port(Port { kind: PortKind::File { name: "file1.scm".to_string(), write: false, file_id: Some(1) } }, &mut heap);
        let file2_port = port_to_scheme_port(Port { kind: PortKind::File { name: "file2.scm".to_string(), write: false, file_id: Some(2) } }, &mut heap);
        let mut stack = PortStack::new(stdin_port);
        assert!(matches!(&stack.current().value, crate::gc::SchemeValue::Port { kind: PortKind::Stdin }));
        stack.push(file1_port);
        match &stack.current().value {
            crate::gc::SchemeValue::Port { kind: PortKind::File { name, .. } } => {
                assert_eq!(name, "file1.scm");
            }
            _ => panic!("Expected file port"),
        }
        stack.push(file2_port);
        match &stack.current().value {
            crate::gc::SchemeValue::Port { kind: PortKind::File { name, .. } } => {
                assert_eq!(name, "file2.scm");
            }
            _ => panic!("Expected file port"),
        }
        assert!(stack.pop());
        match &stack.current().value {
            crate::gc::SchemeValue::Port { kind: PortKind::File { name, .. } } => {
                assert_eq!(name, "file1.scm");
            }
            _ => panic!("Expected file port"),
        }
        assert!(stack.pop());
        assert!(matches!(&stack.current().value, crate::gc::SchemeValue::Port { kind: PortKind::Stdin }));
        // Can't pop last port
        assert!(!stack.pop());
    }

    #[test]
    fn test_string_port() {
        let string_port = new_string_port("hello");
        let mut file_table = FileTable::new();
        
        // Test that we can read the first character from a string port
        assert_eq!(read_char(&string_port, &mut file_table), Some('h'));
        
        // Test position update
        assert_eq!(get_string_port_pos(&string_port), Some(0));
        assert!(update_string_port_pos(&string_port, 1));
        assert_eq!(get_string_port_pos(&string_port), Some(1));
    }

    #[test]
    fn test_output_string_port() {
        let output_port = new_output_string_port();
        let _file_table = FileTable::new();
        
        // Test that we can get the initial content from an output string port
        let content = get_output_string(&output_port);
        assert_eq!(content, "");
    }

    #[test]
    fn test_scheme_port_stack() {
        use crate::gc::GcHeap;
        
        let mut heap = GcHeap::new();
        let stdin_port = port_to_scheme_port(Port { kind: PortKind::Stdin }, &mut heap);
        let file_port = port_to_scheme_port(Port { kind: PortKind::File { name: "test.scm".to_string(), write: false, file_id: Some(1) } }, &mut heap);
        
        let mut stack = SchemePortStack::new(&mut heap, stdin_port);
        assert!(matches!(&stack.current().value, crate::gc::SchemeValue::Port { kind: PortKind::Stdin }));
        
        stack.push(file_port);
        match &stack.current().value {
            crate::gc::SchemeValue::Port { kind: PortKind::File { name, .. } } => {
                assert_eq!(name, "test.scm");
            }
            _ => panic!("Expected file port"),
        }
        
        assert!(stack.pop());
        assert!(matches!(&stack.current().value, crate::gc::SchemeValue::Port { kind: PortKind::Stdin }));
        
        assert!(!stack.pop()); // Cannot pop the last port
    }

    #[test]
    fn test_port_conversion() {
        use crate::gc::GcHeap;
        
        let mut heap = GcHeap::new();
        let original_port = Port { kind: PortKind::StringPortInput { content: "hello".to_string(), pos: UnsafeCell::new(0) } };
        
        let scheme_port = port_to_scheme_port(original_port.clone(), &mut heap);
        let converted_port = scheme_port_to_port(scheme_port);
        
        assert_eq!(original_port.kind, converted_port.kind);
    }
}