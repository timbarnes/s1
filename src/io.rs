use crate::gc::{GcHeap, GcRef, SchemeValue, PortKind, new_port};
use std::cell::RefCell;
use std::io::{self, Write, Read, BufRead, BufReader, BufWriter};
use std::collections::HashMap;
use std::fs::File;

/// Manages a stack of ports for input/output operations.
pub struct PortStack {
    stack: Vec<GcRef>,
}

impl PortStack {
    pub fn new(initial: GcRef) -> Self {
        Self { stack: vec![initial] }
    }
    pub fn current(&self) -> GcRef {
        self.stack.last().unwrap().clone()
    }
    pub fn push(&mut self, port: GcRef) {
        self.stack.push(port);
    }
    pub fn pop(&mut self) -> bool {
        if self.stack.len() > 1 {
            self.stack.pop();
            true
        } else {
            false
        }
    }
}

/// Manages open file handles for file ports.
pub struct FileTable {
    next_id: usize,
    files: HashMap<usize, File>,
}

impl FileTable {
    pub fn new() -> Self {
        Self { next_id: 1, files: HashMap::new() }
    }
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
    pub fn close_file(&mut self, id: usize) {
        self.files.remove(&id);
    }
    pub fn get(&mut self, id: usize) -> Option<&mut File> {
        self.files.get_mut(&id)
    }
}

/// Read a line from the current input port.
pub fn read_line(heap: &mut GcHeap, port_stack: &mut PortStack, file_table: &mut FileTable) -> Option<String> {
    let port = port_stack.current();
    match &port.borrow().value {
        SchemeValue::Port(p) => match &p.kind {
            PortKind::Stdin => {
                let mut buf = String::new();
                let n = io::stdin().read_line(&mut buf).ok()?;
                if n == 0 {
                    if port_stack.pop() {
                        return read_line(heap, port_stack, file_table);
                    } else {
                        return None;
                    }
                }
                Some(buf)
            }
            PortKind::File { .. } => {
                if let Some(file_id) = p.file_id {
                    if let Some(file) = file_table.get(file_id) {
                        let mut reader = BufReader::new(file);
                        let mut buf = String::new();
                        let n = reader.read_line(&mut buf).ok()?;
                        if n == 0 {
                            port_stack.pop();
                            return read_line(heap, port_stack, file_table);
                        }
                        Some(buf)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        },
        _ => None,
    }
}

/// Write a line to the current output port.
pub fn write_line(heap: &mut GcHeap, port_stack: &mut PortStack, file_table: &mut FileTable, line: &str) -> bool {
    let port = port_stack.current();
    match &port.borrow().value {
        SchemeValue::Port(p) => match &p.kind {
            PortKind::Stdout => {
                print!("{}", line);
                io::stdout().flush().ok();
                true
            }
            PortKind::File { .. } => {
                if let Some(file_id) = p.file_id {
                    if let Some(file) = file_table.get(file_id) {
                        let mut writer = BufWriter::new(file);
                        writer.write_all(line.as_bytes()).is_ok()
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            _ => false,
        },
        _ => false,
    }
}

/// Read a single character from the current input port.
pub fn read_char(heap: &mut GcHeap, port_stack: &mut PortStack, file_table: &mut FileTable) -> Option<char> {
    let port = port_stack.current();
    match &port.borrow().value {
        SchemeValue::Port(p) => match &p.kind {
            PortKind::Stdin => {
                let mut buf = [0u8; 1];
                match io::stdin().read_exact(&mut buf) {
                    Ok(_) => Some(buf[0] as char),
                    Err(_) => {
                        if port_stack.pop() {
                            return read_char(heap, port_stack, file_table);
                        } else {
                            return None;
                        }
                    }
                }
            }
            PortKind::File { .. } => {
                if let Some(file_id) = p.file_id {
                    if let Some(file) = file_table.get(file_id) {
                        let mut buf = [0u8; 1];
                        match file.read_exact(&mut buf) {
                            Ok(_) => Some(buf[0] as char),
                            Err(_) => {
                                port_stack.pop();
                                read_char(heap, port_stack, file_table)
                            }
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        },
        _ => None,
    }
}

/// Write a single character to the current output port.
pub fn write_char(heap: &mut GcHeap, port_stack: &mut PortStack, file_table: &mut FileTable, ch: char) -> bool {
    let port = port_stack.current();
    match &port.borrow().value {
        SchemeValue::Port(p) => match &p.kind {
            PortKind::Stdout => {
                print!("{}", ch);
                io::stdout().flush().ok();
                true
            }
            PortKind::File { .. } => {
                if let Some(file_id) = p.file_id {
                    if let Some(file) = file_table.get(file_id) {
                        let mut writer = BufWriter::new(file);
                        writer.write_all(&[ch as u8]).is_ok()
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            _ => false,
        },
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gc::{PortKind, new_port};
    use std::fs::{File, remove_file};
    use std::io::Write;

    #[test]
    fn test_port_stack_push_pop() {
        let mut heap = GcHeap::new();
        let stdin_port = new_port(&mut heap, PortKind::Stdin, None);
        let file_port = new_port(&mut heap, PortKind::File { name: "foo.scm".to_string(), write: false }, Some(0));
        let mut stack = PortStack::new(stdin_port.clone());
        assert_eq!(stack.current().borrow().value, stdin_port.borrow().value);
        stack.push(file_port.clone());
        assert_eq!(stack.current().borrow().value, file_port.borrow().value);
        assert!(stack.pop());
        assert_eq!(stack.current().borrow().value, stdin_port.borrow().value);
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
        let mut heap = GcHeap::new();
        let mut table = FileTable::new();
        let fname = "test_io_file2.txt";
        // Write
        let id = table.open_file(fname, true).unwrap();
        let port = new_port(&mut heap, PortKind::File { name: fname.to_string(), write: true }, Some(id));
        let mut stack = PortStack::new(port.clone());
        assert!(write_line(&mut heap, &mut stack, &mut table, "abc\n"));
        table.close_file(id);
        // Read
        let id = table.open_file(fname, false).unwrap();
        let port = new_port(&mut heap, PortKind::File { name: fname.to_string(), write: false }, Some(id));
        let mut stack = PortStack::new(port.clone());
        let line = read_line(&mut heap, &mut stack, &mut table).unwrap();
        assert_eq!(line, "abc\n");
        table.close_file(id);
        remove_file(fname).unwrap();
    }

    #[test]
    fn test_file_read_write_char() {
        let mut heap = GcHeap::new();
        let mut table = FileTable::new();
        let fname = "test_io_file3.txt";
        // Write a char
        let id = table.open_file(fname, true).unwrap();
        let port = new_port(&mut heap, PortKind::File { name: fname.to_string(), write: true }, Some(id));
        let mut stack = PortStack::new(port.clone());
        assert!(write_char(&mut heap, &mut stack, &mut table, 'Z'));
        table.close_file(id);
        // Read a char
        let id = table.open_file(fname, false).unwrap();
        let port = new_port(&mut heap, PortKind::File { name: fname.to_string(), write: false }, Some(id));
        let mut stack = PortStack::new(port.clone());
        let ch = read_char(&mut heap, &mut stack, &mut table).unwrap();
        assert_eq!(ch, 'Z');
        table.close_file(id);
        remove_file(fname).unwrap();
    }

    #[test]
    fn test_port_stack_nesting_and_fallback() {
        let mut heap = GcHeap::new();
        let stdin_port = new_port(&mut heap, PortKind::Stdin, None);
        let file1_port = new_port(&mut heap, PortKind::File { name: "file1.scm".to_string(), write: false }, Some(1));
        let file2_port = new_port(&mut heap, PortKind::File { name: "file2.scm".to_string(), write: false }, Some(2));
        let mut stack = PortStack::new(stdin_port.clone());
        assert_eq!(stack.current().borrow().value, stdin_port.borrow().value);
        stack.push(file1_port.clone());
        assert_eq!(stack.current().borrow().value, file1_port.borrow().value);
        stack.push(file2_port.clone());
        assert_eq!(stack.current().borrow().value, file2_port.borrow().value);
        assert!(stack.pop());
        assert_eq!(stack.current().borrow().value, file1_port.borrow().value);
        assert!(stack.pop());
        assert_eq!(stack.current().borrow().value, stdin_port.borrow().value);
        // Can't pop last port
        assert!(!stack.pop());
    }
} 