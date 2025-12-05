/// Display output functions
///
///
use crate::env::{EnvOps, EnvRef};
use crate::gc::{GcHeap, GcRef};
use crate::io;
use crate::printer::{display_value, print_value};
use crate::register_builtin_family;
use std::io::Write;

pub fn register_display_builtins(heap: &mut GcHeap, env: EnvRef) {
    register_builtin_family!(heap, env,
        "write" => (write, "(write value [port]) write a SchemeValue in Scheme-readable format"),
        "display" => (display, "(display value [port]) print a SchemeValue in human-readable format"),
        "newline" => (newline, "(newline [port]) print a newline character"),
    );
}

/// write a SchemeValue in Scheme-readable format
///
pub fn write(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 1 || args.len() > 2 {
        return Err("write: expected 1 or 2 arguments".to_string());
    }
    let s = print_value(&args[0]);

    if args.len() == 2 {
        let port_ref = args[1];
        let port_kind = heap.get_port_mut(port_ref);
        match port_kind {
            io::PortKind::StringPortOutput { content } => {
                content.push_str(&s);
            }
            _ => {
                print!("{}", s);
                std::io::stdout().flush().unwrap();
            }
        }
    } else {
        print!("{}", s);
        std::io::stdout().flush().unwrap();
    }
    Ok(heap.nil_s())
}

/// print a SchemeValue in human-readable format
///
pub fn display(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 1 || args.len() > 2 {
        return Err("display: expected 1 or 2 arguments".to_string());
    }
    let s = display_value(&args[0]);

    if args.len() == 2 {
        let port_ref = args[1];
        let port_kind = heap.get_port_mut(port_ref);
        match port_kind {
            io::PortKind::StringPortOutput { content } => {
                content.push_str(&s);
            }
            _ => {
                print!("{}", s);
                std::io::stdout().flush().unwrap();
            }
        }
    } else {
        print!("{}", s);
        std::io::stdout().flush().unwrap();
    }

    Ok(heap.nil_s())
}

pub fn newline(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() > 1 {
        return Err("newline: expected 0 or 1 arguments".to_string());
    }

    if args.len() == 1 {
        let port_ref = args[0];
        let port_kind = heap.get_port_mut(port_ref);
        match port_kind {
            io::PortKind::StringPortOutput { content } => {
                content.push('\n');
            }
            _ => {
                println!();
            }
        }
    } else {
        println!();
    }
    Ok(heap.nil_s())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::{RunTime, RunTimeStruct};
    use crate::gc::{new_int, new_port, new_string};
    use crate::io::{get_output_string, new_output_string_port};
    use num_bigint::BigInt;
    // use std::fs::File; // No longer needed
    // use std::io::{self, Read, Seek, SeekFrom}; // No longer needed
    // use std::os::unix::io::AsRawFd; // No longer needed
    // use tempfile::tempfile; // No longer needed
    // use libc;

    // The StdoutRedirect struct and associated test_output function are no longer needed
    // as the built-in functions now write to an in-memory string port during tests.

    #[test]
    fn test_write_builtin() {
        let mut ev = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut ev);
        let heap = &mut ec.heap;

        // Test with string
        let mut port_kind = new_output_string_port();
        let port = new_port(heap, port_kind.clone());
        let val = new_string(heap, "hello");
        write(heap, &[val, port]).unwrap();
        port_kind = heap.get_port_mut(port).clone();
        assert_eq!(get_output_string(&mut port_kind), "\"hello\"");

        // Test with integer
        let mut port_kind_int = new_output_string_port();
        let port_int = new_port(heap, port_kind_int.clone());
        let val_int = new_int(heap, BigInt::from(123));
        write(heap, &[val_int, port_int]).unwrap();
        port_kind_int = heap.get_port_mut(port_int).clone();
        assert_eq!(get_output_string(&mut port_kind_int), "123");
    }

    #[test]
    fn test_display_builtin() {
        let mut ev = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut ev);
        let heap = &mut ec.heap;

        // Test with string
        let mut port_kind = new_output_string_port();
        let port = new_port(heap, port_kind.clone());
        let val = new_string(heap, "hello");
        display(heap, &[val, port]).unwrap();
        port_kind = heap.get_port_mut(port).clone();
        assert_eq!(get_output_string(&mut port_kind), "hello");

        // Test with integer
        let mut port_kind_int = new_output_string_port();
        let port_int = new_port(heap, port_kind_int.clone());
        let val_int = new_int(heap, BigInt::from(123));
        display(heap, &[val_int, port_int]).unwrap();
        port_kind_int = heap.get_port_mut(port_int).clone();
        assert_eq!(get_output_string(&mut port_kind_int), "123");
    }

    #[test]
    fn test_newline_builtin() {
        let mut ev = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut ev);
        let heap = &mut ec.heap;

        let mut port_kind = new_output_string_port();
        let port = new_port(heap, port_kind.clone());
        newline(heap, &[port]).unwrap();
        port_kind = heap.get_port_mut(port).clone();
        assert_eq!(get_output_string(&mut port_kind), "\n");
    }
}
