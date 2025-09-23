use crate::env::{EnvOps, EnvRef};
use crate::gc::{GcHeap, GcRef, SchemeValue, new_bool};
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
        "eof-object?" => eof_object_q,
        "input-port?" => input_port_q,
        "output-port?" => output_port_q,
        "flush-output" => flush_output,
    );
}

fn output_port_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("output-port?: expected 1 argument".to_string());
    }
    let is_output_port = match &heap.get_value(args[0]) {
        SchemeValue::Port(kind) => match kind {
            crate::io::PortKind::Stdout
            | crate::io::PortKind::Stderr
            | crate::io::PortKind::StringPortOutput { .. } => true,
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

/// (flush-output [port]) -> #<void>
/// TODO: Implement flush-output for other ports than stdout
fn flush_output(heap: &mut GcHeap, _args: &[GcRef]) -> Result<GcRef, String> {
    
    std::io::stdout().flush().ok();
    Ok(heap.void())
}
