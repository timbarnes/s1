//! Ports and input/output procedures (R7RS 6.13).
//!
//! A port is a heap object holding a `PortKind`; every procedure here works
//! on the port in place (`port_mut`), so all references to a port share its
//! position, accumulated output and open/closed state. The current input,
//! output and error ports live in `RunTime::current_ports`; the procedures
//! that return them also accept the parameter protocol `parameterize` uses
//! (see `make-parameter` in s1-core.scm), so they can be parameterized.

use crate::env::{EnvOps, EnvRef};
use crate::eval::{CEKState, Control, KontRef, RunTime};
use crate::gc::{
    ErrorKind, GcHeap, GcRef, SchemeValue, list_from_slice, new_bool, new_char, new_port, new_string,
    new_sys_builtin,
};
use crate::io::PortKind;
use crate::parser::{ParseError, parse};
use crate::printer::{display_value, print_value};
use crate::{gc_value, gc_value_mut, register_builtin_family, register_sys_builtins};
use std::io::Write;

/// Indexes into `RunTime::current_ports`.
pub const INPUT: usize = 0;
pub const OUTPUT: usize = 1;
pub const ERROR: usize = 2;

pub fn register_port_builtins(rt: &mut RunTime, env: EnvRef) {
    register_sys_builtins!(rt, env,
        "current-input-port" => current_input_port_sp,
        "current-output-port" => current_output_port_sp,
        "current-error-port" => current_error_port_sp,
        "open-input-file" => open_input_file_sp,
        "open-binary-input-file" => open_binary_input_file_sp,
        "open-output-file" => open_output_file_sp,
        "open-binary-output-file" => open_binary_output_file_sp,
        "close-port" => close_port_sp,
        "close-input-port" => close_input_port_sp,
        "close-output-port" => close_output_port_sp,
        "delete-file" => delete_file_sp,
        "read" => read_sp,
        "read-char" => read_char_sp,
        "peek-char" => peek_char_sp,
        "read-line" => read_line_sp,
        "read-string" => read_string_sp,
        "char-ready?" => char_ready_sp,
        "write" => write_sp,
        "write-shared" => write_shared_sp,
        "write-simple" => write_simple_sp,
        "display" => display_sp,
        "newline" => newline_sp,
        "write-char" => write_char_sp,
        "write-string" => write_string_sp,
        "write-u8" => write_u8_sp,
        "write-bytevector" => write_bytevector_sp,
        "flush-output-port" => flush_output_port_sp,
        "flush-output" => flush_output_port_sp,
        "push-port!" => push_port_sp,
        "pop-port!" => pop_port_sp,
    );
    register_builtin_family!(rt.heap, env,
        "port?" => (port_q, "(port? obj) Returns #t if obj is a port"),
        "input-port?" => (input_port_q, "(input-port? obj) Returns #t if obj is an input port"),
        "output-port?" => (output_port_q, "(output-port? obj) Returns #t if obj is an output port"),
        "textual-port?" => (textual_port_q, "(textual-port? obj) Returns #t if obj is a textual port"),
        "binary-port?" => (binary_port_q, "(binary-port? obj) Returns #t if obj is a binary port"),
        "input-port-open?" => (input_port_open_q, "(input-port-open? port) Returns #t if port is an open input port"),
        "output-port-open?" => (output_port_open_q, "(output-port-open? port) Returns #t if port is an open output port"),
        "eof-object" => (eof_object, "(eof-object) Returns the end-of-file object"),
        "eof-object?" => (eof_object_q, "(eof-object? obj) Returns #t if obj is the end-of-file object"),
        "open-input-string" => (open_input_string, "(open-input-string string) Returns a textual input port reading string"),
        "open-output-string" => (open_output_string, "(open-output-string) Returns a textual output port that accumulates a string"),
        "get-output-string" => (get_output_string, "(get-output-string port) Returns the string written to an open-output-string port so far"),
        "open-input-bytevector" => (open_input_bytevector, "(open-input-bytevector bytevector) Returns a binary input port reading the bytes"),
        "open-output-bytevector" => (open_output_bytevector, "(open-output-bytevector) Returns a binary output port that accumulates bytes"),
        "get-output-bytevector" => (get_output_bytevector, "(get-output-bytevector port) Returns the bytes written to an open-output-bytevector port so far"),
        "read-u8" => (read_u8, "(read-u8 port) Returns the next byte from a binary input port, or the eof object"),
        "peek-u8" => (peek_u8, "(peek-u8 port) Returns the next byte without consuming it, or the eof object"),
        "u8-ready?" => (u8_ready, "(u8-ready? port) Returns #t if a byte can be read without waiting"),
        "read-bytevector" => (read_bytevector, "(read-bytevector k port) Returns up to k bytes as a bytevector, or the eof object"),
        "read-bytevector!" => (read_bytevector_into, "(read-bytevector! bv port [start [end]]) Reads bytes into bv; returns the count, or the eof object"),
        "file-exists?" => (file_exists_q, "(file-exists? filename) Returns #t if the file exists"),
        "%check-input-port" => (check_input_port, "(%check-input-port obj) Internal: current-input-port's parameter converter"),
        "%check-output-port" => (check_output_port, "(%check-output-port obj) Internal: the output port parameters' converter"),
    );
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Finish a sys-builtin with `value` as its result.
fn done(state: &mut CEKState, value: GcRef, next: KontRef) -> Result<(), String> {
    state.control = Control::Value(value);
    state.kont = next;
    Ok(())
}

fn arity(args: &[GcRef], min: usize, max: usize, who: &str) -> Result<(), String> {
    if args.len() < min || args.len() > max {
        Err(format!("{}: wrong number of arguments ({})", who, args.len()))
    } else {
        Ok(())
    }
}

/// The port `v`, to read or modify in place.
pub fn port_mut(v: GcRef, who: &str) -> Result<&'static mut PortKind, String> {
    match gc_value_mut!(v) {
        SchemeValue::Port(kind) => Ok(&mut **kind),
        _ => Err(format!("{}: expected a port, got {}", who, print_value(&v))),
    }
}

fn port_ref(v: GcRef) -> Option<&'static PortKind> {
    match gc_value!(v) {
        SchemeValue::Port(kind) => Some(&**kind),
        _ => None,
    }
}

/// The port argument at `args[i]`, or the current port `which`.
fn port_or_current(rt: &RunTime, args: &[GcRef], i: usize, which: usize) -> GcRef {
    args.get(i).copied().unwrap_or(rt.current_ports[which])
}

fn closed_error(who: &str) -> String {
    format!("{}: the port is closed", who)
}

/// Write text to a textual output port.
pub fn put_str(rt: &mut RunTime, port: GcRef, text: &str, who: &str) -> Result<(), String> {
    match port_mut(port, who)? {
        PortKind::Stdout => print!("{}", text),
        PortKind::Stderr => {
            // Keep stderr output in order with buffered stdout output.
            std::io::stdout().flush().ok();
            eprint!("{}", text);
        }
        PortKind::StringPortOutput { content } => content.push_str(text),
        PortKind::FileOutput { id, binary: false, name } => {
            let file = rt.file_table.get(*id).ok_or_else(|| format!("{}: file {} is not open", who, name))?;
            file.write_all(text.as_bytes())
                .map_err(|e| format!("{}: could not write to {}: {}", who, name, e))?;
        }
        PortKind::Closed { .. } => return Err(closed_error(who)),
        _ => return Err(format!("{}: not a textual output port", who)),
    }
    Ok(())
}

/// Read (or, with `peek`, look at) the next character of a textual input
/// port; `None` at end of file.
fn get_char(port: GcRef, who: &str, peek: bool) -> Result<Option<char>, String> {
    let kind = port_mut(port, who)?;
    match kind {
        PortKind::Stdin | PortKind::StringPortInput { .. } => {
            let c = kind.next_char_utf8();
            if peek {
                if let Some(c) = c {
                    kind.unread_char(c);
                }
            }
            Ok(c)
        }
        PortKind::Closed { .. } => Err(closed_error(who)),
        _ => Err(format!("{}: not a textual input port", who)),
    }
}

fn char_or_eof(heap: &mut GcHeap, c: Option<char>) -> GcRef {
    match c {
        Some(c) => new_char(heap, c),
        None => heap.eof(),
    }
}

/// Raise a `file-error?` error object naming the file as its irritant.
pub fn raise_file_error(rt: &mut RunTime, state: &mut CEKState, msg: &str, filename: GcRef) -> Result<(), String> {
    let irritants = list_from_slice(&[filename], rt.heap);
    crate::eval::exceptions::raise_error(state, rt, ErrorKind::File, msg, irritants);
    Ok(())
}

fn string_arg(v: GcRef, who: &str) -> Result<&'static String, String> {
    match gc_value!(v) {
        SchemeValue::Str(s) => Ok(s),
        _ => Err(format!("{}: expected a string, got {}", who, print_value(&v))),
    }
}

// ---------------------------------------------------------------------------
// Current ports
// ---------------------------------------------------------------------------

/// `(current-*-port)` returns the port; `parameterize` also calls it with
/// `%param-converter` (to get the converter) and `%param-set` (to install a
/// value), the protocol `make-parameter`'s objects follow.
fn current_port(
    rt: &mut RunTime,
    args: &[GcRef],
    state: &mut CEKState,
    next: KontRef,
    which: usize,
    checker: &str,
) -> Result<(), String> {
    let set = rt.heap.intern_symbol("%param-set");
    let converter = rt.heap.intern_symbol("%param-converter");
    let value = match args {
        [] => rt.current_ports[which],
        [m] if *m == converter => {
            let name = rt.heap.intern_symbol(checker);
            global_value(state, name).ok_or_else(|| format!("{} is not defined", checker))?
        }
        [m, port] if *m == set => {
            rt.current_ports[which] = *port;
            rt.heap.unspecified()
        }
        _ => return Err("current port: expects no arguments".to_string()),
    };
    done(state, value, next)
}

fn global_value(state: &CEKState, sym: GcRef) -> Option<GcRef> {
    let mut env = state.env.clone();
    while let Some(parent) = env.parent() {
        env = parent;
    }
    env.lookup_local(sym)
}

fn current_input_port_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    current_port(rt, args, state, next, INPUT, "%check-input-port")
}

fn current_output_port_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    current_port(rt, args, state, next, OUTPUT, "%check-output-port")
}

fn current_error_port_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    current_port(rt, args, state, next, ERROR, "%check-output-port")
}

fn check_input_port(_heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    match args {
        [p] if port_ref(*p).is_some_and(PortKind::is_input) => Ok(*p),
        _ => Err("current-input-port: the value must be an input port".to_string()),
    }
}

fn check_output_port(_heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    match args {
        [p] if port_ref(*p).is_some_and(PortKind::is_output) => Ok(*p),
        _ => Err("current output port: the value must be an output port".to_string()),
    }
}

// ---------------------------------------------------------------------------
// Opening and closing
// ---------------------------------------------------------------------------

fn open_input(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef, binary: bool) -> Result<(), String> {
    let who = if binary { "open-binary-input-file" } else { "open-input-file" };
    arity(args, 1, 1, who)?;
    let name = string_arg(args[0], who)?;
    let kind = if binary {
        match std::fs::read(name) {
            Ok(bytes) => PortKind::BytevectorInput { bytes, pos: 0 },
            Err(e) => return raise_file_error(rt, state, &format!("{}: could not open file: {}", who, e), args[0]),
        }
    } else {
        match std::fs::read_to_string(name) {
            Ok(content) => crate::io::new_string_port_input(&content),
            Err(e) => return raise_file_error(rt, state, &format!("{}: could not open file: {}", who, e), args[0]),
        }
    };
    let port = new_port(rt.heap, kind);
    done(state, port, next)
}

fn open_input_file_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    open_input(rt, args, state, next, false)
}

fn open_binary_input_file_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    open_input(rt, args, state, next, true)
}

fn open_output(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef, binary: bool) -> Result<(), String> {
    let who = if binary { "open-binary-output-file" } else { "open-output-file" };
    arity(args, 1, 1, who)?;
    let name = string_arg(args[0], who)?;
    match rt.file_table.open_file(name, true) {
        Ok(id) => {
            let port = new_port(rt.heap, PortKind::FileOutput { name: name.clone(), id, binary });
            done(state, port, next)
        }
        Err(e) => raise_file_error(rt, state, &format!("{}: could not open file: {}", who, e), args[0]),
    }
}

fn open_output_file_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    open_output(rt, args, state, next, false)
}

fn open_binary_output_file_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    open_output(rt, args, state, next, true)
}

/// Close `port` (closing an already closed port does nothing).
fn close(rt: &mut RunTime, port: GcRef, who: &str) -> Result<(), String> {
    let kind = port_mut(port, who)?;
    if let PortKind::FileOutput { id, .. } = kind {
        rt.file_table.close_file(*id);
    }
    *kind = kind.closed();
    Ok(())
}

fn close_port_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 1, 1, "close-port")?;
    close(rt, args[0], "close-port")?;
    done(state, rt.heap.unspecified(), next)
}

fn close_input_port_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 1, 1, "close-input-port")?;
    if !port_mut(args[0], "close-input-port")?.is_input() {
        return Err("close-input-port: not an input port".to_string());
    }
    close(rt, args[0], "close-input-port")?;
    done(state, rt.heap.unspecified(), next)
}

fn close_output_port_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 1, 1, "close-output-port")?;
    if !port_mut(args[0], "close-output-port")?.is_output() {
        return Err("close-output-port: not an output port".to_string());
    }
    close(rt, args[0], "close-output-port")?;
    done(state, rt.heap.unspecified(), next)
}

fn delete_file_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 1, 1, "delete-file")?;
    let name = string_arg(args[0], "delete-file")?;
    match std::fs::remove_file(name) {
        Ok(()) => done(state, rt.heap.unspecified(), next),
        Err(e) => raise_file_error(rt, state, &format!("delete-file: could not delete file: {}", e), args[0]),
    }
}

// ---------------------------------------------------------------------------
// Textual input
// ---------------------------------------------------------------------------

/// (read [port])
fn read_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 0, 1, "read")?;
    let port = port_or_current(rt, args, 0, INPUT);
    let kind = port_mut(port, "read")?;
    let result = match kind {
        PortKind::Stdin | PortKind::StringPortInput { .. } => parse(rt.heap, kind),
        PortKind::Closed { .. } => return Err(closed_error("read")),
        _ => return Err("read: not a textual input port".to_string()),
    };
    match result {
        Ok(expr) => done(state, expr, next),
        Err(ParseError::Eof) => done(state, rt.heap.eof(), next),
        Err(ParseError::Syntax(err)) => {
            let msg = format!("read: syntax error: {}", err);
            let nil = rt.heap.nil_s();
            crate::eval::exceptions::raise_error(state, rt, ErrorKind::Read, &msg, nil);
            Ok(())
        }
    }
}

fn read_char_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 0, 1, "read-char")?;
    let c = get_char(port_or_current(rt, args, 0, INPUT), "read-char", false)?;
    let value = char_or_eof(rt.heap, c);
    done(state, value, next)
}

fn peek_char_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 0, 1, "peek-char")?;
    let c = get_char(port_or_current(rt, args, 0, INPUT), "peek-char", true)?;
    let value = char_or_eof(rt.heap, c);
    done(state, value, next)
}

/// (read-line [port]): the characters up to the next line ending (\n, \r
/// or \r\n), which is consumed but not included; the eof object at end of
/// file.
fn read_line_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 0, 1, "read-line")?;
    let port = port_or_current(rt, args, 0, INPUT);
    let mut line = String::new();
    let mut any = false;
    loop {
        match get_char(port, "read-line", false)? {
            None => break,
            Some('\n') => {
                any = true;
                break;
            }
            Some('\r') => {
                any = true;
                if get_char(port, "read-line", true)? == Some('\n') {
                    get_char(port, "read-line", false)?;
                }
                break;
            }
            Some(c) => {
                any = true;
                line.push(c);
            }
        }
    }
    let value = if any { new_string(rt.heap, &line) } else { rt.heap.eof() };
    done(state, value, next)
}

/// (read-string k [port]): up to k characters; the eof object if none.
fn read_string_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 1, 2, "read-string")?;
    let k = match gc_value!(args[0]) {
        SchemeValue::Int(i) => num_traits::ToPrimitive::to_usize(i),
        _ => None,
    }
    .ok_or("read-string: the count must be a non-negative exact integer")?;
    let port = port_or_current(rt, args, 1, INPUT);
    let mut out = String::new();
    let mut count = 0;
    while count < k {
        match get_char(port, "read-string", false)? {
            Some(c) => {
                out.push(c);
                count += 1;
            }
            None => break,
        }
    }
    let value = if count == 0 && k > 0 { rt.heap.eof() } else { new_string(rt.heap, &out) };
    done(state, value, next)
}

/// (char-ready? [port]): #t if a character can be read without waiting
/// (always, for string ports, including at end of file).
fn char_ready_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 0, 1, "char-ready?")?;
    let ready = match port_mut(port_or_current(rt, args, 0, INPUT), "char-ready?")? {
        PortKind::StringPortInput { .. } => true,
        PortKind::Stdin => false,
        PortKind::Closed { .. } => return Err(closed_error("char-ready?")),
        _ => return Err("char-ready?: not a textual input port".to_string()),
    };
    done(state, new_bool(rt.heap, ready), next)
}

// ---------------------------------------------------------------------------
// Textual output
// ---------------------------------------------------------------------------

fn write_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 1, 2, "write")?;
    let text = print_value(&args[0]);
    put_str(rt, port_or_current(rt, args, 1, OUTPUT), &text, "write")?;
    done(state, rt.heap.void(), next)
}

fn write_shared_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 1, 2, "write-shared")?;
    let text = crate::printer::write_shared_value(&args[0]);
    put_str(rt, port_or_current(rt, args, 1, OUTPUT), &text, "write-shared")?;
    done(state, rt.heap.void(), next)
}

fn write_simple_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 1, 2, "write-simple")?;
    let text = crate::printer::write_simple_value(&args[0]);
    put_str(rt, port_or_current(rt, args, 1, OUTPUT), &text, "write-simple")?;
    done(state, rt.heap.void(), next)
}

fn display_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 1, 2, "display")?;
    let text = display_value(&args[0]);
    put_str(rt, port_or_current(rt, args, 1, OUTPUT), &text, "display")?;
    done(state, rt.heap.void(), next)
}

fn newline_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 0, 1, "newline")?;
    put_str(rt, port_or_current(rt, args, 0, OUTPUT), "\n", "newline")?;
    done(state, rt.heap.void(), next)
}

fn write_char_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 1, 2, "write-char")?;
    let c = match gc_value!(args[0]) {
        SchemeValue::Char(c) => *c,
        _ => return Err("write-char: expected a character".to_string()),
    };
    put_str(rt, port_or_current(rt, args, 1, OUTPUT), &c.to_string(), "write-char")?;
    done(state, rt.heap.void(), next)
}

/// (write-string string [port [start [end]]])
fn write_string_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 1, 4, "write-string")?;
    let s = string_arg(args[0], "write-string")?;
    let chars: Vec<char> = s.chars().collect();
    let (start, end) = crate::builtin::range_args(args, 2, chars.len(), "write-string")?;
    let text: String = chars[start..end].iter().collect();
    put_str(rt, port_or_current(rt, args, 1, OUTPUT), &text, "write-string")?;
    done(state, rt.heap.void(), next)
}

fn flush_output_port_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 0, 1, "flush-output-port")?;
    match port_mut(port_or_current(rt, args, 0, OUTPUT), "flush-output-port")? {
        PortKind::Stdout => {
            std::io::stdout().flush().ok();
        }
        PortKind::Stderr => {
            std::io::stderr().flush().ok();
        }
        PortKind::FileOutput { id, .. } => {
            if let Some(file) = rt.file_table.get(*id) {
                file.flush().ok();
            }
        }
        PortKind::Closed { .. } => return Err(closed_error("flush-output-port")),
        _ => {}
    }
    done(state, rt.heap.void(), next)
}

// ---------------------------------------------------------------------------
// Binary output (needs the file table, so these are sys-builtins)
// ---------------------------------------------------------------------------

/// Write bytes to a binary output port.
fn put_bytes(rt: &mut RunTime, port: GcRef, bytes: &[u8], who: &str) -> Result<(), String> {
    match port_mut(port, who)? {
        PortKind::BytevectorOutput { bytes: out } => out.extend_from_slice(bytes),
        PortKind::FileOutput { id, binary: true, name } => {
            let file = rt.file_table.get(*id).ok_or_else(|| format!("{}: file {} is not open", who, name))?;
            file.write_all(bytes)
                .map_err(|e| format!("{}: could not write to {}: {}", who, name, e))?;
        }
        PortKind::Closed { .. } => return Err(closed_error(who)),
        _ => return Err(format!("{}: not a binary output port", who)),
    }
    Ok(())
}

fn write_u8_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 1, 2, "write-u8")?;
    let byte = match gc_value!(args[0]) {
        SchemeValue::Int(i) => num_traits::ToPrimitive::to_u8(i),
        _ => None,
    }
    .ok_or("write-u8: expected a byte (exact integer 0-255)")?;
    put_bytes(rt, port_or_current(rt, args, 1, OUTPUT), &[byte], "write-u8")?;
    done(state, rt.heap.void(), next)
}

/// (write-bytevector bv [port [start [end]]])
fn write_bytevector_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 1, 4, "write-bytevector")?;
    let bv = match gc_value!(args[0]) {
        SchemeValue::Bytevector(b) => b,
        _ => return Err("write-bytevector: expected a bytevector".to_string()),
    };
    let (start, end) = crate::builtin::range_args(args, 2, bv.len(), "write-bytevector")?;
    let bytes = bv[start..end].to_vec();
    put_bytes(rt, port_or_current(rt, args, 1, OUTPUT), &bytes, "write-bytevector")?;
    done(state, rt.heap.void(), next)
}

// ---------------------------------------------------------------------------
// Loading support: the REPL reads forms from the port on top of this stack
// ---------------------------------------------------------------------------

/// (push-port! port): read and evaluate forms from port next (used by load).
fn push_port_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 1, 1, "push-port!")?;
    rt.port_stack.push(args[0]);
    done(state, rt.heap.void(), next)
}

fn pop_port_sp(rt: &mut RunTime, args: &[GcRef], state: &mut CEKState, next: KontRef) -> Result<(), String> {
    arity(args, 0, 0, "pop-port!")?;
    let port = rt.port_stack.pop().ok_or("pop-port!: the port stack is empty")?;
    done(state, port, next)
}

// ---------------------------------------------------------------------------
// Predicates and string ports (no runtime state needed)
// ---------------------------------------------------------------------------

fn port_test(heap: &mut GcHeap, args: &[GcRef], who: &str, test: fn(&PortKind) -> bool) -> Result<GcRef, String> {
    arity(args, 1, 1, who)?;
    let result = port_ref(args[0]).is_some_and(test);
    Ok(new_bool(heap, result))
}

fn port_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    port_test(heap, args, "port?", |_| true)
}
fn input_port_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    port_test(heap, args, "input-port?", PortKind::is_input)
}
fn output_port_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    port_test(heap, args, "output-port?", PortKind::is_output)
}
fn textual_port_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    port_test(heap, args, "textual-port?", PortKind::is_textual)
}
fn binary_port_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    port_test(heap, args, "binary-port?", |p| !p.is_textual())
}
fn input_port_open_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    port_test(heap, args, "input-port-open?", |p| p.is_input() && p.is_open())
}
fn output_port_open_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    port_test(heap, args, "output-port-open?", |p| p.is_output() && p.is_open())
}

fn eof_object(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 0, 0, "eof-object")?;
    Ok(heap.eof())
}

fn eof_object_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, 1, "eof-object?")?;
    Ok(new_bool(heap, matches!(gc_value!(args[0]), SchemeValue::Eof)))
}

fn open_input_string(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, 1, "open-input-string")?;
    let s = string_arg(args[0], "open-input-string")?.clone();
    Ok(new_port(heap, crate::io::new_string_port_input(&s)))
}

fn open_output_string(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 0, 0, "open-output-string")?;
    Ok(new_port(heap, PortKind::StringPortOutput { content: String::new() }))
}

fn get_output_string(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, 1, "get-output-string")?;
    match port_mut(args[0], "get-output-string")? {
        PortKind::StringPortOutput { content } => {
            let s = content.clone();
            Ok(new_string(heap, &s))
        }
        _ => Err("get-output-string: not a port made by open-output-string".to_string()),
    }
}

fn file_exists_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, 1, "file-exists?")?;
    let name = string_arg(args[0], "file-exists?")?;
    Ok(new_bool(heap, std::path::Path::new(name).exists()))
}

// ---------------------------------------------------------------------------
// Bytevector ports and binary input
// ---------------------------------------------------------------------------

fn open_input_bytevector(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, 1, "open-input-bytevector")?;
    let bytes = match gc_value!(args[0]) {
        SchemeValue::Bytevector(b) => b.clone(),
        _ => return Err("open-input-bytevector: expected a bytevector".to_string()),
    };
    Ok(new_port(heap, PortKind::BytevectorInput { bytes, pos: 0 }))
}

fn open_output_bytevector(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 0, 0, "open-output-bytevector")?;
    Ok(new_port(heap, PortKind::BytevectorOutput { bytes: Vec::new() }))
}

fn get_output_bytevector(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, 1, "get-output-bytevector")?;
    match port_mut(args[0], "get-output-bytevector")? {
        PortKind::BytevectorOutput { bytes } => {
            let copy = bytes.clone();
            Ok(crate::gc::new_bytevector(heap, copy))
        }
        _ => Err("get-output-bytevector: not a port made by open-output-bytevector".to_string()),
    }
}

/// The unread bytes of a binary input port, and its position to advance.
fn binary_input(port: GcRef, who: &str) -> Result<(&'static [u8], &'static mut usize), String> {
    match port_mut(port, who)? {
        PortKind::BytevectorInput { bytes, pos } => Ok((&bytes[..], pos)),
        PortKind::Closed { .. } => Err(closed_error(who)),
        _ => Err(format!("{}: not a binary input port", who)),
    }
}

fn byte_or_eof(heap: &mut GcHeap, b: Option<u8>) -> GcRef {
    match b {
        Some(b) => crate::gc::new_int(heap, num_bigint::BigInt::from(b)),
        None => heap.eof(),
    }
}

/// The port argument of a binary input procedure. There is no binary
/// current input port, so it is required.
fn binary_port_arg(args: &[GcRef], i: usize, who: &str) -> Result<GcRef, String> {
    args.get(i).copied().ok_or_else(|| format!("{}: expects a binary input port", who))
}

fn read_u8(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 0, 1, "read-u8")?;
    let (bytes, pos) = binary_input(binary_port_arg(args, 0, "read-u8")?, "read-u8")?;
    let b = bytes.get(*pos).copied();
    if b.is_some() {
        *pos += 1;
    }
    Ok(byte_or_eof(heap, b))
}

fn peek_u8(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 0, 1, "peek-u8")?;
    let (bytes, pos) = binary_input(binary_port_arg(args, 0, "peek-u8")?, "peek-u8")?;
    let b = bytes.get(*pos).copied();
    Ok(byte_or_eof(heap, b))
}

fn u8_ready(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 0, 1, "u8-ready?")?;
    binary_input(binary_port_arg(args, 0, "u8-ready?")?, "u8-ready?")?;
    Ok(new_bool(heap, true))
}

/// (read-bytevector k port)
fn read_bytevector(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, 2, "read-bytevector")?;
    let k = match gc_value!(args[0]) {
        SchemeValue::Int(i) => num_traits::ToPrimitive::to_usize(i),
        _ => None,
    }
    .ok_or("read-bytevector: the count must be a non-negative exact integer")?;
    let (bytes, pos) = binary_input(binary_port_arg(args, 1, "read-bytevector")?, "read-bytevector")?;
    let available = bytes.len() - *pos;
    if available == 0 && k > 0 {
        return Ok(heap.eof());
    }
    let n = k.min(available);
    let out = bytes[*pos..*pos + n].to_vec();
    *pos += n;
    Ok(crate::gc::new_bytevector(heap, out))
}

/// (read-bytevector! bv port [start [end]]): the number of bytes read into
/// bv starting at start, or the eof object if none were available.
fn read_bytevector_into(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, 4, "read-bytevector!")?;
    let target_len = match gc_value!(args[0]) {
        SchemeValue::Bytevector(b) => b.len(),
        _ => return Err("read-bytevector!: expected a bytevector".to_string()),
    };
    let (start, end) = crate::builtin::range_args(args, 2, target_len, "read-bytevector!")?;
    let (bytes, pos) = binary_input(binary_port_arg(args, 1, "read-bytevector!")?, "read-bytevector!")?;
    let available = bytes.len() - *pos;
    if available == 0 && end > start {
        return Ok(heap.eof());
    }
    let n = (end - start).min(available);
    let src = bytes[*pos..*pos + n].to_vec();
    *pos += n;
    if let SchemeValue::Bytevector(target) = gc_value_mut!(args[0]) {
        target[start..start + n].copy_from_slice(&src);
    }
    Ok(crate::gc::new_int(heap, num_bigint::BigInt::from(n)))
}
