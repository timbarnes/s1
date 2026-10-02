//! Bytevectors (R7RS 6.9).

use super::range_args;
use crate::env::{EnvOps, EnvRef};
use crate::gc::{GcHeap, GcRef, SchemeValue, new_bool, new_bytevector, new_int, new_string};
use crate::printer::print_value;
use crate::{gc_value, gc_value_mut, register_builtin_family};
use num_bigint::BigInt;
use num_traits::ToPrimitive;

pub fn register_bytevector_builtins(heap: &mut GcHeap, env: EnvRef) {
    register_builtin_family!(heap, env,
        "bytevector?" => (bytevector_q, "(bytevector? obj) Returns #t if obj is a bytevector"),
        "make-bytevector" => (make_bytevector, "(make-bytevector k [byte]) Returns a bytevector of k bytes, each byte (default 0)"),
        "bytevector" => (bytevector, "(bytevector byte ...) Returns a bytevector of the given bytes"),
        "bytevector-length" => (bytevector_length, "(bytevector-length bv) Returns the number of bytes in bv"),
        "bytevector-u8-ref" => (bytevector_u8_ref, "(bytevector-u8-ref bv k) Returns byte k of bv"),
        "bytevector-u8-set!" => (bytevector_u8_set, "(bytevector-u8-set! bv k byte) Stores byte in element k of bv"),
        "bytevector-copy" => (bytevector_copy, "(bytevector-copy bv [start [end]]) Returns a new bytevector of bytes start to end of bv"),
        "bytevector-copy!" => (bytevector_copy_to, "(bytevector-copy! to at from [start [end]]) Copies bytes start to end of from into to, starting at index at"),
        "bytevector-append" => (bytevector_append, "(bytevector-append bv ...) Returns a new bytevector of the bytes of each bv in turn"),
        "utf8->string" => (utf8_to_string, "(utf8->string bv [start [end]]) Decodes bytes start to end of bv as UTF-8"),
        "string->utf8" => (string_to_utf8, "(string->utf8 string [start [end]]) Encodes characters start to end of string as UTF-8"),
    );
}

fn bytes_of(v: GcRef, who: &str) -> Result<&'static Vec<u8>, String> {
    match gc_value!(v) {
        SchemeValue::Bytevector(b) => Ok(b),
        _ => Err(format!("{}: expected a bytevector, got {}", who, print_value(&v))),
    }
}

fn bytes_of_mut(v: GcRef, who: &str) -> Result<&'static mut Vec<u8>, String> {
    match gc_value_mut!(v) {
        SchemeValue::Bytevector(b) => Ok(b),
        _ => Err(format!("{}: expected a bytevector, got {}", who, print_value(&v))),
    }
}

fn byte(v: GcRef, who: &str) -> Result<u8, String> {
    match gc_value!(v) {
        SchemeValue::Int(i) => i.to_u8(),
        _ => None,
    }
    .ok_or_else(|| format!("{}: expected a byte (exact integer 0-255), got {}", who, print_value(&v)))
}

fn index(v: GcRef, len: usize, who: &str) -> Result<usize, String> {
    match gc_value!(v) {
        SchemeValue::Int(i) => i.to_usize().filter(|k| *k < len),
        _ => None,
    }
    .ok_or_else(|| format!("{}: index {} is out of range for length {}", who, print_value(&v), len))
}

fn arity(args: &[GcRef], min: usize, max: usize, who: &str) -> Result<(), String> {
    if args.len() < min || args.len() > max {
        Err(format!("{}: wrong number of arguments ({})", who, args.len()))
    } else {
        Ok(())
    }
}

fn bytevector_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, 1, "bytevector?")?;
    Ok(new_bool(heap, matches!(gc_value!(args[0]), SchemeValue::Bytevector(_))))
}

fn make_bytevector(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, 2, "make-bytevector")?;
    let k = match gc_value!(args[0]) {
        SchemeValue::Int(i) => i.to_usize(),
        _ => None,
    }
    .ok_or("make-bytevector: length must be a non-negative exact integer")?;
    let fill = match args.get(1) {
        Some(b) => byte(*b, "make-bytevector")?,
        None => 0,
    };
    Ok(new_bytevector(heap, vec![fill; k]))
}

fn bytevector(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let bytes = args.iter().map(|b| byte(*b, "bytevector")).collect::<Result<Vec<u8>, String>>()?;
    Ok(new_bytevector(heap, bytes))
}

fn bytevector_length(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, 1, "bytevector-length")?;
    let len = bytes_of(args[0], "bytevector-length")?.len();
    Ok(new_int(heap, BigInt::from(len)))
}

fn bytevector_u8_ref(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 2, 2, "bytevector-u8-ref")?;
    let b = bytes_of(args[0], "bytevector-u8-ref")?;
    let k = index(args[1], b.len(), "bytevector-u8-ref")?;
    Ok(new_int(heap, BigInt::from(b[k])))
}

fn bytevector_u8_set(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 3, 3, "bytevector-u8-set!")?;
    let value = byte(args[2], "bytevector-u8-set!")?;
    let b = bytes_of_mut(args[0], "bytevector-u8-set!")?;
    let k = index(args[1], b.len(), "bytevector-u8-set!")?;
    b[k] = value;
    Ok(heap.unspecified())
}

fn bytevector_copy(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, 3, "bytevector-copy")?;
    let b = bytes_of(args[0], "bytevector-copy")?;
    let (start, end) = range_args(args, 1, b.len(), "bytevector-copy")?;
    let copy = b[start..end].to_vec();
    Ok(new_bytevector(heap, copy))
}

fn bytevector_copy_to(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 3, 5, "bytevector-copy!")?;
    let from = bytes_of(args[2], "bytevector-copy!")?;
    let (start, end) = range_args(args, 3, from.len(), "bytevector-copy!")?;
    // Copy out first: `to` and `from` may be the same bytevector.
    let src = from[start..end].to_vec();
    let to = bytes_of_mut(args[0], "bytevector-copy!")?;
    let at = match gc_value!(args[1]) {
        SchemeValue::Int(i) => i.to_usize().filter(|a| a + src.len() <= to.len()),
        _ => None,
    }
    .ok_or("bytevector-copy!: the copied bytes don't fit at that index")?;
    to[at..at + src.len()].copy_from_slice(&src);
    Ok(heap.unspecified())
}

fn bytevector_append(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let mut out = Vec::new();
    for a in args {
        out.extend_from_slice(bytes_of(*a, "bytevector-append")?);
    }
    Ok(new_bytevector(heap, out))
}

fn utf8_to_string(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, 3, "utf8->string")?;
    let b = bytes_of(args[0], "utf8->string")?;
    let (start, end) = range_args(args, 1, b.len(), "utf8->string")?;
    let s = std::str::from_utf8(&b[start..end]).map_err(|e| format!("utf8->string: invalid UTF-8: {}", e))?;
    Ok(new_string(heap, s))
}

fn string_to_utf8(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, 3, "string->utf8")?;
    let s = match gc_value!(args[0]) {
        SchemeValue::Str(s) => s,
        _ => return Err("string->utf8: expected a string".to_string()),
    };
    let (start, end) = range_args(args, 1, s.chars().count(), "string->utf8")?;
    let part: String = s.chars().skip(start).take(end - start).collect();
    Ok(new_bytevector(heap, part.into_bytes()))
}
