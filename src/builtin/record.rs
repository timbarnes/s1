//! Records (R7RS 5.5). `define-record-type` is a `syntax-rules` macro in
//! scheme/s1-core.scm that expands into definitions calling these
//! primitives; they are not meant to be called directly.

use crate::env::{EnvOps, EnvRef};
use crate::gc::{GcHeap, GcObject, GcRef, Record, RecordType, SchemeValue, list_to_vec, new_bool};
use crate::gc_value;
use crate::printer::print_value;
use crate::register_builtin_family;

pub fn register_record_builtins(heap: &mut GcHeap, env: EnvRef) {
    register_builtin_family!(heap, env,
        "%make-record-type" => (make_record_type, "(%make-record-type name field-specs) Internal: a new record type for define-record-type"),
        "%record-make" => (record_make, "(%record-make type field-names values) Internal: a record constructor's work"),
        "%record?" => (record_q, "(%record? obj type) Internal: a record predicate's work"),
        "%record-get" => (record_get, "(%record-get obj type field) Internal: a record accessor's work"),
        "%record-set!" => (record_set, "(%record-set! obj type field value) Internal: a record modifier's work"),
    );
}

fn record_type(v: GcRef, who: &str) -> Result<&'static RecordType, String> {
    match gc_value!(v) {
        SchemeValue::RecordType(t) => Ok(t),
        _ => Err(format!("{}: not a record type: {}", who, print_value(&v))),
    }
}

/// The index of field `name` in `t`.
fn field_index(t: &RecordType, name: GcRef, who: &str) -> Result<usize, String> {
    t.fields
        .iter()
        .position(|f| *f == name)
        .ok_or_else(|| format!("{}: {} has no field {}", who, print_value(&t.name), print_value(&name)))
}

/// The record `obj`, checked to be of type `rtype`.
fn record_of(obj: GcRef, rtype: GcRef, who: &str) -> Result<&'static mut Record, String> {
    match crate::gc_value_mut!(obj) {
        SchemeValue::Record(r) if r.rtype == rtype => Ok(r),
        _ => Err(format!(
            "{}: expected a {} record, got {}",
            who,
            match gc_value!(rtype) {
                SchemeValue::RecordType(t) => print_value(&t.name),
                _ => "?".to_string(),
            },
            print_value(&obj)
        )),
    }
}

/// `(%make-record-type name (field-spec ...))`: each spec is `(field accessor
/// [modifier])` or a bare field name.
fn make_record_type(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("%make-record-type: expects 2 arguments".to_string());
    }
    let specs = list_to_vec(heap, args[1])?;
    let mut fields = Vec::with_capacity(specs.len());
    for spec in specs {
        let name = match gc_value!(spec) {
            SchemeValue::Pair(car, _) => *car,
            _ => spec,
        };
        if !matches!(gc_value!(name), SchemeValue::Symbol(_)) {
            return Err(format!("define-record-type: bad field spec {}", print_value(&spec)));
        }
        if fields.contains(&name) {
            return Err(format!("define-record-type: duplicate field {}", print_value(&name)));
        }
        fields.push(name);
    }
    Ok(heap.alloc(GcObject {
        value: SchemeValue::RecordType(Box::new(RecordType { name: args[0], fields })),
        marked: 0,
    }))
}

/// `(%record-make type (field ...) (value ...))`: fields not given to the
/// constructor start out unspecified.
fn record_make(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 3 {
        return Err("%record-make: expects 3 arguments".to_string());
    }
    let t = record_type(args[0], "record constructor")?;
    let names = list_to_vec(heap, args[1])?;
    let values = list_to_vec(heap, args[2])?;
    let mut fields = vec![heap.unspecified(); t.fields.len()];
    for (name, value) in names.iter().zip(values) {
        fields[field_index(t, *name, "record constructor")?] = value;
    }
    Ok(heap.alloc(GcObject {
        value: SchemeValue::Record(Box::new(Record { rtype: args[0], fields })),
        marked: 0,
    }))
}

fn record_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("%record?: expects 2 arguments".to_string());
    }
    let is = matches!(gc_value!(args[0]), SchemeValue::Record(r) if r.rtype == args[1]);
    Ok(new_bool(heap, is))
}

fn record_get(_heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 3 {
        return Err("%record-get: expects 3 arguments".to_string());
    }
    let t = record_type(args[1], "record accessor")?;
    let i = field_index(t, args[2], "record accessor")?;
    Ok(record_of(args[0], args[1], "record accessor")?.fields[i])
}

fn record_set(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 4 {
        return Err("%record-set!: expects 4 arguments".to_string());
    }
    let t = record_type(args[1], "record modifier")?;
    let i = field_index(t, args[2], "record modifier")?;
    record_of(args[0], args[1], "record modifier")?.fields[i] = args[3];
    Ok(heap.unspecified())
}
