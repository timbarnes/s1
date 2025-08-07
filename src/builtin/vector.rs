use std::vec;

use crate::eval::EvalContext;
/// Vector functions
///
use crate::gc::{GcRef, SchemeValue, new_int, new_vector};
use num_bigint::BigInt;
use num_traits::ToPrimitive;

/// Creates a new vector from a list of arguments.
/// (vector arg1 arg2 ...)
pub fn vector(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() > 0 {
        let values = args.to_vec();
        Ok(new_vector(ec.heap, values))
    } else {
        Err("vector: expects at least 1 argument".to_string())
    }
}

/// Makes a vector of specified length and optionally initializes it with a default value.
/// (make-vector length [default])
pub fn make_vector(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    match args.len() {
        0 => Err("make-vector: length parameter required.".to_string()),
        1 | 2 => {
            let mut init = crate::gc::get_nil(ec.heap);
            if args.len() == 2 {
                init = args[1];
            }
            let length = match &ec.heap.get_value(args[0]) {
                SchemeValue::Int(n) => n.to_usize().unwrap(),
                _ => return Err("make-vector: length parameter must be a number".to_string()),
            };
            let vector = new_vector(ec.heap, vec![init; length]);
            Ok(vector)
        }
        _ => Err("make-vector: requires length and optional initialization value".to_string()),
    }
}

// Returns the length of the vector
// (vector-length vector)
pub fn vector_length(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 1 {
        match &ec.heap.get_value(args[0]) {
            SchemeValue::Vector(v) => {
                let len = BigInt::from(v.len());
                Ok(new_int(ec.heap, len))
            }
            _ => Err("vector-length: argument must be a vector".to_string()),
        }
    } else {
        Err("vector-length: expects exactly 1 argument".to_string())
    }
}

// Returns the element at the given index in the vector
// (vector-ref vector index)
pub fn vector_ref(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 2 {
        let vector = match &ec.heap.get_value(args[0]) {
            SchemeValue::Vector(v) => v,
            _ => return Err("vector-ref: first argument must be a vector".to_string()),
        };
        let index = match &ec.heap.get_value(args[1]) {
            SchemeValue::Int(n) => n.to_usize().unwrap(),
            _ => return Err("vector-ref: second argument must be a number".to_string()),
        };
        if index >= vector.len() {
            return Err("vector-ref: index out of bounds".to_string());
        }
        Ok(vector[index])
    } else {
        Err("vector-ref: expects exactly 2 arguments".to_string())
    }
}

// Stores the element at the given index in the vector
// (vector-set! vector index value)
pub fn vector_set(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 3 {
        let vector = match &ec.heap.get_value(args[0]) {
            SchemeValue::Vector(v) => v,
            _ => return Err("vector-ref: first argument must be a vector".to_string()),
        };
        let index = match &ec.heap.get_value(args[1]) {
            SchemeValue::Int(n) => n.to_usize().unwrap(),
            _ => return Err("vector-ref: second argument must be a number".to_string()),
        };
        if index >= vector.len() {
            return Err("vector-set: index out of bounds".to_string());
        }
        //vector[index] = args[2];
        //let result = new_vector(heap, *vector);
        Ok(args[0])
    } else {
        Err("vector-set!: expects exactly 2 arguments".to_string())
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

pub fn register_vector_builtins(heap: &mut crate::gc::GcHeap, env: &mut crate::env::Environment) {
    register_builtin_family!(heap, env,
        "vector" => vector,
        "make-vector" => make_vector,
        "vector-length" => vector_length,
        "vector-ref" => vector_ref,
        "vector-set!" => vector_set,
    );
}
