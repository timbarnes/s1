//! Vectors (R7RS 6.8).

use crate::env::{EnvOps, EnvRef};
use crate::gc::{GcHeap, GcRef, SchemeValue, list_from_slice, list_to_vec, new_int, new_vector};
use crate::register_builtin_family;
use crate::{gc_value, gc_value_mut};
use num_bigint::BigInt;
use num_traits::{Signed, ToPrimitive};
use std::vec;

/// Bind the vector procedures in `env`.
pub fn register_vector_builtins(heap: &mut crate::gc::GcHeap, env: EnvRef) {
    register_builtin_family!(heap, env,
        "vector" => (vector, "(vector arg1 arg2 ...) Create a new vector from a list of arguments"),
        "make-vector" => (make_vector, "(make-vector length [default]) Make a vector of specified length and optionally initializes it with a default value"),
        "vector-length" => (vector_length, "(vector-length vector) Get the length of a vector"),
        "vector-ref" => (vector_ref, "(vector-ref vector index) Get the element at the specified index in a vector"),
        "vector-set!" => (vector_set, "(vector-set! vector index value) Set the element at the specified index in a vector"),
        "vector->list" => (vector_to_list, "(vector->list vector [start [end]]) Returns a list of the elements of vector from start to end"),
        "vector-copy" => (vector_copy, "(vector-copy vector [start [end]]) Returns a new vector of the elements of vector from start to end"),
        "vector-copy!" => (vector_copy_to, "(vector-copy! to at from [start [end]]) Copies the elements start to end of from into to, starting at index at"),
        "vector-append" => (vector_append, "(vector-append vector ...) Returns a new vector of the elements of each vector in turn"),
        "list->vector" => (list_to_vector, "(list->vector list) Convert a list to a vector"),
        "vector-fill!" => (vector_fill, "(vector-fill! vector fill [start [end]]) Stores fill in the elements of vector from start to end"),
    );
}

/// Creates a new vector from a list of arguments.
/// `(vector arg1 arg2 ...)`
pub fn vector(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let values = args.to_vec();
    Ok(new_vector(heap, values))
}

/// Makes a vector of specified length and optionally initializes it with a default value.
/// `(make-vector length [default])`
pub fn make_vector(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    match args.len() {
        1 | 2 => {
            let fill = if args.len() == 2 {
                args[1]
            } else {
                heap.nil_s()
            };
            let length = match &heap.get_value(args[0]) {
                SchemeValue::Int(n) => {
                    if n.is_negative() {
                        return Err(
                            "make-vector: length must be a non-negative integer".to_string()
                        );
                    }
                    n.to_usize().unwrap()
                }
                _ => return Err("make-vector: length parameter must be an integer".to_string()),
            };
            let vector = new_vector(heap, vec![fill; length]);
            Ok(vector)
        }
        _ => Err("make-vector: expects 1 or 2 arguments".to_string()),
    }
}

// Returns the length of the vector
// (vector-length vector)
pub fn vector_length(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 1 {
        match &heap.get_value(args[0]) {
            SchemeValue::Vector(v) => {
                let len = BigInt::from(v.len());
                Ok(new_int(heap, len))
            }
            _ => Err("vector-length: argument must be a vector".to_string()),
        }
    } else {
        Err("vector-length: expects exactly 1 argument".to_string())
    }
}

// Returns the element at the given index in the vector
// (vector-ref vector index)
pub fn vector_ref(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() == 2 {
        let vector = match &heap.get_value(args[0]) {
            SchemeValue::Vector(v) => v,
            _ => return Err("vector-ref: first argument must be a vector".to_string()),
        };
        let index = match &heap.get_value(args[1]) {
            SchemeValue::Int(n) => {
                if n.is_negative() {
                    return Err("vector-ref: index must be a non-negative integer".to_string());
                }
                n.to_usize().unwrap()
            }
            _ => return Err("vector-ref: second argument must be an integer".to_string()),
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
pub fn vector_set(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 3 {
        return Err("vector-set!: expects exactly 3 arguments".to_string());
    }
    let index = match &heap.get_value(args[1]) {
        SchemeValue::Int(n) => {
            if n.is_negative() {
                return Err("vector-set!: index must be a non-negative integer".to_string());
            }
            n.to_usize().unwrap()
        }
        _ => return Err("vector-set!: second argument must be an integer".to_string()),
    };

    let vector = gc_value_mut!(args[0]);
    match vector {
        SchemeValue::Vector(v) => {
            if index >= v.len() {
                return Err("vector-set!: index out of bounds".to_string());
            }
            v[index] = args[2];
        }
        _ => return Err("vector-set!: first argument must be a vector".to_string()),
    }
    Ok(heap.void())
}

fn vector_of(v: GcRef, who: &str) -> Result<&'static Vec<GcRef>, String> {
    match gc_value!(v) {
        SchemeValue::Vector(items) => Ok(items),
        _ => Err(format!("{}: expected a vector, got {}", who, crate::printer::print_value(&v))),
    }
}

fn arity(args: &[GcRef], min: usize, max: usize, who: &str) -> Result<(), String> {
    if args.len() < min || args.len() > max {
        Err(format!("{}: wrong number of arguments ({})", who, args.len()))
    } else {
        Ok(())
    }
}

/// `(vector->list vector [start [end]])`
fn vector_to_list(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, 3, "vector->list")?;
    let v = vector_of(args[0], "vector->list")?;
    let (start, end) = super::range_args(args, 1, v.len(), "vector->list")?;
    Ok(list_from_slice(&v[start..end], heap))
}

/// `(vector-copy vector [start [end]])`
fn vector_copy(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 1, 3, "vector-copy")?;
    let v = vector_of(args[0], "vector-copy")?;
    let (start, end) = super::range_args(args, 1, v.len(), "vector-copy")?;
    let copy = v[start..end].to_vec();
    Ok(new_vector(heap, copy))
}

/// `(vector-copy! to at from [start [end]])`
fn vector_copy_to(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 3, 5, "vector-copy!")?;
    let from = vector_of(args[2], "vector-copy!")?;
    let (start, end) = super::range_args(args, 3, from.len(), "vector-copy!")?;
    // Copy out first: `to` and `from` may be the same vector.
    let src = from[start..end].to_vec();
    let to = match gc_value_mut!(args[0]) {
        SchemeValue::Vector(items) => items,
        _ => return Err("vector-copy!: expected a vector".to_string()),
    };
    let at = match gc_value!(args[1]) {
        SchemeValue::Int(i) => i.to_usize().filter(|a| a + src.len() <= to.len()),
        _ => None,
    }
    .ok_or("vector-copy!: the copied elements don't fit at that index")?;
    to[at..at + src.len()].copy_from_slice(&src);
    Ok(heap.unspecified())
}

/// `(vector-append vector ...)`
fn vector_append(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let mut out = Vec::new();
    for a in args {
        out.extend_from_slice(vector_of(*a, "vector-append")?);
    }
    Ok(new_vector(heap, out))
}

/// `(list->vector list)` -> vector
fn list_to_vector(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("list->vector: expects exactly 1 argument".to_string());
    }

    match gc_value!(args[0]) {
        SchemeValue::Pair(_, _) | SchemeValue::Nil => {
            let vec = list_to_vec(heap, args[0])?;
            Ok(new_vector(heap, vec))
        }
        _ => Err("list->vector: argument must be a list".to_string()),
    }
}

/// `(vector-fill! vector fill [start [end]])` -> unspecified
fn vector_fill(_heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    arity(args, 2, 4, "vector-fill!")?;
    let fill_val = args[1];
    let len = vector_of(args[0], "vector-fill!")?.len();
    let (start, end) = super::range_args(args, 2, len, "vector-fill!")?;
    if let SchemeValue::Vector(v) = gc_value_mut!(args[0]) {
        for elem in &mut v[start..end] {
            *elem = fill_val;
        }
    }
    // R7RS leaves the result unspecified; s1 has always returned the vector.
    Ok(args[0])
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::{RunTime, RunTimeStruct};
    use crate::gc::{new_bool, new_char, new_int};

    #[test]
    fn test_vector_builtin() {
        let mut ev = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut ev);
        let heap = &mut ec.heap;

        // Empty vector
        let result = vector(heap, &[]).unwrap();
        if let SchemeValue::Vector(v) = heap.get_value(result) {
            assert!(v.is_empty());
        } else {
            panic!("Expected vector");
        }

        // Vector with elements
        let arg1 = new_int(heap, BigInt::from(1));
        let arg2 = new_bool(heap, true);
        let result = vector(heap, &[arg1, arg2]).unwrap();
        if let SchemeValue::Vector(v) = heap.get_value(result) {
            assert_eq!(v.len(), 2);
            assert!(matches!(heap.get_value(v[0]), SchemeValue::Int(i) if *i == BigInt::from(1)));
            assert!(matches!(heap.get_value(v[1]), SchemeValue::Bool(true)));
        } else {
            panic!("Expected vector");
        }
    }

    #[test]
    fn test_make_vector_builtin() {
        let mut ev = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut ev);
        let heap = &mut ec.heap;

        // Just length
        let len_arg = new_int(heap, BigInt::from(3));
        let result = make_vector(heap, &[len_arg]).unwrap();
        if let SchemeValue::Vector(v) = heap.get_value(result) {
            assert_eq!(v.len(), 3);
            for elem in v {
                assert!(matches!(heap.get_value(*elem), SchemeValue::Nil));
            }
        } else {
            panic!("Expected vector");
        }

        // Length and fill
        let len_arg = new_int(heap, BigInt::from(5));
        let fill_arg = new_char(heap, 'a');
        let result = make_vector(heap, &[len_arg, fill_arg]).unwrap();
        if let SchemeValue::Vector(v) = heap.get_value(result) {
            assert_eq!(v.len(), 5);
            for elem in v {
                assert!(matches!(heap.get_value(*elem), SchemeValue::Char('a')));
            }
        } else {
            panic!("Expected vector");
        }

        // Zero length
        let len_arg = new_int(heap, BigInt::from(0));
        let result = make_vector(heap, &[len_arg]).unwrap();
        if let SchemeValue::Vector(v) = heap.get_value(result) {
            assert!(v.is_empty());
        } else {
            panic!("Expected vector");
        }

        // Error cases
        assert!(make_vector(heap, &[]).is_err());
        let neg_len = new_int(heap, BigInt::from(-1));
        assert!(make_vector(heap, &[neg_len]).is_err());
        let not_int = new_bool(heap, false);
        assert!(make_vector(heap, &[not_int]).is_err());
    }

    #[test]
    fn test_vector_length_builtin() {
        let mut ev = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut ev);
        let heap = &mut ec.heap;

        let val = new_int(heap, BigInt::from(1));
        let vec_arg = vector(heap, &[val]).unwrap();
        let len = vector_length(heap, &[vec_arg]).unwrap();
        assert!(matches!(heap.get_value(len), SchemeValue::Int(i) if *i == BigInt::from(1)));

        let empty_vec = vector(heap, &[]).unwrap();
        let len = vector_length(heap, &[empty_vec]).unwrap();
        assert!(matches!(heap.get_value(len), SchemeValue::Int(i) if *i == BigInt::from(0)));

        assert!(vector_length(heap, &[]).is_err());
        let non_vec_arg = new_int(heap, BigInt::from(1));
        assert!(vector_length(heap, &[non_vec_arg]).is_err());
    }

    #[test]
    fn test_vector_ref_builtin() {
        let mut ev = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut ev);
        let heap = &mut ec.heap;

        let val1 = new_int(heap, BigInt::from(10));
        let val2 = new_int(heap, BigInt::from(20));
        let vec = vector(heap, &[val1, val2]).unwrap();

        let index0 = new_int(heap, BigInt::from(0));
        let result = vector_ref(heap, &[vec, index0]).unwrap();
        assert_eq!(result, val1);

        let index1 = new_int(heap, BigInt::from(1));
        let result = vector_ref(heap, &[vec, index1]).unwrap();
        assert_eq!(result, val2);

        let invalid_index = new_int(heap, BigInt::from(2));
        assert!(vector_ref(heap, &[vec, invalid_index]).is_err());
        let neg_index = new_int(heap, BigInt::from(-1));
        assert!(vector_ref(heap, &[vec, neg_index]).is_err());
        let not_a_vec = new_int(heap, BigInt::from(0));
        assert!(vector_ref(heap, &[not_a_vec, index0]).is_err());
    }

    #[test]
    fn test_vector_set_builtin() {
        let mut ev = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut ev);
        let heap = &mut ec.heap;

        let val1 = new_int(heap, BigInt::from(10));
        let val2 = new_int(heap, BigInt::from(20));
        let vec = vector(heap, &[val1, val2]).unwrap();

        let index1 = new_int(heap, BigInt::from(1));
        let new_val = new_char(heap, 'z');
        vector_set(heap, &[vec, index1, new_val]).unwrap();

        let result = vector_ref(heap, &[vec, index1]).unwrap();
        assert_eq!(result, new_val);

        let invalid_index = new_int(heap, BigInt::from(3));
        assert!(vector_set(heap, &[vec, invalid_index, new_val]).is_err());
        let neg_index = new_int(heap, BigInt::from(-1));
        assert!(vector_set(heap, &[vec, neg_index, new_val]).is_err());
    }

    #[test]
    fn test_vector_to_list_builtin() {
        let mut ev = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut ev);
        let heap = &mut ec.heap;

        let val1 = new_int(heap, BigInt::from(1));
        let val2 = new_bool(heap, true);
        let vec = vector(heap, &[val1, val2]).unwrap();

        let list = vector_to_list(heap, &[vec]).unwrap();

        let elements = list_to_vec(heap, list).unwrap();
        assert_eq!(elements.len(), 2);
        assert_eq!(elements[0], val1);
        assert_eq!(elements[1], val2);

        let empty_vec = vector(heap, &[]).unwrap();
        let empty_list = vector_to_list(heap, &[empty_vec]).unwrap();
        assert_eq!(empty_list, heap.nil_s());
    }

    #[test]
    fn test_list_to_vector_builtin() {
        let mut ev = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut ev);
        let heap = &mut ec.heap;

        let val1 = new_int(heap, BigInt::from(1));
        let val2 = new_bool(heap, true);
        let list = list_from_slice(&[val1, val2], heap);

        let vec_res = list_to_vector(heap, &[list]).unwrap();
        if let SchemeValue::Vector(v) = heap.get_value(vec_res) {
            assert_eq!(v.len(), 2);
            assert_eq!(v[0], val1);
            assert_eq!(v[1], val2);
        } else {
            panic!("Expected vector");
        }

        let empty_list = heap.nil_s();
        let empty_vec_res = list_to_vector(heap, &[empty_list]).unwrap();
        if let SchemeValue::Vector(v) = heap.get_value(empty_vec_res) {
            assert!(v.is_empty());
        } else {
            panic!("Expected vector");
        }
    }

    #[test]
    fn test_vector_fill_builtin() {
        let mut ev = RunTimeStruct::new();
        let mut ec = RunTime::from_eval(&mut ev);
        let heap = &mut ec.heap;

        let val1 = new_int(heap, BigInt::from(1));
        let val2 = new_int(heap, BigInt::from(2));
        let vec = vector(heap, &[val1, val2]).unwrap();

        let fill_val = new_char(heap, 'f');
        vector_fill(heap, &[vec, fill_val]).unwrap();

        if let SchemeValue::Vector(v) = heap.get_value(vec) {
            for elem in v {
                assert_eq!(*elem, fill_val);
            }
        } else {
            panic!("Expected vector");
        }
    }
}
