/// Vector functions
///
use crate::env::{EnvOps, EnvRef};
use crate::gc::{GcHeap, GcRef, SchemeValue, list_from_slice, list_to_vec, new_int, new_vector};
use crate::register_builtin_family;
use crate::{gc_value, gc_value_mut};
use num_bigint::BigInt;
use num_traits::{Signed, ToPrimitive, Zero};
use std::vec;

pub fn register_vector_builtins(heap: &mut crate::gc::GcHeap, env: EnvRef) {
    register_builtin_family!(heap, env,
        "vector" => vector,
        "make-vector" => make_vector,
        "vector-length" => vector_length,
        "vector-ref" => vector_ref,
        "vector-set!" => vector_set,
        "vector->list" => vector_to_list,
        "list->vector" => list_to_vector,
        "vector-fill!" => vector_fill,
    );
}

/// Creates a new vector from a list of arguments.
/// (vector arg1 arg2 ...)
pub fn vector(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let values = args.to_vec();
    Ok(new_vector(heap, values))
}

/// Makes a vector of specified length and optionally initializes it with a default value.
/// (make-vector length [default])
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

/// (vector->list vector) -> list
fn vector_to_list(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("vector->list: expects exactly 1 argument".to_string());
    }

    match gc_value!(args[0]) {
        SchemeValue::Vector(v) => Ok(list_from_slice(&v[..], heap)),
        _ => Err("vector->list: argument must be a vector".to_string()),
    }
}

/// (list->vector list) -> vector
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

/// (vector-fill! vector fill) -> unspecified
fn vector_fill(_heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("vector-fill!: expects a vector and a fill value".to_string());
    }

    let fill_val = args[1];
    match gc_value_mut!(args[0]) {
        SchemeValue::Vector(v) => {
            for elem in v.iter_mut() {
                *elem = fill_val;
            }
        }
        _ => return Err("vector-fill!: first argument must be a vector".to_string()),
    };

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
            assert_eq!(*heap.get_value(v[0]), SchemeValue::Int(BigInt::from(1)));
            assert_eq!(*heap.get_value(v[1]), SchemeValue::Bool(true));
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
                assert_eq!(*heap.get_value(*elem), SchemeValue::Nil);
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
                assert_eq!(*heap.get_value(*elem), SchemeValue::Char('a'));
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
        assert_eq!(*heap.get_value(len), SchemeValue::Int(BigInt::from(1)));

        let empty_vec = vector(heap, &[]).unwrap();
        let len = vector_length(heap, &[empty_vec]).unwrap();
        assert_eq!(*heap.get_value(len), SchemeValue::Int(BigInt::from(0)));

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
