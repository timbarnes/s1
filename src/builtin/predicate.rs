use crate::env::{EnvOps, EnvRef};
use crate::gc::{Callable, GcHeap, GcRef, SchemeValue, get_symbol, new_bool};
use crate::gc_value;

pub fn number_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let arg = args.get(0).ok_or("number?: expected 1 argument")?;
    let is_number = match &gc_value!(*arg) {
        SchemeValue::Int(_) | SchemeValue::Float(_) => true,
        _ => false,
    };
    Ok(new_bool(heap, is_number))
}

pub fn equal_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("equal?: expected exactly 2 arguments".to_string());
    }

    let is_equal = crate::gc::equal(heap, args[0], args[1]);
    Ok(new_bool(heap, is_equal))
}

pub fn eq_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("eq?: expected exactly 2 arguments".to_string());
    }

    let is_equal = crate::gc::eq(heap, args[0], args[1]);
    Ok(new_bool(heap, is_equal))
}

pub fn type_of(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let arg = args.get(0).ok_or("type-of: expected 1 argument")?;
    let type_name = match &gc_value!(*arg) {
        SchemeValue::Int(_) => "integer",
        SchemeValue::Float(_) => "float",
        SchemeValue::Symbol(_) => "symbol",
        SchemeValue::Pair(_, _) => "pair",
        SchemeValue::Str(_) => "string",
        SchemeValue::Vector(_) => "vector",
        SchemeValue::Bool(_) => "boolean",
        SchemeValue::Char(_) => "char",
        SchemeValue::Callable(c) => match c {
            Callable::Builtin { .. } => "builtin",
            Callable::SpecialForm { .. } => "special-form",
            Callable::Closure { .. } => "closure",
            Callable::Macro { .. } => "macro",
            Callable::SysBuiltin { .. } => "sys-builtin",
        },
        SchemeValue::Nil => "null",
        _ => "unknown",
    };
    Ok(get_symbol(heap, type_name))
}

macro_rules! register_builtin_family {
    ($heap:expr, $env:expr, $($name:expr => $func:expr),* $(,)?) => {
        $(
            $env.define($heap.intern_symbol($name),
                crate::gc::new_builtin($heap, $func,
                    concat!($name, ": builtin function").to_string()));
        )*
    };
}

pub fn register_predicate_builtins(heap: &mut crate::gc::GcHeap, env: EnvRef) {
    register_builtin_family!(heap, env,
        "type-of" => type_of,
        "equal?" => equal_q,
        "number?" => number_q,
        "eq?" => eq_q,
    );
}

mod tests {
    use super::*;
    use crate::gc_value;

    #[test]
    fn test_number_q() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);

        // Test with integer
        let int_val = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(42));
        let result = number_q(&mut ec.heap, &[int_val]);
        assert!(result.is_ok());
        assert!(matches!(
            &gc_value!(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with float
        let float_val = crate::gc::new_float(ec.heap, 3.14);
        let result = number_q(&mut ec.heap, &[float_val]);
        assert!(result.is_ok());
        assert!(matches!(
            &gc_value!(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with non-number
        let sym_val = crate::gc::get_symbol(ec.heap, "foo");
        let result = number_q(&mut ec.heap, &[sym_val]);
        assert!(result.is_ok());
        assert!(matches!(
            &gc_value!(result.unwrap()),
            SchemeValue::Bool(false)
        ));

        // Test error case: no arguments
        let result = number_q(&mut ec.heap, &[]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expected 1 argument"));
    }

    #[test]
    fn test_eq_equal_q() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);

        // Test with equal integers
        let int1 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(42));
        let int2 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(42));
        let result = eq_q(&mut ec.heap, &[int1, int2]);
        assert!(result.is_ok());
        assert!(matches!(
            &gc_value!(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with unequal integers
        let int3 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let int4 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let result = eq_q(&mut ec.heap, &[int3, int4]);
        assert!(result.is_ok());
        assert!(matches!(
            &gc_value!(result.unwrap()),
            SchemeValue::Bool(false)
        ));

        // Test with equal floats
        let float1 = crate::gc::new_float(ec.heap, 3.14);
        let float2 = crate::gc::new_float(ec.heap, 3.14);
        let result = eq_q(&mut ec.heap, &[float1, float2]);
        assert!(result.is_ok());
        assert!(matches!(
            &gc_value!(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with unequal floats
        let float3 = crate::gc::new_float(ec.heap, 1.0);
        let float4 = crate::gc::new_float(ec.heap, 2.0);
        let result = eq_q(&mut ec.heap, &[float3, float4]);
        assert!(result.is_ok());
        assert!(matches!(
            &gc_value!(result.unwrap()),
            SchemeValue::Bool(false)
        ));

        // Test with equal symbols
        let sym1 = crate::gc::get_symbol(ec.heap, "foo");
        let sym2 = crate::gc::get_symbol(ec.heap, "foo");
        let result = eq_q(&mut ec.heap, &[sym1, sym2]);
        assert!(result.is_ok());
        assert!(matches!(
            &gc_value!(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with unequal symbols
        let sym3 = crate::gc::get_symbol(ec.heap, "bar");
        let sym4 = crate::gc::get_symbol(ec.heap, "baz");
        let result = eq_q(&mut ec.heap, &[sym3, sym4]);
        assert!(result.is_ok());
        assert!(matches!(
            &gc_value!(result.unwrap()),
            SchemeValue::Bool(false)
        ));

        // Test with equal pairs
        let car1 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let cdr1 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let pair1 = crate::gc::new_pair(ec.heap, car1, cdr1);
        let car2 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let cdr2 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let pair2 = crate::gc::new_pair(ec.heap, car2, cdr2);
        let result = equal_q(&mut ec.heap, &[pair1, pair2]);
        assert!(result.is_ok());
        assert!(matches!(
            &gc_value!(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with unequal pairs
        let car3 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let cdr3 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let pair3 = crate::gc::new_pair(ec.heap, car3, cdr3);
        let car4 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(3));
        let cdr4 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(4));
        let pair4 = crate::gc::new_pair(ec.heap, car4, cdr4);
        let result = equal_q(&mut ec.heap, &[pair3, pair4]);
        assert!(result.is_ok());
        assert!(matches!(
            &gc_value!(result.unwrap()),
            SchemeValue::Bool(false)
        ));

        // Test with equal strings
        let str1 = crate::gc::new_string(ec.heap, "hello");
        let str2 = crate::gc::new_string(ec.heap, "hello");
        let result = equal_q(&mut ec.heap, &[str1, str2]);
        assert!(result.is_ok());
        assert!(matches!(
            &gc_value!(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with unequal strings
        let str3 = crate::gc::new_string(ec.heap, "world");
        let str4 = crate::gc::new_string(ec.heap, "hello");
        let result = equal_q(&mut ec.heap, &[str3, str4]);
        assert!(result.is_ok());
        assert!(matches!(
            &gc_value!(result.unwrap()),
            SchemeValue::Bool(false)
        ));

        // Test with equal vectors
        let vec_elem1 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let vec1 = crate::gc::new_vector(ec.heap, vec![vec_elem1]);
        let vec_elem2 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let vec2 = crate::gc::new_vector(ec.heap, vec![vec_elem2]);
        let result = equal_q(&mut ec.heap, &[vec1, vec2]);
        assert!(result.is_ok());
        assert!(matches!(
            &gc_value!(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with unequal vectors
        let vec_elem3 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let vec3 = crate::gc::new_vector(ec.heap, vec![vec_elem3]);
        let vec_elem4 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let vec4 = crate::gc::new_vector(ec.heap, vec![vec_elem4]);
        let result = equal_q(&mut ec.heap, &[vec3, vec4]);
        assert!(result.is_ok());
        assert!(matches!(
            &gc_value!(result.unwrap()),
            SchemeValue::Bool(false)
        ));

        // Test with equal booleans
        let bool1 = crate::gc::new_bool(ec.heap, true);
        let bool2 = crate::gc::new_bool(ec.heap, true);
        let result = eq_q(&mut ec.heap, &[bool1, bool2]);
        assert!(result.is_ok());
        assert!(matches!(
            &gc_value!(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with unequal booleans
        let bool3 = crate::gc::new_bool(ec.heap, true);
        let bool4 = crate::gc::new_bool(ec.heap, false);
        let result = eq_q(&mut ec.heap, &[bool3, bool4]);
        assert!(result.is_ok());
        assert!(matches!(
            &gc_value!(result.unwrap()),
            SchemeValue::Bool(false)
        ));

        // Test with equal characters
        let char1 = crate::gc::new_char(ec.heap, 'a');
        let char2 = crate::gc::new_char(ec.heap, 'a');
        let result = eq_q(&mut ec.heap, &[char1, char2]);
        assert!(result.is_ok());
        assert!(matches!(
            &gc_value!(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with unequal characters
        let char3 = crate::gc::new_char(ec.heap, 'a');
        let char4 = crate::gc::new_char(ec.heap, 'b');
        let result = eq_q(&mut ec.heap, &[char3, char4]);
        assert!(result.is_ok());
        assert!(matches!(
            &gc_value!(result.unwrap()),
            SchemeValue::Bool(false)
        ));

        // Test with equal nil
        let nil1 = ec.heap.nil_s();
        let nil2 = ec.heap.nil_s();
        let result = eq_q(&mut ec.heap, &[nil1, nil2]);
        assert!(result.is_ok());
        assert!(matches!(
            &gc_value!(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with unequal nil
        let nil3 = ec.heap.nil_s();
        let nil4 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let result = eq_q(&mut ec.heap, &[nil3, nil4]);
        assert!(result.is_ok());
        assert!(matches!(
            &gc_value!(result.unwrap()),
            SchemeValue::Bool(false)
        ));
    }

    #[test]

    fn test_type_of() {
        fn as_type(val: &GcRef) -> &'static str {
            match &gc_value!(*val) {
                SchemeValue::Symbol(s) => match s.as_str() {
                    "integer" => "integer",
                    "float" => "float",
                    "symbol" => "symbol",
                    "pair" => "pair",
                    "string" => "string",
                    "vector" => "vector",
                    "boolean" => "boolean",
                    "char" => "char",
                    "primitive" => "primitive",
                    "closure" => "closure",
                    "null" => "null",
                    _ => panic!("unexpected symbol from type_of: {}", s),
                },
                _ => panic!("type_of did not return a symbol"),
            }
        }

        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);

        // Test with integer
        let int_val = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(42));
        let result = type_of(&mut ec.heap, &[int_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "integer");

        // Test with float
        let float_val = crate::gc::new_float(ec.heap, 3.14);
        let result = type_of(&mut ec.heap, &[float_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "float");

        // Test with symbol
        let sym_val = crate::gc::get_symbol(ec.heap, "foo");
        let result = type_of(&mut ec.heap, &[sym_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "symbol");

        // Test with pair
        let car = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let cdr = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let pair_val = crate::gc::new_pair(ec.heap, car, cdr);
        let result = type_of(&mut ec.heap, &[pair_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "pair");

        // Test with string
        let str_val = crate::gc::new_string(ec.heap, "hello");
        let result = type_of(&mut ec.heap, &[str_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "string");

        // Test with vector
        let vec_elem = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let vec_val = crate::gc::new_vector(ec.heap, vec![vec_elem]);
        let result = type_of(&mut ec.heap, &[vec_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "vector");

        // Test with boolean
        let bool_val = crate::gc::new_bool(ec.heap, true);
        let result = type_of(&mut ec.heap, &[bool_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "boolean");

        // Test with character
        let char_val = crate::gc::new_char(ec.heap, 'a');
        let result = type_of(&mut ec.heap, &[char_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "char");

        // Test with nil
        let nil_val = ec.heap.nil_s();
        let result = type_of(&mut ec.heap, &[nil_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "null");

        // Test error case: no arguments
        let result = type_of(&mut ec.heap, &[]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expected 1 argument"));
    }
}
