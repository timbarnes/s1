use crate::eval::EvalContext;
use crate::gc::{Callable, GcRef, SchemeValue, get_symbol, new_bool};

pub fn number_q(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    let arg = args.get(0).ok_or("number?: expected 1 argument")?;
    let is_number = match &ec.heap.get_value(*arg) {
        SchemeValue::Int(_) | SchemeValue::Float(_) => true,
        _ => false,
    };
    Ok(new_bool(ec.heap, is_number))
}

pub fn eq_q(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("eq?: expected exactly 2 arguments".to_string());
    }

    let is_equal = crate::gc::eq(ec.heap, args[0], args[1]);
    Ok(new_bool(ec.heap, is_equal))
}

pub fn type_of(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    let arg = args.get(0).ok_or("type-of: expected 1 argument")?;
    let type_name = match &ec.heap.get_value(*arg) {
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
        },
        SchemeValue::Nil => "null",
        _ => "unknown",
    };
    Ok(get_symbol(ec.heap, type_name))
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

pub fn register_predicate_builtins(
    heap: &mut crate::gc::GcHeap,
    env: &mut crate::env::Environment,
) {
    register_builtin_family!(heap, env,
        "type-of" => type_of,
        "eq?" => eq_q,
        "number?" => number_q,
    );
}

mod tests {
    use super::*;

    #[test]
    fn test_number_q() {
        let mut ev = crate::eval::Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut ev);

        // Test with integer
        let int_val = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(42));
        let result = number_q(&mut ec, &[int_val]);
        assert!(result.is_ok());
        assert!(matches!(
            &ec.heap.get_value(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with float
        let float_val = crate::gc::new_float(ec.heap, 3.14);
        let result = number_q(&mut ec, &[float_val]);
        assert!(result.is_ok());
        assert!(matches!(
            &ec.heap.get_value(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with non-number
        let sym_val = crate::gc::get_symbol(ec.heap, "foo");
        let result = number_q(&mut ec, &[sym_val]);
        assert!(result.is_ok());
        assert!(matches!(
            &ec.heap.get_value(result.unwrap()),
            SchemeValue::Bool(false)
        ));

        // Test error case: no arguments
        let result = number_q(&mut ec, &[]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expected 1 argument"));
    }

    #[test]
    fn test_eq_q() {
        let mut ev = crate::eval::Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut ev);

        // Test with equal integers
        let int1 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(42));
        let int2 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(42));
        let result = eq_q(&mut ec, &[int1, int2]);
        assert!(result.is_ok());
        assert!(matches!(
            &ec.heap.get_value(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with unequal integers
        let int3 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let int4 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let result = eq_q(&mut ec, &[int3, int4]);
        assert!(result.is_ok());
        assert!(matches!(
            &ec.heap.get_value(result.unwrap()),
            SchemeValue::Bool(false)
        ));

        // Test with equal floats
        let float1 = crate::gc::new_float(ec.heap, 3.14);
        let float2 = crate::gc::new_float(ec.heap, 3.14);
        let result = eq_q(&mut ec, &[float1, float2]);
        assert!(result.is_ok());
        assert!(matches!(
            &ec.heap.get_value(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with unequal floats
        let float3 = crate::gc::new_float(ec.heap, 1.0);
        let float4 = crate::gc::new_float(ec.heap, 2.0);
        let result = eq_q(&mut ec, &[float3, float4]);
        assert!(result.is_ok());
        assert!(matches!(
            &ec.heap.get_value(result.unwrap()),
            SchemeValue::Bool(false)
        ));

        // Test with equal symbols
        let sym1 = crate::gc::get_symbol(ec.heap, "foo");
        let sym2 = crate::gc::get_symbol(ec.heap, "foo");
        let result = eq_q(&mut ec, &[sym1, sym2]);
        assert!(result.is_ok());
        assert!(matches!(
            &ec.heap.get_value(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with unequal symbols
        let sym3 = crate::gc::get_symbol(ec.heap, "bar");
        let sym4 = crate::gc::get_symbol(ec.heap, "baz");
        let result = eq_q(&mut ec, &[sym3, sym4]);
        assert!(result.is_ok());
        assert!(matches!(
            &ec.heap.get_value(result.unwrap()),
            SchemeValue::Bool(false)
        ));

        // Test with equal pairs
        let car1 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let cdr1 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let pair1 = crate::gc::new_pair(ec.heap, car1, cdr1);
        let car2 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let cdr2 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let pair2 = crate::gc::new_pair(ec.heap, car2, cdr2);
        let result = eq_q(&mut ec, &[pair1, pair2]);
        assert!(result.is_ok());
        assert!(matches!(
            &ec.heap.get_value(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with unequal pairs
        let car3 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let cdr3 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let pair3 = crate::gc::new_pair(ec.heap, car3, cdr3);
        let car4 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(3));
        let cdr4 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(4));
        let pair4 = crate::gc::new_pair(ec.heap, car4, cdr4);
        let result = eq_q(&mut ec, &[pair3, pair4]);
        assert!(result.is_ok());
        assert!(matches!(
            &ec.heap.get_value(result.unwrap()),
            SchemeValue::Bool(false)
        ));

        // Test with equal strings
        let str1 = crate::gc::new_string(ec.heap, "hello");
        let str2 = crate::gc::new_string(ec.heap, "hello");
        let result = eq_q(&mut ec, &[str1, str2]);
        assert!(result.is_ok());
        assert!(matches!(
            &ec.heap.get_value(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with unequal strings
        let str3 = crate::gc::new_string(ec.heap, "world");
        let str4 = crate::gc::new_string(ec.heap, "hello");
        let result = eq_q(&mut ec, &[str3, str4]);
        assert!(result.is_ok());
        assert!(matches!(
            &ec.heap.get_value(result.unwrap()),
            SchemeValue::Bool(false)
        ));

        // Test with equal vectors
        let vec_elem1 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let vec1 = crate::gc::new_vector(ec.heap, vec![vec_elem1]);
        let vec_elem2 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let vec2 = crate::gc::new_vector(ec.heap, vec![vec_elem2]);
        let result = eq_q(&mut ec, &[vec1, vec2]);
        assert!(result.is_ok());
        assert!(matches!(
            &ec.heap.get_value(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with unequal vectors
        let vec_elem3 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let vec3 = crate::gc::new_vector(ec.heap, vec![vec_elem3]);
        let vec_elem4 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let vec4 = crate::gc::new_vector(ec.heap, vec![vec_elem4]);
        let result = eq_q(&mut ec, &[vec3, vec4]);
        assert!(result.is_ok());
        assert!(matches!(
            &ec.heap.get_value(result.unwrap()),
            SchemeValue::Bool(false)
        ));

        // Test with equal booleans
        let bool1 = crate::gc::new_bool(ec.heap, true);
        let bool2 = crate::gc::new_bool(ec.heap, true);
        let result = eq_q(&mut ec, &[bool1, bool2]);
        assert!(result.is_ok());
        assert!(matches!(
            &ec.heap.get_value(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with unequal booleans
        let bool3 = crate::gc::new_bool(ec.heap, true);
        let bool4 = crate::gc::new_bool(ec.heap, false);
        let result = eq_q(&mut ec, &[bool3, bool4]);
        assert!(result.is_ok());
        assert!(matches!(
            &ec.heap.get_value(result.unwrap()),
            SchemeValue::Bool(false)
        ));

        // Test with equal characters
        let char1 = crate::gc::new_char(ec.heap, 'a');
        let char2 = crate::gc::new_char(ec.heap, 'a');
        let result = eq_q(&mut ec, &[char1, char2]);
        assert!(result.is_ok());
        assert!(matches!(
            &ec.heap.get_value(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with unequal characters
        let char3 = crate::gc::new_char(ec.heap, 'a');
        let char4 = crate::gc::new_char(ec.heap, 'b');
        let result = eq_q(&mut ec, &[char3, char4]);
        assert!(result.is_ok());
        assert!(matches!(
            &ec.heap.get_value(result.unwrap()),
            SchemeValue::Bool(false)
        ));

        // Test with equal nil
        let nil1 = crate::gc::get_nil(ec.heap);
        let nil2 = crate::gc::get_nil(ec.heap);
        let result = eq_q(&mut ec, &[nil1, nil2]);
        assert!(result.is_ok());
        assert!(matches!(
            &ec.heap.get_value(result.unwrap()),
            SchemeValue::Bool(true)
        ));

        // Test with unequal nil
        let nil3 = crate::gc::get_nil(ec.heap);
        let nil4 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let result = eq_q(&mut ec, &[nil3, nil4]);
        assert!(result.is_ok());
        assert!(matches!(
            &ec.heap.get_value(result.unwrap()),
            SchemeValue::Bool(false)
        ));
    }

    #[test]
    fn test_type_of() {
        let mut ev = crate::eval::Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut ev);

        // Test with integer
        let int_val = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(42));
        let result = type_of(&mut ec, &[int_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&mut ec, &result.unwrap()), "integer");

        // Test with float
        let float_val = crate::gc::new_float(ec.heap, 3.14);
        let result = type_of(&mut ec, &[float_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&mut ec, &result.unwrap()), "float");

        // Test with symbol
        let sym_val = crate::gc::get_symbol(ec.heap, "foo");
        let result = type_of(&mut ec, &[sym_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&mut ec, &result.unwrap()), "symbol");

        // Test with pair
        let car = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let cdr = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let pair_val = crate::gc::new_pair(ec.heap, car, cdr);
        let result = type_of(&mut ec, &[pair_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&mut ec, &result.unwrap()), "pair");

        // Test with string
        let str_val = crate::gc::new_string(ec.heap, "hello");
        let result = type_of(&mut ec, &[str_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&mut ec, &result.unwrap()), "string");

        // Test with vector
        let vec_elem = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let vec_val = crate::gc::new_vector(ec.heap, vec![vec_elem]);
        let result = type_of(&mut ec, &[vec_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&mut ec, &result.unwrap()), "vector");

        // Test with boolean
        let bool_val = crate::gc::new_bool(ec.heap, true);
        let result = type_of(&mut ec, &[bool_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&mut ec, &result.unwrap()), "boolean");

        // Test with character
        let char_val = crate::gc::new_char(ec.heap, 'a');
        let result = type_of(&mut ec, &[char_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&mut ec, &result.unwrap()), "char");

        // Test with nil
        let nil_val = crate::gc::get_nil(ec.heap);
        let result = type_of(&mut ec, &[nil_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&mut ec, &result.unwrap()), "null");

        // Test error case: no arguments
        let result = type_of(&mut ec, &[]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expected 1 argument"));
    }

    fn as_type(ec: &mut EvalContext, val: &GcRef) -> &'static str {
        match &ec.heap.get_value(*val) {
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
}
