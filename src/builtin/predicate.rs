use crate::gc::{GcHeap, GcRefSimple, new_bool_simple, new_symbol_simple, SchemeValueSimple};

pub fn number_q(heap: &mut GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    let arg = args.get(0).ok_or("number?: expected 1 argument")?;
    let is_number = match &arg.value {
        SchemeValueSimple::Int(_) | SchemeValueSimple::Float(_) => true,
        _ => false,
    };
    Ok(new_bool_simple(heap, is_number))
}

pub fn eq_q(heap: &mut GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if args.len() != 2 {
        return Err("eq?: expected exactly 2 arguments".to_string());
    }
    
    let val1 = &args[0];
    let val2 = &args[1];
    
    // Use the existing PartialEq implementation
    let is_equal = val1.value == val2.value;
    Ok(new_bool_simple(heap, is_equal))
}

pub fn type_of(heap: &mut GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    let arg = args.get(0).ok_or("type-of: expected 1 argument")?;
    let type_name = match &arg.value {
        SchemeValueSimple::Int(_) => "integer",
        SchemeValueSimple::Float(_) => "float",
        SchemeValueSimple::Symbol(_) => "symbol",
        SchemeValueSimple::Pair(_, _) => "pair",
        SchemeValueSimple::Str(_) => "string",
        SchemeValueSimple::Vector(_) => "vector",
        SchemeValueSimple::Bool(_) => "boolean",
        SchemeValueSimple::Char(_) => "char",
        SchemeValueSimple::Primitive { .. } => "primitive",
        SchemeValueSimple::Closure { .. } => "closure",
        SchemeValueSimple::Nil => "nil",
        _ => "unknown",
    };
    Ok(new_symbol_simple(heap, type_name))
}

mod tests {
    use super::*;

    #[test]
    fn test_number_q() {
        let mut heap = GcHeap::new();
        
        // Test with integer
        let int_val = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(42));
        let result = number_q(&mut heap, &[int_val]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(true)));
        
        // Test with float
        let float_val = crate::gc::new_float_simple(&mut heap, 3.14);
        let result = number_q(&mut heap, &[float_val]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(true)));
        
        // Test with non-number
        let sym_val = crate::gc::new_symbol_simple(&mut heap, "foo");
        let result = number_q(&mut heap, &[sym_val]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(false)));
        
        // Test error case: no arguments
        let result = number_q(&mut heap, &[]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expected 1 argument"));
    }

    #[test]
    fn test_eq_q() {
        let mut heap = GcHeap::new();
        
        // Test with equal integers
        let int1 = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(42));
        let int2 = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(42));
        let result = eq_q(&mut heap, &[int1, int2]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(true)));
        
        // Test with unequal integers
        let int3 = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(1));
        let int4 = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(2));
        let result = eq_q(&mut heap, &[int3, int4]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(false)));
        
        // Test with equal floats
        let float1 = crate::gc::new_float_simple(&mut heap, 3.14);
        let float2 = crate::gc::new_float_simple(&mut heap, 3.14);
        let result = eq_q(&mut heap, &[float1, float2]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(true)));
        
        // Test with unequal floats
        let float3 = crate::gc::new_float_simple(&mut heap, 1.0);
        let float4 = crate::gc::new_float_simple(&mut heap, 2.0);
        let result = eq_q(&mut heap, &[float3, float4]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(false)));
        
        // Test with equal symbols
        let sym1 = crate::gc::new_symbol_simple(&mut heap, "foo");
        let sym2 = crate::gc::new_symbol_simple(&mut heap, "foo");
        let result = eq_q(&mut heap, &[sym1, sym2]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(true)));
        
        // Test with unequal symbols
        let sym3 = crate::gc::new_symbol_simple(&mut heap, "bar");
        let sym4 = crate::gc::new_symbol_simple(&mut heap, "baz");
        let result = eq_q(&mut heap, &[sym3, sym4]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(false)));
        
        // Test with equal pairs
        let car1 = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(1));
        let cdr1 = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(2));
        let pair1 = crate::gc::new_pair_simple(&mut heap, car1, cdr1);
        let car2 = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(1));
        let cdr2 = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(2));
        let pair2 = crate::gc::new_pair_simple(&mut heap, car2, cdr2);
        let result = eq_q(&mut heap, &[pair1, pair2]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(true)));
        
        // Test with unequal pairs
        let car3 = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(1));
        let cdr3 = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(2));
        let pair3 = crate::gc::new_pair_simple(&mut heap, car3, cdr3);
        let car4 = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(3));
        let cdr4 = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(4));
        let pair4 = crate::gc::new_pair_simple(&mut heap, car4, cdr4);
        let result = eq_q(&mut heap, &[pair3, pair4]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(false)));
        
        // Test with equal strings
        let str1 = crate::gc::new_string_simple(&mut heap, "hello");
        let str2 = crate::gc::new_string_simple(&mut heap, "hello");
        let result = eq_q(&mut heap, &[str1, str2]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(true)));
        
        // Test with unequal strings
        let str3 = crate::gc::new_string_simple(&mut heap, "world");
        let str4 = crate::gc::new_string_simple(&mut heap, "hello");
        let result = eq_q(&mut heap, &[str3, str4]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(false)));
        
        // Test with equal vectors
        let vec_elem1 = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(1));
        let vec1 = crate::gc::new_vector_simple(&mut heap, vec![vec_elem1]);
        let vec_elem2 = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(1));
        let vec2 = crate::gc::new_vector_simple(&mut heap, vec![vec_elem2]);
        let result = eq_q(&mut heap, &[vec1, vec2]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(true)));
        
        // Test with unequal vectors
        let vec_elem3 = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(1));
        let vec3 = crate::gc::new_vector_simple(&mut heap, vec![vec_elem3]);
        let vec_elem4 = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(2));
        let vec4 = crate::gc::new_vector_simple(&mut heap, vec![vec_elem4]);
        let result = eq_q(&mut heap, &[vec3, vec4]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(false)));
        
        // Test with equal booleans
        let bool1 = crate::gc::new_bool_simple(&mut heap, true);
        let bool2 = crate::gc::new_bool_simple(&mut heap, true);
        let result = eq_q(&mut heap, &[bool1, bool2]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(true)));
        
        // Test with unequal booleans
        let bool3 = crate::gc::new_bool_simple(&mut heap, true);
        let bool4 = crate::gc::new_bool_simple(&mut heap, false);
        let result = eq_q(&mut heap, &[bool3, bool4]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(false)));
        
        // Test with equal characters
        let char1 = crate::gc::new_char_simple(&mut heap, 'a');
        let char2 = crate::gc::new_char_simple(&mut heap, 'a');
        let result = eq_q(&mut heap, &[char1, char2]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(true)));
        
        // Test with unequal characters
        let char3 = crate::gc::new_char_simple(&mut heap, 'a');
        let char4 = crate::gc::new_char_simple(&mut heap, 'b');
        let result = eq_q(&mut heap, &[char3, char4]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(false)));
        
        // Test with equal nil
        let nil1 = crate::gc::new_nil_simple(&mut heap);
        let nil2 = crate::gc::new_nil_simple(&mut heap);
        let result = eq_q(&mut heap, &[nil1, nil2]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(true)));
        
        // Test with unequal nil
        let nil3 = crate::gc::new_nil_simple(&mut heap);
        let nil4 = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(1));
        let result = eq_q(&mut heap, &[nil3, nil4]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(false)));
    }

    #[test]
    fn test_type_of() {
        let mut heap = GcHeap::new();
        
        // Test with integer
        let int_val = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(42));
        let result = type_of(&mut heap, &[int_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "integer");
        
        // Test with float
        let float_val = crate::gc::new_float_simple(&mut heap, 3.14);
        let result = type_of(&mut heap, &[float_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "float");
        
        // Test with symbol
        let sym_val = crate::gc::new_symbol_simple(&mut heap, "foo");
        let result = type_of(&mut heap, &[sym_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "symbol");
        
        // Test with pair
        let car = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(1));
        let cdr = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(2));
        let pair_val = crate::gc::new_pair_simple(&mut heap, car, cdr);
        let result = type_of(&mut heap, &[pair_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "pair");
        
        // Test with string
        let str_val = crate::gc::new_string_simple(&mut heap, "hello");
        let result = type_of(&mut heap, &[str_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "string");
        
        // Test with vector
        let vec_elem = crate::gc::new_int_simple(&mut heap, num_bigint::BigInt::from(1));
        let vec_val = crate::gc::new_vector_simple(&mut heap, vec![vec_elem]);
        let result = type_of(&mut heap, &[vec_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "vector");
        
        // Test with boolean
        let bool_val = crate::gc::new_bool_simple(&mut heap, true);
        let result = type_of(&mut heap, &[bool_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "boolean");
        
        // Test with character
        let char_val = crate::gc::new_char_simple(&mut heap, 'a');
        let result = type_of(&mut heap, &[char_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "char");
        
        // Test with nil
        let nil_val = crate::gc::new_nil_simple(&mut heap);
        let result = type_of(&mut heap, &[nil_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "nil");
        
        // Test error case: no arguments
        let result = type_of(&mut heap, &[]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expected 1 argument"));
    }

    fn as_type(val: &GcRefSimple) -> &'static str {
        match &val.value {
            SchemeValueSimple::Symbol(s) => match s.as_str() {
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
                "nil" => "nil",
                _ => panic!("unexpected symbol from type_of: {}", s),
            },
            _ => panic!("type_of did not return a symbol"),
        }
    }
} 