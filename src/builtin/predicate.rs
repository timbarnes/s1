use crate::gc::{GcHeap, GcRefSimple, new_bool_simple, new_symbol_simple, SchemeValueSimple};

pub fn number_q(heap: &mut GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    let arg = args.get(0).ok_or("number?: expected 1 argument")?;
    let is_number = match &arg.value {
        SchemeValueSimple::Int(_) | SchemeValueSimple::Float(_) => true,
        _ => false,
    };
    Ok(new_bool_simple(heap, is_number))
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
    use crate::gc::{new_int_simple, new_float_simple, new_symbol_simple, new_pair_simple, new_string_simple, new_vector_simple, new_bool_simple, new_char_simple, new_nil_simple};
    use num_bigint::BigInt;
    
    #[test]
    fn test_number_q() {
        let mut heap = GcHeap::new();
        
        // Test with integer
        let int_val = new_int_simple(&mut heap, BigInt::from(42));
        let result = number_q(&mut heap, &[int_val]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(true)));
        
        // Test with float
        let float_val = new_float_simple(&mut heap, 3.14);
        let result = number_q(&mut heap, &[float_val]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(true)));
        
        // Test with non-number
        let sym_val = new_symbol_simple(&mut heap, "foo");
        let result = number_q(&mut heap, &[sym_val]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(false)));
        
        // Test error case: no arguments
        let result = number_q(&mut heap, &[]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expected 1 argument"));
    }

    #[test]
    fn test_type_of() {
        let mut heap = GcHeap::new();
        
        // Test with integer
        let int_val = new_int_simple(&mut heap, BigInt::from(42));
        let result = type_of(&mut heap, &[int_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "integer");
        
        // Test with float
        let float_val = new_float_simple(&mut heap, 3.14);
        let result = type_of(&mut heap, &[float_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "float");
        
        // Test with symbol
        let sym_val = new_symbol_simple(&mut heap, "foo");
        let result = type_of(&mut heap, &[sym_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "symbol");
        
        // Test with pair
        let car = new_int_simple(&mut heap, BigInt::from(1));
        let cdr = new_int_simple(&mut heap, BigInt::from(2));
        let pair_val = new_pair_simple(&mut heap, car, cdr);
        let result = type_of(&mut heap, &[pair_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "pair");
        
        // Test with string
        let str_val = new_string_simple(&mut heap, "hello");
        let result = type_of(&mut heap, &[str_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "string");
        
        // Test with vector
        let vec_elem = new_int_simple(&mut heap, BigInt::from(1));
        let vec_val = new_vector_simple(&mut heap, vec![vec_elem]);
        let result = type_of(&mut heap, &[vec_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "vector");
        
        // Test with boolean
        let bool_val = new_bool_simple(&mut heap, true);
        let result = type_of(&mut heap, &[bool_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "boolean");
        
        // Test with character
        let char_val = new_char_simple(&mut heap, 'a');
        let result = type_of(&mut heap, &[char_val]);
        assert!(result.is_ok());
        assert_eq!(as_type(&result.unwrap()), "char");
        
        // Test with nil
        let nil_val = new_nil_simple(&mut heap);
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