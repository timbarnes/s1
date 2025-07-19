use crate::gc::{GcHeap, GcRef, SchemeValue, new_bool, new_symbol};
use crate::gc::{GcRefSimple, new_bool_simple, new_symbol_simple, SchemeValueSimple};

pub fn number_q(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let arg = args.get(0).ok_or("number?: expected 1 argument")?;
    let is_number = match &arg.borrow().value {
        SchemeValue::Int(_) | SchemeValue::Float(_) => true,
        _ => false,
    };
    Ok(new_bool(heap, is_number))
}

pub fn type_of(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    let arg = args.get(0).ok_or("type-of: expected 1 argument")?;
    let type_name = match &arg.borrow().value {
        SchemeValue::Int(_) => "integer",
        SchemeValue::Float(_) => "float",
        SchemeValue::Symbol(_) => "symbol",
        SchemeValue::Pair(_, _) => "pair",
        SchemeValue::Str(_) => "string",
        SchemeValue::Vector(_) => "vector",
        SchemeValue::Closure { .. } => "closure",
        SchemeValue::Bool(_) => "boolean",
        SchemeValue::Char(_) => "char",
        SchemeValue::Primitive { .. } => "primitive",
        SchemeValue::EnvFrame(_) => "env-frame",
        SchemeValue::Nil => "nil",
        _ => "unknown",
    };
    Ok(new_symbol(heap, type_name))
}

// ============================================================================
// SIMPLE PREDICATE BUILTINS (Reference-based GC system)
// ============================================================================

pub fn number_q_simple(heap: &mut GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    let arg = args.get(0).ok_or("number?: expected 1 argument")?;
    let is_number = match &arg.value {
        SchemeValueSimple::Int(_) | SchemeValueSimple::Float(_) => true,
        _ => false,
    };
    Ok(new_bool_simple(heap, is_number))
}

pub fn type_of_simple(heap: &mut GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
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
        SchemeValueSimple::Nil => "nil",
        _ => "unknown",
    };
    Ok(new_symbol_simple(heap, type_name))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gc::{GcHeap, new_int, new_float, new_symbol, new_pair, new_string, new_vector, new_bool, new_char, new_primitive, new_env_frame, new_nil};
    use crate::gc::{new_int_simple, new_float_simple, new_symbol_simple};
    use std::rc::Rc;
    use num_bigint::BigInt;
    
    #[test]
    fn test_type_of() {
        let mut heap = GcHeap::new();
        let int_val = new_int(&mut heap, BigInt::from(42));
        let float_val = new_float(&mut heap, 3.14);
        let sym_val = new_symbol(&mut heap, "foo");
        let pair_car = new_int(&mut heap, BigInt::from(1));
        let pair_cdr = new_int(&mut heap, BigInt::from(2));
        let pair_val = new_pair(&mut heap, pair_car, pair_cdr);
        let str_val = new_string(&mut heap, "hi");
        let vec_elem = new_int(&mut heap, BigInt::from(1));
        let vec_val = new_vector(&mut heap, vec![vec_elem]);
        let clo_body = new_symbol(&mut heap, "body");
        let clo_env = new_nil(&mut heap);
        let param_x = new_symbol(&mut heap, "x");
        let clo_val = crate::gc::new_closure(&mut heap, vec![param_x], clo_body, clo_env);
        let bool_val = new_bool(&mut heap, true);
        let char_val = new_char(&mut heap, 'a');
        let env_val = new_env_frame(&mut heap, std::collections::HashMap::new());
        let nil_val = new_nil(&mut heap);
        let prim_val = new_primitive(&mut heap, Rc::new(|heap, _| Ok(new_nil(heap))), "doc".to_string(), false);

        assert_eq!(as_type(&type_of(&mut heap, &[int_val]).unwrap()), "integer");
        assert_eq!(as_type(&type_of(&mut heap, &[float_val]).unwrap()), "float");
        assert_eq!(as_type(&type_of(&mut heap, &[sym_val]).unwrap()), "symbol");
        assert_eq!(as_type(&type_of(&mut heap, &[pair_val]).unwrap()), "pair");
        assert_eq!(as_type(&type_of(&mut heap, &[str_val]).unwrap()), "string");
        assert_eq!(as_type(&type_of(&mut heap, &[vec_val]).unwrap()), "vector");
        assert_eq!(as_type(&type_of(&mut heap, &[clo_val]).unwrap()), "closure");
        assert_eq!(as_type(&type_of(&mut heap, &[bool_val]).unwrap()), "boolean");
        assert_eq!(as_type(&type_of(&mut heap, &[char_val]).unwrap()), "char");
        assert_eq!(as_type(&type_of(&mut heap, &[prim_val]).unwrap()), "primitive");
        assert_eq!(as_type(&type_of(&mut heap, &[env_val]).unwrap()), "env-frame");
        assert_eq!(as_type(&type_of(&mut heap, &[nil_val]).unwrap()), "nil");
    }

    fn as_type(val: &GcRef) -> &'static str {
        match &val.borrow().value {
            SchemeValue::Symbol(s) => match s.as_str() {
                "integer" => "integer",
                "float" => "float",
                "symbol" => "symbol",
                "pair" => "pair",
                "string" => "string",
                "vector" => "vector",
                "closure" => "closure",
                "boolean" => "boolean",
                "char" => "char",
                "primitive" => "primitive",
                "env-frame" => "env-frame",
                "nil" => "nil",
                _ => panic!("unexpected symbol from type_of: {}", s),
            },
            _ => panic!("type_of did not return a symbol"),
        }
    }

    // ============================================================================
    // TESTS FOR SIMPLE PREDICATE BUILTINS (Reference-based GC system)
    // ============================================================================

    #[test]
    fn test_number_q_simple() {
        let mut heap = GcHeap::new();
        
        // Test with integer
        let int_val = new_int_simple(&mut heap, BigInt::from(42));
        let result = number_q_simple(&mut heap, &[int_val]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(true)));
        
        // Test with float
        let float_val = new_float_simple(&mut heap, 3.14);
        let result = number_q_simple(&mut heap, &[float_val]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(true)));
        
        // Test with non-number
        let sym_val = new_symbol_simple(&mut heap, "foo");
        let result = number_q_simple(&mut heap, &[sym_val]);
        assert!(result.is_ok());
        assert!(matches!(&result.unwrap().value, SchemeValueSimple::Bool(false)));
        
        // Test error case: no arguments
        let result = number_q_simple(&mut heap, &[]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expected 1 argument"));
    }

    #[test]
    fn test_type_of_simple() {
        let mut heap = GcHeap::new();
        
        // Test with integer
        let int_val = new_int_simple(&mut heap, BigInt::from(42));
        let result = type_of_simple(&mut heap, &[int_val]);
        assert!(result.is_ok());
        assert_eq!(as_type_simple(&result.unwrap()), "integer");
        
        // Test with float
        let float_val = new_float_simple(&mut heap, 3.14);
        let result = type_of_simple(&mut heap, &[float_val]);
        assert!(result.is_ok());
        assert_eq!(as_type_simple(&result.unwrap()), "float");
        
        // Test with symbol
        let sym_val = new_symbol_simple(&mut heap, "foo");
        let result = type_of_simple(&mut heap, &[sym_val]);
        assert!(result.is_ok());
        assert_eq!(as_type_simple(&result.unwrap()), "symbol");
        
        // Test error case: no arguments
        let result = type_of_simple(&mut heap, &[]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expected 1 argument"));
    }

    fn as_type_simple(val: &GcRefSimple) -> &'static str {
        match &val.value {
            SchemeValueSimple::Symbol(s) => match s.as_str() {
                "integer" => "integer",
                "float" => "float",
                "symbol" => "symbol",
                "pair" => "pair",
                "string" => "string",
                "vector" => "vector",
                "closure" => "closure",
                "boolean" => "boolean",
                "char" => "char",
                "primitive" => "primitive",
                "env-frame" => "env-frame",
                "nil" => "nil",
                _ => panic!("unexpected symbol from type_of_simple: {}", s),
            },
            _ => panic!("type_of_simple did not return a symbol"),
        }
    }
} 