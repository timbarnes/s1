use num_bigint::BigInt;
use num_traits::{ToPrimitive, Zero, One};
use crate::gc::{SchemeValue, new_int, new_float};
use crate::gc::{GcRefSimple, new_int_simple, new_float_simple};

pub fn plus_builtin(heap: &mut crate::gc::GcHeap, args: &[crate::gc::GcRef]) -> Result<crate::gc::GcRef, String> {
    if args.len() < 2 {
        return Err("+: expects at least 2 arguments".to_string());
    }
    let mut is_float = false;
    let mut sum_int = BigInt::from(0);
    let mut sum_float = 0.0;
    for arg in args {
        match &arg.borrow().value {
            SchemeValue::Int(i) => {
                if is_float {
                    sum_float += i.to_f64().unwrap();
                } else {
                    sum_int += i;
                }
            }
            SchemeValue::Float(f) => {
                if !is_float {
                    sum_float = sum_int.to_f64().unwrap();
                    is_float = true;
                }
                sum_float += f;
            }
            _ => return Err("+: all arguments must be numbers".to_string()),
        }
    }
    if is_float {
        Ok(new_float(heap, sum_float))
    } else {
        Ok(new_int(heap, sum_int))
    }
}

pub fn minus_builtin(heap: &mut crate::gc::GcHeap, args: &[crate::gc::GcRef]) -> Result<crate::gc::GcRef, String> {
    if args.len() < 1 {
        return Err("-: expects at least 1 argument".to_string());
    }
    let mut is_float = false;
    let mut result_int;
    let mut result_float;
    let mut iter = args.iter();
    match &iter.next().unwrap().borrow().value {
        SchemeValue::Int(i) => {
            result_int = i.clone();
            result_float = i.to_f64().unwrap();
        }
        SchemeValue::Float(f) => {
            is_float = true;
            result_int = BigInt::zero();
            result_float = *f;
        }
        _ => return Err("-: all arguments must be numbers".to_string()),
    }
    if args.len() == 1 {
        // Unary minus
        if is_float {
            return Ok(new_float(heap, -result_float));
        } else {
            return Ok(new_int(heap, -result_int));
        }
    }
    for arg in iter {
        match &arg.borrow().value {
            SchemeValue::Int(i) => {
                if is_float {
                    result_float -= i.to_f64().unwrap();
                } else {
                    result_int -= i;
                }
            }
            SchemeValue::Float(f) => {
                if !is_float {
                    result_float = result_int.to_f64().unwrap();
                    is_float = true;
                }
                result_float -= f;
            }
            _ => return Err("-: all arguments must be numbers".to_string()),
        }
    }
    if is_float {
        Ok(new_float(heap, result_float))
    } else {
        Ok(new_int(heap, result_int))
    }
}

pub fn times_builtin(heap: &mut crate::gc::GcHeap, args: &[crate::gc::GcRef]) -> Result<crate::gc::GcRef, String> {
    if args.len() < 2 {
        return Err("*: expects at least 2 arguments".to_string());
    }
    let mut is_float = false;
    let mut prod_int = BigInt::one();
    let mut prod_float = 1.0;
    for arg in args {
        match &arg.borrow().value {
            SchemeValue::Int(i) => {
                if is_float {
                    prod_float *= i.to_f64().unwrap();
                } else {
                    prod_int *= i;
                }
            }
            SchemeValue::Float(f) => {
                if !is_float {
                    prod_float = prod_int.to_f64().unwrap();
                    is_float = true;
                }
                prod_float *= f;
            }
            _ => return Err("*: all arguments must be numbers".to_string()),
        }
    }
    if is_float {
        Ok(new_float(heap, prod_float))
    } else {
        Ok(new_int(heap, prod_int))
    }
}

pub fn div_builtin(heap: &mut crate::gc::GcHeap, args: &[crate::gc::GcRef]) -> Result<crate::gc::GcRef, String> {
    if args.len() < 2 {
        return Err("/: expects at least 2 arguments".to_string());
    }
    // let mut is_float = false;
    let mut result_float;
    let mut iter = args.iter();
    match &iter.next().unwrap().borrow().value {
        SchemeValue::Int(i) => {
            result_float = i.to_f64().unwrap();
        }
        SchemeValue::Float(f) => {
            // is_float = true;
            result_float = *f;
        }
        _ => return Err("/: all arguments must be numbers".to_string()),
    }
    for arg in iter {
        match &arg.borrow().value {
            SchemeValue::Int(i) => {
                result_float /= i.to_f64().unwrap();
            }
            SchemeValue::Float(f) => {
                result_float /= f;
            }
            _ => return Err("/: all arguments must be numbers".to_string()),
        }
    }
    Ok(new_float(heap, result_float))
}

pub fn mod_builtin(heap: &mut crate::gc::GcHeap, args: &[crate::gc::GcRef]) -> Result<crate::gc::GcRef, String> {
    if args.len() != 2 {
        return Err("mod: expects exactly 2 arguments".to_string());
    }
    let a_borrow = args[0].borrow();
    let b_borrow = args[1].borrow();
    let a = match &a_borrow.value {
        SchemeValue::Int(i) => i,
        _ => return Err("mod: arguments must be integers".to_string()),
    };
    let b = match &b_borrow.value {
        SchemeValue::Int(i) => i,
        _ => return Err("mod: arguments must be integers".to_string()),
    };
    if b.is_zero() {
        return Err("mod: division by zero".to_string());
    }
    Ok(new_int(heap, a % b))
} 

// ============================================================================
// SIMPLE NUMBER BUILTINS (Reference-based GC system)
// ============================================================================

pub fn plus_builtin_simple(heap: &mut crate::gc::GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if args.len() < 2 {
        return Err("+: expects at least 2 arguments".to_string());
    }
    let mut is_float = false;
    let mut sum_int = BigInt::from(0);
    let mut sum_float = 0.0;
    for arg in args {
        match &arg.value {
            SchemeValue::Int(i) => {
                if is_float {
                    sum_float += i.to_f64().unwrap();
                } else {
                    sum_int += i;
                }
            }
            SchemeValue::Float(f) => {
                if !is_float {
                    sum_float = sum_int.to_f64().unwrap();
                    is_float = true;
                }
                sum_float += f;
            }
            _ => return Err("+: all arguments must be numbers".to_string()),
        }
    }
    if is_float {
        Ok(new_float_simple(heap, sum_float))
    } else {
        Ok(new_int_simple(heap, sum_int))
    }
}

pub fn minus_builtin_simple(heap: &mut crate::gc::GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if args.len() < 1 {
        return Err("-: expects at least 1 argument".to_string());
    }
    let mut is_float = false;
    let mut result_int;
    let mut result_float;
    let mut iter = args.iter();
    match &iter.next().unwrap().value {
        SchemeValue::Int(i) => {
            result_int = i.clone();
            result_float = i.to_f64().unwrap();
        }
        SchemeValue::Float(f) => {
            is_float = true;
            result_int = BigInt::zero();
            result_float = *f;
        }
        _ => return Err("-: all arguments must be numbers".to_string()),
    }
    if args.len() == 1 {
        // Unary minus
        if is_float {
            return Ok(new_float_simple(heap, -result_float));
        } else {
            return Ok(new_int_simple(heap, -result_int));
        }
    }
    for arg in iter {
        match &arg.value {
            SchemeValue::Int(i) => {
                if is_float {
                    result_float -= i.to_f64().unwrap();
                } else {
                    result_int -= i;
                }
            }
            SchemeValue::Float(f) => {
                if !is_float {
                    result_float = result_int.to_f64().unwrap();
                    is_float = true;
                }
                result_float -= f;
            }
            _ => return Err("-: all arguments must be numbers".to_string()),
        }
    }
    if is_float {
        Ok(new_float_simple(heap, result_float))
    } else {
        Ok(new_int_simple(heap, result_int))
    }
}

pub fn times_builtin_simple(heap: &mut crate::gc::GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if args.len() < 2 {
        return Err("*: expects at least 2 arguments".to_string());
    }
    let mut is_float = false;
    let mut prod_int = BigInt::one();
    let mut prod_float = 1.0;
    for arg in args {
        match &arg.value {
            SchemeValue::Int(i) => {
                if is_float {
                    prod_float *= i.to_f64().unwrap();
                } else {
                    prod_int *= i;
                }
            }
            SchemeValue::Float(f) => {
                if !is_float {
                    prod_float = prod_int.to_f64().unwrap();
                    is_float = true;
                }
                prod_float *= f;
            }
            _ => return Err("*: all arguments must be numbers".to_string()),
        }
    }
    if is_float {
        Ok(new_float_simple(heap, prod_float))
    } else {
        Ok(new_int_simple(heap, prod_int))
    }
}

pub fn div_builtin_simple(heap: &mut crate::gc::GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if args.len() < 2 {
        return Err("/: expects at least 2 arguments".to_string());
    }
    let mut result_float;
    let mut iter = args.iter();
    match &iter.next().unwrap().value {
        SchemeValue::Int(i) => {
            result_float = i.to_f64().unwrap();
        }
        SchemeValue::Float(f) => {
            result_float = *f;
        }
        _ => return Err("/: all arguments must be numbers".to_string()),
    }
    for arg in iter {
        match &arg.value {
            SchemeValue::Int(i) => {
                result_float /= i.to_f64().unwrap();
            }
            SchemeValue::Float(f) => {
                result_float /= f;
            }
            _ => return Err("/: all arguments must be numbers".to_string()),
        }
    }
    Ok(new_float_simple(heap, result_float))
}

pub fn mod_builtin_simple(heap: &mut crate::gc::GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if args.len() != 2 {
        return Err("mod: expects exactly 2 arguments".to_string());
    }
    let a = match &args[0].value {
        SchemeValue::Int(i) => i,
        _ => return Err("mod: arguments must be integers".to_string()),
    };
    let b = match &args[1].value {
        SchemeValue::Int(i) => i,
        _ => return Err("mod: arguments must be integers".to_string()),
    };
    if b.is_zero() {
        return Err("mod: division by zero".to_string());
    }
    Ok(new_int_simple(heap, a % b))
} 

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gc::{GcHeap, new_int_simple, new_float_simple};
    use num_bigint::BigInt;

    #[test]
    fn test_plus_builtin_simple() {
        let mut heap = GcHeap::new();
        
        // Test adding integers
        let arg1 = new_int_simple(&mut heap, BigInt::from(3));
        let arg2 = new_int_simple(&mut heap, BigInt::from(4));
        let args = vec![arg1, arg2];
        let result = plus_builtin_simple(&mut heap, &args);
        assert!(result.is_ok());
        assert_eq!(&result.unwrap().value, &SchemeValue::Int(BigInt::from(7)));
        
        // Test adding floats
        let arg1 = new_float_simple(&mut heap, 3.5);
        let arg2 = new_float_simple(&mut heap, 4.5);
        let args = vec![arg1, arg2];
        let result = plus_builtin_simple(&mut heap, &args);
        assert!(result.is_ok());
        assert_eq!(&result.unwrap().value, &SchemeValue::Float(8.0));
        
        // Test error case: too few arguments
        let args = vec![new_int_simple(&mut heap, BigInt::from(42))];
        let result = plus_builtin_simple(&mut heap, &args);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expects at least 2 arguments"));
    }

    #[test]
    fn test_minus_builtin_simple() {
        let mut heap = GcHeap::new();
        
        // Test subtracting integers
        let arg1 = new_int_simple(&mut heap, BigInt::from(10));
        let arg2 = new_int_simple(&mut heap, BigInt::from(3));
        let args = vec![arg1, arg2];
        let result = minus_builtin_simple(&mut heap, &args);
        assert!(result.is_ok());
        assert_eq!(&result.unwrap().value, &SchemeValue::Int(BigInt::from(7)));
        
        // Test unary minus
        let arg = new_int_simple(&mut heap, BigInt::from(42));
        let args = vec![arg];
        let result = minus_builtin_simple(&mut heap, &args);
        assert!(result.is_ok());
        assert_eq!(&result.unwrap().value, &SchemeValue::Int(BigInt::from(-42)));
        
        // Test error case: no arguments
        let args: Vec<GcRefSimple> = vec![];
        let result = minus_builtin_simple(&mut heap, &args);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expects at least 1 argument"));
    }

    #[test]
    fn test_times_builtin_simple() {
        let mut heap = GcHeap::new();
        
        // Test multiplying integers
        let arg1 = new_int_simple(&mut heap, BigInt::from(3));
        let arg2 = new_int_simple(&mut heap, BigInt::from(4));
        let args = vec![arg1, arg2];
        let result = times_builtin_simple(&mut heap, &args);
        assert!(result.is_ok());
        assert_eq!(&result.unwrap().value, &SchemeValue::Int(BigInt::from(12)));
        
        // Test error case: too few arguments
        let args = vec![new_int_simple(&mut heap, BigInt::from(42))];
        let result = times_builtin_simple(&mut heap, &args);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expects at least 2 arguments"));
    }

    #[test]
    fn test_div_builtin_simple() {
        let mut heap = GcHeap::new();
        
        // Test dividing integers (should return float)
        let arg1 = new_int_simple(&mut heap, BigInt::from(10));
        let arg2 = new_int_simple(&mut heap, BigInt::from(2));
        let args = vec![arg1, arg2];
        let result = div_builtin_simple(&mut heap, &args);
        assert!(result.is_ok());
        assert_eq!(&result.unwrap().value, &SchemeValue::Float(5.0));
        
        // Test error case: too few arguments
        let args = vec![new_int_simple(&mut heap, BigInt::from(42))];
        let result = div_builtin_simple(&mut heap, &args);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expects at least 2 arguments"));
    }

    #[test]
    fn test_mod_builtin_simple() {
        let mut heap = GcHeap::new();
        
        // Test modulo operation
        let arg1 = new_int_simple(&mut heap, BigInt::from(10));
        let arg2 = new_int_simple(&mut heap, BigInt::from(3));
        let args = vec![arg1, arg2];
        let result = mod_builtin_simple(&mut heap, &args);
        assert!(result.is_ok());
        assert_eq!(&result.unwrap().value, &SchemeValue::Int(BigInt::from(1)));
        
        // Test error case: wrong number of arguments
        let args = vec![new_int_simple(&mut heap, BigInt::from(42))];
        let result = mod_builtin_simple(&mut heap, &args);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expects exactly 2 arguments"));
        
        // Test error case: division by zero
        let arg1 = new_int_simple(&mut heap, BigInt::from(10));
        let arg2 = new_int_simple(&mut heap, BigInt::from(0));
        let args = vec![arg1, arg2];
        let result = mod_builtin_simple(&mut heap, &args);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("division by zero"));
    }
} 