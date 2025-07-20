use num_bigint::BigInt;
use num_traits::{ToPrimitive, Zero, One};
use crate::gc::{GcRefSimple, new_int_simple, new_float_simple, SchemeValueSimple};

pub fn plus_builtin(heap: &mut crate::gc::GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if args.len() < 2 {
        return Err("+: expects at least 2 arguments".to_string());
    }
    let mut is_float = false;
    let mut sum_int = BigInt::from(0);
    let mut sum_float = 0.0;
    for arg in args {
        match &arg.value {
            SchemeValueSimple::Int(i) => {
                if is_float {
                    sum_float += i.to_f64().unwrap();
                } else {
                    sum_int += i;
                }
            }
            SchemeValueSimple::Float(f) => {
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

pub fn minus_builtin(heap: &mut crate::gc::GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if args.len() < 1 {
        return Err("-: expects at least 1 argument".to_string());
    }
    let mut is_float = false;
    let mut result_int;
    let mut result_float;
    let mut iter = args.iter();
    match &iter.next().unwrap().value {
        SchemeValueSimple::Int(i) => {
            result_int = i.clone();
            result_float = i.to_f64().unwrap();
        }
        SchemeValueSimple::Float(f) => {
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
            SchemeValueSimple::Int(i) => {
                if is_float {
                    result_float -= i.to_f64().unwrap();
                } else {
                    result_int -= i;
                }
            }
            SchemeValueSimple::Float(f) => {
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

pub fn times_builtin(heap: &mut crate::gc::GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if args.len() < 2 {
        return Err("*: expects at least 2 arguments".to_string());
    }
    let mut is_float = false;
    let mut prod_int = BigInt::one();
    let mut prod_float = 1.0;
    for arg in args {
        match &arg.value {
            SchemeValueSimple::Int(i) => {
                if is_float {
                    prod_float *= i.to_f64().unwrap();
                } else {
                    prod_int *= i;
                }
            }
            SchemeValueSimple::Float(f) => {
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

pub fn div_builtin(heap: &mut crate::gc::GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if args.len() < 2 {
        return Err("/: expects at least 2 arguments".to_string());
    }
    let mut result_float;
    let mut iter = args.iter();
    match &iter.next().unwrap().value {
        SchemeValueSimple::Int(i) => {
            result_float = i.to_f64().unwrap();
        }
        SchemeValueSimple::Float(f) => {
            result_float = *f;
        }
        _ => return Err("/: all arguments must be numbers".to_string()),
    }
    for arg in iter {
        match &arg.value {
            SchemeValueSimple::Int(i) => {
                result_float /= i.to_f64().unwrap();
            }
            SchemeValueSimple::Float(f) => {
                result_float /= f;
            }
            _ => return Err("/: all arguments must be numbers".to_string()),
        }
    }
    Ok(new_float_simple(heap, result_float))
}

pub fn mod_builtin(heap: &mut crate::gc::GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if args.len() != 2 {
        return Err("mod: expects exactly 2 arguments".to_string());
    }
    let a = match &args[0].value {
        SchemeValueSimple::Int(i) => i,
        _ => return Err("mod: arguments must be integers".to_string()),
    };
    let b = match &args[1].value {
        SchemeValueSimple::Int(i) => i,
        _ => return Err("mod: arguments must be integers".to_string()),
    };
    if b.is_zero() {
        return Err("mod: division by zero".to_string());
    }
    Ok(new_int_simple(heap, a % b))
}

mod tests {
    use super::*;
    use crate::gc::GcHeap;

    #[test]
    fn test_plus_builtin() {
        let mut heap = GcHeap::new();
        let args = vec![
            new_int_simple(&mut heap, BigInt::from(1)),
            new_int_simple(&mut heap, BigInt::from(2)),
            new_int_simple(&mut heap, BigInt::from(3)),
        ];
        let result = plus_builtin(&mut heap, &args).unwrap();
        match &result.value {
            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "6"),
            _ => panic!("Expected integer"),
        }
    }

    #[test]
    fn test_minus_builtin() {
        let mut heap = GcHeap::new();
        let args = vec![
            new_int_simple(&mut heap, BigInt::from(10)),
            new_int_simple(&mut heap, BigInt::from(3)),
        ];
        let result = minus_builtin(&mut heap, &args).unwrap();
        match &result.value {
            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "7"),
            _ => panic!("Expected integer"),
        }
    }

    #[test]
    fn test_times_builtin() {
        let mut heap = GcHeap::new();
        let args = vec![
            new_int_simple(&mut heap, BigInt::from(2)),
            new_int_simple(&mut heap, BigInt::from(3)),
            new_int_simple(&mut heap, BigInt::from(4)),
        ];
        let result = times_builtin(&mut heap, &args).unwrap();
        match &result.value {
            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "24"),
            _ => panic!("Expected integer"),
        }
    }

    #[test]
    fn test_div_builtin() {
        let mut heap = GcHeap::new();
        let args = vec![
            new_int_simple(&mut heap, BigInt::from(10)),
            new_int_simple(&mut heap, BigInt::from(2)),
        ];
        let result = div_builtin(&mut heap, &args).unwrap();
        match &result.value {
            SchemeValueSimple::Float(f) => assert_eq!(*f, 5.0),
            _ => panic!("Expected float"),
        }
    }

    #[test]
    fn test_mod_builtin() {
        let mut heap = GcHeap::new();
        let args = vec![
            new_int_simple(&mut heap, BigInt::from(7)),
            new_int_simple(&mut heap, BigInt::from(3)),
        ];
        let result = mod_builtin(&mut heap, &args).unwrap();
        match &result.value {
            SchemeValueSimple::Int(i) => assert_eq!(i.to_string(), "1"),
            _ => panic!("Expected integer"),
        }
    }
} 