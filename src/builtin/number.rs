use crate::gc::{GcRef, SchemeValue, new_bool, new_float, new_int};
use num_bigint::BigInt;
use num_traits::{One, ToPrimitive, Zero};

pub fn plus_builtin(heap: &mut crate::gc::GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
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
        Ok(new_float(heap, sum_float))
    } else {
        Ok(new_int(heap, sum_int))
    }
}

pub fn minus_builtin(heap: &mut crate::gc::GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
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
            return Ok(new_float(heap, -result_float));
        } else {
            return Ok(new_int(heap, -result_int));
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
        Ok(new_float(heap, result_float))
    } else {
        Ok(new_int(heap, result_int))
    }
}

pub fn times_builtin(heap: &mut crate::gc::GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
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
        Ok(new_float(heap, prod_float))
    } else {
        Ok(new_int(heap, prod_int))
    }
}

pub fn div_builtin(heap: &mut crate::gc::GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
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
    Ok(new_float(heap, result_float))
}

pub fn mod_builtin(heap: &mut crate::gc::GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
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
    Ok(new_int(heap, a % b))
}

pub fn eq_builtin(heap: &mut crate::gc::GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 2 {
        return Err("=: expects at least 2 arguments".to_string());
    }

    // Convert all arguments to f64 for comparison
    let mut numbers = Vec::new();
    for arg in args {
        let num = match &arg.value {
            SchemeValue::Int(i) => i.to_f64().unwrap(),
            SchemeValue::Float(f) => *f,
            _ => return Err("=: all arguments must be numbers".to_string()),
        };
        numbers.push(num);
    }

    // Check if all numbers are equal
    let first = numbers[0];
    let all_equal = numbers.iter().all(|&n| n == first);

    Ok(new_bool(heap, all_equal))
}

pub fn lt_builtin(heap: &mut crate::gc::GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 2 {
        return Err("<: expects at least 2 arguments".to_string());
    }

    // Convert all arguments to f64 for comparison
    let mut numbers = Vec::new();
    for arg in args {
        let num = match &arg.value {
            SchemeValue::Int(i) => i.to_f64().unwrap(),
            SchemeValue::Float(f) => *f,
            _ => return Err("<: all arguments must be numbers".to_string()),
        };
        numbers.push(num);
    }

    // Check if numbers are in strictly increasing order
    let mut prev = numbers[0];
    for &num in &numbers[1..] {
        if prev >= num {
            return Ok(new_bool(heap, false));
        }
        prev = num;
    }

    Ok(new_bool(heap, true))
}

pub fn gt_builtin(heap: &mut crate::gc::GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 2 {
        return Err(">: expects at least 2 arguments".to_string());
    }

    // Convert all arguments to f64 for comparison
    let mut numbers = Vec::new();
    for arg in args {
        let num = match &arg.value {
            SchemeValue::Int(i) => i.to_f64().unwrap(),
            SchemeValue::Float(f) => *f,
            _ => return Err(">: all arguments must be numbers".to_string()),
        };
        numbers.push(num);
    }

    // Check if numbers are in strictly decreasing order
    let mut prev = numbers[0];
    for &num in &numbers[1..] {
        if prev <= num {
            return Ok(new_bool(heap, false));
        }
        prev = num;
    }

    Ok(new_bool(heap, true))
}

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_plus_builtin() {
        let mut heap = crate::gc::GcHeap::new();
        let args = vec![
            new_int(&mut heap, BigInt::from(1)),
            new_int(&mut heap, BigInt::from(2)),
            new_int(&mut heap, BigInt::from(3)),
        ];
        let result = plus_builtin(&mut heap, &args).unwrap();
        match &result.value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "6"),
            _ => panic!("Expected integer"),
        }
    }

    #[test]
    fn test_minus_builtin() {
        let mut heap = crate::gc::GcHeap::new();
        let args = vec![
            new_int(&mut heap, BigInt::from(10)),
            new_int(&mut heap, BigInt::from(3)),
        ];
        let result = minus_builtin(&mut heap, &args).unwrap();
        match &result.value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "7"),
            _ => panic!("Expected integer"),
        }
    }

    #[test]
    fn test_times_builtin() {
        let mut heap = crate::gc::GcHeap::new();
        let args = vec![
            new_int(&mut heap, BigInt::from(2)),
            new_int(&mut heap, BigInt::from(3)),
            new_int(&mut heap, BigInt::from(4)),
        ];
        let result = times_builtin(&mut heap, &args).unwrap();
        match &result.value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "24"),
            _ => panic!("Expected integer"),
        }
    }

    #[test]
    fn test_div_builtin() {
        let mut heap = crate::gc::GcHeap::new();
        let args = vec![
            new_int(&mut heap, BigInt::from(10)),
            new_int(&mut heap, BigInt::from(2)),
        ];
        let result = div_builtin(&mut heap, &args).unwrap();
        match &result.value {
            SchemeValue::Float(f) => assert_eq!(*f, 5.0),
            _ => panic!("Expected float"),
        }
    }

    #[test]
    fn test_mod_builtin() {
        let mut heap = crate::gc::GcHeap::new();
        let args = vec![
            new_int(&mut heap, BigInt::from(7)),
            new_int(&mut heap, BigInt::from(3)),
        ];
        let result = mod_builtin(&mut heap, &args).unwrap();
        match &result.value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "1"),
            _ => panic!("Expected integer"),
        }
    }

    #[test]
    fn test_eq_builtin() {
        let mut heap = crate::gc::GcHeap::new();

        // Test equal integers
        let args = vec![
            new_int(&mut heap, BigInt::from(5)),
            new_int(&mut heap, BigInt::from(5)),
        ];
        let result = eq_builtin(&mut heap, &args).unwrap();
        assert!(matches!(&result.value, SchemeValue::Bool(true)));

        // Test unequal integers
        let args = vec![
            new_int(&mut heap, BigInt::from(5)),
            new_int(&mut heap, BigInt::from(6)),
        ];
        let result = eq_builtin(&mut heap, &args).unwrap();
        assert!(matches!(&result.value, SchemeValue::Bool(false)));

        // Test mixed int and float
        let args = vec![
            new_int(&mut heap, BigInt::from(5)),
            new_float(&mut heap, 5.0),
        ];
        let result = eq_builtin(&mut heap, &args).unwrap();
        assert!(matches!(&result.value, SchemeValue::Bool(true)));

        // Test multiple equal values
        let args = vec![
            new_int(&mut heap, BigInt::from(5)),
            new_float(&mut heap, 5.0),
            new_int(&mut heap, BigInt::from(5)),
        ];
        let result = eq_builtin(&mut heap, &args).unwrap();
        assert!(matches!(&result.value, SchemeValue::Bool(true)));

        // Test multiple unequal values
        let args = vec![
            new_int(&mut heap, BigInt::from(5)),
            new_float(&mut heap, 5.0),
            new_int(&mut heap, BigInt::from(6)),
        ];
        let result = eq_builtin(&mut heap, &args).unwrap();
        assert!(matches!(&result.value, SchemeValue::Bool(false)));
    }

    #[test]
    fn test_lt_builtin() {
        let mut heap = crate::gc::GcHeap::new();

        // Test strictly increasing integers
        let args = vec![
            new_int(&mut heap, BigInt::from(1)),
            new_int(&mut heap, BigInt::from(2)),
            new_int(&mut heap, BigInt::from(3)),
        ];
        let result = lt_builtin(&mut heap, &args).unwrap();
        assert!(matches!(&result.value, SchemeValue::Bool(true)));

        // Test not strictly increasing
        let args = vec![
            new_int(&mut heap, BigInt::from(1)),
            new_int(&mut heap, BigInt::from(2)),
            new_int(&mut heap, BigInt::from(2)),
        ];
        let result = lt_builtin(&mut heap, &args).unwrap();
        assert!(matches!(&result.value, SchemeValue::Bool(false)));

        // Test mixed int and float
        let args = vec![
            new_int(&mut heap, BigInt::from(1)),
            new_float(&mut heap, 1.5),
            new_int(&mut heap, BigInt::from(2)),
        ];
        let result = lt_builtin(&mut heap, &args).unwrap();
        assert!(matches!(&result.value, SchemeValue::Bool(true)));

        // Test decreasing sequence
        let args = vec![
            new_int(&mut heap, BigInt::from(3)),
            new_int(&mut heap, BigInt::from(2)),
            new_int(&mut heap, BigInt::from(1)),
        ];
        let result = lt_builtin(&mut heap, &args).unwrap();
        assert!(matches!(&result.value, SchemeValue::Bool(false)));
    }

    #[test]
    fn test_gt_builtin() {
        let mut heap = crate::gc::GcHeap::new();

        // Test strictly decreasing integers
        let args = vec![
            new_int(&mut heap, BigInt::from(3)),
            new_int(&mut heap, BigInt::from(2)),
            new_int(&mut heap, BigInt::from(1)),
        ];
        let result = gt_builtin(&mut heap, &args).unwrap();
        assert!(matches!(&result.value, SchemeValue::Bool(true)));

        // Test not strictly decreasing
        let args = vec![
            new_int(&mut heap, BigInt::from(3)),
            new_int(&mut heap, BigInt::from(2)),
            new_int(&mut heap, BigInt::from(2)),
        ];
        let result = gt_builtin(&mut heap, &args).unwrap();
        assert!(matches!(&result.value, SchemeValue::Bool(false)));

        // Test mixed int and float
        let args = vec![
            new_int(&mut heap, BigInt::from(3)),
            new_float(&mut heap, 2.5),
            new_int(&mut heap, BigInt::from(2)),
        ];
        let result = gt_builtin(&mut heap, &args).unwrap();
        assert!(matches!(&result.value, SchemeValue::Bool(true)));

        // Test increasing sequence
        let args = vec![
            new_int(&mut heap, BigInt::from(1)),
            new_int(&mut heap, BigInt::from(2)),
            new_int(&mut heap, BigInt::from(3)),
        ];
        let result = gt_builtin(&mut heap, &args).unwrap();
        assert!(matches!(&result.value, SchemeValue::Bool(false)));
    }
}
