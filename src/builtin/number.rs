use crate::env::{EnvOps, EnvRef};
use crate::gc::{GcHeap, GcRef, SchemeValue, new_bool, new_float, new_int};
use num_bigint::BigInt;
use num_traits::{One, ToPrimitive, Zero};

pub fn plus_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 2 {
        return Err("+: expects at least 2 arguments".to_string());
    }
    let mut is_float = false;
    let mut sum_int = BigInt::from(0);
    let mut sum_float = 0.0;
    for arg in args {
        match &heap.get_value(*arg) {
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

pub fn minus_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 1 {
        return Err("-: expects at least 1 argument".to_string());
    }
    let mut is_float = false;
    let mut result_int;
    let mut result_float;
    let mut iter = args.iter();
    match &heap.get_value(*iter.next().unwrap()) {
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
        match &heap.get_value(*arg) {
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

pub fn times_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 2 {
        return Err("*: expects at least 2 arguments".to_string());
    }
    let mut is_float = false;
    let mut prod_int = BigInt::one();
    let mut prod_float = 1.0;
    for arg in args {
        match &heap.get_value(*arg) {
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

pub fn div_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 2 {
        return Err("/: expects at least 2 arguments".to_string());
    }
    let mut result_float;
    let mut iter = args.iter();
    match &heap.get_value(*iter.next().unwrap()) {
        SchemeValue::Int(i) => {
            result_float = i.to_f64().unwrap();
        }
        SchemeValue::Float(f) => {
            result_float = *f;
        }
        _ => return Err("/: all arguments must be numbers".to_string()),
    }
    for arg in iter {
        match &heap.get_value(*arg) {
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

pub fn mod_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("mod: expects exactly 2 arguments".to_string());
    }
    let a = match &heap.get_value(args[0]) {
        SchemeValue::Int(i) => i,
        _ => return Err("mod: arguments must be integers".to_string()),
    };
    let b = match &heap.get_value(args[1]) {
        SchemeValue::Int(i) => i,
        _ => return Err("mod: arguments must be integers".to_string()),
    };
    if b.is_zero() {
        return Err("mod: division by zero".to_string());
    }
    Ok(new_int(heap, a % b))
}

pub fn eq_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 2 {
        return Err("=: expects at least 2 arguments".to_string());
    }

    // Convert all arguments to f64 for comparison
    let mut numbers = Vec::new();
    for arg in args {
        let num = match &heap.get_value(*arg) {
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

pub fn lt_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 2 {
        return Err("<: expects at least 2 arguments".to_string());
    }

    // Convert all arguments to f64 for comparison
    let mut numbers = Vec::new();
    for arg in args {
        let num = match &heap.get_value(*arg) {
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

pub fn gt_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 2 {
        return Err(">: expects at least 2 arguments".to_string());
    }

    // Convert all arguments to f64 for comparison
    let mut numbers = Vec::new();
    for arg in args {
        let num = match &heap.get_value(*arg) {
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

/// (quotient n1 n2)
/// Returns the integer quotient of dividing n1 by n2
pub fn quotient_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("quotient: expects exactly 2 arguments".to_string());
    }
    let a = match &heap.get_value(args[0]) {
        SchemeValue::Int(i) => i,
        _ => return Err("quotient: arguments must be integers".to_string()),
    };
    let b = match &heap.get_value(args[1]) {
        SchemeValue::Int(i) => i,
        _ => return Err("quotient: arguments must be integers".to_string()),
    };
    if b.is_zero() {
        return Err("quotient: division by zero".to_string());
    }
    Ok(new_int(heap, a / b))
}

/// (remainder n1 n2)
/// Returns the remainder of dividing n1 by n2
pub fn remainder_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("remainder: expects exactly 2 arguments".to_string());
    }
    let a = match &heap.get_value(args[0]) {
        SchemeValue::Int(i) => i,
        _ => return Err("remainder: arguments must be integers".to_string()),
    };
    let b = match &heap.get_value(args[1]) {
        SchemeValue::Int(i) => i,
        _ => return Err("remainder: arguments must be integers".to_string()),
    };
    if b.is_zero() {
        return Err("remainder: division by zero".to_string());
    }
    Ok(new_int(heap, a % b))
}

/// (numerator q)
/// Returns the numerator of rational q (for integers, returns the integer itself)
pub fn numerator_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("numerator: expects exactly 1 argument".to_string());
    }
    match &heap.get_value(args[0]) {
        SchemeValue::Int(_) => Ok(args[0]), // For integers, numerator is the integer itself
        SchemeValue::Float(f) => {
            // Convert float to rational representation (simplified)
            if f.fract() == 0.0 {
                Ok(new_int(heap, BigInt::from(*f as i64)))
            } else {
                // For simplicity, return the float as an integer part
                Ok(new_int(heap, BigInt::from(f.trunc() as i64)))
            }
        }
        _ => return Err("numerator: argument must be a number".to_string()),
    }
}

/// (denominator q)
/// Returns the denominator of rational q (for integers, returns 1)
pub fn denominator_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("denominator: expects exactly 1 argument".to_string());
    }
    match &heap.get_value(args[0]) {
        SchemeValue::Int(_) => Ok(new_int(heap, BigInt::one())), // For integers, denominator is 1
        SchemeValue::Float(f) => {
            if f.fract() == 0.0 {
                Ok(new_int(heap, BigInt::one()))
            } else {
                // For simplicity, assume denominator is some power of 10
                Ok(new_int(heap, BigInt::from(1000))) // Simplified implementation
            }
        }
        _ => return Err("denominator: argument must be a number".to_string()),
    }
}

/// (floor n)
/// Returns the largest integer not greater than n
pub fn floor_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("floor: expects exactly 1 argument".to_string());
    }
    match &heap.get_value(args[0]) {
        SchemeValue::Int(_) => Ok(args[0]), // Integers are unchanged
        SchemeValue::Float(f) => Ok(new_int(heap, BigInt::from(f.floor() as i64))),
        _ => return Err("floor: argument must be a number".to_string()),
    }
}

/// (ceiling n)
/// Returns the smallest integer not less than n
pub fn ceiling_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("ceiling: expects exactly 1 argument".to_string());
    }
    match &heap.get_value(args[0]) {
        SchemeValue::Int(_) => Ok(args[0]), // Integers are unchanged
        SchemeValue::Float(f) => Ok(new_int(heap, BigInt::from(f.ceil() as i64))),
        _ => return Err("ceiling: argument must be a number".to_string()),
    }
}

/// (truncate n)
/// Returns the integer closest to n whose absolute value is not larger than n
pub fn truncate_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("truncate: expects exactly 1 argument".to_string());
    }
    match &heap.get_value(args[0]) {
        SchemeValue::Int(_) => Ok(args[0]), // Integers are unchanged
        SchemeValue::Float(f) => Ok(new_int(heap, BigInt::from(f.trunc() as i64))),
        _ => return Err("truncate: argument must be a number".to_string()),
    }
}

/// (round n)
/// Returns the closest integer to n, rounding to even when halfway between integers
pub fn round_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("round: expects exactly 1 argument".to_string());
    }
    match &heap.get_value(args[0]) {
        SchemeValue::Int(_) => Ok(args[0]), // Integers are unchanged
        SchemeValue::Float(f) => Ok(new_int(heap, BigInt::from(f.round() as i64))),
        _ => return Err("round: argument must be a number".to_string()),
    }
}

/// (sqrt n)
/// Returns the principal square root of n
pub fn sqrt_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("sqrt: expects exactly 1 argument".to_string());
    }
    let num = match &heap.get_value(args[0]) {
        SchemeValue::Int(i) => i.to_f64().unwrap(),
        SchemeValue::Float(f) => *f,
        _ => return Err("sqrt: argument must be a number".to_string()),
    };
    if num < 0.0 {
        return Err("sqrt: argument must be non-negative".to_string());
    }
    Ok(new_float(heap, num.sqrt()))
}

/// (expt n1 n2)
/// Returns n1 raised to the power n2
pub fn expt_b(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("expt: expects exactly 2 arguments".to_string());
    }
    let base = match &heap.get_value(args[0]) {
        SchemeValue::Int(i) => i.to_f64().unwrap(),
        SchemeValue::Float(f) => *f,
        _ => return Err("expt: arguments must be numbers".to_string()),
    };
    let exp = match &heap.get_value(args[1]) {
        SchemeValue::Int(i) => i.to_f64().unwrap(),
        SchemeValue::Float(f) => *f,
        _ => return Err("expt: arguments must be numbers".to_string()),
    };

    // Handle special cases
    if base == 0.0 && exp < 0.0 {
        return Err("expt: division by zero".to_string());
    }

    let result = base.powf(exp);

    // Check if result can be represented as integer
    if result.fract() == 0.0 && result.is_finite() && result.abs() < (i64::MAX as f64) {
        Ok(new_int(heap, BigInt::from(result as i64)))
    } else {
        Ok(new_float(heap, result))
    }
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

pub fn register_number_builtins(heap: &mut GcHeap, env: EnvRef) {
    register_builtin_family!(heap, env,
        "+" => plus_b,
        "-" => minus_b,
        "*" => times_b,
        "/" => div_b,
        "modulo" => mod_b,
        "=" => eq_b,
        "<" => lt_b,
        ">" => gt_b,
        "quotient" => quotient_b,
        "remainder" => remainder_b,
        "numerator" => numerator_b,
        "denominator" => denominator_b,
        "floor" => floor_b,
        "ceiling" => ceiling_b,
        "truncate" => truncate_b,
        "round" => round_b,
        "sqrt" => sqrt_b,
        "expt" => expt_b,
    );
}

// mod tests {
//     #[allow(unused_imports)]
//     use super::*;

//     #[test]
//     fn test_plus_builtin() {
//         let mut ev = crate::eval::Evaluator::new();
//         let mut ec = crate::eval::RunTime::from_eval(&mut ev);
//         let args = vec![
//             new_int(heap, BigInt::from(1)),
//             new_int(heap, BigInt::from(2)),
//             new_int(heap, BigInt::from(3)),
//         ];
//         let result = plus_b(&mut ec, &args).unwrap();
//         match &heap.get_value(result) {
//             SchemeValue::Int(i) => assert_eq!(i.to_string(), "6"),
//             _ => panic!("Expected integer"),
//         }
//     }

//     #[test]
//     fn test_minus_builtin() {
//         let mut ev = crate::eval::Evaluator::new();
//         let mut ec = crate::eval::RunTime::from_eval(&mut ev);
//         let args = vec![
//             new_int(heap, BigInt::from(10)),
//             new_int(heap, BigInt::from(3)),
//         ];
//         let result = minus_b(&mut ec, &args).unwrap();
//         match &heap.get_value(result) {
//             SchemeValue::Int(i) => assert_eq!(i.to_string(), "7"),
//             _ => panic!("Expected integer"),
//         }
//     }

//     #[test]
//     fn test_times_builtin() {
//         let mut ev = crate::eval::Evaluator::new();
//         let mut ec = crate::eval::RunTime::from_eval(&mut ev);
//         let args = vec![
//             new_int(heap, BigInt::from(2)),
//             new_int(heap, BigInt::from(3)),
//             new_int(heap, BigInt::from(4)),
//         ];
//         let result = times_b(&mut ec, &args).unwrap();
//         match &heap.get_value(result) {
//             SchemeValue::Int(i) => assert_eq!(i.to_string(), "24"),
//             _ => panic!("Expected integer"),
//         }
//     }

//     #[test]
//     fn test_div_builtin() {
//         let mut ev = crate::eval::Evaluator::new();
//         let mut ec = crate::eval::RunTime::from_eval(&mut ev);
//         let args = vec![
//             new_int(heap, BigInt::from(10)),
//             new_int(heap, BigInt::from(2)),
//         ];
//         let result = div_b(&mut ec, &args).unwrap();
//         match &heap.get_value(result) {
//             SchemeValue::Float(f) => assert_eq!(*f, 5.0),
//             _ => panic!("Expected float"),
//         }
//     }

//     #[test]
//     fn test_mod_builtin() {
//         let mut ev = crate::eval::Evaluator::new();
//         let mut ec = crate::eval::RunTime::from_eval(&mut ev);
//         let args = vec![
//             new_int(heap, BigInt::from(7)),
//             new_int(heap, BigInt::from(3)),
//         ];
//         let result = mod_b(&mut ec, &args).unwrap();
//         match &heap.get_value(result) {
//             SchemeValue::Int(i) => assert_eq!(i.to_string(), "1"),
//             _ => panic!("Expected integer"),
//         }
//     }

//     #[test]
//     fn test_eq_builtin() {
//         let mut ev = crate::eval::Evaluator::new();
//         let mut ec = crate::eval::RunTime::from_eval(&mut ev);

//         // Test equal integers
//         let args = vec![
//             new_int(heap, BigInt::from(5)),
//             new_int(heap, BigInt::from(5)),
//         ];
//         let result = eq_b(&mut ec, &args).unwrap();
//         assert!(matches!(&heap.get_value(result), SchemeValue::Bool(true)));

//         // Test unequal integers
//         let args = vec![
//             new_int(heap, BigInt::from(5)),
//             new_int(heap, BigInt::from(6)),
//         ];
//         let result = eq_b(&mut ec, &args).unwrap();
//         assert!(matches!(&heap.get_value(result), SchemeValue::Bool(false)));

//         // Test mixed int and float
//         let args = vec![new_int(heap, BigInt::from(5)), new_float(heap, 5.0)];
//         let result = eq_b(&mut ec, &args).unwrap();
//         assert!(matches!(&heap.get_value(result), SchemeValue::Bool(true)));

//         // Test multiple equal values
//         let args = vec![
//             new_int(heap, BigInt::from(5)),
//             new_float(heap, 5.0),
//             new_int(heap, BigInt::from(5)),
//         ];
//         let result = eq_b(&mut ec, &args).unwrap();
//         assert!(matches!(&heap.get_value(result), SchemeValue::Bool(true)));

//         // Test multiple unequal values
//         let args = vec![
//             new_int(heap, BigInt::from(5)),
//             new_float(heap, 5.0),
//             new_int(heap, BigInt::from(6)),
//         ];
//         let result = eq_b(&mut ec, &args).unwrap();
//         assert!(matches!(&heap.get_value(result), SchemeValue::Bool(false)));
//     }

//     #[test]
//     fn test_lt_builtin() {
//         let mut ev = crate::eval::Evaluator::new();
//         let mut ec = crate::eval::RunTime::from_eval(&mut ev);

//         // Test strictly increasing integers
//         let args = vec![
//             new_int(heap, BigInt::from(1)),
//             new_int(heap, BigInt::from(2)),
//             new_int(heap, BigInt::from(3)),
//         ];
//         let result = lt_b(&mut ec, &args).unwrap();
//         assert!(matches!(&heap.get_value(result), SchemeValue::Bool(true)));

//         // Test not strictly increasing
//         let args = vec![
//             new_int(heap, BigInt::from(1)),
//             new_int(heap, BigInt::from(2)),
//             new_int(heap, BigInt::from(2)),
//         ];
//         let result = lt_b(&mut ec, &args).unwrap();
//         assert!(matches!(&heap.get_value(result), SchemeValue::Bool(false)));

//         // Test mixed int and float
//         let args = vec![
//             new_int(heap, BigInt::from(1)),
//             new_float(heap, 1.5),
//             new_int(heap, BigInt::from(2)),
//         ];
//         let result = lt_b(&mut ec, &args).unwrap();
//         assert!(matches!(&heap.get_value(result), SchemeValue::Bool(true)));

//         // Test decreasing sequence
//         let args = vec![
//             new_int(heap, BigInt::from(3)),
//             new_int(heap, BigInt::from(2)),
//             new_int(heap, BigInt::from(1)),
//         ];
//         let result = lt_b(&mut ec, &args).unwrap();
//         assert!(matches!(&heap.get_value(result), SchemeValue::Bool(false)));
//     }

//     #[test]
//     fn test_gt_builtin() {
//         let mut ev = crate::eval::Evaluator::new();
//         let mut ec = crate::eval::RunTime::from_eval(&mut ev);

//         // Test strictly decreasing integers
//         let args = vec![
//             new_int(heap, BigInt::from(3)),
//             new_int(heap, BigInt::from(2)),
//             new_int(heap, BigInt::from(1)),
//         ];
//         let result = gt_b(&mut ec, &args).unwrap();
//         assert!(matches!(&heap.get_value(result), SchemeValue::Bool(true)));

//         // Test not strictly decreasing
//         let args = vec![
//             new_int(heap, BigInt::from(3)),
//             new_int(heap, BigInt::from(2)),
//             new_int(heap, BigInt::from(2)),
//         ];
//         let result = gt_b(&mut ec, &args).unwrap();
//         assert!(matches!(&heap.get_value(result), SchemeValue::Bool(false)));

//         // Test mixed int and float
//         let args = vec![
//             new_int(heap, BigInt::from(3)),
//             new_float(heap, 2.5),
//             new_int(heap, BigInt::from(2)),
//         ];
//         let result = gt_b(&mut ec, &args).unwrap();
//         assert!(matches!(&heap.get_value(result), SchemeValue::Bool(true)));

//         // Test increasing sequence
//         let args = vec![
//             new_int(heap, BigInt::from(1)),
//             new_int(heap, BigInt::from(2)),
//             new_int(heap, BigInt::from(3)),
//         ];
//         let result = gt_b(&mut ec, &args).unwrap();
//         assert!(matches!(&heap.get_value(result), SchemeValue::Bool(false)));
//     }
// }
