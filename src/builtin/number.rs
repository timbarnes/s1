use crate::eval::EvalContext;
use crate::gc::{GcHeap, GcRef, SchemeValue, new_bool, new_float, new_int};
use num_bigint::BigInt;
use num_traits::{One, ToPrimitive, Zero};

pub fn plus_builtin(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 2 {
        return Err("+: expects at least 2 arguments".to_string());
    }
    let mut is_float = false;
    let mut sum_int = BigInt::from(0);
    let mut sum_float = 0.0;
    for arg in args {
        match &ec.heap.get_value(*arg) {
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
        Ok(new_float(ec.heap, sum_float))
    } else {
        Ok(new_int(ec.heap, sum_int))
    }
}

pub fn minus_builtin(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 1 {
        return Err("-: expects at least 1 argument".to_string());
    }
    let mut is_float = false;
    let mut result_int;
    let mut result_float;
    let mut iter = args.iter();
    match &ec.heap.get_value(*iter.next().unwrap()) {
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
            return Ok(new_float(ec.heap, -result_float));
        } else {
            return Ok(new_int(ec.heap, -result_int));
        }
    }
    for arg in iter {
        match &ec.heap.get_value(*arg) {
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
        Ok(new_float(ec.heap, result_float))
    } else {
        Ok(new_int(ec.heap, result_int))
    }
}

pub fn times_builtin(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 2 {
        return Err("*: expects at least 2 arguments".to_string());
    }
    let mut is_float = false;
    let mut prod_int = BigInt::one();
    let mut prod_float = 1.0;
    for arg in args {
        match &ec.heap.get_value(*arg) {
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
        Ok(new_float(ec.heap, prod_float))
    } else {
        Ok(new_int(ec.heap, prod_int))
    }
}

pub fn div_builtin(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 2 {
        return Err("/: expects at least 2 arguments".to_string());
    }
    let mut result_float;
    let mut iter = args.iter();
    match &ec.heap.get_value(*iter.next().unwrap()) {
        SchemeValue::Int(i) => {
            result_float = i.to_f64().unwrap();
        }
        SchemeValue::Float(f) => {
            result_float = *f;
        }
        _ => return Err("/: all arguments must be numbers".to_string()),
    }
    for arg in iter {
        match &ec.heap.get_value(*arg) {
            SchemeValue::Int(i) => {
                result_float /= i.to_f64().unwrap();
            }
            SchemeValue::Float(f) => {
                result_float /= f;
            }
            _ => return Err("/: all arguments must be numbers".to_string()),
        }
    }
    Ok(new_float(ec.heap, result_float))
}

pub fn mod_builtin(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("mod: expects exactly 2 arguments".to_string());
    }
    let a = match &ec.heap.get_value(args[0]) {
        SchemeValue::Int(i) => i,
        _ => return Err("mod: arguments must be integers".to_string()),
    };
    let b = match &ec.heap.get_value(args[1]) {
        SchemeValue::Int(i) => i,
        _ => return Err("mod: arguments must be integers".to_string()),
    };
    if b.is_zero() {
        return Err("mod: division by zero".to_string());
    }
    Ok(new_int(ec.heap, a % b))
}

pub fn eq_builtin(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 2 {
        return Err("=: expects at least 2 arguments".to_string());
    }

    // Convert all arguments to f64 for comparison
    let mut numbers = Vec::new();
    for arg in args {
        let num = match &ec.heap.get_value(*arg) {
            SchemeValue::Int(i) => i.to_f64().unwrap(),
            SchemeValue::Float(f) => *f,
            _ => return Err("=: all arguments must be numbers".to_string()),
        };
        numbers.push(num);
    }

    // Check if all numbers are equal
    let first = numbers[0];
    let all_equal = numbers.iter().all(|&n| n == first);

    Ok(new_bool(ec.heap, all_equal))
}

pub fn lt_builtin(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 2 {
        return Err("<: expects at least 2 arguments".to_string());
    }

    // Convert all arguments to f64 for comparison
    let mut numbers = Vec::new();
    for arg in args {
        let num = match &ec.heap.get_value(*arg) {
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
            return Ok(new_bool(ec.heap, false));
        }
        prev = num;
    }

    Ok(new_bool(ec.heap, true))
}

pub fn gt_builtin(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() < 2 {
        return Err(">: expects at least 2 arguments".to_string());
    }

    // Convert all arguments to f64 for comparison
    let mut numbers = Vec::new();
    for arg in args {
        let num = match &ec.heap.get_value(*arg) {
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
            return Ok(new_bool(ec.heap, false));
        }
        prev = num;
    }

    Ok(new_bool(ec.heap, true))
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

pub fn register_number_builtins(heap: &mut GcHeap, env: &mut crate::env::Environment) {
    register_builtin_family!(heap, env,
        "+" => plus_builtin,
        "-" => minus_builtin,
        "*" => times_builtin,
        "/" => div_builtin,
        "mod" => mod_builtin,
        "=" => eq_builtin,
        "<" => lt_builtin,
        ">" => gt_builtin,
    );
}

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_plus_builtin() {
        let mut ev = crate::eval::Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut ev);
        let args = vec![
            new_int(ec.heap, BigInt::from(1)),
            new_int(ec.heap, BigInt::from(2)),
            new_int(ec.heap, BigInt::from(3)),
        ];
        let result = plus_builtin(&mut ec, &args).unwrap();
        match &ec.heap.get_value(result) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "6"),
            _ => panic!("Expected integer"),
        }
    }

    #[test]
    fn test_minus_builtin() {
        let mut ev = crate::eval::Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut ev);
        let args = vec![
            new_int(ec.heap, BigInt::from(10)),
            new_int(ec.heap, BigInt::from(3)),
        ];
        let result = minus_builtin(&mut ec, &args).unwrap();
        match &ec.heap.get_value(result) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "7"),
            _ => panic!("Expected integer"),
        }
    }

    #[test]
    fn test_times_builtin() {
        let mut ev = crate::eval::Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut ev);
        let args = vec![
            new_int(ec.heap, BigInt::from(2)),
            new_int(ec.heap, BigInt::from(3)),
            new_int(ec.heap, BigInt::from(4)),
        ];
        let result = times_builtin(&mut ec, &args).unwrap();
        match &ec.heap.get_value(result) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "24"),
            _ => panic!("Expected integer"),
        }
    }

    #[test]
    fn test_div_builtin() {
        let mut ev = crate::eval::Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut ev);
        let args = vec![
            new_int(ec.heap, BigInt::from(10)),
            new_int(ec.heap, BigInt::from(2)),
        ];
        let result = div_builtin(&mut ec, &args).unwrap();
        match &ec.heap.get_value(result) {
            SchemeValue::Float(f) => assert_eq!(*f, 5.0),
            _ => panic!("Expected float"),
        }
    }

    #[test]
    fn test_mod_builtin() {
        let mut ev = crate::eval::Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut ev);
        let args = vec![
            new_int(ec.heap, BigInt::from(7)),
            new_int(ec.heap, BigInt::from(3)),
        ];
        let result = mod_builtin(&mut ec, &args).unwrap();
        match &ec.heap.get_value(result) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "1"),
            _ => panic!("Expected integer"),
        }
    }

    #[test]
    fn test_eq_builtin() {
        let mut ev = crate::eval::Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut ev);

        // Test equal integers
        let args = vec![
            new_int(ec.heap, BigInt::from(5)),
            new_int(ec.heap, BigInt::from(5)),
        ];
        let result = eq_builtin(&mut ec, &args).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Bool(true)
        ));

        // Test unequal integers
        let args = vec![
            new_int(ec.heap, BigInt::from(5)),
            new_int(ec.heap, BigInt::from(6)),
        ];
        let result = eq_builtin(&mut ec, &args).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Bool(false)
        ));

        // Test mixed int and float
        let args = vec![new_int(ec.heap, BigInt::from(5)), new_float(ec.heap, 5.0)];
        let result = eq_builtin(&mut ec, &args).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Bool(true)
        ));

        // Test multiple equal values
        let args = vec![
            new_int(ec.heap, BigInt::from(5)),
            new_float(ec.heap, 5.0),
            new_int(ec.heap, BigInt::from(5)),
        ];
        let result = eq_builtin(&mut ec, &args).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Bool(true)
        ));

        // Test multiple unequal values
        let args = vec![
            new_int(ec.heap, BigInt::from(5)),
            new_float(ec.heap, 5.0),
            new_int(ec.heap, BigInt::from(6)),
        ];
        let result = eq_builtin(&mut ec, &args).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Bool(false)
        ));
    }

    #[test]
    fn test_lt_builtin() {
        let mut ev = crate::eval::Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut ev);

        // Test strictly increasing integers
        let args = vec![
            new_int(ec.heap, BigInt::from(1)),
            new_int(ec.heap, BigInt::from(2)),
            new_int(ec.heap, BigInt::from(3)),
        ];
        let result = lt_builtin(&mut ec, &args).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Bool(true)
        ));

        // Test not strictly increasing
        let args = vec![
            new_int(ec.heap, BigInt::from(1)),
            new_int(ec.heap, BigInt::from(2)),
            new_int(ec.heap, BigInt::from(2)),
        ];
        let result = lt_builtin(&mut ec, &args).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Bool(false)
        ));

        // Test mixed int and float
        let args = vec![
            new_int(ec.heap, BigInt::from(1)),
            new_float(ec.heap, 1.5),
            new_int(ec.heap, BigInt::from(2)),
        ];
        let result = lt_builtin(&mut ec, &args).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Bool(true)
        ));

        // Test decreasing sequence
        let args = vec![
            new_int(ec.heap, BigInt::from(3)),
            new_int(ec.heap, BigInt::from(2)),
            new_int(ec.heap, BigInt::from(1)),
        ];
        let result = lt_builtin(&mut ec, &args).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Bool(false)
        ));
    }

    #[test]
    fn test_gt_builtin() {
        let mut ev = crate::eval::Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut ev);

        // Test strictly decreasing integers
        let args = vec![
            new_int(ec.heap, BigInt::from(3)),
            new_int(ec.heap, BigInt::from(2)),
            new_int(ec.heap, BigInt::from(1)),
        ];
        let result = gt_builtin(&mut ec, &args).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Bool(true)
        ));

        // Test not strictly decreasing
        let args = vec![
            new_int(ec.heap, BigInt::from(3)),
            new_int(ec.heap, BigInt::from(2)),
            new_int(ec.heap, BigInt::from(2)),
        ];
        let result = gt_builtin(&mut ec, &args).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Bool(false)
        ));

        // Test mixed int and float
        let args = vec![
            new_int(ec.heap, BigInt::from(3)),
            new_float(ec.heap, 2.5),
            new_int(ec.heap, BigInt::from(2)),
        ];
        let result = gt_builtin(&mut ec, &args).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Bool(true)
        ));

        // Test increasing sequence
        let args = vec![
            new_int(ec.heap, BigInt::from(1)),
            new_int(ec.heap, BigInt::from(2)),
            new_int(ec.heap, BigInt::from(3)),
        ];
        let result = gt_builtin(&mut ec, &args).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Bool(false)
        ));
    }
}
