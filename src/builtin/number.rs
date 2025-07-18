use num_bigint::BigInt;
use num_traits::{ToPrimitive, Zero, One};
use crate::gc::{SchemeValue, new_int, new_float};

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