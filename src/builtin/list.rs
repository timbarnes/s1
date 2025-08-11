use crate::eval::EvalContext;
use crate::gc::{GcHeap, GcRef, SchemeValue, get_nil, new_pair, set_car, set_cdr};

/// Builtin function: (car pair)
///
/// Returns the first element of a pair.
pub fn car_builtin(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("car: expects exactly 1 argument".to_string());
    }

    match &ec.heap.get_value(args[0]) {
        SchemeValue::Pair(car, _) => Ok(*car),
        _ => Err("car: argument must be a pair".to_string()),
    }
}

/// Builtin function: (cdr pair)
///
/// Returns the second element of a pair.
pub fn cdr_builtin(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("cdr: expects exactly 1 argument".to_string());
    }

    match &ec.heap.get_value(args[0]) {
        SchemeValue::Pair(_, cdr) => Ok(*cdr),
        _ => Err("cdr: argument must be a pair".to_string()),
    }
}

/// Builtin function: (cons car cdr)
///
/// Creates a new pair with the given car and cdr.
pub fn cons_builtin(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("cons: expects exactly 2 arguments".to_string());
    }

    let car = args[0];
    let cdr = args[1];

    Ok(new_pair(ec.heap, car, cdr))
}

/// Builtin function: (list arg1 arg2 ...)
///
/// Creates a list from the given arguments.
pub fn list_builtin(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.is_empty() {
        return Ok(get_nil(ec.heap));
    }

    // Build the list from right to left
    let mut result = get_nil(ec.heap);
    for &arg in args.iter().rev() {
        result = new_pair(ec.heap, arg, result);
    }

    Ok(result)
}

/// Builtin function: (append list1 list2 ...)
///
/// Appends multiple lists together.
pub fn append_builtin(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.is_empty() {
        return Ok(get_nil(ec.heap));
    }

    if args.len() == 1 {
        return Ok(args[0]);
    }

    // Helper function to copy a list
    fn copy_list(ec: &mut EvalContext, lst: GcRef) -> Result<GcRef, String> {
        let val = unsafe { &(*lst).value };
        match &val {
            SchemeValue::Nil => Ok(get_nil(ec.heap)),
            SchemeValue::Pair(car, cdr) => {
                let car = car; // GcRef, just a pointer
                let cdr = cdr; // GcRef, just a pointer

                let new_cdr = copy_list(ec, *cdr)?;
                Ok(new_pair(ec.heap, *car, new_cdr))
            }
            _ => Err("append: arguments must be lists".to_string()),
        }
    }

    // Helper function to get the last pair of a list
    fn get_last_pair(ec: &mut EvalContext, lst: GcRef) -> Result<GcRef, String> {
        match &ec.heap.get_value(lst) {
            SchemeValue::Nil => Err("append: cannot append to empty list".to_string()),
            SchemeValue::Pair(_, cdr) => match &ec.heap.get_value(*cdr) {
                SchemeValue::Nil => Ok(lst),
                _ => get_last_pair(ec, *cdr),
            },
            _ => Err("append: arguments must be lists".to_string()),
        }
    }

    // Start with a copy of the first list
    let mut result = copy_list(ec, args[0])?;

    // For each subsequent list, append it to the result
    for &arg in &args[1..] {
        match &ec.heap.get_value(arg) {
            SchemeValue::Nil => {
                // Empty list, nothing to append
                continue;
            }
            SchemeValue::Pair(_, _) => {
                // Get the last pair of the result list
                let _last_pair = get_last_pair(ec, result)?;

                // Copy the current list
                let _list_copy = copy_list(ec, arg)?;

                // Update the cdr of the last pair to point to the copied list
                // This is a bit tricky since we need to modify the existing pair
                // For now, we'll rebuild the entire result list
                // TODO: Implement proper mutable pair modification

                // For now, we'll use a simpler approach: rebuild the entire result
                let mut all_elements = Vec::new();

                // Collect all elements from the result
                let mut current = result;
                loop {
                    match &ec.heap.get_value(current) {
                        SchemeValue::Nil => break,
                        SchemeValue::Pair(car, cdr) => {
                            all_elements.push(*car);
                            current = *cdr;
                        }
                        _ => return Err("append: arguments must be lists".to_string()),
                    }
                }

                // Collect all elements from the current list
                let mut current_list = arg;
                loop {
                    match &ec.heap.get_value(current_list) {
                        SchemeValue::Nil => break,
                        SchemeValue::Pair(car, cdr) => {
                            all_elements.push(*car);
                            current_list = *cdr;
                        }
                        _ => return Err("append: arguments must be lists".to_string()),
                    }
                }

                // Rebuild the result list
                result = get_nil(ec.heap);
                for &element in all_elements.iter().rev() {
                    result = new_pair(ec.heap, element, result);
                }
            }
            _ => return Err("append: arguments must be lists".to_string()),
        }
    }

    Ok(result)
}

pub fn set_car_builtin(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("set-car!: wrong number of arguments".to_string());
    }
    let pair_ref = args[0];
    let new_car = args[1];
    set_car(pair_ref, new_car).unwrap();
    Ok(ec.heap.nil_s())
}

pub fn set_cdr_builtin(ec: &mut EvalContext, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("set-car!: wrong number of arguments".to_string());
    }
    let pair_ref = args[0];
    let new_cdr = args[1];
    set_cdr(pair_ref, new_cdr).unwrap();
    Ok(ec.heap.nil_s())
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

pub fn register_list_builtins(heap: &mut GcHeap, env: &mut crate::env::Environment) {
    register_builtin_family!(heap, env,
        "car" => car_builtin,
        "cdr" => cdr_builtin,
        "set-car!" => set_car_builtin,
        "set-cdr!" => set_cdr_builtin,
        "cons" => cons_builtin,
        "list" => list_builtin,
        "append" => append_builtin,
    );
}

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_car_builtin() {
        let mut ev = crate::eval::Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut ev);

        // Create a pair
        let car = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let cdr = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let pair = crate::gc::new_pair(ec.heap, car, cdr);

        // Test car
        let result = car_builtin(&mut ec, &[pair]).unwrap();
        assert!(matches!(&ec.heap.get_value(result), SchemeValue::Int(i) if i.to_string() == "1"));

        // Test error: wrong number of arguments
        let result = car_builtin(&mut ec, &[]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expects exactly 1 argument"));

        // Test error: not a pair
        let not_pair = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(42));
        let result = car_builtin(&mut ec, &[not_pair]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("argument must be a pair"));
    }

    #[test]
    fn test_cdr_builtin() {
        let mut ev = crate::eval::Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut ev);

        // Create a pair
        let car = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let cdr = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let pair = crate::gc::new_pair(ec.heap, car, cdr);

        // Test cdr
        let result = cdr_builtin(&mut ec, &[pair]).unwrap();
        assert!(matches!(&ec.heap.get_value(result), SchemeValue::Int(i) if i.to_string() == "2"));

        // Test error: wrong number of arguments
        let result = cdr_builtin(&mut ec, &[]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expects exactly 1 argument"));

        // Test error: not a pair
        let not_pair = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(42));
        let result = cdr_builtin(&mut ec, &[not_pair]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("argument must be a pair"));
    }

    #[test]
    fn test_cons_builtin() {
        let mut ev = crate::eval::Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut ev);

        let car = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let cdr = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));

        // Test cons
        let result = cons_builtin(&mut ec, &[car, cdr]).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Pair(_, _)
        ));

        // Test error: wrong number of arguments
        let result = cons_builtin(&mut ec, &[car]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expects exactly 2 arguments"));
    }

    #[test]
    fn test_list_builtin() {
        let mut ev = crate::eval::Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut ev);

        // Test empty list
        let result = list_builtin(&mut ec, &[]).unwrap();
        assert!(matches!(&ec.heap.get_value(result), SchemeValue::Nil));

        // Test single element
        let elem = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(42));
        let result = list_builtin(&mut ec, &[elem]).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Pair(_, _)
        ));

        // Test multiple elements
        let elem1 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let elem2 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let elem3 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(3));
        let result = list_builtin(&mut ec, &[elem1, elem2, elem3]).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Pair(_, _)
        ));
    }

    #[test]
    fn test_append_builtin() {
        let mut ev = crate::eval::Evaluator::new();
        let mut ec = crate::eval::EvalContext::from_eval(&mut ev);

        // Test empty arguments
        let result = append_builtin(&mut ec, &[]).unwrap();
        assert!(matches!(&ec.heap.get_value(result), SchemeValue::Nil));

        // Test single list
        let elem1 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let elem2 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let list1 = list_builtin(&mut ec, &[elem1, elem2]).unwrap();
        let result = append_builtin(&mut ec, &[list1]).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Pair(_, _)
        ));

        // Test appending two lists
        let elem3 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(3));
        let elem4 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(4));
        let list2 = list_builtin(&mut ec, &[elem3, elem4]).unwrap();
        let result = append_builtin(&mut ec, &[list1, list2]).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Pair(_, _)
        ));

        // Test appending empty list
        let empty_list = get_nil(ec.heap);
        let result = append_builtin(&mut ec, &[list1, empty_list]).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Pair(_, _)
        ));
    }
}
