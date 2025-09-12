use crate::env::{EnvOps, EnvRef};
use crate::gc::{GcHeap, GcRef, SchemeValue, get_nil, new_pair, set_car, set_cdr};
use crate::gc_value;
use num_traits::ToPrimitive;

/// Builtin function: (car pair)
///
/// Returns the first element of a pair.
pub fn car_builtin(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("car: expects exactly 1 argument".to_string());
    }
    match &heap.get_value(args[0]) {
        SchemeValue::Pair(car, _) => Ok(*car),
        _ => Err("car: argument must be a pair".to_string()),
    }
}

/// Builtin function: (cdr pair)
///
/// Returns the second element of a pair.
pub fn cdr_builtin(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("cdr: expects exactly 1 argument".to_string());
    }

    match &heap.get_value(args[0]) {
        SchemeValue::Pair(_, cdr) => Ok(*cdr),
        _ => Err("cdr: argument must be a pair".to_string()),
    }
}

/// Builtin function: (cons car cdr)
///
/// Creates a new pair with the given car and cdr.
pub fn cons_builtin(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("cons: expects exactly 2 arguments".to_string());
    }

    let car = args[0];
    let cdr = args[1];

    Ok(new_pair(heap, car, cdr))
}

/// Builtin function: (list arg1 arg2 ...)
///
/// Creates a list from the given arguments.
pub fn list_builtin(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.is_empty() {
        return Ok(get_nil(heap));
    }

    // Build the list from right to left
    let mut result = get_nil(heap);
    for &arg in args.iter().rev() {
        result = new_pair(heap, arg, result);
    }

    Ok(result)
}

/// Builtin function: (append list1 list2 ...)
///
/// Appends multiple lists together.
pub fn append_builtin(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.is_empty() {
        return Ok(heap.nil_s());
    }

    let num_args = args.len();
    let last_arg = args[num_args - 1];

    // If there's only one argument, just return it.
    if num_args == 1 {
        return Ok(last_arg);
    }

    let mut result_head = heap.nil_s();
    let mut result_tail = heap.nil_s();

    // Iterate through all but the last argument.
    for &arg in &args[..num_args - 1] {
        let mut current = arg;
        loop {
            match &gc_value!(current) {
                SchemeValue::Pair(car, cdr) => {
                    let new_pair = new_pair(heap, *car, heap.nil_s());
                    if result_head == heap.nil_s() {
                        result_head = new_pair;
                        result_tail = new_pair;
                    } else {
                        set_cdr(result_tail, new_pair)?;
                        result_tail = new_pair;
                    }
                    current = *cdr;
                }
                SchemeValue::Nil => break,
                _ => {
                    return Err(
                        "append: arguments before the last must be proper lists".to_string()
                    );
                }
            }
        }
    }

    // If all but the last argument were empty lists, the result is just the last argument.
    if result_head == heap.nil_s() {
        return Ok(last_arg);
    }

    // Attach the last argument.
    set_cdr(result_tail, last_arg)?;

    Ok(result_head)
}

pub fn reverse_builtin(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 1 {
        return Err("reverse: expects exactly 1 argument".to_string());
    }

    let mut result = heap.nil_s();
    let mut current = args[0];

    loop {
        match &gc_value!(current) {
            SchemeValue::Pair(car, cdr) => {
                result = new_pair(heap, *car, result);
                current = *cdr;
            }
            SchemeValue::Nil => break,
            _ => return Err("reverse: argument must be a proper list".to_string()),
        }
    }

    Ok(result)
}

pub fn list_tail_builtin(_heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("list-tail: expects exactly 2 arguments".to_string());
    }

    let mut list = args[0];
    let k = match &gc_value!(args[1]) {
        SchemeValue::Int(i) => i
            .to_i64()
            .ok_or("list-tail: index must be an exact integer")?,
        _ => return Err("list-tail: index must be an exact integer".to_string()),
    };

    if k < 0 {
        return Err("list-tail: index must be non-negative".to_string());
    }

    for _ in 0..k {
        match &gc_value!(list) {
            SchemeValue::Pair(_, cdr) => {
                list = *cdr;
            }
            _ => return Err("list-tail: index out of bounds".to_string()),
        }
    }

    Ok(list)
}

pub fn list_ref_builtin(_heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("list-ref: expects exactly 2 arguments".to_string());
    }

    let list = args[0];
    let k = match &gc_value!(args[1]) {
        SchemeValue::Int(i) => i
            .to_i64()
            .ok_or("list-ref: index must be an exact integer")?,
        _ => return Err("list-ref: index must be an exact integer".to_string()),
    };

    if k < 0 {
        return Err("list-ref: index must be non-negative".to_string());
    }

    let mut current = list;
    for _ in 0..k {
        match &gc_value!(current) {
            SchemeValue::Pair(_, cdr) => {
                current = *cdr;
            }
            _ => return Err("list-ref: index out of bounds".to_string()),
        }
    }

    match &gc_value!(current) {
        SchemeValue::Pair(car, _) => Ok(*car),
        _ => Err("list-ref: index out of bounds".to_string()),
    }
}

pub fn set_car_builtin(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("set-car!: wrong number of arguments".to_string());
    }
    let pair_ref = args[0];
    let new_car = args[1];
    set_car(pair_ref, new_car).unwrap();
    Ok(heap.nil_s())
}

pub fn set_cdr_builtin(heap: &mut GcHeap, args: &[GcRef]) -> Result<GcRef, String> {
    if args.len() != 2 {
        return Err("set-car!: wrong number of arguments".to_string());
    }
    let pair_ref = args[0];
    let new_cdr = args[1];
    set_cdr(pair_ref, new_cdr).unwrap();
    Ok(heap.nil_s())
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

pub fn register_list_builtins(heap: &mut GcHeap, env: EnvRef) {
    register_builtin_family!(heap, env,
        "car" => car_builtin,
        "cdr" => cdr_builtin,
        "set-car!" => set_car_builtin,
        "set-cdr!" => set_cdr_builtin,
        "cons" => cons_builtin,
        "list" => list_builtin,
        "append" => append_builtin,
        "reverse" => reverse_builtin,
        "list-tail" => list_tail_builtin,
        "list-ref" => list_ref_builtin,
    );
}

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_car_builtin() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);

        // Create a pair
        let car = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let cdr = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let pair = crate::gc::new_pair(ec.heap, car, cdr);

        // Test car
        let result = car_builtin(&mut ec.heap, &[pair]).unwrap();
        assert!(matches!(&ec.heap.get_value(result), SchemeValue::Int(i) if i.to_string() == "1"));

        // Test error: wrong number of arguments
        let result = car_builtin(&mut ec.heap, &[]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expects exactly 1 argument"));

        // Test error: not a pair
        let not_pair = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(42));
        let result = car_builtin(&mut ec.heap, &[not_pair]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("argument must be a pair"));
    }

    #[test]
    fn test_cdr_builtin() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);

        // Create a pair
        let car = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let cdr = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let pair = crate::gc::new_pair(ec.heap, car, cdr);

        // Test cdr
        let result = cdr_builtin(&mut ec.heap, &[pair]).unwrap();
        assert!(matches!(&ec.heap.get_value(result), SchemeValue::Int(i) if i.to_string() == "2"));

        // Test error: wrong number of arguments
        let result = cdr_builtin(&mut ec.heap, &[]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expects exactly 1 argument"));

        // Test error: not a pair
        let not_pair = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(42));
        let result = cdr_builtin(&mut ec.heap, &[not_pair]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("argument must be a pair"));
    }

    #[test]
    fn test_cons_builtin() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);

        let car = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let cdr = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));

        // Test cons
        let result = cons_builtin(&mut ec.heap, &[car, cdr]).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Pair(_, _)
        ));

        // Test error: wrong number of arguments
        let result = cons_builtin(&mut ec.heap, &[car]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expects exactly 2 arguments"));
    }

    #[test]
    fn test_list_builtin() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);

        // Test empty list
        let result = list_builtin(&mut ec.heap, &[]).unwrap();
        assert!(matches!(&ec.heap.get_value(result), SchemeValue::Nil));

        // Test single element
        let elem = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(42));
        let result = list_builtin(&mut ec.heap, &[elem]).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Pair(_, _)
        ));

        // Test multiple elements
        let elem1 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let elem2 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let elem3 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(3));
        let result = list_builtin(&mut ec.heap, &[elem1, elem2, elem3]).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Pair(_, _)
        ));
    }

    #[test]
    fn test_append_builtin() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);

        // Test empty arguments
        let result = append_builtin(&mut ec.heap, &[]).unwrap();
        assert!(matches!(&ec.heap.get_value(result), SchemeValue::Nil));

        // Test single list
        let elem1 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let elem2 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let list1 = list_builtin(&mut ec.heap, &[elem1, elem2]).unwrap();
        let result = append_builtin(&mut ec.heap, &[list1]).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Pair(_, _)
        ));

        // Test appending two lists
        let elem3 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(3));
        let elem4 = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(4));
        let list2 = list_builtin(&mut ec.heap, &[elem3, elem4]).unwrap();
        let result = append_builtin(&mut ec.heap, &[list1, list2]).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Pair(_, _)
        ));

        // Test appending empty list
        let empty_list = get_nil(ec.heap);
        let result = append_builtin(&mut ec.heap, &[list1, empty_list]).unwrap();
        assert!(matches!(
            &ec.heap.get_value(result),
            SchemeValue::Pair(_, _)
        ));
    }

    #[test]
    fn test_append_improper_list() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);

        // (append '(1 2) 'a)
        let one = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let two = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let list1 = list_builtin(&mut ec.heap, &[one, two]).unwrap();
        let sym_a = ec.heap.intern_symbol("a");
        let result = append_builtin(&mut ec.heap, &[list1, sym_a]).unwrap();
        let third = crate::gc::cdr(crate::gc::cdr(result).unwrap()).unwrap();
        assert!(matches!(&ec.heap.get_value(third), SchemeValue::Symbol(s) if s == "a"));

        // (append '() 'a)
        let empty_list = get_nil(ec.heap);
        let result = append_builtin(&mut ec.heap, &[empty_list, sym_a]).unwrap();
        assert!(matches!(&ec.heap.get_value(result), SchemeValue::Symbol(s) if s == "a"));

        // (append '(1 2) '(c . d))
        let c = ec.heap.intern_symbol("c");
        let d = ec.heap.intern_symbol("d");
        let improper_list = cons_builtin(&mut ec.heap, &[c, d]).unwrap();
        let result = append_builtin(&mut ec.heap, &[list1, improper_list]).unwrap();
        let third = crate::gc::cdr(crate::gc::cdr(result).unwrap()).unwrap();
        assert!(matches!(&ec.heap.get_value(third), SchemeValue::Pair(_, _)));
        let fourth = crate::gc::cdr(third).unwrap();
        assert!(matches!(&ec.heap.get_value(fourth), SchemeValue::Symbol(s) if s == "d"));
    }

    #[test]
    fn test_reverse_builtin() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);

        // (reverse '(1 2 3))
        let one = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let two = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let three = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(3));
        let list1 = list_builtin(&mut ec.heap, &[one, two, three]).unwrap();
        let result = reverse_builtin(&mut ec.heap, &[list1]).unwrap();
        let first = crate::gc::car(result).unwrap();
        assert!(matches!(&ec.heap.get_value(first), SchemeValue::Int(i) if i.to_string() == "3"));
        let second = crate::gc::car(crate::gc::cdr(result).unwrap()).unwrap();
        assert!(matches!(&ec.heap.get_value(second), SchemeValue::Int(i) if i.to_string() == "2"));
        let third =
            crate::gc::car(crate::gc::cdr(crate::gc::cdr(result).unwrap()).unwrap()).unwrap();
        assert!(matches!(&ec.heap.get_value(third), SchemeValue::Int(i) if i.to_string() == "1"));
    }

    #[test]
    fn test_list_tail_builtin() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);

        // (list-tail '(1 2 3) 1)
        let one = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let two = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let three = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(3));
        let list1 = list_builtin(&mut ec.heap, &[one, two, three]).unwrap();
        let k = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let result = list_tail_builtin(&mut ec.heap, &[list1, k]).unwrap();
        let first = crate::gc::car(result).unwrap();
        assert!(matches!(&ec.heap.get_value(first), SchemeValue::Int(i) if i.to_string() == "2"));
    }

    #[test]
    fn test_list_ref_builtin() {
        let mut ev = crate::eval::RunTimeStruct::new();
        let mut ec = crate::eval::RunTime::from_eval(&mut ev);

        // (list-ref '(1 2 3) 1)
        let one = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let two = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(2));
        let three = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(3));
        let list1 = list_builtin(&mut ec.heap, &[one, two, three]).unwrap();
        let k = crate::gc::new_int(ec.heap, num_bigint::BigInt::from(1));
        let result = list_ref_builtin(&mut ec.heap, &[list1, k]).unwrap();
        assert!(matches!(&ec.heap.get_value(result), SchemeValue::Int(i) if i.to_string() == "2"));
    }
}
