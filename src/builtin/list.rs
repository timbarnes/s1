use crate::gc::{GcRefSimple, new_pair_simple, new_nil_simple, SchemeValueSimple};

/// Builtin function: (car pair)
/// 
/// Returns the first element of a pair.
pub fn car_builtin(_heap: &mut crate::gc::GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if args.len() != 1 {
        return Err("car: expects exactly 1 argument".to_string());
    }
    
    match &args[0].value {
        SchemeValueSimple::Pair(car, _) => Ok(*car),
        _ => Err("car: argument must be a pair".to_string()),
    }
}

/// Builtin function: (cdr pair)
/// 
/// Returns the second element of a pair.
pub fn cdr_builtin(_heap: &mut crate::gc::GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if args.len() != 1 {
        return Err("cdr: expects exactly 1 argument".to_string());
    }
    
    match &args[0].value {
        SchemeValueSimple::Pair(_, cdr) => Ok(*cdr),
        _ => Err("cdr: argument must be a pair".to_string()),
    }
}

/// Builtin function: (cons car cdr)
/// 
/// Creates a new pair with the given car and cdr.
pub fn cons_builtin(heap: &mut crate::gc::GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if args.len() != 2 {
        return Err("cons: expects exactly 2 arguments".to_string());
    }
    
    let car = args[0];
    let cdr = args[1];
    
    Ok(new_pair_simple(heap, car, cdr))
}

/// Builtin function: (list arg1 arg2 ...)
/// 
/// Creates a list from the given arguments.
pub fn list_builtin(heap: &mut crate::gc::GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if args.is_empty() {
        return Ok(new_nil_simple(heap));
    }
    
    // Build the list from right to left
    let mut result = new_nil_simple(heap);
    for &arg in args.iter().rev() {
        result = new_pair_simple(heap, arg, result);
    }
    
    Ok(result)
}

/// Builtin function: (append list1 list2 ...)
/// 
/// Appends multiple lists together.
pub fn append_builtin(heap: &mut crate::gc::GcHeap, args: &[GcRefSimple]) -> Result<GcRefSimple, String> {
    if args.is_empty() {
        return Ok(new_nil_simple(heap));
    }
    
    if args.len() == 1 {
        return Ok(args[0]);
    }
    
    // Helper function to copy a list
    fn copy_list(heap: &mut crate::gc::GcHeap, lst: GcRefSimple) -> Result<GcRefSimple, String> {
        match &lst.value {
            SchemeValueSimple::Nil => Ok(new_nil_simple(heap)),
            SchemeValueSimple::Pair(car, cdr) => {
                let new_cdr = copy_list(heap, *cdr)?;
                Ok(new_pair_simple(heap, *car, new_cdr))
            }
            _ => Err("append: arguments must be lists".to_string()),
        }
    }
    
    // Helper function to get the last pair of a list
    fn get_last_pair(lst: GcRefSimple) -> Result<GcRefSimple, String> {
        match &lst.value {
            SchemeValueSimple::Nil => Err("append: cannot append to empty list".to_string()),
            SchemeValueSimple::Pair(_, cdr) => {
                match &cdr.value {
                    SchemeValueSimple::Nil => Ok(lst),
                    _ => get_last_pair(*cdr),
                }
            }
            _ => Err("append: arguments must be lists".to_string()),
        }
    }
    
    // Start with a copy of the first list
    let mut result = copy_list(heap, args[0])?;
    
    // For each subsequent list, append it to the result
    for &arg in &args[1..] {
        match &arg.value {
            SchemeValueSimple::Nil => {
                // Empty list, nothing to append
                continue;
            }
            SchemeValueSimple::Pair(_, _) => {
                // Get the last pair of the result list
                let last_pair = get_last_pair(result)?;
                
                // Copy the current list
                let list_copy = copy_list(heap, arg)?;
                
                // Update the cdr of the last pair to point to the copied list
                // This is a bit tricky since we need to modify the existing pair
                // For now, we'll rebuild the entire result list
                // TODO: Implement proper mutable pair modification
                
                // For now, we'll use a simpler approach: rebuild the entire result
                let mut all_elements = Vec::new();
                
                // Collect all elements from the result
                let mut current = result;
                loop {
                    match &current.value {
                        SchemeValueSimple::Nil => break,
                        SchemeValueSimple::Pair(car, cdr) => {
                            all_elements.push(*car);
                            current = *cdr;
                        }
                        _ => return Err("append: arguments must be lists".to_string()),
                    }
                }
                
                // Collect all elements from the current list
                let mut current_list = arg;
                loop {
                    match &current_list.value {
                        SchemeValueSimple::Nil => break,
                        SchemeValueSimple::Pair(car, cdr) => {
                            all_elements.push(*car);
                            current_list = *cdr;
                        }
                        _ => return Err("append: arguments must be lists".to_string()),
                    }
                }
                
                // Rebuild the result list
                result = new_nil_simple(heap);
                for &element in all_elements.iter().rev() {
                    result = new_pair_simple(heap, element, result);
                }
            }
            _ => return Err("append: arguments must be lists".to_string()),
        }
    }
    
    Ok(result)
}

mod tests {
    use super::*;
    use crate::gc::GcHeap;
    use num_bigint::BigInt;

    #[test]
    fn test_car_builtin() {
        let mut heap = GcHeap::new();
        
        // Create a pair
        let car = crate::gc::new_int_simple(&mut heap, BigInt::from(1));
        let cdr = crate::gc::new_int_simple(&mut heap, BigInt::from(2));
        let pair = crate::gc::new_pair_simple(&mut heap, car, cdr);
        
        // Test car
        let result = car_builtin(&mut heap, &[pair]).unwrap();
        assert!(matches!(&result.value, SchemeValueSimple::Int(i) if i.to_string() == "1"));
        
        // Test error: wrong number of arguments
        let result = car_builtin(&mut heap, &[]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expects exactly 1 argument"));
        
        // Test error: not a pair
        let not_pair = crate::gc::new_int_simple(&mut heap, BigInt::from(42));
        let result = car_builtin(&mut heap, &[not_pair]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("argument must be a pair"));
    }

    #[test]
    fn test_cdr_builtin() {
        let mut heap = GcHeap::new();
        
        // Create a pair
        let car = crate::gc::new_int_simple(&mut heap, BigInt::from(1));
        let cdr = crate::gc::new_int_simple(&mut heap, BigInt::from(2));
        let pair = crate::gc::new_pair_simple(&mut heap, car, cdr);
        
        // Test cdr
        let result = cdr_builtin(&mut heap, &[pair]).unwrap();
        assert!(matches!(&result.value, SchemeValueSimple::Int(i) if i.to_string() == "2"));
        
        // Test error: wrong number of arguments
        let result = cdr_builtin(&mut heap, &[]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expects exactly 1 argument"));
        
        // Test error: not a pair
        let not_pair = crate::gc::new_int_simple(&mut heap, BigInt::from(42));
        let result = cdr_builtin(&mut heap, &[not_pair]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("argument must be a pair"));
    }

    #[test]
    fn test_cons_builtin() {
        let mut heap = GcHeap::new();
        
        let car = crate::gc::new_int_simple(&mut heap, BigInt::from(1));
        let cdr = crate::gc::new_int_simple(&mut heap, BigInt::from(2));
        
        // Test cons
        let result = cons_builtin(&mut heap, &[car, cdr]).unwrap();
        assert!(matches!(&result.value, SchemeValueSimple::Pair(_, _)));
        
        // Test error: wrong number of arguments
        let result = cons_builtin(&mut heap, &[car]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expects exactly 2 arguments"));
    }

    #[test]
    fn test_list_builtin() {
        let mut heap = GcHeap::new();
        
        // Test empty list
        let result = list_builtin(&mut heap, &[]).unwrap();
        assert!(matches!(&result.value, SchemeValueSimple::Nil));
        
        // Test single element
        let elem = crate::gc::new_int_simple(&mut heap, BigInt::from(42));
        let result = list_builtin(&mut heap, &[elem]).unwrap();
        assert!(matches!(&result.value, SchemeValueSimple::Pair(_, _)));
        
        // Test multiple elements
        let elem1 = crate::gc::new_int_simple(&mut heap, BigInt::from(1));
        let elem2 = crate::gc::new_int_simple(&mut heap, BigInt::from(2));
        let elem3 = crate::gc::new_int_simple(&mut heap, BigInt::from(3));
        let result = list_builtin(&mut heap, &[elem1, elem2, elem3]).unwrap();
        assert!(matches!(&result.value, SchemeValueSimple::Pair(_, _)));
    }

    #[test]
    fn test_append_builtin() {
        let mut heap = GcHeap::new();
        
        // Test empty arguments
        let result = append_builtin(&mut heap, &[]).unwrap();
        assert!(matches!(&result.value, SchemeValueSimple::Nil));
        
        // Test single list
        let elem1 = crate::gc::new_int_simple(&mut heap, BigInt::from(1));
        let elem2 = crate::gc::new_int_simple(&mut heap, BigInt::from(2));
        let list1 = list_builtin(&mut heap, &[elem1, elem2]).unwrap();
        let result = append_builtin(&mut heap, &[list1]).unwrap();
        assert!(matches!(&result.value, SchemeValueSimple::Pair(_, _)));
        
        // Test appending two lists
        let elem3 = crate::gc::new_int_simple(&mut heap, BigInt::from(3));
        let elem4 = crate::gc::new_int_simple(&mut heap, BigInt::from(4));
        let list2 = list_builtin(&mut heap, &[elem3, elem4]).unwrap();
        let result = append_builtin(&mut heap, &[list1, list2]).unwrap();
        assert!(matches!(&result.value, SchemeValueSimple::Pair(_, _)));
        
        // Test appending empty list
        let empty_list = new_nil_simple(&mut heap);
        let result = append_builtin(&mut heap, &[list1, empty_list]).unwrap();
        assert!(matches!(&result.value, SchemeValueSimple::Pair(_, _)));
    }
} 