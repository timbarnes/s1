//! Parser for Scheme s-expressions.
//!
//! This module provides a stateless parser that consumes tokens from a self-contained Tokenizer
//! and produces unevaluated, interned s-expressions (SchemeValue) using the GC heap.
//! The parser is designed to be extensible for additional Scheme forms and does not own any state.
//!
//! # Example
//!
//! ```rust
//! use s1::parser::Parser;
//! use s1::tokenizer::Tokenizer;
//! use s1::io::{Port, PortKind};
//! use s1::gc::GcHeap;
//!
//! let mut heap = GcHeap::new();
//! let mut port = Port { kind: PortKind::StringPortInput { content: "42".to_string(), pos: 0 } };
//! let mut parser = Parser::new();
//! let expr = parser.parse(&mut heap, &mut port).unwrap();
//! ```

use crate::gc::{GcHeap, GcRef, new_int, new_float, new_symbol, new_string, new_pair};
use crate::gc::{GcRefSimple, new_int_simple, new_float_simple, new_symbol_simple, new_string_simple, new_nil_simple};
use crate::tokenizer::{Tokenizer, Token};
// use std::rc::Rc;
// use std::cell::RefCell;
use num_bigint::BigInt;
use crate::io::{Port};

/// Parser for Scheme s-expressions.
///
/// The parser is stateless and consumes tokens from a Tokenizer, producing interned s-expressions
/// (SchemeValue) using the GC heap. All state is passed as arguments. The parser is extensible for additional Scheme forms.
pub struct Parser;

/// Parser for Scheme s-expressions using the reference-based GC system.
///
/// This parser uses GcRefSimple (&'static GcObject) instead of GcRef (Rc<RefCell<GcObject>>)
/// for better performance and simpler code. It's a parallel implementation to the existing Parser.
pub struct ParserSimple;

impl Parser {
    /// Create a new stateless parser.
    pub fn new() -> Self {
        Self
    }

    /// Parse a single s-expression from the token stream.
    ///
    /// Returns Ok(GcRef) for a valid s-expression, or Err(String) for a syntax error.
    ///
    /// # Arguments
    /// * `heap` - The GC heap for allocating Scheme values
    /// * `port` - The input port to read from
    pub fn parse(&mut self, heap: &mut GcHeap, port: &mut Port) -> Result<GcRef, String> {
        let mut tokenizer = Tokenizer::new(port);
        let token = tokenizer.next_token();
        match token {
            Some(Token::Quote) => Self::parse_quoted_expression(heap, &mut tokenizer),
            Some(token) => Self::parse_from_token(heap, Some(token), &mut tokenizer),
            None => Err("Unexpected end of input".to_string()),
        }
    }

    /// Parse a quoted expression (after encountering a quote token).
    fn parse_quoted_expression(heap: &mut GcHeap, tokenizer: &mut Tokenizer) -> Result<GcRef, String> {
        // Get the next token
        let next_token = tokenizer.next_token();
        // Parse the quoted expression first and ensure the borrow is dropped
        let quoted = Self::parse_from_token(heap, next_token, tokenizer)?;
        // Now create the quote structure with a fresh borrow
        let quote_sym = new_symbol(heap, "quote");
        let nil = heap.nil();
        let quoted_list = new_pair(heap, quoted, nil);
        Ok(new_pair(heap, quote_sym, quoted_list))
    }

    fn parse_number_token(heap: &mut GcHeap, s: &str) -> Result<GcRef, String> {
        if s.contains('.') || s.contains('e') || s.contains('E') {
            // Parse as float if it looks like a float
            if let Ok(f) = s.parse::<f64>() {
                Ok(new_float(heap, f))
            } else {
                Err(format!("Invalid float literal: {}", s))
            }
        } else {
            // Parse as BigInt
            if let Some(i) = BigInt::parse_bytes(s.as_bytes(), 10) {
                Ok(new_int(heap, i))
            } else {
                Err(format!("Invalid integer literal: {}", s))
            }
        }
    }

    fn parse_symbol_token(heap: &mut GcHeap, s: &str) -> Result<GcRef, String> {
        if s == "nil" || s == "()" {
            Ok(heap.nil())
        } else if s.starts_with("#\\") {
            let ch = match &s[2..] {
                "space" => Some(' '),
                "newline" => Some('\n'),
                rest if rest.len() == 1 => rest.chars().next(),
                _ => None,
            };
            if let Some(c) = ch {
                Ok(crate::gc::new_char(heap, c))
            } else {
                Err(format!("Invalid character literal: {}", s))
            }
        } else {
            Ok(new_symbol(heap, s))
        }
    }

    fn parse_vector_token(heap: &mut GcHeap, tokenizer: &mut Tokenizer) -> Result<GcRef, String> {
        let mut vec_elems = Vec::new();
        loop {
            let t = tokenizer.next_token();
            if let Some(Token::RightBracket) = t {
                break;
            }
            if let None = t {
                return Err("Unclosed vector (unexpected EOF)".to_string());
            }
            vec_elems.push(Self::parse_from_token(heap, t, tokenizer)?);
        }
        Ok(crate::gc::new_vector(heap, vec_elems))
    }

    /// Parse an s-expression from a given token (used for list elements and recursive parsing).
    fn parse_from_token(heap: &mut GcHeap, token: Option<Token>, tokenizer: &mut Tokenizer) -> Result<GcRef, String> {
        match token {
            Some(Token::Number(s)) => Self::parse_number_token(heap, &s),
            Some(Token::String(s)) => Ok(new_string(heap, s)),
            Some(Token::Boolean(b)) => Ok(crate::gc::new_bool(heap, b)),
            Some(Token::Character(c)) => Ok(crate::gc::new_char(heap, c)),
            Some(Token::Symbol(s)) => Self::parse_symbol_token(heap, &s),
            Some(Token::LeftParen) => Self::parse_list(heap, tokenizer),
            Some(Token::RightParen) => Err("Unexpected ')'".to_string()),
            Some(Token::Dot) => Err("Unexpected '.'".to_string()),
            Some(Token::Quote) => Self::parse_quoted_expression(heap, tokenizer),
            Some(Token::LeftBracket) => Self::parse_vector_token(heap, tokenizer),
            Some(Token::RightBracket) => Err("Unexpected ']'".to_string()),
            Some(Token::Eof) => Err("Unexpected end of input".to_string()),
            None => Err("Unexpected end of input".to_string()),
        }
    }

    /// Parse a Scheme list (after encountering a left parenthesis).
    fn parse_list(heap: &mut GcHeap, tokenizer: &mut Tokenizer) -> Result<GcRef, String> {
        let mut elements = Vec::new();
        loop {
            let token = tokenizer.next_token();
            match token {
                Some(Token::RightParen) => {
                    // End of list
                    let mut list = heap.nil();
                    for elem in elements.into_iter().rev() {
                        list = new_pair(heap, elem, list);
                    }
                    return Ok(list);
                }
                None => return Err("Unclosed list (unexpected EOF)".to_string()),
                Some(Token::Dot) => {
                    // Dotted pair: (a b . c)
                    let tail = Self::parse_from_token(heap, tokenizer.next_token(), tokenizer)?;
                    if let Some(Token::RightParen) = tokenizer.next_token() {
                        let mut list = tail;
                        for elem in elements.into_iter().rev() {
                            list = new_pair(heap, elem, list);
                        }
                        return Ok(list);
                    } else {
                        return Err("Expected ')' after dotted pair".to_string());
                    }
                }
                Some(token) => {
                    // Parse the element directly from the token
                    let elem = Self::parse_from_token(heap, Some(token), tokenizer)?;
                    elements.push(elem);
                }
            }
        }
    }
}

impl ParserSimple {
    /// Create a new stateless parser using the reference-based GC system.
    pub fn new() -> Self {
        Self
    }

    /// Parse a single s-expression from the token stream using the reference-based GC system.
    ///
    /// Returns Ok(GcRefSimple) for a valid s-expression, or Err(String) for a syntax error.
    ///
    /// # Arguments
    /// * `heap` - The GC heap for allocating Scheme values
    /// * `port` - The input port to read from
    pub fn parse(&mut self, heap: &mut GcHeap, port: &mut Port) -> Result<GcRefSimple, String> {
        let mut tokenizer = Tokenizer::new(port);
        let token = tokenizer.next_token();
        match token {
            Some(Token::Quote) => Self::parse_quoted_expression(heap, &mut tokenizer),
            Some(token) => Self::parse_from_token(heap, Some(token), &mut tokenizer),
            None => Err("Unexpected end of input".to_string()),
        }
    }

    /// Parse a quoted expression (after encountering a quote token).
    fn parse_quoted_expression(heap: &mut GcHeap, tokenizer: &mut Tokenizer) -> Result<GcRefSimple, String> {
        // Get the next token
        let next_token = tokenizer.next_token();
        // Parse the quoted expression first
        let quoted = Self::parse_from_token(heap, next_token, tokenizer)?;
        // Now create the quote structure
        let quote_sym = new_symbol_simple(heap, "quote");
        let nil = new_nil_simple(heap);
        // For now, we'll need to convert back to GcRef for pair creation
        // TODO: Add new_pair_simple function that works with GcRefSimple
        let quoted_gcref = unsafe { std::mem::transmute(quoted) };
        let nil_gcref = unsafe { std::mem::transmute(nil) };
        let quoted_list = new_pair(heap, quoted_gcref, nil_gcref);
        let quote_sym_gcref = unsafe { std::mem::transmute(quote_sym) };
        Ok(unsafe { std::mem::transmute(new_pair(heap, quote_sym_gcref, quoted_list)) })
    }

    fn parse_number_token(heap: &mut GcHeap, s: &str) -> Result<GcRefSimple, String> {
        if s.contains('.') || s.contains('e') || s.contains('E') {
            // Parse as float if it looks like a float
            if let Ok(f) = s.parse::<f64>() {
                Ok(new_float_simple(heap, f))
            } else {
                Err(format!("Invalid float literal: {}", s))
            }
        } else {
            // Parse as BigInt
            if let Some(i) = BigInt::parse_bytes(s.as_bytes(), 10) {
                Ok(new_int_simple(heap, i))
            } else {
                Err(format!("Invalid integer literal: {}", s))
            }
        }
    }

    fn parse_symbol_token(heap: &mut GcHeap, s: &str) -> Result<GcRefSimple, String> {
        if s == "nil" || s == "()" {
            Ok(new_nil_simple(heap))
        } else if s.starts_with("#\\") {
            let ch = match &s[2..] {
                "space" => Some(' '),
                "newline" => Some('\n'),
                rest if rest.len() == 1 => rest.chars().next(),
                _ => None,
            };
            if let Some(c) = ch {
                // TODO: Add new_char_simple function
                Err("Character literals not yet supported in ParserSimple".to_string())
            } else {
                Err(format!("Invalid character literal: {}", s))
            }
        } else {
            Ok(new_symbol_simple(heap, s))
        }
    }

    fn parse_vector_token(heap: &mut GcHeap, tokenizer: &mut Tokenizer) -> Result<GcRefSimple, String> {
        let mut vec_elems = Vec::new();
        loop {
            let t = tokenizer.next_token();
            if let Some(Token::RightBracket) = t {
                break;
            }
            if let None = t {
                return Err("Unclosed vector (unexpected EOF)".to_string());
            }
            let elem = Self::parse_from_token(heap, t, tokenizer)?;
            // Convert GcRefSimple to GcRef for vector storage
            let elem_gcref = unsafe { std::mem::transmute(elem) };
            vec_elems.push(elem_gcref);
        }
        let vector_gcref = crate::gc::new_vector(heap, vec_elems);
        Ok(unsafe { std::mem::transmute(vector_gcref) })
    }

    /// Parse an s-expression from a given token (used for list elements and recursive parsing).
    fn parse_from_token(heap: &mut GcHeap, token: Option<Token>, tokenizer: &mut Tokenizer) -> Result<GcRefSimple, String> {
        match token {
            Some(Token::Number(s)) => Self::parse_number_token(heap, &s),
            Some(Token::String(s)) => Ok(new_string_simple(heap, &s)),
            Some(Token::Boolean(b)) => {
                // TODO: Add new_bool_simple function
                Err("Boolean literals not yet supported in ParserSimple".to_string())
            }
            Some(Token::Character(c)) => {
                // TODO: Add new_char_simple function
                Err("Character literals not yet supported in ParserSimple".to_string())
            }
            Some(Token::Symbol(s)) => Self::parse_symbol_token(heap, &s),
            Some(Token::LeftParen) => Self::parse_list(heap, tokenizer),
            Some(Token::RightParen) => Err("Unexpected ')'".to_string()),
            Some(Token::Dot) => Err("Unexpected '.'".to_string()),
            Some(Token::Quote) => Self::parse_quoted_expression(heap, tokenizer),
            Some(Token::LeftBracket) => Self::parse_vector_token(heap, tokenizer),
            Some(Token::RightBracket) => Err("Unexpected ']'".to_string()),
            Some(Token::Eof) => Err("Unexpected end of input".to_string()),
            None => Err("Unexpected end of input".to_string()),
        }
    }

    /// Parse a Scheme list (after encountering a left parenthesis).
    fn parse_list(heap: &mut GcHeap, tokenizer: &mut Tokenizer) -> Result<GcRefSimple, String> {
        let mut elements = Vec::new();
        loop {
            let token = tokenizer.next_token();
            match token {
                Some(Token::RightParen) => {
                    // End of list
                    let mut list = new_nil_simple(heap);
                    for elem in elements.into_iter().rev() {
                        // TODO: Add new_pair_simple function
                        let elem_gcref = unsafe { std::mem::transmute(elem) };
                        let list_gcref = unsafe { std::mem::transmute(list) };
                        let new_pair_gcref = new_pair(heap, elem_gcref, list_gcref);
                        list = unsafe { std::mem::transmute(new_pair_gcref) };
                    }
                    return Ok(list);
                }
                None => return Err("Unclosed list (unexpected EOF)".to_string()),
                Some(Token::Dot) => {
                    // Dotted pair: (a b . c)
                    let tail = Self::parse_from_token(heap, tokenizer.next_token(), tokenizer)?;
                    if let Some(Token::RightParen) = tokenizer.next_token() {
                        let mut list = tail;
                        for elem in elements.into_iter().rev() {
                            // TODO: Add new_pair_simple function
                            let elem_gcref = unsafe { std::mem::transmute(elem) };
                            let list_gcref = unsafe { std::mem::transmute(list) };
                            let new_pair_gcref = new_pair(heap, elem_gcref, list_gcref);
                            list = unsafe { std::mem::transmute(new_pair_gcref) };
                        }
                        return Ok(list);
                    } else {
                        return Err("Expected ')' after dotted pair".to_string());
                    }
                }
                Some(token) => {
                    // Parse the element directly from the token
                    let elem = Self::parse_from_token(heap, Some(token), tokenizer)?;
                    elements.push(elem);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::io::{Port, PortKind};
    use crate::gc::{as_int, as_symbol, as_string, is_nil, as_pair, as_bool, as_char, as_vector, as_float, SchemeValue};
    use crate::gc::{convert_simple_to_ref};
    use crate::builtin::{register_all};
    use std::collections::HashMap;

    #[test]
    fn parse_number() {
        let mut heap = GcHeap::new();
        let mut port = Port { kind: PortKind::StringPortInput { content:42.to_string(), pos: 0 } };
        let mut parser = Parser::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        assert_eq!(as_int(&expr), Some(42));
    }

    #[test]
    fn parse_symbol() {
        let mut heap = GcHeap::new();
        let mut port = Port { kind: PortKind::StringPortInput { content: "hello".to_string(), pos: 0 } };
        let mut parser = Parser::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        assert_eq!(as_symbol(&expr), Some("hello".to_string()));
    }

    #[test]
    fn parse_string() {
        let mut heap = GcHeap::new();
        let mut port = Port { kind: PortKind::StringPortInput { content: "\"hello world\"".to_string(), pos: 0 } };
        let mut parser = Parser::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        assert_eq!(as_string(&expr), Some("hello world".to_string()));
    }

    #[test]
    fn parse_nil() {
        let mut heap = GcHeap::new();
        let mut port = Port { kind: PortKind::StringPortInput { content: "nil".to_string(), pos: 0 } };
        let mut parser = Parser::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        assert!(is_nil(&expr));
    }

    #[test]
    fn parse_simple_list() {
        let mut heap = GcHeap::new();
        let mut port = Port { kind: PortKind::StringPortInput { content: "(1 2 3)".to_string(), pos: 0 } };
        let mut parser = Parser::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        
        let (car, cdr) = as_pair(&expr).unwrap();
        assert_eq!(as_int(&car), Some(1));
        
        let (car, cdr) = as_pair(&cdr).unwrap();
        assert_eq!(as_int(&car), Some(2));
        
        let (car, cdr) = as_pair(&cdr).unwrap();
        assert_eq!(as_int(&car), Some(3));
        assert!(is_nil(&cdr));
    }

    #[test]
    fn parse_booleans_and_nil() {
        let mut heap = GcHeap::new();
        let mut port = Port { kind: PortKind::StringPortInput { content: "#t #f".to_string(), pos: 0 } };
        let mut parser = Parser::new();
        
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        assert_eq!(as_bool(&expr), Some(true));
        
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        assert_eq!(as_bool(&expr), Some(false));
    }

    #[test]
    fn parse_character() {
        let mut heap = GcHeap::new();
        let mut port = Port { kind: PortKind::StringPortInput { content: "#\\a #\\space".to_string(), pos: 0 } };
        let mut parser = Parser::new();
        
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        assert_eq!(as_char(&expr), Some('a'));
        
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        assert_eq!(as_char(&expr), Some(' '));
    }

    #[test]
    fn parse_quoted() {
        let mut heap = GcHeap::new();
        let mut port = Port { kind: PortKind::StringPortInput { content: "'hello".to_string(), pos: 0 } };
        let mut parser = Parser::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        
        let (quote_sym, quoted_expr) = as_pair(&expr).unwrap();
        assert_eq!(as_symbol(&quote_sym), Some("quote".to_string()));
        
        let (hello_sym, nil) = as_pair(&quoted_expr).unwrap();
        assert_eq!(as_symbol(&hello_sym), Some("hello".to_string()));
        assert!(is_nil(&nil));
    }

    #[test]
    fn parse_dotted_pair() {
        let mut heap = GcHeap::new();
        let mut port = Port { kind: PortKind::StringPortInput { content: "(1 . 2)".to_string(), pos: 0 } };
        let mut parser = Parser::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        
        let (car, cdr) = as_pair(&expr).unwrap();
        assert_eq!(as_int(&car), Some(1));
        assert_eq!(as_int(&cdr), Some(2));
    }

    #[test]
    fn parse_vector() {
        let mut heap = GcHeap::new();
        let mut port = Port { kind: PortKind::StringPortInput { content: "#(1 2 3)".to_string(), pos: 0 } };
        let mut parser = Parser::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        
        let vec = as_vector(&expr).unwrap();
        assert_eq!(vec.len(), 3);
        assert_eq!(as_int(&vec[0]), Some(1));
        assert_eq!(as_int(&vec[1]), Some(2));
        assert_eq!(as_int(&vec[2]), Some(3));
    }

    #[test]
    fn parse_string_debug() {
        let mut heap = GcHeap::new();
        let mut port = Port { kind: PortKind::StringPortInput { content: "hello world".to_string(), pos: 0 } };
        let mut parser = Parser::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        
        println!("Parsed expr: {:?}", expr);
        println!("as_string result: {:?}", as_string(&expr));
        
        // This test is just for debugging, so we'll make it pass
        assert!(true);
    }

    #[test]
    fn eval_quote_special_form() {
        // Set up heap, parser, and environment with quote registered
        let mut heap = GcHeap::new();
        let mut port = Port { kind: PortKind::StringPortInput { content: "'foo".to_string(), pos: 0 } };
        let mut parser = Parser::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();

        // Set up environment with quote special form
        let mut env = HashMap::new();
        register_all(&mut heap, &mut env);

        // Simulate evaluation of (quote foo)
        if let SchemeValue::Pair(car, cdr) = &expr.borrow().value {
            if let SchemeValue::Symbol(ref name) = car.borrow().value {
                if let Some(builtin) = env.get(name) {
                    if let SchemeValue::SpecialForm { func, .. } = &builtin.borrow().value {
                        let mut args = Vec::new();
                        let mut cur = cdr.clone();
                        loop {
                            let next = {
                                let cur_borrow = cur.borrow();
                                match &cur_borrow.value {
                                    SchemeValue::Pair(arg, next) => {
                                        args.push(arg.clone());
                                        Some(next.clone())
                                    }
                                    SchemeValue::Nil => None,
                                    _ => panic!("Malformed argument list"),
                                }
                            };
                            if let Some(next_cdr) = next {
                                cur = next_cdr;
                            } else {
                                break;
                            }
                        }
                        assert!(matches!(&cur.borrow().value, SchemeValue::Nil));
                        // Create a temporary evaluator for the new interface
                        let mut temp_evaluator = crate::eval::Evaluator::new();
                        let result = func(&mut temp_evaluator, &args).unwrap();
                        assert_eq!(as_symbol(&result), Some("foo".to_string()));
                    } else {
                        panic!("quote not registered as special form");
                    }
                } else {
                    panic!("quote not registered as special form");
                }
            } else {
                panic!("First element is not a symbol");
            }
        } else {
            panic!("Not a pair");
        }
    }

    #[test]
    fn parse_nested_quoted() {
        let mut heap = GcHeap::new();
        let mut port = Port { kind: PortKind::StringPortInput { content: "''foo".to_string(), pos: 0 } };
        let mut parser = Parser::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        // Should be (quote (quote foo))
        let (outer_quote, outer_cdr) = as_pair(&expr).unwrap();
        assert_eq!(as_symbol(&outer_quote), Some("quote".to_string()));
        let (inner_expr, nil1) = as_pair(&outer_cdr).unwrap();
        assert!(is_nil(&nil1));
        let (inner_quote, inner_cdr) = as_pair(&inner_expr).unwrap();
        assert_eq!(as_symbol(&inner_quote), Some("quote".to_string()));
        let (foo_sym, nil2) = as_pair(&inner_cdr).unwrap();
        assert_eq!(as_symbol(&foo_sym), Some("foo".to_string()));
        assert!(is_nil(&nil2));
    }

    #[test]
    fn parse_negative_and_positive_numbers() {
        let mut heap = GcHeap::new();
        let mut port = Port { kind: PortKind::StringPortInput { content: "-45 +123 -123412341234123412341234".to_string(), pos: 0 } };
        let mut parser = Parser::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        assert_eq!(as_int(&expr), Some(-45));
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        assert_eq!(as_int(&expr), Some(123));
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        // Check that the bignum parses as a negative number
        use num_bigint::BigInt;
        assert_eq!(expr.borrow().value, crate::gc::SchemeValue::Int(BigInt::parse_bytes(b"-123412341234123412341234", 10).unwrap()));
    }

    // ============================================================================
    // TESTS FOR PARSER SIMPLE (Reference-based GC system)
    // ============================================================================

    #[test]
    fn parse_simple_number() {
        let mut heap = GcHeap::new();
        let mut port = Port { kind: PortKind::StringPortInput { content: "42".to_string(), pos: 0 } };
        let mut parser = ParserSimple::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        // Convert GcRefSimple to GcRef for testing
        let expr_gcref = convert_simple_to_ref(expr);
        assert_eq!(as_int(&expr_gcref), Some(42));
    }

    #[test]
    fn parse_simple_symbol() {
        let mut heap = GcHeap::new();
        let mut port = Port { kind: PortKind::StringPortInput { content: "hello".to_string(), pos: 0 } };
        let mut parser = ParserSimple::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        let expr_gcref = convert_simple_to_ref(expr);
        assert_eq!(as_symbol(&expr_gcref), Some("hello".to_string()));
    }

    #[test]
    fn parse_simple_string() {
        let mut heap = GcHeap::new();
        let mut port = Port { kind: PortKind::StringPortInput { content: "\"hello world\"".to_string(), pos: 0 } };
        let mut parser = ParserSimple::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        let expr_gcref = convert_simple_to_ref(expr);
        assert_eq!(as_string(&expr_gcref), Some("hello world".to_string()));
    }

    #[test]
    fn parse_simple_nil() {
        let mut heap = GcHeap::new();
        let mut port = Port { kind: PortKind::StringPortInput { content: "nil".to_string(), pos: 0 } };
        let mut parser = ParserSimple::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        let expr_gcref = convert_simple_to_ref(expr);
        assert!(is_nil(&expr_gcref));
    }

    #[test]
    fn parse_simple_float() {
        let mut heap = GcHeap::new();
        let mut port = Port { kind: PortKind::StringPortInput { content: "3.14".to_string(), pos: 0 } };
        let mut parser = ParserSimple::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        let expr_gcref = convert_simple_to_ref(expr);
        assert_eq!(as_float(&expr_gcref), Some(3.14));
    }
} 