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
//! let mut port = new_string_port("42") };
//! let mut parser = Parser::new();
//! let expr = parser.parse(&mut heap, &mut port).unwrap();
//! ```

use crate::gc::{
    GcHeap, GcRef, new_int, new_float, new_symbol, new_string, 
    new_bool, new_char, new_nil, new_pair, new_vector
};
use crate::tokenizer::{Tokenizer, Token};
use num_bigint::BigInt;
use crate::io::{Port};

#[derive(Debug, PartialEq)]
pub enum ParseError {
    Eof,
    Syntax(String),
}

/// Parser for Scheme s-expressions using the reference-based GC system.
///
/// This parser uses GcRefSimple (&'static GcObject) for better performance and simpler code.
/// The parser is stateless and consumes tokens from a Tokenizer, producing interned s-expressions
/// (SchemeValue) using the GC heap. All state is passed as arguments.
pub struct Parser;

impl Parser {
    /// Create a new stateless parser.
    pub fn new() -> Self {
        Self
    }

    /// Parse a single s-expression from the token stream.
    ///
    /// Returns Ok(GcRefSimple) for a valid s-expression, or Err(String) for a syntax error.
    ///
    /// # Arguments
    /// * `heap` - The GC heap for allocating Scheme values
    /// * `port` - The input port to read from
    pub fn parse(&mut self, heap: &mut GcHeap, port: &mut Port) -> Result<GcRef, ParseError> {
        let mut tokenizer = Tokenizer::new(port);
        let token = tokenizer.next_token();
        match token {
            Some(Token::Quote) => Self::parse_quoted_expression(heap, &mut tokenizer),
            Some(token) => Self::parse_from_token(heap, Some(token), &mut tokenizer),
            None => Err(ParseError::Eof),
        }
    }

    /// Parse a quoted expression (after encountering a quote token).
    fn parse_quoted_expression(heap: &mut GcHeap, tokenizer: &mut Tokenizer) -> Result<GcRef, ParseError> {
        // Get the next token
        let next_token = tokenizer.next_token();
        // Parse the quoted expression first and ensure the borrow is dropped
        let quoted = Self::parse_from_token(heap, next_token, tokenizer)?;
        // Now create the quote structure with a fresh borrow
        let quote_sym = new_symbol(heap, "quote");
        let nil = new_nil(heap);
        let quoted_list = new_pair(heap, quoted, nil);
        Ok(new_pair(heap, quote_sym, quoted_list))
    }

    fn parse_number_token(heap: &mut GcHeap, s: &str) -> Result<GcRef, ParseError> {
        if s.contains('.') || s.contains('e') || s.contains('E') {
            // Parse as float if it looks like a float
            if let Ok(f) = s.parse::<f64>() {
                Ok(new_float(heap, f))
            } else {
                Err(ParseError::Syntax(format!("Invalid float literal: {}", s)))
            }
        } else {
            // Parse as BigInt
            if let Some(i) = BigInt::parse_bytes(s.as_bytes(), 10) {
                Ok(new_int(heap, i))
            } else {
                Err(ParseError::Syntax(format!("Invalid integer literal: {}", s)))
            }
        }
    }

    fn parse_symbol_token(heap: &mut GcHeap, s: &str) -> Result<GcRef, ParseError> {
        if s == "nil" || s == "()" {
            Ok(new_nil(heap))
        } else if s.starts_with("#\\") {
            let ch = match &s[2..] {
                "space" => Some(' '),
                "newline" => Some('\n'),
                rest if rest.len() == 1 => rest.chars().next(),
                _ => None,
            };
            if let Some(c) = ch {
                Ok(new_char(heap, c))
            } else {
                Err(ParseError::Syntax(format!("Invalid character literal: {}", s)))
            }
        } else {
            Ok(new_symbol(heap, s))
        }
    }

    fn parse_vector_token(heap: &mut GcHeap, tokenizer: &mut Tokenizer) -> Result<GcRef, ParseError> {
        let mut vec_elems = Vec::new();
        loop {
            let t = tokenizer.next_token();
            if let Some(Token::RightBracket) = t {
                break;
            }
            if let None = t {
                return Err(ParseError::Syntax("Unclosed vector (unexpected EOF)".to_string()));
            }
            vec_elems.push(Self::parse_from_token(heap, t, tokenizer)?);
        }
        Ok(new_vector(heap, vec_elems))
    }

    /// Parse an s-expression from a given token (used for list elements and recursive parsing).
    fn parse_from_token(heap: &mut GcHeap, token: Option<Token>, tokenizer: &mut Tokenizer) -> Result<GcRef, ParseError> {
        match token {
            Some(Token::Number(s)) => Self::parse_number_token(heap, &s),
            Some(Token::String(s)) => Ok(new_string(heap, &s)),
            Some(Token::Boolean(b)) => Ok(new_bool(heap, b)),
            Some(Token::Character(c)) => Ok(new_char(heap, c)),
            Some(Token::Symbol(s)) => Self::parse_symbol_token(heap, &s),
            Some(Token::LeftParen) => Self::parse_list(heap, tokenizer),
            Some(Token::RightParen) => Err(ParseError::Syntax("Unexpected ')'".to_string())),
            Some(Token::Dot) => Err(ParseError::Syntax("Unexpected '.'".to_string())),
            Some(Token::Quote) => Self::parse_quoted_expression(heap, tokenizer),
            Some(Token::LeftBracket) => Self::parse_vector_token(heap, tokenizer),
            Some(Token::RightBracket) => Err(ParseError::Syntax("Unexpected ']'".to_string())),
            Some(Token::Eof) => Err(ParseError::Eof),
            None => Err(ParseError::Eof),
        }
    }

    /// Parse a Scheme list (after encountering a left parenthesis).
    fn parse_list(heap: &mut GcHeap, tokenizer: &mut Tokenizer) -> Result<GcRef, ParseError> {
        let mut elements = Vec::new();
        loop {
            let token = tokenizer.next_token();
            match token {
                Some(Token::RightParen) => {
                    // End of list
                    let mut list = new_nil(heap);
                    for elem in elements.into_iter().rev() {
                        list = new_pair(heap, elem, list);
                    }
                    return Ok(list);
                }
                None => return Err(ParseError::Syntax("Unclosed list (unexpected EOF)".to_string())),
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
                        return Err(ParseError::Syntax("Expected ')' after dotted pair".to_string()));
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

mod tests {
    use super::*;
    use crate::io::{Port, PortKind};
    use crate::gc::SchemeValue;

    #[test]
    fn parse_number() {
        let mut heap = GcHeap::new();
        let mut port = crate::io::new_string_port_input("42");
        let mut parser = Parser::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        match &expr.value {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "42"),
            _ => panic!("Expected integer, got {:?}", expr.value),
        }
    }

    #[test]
    fn parse_symbol() {
        let mut heap = GcHeap::new();
        let mut port = crate::io::new_string_port_input("hello");
        let mut parser = Parser::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        match &expr.value {
            SchemeValue::Symbol(s) => assert_eq!(s, "hello"),
            _ => panic!("Expected symbol, got {:?}", expr.value),
        }
    }

    #[test]
    fn parse_string() {
        let mut heap = GcHeap::new();
        let mut port = crate::io::new_string_port_input("\"hello world\"");
        let mut parser = Parser::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        match &expr.value {
            SchemeValue::Str(s) => assert_eq!(s, "hello world"),
            _ => panic!("Expected string, got {:?}", expr.value),
        }
    }

    #[test]
    fn parse_nil() {
        let mut heap = GcHeap::new();
        let mut port = crate::io::new_string_port_input("nil");
        let mut parser = Parser::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        match &expr.value {
            SchemeValue::Nil => assert!(true), // Success
            _ => panic!("Expected nil, got {:?}", expr.value),
        }
    }

    #[test]
    fn parse_list() {
        let mut heap = GcHeap::new();
        let mut port = crate::io::new_string_port_input("(1 2 3)");
        let mut parser = Parser::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        
        match &expr.value {
            SchemeValue::Pair(car, cdr) => {
                match &car.value {
                    SchemeValue::Int(i) => assert_eq!(i.to_string(), "1"),
                    _ => panic!("Expected integer 1, got {:?}", car.value),
                }
                match &cdr.value {
                    SchemeValue::Pair(car2, cdr2) => {
                        match &car2.value {
                            SchemeValue::Int(i) => assert_eq!(i.to_string(), "2"),
                            _ => panic!("Expected integer 2, got {:?}", car2.value),
                        }
                        match &cdr2.value {
                            SchemeValue::Pair(car3, cdr3) => {
                                match &car3.value {
                                    SchemeValue::Int(i) => assert_eq!(i.to_string(), "3"),
                                    _ => panic!("Expected integer 3, got {:?}", car3.value),
                                }
                                match &cdr3.value {
                                    SchemeValue::Nil => assert!(true), // Success
                                    _ => panic!("Expected nil, got {:?}", cdr3.value),
                                }
                            }
                            _ => panic!("Expected pair, got {:?}", cdr2.value),
                        }
                    }
                    _ => panic!("Expected pair, got {:?}", cdr.value),
                }
            }
            _ => panic!("Expected pair, got {:?}", expr.value),
        }
    }

    #[test]
    fn parse_booleans() {
        let mut heap = GcHeap::new();
        let mut port = crate::io::new_string_port_input("#t #f");
        let mut parser = Parser::new();
        
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        match &expr.value {
            SchemeValue::Bool(b) => assert_eq!(*b, true),
            _ => panic!("Expected true, got {:?}", expr.value),
        }
        
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        match &expr.value {
            SchemeValue::Bool(b) => assert_eq!(*b, false),
            _ => panic!("Expected false, got {:?}", expr.value),
        }
    }

    #[test]
    fn parse_character() {
        let mut heap = GcHeap::new();
        let mut port = crate::io::new_string_port_input("#\\a #\\space");
        let mut parser = Parser::new();
        
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        match &expr.value {
            SchemeValue::Char(c) => assert_eq!(*c, 'a'),
            _ => panic!("Expected character 'a', got {:?}", expr.value),
        }
        
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        match &expr.value {
            SchemeValue::Char(c) => assert_eq!(*c, ' '),
            _ => panic!("Expected character ' ', got {:?}", expr.value),
        }
    }

    #[test]
    fn parse_quoted() {
        let mut heap = GcHeap::new();
        let mut port = crate::io::new_string_port_input("'hello");
        let mut parser = Parser::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        
        match &expr.value {
            SchemeValue::Pair(quote_sym, quoted_expr) => {
                match &quote_sym.value {
                    SchemeValue::Symbol(s) => assert_eq!(s, "quote"),
                    _ => panic!("Expected symbol 'quote', got {:?}", quote_sym.value),
                }
                match &quoted_expr.value {
                    SchemeValue::Pair(hello_sym, nil) => {
                        match &hello_sym.value {
                            SchemeValue::Symbol(s) => assert_eq!(s, "hello"),
                            _ => panic!("Expected symbol 'hello', got {:?}", hello_sym.value),
                        }
                        match &nil.value {
                            SchemeValue::Nil => assert!(true), // Success
                            _ => panic!("Expected nil, got {:?}", nil.value),
                        }
                    }
                    _ => panic!("Expected pair, got {:?}", quoted_expr.value),
                }
            }
            _ => panic!("Expected pair, got {:?}", expr.value),
        }
    }

    #[test]
    fn parse_dotted_pair() {
        let mut heap = GcHeap::new();
        let mut port = crate::io::new_string_port_input("(1 . 2)");
        let mut parser = Parser::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        
        match &expr.value {
            SchemeValue::Pair(car, cdr) => {
                match &car.value {
                    SchemeValue::Int(i) => assert_eq!(i.to_string(), "1"),
                    _ => panic!("Expected integer 1, got {:?}", car.value),
                }
                match &cdr.value {
                    SchemeValue::Int(i) => assert_eq!(i.to_string(), "2"),
                    _ => panic!("Expected integer 2, got {:?}", cdr.value),
                }
            }
            _ => panic!("Expected pair, got {:?}", expr.value),
        }
    }

    #[test]
    fn parse_vector() {
        let mut heap = GcHeap::new();
        let mut port = crate::io::new_string_port_input("#(1 2 3)");
        let mut parser = Parser::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        
        match &expr.value {
            SchemeValue::Vector(v) => {
                assert_eq!(v.len(), 3);
                match &v[0].value {
                    SchemeValue::Int(i) => assert_eq!(i.to_string(), "1"),
                    _ => panic!("Expected integer 1, got {:?}", v[0].value),
                }
                match &v[1].value {
                    SchemeValue::Int(i) => assert_eq!(i.to_string(), "2"),
                    _ => panic!("Expected integer 2, got {:?}", v[1].value),
                }
                match &v[2].value {
                    SchemeValue::Int(i) => assert_eq!(i.to_string(), "3"),
                    _ => panic!("Expected integer 3, got {:?}", v[2].value),
                }
            }
            _ => panic!("Expected vector, got {:?}", expr.value),
        }
    }

    #[test]
    fn parse_float() {
        let mut heap = GcHeap::new();
        let mut port = crate::io::new_string_port_input("3.14");
        let mut parser = Parser::new();
        let expr = parser.parse(&mut heap, &mut port).unwrap();
        match &expr.value {
            SchemeValue::Float(f) => assert_eq!(*f, 3.14),
            _ => panic!("Expected float, got {:?}", expr.value),
        }
    }
} 