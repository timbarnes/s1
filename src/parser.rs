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
    GcHeap, GcRef, get_nil, get_symbol, new_bool, new_char, new_float, new_int, new_pair,
    new_string, new_vector,
};
use crate::io::PortKind;
use crate::tokenizer::{Token, Tokenizer};
use num_bigint::BigInt;

#[derive(Debug, PartialEq)]
pub enum ParseError {
    Eof,
    Syntax(String),
    Other(String),
}

/// Parser for Scheme s-expressions using the reference-based GC system.
///
/// This parser uses GcRefSimple (&'static GcObject) for better performance and simpler code.
/// The parser is stateless and consumes tokens from a Tokenizer, producing interned s-expressions
/// (SchemeValue) using the GC heap. All state is passed as arguments.

/// Parse a single s-expression from the token stream.
///
/// Returns Ok(GcRefSimple) for a valid s-expression, or Err(String) for a syntax error.
///
/// # Arguments
/// * `heap` - The GC heap for allocating Scheme values
/// * `port` - The input port to read from
pub fn parse(heap: &mut GcHeap, port_ref: &mut PortKind) -> Result<GcRef, ParseError> {
    let mut tokenizer = Tokenizer::new(port_ref);
    let token = tokenizer.next_token();
    match token {
        Some(Token::Quote) => parse_quoted_expression(heap, &mut tokenizer, "quote".to_string()),
        Some(token) => parse_from_token(heap, Some(token), &mut tokenizer),
        None => Err(ParseError::Eof),
    }
}

/// Parse a quoted expression (after encountering a quote token).
fn parse_quoted_expression(
    heap: &mut GcHeap,
    tokenizer: &mut Tokenizer,
    sym: String, //
) -> Result<GcRef, ParseError> {
    let next_token = tokenizer.next_token();
    let quoted = parse_from_token(heap, next_token, tokenizer)?;
    let quote_sym = get_symbol(heap, sym.as_str());
    let nil = get_nil(heap);
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
            Err(ParseError::Syntax(format!(
                "Invalid integer literal: {}",
                s
            )))
        }
    }
}

fn parse_symbol_token(heap: &mut GcHeap, s: &str) -> Result<GcRef, ParseError> {
    if s == "nil" || s == "()" {
        Ok(get_nil(heap))
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
            Err(ParseError::Syntax(format!(
                "Invalid character literal: {}",
                s
            )))
        }
    } else {
        Ok(get_symbol(heap, s))
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
            return Err(ParseError::Syntax(
                "Unclosed vector (unexpected EOF)".to_string(),
            ));
        }
        vec_elems.push(parse_from_token(heap, t, tokenizer)?);
    }
    Ok(new_vector(heap, vec_elems))
}

/// Parse an s-expression from a given token (used for list elements and recursive parsing).
fn parse_from_token(
    heap: &mut GcHeap,
    token: Option<Token>,
    tokenizer: &mut Tokenizer,
) -> Result<GcRef, ParseError> {
    match token {
        Some(Token::Number(s)) => parse_number_token(heap, &s),
        Some(Token::String(s)) => Ok(new_string(heap, &s)),
        Some(Token::Boolean(b)) => Ok(new_bool(heap, b)),
        Some(Token::Character(c)) => Ok(new_char(heap, c)),
        Some(Token::Symbol(s)) => parse_symbol_token(heap, &s),
        Some(Token::LeftParen) => parse_list(heap, tokenizer),
        Some(Token::RightParen) => Err(ParseError::Syntax("Unexpected ')'".to_string())),
        Some(Token::Dot) => Err(ParseError::Syntax("Unexpected '.'".to_string())),
        Some(Token::Quote) => parse_quoted_expression(heap, tokenizer, "quote".to_string()),
        Some(Token::LeftBracket) => parse_vector_token(heap, tokenizer),
        Some(Token::RightBracket) => Err(ParseError::Syntax("Unexpected ']'".to_string())),
        Some(Token::QuasiQuote) => {
            parse_quoted_expression(heap, tokenizer, "quasiquote".to_string())
        }
        Some(Token::Unquote) => parse_quoted_expression(heap, tokenizer, "unquote".to_string()),
        Some(Token::UnquoteSplicing) => {
            parse_quoted_expression(heap, tokenizer, "unquote-splicing".to_string())
        }
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
                let mut list = get_nil(heap);
                for elem in elements.into_iter().rev() {
                    list = new_pair(heap, elem, list);
                }
                return Ok(list);
            }
            None => {
                return Err(ParseError::Syntax(
                    "Unclosed list (unexpected EOF)".to_string(),
                ));
            }
            Some(Token::Dot) => {
                // Dotted pair: (a b . c)
                let tail = parse_from_token(heap, tokenizer.next_token(), tokenizer)?;
                if let Some(Token::RightParen) = tokenizer.next_token() {
                    let mut list = tail;
                    for elem in elements.into_iter().rev() {
                        list = new_pair(heap, elem, list);
                    }
                    return Ok(list);
                } else {
                    return Err(ParseError::Syntax(
                        "Expected ')' after dotted pair".to_string(),
                    ));
                }
            }
            Some(token) => {
                // Parse the element directly from the token
                let elem = parse_from_token(heap, Some(token), tokenizer)?;
                elements.push(elem);
            }
        }
    }
}

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn parse_number() {
        use crate::gc::SchemeValue;
        let mut ev = crate::eval::Evaluator::new();
        let ec = crate::eval::EvalContext::from_eval(&mut ev);
        let mut port = crate::io::new_string_port_input("42");
        let expr = parse(ec.heap, &mut port).unwrap();
        match &ec.heap.get_value(expr) {
            SchemeValue::Int(i) => assert_eq!(i.to_string(), "42"),
            _ => panic!(
                "Expected integer, got {}",
                crate::printer::print_value(&expr)
            ),
        }
    }

    #[test]
    fn parse_symbol() {
        use crate::printer::print_value;
        let mut ev = crate::eval::Evaluator::new();
        let ec = crate::eval::EvalContext::from_eval(&mut ev);
        let mut port = crate::io::new_string_port_input("hello");
        let expr = parse(ec.heap, &mut port).unwrap();
        match &ec.heap.get_value(expr) {
            crate::gc::SchemeValue::Symbol(s) => assert_eq!(s, "hello"),
            _ => panic!("Expected symbol, got {}", print_value(&expr)),
        }
    }

    #[test]
    fn parse_string() {
        use crate::printer::print_value;
        let mut ev = crate::eval::Evaluator::new();
        let ec = crate::eval::EvalContext::from_eval(&mut ev);
        let _port = crate::io::new_string_port_input("hello");
        let mut port = crate::io::new_string_port_input("\"hello world\"");
        let expr = parse(ec.heap, &mut port).unwrap();
        match &ec.heap.get_value(expr) {
            crate::gc::SchemeValue::Str(s) => assert_eq!(s, "hello world"),
            _ => panic!("Expected string, got {}", print_value(&expr)),
        }
    }

    #[test]
    fn parse_nil() {
        use crate::printer::print_value;
        let mut ev = crate::eval::Evaluator::new();
        let ec = crate::eval::EvalContext::from_eval(&mut ev);
        let _port = crate::io::new_string_port_input("hello");
        let mut port = crate::io::new_string_port_input("nil");
        let expr = parse(ec.heap, &mut port).unwrap();
        match &ec.heap.get_value(expr) {
            crate::gc::SchemeValue::Nil => assert!(true), // Success
            _ => panic!("Expected nil, got {}", print_value(&expr)),
        }
    }

    #[test]
    fn parse_list() {
        use crate::gc::SchemeValue;
        use crate::printer::print_value;
        let mut ev = crate::eval::Evaluator::new();
        let ec = crate::eval::EvalContext::from_eval(&mut ev);
        //let mut str_port = crate::io::new_string_port_input("hello");
        let mut str_port = crate::io::new_string_port_input("(1 2 3)");
        let expr = parse(ec.heap, &mut str_port).unwrap();

        match &ec.heap.get_value(expr) {
            SchemeValue::Pair(car, cdr) => {
                match &ec.heap.get_value(*car) {
                    SchemeValue::Int(i) => assert_eq!(i.to_string(), "1"),
                    _ => panic!("Expected integer 1, got {}", print_value(car)),
                }
                match &ec.heap.get_value(*cdr) {
                    SchemeValue::Pair(car2, cdr2) => {
                        match &ec.heap.get_value(*car2) {
                            crate::gc::SchemeValue::Int(i) => assert_eq!(i.to_string(), "2"),
                            _ => {
                                panic!("Expected integer 2, got {}", print_value(car2))
                            }
                        }
                        match &ec.heap.get_value(*cdr2) {
                            crate::gc::SchemeValue::Pair(car3, cdr3) => {
                                match &ec.heap.get_value(*car3) {
                                    crate::gc::SchemeValue::Int(i) => {
                                        assert_eq!(i.to_string(), "3")
                                    }
                                    _ => {
                                        panic!("Expected integer 3, got {}", print_value(car3))
                                    }
                                }
                                match &ec.heap.get_value(*cdr3) {
                                    SchemeValue::Nil => assert!(true), // Success
                                    _ => panic!("Expected nil, got {}", print_value(cdr3)),
                                }
                            }
                            _ => panic!("Expected pair, got {}", print_value(cdr2)),
                        }
                    }
                    _ => panic!("Expected pair, got {}", print_value(cdr)),
                }
            }
            _ => panic!("Expected pair, got {}", print_value(&expr)),
        }
    }

    #[test]
    fn parse_booleans() {
        use crate::printer::print_value;
        let mut ev = crate::eval::Evaluator::new();
        let ec = crate::eval::EvalContext::from_eval(&mut ev);
        let mut port = crate::io::new_string_port_input("#t #f");

        let expr = parse(ec.heap, &mut port).unwrap();
        match &ec.heap.get_value(expr) {
            crate::gc::SchemeValue::Bool(b) => assert_eq!(*b, true),
            _ => panic!("Expected true, got {}", print_value(&expr)),
        }

        let expr = parse(ec.heap, &mut port).unwrap();
        match &ec.heap.get_value(expr) {
            crate::gc::SchemeValue::Bool(b) => assert_eq!(*b, false),
            _ => panic!("Expected false, got {}", print_value(&expr)),
        }
    }

    #[test]
    fn parse_character() {
        use crate::gc::SchemeValue;
        use crate::printer::print_value;
        let mut ev = crate::eval::Evaluator::new();
        let ec = crate::eval::EvalContext::from_eval(&mut ev);
        let heap = GcHeap::new();
        let mut port = crate::io::new_string_port_input("#\\a #\\space");

        let expr = parse(ec.heap, &mut port).unwrap();
        match &heap.get_value(expr) {
            SchemeValue::Char(c) => assert_eq!(*c, 'a'),
            _ => panic!("Expected character 'a', got {}", print_value(&expr)),
        }

        let expr = parse(ec.heap, &mut port).unwrap();
        match &heap.get_value(expr) {
            SchemeValue::Char(c) => assert_eq!(*c, ' '),
            _ => panic!("Expected character ' ', got {}", print_value(&expr)),
        }
    }

    #[test]
    fn parse_quoted() {
        use crate::gc::SchemeValue;
        use crate::printer::print_value;
        let mut ev = crate::eval::Evaluator::new();
        let ec = crate::eval::EvalContext::from_eval(&mut ev);
        let heap = GcHeap::new();
        let mut port = crate::io::new_string_port_input("'hello");
        let expr = parse(ec.heap, &mut port).unwrap();

        match &heap.get_value(expr) {
            SchemeValue::Pair(quote_sym, quoted_expr) => {
                match &heap.get_value(*quote_sym) {
                    SchemeValue::Symbol(s) => assert_eq!(s, "quote"),
                    _ => panic!("Expected symbol 'quote', got {}", print_value(quote_sym)),
                }
                match &heap.get_value(*quoted_expr) {
                    SchemeValue::Pair(hello_sym, nil) => {
                        match &heap.get_value(*hello_sym) {
                            SchemeValue::Symbol(s) => assert_eq!(s, "hello"),
                            _ => panic!("Expected symbol 'hello', got {}", print_value(hello_sym)),
                        }
                        match &heap.get_value(*nil) {
                            SchemeValue::Nil => assert!(true), // Success
                            _ => panic!("Expected nil, got {}", print_value(nil)),
                        }
                    }
                    _ => panic!("Expected pair, got {}", print_value(quoted_expr)),
                }
            }
            _ => panic!("Expected pair, got {}", print_value(&expr)),
        }
    }

    #[test]
    fn parse_dotted_pair() {
        use crate::gc::SchemeValue;
        use crate::printer::print_value;
        let mut ev = crate::eval::Evaluator::new();
        let ec = crate::eval::EvalContext::from_eval(&mut ev);
        let mut port = crate::io::new_string_port_input("(1 . 2)");
        let expr = parse(ec.heap, &mut port).unwrap();

        match &ec.heap.get_value(expr) {
            SchemeValue::Pair(car, cdr) => {
                match &ec.heap.get_value(*car) {
                    SchemeValue::Int(i) => assert_eq!(i.to_string(), "1"),
                    _ => panic!("Expected integer 1, got {}", print_value(car)),
                }
                match &ec.heap.get_value(*cdr) {
                    SchemeValue::Int(i) => assert_eq!(i.to_string(), "2"),
                    _ => panic!("Expected integer 2, got {}", print_value(cdr)),
                }
            }
            _ => panic!("Expected pair, got {}", print_value(&expr)),
        }
    }

    #[test]
    fn parse_vector() {
        use crate::gc::SchemeValue;
        use crate::printer::print_value;
        let mut ev = crate::eval::Evaluator::new();
        let ec = crate::eval::EvalContext::from_eval(&mut ev);
        let mut port = crate::io::new_string_port_input("#(1 2 3)");
        let expr = parse(ec.heap, &mut port).unwrap();

        match &ec.heap.get_value(expr) {
            SchemeValue::Vector(v) => {
                assert_eq!(v.len(), 3);
                match &ec.heap.get_value(v[0]) {
                    SchemeValue::Int(i) => assert_eq!(i.to_string(), "1"),
                    _ => panic!("Expected integer 1, got {}", print_value(&v[0])),
                }
                match &ec.heap.get_value(v[1]) {
                    SchemeValue::Int(i) => assert_eq!(i.to_string(), "2"),
                    _ => panic!("Expected integer 2, got {}", print_value(&v[1])),
                }
                match &ec.heap.get_value(v[2]) {
                    SchemeValue::Int(i) => assert_eq!(i.to_string(), "3"),
                    _ => panic!("Expected integer 3, got {}", print_value(&v[2])),
                }
            }
            _ => panic!("Expected vector, got {}", print_value(&expr)),
        }
    }

    #[test]
    fn parse_float() {
        use crate::gc::SchemeValue;
        use crate::printer::print_value;
        let mut ev = crate::eval::Evaluator::new();
        let ec = crate::eval::EvalContext::from_eval(&mut ev);
        let mut port = crate::io::new_string_port_input("3.14");
        let expr = parse(ec.heap, &mut port).unwrap();
        match &ec.heap.get_value(expr) {
            SchemeValue::Float(f) => assert_eq!(*f, 3.14),
            _ => panic!("Expected float, got {}", print_value(&expr)),
        }
    }
}
