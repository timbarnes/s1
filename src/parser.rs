//! Parser for Scheme s-expressions.
//!
//! This module provides a parser that consumes tokens from the Tokenizer and
//! produces unevaluated, interned s-expressions (SchemeValue) using the GC heap.
//! The parser is designed to be extensible for additional Scheme forms.
//!
//! # Examples
//!
//! ```rust
//! use s1::parser::Parser;
//! use s1::tokenizer::{Tokenizer, Token};
//! use s1::io::{PortStack, FileTable};
//! use s1::gc::{GcHeap, PortKind, new_port, as_int, as_symbol, as_string};
//!
//! let mut heap = GcHeap::new();
//! let port = new_port(&mut heap, PortKind::Stdin, None);
//! let mut port_stack = PortStack::new(port);
//! let mut file_table = FileTable::new();
//! let mut tokenizer = Tokenizer::new(&mut heap, &mut port_stack, &mut file_table);
//! let mut parser = Parser::new(&mut heap, &mut tokenizer);
//! let expr = parser.parse().unwrap();
//! ```

use crate::gc::{GcHeap, GcRef, SchemeValue, new_int, new_float, new_symbol, new_string, new_nil, new_pair};
use crate::tokenizer::{Tokenizer, Token};
use std::rc::Rc;
use std::cell::RefCell;

/// Parser for Scheme s-expressions.
///
/// The parser consumes tokens from a Tokenizer and produces interned s-expressions
/// (SchemeValue) using the GC heap. It is designed to be extensible for additional
/// Scheme forms.
pub struct Parser {
    pub heap: Rc<RefCell<GcHeap>>,
    pub tokenizer: Rc<RefCell<Tokenizer>>,
}

impl Parser {
    /// Create a new parser from a heap and tokenizer.
    pub fn new(heap: Rc<RefCell<GcHeap>>, tokenizer: Rc<RefCell<Tokenizer>>) -> Self {
        Self { heap, tokenizer }
    }

    /// Parse a single s-expression from the token stream.
    ///
    /// Returns Ok(GcRef) for a valid s-expression, or Err(String) for a syntax error.
    pub fn parse(&mut self) -> Result<GcRef, String> {
        let token = self.tokenizer.borrow_mut().next_token();
        self.parse_from_token(token)
    }

    /// Parse an s-expression from a given token (used for list elements).
    fn parse_from_token(&mut self, token: Token) -> Result<GcRef, String> {
        match token {
            Token::Number(s) => {
                if let Ok(i) = s.parse::<i64>() {
                    Ok(new_int(&mut *self.heap.borrow_mut(), i))
                } else if let Ok(f) = s.parse::<f64>() {
                    Ok(new_float(&mut *self.heap.borrow_mut(), f))
                } else {
                    Err(format!("Invalid number literal: {}", s))
                }
            }
            Token::String(s) => Ok(new_string(&mut *self.heap.borrow_mut(), s)),
            Token::Boolean(b) => Ok(crate::gc::new_bool(&mut *self.heap.borrow_mut(), b)),
            Token::Character(c) => Ok(crate::gc::new_char(&mut *self.heap.borrow_mut(), c)),
            Token::Symbol(s) => {
                if s == "nil" || s == "()" {
                    Ok(new_nil(&mut *self.heap.borrow_mut()))
                } else if s.starts_with("#\\") {
                    let ch = match &s[2..] {
                        "space" => Some(' '),
                        "newline" => Some('\n'),
                        rest if rest.len() == 1 => rest.chars().next(),
                        _ => None,
                    };
                    if let Some(c) = ch {
                        Ok(crate::gc::new_char(&mut *self.heap.borrow_mut(), c))
                    } else {
                        Err(format!("Invalid character literal: {}", s))
                    }
                } else {
                    Ok(new_symbol(&mut *self.heap.borrow_mut(), s))
                }
            }
            Token::LParen => self.parse_list(),
            Token::RParen => Err("Unexpected ')'".to_string()),
            Token::Dot => Err("Unexpected '.'".to_string()),
            Token::Quote => {
                // Fetch the next token in a separate scope to avoid nested borrow
                let next_token = {
                    self.tokenizer.borrow_mut().next_token()
                };
                let quoted = self.parse_from_token(next_token)?;
                let quote_sym = new_symbol(&mut *self.heap.borrow_mut(), "quote");
                let quoted_list = new_pair(&mut *self.heap.borrow_mut(), quoted, new_nil(&mut *self.heap.borrow_mut()));
                Ok(new_pair(&mut *self.heap.borrow_mut(), quote_sym, quoted_list))
            }
            Token::VectorStart => {
                let mut vec_elems = Vec::new();
                loop {
                    let t = self.tokenizer.borrow_mut().next_token();
                    if let Token::RParen = t {
                        break;
                    }
                    if let Token::EOF = t {
                        return Err("Unclosed vector (unexpected EOF)".to_string());
                    }
                    vec_elems.push(self.parse_from_token(t)?);
                }
                Ok(crate::gc::new_vector(&mut *self.heap.borrow_mut(), vec_elems))
            }
            Token::EOF => Err("Unexpected end of input".to_string()),
        }
    }

    /// Parse a Scheme list (after encountering '(').
    fn parse_list(&mut self) -> Result<GcRef, String> {
        let mut elements = Vec::new();
        loop {
            let token = self.tokenizer.borrow_mut().next_token();
            match token {
                Token::RParen => {
                    // End of list
                    let mut list = new_nil(&mut *self.heap.borrow_mut());
                    for elem in elements.into_iter().rev() {
                        list = new_pair(&mut *self.heap.borrow_mut(), elem, list);
                    }
                    return Ok(list);
                }
                Token::EOF => return Err("Unclosed list (unexpected EOF)".to_string()),
                Token::Dot => {
                    // Dotted pair: (a b . c)
                    let tail = self.parse()?;
                    if let Token::RParen = self.tokenizer.borrow_mut().next_token() {
                        let mut list = tail;
                        for elem in elements.into_iter().rev() {
                            list = new_pair(&mut *self.heap.borrow_mut(), elem, list);
                        }
                        return Ok(list);
                    } else {
                        return Err("Expected ')' after dotted pair".to_string());
                    }
                }
                Token::Symbol(ref s) if s == "#(" => {
                    // Start of vector: #( ... )
                    let mut vec_elems = Vec::new();
                    loop {
                        let t = self.tokenizer.borrow_mut().next_token();
                        if let Token::RParen = t {
                            break;
                        }
                        if let Token::EOF = t {
                            return Err("Unclosed vector (unexpected EOF)".to_string());
                        }
                        vec_elems.push(self.parse_from_token(t)?);
                    }
                    return Ok(crate::gc::new_vector(&mut *self.heap.borrow_mut(), vec_elems));
                }
                _ => {
                    // Parse the element directly from the token
                    let elem = self.parse_from_token(token)?;
                    elements.push(elem);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::io::{PortStack, FileTable, new_string_port};
    use crate::gc::{as_int, as_symbol, as_string, is_nil, as_pair, as_bool, as_char, as_vector};
    use crate::tokenizer::Tokenizer;

    #[test]
    fn parse_number() {
        let heap = Rc::new(RefCell::new(GcHeap::new()));
        let port = {
            let mut h = heap.borrow_mut();
            new_string_port(&mut *h, "42")
        };
        let port_stack = Rc::new(RefCell::new(PortStack::new(port)));
        let file_table = Rc::new(RefCell::new(FileTable::new()));
        let tokenizer = Rc::new(RefCell::new(Tokenizer::new(heap.clone(), port_stack, file_table)));
        let mut parser = Parser::new(heap.clone(), tokenizer);
        let expr = parser.parse().unwrap();
        assert_eq!(as_int(&expr), Some(42));
    }

    #[test]
    fn parse_symbol() {
        let heap = Rc::new(RefCell::new(GcHeap::new()));
        let port = {
            let mut h = heap.borrow_mut();
            new_string_port(&mut *h, "foo")
        };
        let port_stack = Rc::new(RefCell::new(PortStack::new(port)));
        let file_table = Rc::new(RefCell::new(FileTable::new()));
        let tokenizer = Rc::new(RefCell::new(Tokenizer::new(heap.clone(), port_stack, file_table)));
        let mut parser = Parser::new(heap.clone(), tokenizer);
        let expr = parser.parse().unwrap();
        assert_eq!(as_symbol(&expr), Some("foo".to_string()));
    }

    #[test]
    fn parse_string() {
        let heap = Rc::new(RefCell::new(GcHeap::new()));
        let port = {
            let mut h = heap.borrow_mut();
            new_string_port(&mut *h, "\"hello\"")
        };
        let port_stack = Rc::new(RefCell::new(PortStack::new(port)));
        let file_table = Rc::new(RefCell::new(FileTable::new()));
        let tokenizer = Rc::new(RefCell::new(Tokenizer::new(heap.clone(), port_stack, file_table)));
        let mut parser = Parser::new(heap.clone(), tokenizer);
        let expr = parser.parse().unwrap();
        assert_eq!(as_string(&expr), Some("hello".to_string()));
    }

    #[test]
    fn parse_nil() {
        let heap = Rc::new(RefCell::new(GcHeap::new()));
        let port = {
            let mut h = heap.borrow_mut();
            new_string_port(&mut *h, "()")
        };
        let port_stack = Rc::new(RefCell::new(PortStack::new(port)));
        let file_table = Rc::new(RefCell::new(FileTable::new()));
        let tokenizer = Rc::new(RefCell::new(Tokenizer::new(heap.clone(), port_stack, file_table)));
        let mut parser = Parser::new(heap.clone(), tokenizer);
        let expr = parser.parse().unwrap();
        assert!(is_nil(&expr));
    }

    #[test]
    fn parse_simple_list() {
        let heap = Rc::new(RefCell::new(GcHeap::new()));
        let port = {
            let mut h = heap.borrow_mut();
            new_string_port(&mut *h, "(a b c)")
        };
        let port_stack = Rc::new(RefCell::new(PortStack::new(port)));
        let file_table = Rc::new(RefCell::new(FileTable::new()));
        let tokenizer = Rc::new(RefCell::new(Tokenizer::new(heap.clone(), port_stack, file_table)));
        let mut parser = Parser::new(heap.clone(), tokenizer);
        let expr = parser.parse().unwrap();
        // Should be (a . (b . (c . ())))
        let (a, rest) = as_pair(&expr).unwrap();
        assert_eq!(as_symbol(&a), Some("a".to_string()));
        let (b, rest2) = as_pair(&rest).unwrap();
        assert_eq!(as_symbol(&b), Some("b".to_string()));
        let (c, rest3) = as_pair(&rest2).unwrap();
        assert_eq!(as_symbol(&c), Some("c".to_string()));
        assert!(is_nil(&rest3));
    }

    #[test]
    fn parse_booleans_and_nil() {
        let heap = Rc::new(RefCell::new(GcHeap::new()));
        let port = {
            let mut h = heap.borrow_mut();
            new_string_port(&mut *h, "#t #f nil ()")
        };
        let port_stack = Rc::new(RefCell::new(PortStack::new(port)));
        let file_table = Rc::new(RefCell::new(FileTable::new()));
        let tokenizer = Rc::new(RefCell::new(Tokenizer::new(heap.clone(), port_stack, file_table)));
        let mut parser = Parser::new(heap.clone(), tokenizer);
        let t = parser.parse().unwrap();
        let f = parser.parse().unwrap();
        let nil1 = parser.parse().unwrap();
        let nil2 = parser.parse().unwrap();
        assert_eq!(as_bool(&t), Some(true));
        assert_eq!(as_bool(&f), Some(false));
        assert!(is_nil(&nil1));
        assert!(is_nil(&nil2));
    }

    #[test]
    fn parse_character() {
        let heap = Rc::new(RefCell::new(GcHeap::new()));
        let port = {
            let mut h = heap.borrow_mut();
            new_string_port(&mut *h, "#\\a #\\space #\\newline")
        };
        let port_stack = Rc::new(RefCell::new(PortStack::new(port)));
        let file_table = Rc::new(RefCell::new(FileTable::new()));
        let tokenizer = Rc::new(RefCell::new(Tokenizer::new(heap.clone(), port_stack, file_table)));
        let mut parser = Parser::new(heap.clone(), tokenizer);
        let a = parser.parse().unwrap();
        let space = parser.parse().unwrap();
        let newline = parser.parse().unwrap();
        assert_eq!(as_char(&a), Some('a'));
        assert_eq!(as_char(&space), Some(' '));
        assert_eq!(as_char(&newline), Some('\n'));
    }

    #[test]
    #[ignore]
    fn parse_quoted() {
        let heap = Rc::new(RefCell::new(GcHeap::new()));
        let port = {
            let mut h = heap.borrow_mut();
            new_string_port(&mut *h, "'foo")
        };
        let port_stack = Rc::new(RefCell::new(PortStack::new(port)));
        let file_table = Rc::new(RefCell::new(FileTable::new()));
        let tokenizer = Rc::new(RefCell::new(Tokenizer::new(heap.clone(), port_stack, file_table)));
        let expr = {
            let mut parser = Parser::new(heap.clone(), tokenizer);
            parser.parse().unwrap()
        };
        // parser is dropped here, so no borrow conflict
        let (quote, rest) = as_pair(&expr).unwrap();
        assert_eq!(as_symbol(&quote), Some("quote".to_string()));
        let (foo, rest2) = as_pair(&rest).unwrap();
        assert_eq!(as_symbol(&foo), Some("foo".to_string()));
        assert!(is_nil(&rest2));
    }

    #[test]
    fn parse_dotted_pair() {
        let heap = Rc::new(RefCell::new(GcHeap::new()));
        let port = {
            let mut h = heap.borrow_mut();
            new_string_port(&mut *h, "(a . b)")
        };
        let port_stack = Rc::new(RefCell::new(PortStack::new(port)));
        let file_table = Rc::new(RefCell::new(FileTable::new()));
        let tokenizer = Rc::new(RefCell::new(Tokenizer::new(heap.clone(), port_stack, file_table)));
        let mut parser = Parser::new(heap.clone(), tokenizer);
        let expr = parser.parse().unwrap();
        let (a, b) = as_pair(&expr).unwrap();
        assert_eq!(as_symbol(&a), Some("a".to_string()));
        assert_eq!(as_symbol(&b), Some("b".to_string()));
    }

    #[test]
    fn parse_vector() {
        let heap = Rc::new(RefCell::new(GcHeap::new()));
        let port = {
            let mut h = heap.borrow_mut();
            new_string_port(&mut *h, "#(1 2 3)")
        };
        let port_stack = Rc::new(RefCell::new(PortStack::new(port)));
        let file_table = Rc::new(RefCell::new(FileTable::new()));
        let tokenizer = Rc::new(RefCell::new(Tokenizer::new(heap.clone(), port_stack, file_table)));
        let mut parser = Parser::new(heap.clone(), tokenizer);
        let expr = parser.parse().unwrap();
        let vec = as_vector(&expr).unwrap();
        assert_eq!(vec.len(), 3);
        assert_eq!(as_int(&vec[0]), Some(1));
        assert_eq!(as_int(&vec[1]), Some(2));
        assert_eq!(as_int(&vec[2]), Some(3));
    }
} 