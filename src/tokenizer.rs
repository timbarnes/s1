//! Tokenizer for the Scheme interpreter.
//!
//! This module provides lexical analysis for Scheme source code, converting
//! character streams into tokens. The tokenizer handles:
//! - Parentheses, dots, and quotes
//! - Numbers (integers and floats)
//! - Symbols (identifiers)
//! - Strings with escape sequences
//! - Comments (semicolon to end of line)
//! - Whitespace skipping
//!
//! # Examples
//!
//! ```rust
//! use s1::tokenizer::{Tokenizer, Token};
//! use s1::io::{PortStack, FileTable};
//! use s1::gc::{GcHeap, PortKind, new_port};
//!
//! let mut heap = GcHeap::new();
//! let stdin_port = new_port(&mut heap, PortKind::Stdin, None);
//! let mut port_stack = PortStack::new(stdin_port);
//! let mut file_table = FileTable::new();
//!
//! let mut tokenizer = Tokenizer::new(&mut heap, &mut port_stack, &mut file_table);
//! let token = tokenizer.next_token();
//! ```
//!
//! # Token Types
//!
//! The tokenizer produces the following token types:
//! - `LParen` - Left parenthesis `(`
//! - `RParen` - Right parenthesis `)`
//! - `Dot` - Dot `.` (when standalone)
//! - `Quote` - Quote `'`
//! - `String` - String literals in double quotes
//! - `Number` - Numeric literals (integers and floats)
//! - `Symbol` - Identifiers and special symbols
//! - `EOF` - End of input

use crate::io::{PortStack, FileTable, read_char};
use crate::gc::GcHeap;
use std::rc::Rc;
use std::cell::RefCell;

/// Represents a lexical token in Scheme source code.
///
/// Tokens are the basic building blocks that the parser uses to construct
/// the abstract syntax tree. Each token represents a meaningful unit of
/// Scheme syntax.
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    /// Left parenthesis `(`
    LParen,
    /// Right parenthesis `)`
    RParen,
    /// Dot `.` (when it's a standalone token, not part of a symbol)
    Dot,
    /// Quote `'`
    Quote,
    /// String literal in double quotes
    String(String),
    /// Numeric literal (integer or float)
    Number(String),
    /// Symbol (identifier or special symbol)
    Symbol(String),
    /// Boolean literal (#t or #f)
    Boolean(bool),
    /// Character literal (#\...)
    Character(char),
    /// Vector start (#(...))
    VectorStart,
    /// End of input
    EOF,
}

/// Lexical analyzer that converts character streams into tokens.
///
/// The tokenizer reads characters from a port stack and produces tokens
/// according to Scheme lexical rules. It handles whitespace, comments,
/// and all Scheme lexical constructs.
///
/// # Examples
///
/// ```rust
/// use s1::tokenizer::{Tokenizer, Token};
/// use s1::io::{PortStack, FileTable};
/// use s1::gc::{GcHeap, PortKind, new_port};
///
/// let mut heap = GcHeap::new();
/// let port = new_port(&mut heap, PortKind::Stdin, None);
/// let mut port_stack = PortStack::new(port);
/// let mut file_table = FileTable::new();
///
/// let mut tokenizer = Tokenizer::new(&mut heap, &mut port_stack, &mut file_table);
/// let token = tokenizer.next_token();
/// ```
pub struct Tokenizer {
    /// Shared reference to the garbage collected heap
    pub heap: Rc<RefCell<GcHeap>>,
    /// Reference to the port stack for reading input
    pub port_stack: Rc<RefCell<PortStack>>,
    /// Reference to the file table for file port operations
    pub file_table: Rc<RefCell<FileTable>>,
    /// Character buffer for unreading characters
    buffer: Vec<char>,
}

impl Tokenizer {
    /// Create a new tokenizer that reads from the given port stack.
    ///
    /// The tokenizer will read characters from the current port in the
    /// port stack and produce tokens according to Scheme lexical rules.
    ///
    /// # Arguments
    ///
    /// * `heap` - The garbage collected heap for memory management
    /// * `port_stack` - The port stack to read characters from
    /// * `file_table` - The file table for file port operations
    ///
    /// # Examples
    ///
    /// ```rust
    /// use s1::tokenizer::Tokenizer;
    /// use s1::io::{PortStack, FileTable};
    /// use s1::gc::{GcHeap, PortKind, new_port};
    ///
    /// let mut heap = GcHeap::new();
    /// let port = new_port(&mut heap, PortKind::Stdin, None);
    /// let mut port_stack = PortStack::new(port);
    /// let mut file_table = FileTable::new();
    ///
    /// let tokenizer = Tokenizer::new(&mut heap, &mut port_stack, &mut file_table);
    /// ```
    pub fn new(heap: Rc<RefCell<GcHeap>>, port_stack: Rc<RefCell<PortStack>>, file_table: Rc<RefCell<FileTable>>) -> Self {
        Self { heap, port_stack, file_table, buffer: Vec::new() }
    }

    /// Read a character from the input stream.
    ///
    /// This method first checks the internal buffer for unread characters,
    /// then falls back to reading from the port stack.
    ///
    /// # Returns
    ///
    /// Returns `Some(char)` if a character was read, or `None` if the
    /// input stream is exhausted.
    fn read_char(&mut self) -> Option<char> {
        if let Some(c) = self.buffer.pop() {
            Some(c)
        } else {
            read_char(&mut *self.heap.borrow_mut(), &mut *self.port_stack.borrow_mut(), &mut *self.file_table.borrow_mut())
        }
    }

    /// Put a character back into the input stream.
    ///
    /// This method allows the tokenizer to "peek ahead" by reading a character
    /// and then putting it back if it's not needed.
    ///
    /// # Arguments
    ///
    /// * `c` - The character to put back
    pub fn unread_char(&mut self, c: char) {
        self.buffer.push(c);
    }

    /// Put a token back into the input stream.
    ///
    /// This method allows the tokenizer to "peek ahead" by reading a token
    /// and then putting it back if it's not needed.
    ///
    /// # Arguments
    ///
    /// * `token` - The token to put back
    pub fn unread_char_if_token(&mut self, token: Token) {
        match token {
            Token::LParen => self.unread_char('('),
            Token::RParen => self.unread_char(')'),
            Token::Dot => self.unread_char('.'),
            Token::Quote => self.unread_char(' '), // Not supported for now
            Token::String(_) | Token::Number(_) | Token::Symbol(_) | Token::EOF => {}
            Token::Boolean(_) => {},
            Token::Character(_) => {},
            Token::VectorStart => {},
        }
    }

    /// Skip whitespace and comments until the next meaningful character.
    ///
    /// This method consumes whitespace characters and semicolon comments
    /// until it encounters a character that could be part of a token.
    fn skip_whitespace_and_comments(&mut self) {
        loop {
            let c = self.read_char();
            match c {
                Some(';') => {
                    // Skip to end of line
                    while let Some(ch) = self.read_char() {
                        if ch == '\n' { break; }
                    }
                }
                Some(ch) if ch.is_whitespace() => continue,
                Some(ch) => {
                    self.unread_char(ch);
                    break;
                }
                None => break,
            }
        }
    }

    /// Read and return the next token from the input stream.
    ///
    /// This method skips whitespace and comments, then reads characters
    /// to determine the next token type. It handles all Scheme lexical
    /// constructs including parentheses, numbers, symbols, and strings.
    ///
    /// # Returns
    ///
    /// Returns the next token, or `Token::EOF` if the input stream is exhausted.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use s1::tokenizer::{Tokenizer, Token};
    /// use s1::io::{PortStack, FileTable};
    /// use s1::gc::{GcHeap, PortKind, new_port};
    ///
    /// let mut heap = GcHeap::new();
    /// let port = new_port(&mut heap, PortKind::Stdin, None);
    /// let mut port_stack = PortStack::new(port);
    /// let mut file_table = FileTable::new();
    ///
    /// let mut tokenizer = Tokenizer::new(&mut heap, &mut port_stack, &mut file_table);
    /// let token = tokenizer.next_token();
    /// ```
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace_and_comments();
        let c = match self.read_char() {
            Some(c) => c,
            None => return Token::EOF,
        };
        let mut ch = c;
        loop {
            match ch {
                '(' => return Token::LParen,
                ')' => return Token::RParen,
                '.' => {
                    let next = self.read_char();
                    match next {
                        Some(nc) if is_delimiter(nc) => {
                            self.unread_char(nc);
                            return Token::Dot;
                        }
                        None => return Token::Dot,
                        Some(nc) => {
                            self.unread_char(nc);
                            return self.read_symbol('.');
                        }
                    }
                }
                '\'' => return Token::Quote,
                '"' => return self.read_string(),
                '#' => {
                    // Boolean, character, or vector
                    match self.read_char() {
                        Some('t') => return Token::Boolean(true),
                        Some('f') => return Token::Boolean(false),
                        Some('(') => return Token::VectorStart,
                        Some('\\') => {
                            // Character literal
                            let mut name = String::new();
                            while let Some(ch) = self.read_char() {
                                if is_delimiter(ch) { self.unread_char(ch); break; }
                                name.push(ch);
                            }
                            let c = match name.as_str() {
                                "space" => Some(' '),
                                "newline" => Some('\n'),
                                s if s.len() == 1 => s.chars().next(),
                                _ => None,
                            };
                            if let Some(c) = c {
                                return Token::Character(c);
                            } else {
                                return Token::Symbol(format!("#\\{}", name));
                            }
                        }
                        Some(other) => {
                            // Not a recognized # form, treat as symbol
                            let mut s = String::from("#");
                            s.push(other);
                            while let Some(ch) = self.read_char() {
                                if is_delimiter(ch) { self.unread_char(ch); break; }
                                s.push(ch);
                            }
                            return Token::Symbol(s);
                        }
                        None => return Token::Symbol("#".to_string()),
                    }
                }
                ch if ch.is_ascii_digit() || (ch == '-' || ch == '+') => {
                    return self.read_number_or_symbol(ch)
                }
                ch if is_initial_symbol_char(ch) => return self.read_symbol(ch),
                _ => {
                    ch = match self.read_char() {
                        Some(next) => next,
                        None => return Token::EOF,
                    };
                }
            }
        }
    }

    /// Read a string literal from the input stream.
    ///
    /// This method reads characters until it encounters a closing double quote,
    /// handling escape sequences (currently just `\"`).
    ///
    /// # Returns
    ///
    /// Returns a `Token::String` containing the string content.
    fn read_string(&mut self) -> Token {
        let mut s = String::new();
        while let Some(ch) = self.read_char() {
            if ch == '"' {
                break;
            } else if ch == '\\' {
                if let Some(next) = self.read_char() {
                    s.push(next);
                } else {
                    break;
                }
            } else {
                s.push(ch);
            }
        }
        Token::String(s)
    }

    /// Read a number or symbol starting with a digit or sign.
    ///
    /// This method handles the ambiguity between numbers and symbols that
    /// start with digits or signs. It reads characters until it encounters
    /// a delimiter, then determines if the result is a valid number.
    ///
    /// # Arguments
    ///
    /// * `first` - The first character (digit, `+`, or `-`)
    ///
    /// # Returns
    ///
    /// Returns either a `Token::Number` or `Token::Symbol` depending on
    /// whether the characters form a valid number.
    fn read_number_or_symbol(&mut self, first: char) -> Token {
        let mut s = String::new();
        s.push(first);
        while let Some(ch) = self.read_char() {
            if ch.is_ascii_alphanumeric() || ".+-@".contains(ch) {
                s.push(ch);
            } else {
                self.unread_char(ch);
                break;
            }
        }
        // Special case: single '.', '+', or '-' are symbols in Scheme
        if s == "." || s == "+" || s == "-" {
            Token::Symbol(s)
        } else if is_number(&s) {
            Token::Number(s)
        } else {
            Token::Symbol(s)
        }
    }

    /// Read a symbol from the input stream.
    ///
    /// This method reads characters that are valid for symbols until it
    /// encounters a delimiter.
    ///
    /// # Arguments
    ///
    /// * `first` - The first character of the symbol
    ///
    /// # Returns
    ///
    /// Returns a `Token::Symbol` containing the symbol name.
    fn read_symbol(&mut self, first: char) -> Token {
        let mut s = String::new();
        s.push(first);
        while let Some(ch) = self.read_char() {
            if is_subsequent_symbol_char(ch) {
                s.push(ch);
            } else {
                self.unread_char(ch);
                break;
            }
        }
        Token::Symbol(s)
    }
}

/// Check if a character can be the first character of a symbol.
///
/// According to Scheme lexical rules, symbols can start with alphabetic
/// characters or certain special characters.
///
/// # Arguments
///
/// * `ch` - The character to check
///
/// # Returns
///
/// Returns `true` if the character can start a symbol.
///
/// # Examples
///
/// ```rust
/// use s1::tokenizer::is_initial_symbol_char;
///
/// assert!(is_initial_symbol_char('a'));
/// assert!(is_initial_symbol_char('!'));
/// assert!(is_initial_symbol_char('?'));
/// assert!(!is_initial_symbol_char('1'));
/// assert!(!is_initial_symbol_char('('));
/// ```
fn is_initial_symbol_char(ch: char) -> bool {
    ch.is_ascii_alphabetic() || "!$%&*/:<=>?^_~@".contains(ch)
}

/// Check if a character can be part of a symbol (after the first character).
///
/// Subsequent characters in symbols can include digits and additional
/// special characters beyond those allowed as initial characters.
///
/// # Arguments
///
/// * `ch` - The character to check
///
/// # Returns
///
/// Returns `true` if the character can be part of a symbol.
///
/// # Examples
///
/// ```rust
/// use s1::tokenizer::is_subsequent_symbol_char;
///
/// assert!(is_subsequent_symbol_char('a'));
/// assert!(is_subsequent_symbol_char('1'));
/// assert!(is_subsequent_symbol_char('!'));
/// assert!(is_subsequent_symbol_char('+'));
/// assert!(!is_subsequent_symbol_char('('));
/// ```
fn is_subsequent_symbol_char(ch: char) -> bool {
    is_initial_symbol_char(ch) || ch.is_ascii_digit() || ".+-@".contains(ch)
}

/// Check if a string represents a valid number.
///
/// This function attempts to parse the string as both an integer and a float.
///
/// # Arguments
///
/// * `s` - The string to check
///
/// # Returns
///
/// Returns `true` if the string can be parsed as a number.
///
/// # Examples
///
/// ```rust
/// use s1::tokenizer::is_number;
///
/// assert!(is_number("123"));
/// assert!(is_number("-45"));
/// assert!(is_number("3.14"));
/// assert!(is_number("+inf.0"));
/// assert!(!is_number("abc"));
/// assert!(!is_number("123abc"));
/// ```
fn is_number(s: &str) -> bool {
    s.parse::<i64>().is_ok() || s.parse::<f64>().is_ok()
}

/// Check if a character is a delimiter that ends tokens.
///
/// Delimiters include whitespace, parentheses, quotes, and semicolons.
///
/// # Arguments
///
/// * `ch` - The character to check
///
/// # Returns
///
/// Returns `true` if the character is a delimiter.
///
/// # Examples
///
/// ```rust
/// use s1::tokenizer::is_delimiter;
///
/// assert!(is_delimiter(' '));
/// assert!(is_delimiter('('));
/// assert!(is_delimiter(')'));
/// assert!(is_delimiter('"'));
/// assert!(is_delimiter(';'));
/// assert!(!is_delimiter('a'));
/// assert!(!is_delimiter('1'));
/// ```
fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || ch == '(' || ch == ')' || ch == '"' || ch == ';'
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::io::{PortStack, FileTable};
    use crate::gc::{PortKind, new_port};
    use std::fs::File;
    use std::io::Write;

    fn tokenizer_from_str(heap: Rc<RefCell<GcHeap>>, s: &str) -> Tokenizer {
        use crate::io::new_string_port;
        let port = new_string_port(&mut *heap.borrow_mut(), s);
        let port_stack = Rc::new(RefCell::new(PortStack::new(port)));
        let file_table = Rc::new(RefCell::new(FileTable::new()));
        Tokenizer::new(heap, port_stack, file_table)
    }

    #[test]
    fn test_simple_tokens() {
        let heap = Rc::new(RefCell::new(GcHeap::new()));
        let mut t = tokenizer_from_str(heap.clone(), "( ) . '");
        assert_eq!(t.next_token(), Token::LParen);
        assert_eq!(t.next_token(), Token::RParen);
        assert_eq!(t.next_token(), Token::Dot);
        assert_eq!(t.next_token(), Token::Quote);
        assert_eq!(t.next_token(), Token::EOF);
    }

    #[test]
    fn test_numbers_and_symbols() {
        let heap = Rc::new(RefCell::new(GcHeap::new()));
        let mut t = tokenizer_from_str(heap.clone(), "123 -45 3.14 foo bar-1 +inf.0");
        assert_eq!(t.next_token(), Token::Number("123".to_string()));
        assert_eq!(t.next_token(), Token::Number("-45".to_string()));
        assert_eq!(t.next_token(), Token::Number("3.14".to_string()));
        assert_eq!(t.next_token(), Token::Symbol("foo".to_string()));
        assert_eq!(t.next_token(), Token::Symbol("bar-1".to_string()));
        assert_eq!(t.next_token(), Token::Symbol("+inf.0".to_string()));
        assert_eq!(t.next_token(), Token::EOF);
    }

    #[test]
    fn test_strings() {
        let heap = Rc::new(RefCell::new(GcHeap::new()));
        let mut t = tokenizer_from_str(heap.clone(), "\"hello\" \"a b c\"");
        assert_eq!(t.next_token(), Token::String("hello".to_string()));
        assert_eq!(t.next_token(), Token::String("a b c".to_string()));
        assert_eq!(t.next_token(), Token::EOF);
    }

    #[test]
    fn test_comments_and_whitespace() {
        let heap = Rc::new(RefCell::new(GcHeap::new()));
        let mut t = tokenizer_from_str(heap.clone(), "foo ; this is a comment\nbar\n; full line\n123");
        assert_eq!(t.next_token(), Token::Symbol("foo".to_string()));
        assert_eq!(t.next_token(), Token::Symbol("bar".to_string()));
        assert_eq!(t.next_token(), Token::Number("123".to_string()));
        assert_eq!(t.next_token(), Token::EOF);
    }

    #[test]
    fn test_multiple_tokens_per_line() {
        let heap = Rc::new(RefCell::new(GcHeap::new()));
        let mut t = tokenizer_from_str(heap.clone(), "(foo 123) (bar)");
        assert_eq!(t.next_token(), Token::LParen);
        assert_eq!(t.next_token(), Token::Symbol("foo".to_string()));
        assert_eq!(t.next_token(), Token::Number("123".to_string()));
        assert_eq!(t.next_token(), Token::RParen);
        assert_eq!(t.next_token(), Token::LParen);
        assert_eq!(t.next_token(), Token::Symbol("bar".to_string()));
        assert_eq!(t.next_token(), Token::RParen);
        assert_eq!(t.next_token(), Token::EOF);
    }

    #[test]
    fn test_special_symbol_chars() {
        let heap = Rc::new(RefCell::new(GcHeap::new()));
        let mut t = tokenizer_from_str(heap.clone(), "foo@bar . + - ... @@@");
        assert_eq!(t.next_token(), Token::Symbol("foo@bar".to_string()));
        assert_eq!(t.next_token(), Token::Dot);
        assert_eq!(t.next_token(), Token::Symbol("+".to_string()));
        assert_eq!(t.next_token(), Token::Symbol("-".to_string()));
        assert_eq!(t.next_token(), Token::Symbol("...".to_string()));
        assert_eq!(t.next_token(), Token::Symbol("@@@".to_string()));
        assert_eq!(t.next_token(), Token::EOF);
    }

    #[test]
    fn test_eof_handling() {
        let heap = Rc::new(RefCell::new(GcHeap::new()));
        let mut t = tokenizer_from_str(heap.clone(), "");
        assert_eq!(t.next_token(), Token::EOF);
        let mut t = tokenizer_from_str(heap.clone(), "foo");
        assert_eq!(t.next_token(), Token::Symbol("foo".to_string()));
        assert_eq!(t.next_token(), Token::EOF);
    }

    #[test]
    fn test_booleans_and_nil() {
        let heap = Rc::new(RefCell::new(GcHeap::new()));
        let mut t = tokenizer_from_str(heap.clone(), "#t #f nil ()");
        assert_eq!(t.next_token(), Token::Boolean(true));
        assert_eq!(t.next_token(), Token::Boolean(false));
        assert_eq!(t.next_token(), Token::Symbol("nil".to_string()));
        assert_eq!(t.next_token(), Token::LParen);
        assert_eq!(t.next_token(), Token::RParen);
        assert_eq!(t.next_token(), Token::EOF);
    }

    #[test]
    fn test_character() {
        let heap = Rc::new(RefCell::new(GcHeap::new()));
        let mut t = tokenizer_from_str(heap.clone(), "#\\a #\\space #\\newline");
        assert_eq!(t.next_token(), Token::Character('a'));
        assert_eq!(t.next_token(), Token::Character(' '));
        assert_eq!(t.next_token(), Token::Character('\n'));
        assert_eq!(t.next_token(), Token::EOF);
    }

    #[test]
    fn test_vector_start() {
        let heap = Rc::new(RefCell::new(GcHeap::new()));
        let mut t = tokenizer_from_str(heap.clone(), "#(1 2 3)");
        assert_eq!(t.next_token(), Token::VectorStart);
        let n1 = t.next_token();
        assert_eq!(n1, Token::Number("1".to_string()));
        let n2 = t.next_token();
        assert_eq!(n2, Token::Number("2".to_string()));
        let n3 = t.next_token();
        assert_eq!(n3, Token::Number("3".to_string()));
        let rparen = t.next_token();
        assert_eq!(rparen, Token::RParen);
        assert_eq!(t.next_token(), Token::EOF);
    }
} 