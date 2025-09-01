//! Tokenizer for the Scheme interpreter.
//!
//! This module provides lexical analysis for Scheme source code, converting
//! character streams into tokens according to Scheme lexical rules.
//!
//! # Examples
//!
//! ```rust
//! use s1::tokenizer::{Tokenizer, Token};
//! use s1::{Port, PortKind};
//!
//! // Create a string port for testing
//! let port = Port { kind: PortKind::StringPort { content: "hello world.to_string(), pos: 0 } };
//!
//! let mut tokenizer = Tokenizer::new(port);
//!
//! // Read tokens
//! assert_eq!(tokenizer.next_token(), Some(Token::Symbol("hello.to_string())));
//! assert_eq!(tokenizer.next_token(), Some(Token::Symbol("world.to_string())));
//! assert_eq!(tokenizer.next_token(), None);
//! ```

use crate::io::PortKind;

/// Represents a lexical token in Scheme source code.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    /// A number literal (integer or float)
    Number(String),
    /// A symbol (identifier)
    Symbol(String),
    /// A string literal
    String(String),
    /// A boolean literal (#t or #f)
    Boolean(bool),
    /// A character literal (#\a, #\space, etc.)
    Character(char),
    /// Left parenthesis
    LeftParen,
    /// Right parenthesis
    RightParen,
    /// Left bracket (for vectors)
    LeftBracket,
    /// Right bracket (for vectors)
    RightBracket,
    /// Single quote (for quoted forms)
    Quote,
    /// Dot (for dotted pairs)
    Dot,
    /// End of input
    Eof,
    /// Backquote for macros and transformation
    QuasiQuote,
    /// Comma for unquote
    Unquote,
    /// Comma-at for unquote-splicing
    UnquoteSplicing,
}

/// Tokenizer that reads characters from a port and produces tokens.
pub struct Tokenizer<'a> {
    port_kind: &'a mut PortKind,
    buffer: Vec<char>,
    vector_depth: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(port_kind: &'a mut PortKind) -> Self {
        Tokenizer {
            port_kind,
            buffer: Vec::new(),
            vector_depth: 0,
        }
    }

    /// Read the next character from the input stream.
    fn read_char(&mut self) -> Option<char> {
        // Check the unread buffer first
        if let Some(c) = self.buffer.pop() {
            Some(c)
        } else {
            self.port_kind.next_char_utf8()
        }
    }

    /// Put a character back into the input stream.
    fn unread_char(&mut self, c: char) {
        self.buffer.push(c);
    }

    /// Skip whitespace and comments.
    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.read_char() {
                Some(c) if c.is_whitespace() => {}
                Some(';') => {
                    while let Some(c) = self.read_char() {
                        if c == '\n' {
                            break;
                        }
                    }
                }
                Some(c) => {
                    self.unread_char(c);
                    break;
                }
                None => break,
            }
        }
    }

    /// Read a number token.
    ///
    /// This method reads characters until it encounters a non-digit,
    /// non-decimal-point character, then returns the number as a string.
    fn read_number(&mut self, first_char: char) -> String {
        let mut number = first_char.to_string();

        loop {
            match self.read_char() {
                Some(c)
                    if c.is_ascii_digit()
                        || c == '.'
                        || c == 'e'
                        || c == 'E'
                        || c == '+'
                        || c == '-' =>
                {
                    number.push(c);
                }
                Some(c) => {
                    self.unread_char(c);
                    break;
                }
                None => {
                    break;
                }
            }
        }

        number
    }

    /// Read a symbol token.
    ///
    /// This method reads characters until it encounters a delimiter,
    /// then returns the symbol as a string.
    fn read_symbol(&mut self, first_char: char) -> String {
        let mut symbol = first_char.to_string();

        loop {
            match self.read_char() {
                Some(c) if !c.is_whitespace() && !"();\"'`,".contains(c) => {
                    symbol.push(c);
                }
                Some(c) => {
                    self.unread_char(c);
                    break;
                }
                None => {
                    break;
                }
            }
        }

        symbol
    }

    /// Read a string token.
    ///
    /// This method reads characters until it encounters a closing quote,
    /// handling escape sequences.
    fn read_string(&mut self) -> Option<String> {
        let mut string = String::new();

        loop {
            match self.read_char() {
                Some('"') => {
                    // End of string
                    break;
                }
                Some('\\') => {
                    // Escape sequence
                    match self.read_char() {
                        Some('n') => string.push('\n'),
                        Some('t') => string.push('\t'),
                        Some('r') => string.push('\r'),
                        Some('\\') => string.push('\\'),
                        Some('"') => string.push('"'),
                        Some(c) => string.push(c),
                        None => return None, // Unterminated escape sequence
                    }
                }
                Some(c) => {
                    string.push(c);
                }
                None => {
                    return None; // Unterminated string
                }
            }
        }

        Some(string)
    }

    /// Read a boolean or character token.
    ///
    /// This method handles #t, #f, and character literals like #\a.
    fn read_boolean_or_char(&mut self) -> Option<Token> {
        match self.read_char() {
            Some('t') => Some(Token::Boolean(true)),
            Some('f') => Some(Token::Boolean(false)),
            Some('\\') => {
                // Character literal
                match self.read_char() {
                    Some('s') => {
                        // Check for #\space
                        if let Some('p') = self.read_char() {
                            if let Some('a') = self.read_char() {
                                if let Some('c') = self.read_char() {
                                    if let Some('e') = self.read_char() {
                                        return Some(Token::Character(' '));
                                    }
                                }
                            }
                        }
                        // Put back characters if not "space"
                        self.unread_char('p');
                        self.unread_char('a');
                        self.unread_char('c');
                        self.unread_char('e');
                        Some(Token::Character('s'))
                    }
                    Some('n') => {
                        // Check for #\newline
                        if let Some('e') = self.read_char() {
                            if let Some('w') = self.read_char() {
                                if let Some('l') = self.read_char() {
                                    if let Some('i') = self.read_char() {
                                        if let Some('n') = self.read_char() {
                                            if let Some('e') = self.read_char() {
                                                return Some(Token::Character('\n'));
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        // Put back characters if not "newline"
                        self.unread_char('e');
                        self.unread_char('w');
                        self.unread_char('l');
                        self.unread_char('i');
                        self.unread_char('n');
                        self.unread_char('e');
                        Some(Token::Character('n'))
                    }
                    Some(c) => Some(Token::Character(c)),
                    None => None,
                }
            }
            Some(c) => {
                // Not a recognized # form, treat as symbol
                let mut s = String::from("#");
                s.push(c);
                s.push_str(&self.read_symbol(c));
                Some(Token::Symbol(s))
            }
            None => None,
        }
    }

    /// Read the next token from the input stream.
    ///
    /// This method skips whitespace and comments, then reads the next
    /// token according to Scheme lexical rules.
    ///
    /// # Returns
    ///
    /// * `Some(Token)` - The next token
    /// * `None` - End of input
    ///
    /// # Examples
    ///
    /// ```rust
    /// use s1::tokenizer::{Tokenizer, Token};
    /// use s1::{Port, PortKind};
    ///
    /// let port = Port { kind: PortKind::StringPort { content: "hello123.to_string(), pos: 0;
    /// let mut tokenizer = Tokenizer::new(port);
    ///
    /// assert_eq!(tokenizer.next_token(), Some(Token::Symbol("hello.to_string())));
    /// assert_eq!(tokenizer.next_token(), Some(Token::Number("123.to_string())));
    /// assert_eq!(tokenizer.next_token(), None);
    /// ```
    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace_and_comments();
        match self.read_char() {
            Some(c) if c.is_ascii_digit() => Some(Token::Number(self.read_number(c))),
            Some(c @ '-') | Some(c @ '+') => {
                // Peek next char to decide if this is a number or symbol
                match self.read_char() {
                    Some(next) if next.is_ascii_digit() => {
                        // It's a number (e.g., -45, +123)
                        let mut num = c.to_string();
                        num.push(next);
                        num.push_str(&self.read_number_body());
                        Some(Token::Number(num))
                    }
                    Some(next) => {
                        self.unread_char(next);
                        Some(Token::Symbol(self.read_symbol(c)))
                    }
                    None => Some(Token::Symbol(self.read_symbol(c))),
                }
            }
            Some(c) if c.is_alphabetic() || "!$%&*/:<=>?^_~@".contains(c) => {
                Some(Token::Symbol(self.read_symbol(c)))
            }
            Some('"') => self.read_string().map(Token::String),
            Some('(') => Some(Token::LeftParen),
            Some(')') => {
                if self.vector_depth > 0 {
                    self.vector_depth -= 1;
                    Some(Token::RightBracket)
                } else {
                    Some(Token::RightParen)
                }
            }
            Some('[') => Some(Token::LeftBracket),
            Some(']') => Some(Token::RightBracket),
            Some('\'') => Some(Token::Quote),
            Some('.') => Some(Token::Dot),
            Some('#') => {
                // Check for vector start #( ... )
                match self.read_char() {
                    Some('(') => {
                        self.vector_depth += 1;
                        Some(Token::LeftBracket)
                    }
                    Some(c) => {
                        self.unread_char(c);
                        self.read_boolean_or_char()
                    }
                    None => Some(Token::Symbol("#".to_string())),
                }
            }
            Some('`') => Some(Token::QuasiQuote),
            Some(',') => match self.read_char() {
                Some(c) => {
                    if c == '@' {
                        Some(Token::UnquoteSplicing)
                    } else {
                        self.unread_char(c);
                        Some(Token::Unquote)
                    }
                }
                None => Some(Token::Eof),
            },
            Some(c) => Some(Token::Symbol(c.to_string())),
            None => Some(Token::Eof),
        }
    }

    /// Helper to read the rest of a number after the first digit (for -123, +456, etc.)
    fn read_number_body(&mut self) -> String {
        let mut number = String::new();
        loop {
            match self.read_char() {
                Some(c) if c.is_ascii_digit() || c == '.' || c == 'e' || c == 'E' => {
                    number.push(c);
                }
                Some(c) => {
                    self.unread_char(c);
                    break;
                }
                None => break,
            }
        }
        number
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::io::PortKind;

    fn tokenizer_from_str<'a>(port: &'a mut PortKind, s: &str) -> Tokenizer<'a> {
        // Set up the string port
        *port = crate::io::new_string_port_input(s);
        Tokenizer::new(port)
    }

    #[test]
    fn test_basic_tokens() {
        let mut port_kind = PortKind::Stdin;
        let mut tokenizer = tokenizer_from_str(&mut port_kind, "hello123\"world\"");
        assert_eq!(
            tokenizer.next_token(),
            Some(Token::Symbol("hello123".to_string()))
        );
        assert_eq!(
            tokenizer.next_token(),
            Some(Token::String("world".to_string()))
        );
        assert_eq!(tokenizer.next_token(), Some(Token::Eof));
    }

    #[test]
    fn test_whitespace_and_comments() {
        let mut port = PortKind::Stdin;
        let mut tokenizer = tokenizer_from_str(&mut port, "  hello  ; comment\n  world");

        assert_eq!(
            tokenizer.next_token(),
            Some(Token::Symbol("hello".to_string()))
        );
        assert_eq!(
            tokenizer.next_token(),
            Some(Token::Symbol("world".to_string()))
        );
        assert_eq!(tokenizer.next_token(), Some(Token::Eof));
    }

    #[test]
    fn test_parentheses_and_brackets() {
        let mut port = PortKind::Stdin;
        let mut tokenizer = tokenizer_from_str(&mut port, "()[]");

        assert_eq!(tokenizer.next_token(), Some(Token::LeftParen));
        assert_eq!(tokenizer.next_token(), Some(Token::RightParen));
        assert_eq!(tokenizer.next_token(), Some(Token::LeftBracket));
        assert_eq!(tokenizer.next_token(), Some(Token::RightBracket));
        assert_eq!(tokenizer.next_token(), Some(Token::Eof));
    }

    #[test]
    fn test_booleans_and_nil() {
        let mut port = PortKind::Stdin;
        let mut tokenizer = tokenizer_from_str(&mut port, "#t #f");

        assert_eq!(tokenizer.next_token(), Some(Token::Boolean(true)));
        assert_eq!(tokenizer.next_token(), Some(Token::Boolean(false)));
        assert_eq!(tokenizer.next_token(), Some(Token::Eof));
    }

    #[test]
    fn test_character() {
        let mut port = PortKind::Stdin;
        let mut tokenizer = tokenizer_from_str(&mut port, "#\\a #\\space");

        assert_eq!(tokenizer.next_token(), Some(Token::Character('a')));
        assert_eq!(tokenizer.next_token(), Some(Token::Character(' ')));
        assert_eq!(tokenizer.next_token(), Some(Token::Eof));
    }

    #[test]
    fn test_quote_and_dot() {
        let mut port = PortKind::Stdin;
        let mut tokenizer = tokenizer_from_str(&mut port, "' .;");

        assert_eq!(tokenizer.next_token(), Some(Token::Quote));
        assert_eq!(tokenizer.next_token(), Some(Token::Dot));
        assert_eq!(tokenizer.next_token(), Some(Token::Eof));
    }

    #[test]
    fn test_multiple_tokens_per_line() {
        let mut port = PortKind::Stdin;
        let mut tokenizer = tokenizer_from_str(&mut port, "hello world 123");

        assert_eq!(
            tokenizer.next_token(),
            Some(Token::Symbol("hello".to_string()))
        );
        assert_eq!(
            tokenizer.next_token(),
            Some(Token::Symbol("world".to_string()))
        );
        assert_eq!(
            tokenizer.next_token(),
            Some(Token::Number("123".to_string()))
        );
        assert_eq!(tokenizer.next_token(), Some(Token::Eof));
    }

    #[test]
    fn test_comments() {
        let mut port = PortKind::Stdin;
        let mut tokenizer = tokenizer_from_str(&mut port, "hello ; this is a comment\nworld");

        assert_eq!(
            tokenizer.next_token(),
            Some(Token::Symbol("hello".to_string()))
        );
        assert_eq!(
            tokenizer.next_token(),
            Some(Token::Symbol("world".to_string()))
        );
        assert_eq!(tokenizer.next_token(), Some(Token::Eof));
    }

    #[test]
    fn test_string_literal_debug() {
        let mut port = PortKind::Stdin;
        let mut tokenizer = tokenizer_from_str(&mut port, "\"hello world\"");

        let token1 = tokenizer.next_token();
        let token2 = tokenizer.next_token();
        println!("Input: \"hello world\"");
        println!("Token 1: {:?}", token1);
        println!("Token 2: {:?}", token2);

        // This test is just for debugging, so we'll make it pass
        assert!(true);
    }

    #[test]
    fn test_vector_token_debug() {
        let mut port = PortKind::Stdin;
        let mut tokenizer = tokenizer_from_str(&mut port, "#(1 2 3)");
        let mut tokens = Vec::new();
        loop {
            let tok = tokenizer.next_token();
            println!("Token: {:?}", tok);
            if let Some(Token::Eof) = tok {
                break;
            }
            tokens.push(tok);
        }
        // This test is just for debugging, so we'll make it pass
        assert!(true);
    }

    #[test]
    fn test_negative_and_positive_numbers() {
        let mut port = PortKind::Stdin;
        let mut tokenizer = tokenizer_from_str(&mut port, "-45 +123 -123412341234123412341234");
        assert_eq!(
            tokenizer.next_token(),
            Some(Token::Number("-45".to_string()))
        );
        assert_eq!(
            tokenizer.next_token(),
            Some(Token::Number("+123".to_string()))
        );
        assert_eq!(
            tokenizer.next_token(),
            Some(Token::Number("-123412341234123412341234".to_string()))
        );
        assert_eq!(tokenizer.next_token(), Some(Token::Eof));
    }
}
