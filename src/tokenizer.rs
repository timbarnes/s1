use crate::io::{PortStack, FileTable, read_char};
use crate::gc::GcHeap;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    LParen,
    RParen,
    Dot,
    Quote,
    String(String),
    Number(String),
    Symbol(String),
    EOF,
}

pub struct Tokenizer<'a> {
    heap: &'a mut GcHeap,
    port_stack: &'a mut PortStack,
    file_table: &'a mut FileTable,
    buffer: Vec<char>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(heap: &'a mut GcHeap, port_stack: &'a mut PortStack, file_table: &'a mut FileTable) -> Self {
        Self { heap, port_stack, file_table, buffer: Vec::new() }
    }

    fn read_char(&mut self) -> Option<char> {
        if let Some(c) = self.buffer.pop() {
            Some(c)
        } else {
            // Use the global heap for all tokenization
            read_char(self.heap, self.port_stack, self.file_table)
        }
    }

    fn unread_char(&mut self, c: char) {
        self.buffer.push(c);
    }

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
                    // Peek ahead: if next char is delimiter/whitespace/EOF, it's a dot token
                    let next = self.read_char();
                    match next {
                        Some(nc) if is_delimiter(nc) => {
                            self.unread_char(nc);
                            return Token::Dot;
                        }
                        None => return Token::Dot,
                        Some(nc) => {
                            // Not a delimiter, treat as symbol starting with '.'
                            self.unread_char(nc);
                            return self.read_symbol('.');
                        }
                    }
                }
                '\'' => return Token::Quote,
                '"' => return self.read_string(),
                ch if ch.is_ascii_digit() || (ch == '-' || ch == '+') => {
                    return self.read_number_or_symbol(ch)
                }
                ch if is_initial_symbol_char(ch) => return self.read_symbol(ch),
                _ => {
                    // Skip unknown chars, try next
                    ch = match self.read_char() {
                        Some(next) => next,
                        None => return Token::EOF,
                    };
                }
            }
        }
    }

    fn read_string(&mut self) -> Token {
        let mut s = String::new();
        while let Some(ch) = self.read_char() {
            if ch == '"' {
                return Token::String(s);
            } else if ch == '\\' {
                // Handle escape
                if let Some(next) = self.read_char() {
                    s.push(next);
                } else {
                    // EOF after escape, return what we have
                    return Token::String(s);
                }
            } else {
                s.push(ch);
            }
        }
        // EOF before closing quote, return what we have
        Token::String(s)
    }

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

fn is_initial_symbol_char(ch: char) -> bool {
    ch.is_ascii_alphabetic() || "!$%&*/:<=>?^_~@".contains(ch)
}

fn is_subsequent_symbol_char(ch: char) -> bool {
    is_initial_symbol_char(ch) || ch.is_ascii_digit() || ".+-@".contains(ch)
}

// Helper: check if a string is a valid number
fn is_number(s: &str) -> bool {
    s.parse::<i64>().is_ok() || s.parse::<f64>().is_ok()
}

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

    fn tokenizer_from_str<'a>(heap: &'a mut GcHeap, s: &str) -> Tokenizer<'a> {
        use crate::io::new_string_port;
        let port = new_string_port(heap, s);
        let mut port_stack = Box::new(PortStack::new(port));
        let mut file_table = Box::new(FileTable::new());
        Tokenizer::new(heap, Box::leak(port_stack), Box::leak(file_table))
    }

    #[test]
    fn test_simple_tokens() {
        let mut heap = GcHeap::new();
        let mut t = tokenizer_from_str(&mut heap, "( ) . '");
        assert_eq!(t.next_token(), Token::LParen);
        assert_eq!(t.next_token(), Token::RParen);
        assert_eq!(t.next_token(), Token::Dot);
        assert_eq!(t.next_token(), Token::Quote);
        assert_eq!(t.next_token(), Token::EOF);
    }

    #[test]
    fn test_numbers_and_symbols() {
        let mut heap = GcHeap::new();
        let mut t = tokenizer_from_str(&mut heap, "123 -45 3.14 foo bar-1 +inf.0");
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
        let mut heap = GcHeap::new();
        let mut t = tokenizer_from_str(&mut heap, "\"hello\" \"a b c\"");
        assert_eq!(t.next_token(), Token::String("hello".to_string()));
        assert_eq!(t.next_token(), Token::String("a b c".to_string()));
        assert_eq!(t.next_token(), Token::EOF);
    }

    #[test]
    fn test_comments_and_whitespace() {
        let mut heap = GcHeap::new();
        let mut t = tokenizer_from_str(&mut heap, "foo ; this is a comment\nbar\n; full line\n123");
        assert_eq!(t.next_token(), Token::Symbol("foo".to_string()));
        assert_eq!(t.next_token(), Token::Symbol("bar".to_string()));
        assert_eq!(t.next_token(), Token::Number("123".to_string()));
        assert_eq!(t.next_token(), Token::EOF);
    }

    #[test]
    fn test_multiple_tokens_per_line() {
        let mut heap = GcHeap::new();
        let mut t = tokenizer_from_str(&mut heap, "(foo 123) (bar)");
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
        let mut heap = GcHeap::new();
        let mut t = tokenizer_from_str(&mut heap, "foo@bar . + - ... @@@");
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
        let mut heap = GcHeap::new();
        let mut t = tokenizer_from_str(&mut heap, "");
        assert_eq!(t.next_token(), Token::EOF);
        let mut t = tokenizer_from_str(&mut heap, "foo");
        assert_eq!(t.next_token(), Token::Symbol("foo".to_string()));
        assert_eq!(t.next_token(), Token::EOF);
    }
} 