//! # Genpay Lexical Analyzer
//! Tools to convert user's source code into abstracted data structures - tokens. <br/>
//! Wikipedia Explanation: <https://en.wikipedia.org/wiki/Lexical_analysis> <br/>
//!
//! Main tool here is the [`Lexer`] structure (you can also check examples there)
//!
//! ## Usage
//! ```rust
//! use genpay_lexer::{Lexer, token_type::TokenType};
//!
//! let input = "1 + 1";
//! let mut lexer = Lexer::new(input, "test.gen");
//!
//! let tokens: Vec<_> = lexer.collect();
//!
//! assert!(tokens.len() == 3);
//!
//! println!("{:?}", tokens);
//! ```

use crate::{
    error::LexerError,
    token::Token,
    token_type::TokenType,
};
use miette::NamedSource;

/// Error Handling Module
pub mod error;
/// Helpful Macros Module
mod macros;
/// Token Object and Implementations
pub mod token;
/// Token Types Enumeration
pub mod token_type;

/// Main lexical analyzer instance
///
/// ### Usage example:
pub struct Lexer<'s> {
    src: &'s str,
    filename: &'s str,
    cursor: usize,
}

impl<'s> Lexer<'s> {
    pub fn new(src: &'s str, filename: &'s str) -> Self {
        Lexer {
            src,
            filename,
            cursor: 0,
        }
    }

    /* helpers ----------------------------------------------------------- */
    fn peek(&self) -> Option<u8> {
        self.src.as_bytes().get(self.cursor).copied()
    }
    fn bump(&mut self) {
        self.cursor += 1;
    }
    fn slice(&self, start: usize) -> &'s str {
        &self.src[start..self.cursor]
    }
    fn skip_ws(&mut self) {
        while let Some(b) = self.peek() {
            if b.is_ascii_whitespace() {
                self.bump();
            } else {
                break;
            }
        }
    }
    fn eat_while<F>(&mut self, mut f: F) -> &'s str
    where
        F: FnMut(u8) -> bool,
    {
        let start = self.cursor;
        while let Some(b) = self.peek() {
            if b == b'\\' {
                self.bump();
                self.bump();
                continue;
            }
            if f(b) {
                self.bump();
            } else {
                break;
            }
        }
        self.slice(start)
    }

    fn eat_while_radix<F>(&mut self, _radix: u32, mut f: F) -> &'s str
    where
        F: FnMut(u8) -> bool,
    {
        let start = self.cursor;
        while let Some(b) = self.peek() {
            if f(b) {
                self.bump();
            } else {
                break;
            }
        }
        self.slice(start)
    }

    /* next token -------------------------------------------------------- */
    pub fn next_token(&mut self) -> Result<Token<'s>, LexerError> {
        self.skip_ws();
        let start = self.cursor;
        match self.peek() {
            None => Ok(Token::new("", TokenType::EOF, (start, start))),
            Some(b'a'..=b'z' | b'A'..=b'Z' | b'_') => {
                let ident = self.eat_while(|b| b.is_ascii_alphanumeric() || b == b'_');
                let token_type = match ident {
                    "let" => TokenType::Keyword,
                    "pub" => TokenType::Keyword,
                    "fn" => TokenType::Keyword,
                    "import" => TokenType::Keyword,
                    "include" => TokenType::Keyword,
                    "extern" => TokenType::Keyword,
                    "return" => TokenType::Keyword,
                    "struct" => TokenType::Keyword,
                    "enum" => TokenType::Keyword,
                    "typedef" => TokenType::Keyword,
                    "if" => TokenType::Keyword,
                    "else" => TokenType::Keyword,
                    "while" => TokenType::Keyword,
                    "for" => TokenType::Keyword,
                    "break" => TokenType::Keyword,
                    "true" => TokenType::Boolean,
                    "false" => TokenType::Boolean,
                    "NULL" => TokenType::Null,
                    "i8" => TokenType::Type,
                    "i16" => TokenType::Type,
                    "i32" => TokenType::Type,
                    "i64" => TokenType::Type,
                    "u8" => TokenType::Type,
                    "u16" => TokenType::Type,
                    "u32" => TokenType::Type,
                    "u64" => TokenType::Type,
                    "usize" => TokenType::Type,
                    "f32" => TokenType::Type,
                    "f64" => TokenType::Type,
                    "bool" => TokenType::Type,
                    "char" => TokenType::Type,
                    "void" => TokenType::Type,
                    _ => TokenType::Identifier,
                };
                Ok(Token::new(ident, token_type, (start, self.cursor)))
            }
            Some(b'0'..=b'9') => {
                let num = self.eat_while_radix(10, |b| b.is_ascii_digit());
                if self.peek() == Some(b'.') {
                    self.bump();
                    let frac = self.eat_while_radix(10, |b| b.is_ascii_digit());
                    let mut value = String::from(num);
                    value.push('.');
                    value.push_str(frac);
                    Ok(Token::new(
                        self.slice(start),
                        TokenType::FloatNumber,
                        (start, self.cursor),
                    ))
                } else if self.peek() == Some(b'x') {
                    self.bump();
                    let num = self.eat_while_radix(16, |b| b.is_ascii_hexdigit());
                    Ok(Token::new(self.slice(start), TokenType::Number, (start, self.cursor)))
                } else if self.peek() == Some(b'b') {
                    self.bump();
                    let num = self.eat_while_radix(2, |b| b == b'0' || b == b'1');
                    Ok(Token::new(self.slice(start), TokenType::Number, (start, self.cursor)))
                } else {
                    Ok(Token::new(num, TokenType::Number, (start, self.cursor)))
                }
            }
            Some(b'"') => {
                self.bump(); // opening quote
                let lit = self.eat_while(|b| b != b'"');
                if self.peek() == Some(b'"') {
                    self.bump();
                }
                Ok(Token::new(lit, TokenType::String, (start, self.cursor)))
            }
            Some(b'\'') => {
                self.bump(); // opening quote
                let lit = self.eat_while(|b| b != b'\'');
                if self.peek() == Some(b'\'') {
                    self.bump();
                }
                Ok(Token::new(lit, TokenType::Char, (start, self.cursor)))
            }
            Some(b'+') => {
                self.bump();
                Ok(Token::new("+", TokenType::Plus, (start, self.cursor)))
            }
            Some(b'-') => {
                self.bump();
                Ok(Token::new("-", TokenType::Minus, (start, self.cursor)))
            }
            Some(b'*') => {
                self.bump();
                Ok(Token::new("*", TokenType::Multiply, (start, self.cursor)))
            }
            Some(b'/') => {
                self.bump();
                if self.peek() == Some(b'/') {
                    self.eat_while(|b| b != b'\n');
                    self.next_token()
                } else {
                    Ok(Token::new("/", TokenType::Divide, (start, self.cursor)))
                }
            }
            Some(b'%') => {
                self.bump();
                Ok(Token::new("%", TokenType::Modulus, (start, self.cursor)))
            }
            Some(b'=') => {
                self.bump();
                if self.peek() == Some(b'=') {
                    self.bump();
                    Ok(Token::new("==", TokenType::Eq, (start, self.cursor)))
                } else {
                    Ok(Token::new("=", TokenType::Equal, (start, self.cursor)))
                }
            }
            Some(b'!') => {
                self.bump();
                if self.peek() == Some(b'=') {
                    self.bump();
                    Ok(Token::new("!=", TokenType::Ne, (start, self.cursor)))
                } else {
                    Ok(Token::new("!", TokenType::Not, (start, self.cursor)))
                }
            }
            Some(b'<') => {
                self.bump();
                if self.peek() == Some(b'=') {
                    self.bump();
                    Ok(Token::new("<=", TokenType::Leq, (start, self.cursor)))
                } else if self.peek() == Some(b'<') {
                    self.bump();
                    Ok(Token::new("<<", TokenType::LShift, (start, self.cursor)))
                } else {
                    Ok(Token::new("<", TokenType::Lt, (start, self.cursor)))
                }
            }
            Some(b'>') => {
                self.bump();
                if self.peek() == Some(b'=') {
                    self.bump();
                    Ok(Token::new(">=", TokenType::Beq, (start, self.cursor)))
                } else if self.peek() == Some(b'>') {
                    self.bump();
                    Ok(Token::new(">>", TokenType::RShift, (start, self.cursor)))
                } else {
                    Ok(Token::new(">", TokenType::Bt, (start, self.cursor)))
                }
            }
            Some(b'&') => {
                self.bump();
                if self.peek() == Some(b'&') {
                    self.bump();
                    Ok(Token::new("&&", TokenType::And, (start, self.cursor)))
                } else {
                    Ok(Token::new("&", TokenType::Ampersand, (start, self.cursor)))
                }
            }
            Some(b'|') => {
                self.bump();
                if self.peek() == Some(b'|') {
                    self.bump();
                    Ok(Token::new("||", TokenType::Or, (start, self.cursor)))
                } else {
                    Ok(Token::new("|", TokenType::Verbar, (start, self.cursor)))
                }
            }
            Some(b'^') => {
                self.bump();
                Ok(Token::new("^", TokenType::Xor, (start, self.cursor)))
            }
            Some(b'(') => {
                self.bump();
                Ok(Token::new("(", TokenType::LParen, (start, self.cursor)))
            }
            Some(b')') => {
                self.bump();
                Ok(Token::new(")", TokenType::RParen, (start, self.cursor)))
            }
            Some(b'{') => {
                self.bump();
                Ok(Token::new("{", TokenType::LBrace, (start, self.cursor)))
            }
            Some(b'}') => {
                self.bump();
                Ok(Token::new("}", TokenType::RBrace, (start, self.cursor)))
            }
            Some(b'[') => {
                self.bump();
                Ok(Token::new("[", TokenType::LBrack, (start, self.cursor)))
            }
            Some(b']') => {
                self.bump();
                Ok(Token::new("]", TokenType::RBrack, (start, self.cursor)))
            }
            Some(b',') => {
                self.bump();
                Ok(Token::new(",", TokenType::Comma, (start, self.cursor)))
            }
            Some(b'.') => {
                self.bump();
                Ok(Token::new(".", TokenType::Dot, (start, self.cursor)))
            }
            Some(b':') => {
                self.bump();
                Ok(Token::new(":", TokenType::DoubleDots, (start, self.cursor)))
            }
            Some(b';') => {
                self.bump();
                Ok(Token::new(";", TokenType::Semicolon, (start, self.cursor)))
            }
            Some(c) => {
                self.bump();
                Err(LexerError::UnknownCharacter {
                    character: c as char,
                    src: NamedSource::new(self.filename, self.src.to_string()),
                    span: (start, 1).into(),
                })
            }
        }
    }
}

impl<'s> Iterator for Lexer<'s> {
    type Item = Result<Token<'s>, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(token) if token.token_type == TokenType::EOF => None,
            Ok(token) => Some(Ok(token)),
            Err(e) => Some(Err(e)),
        }
    }
}
