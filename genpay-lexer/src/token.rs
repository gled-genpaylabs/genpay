//! ## Token
//! **Token** - a structure that contains information of current lexeme: value, token_type and
//! position in source code.
//!
//! ## Example:
//! ```rust
//! use genpay_lexer::{token::Token, token_type::TokenType};
//!
//! let number = Token::new("123", TokenType::Number, (0, 3));
//!
//! assert!(number.value == "123");
//! assert!(number.token_type == TokenType::Number);
//! assert!(number.span == (0, 3));
//! ```

use crate::token_type::TokenType;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token<'s> {
    pub value: &' static str,
    pub token_type: TokenType,
    pub span: (usize, usize),
}

impl<'s> Token<'s> {
    pub fn new(value: &'s str, token_type: TokenType, span: (usize, usize)) -> Self {
        Self {
            value,
            token_type,
            span,
        }
    }
}
