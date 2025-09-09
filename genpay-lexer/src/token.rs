use crate::token_type::TokenType;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Token<'a> {
    pub value: &'a str,
    pub token_type: TokenType,
    pub span: (usize, usize),
}

impl<'a> Token<'a> {
    pub fn new(value: &'a str, token_type: TokenType, span: (usize, usize)) -> Self {
        Self {
            value,
            token_type,
            span,
        }
    }
}
