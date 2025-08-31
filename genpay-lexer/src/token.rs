use crate::token_type::TokenType;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token<'s> {
    pub value: &'s str,
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
