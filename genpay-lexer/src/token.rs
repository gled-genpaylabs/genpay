use crate::token_type::TokenType;
use std::borrow::Cow;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token<'a> {
    pub value: Cow<'a, str>,
    pub token_type: TokenType,
    pub span: (usize, usize),
}

impl<'a> Token<'a> {
    pub fn new(value: impl Into<Cow<'a, str>>, token_type: TokenType, span: (usize, usize)) -> Self {
        Self {
            value: value.into(),
            token_type,
            span,
        }
    }
}
