//! # Values
//! **Values** here is the simplest basic units. They contains constants, identifiers, keywords and etc.<br/>
//! Values is always an internal elemenets of expressions.

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'s> {
    Integer(i64),
    Float(f64),
    String(&'s str),
    Char(char),
    Boolean(bool),
    Identifier(&'s str),
    Keyword(&'s str),
    Null,
    Void,
}
