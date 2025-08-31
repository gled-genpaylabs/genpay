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
