#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
    Integer(i64),
    Float(f64),
    String(&'a str),
    Char(char),
    Boolean(bool),
    Identifier(&'a str),
    Keyword(&'a str),
    Null,
    Void,
}
