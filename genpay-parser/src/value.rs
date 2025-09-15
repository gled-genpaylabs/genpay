#[derive(Debug, Clone, PartialEq)]
pub enum Value<'bump> {
    Integer(i64),
    Float(f64),
    String(&'bump str),
    Char(char),
    Boolean(bool),
    Identifier(&'bump str),
    Keyword(&'bump str),
    Null,
    Void,
}
