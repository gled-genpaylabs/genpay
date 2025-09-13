#[derive(Debug, Clone, Copy, PartialEq)]
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

#[cfg(test)]
mod tests {
    use crate::{expressions::Expressions, statements::Statements, Parser};
    use genpay_lexer::Lexeme as Lexer;
    use crate::value::Value;

    #[test]
    fn basic_values() {
        const SRC: &str = "123; 5.0; 'a'; \"some\"; true";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        let mut ast_iter = ast.into_iter();

        if let Some(Statements::Expression(Expressions::Value(Value::Integer(_), _))) = ast_iter.next()
        {
        } else {
            panic!("Test failure for: {:?}", ast_iter.next().unwrap())
        }

        if let Some(Statements::Expression(Expressions::Value(Value::Float(_), _))) = ast_iter.next() {
        } else {
            panic!("Test failure for: {:?}", ast_iter.nth_back(0).unwrap())
        }

        if let Some(Statements::Expression(Expressions::Value(Value::Char(_), _))) = ast_iter.next() {
        } else {
            panic!("Test failure for: {:?}", ast_iter.nth_back(0).unwrap())
        }

        if let Some(Statements::Expression(Expressions::Value(Value::String(_), _))) = ast_iter.next() {
        } else {
            panic!("Test failure for: {:?}", ast_iter.nth_back(0).unwrap())
        }

        if let Some(Statements::Expression(Expressions::Value(Value::Boolean(_), _))) = ast_iter.next()
        {
        } else {
            panic!("Test failure for: {:?}", ast_iter.nth_back(0).unwrap())
        }
    }
}
