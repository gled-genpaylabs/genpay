use genpay_lexer::Lexer;
use genpay_parser::{
    error::ParserError, expressions::Expressions, statements::Statements, value::Value, Parser,
};

fn new_parser<'s>(src: &'s str, filename: &'s str) -> Parser<'s> {
    let lexer = Lexer::new(src, filename);
    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    for result in lexer {
        match result {
            Ok(token) => tokens.push(token),
            Err(err) => errors.push(err.into()),
        }
    }
    Parser::new(tokens, errors, src, filename)
}

#[test]
fn basic_values() {
    const SRC: &str = "123; 5.0; 'a'; \"some\"; true";
    const FILENAME: &str = "test.dn";

    let mut parser = new_parser(SRC, FILENAME);
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
