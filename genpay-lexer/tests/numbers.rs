use genpay_lexer::{token::Token, token_type::TokenType, Lexer};

fn assert_tokens(lexer: &mut Lexer, expected: &[Token]) {
    for expected_token in expected {
        let token = lexer.next().unwrap().unwrap();
        assert_eq!(token.token_type, expected_token.token_type);
        assert_eq!(token.value, expected_token.value);
    }
    assert!(lexer.next().is_none());
}

#[test]
fn basic_number() {
    let mut lexer = Lexer::new("123", "test.gen");
    let expected = vec![
        Token::new("123", TokenType::Number, (0, 3)),
    ];
    assert_tokens(&mut lexer, &expected);
}

#[test]
fn big_number() {
    let mut lexer = Lexer::new("999999999999999", "test.gen");
    let expected = vec![
        Token::new("999999999999999", TokenType::Number, (0, 15)),
    ];
    assert_tokens(&mut lexer, &expected);
}

#[test]
fn negative_number() {
    let mut lexer = Lexer::new("-15", "test.gen");
    let expected = vec![
        Token::new("-", TokenType::Minus, (0, 1)),
        Token::new("15", TokenType::Number, (1, 3)),
    ];
    assert_tokens(&mut lexer, &expected);
}

#[test]
fn float_number() {
    let mut lexer = Lexer::new("1.0", "test.gen");
    let expected = vec![
        Token::new("1.0", TokenType::FloatNumber, (0, 3)),
    ];
    assert_tokens(&mut lexer, &expected);
}

#[test]
fn advanced_float_number() {
    let mut lexer = Lexer::new("1.89", "test.gen");
    let expected = vec![
        Token::new("1.89", TokenType::FloatNumber, (0, 4)),
    ];
    assert_tokens(&mut lexer, &expected);
}

#[test]
fn big_float_number() {
    let mut lexer = Lexer::new("3.141592653589793", "test.gen");
    let expected = vec![
        Token::new("3.141592653589793", TokenType::FloatNumber, (0, 17)),
    ];
    assert_tokens(&mut lexer, &expected);
}

#[test]
fn negative_float_number() {
    let mut lexer = Lexer::new("-1.89", "test.gen");
    let expected = vec![
        Token::new("-", TokenType::Minus, (0, 1)),
        Token::new("1.89", TokenType::FloatNumber, (1, 5)),
    ];
    assert_tokens(&mut lexer, &expected);
}
