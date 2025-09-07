use genpay_lexer::{Lexer, token::Token, token_type::TokenType};

#[test]
fn basic_number() {
    let mut lexer = Lexer::new("123", "test.pay");
    let tokens = lexer.tokenize().unwrap().0;

    assert_eq!(
        tokens,
        vec![
            Token::new(String::from("123"), TokenType::Number, (0, 2)),
            Token::new(String::new(), TokenType::EOF, (0, 0))
        ]
    )
}

#[test]
fn big_number() {
    let mut lexer = Lexer::new("999999999999999", "test.pay");
    let tokens = lexer.tokenize().unwrap().0;

    assert_eq!(
        tokens,
        vec![
            Token::new(String::from("999999999999999"), TokenType::Number, (0, 14)),
            Token::new(String::new(), TokenType::EOF, (0, 0))
        ]
    )
}

#[test]
fn negative_number() {
    let mut lexer = Lexer::new("-15", "test.pay");
    let tokens = lexer.tokenize().unwrap().0;

    assert_eq!(
        tokens,
        vec![
            Token::new(String::from("-"), TokenType::Minus, (0, 1)),
            Token::new(String::from("15"), TokenType::Number, (1, 2)),
            Token::new(String::new(), TokenType::EOF, (0, 0))
        ]
    )
}

#[test]
fn float_number() {
    let mut lexer = Lexer::new("1.0", "test.pay");
    let tokens = lexer.tokenize().unwrap().0;

    assert_eq!(
        tokens,
        vec![
            Token::new(String::from("1"), TokenType::FloatNumber, (0, 2)),
            Token::new(String::new(), TokenType::EOF, (0, 0))
        ]
    )
}

#[test]
fn advanced_float_number() {
    let mut lexer = Lexer::new("1.89", "test.pay");
    let tokens = lexer.tokenize().unwrap().0;

    assert_eq!(
        tokens,
        vec![
            Token::new(String::from("1.89"), TokenType::FloatNumber, (0, 3)),
            Token::new(String::new(), TokenType::EOF, (0, 0))
        ]
    )
}

#[test]
fn big_float_number() {
    let mut lexer = Lexer::new("3.141592653589793", "test.pay");
    let tokens = lexer.tokenize().unwrap().0;

    assert_eq!(
        tokens,
        vec![
            Token::new(
                String::from("3.141592653589793"),
                TokenType::FloatNumber,
                (0, 16)
            ),
            Token::new(String::new(), TokenType::EOF, (0, 0))
        ]
    )
}

#[test]
fn negative_float_number() {
    let mut lexer = Lexer::new("-1.89", "test.pay");
    let tokens = lexer.tokenize().unwrap().0;

    assert_eq!(
        tokens,
        vec![
            Token::new(String::from("-"), TokenType::Minus, (0, 1)),
            Token::new(String::from("1.89"), TokenType::FloatNumber, (1, 4)),
            Token::new(String::new(), TokenType::EOF, (0, 0))
        ]
    )
}
