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
fn basic_string() {
    let mut lexer = Lexer::new("\"hello\"", "test.genpay");
    let expected = vec![
        Token::new("hello", TokenType::String, (0, 7)),
    ];
    assert_tokens(&mut lexer, &expected);
}

#[test]
fn big_string() {
    let mut lexer = Lexer::new(
        "\"Hello, World! Here's an interesting thing: first LLVM initial release was in 2003 year. The original authors of core was Chris Lattner and Vikram Adve\"", "test.genpay"
    );
    let expected = vec![
        Token::new(
            "Hello, World! Here's an interesting thing: first LLVM initial release was in 2003 year. The original authors of core was Chris Lattner and Vikram Adve",
            TokenType::String,
            (0, 152),
        ),
    ];
    assert_tokens(&mut lexer, &expected);
}

#[test]
fn advanced_string() {
    let mut lexer = Lexer::new("\"¿?👉👈🤠👀\"", "test.genpay");
    let expected = vec![
        Token::new("¿?👉👈🤠👀", TokenType::String, (0, 21)),
    ];
    assert_tokens(&mut lexer, &expected);
}

#[test]
fn basic_char() {
    let mut lexer = Lexer::new("'a'", "test.genpay");
    let expected = vec![
        Token::new("a", TokenType::Char, (0, 3)),
    ];
    assert_tokens(&mut lexer, &expected);
}

#[test]
fn advanced_char() {
    let mut lexer = Lexer::new("'👀'", "test.genpay");
    let expected = vec![
        Token::new("👀", TokenType::Char, (0, 5)),
    ];
    assert_tokens(&mut lexer, &expected);
}
