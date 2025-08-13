use genpay_lexer::{token::Token, token_type::TokenType, Lexer};

fn assert_tokens(lexer: &mut Lexer, expected: &[Token]) {
    for expected_token in expected {
        let token = lexer.next();
        assert_eq!(token.token_type, expected_token.token_type);
        assert_eq!(token.value, expected_token.value);
    }
}

#[test]
fn basic_string() {
    let mut lexer = Lexer::new("\"hello\"");
    let expected = vec![
        Token::new("hello", TokenType::String, (0, 7)),
        Token::new("", TokenType::EOF, (7, 7)),
    ];
    assert_tokens(&mut lexer, &expected);
}

#[test]
fn big_string() {
    let mut lexer = Lexer::new(
        "\"Hello, World! Here's an interesting thing: first LLVM initial release was in 2003 year. The original authors of core was Chris Lattner and Vikram Adve\"",
    );
    let expected = vec![
        Token::new(
            "Hello, World! Here's an interesting thing: first LLVM initial release was in 2003 year. The original authors of core was Chris Lattner and Vikram Adve",
            TokenType::String,
            (0, 152),
        ),
        Token::new("", TokenType::EOF, (152, 152)),
    ];
    assert_tokens(&mut lexer, &expected);
}

#[test]
fn advanced_string() {
    let mut lexer = Lexer::new("\"¿?👉👈🤠👀\"");
    let expected = vec![
        Token::new("¿?👉👈🤠👀", TokenType::String, (0, 21)),
        Token::new("", TokenType::EOF, (21, 21)),
    ];
    assert_tokens(&mut lexer, &expected);
}

#[test]
fn basic_char() {
    let mut lexer = Lexer::new("'a'");
    let expected = vec![
        Token::new("a", TokenType::Char, (0, 3)),
        Token::new("", TokenType::EOF, (3, 3)),
    ];
    assert_tokens(&mut lexer, &expected);
}

#[test]
fn advanced_char() {
    let mut lexer = Lexer::new("'👀'");
    let expected = vec![
        Token::new("👀", TokenType::Char, (0, 5)),
        Token::new("", TokenType::EOF, (5, 5)),
    ];
    assert_tokens(&mut lexer, &expected);
}
