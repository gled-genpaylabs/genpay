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
fn binary_symbols() {
    let mut lexer = Lexer::new("+-*/", "test.genpay");
    let expected = vec![
        Token::new("+", TokenType::Plus, (0, 1)),
        Token::new("-", TokenType::Minus, (1, 2)),
        Token::new("*", TokenType::Multiply, (2, 3)),
        Token::new("/", TokenType::Divide, (3, 4)),
    ];
    assert_tokens(&mut lexer, &expected);
}

#[test]
fn boolean_symbols() {
    let mut lexer = Lexer::new("> < ! && || == !=", "test.genpay");
    let expected = vec![
        Token::new(">", TokenType::Bt, (0, 1)),
        Token::new("<", TokenType::Lt, (2, 3)),
        Token::new("!", TokenType::Not, (4, 5)),
        Token::new("&&", TokenType::And, (6, 8)),
        Token::new("||", TokenType::Or, (9, 11)),
        Token::new("==", TokenType::Eq, (12, 14)),
        Token::new("!=", TokenType::Ne, (15, 17)),
    ];
    assert_tokens(&mut lexer, &expected);
}

#[test]
fn bitwise_symbols() {
    let mut lexer = Lexer::new(">> << ^", "test.genpay");
    let expected = vec![
        Token::new(">>", TokenType::RShift, (0, 2)),
        Token::new("<<", TokenType::LShift, (3, 5)),
        Token::new("^", TokenType::Xor, (6, 7)),
    ];
    assert_tokens(&mut lexer, &expected);
}

#[test]
fn parentheses_symbols() {
    let mut lexer = Lexer::new("(){}[]", "test.genpay");
    let expected = vec![
        Token::new("(", TokenType::LParen, (0, 1)),
        Token::new(")", TokenType::RParen, (1, 2)),
        Token::new("{", TokenType::LBrace, (2, 3)),
        Token::new("}", TokenType::RBrace, (3, 4)),
        Token::new("[", TokenType::LBrack, (4, 5)),
        Token::new("]", TokenType::RBrack, (5, 6)),
    ];
    assert_tokens(&mut lexer, &expected);
}

#[test]
fn other_symbols() {
    let mut lexer = Lexer::new("& ref : ; & | _ . , =", "test.genpay");
    let expected = vec![
        Token::new("&", TokenType::Ampersand, (0, 1)),
        Token::new("ref", TokenType::Identifier, (2, 5)),
        Token::new(":", TokenType::DoubleDots, (6, 7)),
        Token::new(";", TokenType::Semicolon, (8, 9)),
        Token::new("&", TokenType::Ampersand, (10, 11)),
        Token::new("|", TokenType::Verbar, (12, 13)),
        Token::new("_", TokenType::Identifier, (14, 15)),
        Token::new(".", TokenType::Dot, (16, 17)),
        Token::new(",", TokenType::Comma, (18, 19)),
        Token::new("=", TokenType::Equal, (20, 21)),
    ];
    assert_tokens(&mut lexer, &expected);
}
