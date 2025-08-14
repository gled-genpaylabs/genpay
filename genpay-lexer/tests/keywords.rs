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
fn basic_types() {
    let input = "i8 i16 i32 i64 u8 u16 u32 u64 usize char bool void";
    let mut lexer = Lexer::new(input, "test.gen");

    let expected = vec![
        Token::new("i8", TokenType::Type, (0, 2)),
        Token::new("i16", TokenType::Type, (3, 6)),
        Token::new("i32", TokenType::Type, (7, 10)),
        Token::new("i64", TokenType::Type, (11, 14)),
        Token::new("u8", TokenType::Type, (15, 17)),
        Token::new("u16", TokenType::Type, (18, 21)),
        Token::new("u32", TokenType::Type, (22, 25)),
        Token::new("u64", TokenType::Type, (26, 29)),
        Token::new("usize", TokenType::Type, (30, 35)),
        Token::new("char", TokenType::Type, (36, 40)),
        Token::new("bool", TokenType::Type, (41, 45)),
        Token::new("void", TokenType::Type, (46, 50)),
    ];

    assert_tokens(&mut lexer, &expected);
}

#[test]
fn boolean_keywords() {
    let input = "true false";
    let mut lexer = Lexer::new(input, "test.gen");

    let expected = vec![
        Token::new("true", TokenType::Boolean, (0, 4)),
        Token::new("false", TokenType::Boolean, (5, 10)),
    ];

    assert_tokens(&mut lexer, &expected);
}

#[test]
fn main_keywords() {
    let input = "let fn import return struct enum typedef";
    let mut lexer = Lexer::new(input, "test.gen");

    let expected = vec![
        Token::new("let", TokenType::Keyword, (0, 3)),
        Token::new("fn", TokenType::Keyword, (4, 6)),
        Token::new("import", TokenType::Keyword, (7, 13)),
        Token::new("return", TokenType::Keyword, (14, 20)),
        Token::new("struct", TokenType::Keyword, (21, 27)),
        Token::new("enum", TokenType::Keyword, (28, 32)),
        Token::new("typedef", TokenType::Keyword, (33, 40)),
    ];

    assert_tokens(&mut lexer, &expected);
}

#[test]
fn constructions_keywords() {
    let input = "if else while for break";
    let mut lexer = Lexer::new(input, "test.gen");

    let expected = vec![
        Token::new("if", TokenType::Keyword, (0, 2)),
        Token::new("else", TokenType::Keyword, (3, 7)),
        Token::new("while", TokenType::Keyword, (8, 13)),
        Token::new("for", TokenType::Keyword, (14, 17)),
        Token::new("break", TokenType::Keyword, (18, 23)),
    ];

    assert_tokens(&mut lexer, &expected);
}
