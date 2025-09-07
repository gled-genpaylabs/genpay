use genpay_lexer::{Lexer, token::Token, token_type::TokenType};

#[test]
fn binary_symbols() {
    let mut lexer = Lexer::new("+-*/", "test.pay");
    let tokens = lexer.tokenize().unwrap().0;

    assert_eq!(
        tokens,
        vec![
            Token::new(String::from("+"), TokenType::Plus, (0, 1)),
            Token::new(String::from("-"), TokenType::Minus, (1, 2)),
            Token::new(String::from("*"), TokenType::Multiply, (2, 3)),
            Token::new(String::from("/"), TokenType::Divide, (3, 4)),
            Token::new(String::new(), TokenType::EOF, (0, 0))
        ]
    )
}

#[test]
fn boolean_symbols() {
    let mut lexer = Lexer::new("> < ! && || == !=", "test.pay");
    let tokens = lexer.tokenize().unwrap().0;

    assert_eq!(
        tokens,
        vec![
            Token::new(String::from(">"), TokenType::Bt, (0, 0)),
            Token::new(String::from("<"), TokenType::Lt, (2, 2)),
            Token::new(String::from("!"), TokenType::Not, (4, 4)),
            Token::new(String::from("&&"), TokenType::And, (6, 8)),
            Token::new(String::from("||"), TokenType::Or, (9, 11)),
            Token::new(String::from("=="), TokenType::Eq, (12, 13)),
            Token::new(String::from("!="), TokenType::Ne, (15, 16)),
            Token::new(String::new(), TokenType::EOF, (0, 0))
        ]
    )
}

#[test]
fn bitwise_symbols() {
    let mut lexer = Lexer::new(">> << ^", "test.pay");
    let tokens = lexer.tokenize().unwrap().0;

    assert_eq!(
        tokens,
        vec![
            Token::new(String::from(">>"), TokenType::RShift, (0, 2)),
            Token::new(String::from("<<"), TokenType::LShift, (3, 5)),
            Token::new(String::from("^"), TokenType::Xor, (6, 7)),
            Token::new(String::new(), TokenType::EOF, (0, 0))
        ]
    )
}

#[test]
fn parentheses_symbols() {
    let mut lexer = Lexer::new("(){}[]", "test.pay");
    let tokens = lexer.tokenize().unwrap().0;

    assert_eq!(
        tokens,
        vec![
            Token::new(String::from("("), TokenType::LParen, (0, 1)),
            Token::new(String::from(")"), TokenType::RParen, (1, 2)),
            Token::new(String::from("{"), TokenType::LBrace, (2, 3)),
            Token::new(String::from("}"), TokenType::RBrace, (3, 4)),
            Token::new(String::from("["), TokenType::LBrack, (4, 5)),
            Token::new(String::from("]"), TokenType::RBrack, (5, 6)),
            Token::new(String::new(), TokenType::EOF, (0, 0))
        ]
    )
}

#[test]
fn other_symbols() {
    let mut lexer = Lexer::new("&ref : ; & | _ . , =", "test.pay");
    let tokens = lexer.tokenize().unwrap().0;

    assert_eq!(
        tokens,
        vec![
            Token::new(String::from("&"), TokenType::Ref, (0, 1)),
            Token::new(String::from("ref"), TokenType::Identifier, (1, 4)),
            Token::new(String::from(":"), TokenType::DoubleDots, (5, 6)),
            Token::new(String::from(";"), TokenType::Semicolon, (7, 8)),
            Token::new(String::from("&"), TokenType::Ampersand, (9, 10)),
            Token::new(String::from("|"), TokenType::Verbar, (11, 12)),
            Token::new(String::from("_"), TokenType::Identifier, (13, 14)),
            Token::new(String::from("."), TokenType::Dot, (15, 16)),
            Token::new(String::from(","), TokenType::Comma, (17, 18)),
            Token::new(String::from("="), TokenType::Equal, (19, 20)),
            Token::new(String::new(), TokenType::EOF, (0, 0))
        ]
    )
}
