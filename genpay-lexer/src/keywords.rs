use crate::{token::Token, token_type::TokenType};
use std::collections::HashMap;

pub fn get_keywords() -> HashMap<String, Token> {
    HashMap::from([
        // Constructions
        (
            "if".to_string(),
            Token::new("if".to_string(), TokenType::Keyword, (0, 0)),
        ),
        (
            "else".to_string(),
            Token::new("else".to_string(), TokenType::Keyword, (0, 0)),
        ),
        (
            "while".to_string(),
            Token::new("while".to_string(), TokenType::Keyword, (0, 0)),
        ),
        (
            "for".to_string(),
            Token::new("for".to_string(), TokenType::Keyword, (0, 0)),
        ),
        (
            "break".to_string(),
            Token::new("break".to_string(), TokenType::Keyword, (0, 0)),
        ),
        // Tech
        (
            "let".to_string(),
            Token::new("let".to_string(), TokenType::Keyword, (0, 0)),
        ),
        (
            "pub".to_string(),
            Token::new("pub".to_string(), TokenType::Keyword, (0, 0)),
        ),
        (
            "fn".to_string(),
            Token::new("fn".to_string(), TokenType::Keyword, (0, 0)),
        ),
        (
            "import".to_string(),
            Token::new("import".to_string(), TokenType::Keyword, (0, 0)),
        ),
        (
            "include".to_string(),
            Token::new("include".to_string(), TokenType::Keyword, (0, 0)),
        ),
        (
            "extern".to_string(),
            Token::new("extern".to_string(), TokenType::Keyword, (0, 0)),
        ),
        (
            "return".to_string(),
            Token::new("return".to_string(), TokenType::Keyword, (0, 0)),
        ),
        (
            "struct".to_string(),
            Token::new("struct".to_string(), TokenType::Keyword, (0, 0)),
        ),
        (
            "enum".to_string(),
            Token::new("enum".to_string(), TokenType::Keyword, (0, 0)),
        ),
        (
            "typedef".to_string(),
            Token::new("typedef".to_string(), TokenType::Keyword, (0, 0)),
        ),
        (
            "_extern_declare".to_string(),
            Token::new("_extern_declare".to_string(), TokenType::Keyword, (0, 0)),
        ),
        (
            "_link_c".to_string(),
            Token::new("_link_c".to_string(), TokenType::Keyword, (0, 0)),
        ),
        // Types
        (
            "i8".to_string(),
            Token::new("i8".to_string(), TokenType::Type, (0, 0)),
        ),
        (
            "i16".to_string(),
            Token::new("i16".to_string(), TokenType::Type, (0, 0)),
        ),
        (
            "i32".to_string(),
            Token::new("i32".to_string(), TokenType::Type, (0, 0)),
        ),
        (
            "i64".to_string(),
            Token::new("i64".to_string(), TokenType::Type, (0, 0)),
        ),
        (
            "u8".to_string(),
            Token::new("u8".to_string(), TokenType::Type, (0, 0)),
        ),
        (
            "u16".to_string(),
            Token::new("u16".to_string(), TokenType::Type, (0, 0)),
        ),
        (
            "u32".to_string(),
            Token::new("u32".to_string(), TokenType::Type, (0, 0)),
        ),
        (
            "u64".to_string(),
            Token::new("u64".to_string(), TokenType::Type, (0, 0)),
        ),
        (
            "usize".to_string(),
            Token::new("usize".to_string(), TokenType::Type, (0, 0)),
        ),
        (
            "f32".to_string(),
            Token::new("f32".to_string(), TokenType::Type, (0, 0)),
        ),
        (
            "f64".to_string(),
            Token::new("f64".to_string(), TokenType::Type, (0, 0)),
        ),
        (
            "bool".to_string(),
            Token::new("bool".to_string(), TokenType::Type, (0, 0)),
        ),
        (
            "char".to_string(),
            Token::new("char".to_string(), TokenType::Type, (0, 0)),
        ),
        (
            "void".to_string(),
            Token::new("void".to_string(), TokenType::Type, (0, 0)),
        ),
        // Values
        (
            "true".to_string(),
            Token::new("true".to_string(), TokenType::Boolean, (0, 0)),
        ),
        (
            "false".to_string(),
            Token::new("false".to_string(), TokenType::Boolean, (0, 0)),
        ),
        (
            "NULL".to_string(),
            Token::new("NULL".to_string(), TokenType::Null, (0, 0)),
        ),
    ])
}
