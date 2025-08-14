macro_rules! std_symbol {
    ($ch: literal, $typ: expr) => {
        ($ch, Token::new(stringify!($ch).trim_matches('\''), $typ, (0, 0)))
    };
}

macro_rules! std_keyword {
    ($name: literal) => {
        (
            $name.to_string(),
            Token::new($name, TokenType::Keyword, (0, 0)),
        )
    };
}

macro_rules! std_type {
    ($name: literal) => {
        (
            $name.to_string(),
            Token::new($name, TokenType::Type, (0, 0)),
        )
    };
}

macro_rules! std_token {
    ($name: literal, $value: expr) => {
        (
            $name.to_string(),
            Token::new($name, $value, (0, 0)),
        )
    };
}

pub(crate) use std_keyword;
pub(crate) use std_symbol;
pub(crate) use std_token;
pub(crate) use std_type;
