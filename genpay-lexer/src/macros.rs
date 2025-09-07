macro_rules! std_symbol {
    ($ch: literal, $typ: expr) => {
        ($ch, Token::new(String::from($ch), $typ, (0, 0)))
    };
}

pub(crate) use std_symbol;
