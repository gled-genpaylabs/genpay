#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexerError<'a> {
    InvalidNumberConstant {
        const_type: &'static str,
        span: (usize, usize),
    },

    ConstantParserError {
        const_type: &'static str,
        span: (usize, usize),
    },

    UnknownCharacterEscape {
        escape: &'a str,
        span: (usize, usize),
    },

    UnknownCharacter {
        character: char,
        span: (usize, usize),
    },

    UnterminatedString {
        span: (usize, usize),
    },

    UnterminatedChar {
        span: (usize, usize),
    },
}
