#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexerError {
    InvalidNumberConstant {
        const_type: String,
        span: (usize, usize),
    },

    ConstantParserError {
        const_type: String,
        span: (usize, usize),
    },

    UnknownCharacterEscape {
        escape: String,
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
