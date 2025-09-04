use crate::{
    error::LexerError,
    token::Token,
    token_type::TokenType,
};
use miette::NamedSource;
use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut map = HashMap::new();
        map.insert("let", TokenType::Keyword);
        map.insert("pub", TokenType::Keyword);
        map.insert("fn", TokenType::Keyword);
        map.insert("import", TokenType::Keyword);
        map.insert("include", TokenType::Keyword);
        map.insert("extern", TokenType::Keyword);
        map.insert("return", TokenType::Keyword);
        map.insert("struct", TokenType::Keyword);
        map.insert("enum", TokenType::Keyword);
        map.insert("typedef", TokenType::Keyword);
        map.insert("if", TokenType::Keyword);
        map.insert("else", TokenType::Keyword);
        map.insert("while", TokenType::Keyword);
        map.insert("for", TokenType::Keyword);
        map.insert("break", TokenType::Keyword);
        map.insert("true", TokenType::Boolean);
        map.insert("false", TokenType::Boolean);
        map.insert("NULL", TokenType::Null);
        map.insert("i8", TokenType::Type);
        map.insert("i16", TokenType::Type);
        map.insert("i32", TokenType::Type);
        map.insert("i64", TokenType::Type);
        map.insert("u8", TokenType::Type);
        map.insert("u16", TokenType::Type);
        map.insert("u32", TokenType::Type);
        map.insert("u64", TokenType::Type);
        map.insert("usize", TokenType::Type);
        map.insert("f32", TokenType::Type);
        map.insert("f64", TokenType::Type);
        map.insert("bool", TokenType::Type);
        map.insert("char", TokenType::Type);
        map.insert("void", TokenType::Type);
        map
    };
}


fn is_escape_sequence(b: u8) -> bool {
    matches!(b, b'n' | b't' | b'r' | b'\\' | b'\'' | b'"' | b'0')
}

/// Error Handling Module
pub mod error;
/// Token Object and Implementations
pub mod token;
/// Token Types Enumeration
pub mod token_type;

/// Main lexical analyzer instance
///
/// ### Usage example:
pub struct Lexer<'s> {
    src: &'s str,
    filename: &'s str,
    cursor: usize,
}

impl<'s> Lexer<'s> {
    pub fn new(src: &'s str, filename: &'s str) -> Self {
        Lexer {
            src,
            filename,
            cursor: 0,
        }
    }

    /* helpers ----------------------------------------------------------- */
    fn peek(&self) -> Option<u8> {
        self.src.as_bytes().get(self.cursor).copied()
    }
    fn peek_next(&self) -> Option<u8> {
        self.src.as_bytes().get(self.cursor + 1).copied()
    }
    fn bump(&mut self) {
        self.cursor += 1;
    }
    fn slice(&self, start: usize) -> &'s str {
        &self.src[start..self.cursor]
    }
    fn skip_ws(&mut self) {
        while let Some(b) = self.peek() {
            if b.is_ascii_whitespace() {
                self.bump();
            } else {
                break;
            }
        }
    }
    fn eat_while<F>(&mut self, mut f: F) -> &'s str
    where
        F: FnMut(u8) -> bool,
    {
        let start = self.cursor;
        while let Some(b) = self.peek() {
            if f(b) {
                self.bump();
            } else {
                break;
            }
        }
        self.slice(start)
    }

    /* next token -------------------------------------------------------- */
    pub fn next_token(&mut self) -> Result<Token<'s>, LexerError> {
        self.skip_ws();
        let start = self.cursor;
        match self.peek() {
            None => Ok(Token::new("", TokenType::EOF, (start, start))),
            Some(b'a'..=b'z' | b'A'..=b'Z' | b'_') => {
                let ident = self.eat_while(|b| b.is_ascii_alphanumeric() || b == b'_');
                let token_type = KEYWORDS.get(ident).copied().unwrap_or(TokenType::Identifier);
                Ok(Token::new(ident, token_type, (start, self.cursor)))
            }
            Some(b'0'..=b'9') => {
                // Handle hex and binary first.
                if self.src.as_bytes()[self.cursor] == b'0' {
                    if self.peek_next() == Some(b'x') {
                        self.bump(); // 0
                        self.bump(); // x
                        self.eat_while(|b| b.is_ascii_hexdigit());
                        return Ok(Token::new(self.slice(start), TokenType::Number, (start, self.cursor)));
                    }
                    if self.peek_next() == Some(b'b') {
                        self.bump(); // 0
                        self.bump(); // b
                        self.eat_while(|b| b == b'0' || b == b'1');
                        return Ok(Token::new(self.slice(start), TokenType::Number, (start, self.cursor)));
                    }
                }

                // Now decimal numbers (int or float)
                self.eat_while(|b| b.is_ascii_digit());
                let mut is_float = false;

                if self.peek() == Some(b'.') {
                    // check that it's not `.` followed by a non-digit
                    if self.src.as_bytes().get(self.cursor + 1).map_or(false, |c| c.is_ascii_digit()) {
                        is_float = true;
                        self.bump(); // consume '.'
                        self.eat_while(|b| b.is_ascii_digit());
                    }
                }

                if self.peek() == Some(b'e') || self.peek() == Some(b'E') {
                    is_float = true;
                    self.bump(); // consume 'e' or 'E'
                    if self.peek() == Some(b'+') || self.peek() == Some(b'-') {
                        self.bump(); // consume sign
                    }
                    let exponent_digits = self.eat_while(|b| b.is_ascii_digit());
                    if exponent_digits.is_empty() {
                        return Err(LexerError::InvalidNumberFormat {
                            message: "Missing exponent digits".to_string(),
                            src: NamedSource::new(self.filename, self.src.to_string()),
                            span: (self.cursor, 1).into(),
                        });
                    }
                }

                let token_type = if is_float { TokenType::FloatNumber } else { TokenType::Number };
                Ok(Token::new(self.slice(start), token_type, (start, self.cursor)))
            }
            Some(b'"') => {
                self.bump(); // opening quote
                let content_start = self.cursor;
                loop {
                    match self.peek() {
                        Some(b'\\') => {
                            self.bump(); // consume '\'
                            if let Some(b) = self.peek() {
                                if is_escape_sequence(b) {
                                    self.bump();
                                } else {
                                    return Err(LexerError::UnknownCharacterEscape {
                                        escape: (b as char).to_string(),
                                        src: NamedSource::new(self.filename, self.src.to_string()),
                                        span: (self.cursor, 1).into(),
                                    });
                                }
                            } else {
                                return Err(LexerError::UnterminatedString {
                                    src: NamedSource::new(self.filename, self.src.to_string()),
                                    span: (start, self.cursor - start).into(),
                                });
                            }
                        }
                        Some(b'"') => {
                            let content = &self.src[content_start..self.cursor];
                            self.bump(); // closing quote
                            return Ok(Token::new(content, TokenType::String, (start, self.cursor)));
                        }
                        Some(_) => {
                            self.bump();
                        }
                        None => {
                            return Err(LexerError::UnterminatedString {
                                src: NamedSource::new(self.filename, self.src.to_string()),
                                span: (start, self.cursor - start).into(),
                            });
                        }
                    }
                }
            }
            Some(b'\'') => {
                self.bump(); // opening quote
                let content_start = self.cursor;
                loop {
                    match self.peek() {
                        Some(b'\\') => {
                            self.bump(); // consume '\'
                            if let Some(b) = self.peek() {
                                if is_escape_sequence(b) {
                                    self.bump();
                                } else {
                                    return Err(LexerError::UnknownCharacterEscape {
                                        escape: (b as char).to_string(),
                                        src: NamedSource::new(self.filename, self.src.to_string()),
                                        span: (self.cursor, 1).into(),
                                    });
                                }
                            } else {
                                return Err(LexerError::UnterminatedString {
                                    src: NamedSource::new(self.filename, self.src.to_string()),
                                    span: (start, self.cursor - start).into(),
                                });
                            }
                        }
                        Some(b'\'') => {
                            let content = &self.src[content_start..self.cursor];
                            self.bump(); // closing quote
                            return Ok(Token::new(content, TokenType::Char, (start, self.cursor)));
                        }
                        Some(_) => {
                            self.bump();
                        }
                        None => {
                            return Err(LexerError::UnterminatedString {
                                src: NamedSource::new(self.filename, self.src.to_string()),
                                span: (start, self.cursor - start).into(),
                            });
                        }
                    }
                }
            }
            Some(b'+') => {
                self.bump();
                Ok(Token::new("+", TokenType::Plus, (start, self.cursor)))
            }
            Some(b'-') => {
                self.bump();
                Ok(Token::new("-", TokenType::Minus, (start, self.cursor)))
            }
            Some(b'*') => {
                self.bump();
                Ok(Token::new("*", TokenType::Multiply, (start, self.cursor)))
            }
            Some(b'/') => {
                self.bump();
                if self.peek() == Some(b'/') {
                    // Single line comment
                    self.eat_while(|b| b != b'\n');
                    self.next_token()
                } else if self.peek() == Some(b'*') {
                    // Block comment
                    self.bump(); // consume '*'
                    let mut depth = 1;
                    while depth > 0 {
                        match self.peek() {
                            Some(b'*') => {
                                self.bump();
                                if self.peek() == Some(b'/') {
                                    self.bump();
                                    depth -= 1;
                                }
                            }
                            Some(b'/') => {
                                self.bump();
                                if self.peek() == Some(b'*') {
                                    self.bump();
                                    depth += 1;
                                }
                            }
                            Some(_) => {
                                self.bump();
                            }
                            None => {
                                return Err(LexerError::UnterminatedBlockComment {
                                    src: NamedSource::new(self.filename, self.src.to_string()),
                                    span: (start, self.cursor - start).into(),
                                });
                            }
                        }
                    }
                    self.next_token()
                } else {
                    Ok(Token::new("/", TokenType::Divide, (start, self.cursor)))
                }
            }
            Some(b'%') => {
                self.bump();
                Ok(Token::new("%", TokenType::Modulus, (start, self.cursor)))
            }
            Some(b'=') => {
                self.bump();
                if self.peek() == Some(b'=') {
                    self.bump();
                    Ok(Token::new("==", TokenType::Eq, (start, self.cursor)))
                } else {
                    Ok(Token::new("=", TokenType::Equal, (start, self.cursor)))
                }
            }
            Some(b'!') => {
                self.bump();
                if self.peek() == Some(b'=') {
                    self.bump();
                    Ok(Token::new("!=", TokenType::Ne, (start, self.cursor)))
                } else {
                    Ok(Token::new("!", TokenType::Not, (start, self.cursor)))
                }
            }
            Some(b'<') => {
                self.bump();
                if self.peek() == Some(b'=') {
                    self.bump();
                    Ok(Token::new("<=", TokenType::Leq, (start, self.cursor)))
                } else if self.peek() == Some(b'<') {
                    self.bump();
                    Ok(Token::new("<<", TokenType::LShift, (start, self.cursor)))
                } else {
                    Ok(Token::new("<", TokenType::Lt, (start, self.cursor)))
                }
            }
            Some(b'>') => {
                self.bump();
                if self.peek() == Some(b'=') {
                    self.bump();
                    Ok(Token::new(">=", TokenType::Beq, (start, self.cursor)))
                } else if self.peek() == Some(b'>') {
                    self.bump();
                    Ok(Token::new(">>", TokenType::RShift, (start, self.cursor)))
                } else {
                    Ok(Token::new(">", TokenType::Bt, (start, self.cursor)))
                }
            }
            Some(b'&') => {
                self.bump();
                if self.peek() == Some(b'&') {
                    self.bump();
                    Ok(Token::new("&&", TokenType::And, (start, self.cursor)))
                } else {
                    Ok(Token::new("&", TokenType::Ampersand, (start, self.cursor)))
                }
            }
            Some(b'|') => {
                self.bump();
                if self.peek() == Some(b'|') {
                    self.bump();
                    Ok(Token::new("||", TokenType::Or, (start, self.cursor)))
                } else {
                    Ok(Token::new("|", TokenType::Verbar, (start, self.cursor)))
                }
            }
            Some(b'^') => {
                self.bump();
                Ok(Token::new("^", TokenType::Xor, (start, self.cursor)))
            }
            Some(b'(') => {
                self.bump();
                Ok(Token::new("(", TokenType::LParen, (start, self.cursor)))
            }
            Some(b')') => {
                self.bump();
                Ok(Token::new(")", TokenType::RParen, (start, self.cursor)))
            }
            Some(b'{') => {
                self.bump();
                Ok(Token::new("{", TokenType::LBrace, (start, self.cursor)))
            }
            Some(b'}') => {
                self.bump();
                Ok(Token::new("}", TokenType::RBrace, (start, self.cursor)))
            }
            Some(b'[') => {
                self.bump();
                Ok(Token::new("[", TokenType::LBrack, (start, self.cursor)))
            }
            Some(b']') => {
                self.bump();
                Ok(Token::new("]", TokenType::RBrack, (start, self.cursor)))
            }
            Some(b',') => {
                self.bump();
                Ok(Token::new(",", TokenType::Comma, (start, self.cursor)))
            }
            Some(b'.') => {
                self.bump();
                Ok(Token::new(".", TokenType::Dot, (start, self.cursor)))
            }
            Some(b':') => {
                self.bump();
                Ok(Token::new(":", TokenType::DoubleDots, (start, self.cursor)))
            }
            Some(b';') => {
                self.bump();
                Ok(Token::new(";", TokenType::Semicolon, (start, self.cursor)))
            }
            Some(c) => {
                self.bump();
                let context_start = start.saturating_sub(20);
                let context_end = (start + 20).min(self.src.len());
                let context = self.src[context_start..context_end].to_string();
                Err(LexerError::UnknownCharacter {
                    character: c as char,
                    context,
                    src: NamedSource::new(self.filename, self.src.to_string()),
                    span: (start, 1).into(),
                })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{token::Token, token_type::TokenType, Lexer, error::LexerError};

    fn assert_tokens(lexer: &mut Lexer, expected: &[Token]) {
        for expected_token in expected {
            let token = lexer.next().unwrap().unwrap();
            assert_eq!(token.token_type, expected_token.token_type);
            assert_eq!(token.value, expected_token.value);
        }
        assert!(lexer.next().is_none());
    }

    // from keywords.rs
    #[test]
    fn basic_types() {
        let input = "i8 i16 i32 i64 u8 u16 u32 u64 usize char bool void";
        let mut lexer = Lexer::new(input, "test.genpay");

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
        let mut lexer = Lexer::new(input, "test.genpay");

        let expected = vec![
            Token::new("true", TokenType::Boolean, (0, 4)),
            Token::new("false", TokenType::Boolean, (5, 10)),
        ];

        assert_tokens(&mut lexer, &expected);
    }

    #[test]
    fn main_keywords() {
        let input = "let fn import return struct enum typedef";
        let mut lexer = Lexer::new(input, "test.genpay");

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
        let mut lexer = Lexer::new(input, "test.genpay");

        let expected = vec![
            Token::new("if", TokenType::Keyword, (0, 2)),
            Token::new("else", TokenType::Keyword, (3, 7)),
            Token::new("while", TokenType::Keyword, (8, 13)),
            Token::new("for", TokenType::Keyword, (14, 17)),
            Token::new("break", TokenType::Keyword, (18, 23)),
        ];

        assert_tokens(&mut lexer, &expected);
    }

    // from numbers.rs
    #[test]
    fn basic_number() {
        let mut lexer = Lexer::new("123", "test.genpay");
        let expected = vec![
            Token::new("123", TokenType::Number, (0, 3)),
        ];
        assert_tokens(&mut lexer, &expected);
    }

    #[test]
    fn big_number() {
        let mut lexer = Lexer::new("999999999999999", "test.genpay");
        let expected = vec![
            Token::new("999999999999999", TokenType::Number, (0, 15)),
        ];
        assert_tokens(&mut lexer, &expected);
    }

    #[test]
    fn negative_number() {
        let mut lexer = Lexer::new("-15", "test.genpay");
        let expected = vec![
            Token::new("-", TokenType::Minus, (0, 1)),
            Token::new("15", TokenType::Number, (1, 3)),
        ];
        assert_tokens(&mut lexer, &expected);
    }

    #[test]
    fn float_number() {
        let mut lexer = Lexer::new("1.0", "test.genpay");
        let expected = vec![
            Token::new("1.0", TokenType::FloatNumber, (0, 3)),
        ];
        assert_tokens(&mut lexer, &expected);
    }

    #[test]
    fn advanced_float_number() {
        let mut lexer = Lexer::new("1.89", "test.genpay");
        let expected = vec![
            Token::new("1.89", TokenType::FloatNumber, (0, 4)),
        ];
        assert_tokens(&mut lexer, &expected);
    }

    #[test]
    fn big_float_number() {
        let mut lexer = Lexer::new("3.141592653589793", "test.genpay");
        let expected = vec![
            Token::new("3.141592653589793", TokenType::FloatNumber, (0, 17)),
        ];
        assert_tokens(&mut lexer, &expected);
    }

    #[test]
    fn negative_float_number() {
        let mut lexer = Lexer::new("-1.89", "test.genpay");
        let expected = vec![
            Token::new("-", TokenType::Minus, (0, 1)),
            Token::new("1.89", TokenType::FloatNumber, (1, 5)),
        ];
        assert_tokens(&mut lexer, &expected);
    }

    // from strings.rs
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

    // from symbols.rs
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

    #[test]
    fn block_comment() {
        let input = "/* this is a block comment */ 123";
        let mut lexer = Lexer::new(input, "test.genpay");
        let expected = vec![
            Token::new("123", TokenType::Number, (28, 31)),
        ];
        assert_tokens(&mut lexer, &expected);
    }

    #[test]
    fn nested_block_comment() {
        let input = "/* nested /* comment */ */ 123";
        let mut lexer = Lexer::new(input, "test.genpay");
        let expected = vec![
            Token::new("123", TokenType::Number, (27, 30)),
        ];
        assert_tokens(&mut lexer, &expected);
    }

    #[test]
    fn unterminated_block_comment_error() {
        let input = "/* hello";
        let mut lexer = Lexer::new(input, "test.genpay");
        let result = lexer.next().unwrap();
        assert!(matches!(result, Err(LexerError::UnterminatedBlockComment { .. })));
    }

    #[test]
    fn exponent_numbers() {
        let input = "1e5 1.23e4 1.23e+4 1.23E-4 1e-5";
        let mut lexer = Lexer::new(input, "test.genpay");
        let expected = vec![
            Token::new("1e5", TokenType::FloatNumber, (0, 3)),
            Token::new("1.23e4", TokenType::FloatNumber, (4, 10)),
            Token::new("1.23e+4", TokenType::FloatNumber, (11, 18)),
            Token::new("1.23E-4", TokenType::FloatNumber, (19, 26)),
            Token::new("1e-5", TokenType::FloatNumber, (27, 31)),
        ];
        assert_tokens(&mut lexer, &expected);
    }

    #[test]
    fn invalid_exponent_error() {
        let input = "1e";
        let mut lexer = Lexer::new(input, "test.genpay");
        let result = lexer.next().unwrap();
        assert!(matches!(result, Err(LexerError::InvalidNumberFormat { .. })));
    }

    #[test]
    fn unterminated_string_error() {
        let input = "\"hello";
        let mut lexer = Lexer::new(input, "test.genpay");
        let result = lexer.next().unwrap();
        assert!(matches!(result, Err(LexerError::UnterminatedString { .. })));
    }

    #[test]
    fn invalid_escape_sequence_error() {
        let input = "\"\\z\"";
        let mut lexer = Lexer::new(input, "test.genpay");
        let result = lexer.next().unwrap();
        assert!(matches!(result, Err(LexerError::UnknownCharacterEscape { .. })));
    }
}

impl<'s> Iterator for Lexer<'s> {
    type Item = Result<Token<'s>, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(token) if token.token_type == TokenType::EOF => None,
            Ok(token) => Some(Ok(token)),
            Err(e) => Some(Err(e)),
        }
    }
}
