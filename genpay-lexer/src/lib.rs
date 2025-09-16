use crate::{error::LexerError, token::Token, token_type::TokenType};
use lazy_static::lazy_static;
use std::collections::HashMap;

pub mod error;
pub mod token;
pub mod token_type;

pub struct Lexer {
    source: String,
    cursor: usize,
}

impl Lexer {
    pub fn new(source: String) -> Self {
        Lexer { source, cursor: 0 }
    }

    fn is_at_end(&self) -> bool {
        self.cursor >= self.source.len()
    }

    fn peek(&self) -> Option<char> {
        self.source
            .get(self.cursor..)
            .and_then(|s| s.chars().next())
    }

    fn advance(&mut self) -> Option<char> {
        let char = self.peek();
        if let Some(c) = char {
            self.cursor += c.len_utf8();
        }
        char
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn make_token(
        &self,
        token_type: TokenType,
        start: usize,
        len: usize,
    ) -> Result<Token, LexerError> {
        let value = &self.source[start..start + len];
        Ok(Token::new(value.to_string(), token_type, (start, len)))
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) {
        while let Some(c) = self.peek() {
            if c == '\n' {
                break;
            }
            self.advance();
        }
    }

    fn read_string(&mut self) -> Result<Token, LexerError> {
        let start = self.cursor - 1; // The opening quote has already been consumed
        while let Some(c) = self.peek() {
            if c == '"' {
                break;
            }
            if c == '\\' {
                // Handle escape sequences by just skipping the next character
                self.advance();
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(LexerError::UnterminatedString {
                span: (start, self.cursor - start),
            });
        }

        self.advance(); // Consume the closing quote

        let value = &self.source[start + 1..self.cursor - 1];
        let len = self.cursor - start;
        Ok(Token::new(
            value.to_string(),
            TokenType::String,
            (start, len),
        ))
    }

    fn read_char(&mut self) -> Result<Token, LexerError> {
        let start = self.cursor - 1; // The opening quote has already been consumed

        // Handle escaped char
        if self.peek() == Some('\\') {
            self.advance();
            self.advance();
        } else {
            self.advance();
        }

        if self.peek() != Some('\'') {
            return Err(LexerError::UnterminatedChar {
                span: (start, self.cursor - start),
            });
        }

        self.advance(); // Consume closing quote

        let value = &self.source[start + 1..self.cursor - 1];
        let len = self.cursor - start;
        Ok(Token::new(value.to_string(), TokenType::Char, (start, len)))
    }

    fn read_number(&mut self, start: usize) -> Result<Token, LexerError> {
        let mut token_type = TokenType::Number;
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                self.advance();
            } else if c == '.'
                && self
                    .source
                    .chars()
                    .nth(self.cursor + 1)
                    .map_or(false, |c| c.is_ascii_digit())
            {
                token_type = TokenType::FloatNumber;
                self.advance();
            } else {
                break;
            }
        }
        let len = self.cursor - start;
        self.make_token(token_type, start, len)
    }

    fn read_identifier(&mut self, start: usize) -> Result<Token, LexerError> {
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }
        let len = self.cursor - start;
        let value = &self.source[start..start + len];
        let token_type = lookup_identifier(value);
        Ok(Token::new(value.to_string(), token_type, (start, len)))
    }
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut map = HashMap::new();
        map.insert("if", TokenType::Keyword);
        map.insert("else", TokenType::Keyword);
        map.insert("while", TokenType::Keyword);
        map.insert("for", TokenType::Keyword);
        map.insert("break", TokenType::Keyword);
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
        map.insert("_extern_declare", TokenType::Keyword);
        map.insert("_link_c", TokenType::Keyword);
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
        map.insert("true", TokenType::Boolean);
        map.insert("false", TokenType::Boolean);
        map.insert("NULL", TokenType::Null);
        map
    };
}

fn lookup_identifier(s: &str) -> TokenType {
    KEYWORDS.get(s).cloned().unwrap_or(TokenType::Identifier)
}

impl<'a> Iterator for Lexer {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        if self.is_at_end() {
            return None;
        }

        let start = self.cursor;
        let char = match self.peek() {
            Some(c) => c,
            None => return None,
        };

        let result = match char {
            '(' => {
                self.advance();
                self.make_token(TokenType::LParen, start, 1)
            }
            ')' => {
                self.advance();
                self.make_token(TokenType::RParen, start, 1)
            }
            '{' => {
                self.advance();
                self.make_token(TokenType::LBrace, start, 1)
            }
            '}' => {
                self.advance();
                self.make_token(TokenType::RBrace, start, 1)
            }
            '[' => {
                self.advance();
                self.make_token(TokenType::LBrack, start, 1)
            }
            ']' => {
                self.advance();
                self.make_token(TokenType::RBrack, start, 1)
            }
            ',' => {
                self.advance();
                self.make_token(TokenType::Comma, start, 1)
            }
            '.' => {
                self.advance();
                self.make_token(TokenType::Dot, start, 1)
            }
            ';' => {
                self.advance();
                self.make_token(TokenType::Semicolon, start, 1)
            }
            ':' => {
                self.advance();
                self.make_token(TokenType::DoubleDots, start, 1)
            }
            '+' => {
                self.advance();
                if self.match_char('=') {
                    self.make_token(TokenType::PlusAssign, start, 2)
                } else {
                    self.make_token(TokenType::Plus, start, 1)
                }
            }
            '-' => {
                self.advance();
                if self.match_char('=') {
                    self.make_token(TokenType::MinusAssign, start, 2)
                } else {
                    self.make_token(TokenType::Minus, start, 1)
                }
            }
            '*' => {
                self.advance();
                if self.match_char('=') {
                    self.make_token(TokenType::MultiplyAssign, start, 2)
                } else {
                    self.make_token(TokenType::Multiply, start, 1)
                }
            }
            '%' => {
                self.advance();
                self.make_token(TokenType::Modulus, start, 1)
            }
            '^' => {
                self.advance();
                self.make_token(TokenType::Xor, start, 1)
            }
            '|' => {
                self.advance();
                if self.match_char('|') {
                    self.make_token(TokenType::Or, start, 2)
                } else {
                    self.make_token(TokenType::Verbar, start, 1)
                }
            }
            '&' => {
                self.advance();
                if self.match_char('&') {
                    self.make_token(TokenType::And, start, 2)
                } else {
                    self.make_token(TokenType::Ampersand, start, 1)
                }
            }
            '=' => {
                self.advance();
                if self.match_char('=') {
                    self.make_token(TokenType::Eq, start, 2)
                } else if self.match_char('>') {
                    self.make_token(TokenType::Beq, start, 2)
                } else {
                    self.make_token(TokenType::Equal, start, 1)
                }
            }
            '!' => {
                self.advance();
                if self.match_char('=') {
                    self.make_token(TokenType::Ne, start, 2)
                } else {
                    self.make_token(TokenType::Not, start, 1)
                }
            }
            '<' => {
                self.advance();
                if self.match_char('=') {
                    self.make_token(TokenType::Leq, start, 2)
                } else if self.match_char('<') {
                    self.make_token(TokenType::LShift, start, 2)
                } else {
                    self.make_token(TokenType::Lt, start, 1)
                }
            }
            '>' => {
                self.advance();
                if self.match_char('=') {
                    self.make_token(TokenType::Beq, start, 2)
                } else if self.match_char('>') {
                    self.make_token(TokenType::RShift, start, 2)
                } else {
                    self.make_token(TokenType::Bt, start, 1)
                }
            }
            '/' => {
                self.advance();
                if self.match_char('/') {
                    self.skip_comment();
                    return self.next(); // Recurse to get next token
                } else if self.match_char('=') {
                    self.make_token(TokenType::DivideAssign, start, 2)
                } else {
                    self.make_token(TokenType::Divide, start, 1)
                }
            }
            '"' => {
                self.advance();
                self.read_string()
            }
            '\'' => {
                self.advance();
                self.read_char()
            }
            c if c.is_ascii_digit() => self.read_number(start),
            c if c.is_alphabetic() || c == '_' => self.read_identifier(start),
            _ => {
                self.advance();
                Err(LexerError::UnknownCharacter {
                    character: char,
                    span: (start, self.cursor - start),
                })
            }
        };

        Some(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_types() {
        let input = "i8 i16 i32 i64 u8 u16 u32 u64 usize char bool void".to_string();
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect::<Result<_, _>>().unwrap();

        let expected = vec![
            (TokenType::Type, "i8"),
            (TokenType::Type, "i16"),
            (TokenType::Type, "i32"),
            (TokenType::Type, "i64"),
            (TokenType::Type, "u8"),
            (TokenType::Type, "u16"),
            (TokenType::Type, "u32"),
            (TokenType::Type, "u64"),
            (TokenType::Type, "usize"),
            (TokenType::Type, "char"),
            (TokenType::Type, "bool"),
            (TokenType::Type, "void"),
        ];

        assert_eq!(tokens.len(), expected.len());
        for (token, (expected_type, expected_value)) in tokens.iter().zip(expected.iter()) {
            assert_eq!(token.token_type, *expected_type);
            assert_eq!(token.value, *expected_value);
        }
    }

    #[test]
    fn boolean_keywords() {
        let input = "true false".to_string();
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect::<Result<_, _>>().unwrap();

        let expected = vec![(TokenType::Boolean, "true"), (TokenType::Boolean, "false")];

        assert_eq!(tokens.len(), expected.len());
        for (token, (expected_type, expected_value)) in tokens.iter().zip(expected.iter()) {
            assert_eq!(token.token_type, *expected_type);
            assert_eq!(token.value, *expected_value);
        }
    }

    #[test]
    fn main_keywords() {
        let input = "let fn import return struct enum typedef".to_string();
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect::<Result<_, _>>().unwrap();

        let expected = vec![
            (TokenType::Keyword, "let"),
            (TokenType::Keyword, "fn"),
            (TokenType::Keyword, "import"),
            (TokenType::Keyword, "return"),
            (TokenType::Keyword, "struct"),
            (TokenType::Keyword, "enum"),
            (TokenType::Keyword, "typedef"),
        ];

        assert_eq!(tokens.len(), expected.len());
        for (token, (expected_type, expected_value)) in tokens.iter().zip(expected.iter()) {
            assert_eq!(token.token_type, *expected_type);
            assert_eq!(token.value, *expected_value);
        }
    }

    #[test]
    fn basic_number() {
        let input = "123".to_string();
        let mut lexer = Lexer::new(input);
        let token = lexer.next().unwrap().unwrap();
        assert_eq!(
            token,
            Token::new("123".to_string(), TokenType::Number, (0, 3))
        );
    }

    #[test]
    fn float_number() {
        let input = "1.23".to_string();
        let mut lexer = Lexer::new(input);
        let token = lexer.next().unwrap().unwrap();
        assert_eq!(
            token,
            Token::new("1.23".to_string(), TokenType::FloatNumber, (0, 4))
        );
    }

    #[test]
    fn basic_string() {
        let input = "\"hello\"".to_string();
        let mut lexer = Lexer::new(input);
        let token = lexer.next().unwrap().unwrap();
        assert_eq!(
            token,
            Token::new("hello".to_string(), TokenType::String, (0, 7))
        );
    }

    #[test]
    fn string_with_escapes() {
        let input = "\"hello\\\"world\"".to_string();
        let mut lexer = Lexer::new(input);
        let token = lexer.next().unwrap().unwrap();
        assert_eq!(
            token,
            Token::new("hello\\\"world".to_string(), TokenType::String, (0, 14))
        );
    }

    #[test]
    fn basic_char() {
        let input = "'a'".to_string();
        let mut lexer = Lexer::new(input);
        let token = lexer.next().unwrap().unwrap();
        assert_eq!(token, Token::new("a".to_string(), TokenType::Char, (0, 3)));
    }

    #[test]
    fn operators() {
        let input = "+ - * / % = == != < > <= >= && || ! & | ^ << >>".to_string();
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect::<Result<_, _>>().unwrap();

        // NOTE: The original test data had incorrect values for some tokens (e.g. "%.").
        // This has been corrected to match the expected output of the lexer.
        let expected_tokens = vec![
            Token::new("+".to_string(), TokenType::Plus, (0, 1)),
            Token::new("-".to_string(), TokenType::Minus, (2, 1)),
            Token::new("*".to_string(), TokenType::Multiply, (4, 1)),
            Token::new("/".to_string(), TokenType::Divide, (6, 1)),
            Token::new("%".to_string(), TokenType::Modulus, (8, 1)),
            Token::new("=".to_string(), TokenType::Equal, (10, 1)),
            Token::new("==".to_string(), TokenType::Eq, (12, 2)),
            Token::new("!=".to_string(), TokenType::Ne, (15, 2)),
            Token::new("<".to_string(), TokenType::Lt, (18, 1)),
            Token::new(">".to_string(), TokenType::Bt, (20, 1)),
            Token::new("<=".to_string(), TokenType::Leq, (22, 2)),
            Token::new(">=".to_string(), TokenType::Beq, (25, 2)),
            Token::new("&&".to_string(), TokenType::And, (28, 2)),
            Token::new("||".to_string(), TokenType::Or, (31, 2)),
            Token::new("!".to_string(), TokenType::Not, (34, 1)),
            Token::new("&".to_string(), TokenType::Ampersand, (36, 1)),
            Token::new("|".to_string(), TokenType::Verbar, (38, 1)),
            Token::new("^".to_string(), TokenType::Xor, (40, 1)),
            Token::new("<<".to_string(), TokenType::LShift, (42, 2)),
            Token::new(">>".to_string(), TokenType::RShift, (45, 2)),
        ];

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn unterminated_string() {
        let input = "\"hello".to_string();
        let mut lexer = Lexer::new(input);
        let result = lexer.next().unwrap();
        assert!(matches!(result, Err(LexerError::UnterminatedString { .. })));
    }

    #[test]
    fn comment() {
        let input = "// this is a comment\nlet x = 1;".to_string();
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect::<Result<_, _>>().unwrap();

        let expected_tokens = vec![
            Token::new("let".to_string(), TokenType::Keyword, (21, 3)),
            Token::new("x".to_string(), TokenType::Identifier, (25, 1)),
            Token::new("=".to_string(), TokenType::Equal, (27, 1)),
            Token::new("1".to_string(), TokenType::Number, (29, 1)),
            Token::new(";".to_string(), TokenType::Semicolon, (30, 1)),
        ];

        assert_eq!(tokens, expected_tokens);
    }
}
