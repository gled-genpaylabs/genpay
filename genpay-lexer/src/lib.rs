use crate::{error::LexerError, token::Token, token_type::TokenType};

pub mod error;
pub mod token;
pub mod token_type;

pub struct Lexer<'a> {
    source: &'a str,
    cursor: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer { source, cursor: 0 }
    }

    fn is_at_end(&self) -> bool {
        self.cursor >= self.source.len()
    }

    fn peek(&self) -> Option<char> {
        self.source.get(self.cursor..).and_then(|s| s.chars().next())
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
    ) -> Result<Token<'a>, LexerError<'a>> {
        let value = &self.source[start..start + len];
        Ok(Token::new(value, token_type, (start, len)))
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

    fn read_string(&mut self) -> Result<Token<'a>, LexerError<'a>> {
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
        Ok(Token::new(value, TokenType::String, (start, len)))
    }

    fn read_char(&mut self) -> Result<Token<'a>, LexerError<'a>> {
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
        Ok(Token::new(value, TokenType::Char, (start, len)))
    }

    fn read_number(&mut self, start: usize) -> Result<Token<'a>, LexerError<'a>> {
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

    fn read_identifier(&mut self, start: usize) -> Result<Token<'a>, LexerError<'a>> {
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
        Ok(Token::new(value, token_type, (start, len)))
    }
}

fn lookup_identifier(s: &str) -> TokenType {
    match s {
        "if" => TokenType::Keyword,
        "else" => TokenType::Keyword,
        "while" => TokenType::Keyword,
        "for" => TokenType::Keyword,
        "break" => TokenType::Keyword,
        "let" => TokenType::Keyword,
        "pub" => TokenType::Keyword,
        "fn" => TokenType::Keyword,
        "import" => TokenType::Keyword,
        "include" => TokenType::Keyword,
        "extern" => TokenType::Keyword,
        "return" => TokenType::Keyword,
        "struct" => TokenType::Keyword,
        "enum" => TokenType::Keyword,
        "typedef" => TokenType::Keyword,
        "_extern_declare" => TokenType::Keyword,
        "_link_c" => TokenType::Keyword,
        "i8" => TokenType::Type,
        "i16" => TokenType::Type,
        "i32" => TokenType::Type,
        "i64" => TokenType::Type,
        "u8" => TokenType::Type,
        "u16" => TokenType::Type,
        "u32" => TokenType::Type,
        "u64" => TokenType::Type,
        "usize" => TokenType::Type,
        "f32" => TokenType::Type,
        "f64" => TokenType::Type,
        "bool" => TokenType::Type,
        "char" => TokenType::Type,
        "void" => TokenType::Type,
        "true" => TokenType::Boolean,
        "false" => TokenType::Boolean,
        "NULL" => TokenType::Null,
        _ => TokenType::Identifier,
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexerError<'a>>;

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
                self.make_token(TokenType::Plus, start, 1)
            }
            '-' => {
                self.advance();
                self.make_token(TokenType::Minus, start, 1)
            }
            '*' => {
                self.advance();
                self.make_token(TokenType::Multiply, start, 1)
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
        let input = "i8 i16 i32 i64 u8 u16 u32 u64 usize char bool void";
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
        let input = "true false";
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
        let input = "let fn import return struct enum typedef";
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
        let input = "123";
        let mut lexer = Lexer::new(input);
        let token = lexer.next().unwrap().unwrap();
        assert_eq!(token, Token::new("123", TokenType::Number, (0, 3)));
    }

    #[test]
    fn float_number() {
        let input = "1.23";
        let mut lexer = Lexer::new(input);
        let token = lexer.next().unwrap().unwrap();
        assert_eq!(token, Token::new("1.23", TokenType::FloatNumber, (0, 4)));
    }

    #[test]
    fn basic_string() {
        let input = "\"hello\"";
        let mut lexer = Lexer::new(input);
        let token = lexer.next().unwrap().unwrap();
        assert_eq!(token, Token::new("hello", TokenType::String, (0, 7)));
    }

    #[test]
    fn string_with_escapes() {
        let input = "\"hello\\\"world\"";
        let mut lexer = Lexer::new(input);
        let token = lexer.next().unwrap().unwrap();
        assert_eq!(
            token,
            Token::new("hello\\\"world", TokenType::String, (0, 14))
        );
    }

    #[test]
    fn basic_char() {
        let input = "'a'";
        let mut lexer = Lexer::new(input);
        let token = lexer.next().unwrap().unwrap();
        assert_eq!(token, Token::new("a", TokenType::Char, (0, 3)));
    }

    #[test]
    fn operators() {
        let input = "+ - * / % = == != < > <= >= && || ! & | ^ << >>";
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect::<Result<_, _>>().unwrap();

        let expected_tokens = vec![
            Token::new("+", TokenType::Plus, (0, 1)),
            Token::new("-", TokenType::Minus, (2, 1)),
            Token::new("*", TokenType::Multiply, (4, 1)),
            Token::new("/", TokenType::Divide, (6, 1)),
            Token::new("%", TokenType::Modulus, (8, 1)),
            Token::new("=", TokenType::Equal, (10, 1)),
            Token::new("==", TokenType::Eq, (12, 2)),
            Token::new("!=", TokenType::Ne, (15, 2)),
            Token::new("<", TokenType::Lt, (18, 1)),
            Token::new(">", TokenType::Bt, (20, 1)),
            Token::new("<=", TokenType::Leq, (22, 2)),
            Token::new(">=", TokenType::Beq, (25, 2)),
            Token::new("&&", TokenType::And, (28, 2)),
            Token::new("||", TokenType::Or, (31, 2)),
            Token::new("!", TokenType::Not, (34, 1)),
            Token::new("&", TokenType::Ampersand, (36, 1)),
            Token::new("|", TokenType::Verbar, (38, 1)),
            Token::new("^", TokenType::Xor, (40, 1)),
            Token::new("<<", TokenType::LShift, (42, 2)),
            Token::new(">>", TokenType::RShift, (45, 2)),
        ];

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn unterminated_string() {
        let input = "\"hello";
        let mut lexer = Lexer::new(input);
        let result = lexer.next().unwrap();
        assert!(matches!(
            result,
            Err(LexerError::UnterminatedString { .. })
        ));
    }

    #[test]
    fn comment() {
        let input = "// this is a comment\nlet x = 1;";
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect::<Result<_, _>>().unwrap();

        let expected_tokens = vec![
            Token::new("let", TokenType::Keyword, (21, 3)),
            Token::new("x", TokenType::Identifier, (25, 1)),
            Token::new("=", TokenType::Equal, (27, 1)),
            Token::new("1", TokenType::Number, (29, 1)),
            Token::new(";", TokenType::Semicolon, (30, 1)),
        ];

        assert_eq!(tokens, expected_tokens);
    }
}
