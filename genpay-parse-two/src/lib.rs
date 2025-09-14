use genpay_lexer::token::Token;
use genpay_lexer::Lexer;

pub mod error;
pub mod types;

use self::types::Type;

use crate::error::ParserError;
use miette::NamedSource;

struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token<'a>,
    source: NamedSource<String>,
    errors: Vec<ParserError<'a>>,
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    LOWEST,
    TUPLE,      // (T, U)
    POINTER,    // *T
    ARRAY,      // [T; 5]
}

use genpay_lexer::token_type::TokenType;

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, filename: &'a str) -> Self {
        let mut lexer = Lexer::new(source);
        let current_token = lexer.next().unwrap_or(Ok(Token {
            value: "",
            token_type: TokenType::EOF,
            span: (0, 0),
        })).unwrap(); // TODO: Handle errors
        Self {
            lexer,
            current_token,
            source: NamedSource::new(filename, source.to_string()),
            errors: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> Result<Type<'a>, Vec<ParserError<'a>>> {
        let result = self.parse_type(Precedence::LOWEST);

        if self.current_token.token_type != TokenType::EOF {
            self.errors.push(ParserError {
                exception: "Unexpected token",
                help: "Expected end of input",
                src: self.source.clone(),
                span: self.current_token.span,
            });
        }

        if self.errors.is_empty() {
            Ok(result)
        } else {
            Err(self.errors.clone())
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.lexer.next().unwrap_or(Ok(Token {
            value: "",
            token_type: TokenType::EOF,
            span: (0, 0),
        })).unwrap(); // TODO: Handle errors
    }

    fn parse_type(&mut self, _precedence: Precedence) -> Type<'a> {
        self.parse_prefix()
    }

    fn parse_prefix(&mut self) -> Type<'a> {
        match self.current_token.token_type {
            TokenType::Multiply => {
                self.next_token();
                let inner_type = self.parse_type(Precedence::POINTER);
                Type::Pointer(Box::new(inner_type))
            }
            TokenType::Type => {
                let type_name = self.current_token.value;
                self.next_token();
                match type_name {
                    "i8" => Type::I8,
                    "i16" => Type::I16,
                    "i32" => Type::I32,
                    "i64" => Type::I64,
                    "u8" => Type::U8,
                    "u16" => Type::U16,
                    "u32" => Type::U32,
                    "u64" => Type::U64,
                    "usize" => Type::USIZE,
                    "f32" => Type::F32,
                    "f64" => Type::F64,
                    "bool" => Type::Bool,
                    "char" => Type::Char,
                    "void" => Type::Void,
                    _ => unreachable!(), // Should be handled by lexer
                }
            }
            TokenType::Identifier => {
                let alias = self.current_token.value;
                self.next_token();
                Type::Alias(alias)
            }
            TokenType::LBrack => self.parse_array_type(),
            TokenType::LParen => self.parse_tuple_type(),
            _ => {
                self.errors.push(ParserError {
                    exception: "Unexpected token in prefix position",
                    help: "Expected a type",
                    src: self.source.clone(),
                    span: self.current_token.span,
                });
                Type::Void // Return a dummy type
            }
        }
    }

    fn parse_array_type(&mut self) -> Type<'a> {
        self.next_token(); // Consume '['

        if self.current_token.token_type == TokenType::RBrack {
            // Dynamic array: []T
            self.next_token(); // Consume ']'
            let inner_type = self.parse_type(Precedence::LOWEST);
            return Type::DynamicArray(Box::new(inner_type));
        }

        // Sized array: [T; size]
        let inner_type = self.parse_type(Precedence::LOWEST);

        if self.current_token.token_type != TokenType::Semicolon {
            self.errors.push(ParserError {
                exception: "Expected semicolon in array type",
                help: "Expected ';'",
                src: self.source.clone(),
                span: self.current_token.span,
            });
            return Type::Void;
        }
        self.next_token(); // Consume ';'

        if self.current_token.token_type != TokenType::Number {
            self.errors.push(ParserError {
                exception: "Expected number for array size",
                help: "Expected an integer literal",
                src: self.source.clone(),
                span: self.current_token.span,
            });
            return Type::Void;
        }
        let size = self.current_token.value.parse().unwrap(); // TODO: handle parse error
        self.next_token(); // Consume size

        if self.current_token.token_type != TokenType::RBrack {
            self.errors.push(ParserError {
                exception: "Expected ']' at the end of array type",
                help: "Expected ']'",
                src: self.source.clone(),
                span: self.current_token.span,
            });
            return Type::Void;
        }
        self.next_token(); // Consume ']'

        Type::Array(Box::new(inner_type), size)
    }

    fn parse_tuple_type(&mut self) -> Type<'a> {
        self.next_token(); // Consume '('
        let mut types = Vec::new();

        if self.current_token.token_type == TokenType::RParen {
            self.next_token(); // Consume ')'
            return Type::Tuple(types);
        }

        types.push(self.parse_type(Precedence::LOWEST));

        while self.current_token.token_type == TokenType::Comma {
            self.next_token(); // Consume ','
            types.push(self.parse_type(Precedence::LOWEST));
        }

        if self.current_token.token_type != TokenType::RParen {
            self.errors.push(ParserError {
                exception: "Expected ')' at the end of tuple type",
                help: "Expected ')'",
                src: self.source.clone(),
                span: self.current_token.span,
            });
        }
        self.next_token(); // Consume ')'

        Type::Tuple(types)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitive_types() {
        let mut parser = Parser::new("i32", "test.pay");
        let parsed_type = parser.parse().unwrap();
        assert_eq!(parsed_type, Type::I32);
    }

    #[test]
    fn test_pointer_type() {
        let mut parser = Parser::new("*i32", "test.pay");
        let parsed_type = parser.parse().unwrap();
        assert_eq!(parsed_type, Type::Pointer(Box::new(Type::I32)));
    }

    #[test]
    fn test_array_type() {
        let mut parser = Parser::new("[i32; 5]", "test.pay");
        let parsed_type = parser.parse().unwrap();
        assert_eq!(parsed_type, Type::Array(Box::new(Type::I32), 5));
    }

    #[test]
    fn test_dynamic_array_type() {
        let mut parser = Parser::new("[]i32", "test.pay");
        let parsed_type = parser.parse().unwrap();
        assert_eq!(parsed_type, Type::DynamicArray(Box::new(Type::I32)));
    }

    #[test]
    fn test_tuple_type() {
        let mut parser = Parser::new("(i32, bool)", "test.pay");
        let parsed_type = parser.parse().unwrap();
        assert_eq!(parsed_type, Type::Tuple(vec![Type::I32, Type::Bool]));
    }

    #[test]
    fn test_nested_type() {
        let mut parser = Parser::new("*([i32; 5], *u8)", "test.pay");
        let parsed_type = parser.parse().unwrap();
        let expected_type = Type::Pointer(Box::new(Type::Tuple(vec![
            Type::Array(Box::new(Type::I32), 5),
            Type::Pointer(Box::new(Type::U8)),
        ])));
        assert_eq!(parsed_type, expected_type);
    }
}
