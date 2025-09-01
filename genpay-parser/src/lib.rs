use crate::{
    error::{ParserError, ParserWarning},
    expressions::Expressions,
    statements::Statements,
    types::Type,
    value::Value,
};
use bumpalo::Bump;
use genpay_lexer::{token::Token, token_type::TokenType, Lexer};
use miette::NamedSource;

pub use crate::expressions::Spannable;
pub use genpay_lexer::token_type;

/// Custom Defined Error Types
pub mod error;
/// Expressions Enum
pub mod expressions;
/// Statements Enum
pub mod statements;
/// Compiler's Types
pub mod types;
/// Basic Values Enum
pub mod value;

pub type ParserOk<'a> = (Vec<Statements<'a>>, Vec<ParserWarning>);
pub type ParserErr = (Vec<ParserError>, Vec<ParserWarning>);

const BINARY_OPERATORS: [TokenType; 5] = [
    TokenType::Plus,     // +
    TokenType::Minus,    // -
    TokenType::Divide,   // /
    TokenType::Multiply, // *
    TokenType::Modulus,  // %
];

const BOOLEAN_OPERATORS: [TokenType; 8] = [
    TokenType::Lt,  // <
    TokenType::Bt,  // >
    TokenType::Leq, // <=, =<
    TokenType::Beq, // >=, =>
    TokenType::Eq,  // ==
    TokenType::Ne,  // !
    TokenType::Or,  // ||
    TokenType::And, // &&
];

const BITWISE_OPERATORS: [TokenType; 5] = [
    TokenType::LShift,    // <<
    TokenType::RShift,    // >>
    TokenType::Ampersand, // &
    TokenType::Verbar,    // |
    TokenType::Xor,       // ^
];

const PRIORITY_BINARY_OPERATORS: [TokenType; 3] =
    [TokenType::Multiply, TokenType::Divide, TokenType::Modulus];
const PRIORITY_BOOLEAN_OPERATORS: [TokenType; 2] = [TokenType::Or, TokenType::And];

const END_STATEMENT: TokenType = TokenType::Semicolon;

/// Main Syntax Analyzer Object
///
/// **Main Functions:**
/// - [`Parser::new`] - structure builder
/// - [`Parser::parse`] - main parser functions
///
/// Function [`Parser::get_span_expression`] is used to extract span tuple from
/// [`expressions::Expressions`] enum
use std::fmt;

pub struct Parser<'a> {
    source: NamedSource<String>,

    tokens: Vec<Token<'a>>,
    position: usize,

    errors: Vec<ParserError>,
    warnings: Vec<ParserWarning>,
    eof: bool,
}

impl fmt::Debug for Parser<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Parser")
            .field("source", &self.source)
            .field("tokens", &self.tokens)
            .field("position", &self.position)
            .field("errors", &self.errors)
            .field("warnings", &self.warnings)
            .field("eof", &self.eof)
            .finish()
    }
}

impl<'a> Parser<'a> {
    // main

    /// **Structure Builder** <br/>
    /// Requires full ownership for vector of tokens, and source code with filename for error
    /// handling
    pub fn new(source: &'a str, filename: &'a str) -> Self {
        let lexer = Lexer::new(source, filename);
        Self::new_with_lexer(lexer, source, filename)
    }

    pub fn new_with_lexer(lexer: Lexer<'a>, source: &'a str, filename: &'a str) -> Self {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        for result in lexer {
            match result {
                Ok(token) => tokens.push(token),
                Err(err) => errors.push(err.into()),
            }
        }

        Self {
            source: NamedSource::new(filename, source.to_string()),
            tokens,
            position: 0,
            errors,
            warnings: Vec::new(),
            eof: false,
        }
    }

    /// **Main Parser Function** <br/>
    /// Requires new created self instance. **Can be called only once!**
    pub fn parse(
        &mut self,
        expr_arena: &'a Bump,
        stmt_arena: &'a Bump,
    ) -> Result<ParserOk<'a>, ParserErr> {
        let mut output = Vec::new();

        while self.current().token_type != TokenType::EOF {
            match self.statement(expr_arena, stmt_arena) {
                Ok(stmt) => output.push(stmt),
                Err(err) => {
                    self.errors.push(err);
                    self.skip_statement();
                }
            }
        }

        if !self.errors.is_empty() {
            return Err((self.errors.clone(), self.warnings.clone()));
        }
        Ok((output, self.warnings.clone()))
    }

    fn error(&mut self, error: ParserError) {
        self.errors.push(error);
    }

    #[allow(unused)]
    fn warning(&mut self, message: String, span: (usize, usize)) {
        let span = (span.0, span.1.wrapping_sub(span.0));

        self.warnings.push(error::ParserWarning {
            message,
            span: span.into(),
            src: self.source.clone(),
        })
    }

    fn get_basic_type(&mut self, datatype: &'a str, span: (usize, usize)) -> Type<'a> {
        match datatype {
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

            _ => {
                self.error(ParserError::DatatypeException {
                    exception: "unable to parse datatype".to_string(),
                    help: "Check provided datatype for corectness".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span(span),
                });

                Type::Void
            }
        }
    }

    fn parse_type(&mut self) -> Type<'a> {
        let current = self.current();

        match current.token_type {
            TokenType::Type => {
                let _ = self.next();
                self.get_basic_type(current.value, current.span)
            }
            TokenType::LBrack => {
                let _ = self.next();

                if self.current().token_type == TokenType::RBrack {
                    // dynamic array
                    let _ = self.next();
                    let array_type = self.parse_type();

                    return Type::DynamicArray(Box::new(array_type));
                }

                // default array
                let array_type = self.parse_type();

                if !self.expect(TokenType::Semicolon) {
                    // TODO: add error reporting
                }
                let _ = self.next(); // consume semicolon

                let size_token = self.current();
                if size_token.token_type != TokenType::Number {
                    self.error(
                        ParserError::DatatypeException {
                            exception: "array size must be integer constant".to_string(),
                            help: "Consider replacing expression with another which will return integer value".to_string(),
                            src: self.source.clone(),
                            span: error::position_to_span(self.current().span)
                        }
                    );
                    return Type::Void;
                }
                let _ = self.next(); // consume size

                if !self.expect(TokenType::RBrack) {
                    self.error(ParserError::UnclosedExpression {
                        exception: "unclosed brackets in array".to_string(),
                        help: "Consider closing array with brackets".to_string(),
                        src: self.source.clone(),
                        span: error::position_to_span(self.current().span),
                    });

                    return Type::Void;
                }
                let _ = self.next();

                let array_size = size_token.value.parse::<usize>().unwrap();
                Type::Array(Box::new(array_type), array_size)
            }
            TokenType::Multiply => {
                let _ = self.next();
                let ptr_type = self.parse_type();

                Type::Pointer(Box::new(ptr_type))
            }
            TokenType::LParen => {
                let _ = self.next();
                let mut types = Vec::new();

                loop {
                    if self.expect(TokenType::RParen) {
                        break;
                    }
                    types.push(self.parse_type());
                    if self.expect(TokenType::Comma) {
                        let _ = self.next();
                    } else if !self.expect(TokenType::RParen) {
                        // error: expected comma or rparen
                        break;
                    }
                }

                let _ = self.next();
                Type::Tuple(types)
            }
            TokenType::Identifier => {
                let _ = self.next();
                Type::Alias(current.value)
            }
            _ => {
                let _ = self.next();

                self.error(ParserError::DatatypeException {
                    exception: "unknown type found".to_string(),
                    help: "Check provided type and fix it".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span((current.span.0 - 1, current.span.1 - 1)),
                });

                Type::Void
            }
        }
    }

    // fundamental

    fn next(&mut self) -> Token<'a> {
        self.position += 1;

        if self.position < self.tokens.len() {
            self.tokens[self.position].clone()
        } else {
            Token::new("", TokenType::EOF, (0, 1))
        }
    }

    fn current(&self) -> Token<'a> {
        if self.position >= self.tokens.len() {
            let last_span = self.tokens.last().map(|t| t.span).unwrap_or((0, 1));
            return Token::new("", TokenType::EOF, last_span);
        }
        let mut cur = self.tokens[self.position].clone();

        if let TokenType::EOF = cur.token_type {
            if self.position > 0 {
                cur.span = self.tokens[self.position - 1].clone().span;
            }
        }

        cur
    }

    fn peek(&self) -> Option<&Token<'a>> {
        self.peek_nth(1)
    }

    fn peek_nth(&self, n: usize) -> Option<&Token<'a>> {
        self.tokens.get(self.position + n)
    }

    fn previous(&self) -> Option<&Token<'a>> {
        if self.position == 0 {
            return None;
        }
        self.tokens.get(self.position - 1)
    }

    fn expect(&self, expected: TokenType) -> bool {
        self.current().token_type == expected
    }

    fn skip_eos(&mut self) {
        if self.current().token_type == END_STATEMENT {
            let _ = self.next();
        }
    }

    fn skip_statement(&mut self) {
        while !self.expect(TokenType::Semicolon) {
            let _ = self.next();
        }
        self.skip_eos();
    }
}

impl<'a> Parser<'a> {
    fn term(&mut self, expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Expressions<'a> {
        let current = self.current();
        let _ = self.next();

        match current.token_type {
            TokenType::Null => Expressions::Value(Value::Null, current.span),
            TokenType::Number => Expressions::Value(
                Value::Integer(current.value.trim().parse().unwrap()),
                current.span,
            ),
            TokenType::FloatNumber => Expressions::Value(
                Value::Float(current.value.trim().parse().unwrap()),
                current.span,
            ),
            TokenType::String => Expressions::Value(Value::String(current.value), current.span),
            TokenType::Char => Expressions::Value(
                Value::Char(current.value.chars().nth(0).unwrap()),
                current.span,
            ),
            TokenType::Boolean => {
                Expressions::Value(Value::Boolean(current.value == "true"), current.span)
            }
            TokenType::Keyword => Expressions::Value(Value::Keyword(current.value), current.span),

            TokenType::Minus | TokenType::Not => {
                let object = self.term(expr_arena, stmt_arena);
                let span = (current.span.0, object.span().1);

                Expressions::Unary {
                    operand: current.token_type,
                    object: expr_arena.alloc(object),
                    span,
                }
            }
            TokenType::LParen => {
                self.position -= 1;
                let span_start = self.current().span.0;
                self.position += 1;

                if self.expect(TokenType::RParen) {
                    let span_end = self.current().span.1 + 1;
                    let _ = self.next();

                    return Expressions::Tuple {
                        values: &[],
                        span: (span_start, span_end),
                    };
                }

                let expr = self.expression(expr_arena, stmt_arena);

                if self.expect(TokenType::Comma) {
                    let mut values = vec![expr];
                    let _ = self.next();

                    while !self.expect(TokenType::RParen) {
                        if self.expect(TokenType::Comma) {
                            let _ = self.next();
                        } else if self.expect(TokenType::RParen) {
                            break;
                        } else {
                            values.push(self.expression(expr_arena, stmt_arena));
                        }
                    }

                    let span_end = self.current().span.1;
                    if self.expect(TokenType::RParen) {
                        let _ = self.next();
                    }

                    return Expressions::Tuple {
                        values: expr_arena.alloc_slice_fill_iter(values),
                        span: (span_start, span_end),
                    };
                }

                if self.expect(TokenType::RParen) {
                    let _ = self.next();
                }
                expr
            }

            TokenType::Identifier => {
                let output =
                    Expressions::Value(Value::Identifier(current.value), current.span);

                match self.current().token_type {
                    TokenType::LParen => return self.call_expression(current.value, current.span, expr_arena, stmt_arena),
                    TokenType::LBrack => return self.slice_expression(output, expr_arena, stmt_arena),
                    TokenType::Dot => {
                        return self.subelement_expression(output, TokenType::Dot, expr_arena, stmt_arena);
                    }
                    TokenType::LBrace => {
                        let _ = self.next();
                        if self.expect(TokenType::Dot) {
                            self.position -= 2;
                            return self.struct_expression(current.value, expr_arena, stmt_arena);
                        }
                        self.position -= 1;
                        return output;
                    }
                    TokenType::Not => {
                        let _ = self.next();
                        if self.expect(TokenType::LParen) {
                            self.position -= 1;
                            return self.macrocall_expression(current.value, current.span, expr_arena, stmt_arena);
                        }
                        self.position -= 1;
                        return output;
                    }
                    TokenType::DoubleDots => {
                        let _ = self.next();

                        let datatype = self.parse_type();
                        return Expressions::Argument {
                            name: current.value,
                            r#type: datatype,
                            span: (current.span.0, self.current().span.1),
                        };
                    }
                    _ => {}
                }

                output
            }

            TokenType::Ampersand => Expressions::Reference {
                object: expr_arena.alloc(self.term(expr_arena, stmt_arena)),
                span: (current.span.0, self.current().span.1),
            },
            TokenType::Multiply => Expressions::Dereference {
                object: expr_arena.alloc(self.term(expr_arena, stmt_arena)),
                span: (current.span.0, self.current().span.1),
            },
            TokenType::LBrack => {
                let span_start = current.span.0;
                let values =
                    self.expressions_enum(TokenType::LBrack, TokenType::RBrack, TokenType::Comma, expr_arena, stmt_arena);
                let len = values.len();

                let span_end = self.current().span.1;

                Expressions::Array {
                    values,
                    len,
                    span: (span_start, span_end),
                }
            }
            TokenType::LBrace => {
                let span_start = current.span.0;
                let mut block = Vec::new();

                while !self.expect(TokenType::RBrace) {
                    match self.statement(expr_arena, stmt_arena) {
                        Ok(stmt) => block.push(stmt),
                        Err(err) => {
                            self.errors.push(err);
                            self.skip_statement();
                        },
                    }
                }

                let span_end = self.current().span.1;
                if self.expect(TokenType::RBrace) {
                    let _ = self.next();
                }

                Expressions::Scope {
                    block: stmt_arena.alloc_slice_fill_iter(block),
                    span: (span_start, span_end),
                }
            }

            TokenType::Type => {
                let datatype = self.get_basic_type(current.value, current.span);
                Expressions::Argument {
                    name: "@genpay_type",
                    r#type: datatype,
                    span: current.span,
                }
            }

            _ => {
                self.error(ParserError::UnknownExpression {
                    exception: "undefined term".to_string(),
                    help: "Verify provided expression and its syntax".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span((current.span.0, current.span.1)),
                });

                Expressions::None
            }
        }
    }

    fn expression(&mut self, expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Expressions<'a> {
        let node = self.term(expr_arena, stmt_arena);
        let current = self.current();

        match current.token_type {
            tty if BINARY_OPERATORS.contains(&tty) => self.binary_expression(node, expr_arena, stmt_arena),
            tty if BOOLEAN_OPERATORS.contains(&tty) => self.boolean_expression(node, expr_arena, stmt_arena),
            tty if BITWISE_OPERATORS.contains(&tty) => self.bitwise_expression(node, expr_arena, stmt_arena),

            TokenType::LBrack => {
                let span = current.span;
                let _ = self.next();

                if self.expect(TokenType::RBrack) {
                    return node;
                }

                let slice_index = self.expression(expr_arena, stmt_arena);

                if !self.expect(TokenType::RBrack) {
                    self.error(ParserError::UnclosedExpression {
                        exception: "unclosed brackets in expression found".to_string(),
                        help: "Consider closing brackets in expression".to_string(),
                        src: self.source.clone(),
                        span: error::position_to_span(span),
                    });

                    return Expressions::None;
                }

                let span = (span.0, self.current().span.1);
                let _ = self.next();

                Expressions::Slice {
                    object: expr_arena.alloc(node),
                    index: expr_arena.alloc(slice_index),
                    span,
                }
            }

            END_STATEMENT => {
                self.next();
                node
            }

            _ => node,
        }
    }

    fn statement(&mut self, expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Result<Statements<'a>, ParserError> {
        if self.current().token_type == TokenType::EOF {
            self.eof = true;
            return Ok(Statements::None);
        }
        let current = self.current();

        match current.token_type {
            TokenType::Keyword => match current.value {
                "let" => self.annotation_statement(expr_arena, stmt_arena),
                "import" => self.import_statement(expr_arena, stmt_arena),
                "include" => self.include_statement(expr_arena, stmt_arena),
                "extern" => self.extern_statement(expr_arena, stmt_arena),
                "_extern_declare" => self.extern_declare_statement(expr_arena, stmt_arena),
                "_link_c" => self.link_c_statement(expr_arena, stmt_arena),
                "if" => self.if_statement(expr_arena, stmt_arena),
                "else" => {
                    Err(ParserError::UnknownExpression {
                        exception: "unexpected `else` usage outside construction".to_string(),
                        help: "Consider placing keyword in `if/else` construction".to_string(),
                        src: self.source.clone(),
                        span: error::position_to_span(current.span),
                    })
                }

                "while" => self.while_statement(expr_arena, stmt_arena),
                "for" => self.for_statement(expr_arena, stmt_arena),

                "typedef" => self.typedef_statement(expr_arena, stmt_arena),
                "struct" => self.struct_statement(expr_arena, stmt_arena),
                "enum" => self.enum_statement(expr_arena, stmt_arena),

                "pub" => {
                    let _ = self.next();
                    let stmt = self.statement(expr_arena, stmt_arena)?;

                    Ok(match stmt {
                        Statements::FunctionDefineStatement {
                            name,
                            datatype,
                            arguments,
                            block,
                            public: _,
                            span,
                            header_span,
                        } => Statements::FunctionDefineStatement {
                            name,
                            datatype,
                            arguments,
                            block,
                            public: true,
                            span,
                            header_span,
                        },
                        Statements::StructDefineStatement {
                            name,
                            fields,
                            functions,
                            public: _,
                            span,
                        } => Statements::StructDefineStatement {
                            name,
                            fields,
                            functions,
                            public: true,
                            span,
                        },
                        Statements::EnumDefineStatement {
                            name,
                            fields,
                            functions,
                            public: _,
                            span,
                        } => Statements::EnumDefineStatement {
                            name,
                            fields,
                            functions,
                            public: true,
                            span,
                        },

                        _ => {
                            self.error(ParserError::VisibilityError {
                                exception: "visibility is not followed by provided item"
                                    .to_string(),
                                help: "Remove public changer keyword".to_string(),
                                src: self.source.clone(),
                                span: error::position_to_span(current.span),
                            });

                            Statements::None
                        }
                    })
                }
                "fn" => self.fn_statement(expr_arena, stmt_arena),
                "return" => self.return_statement(expr_arena, stmt_arena),
                "break" => self.break_statement(expr_arena, stmt_arena),
                _ => unreachable!(),
            },
            TokenType::LBrace => {
                let span_start = current.span.0;
                let _ = self.next();

                let mut block = Vec::new();
                while !self.expect(TokenType::RBrace) {
                    block.push(self.statement(expr_arena, stmt_arena)?);
                }

                let span = (span_start, self.current().span.1);
                if self.expect(TokenType::RBrace) {
                    let _ = self.next();
                }
                self.skip_eos();

                Ok(Statements::ScopeStatement {
                    block,
                    span,
                })
            }
            TokenType::Multiply => {
                let span_start = self.current().span.0;
                let _ = self.next();

                match self.current().token_type {
                    TokenType::Identifier | TokenType::Multiply => {
                        let stmt = self.statement(expr_arena, stmt_arena)?;

                        self.position -= 1;
                        let span_end = self.current().span.1;
                        self.position += 1;

                        Ok(match stmt {
                            Statements::AssignStatement {
                                object,
                                value,
                                span,
                            } => Statements::DerefAssignStatement {
                                object,
                                value,
                                span,
                            },
                            Statements::FieldAssignStatement {
                                object,
                                value,
                                span,
                            } => Statements::DerefAssignStatement {
                                object,
                                value,
                                span,
                            },
                            Statements::BinaryAssignStatement {
                                object,
                                operand,
                                value,
                                span,
                            } => Statements::DerefAssignStatement {
                                object: object.clone(),
                                value: Expressions::Binary {
                                    operand,
                                    lhs: expr_arena.alloc(object),
                                    rhs: expr_arena.alloc(value),
                                    span,
                                },
                                span,
                            },
                            Statements::SliceAssignStatement {
                                object,
                                index,
                                value,
                                span,
                            } => Statements::DerefAssignStatement {
                                object: Expressions::Slice {
                                    object: expr_arena.alloc(object),
                                    index: expr_arena.alloc(index),
                                    span,
                                },
                                value,
                                span,
                            },
                            Statements::DerefAssignStatement {
                                object,
                                value,
                                span,
                            } => Statements::DerefAssignStatement {
                                object: Expressions::Dereference {
                                    object: expr_arena.alloc(object),
                                    span,
                                },
                                value,
                                span,
                            },
                            _ => {
                                return Err(ParserError::UnsupportedExpression {
                                    exception: "unsupported for dereference statement kind"
                                        .to_string(),
                                    help: "If you didn't want to dereference, delete the operator"
                                        .to_string(),
                                    src: self.source.clone(),
                                    span: error::position_to_span((span_start, span_end)),
                                });
                            }
                        })
                    }
                    _ => {
                        Err(ParserError::UnsupportedExpression {
                            exception: "unsupported for dereference statement kind".to_string(),
                            help: "If you didn't want to dereference, delete the operator"
                                .to_string(),
                            src: self.source.clone(),
                            span: error::position_to_span(self.current().span),
                        })
                    }
                }
            }
            TokenType::Identifier => {
                let next = self.next();
                match next.token_type {
                    TokenType::Equal => self.assign_statement(
                        Expressions::Value(Value::Identifier(current.value), current.span),
                        current.span,
                        expr_arena,
                        stmt_arena,
                    ),
                    TokenType::Not => self.macrocall_statement(current.value, current.span, expr_arena, stmt_arena),
                    TokenType::Dot => {
                        let sub_expr = self.subelement_expression(
                            Expressions::Value(
                                Value::Identifier(current.value),
                                self.current().span,
                            ),
                            TokenType::Dot,
                            expr_arena,
                            stmt_arena,
                        );

                        match self.current().token_type {
                            TokenType::Equal => {
                                let _ = self.next();
                                let value = self.expression(expr_arena, stmt_arena);
                                let span_end = self.current().span.1;
                                self.skip_eos();

                                Ok(Statements::FieldAssignStatement {
                                    object: sub_expr,
                                    value,
                                    span: (current.span.0, span_end),
                                })
                            }
                            TokenType::Plus
                            | TokenType::Minus
                            | TokenType::Multiply
                            | TokenType::Divide => {
                                let operand = self.current().token_type;
                                let _ = self.next();

                                if !self.expect(TokenType::Equal) {
                                    return Err(ParserError::UnknownExpression {
                                        exception: "unexpected binary expression after subelement"
                                            .to_string(),
                                        help: "Consider adding `=` after subelement".to_string(),
                                        src: self.source.clone(),
                                        span: error::position_to_span((
                                            current.span.0,
                                            self.current().span.1,
                                        )),
                                    });
                                }

                                let _ = self.next();
                                let value = self.expression(expr_arena, stmt_arena);
                                let span_end = self.current().span.1;
                                self.skip_eos();

                                Ok(Statements::BinaryAssignStatement {
                                    object: sub_expr,
                                    operand,
                                    value,
                                    span: (current.span.0, span_end),
                                })
                            }
                            TokenType::Semicolon => {
                                self.skip_eos();
                                Ok(Statements::Expression(sub_expr))
                            }
                            _ => {
                                self.position -= 1;
                                let span_end = self.current().span.1;
                                self.position += 1;

                                Err(ParserError::UnknownExpression {
                                    exception: "unknown subelement in statement found".to_string(),
                                    help: "Remove subelement expression if it's not necessary"
                                        .to_string(),
                                    src: self.source.clone(),
                                    span: error::position_to_span((current.span.0, span_end)),
                                })
                            }
                        }
                    }
                    TokenType::LParen => self.call_statement(current.value, current.span, expr_arena, stmt_arena),
                    TokenType::LBrack => self.slice_assign_statement(
                        Expressions::Value(Value::Identifier(current.value), current.span),
                        current.span,
                        expr_arena,
                        stmt_arena,
                    ),

                    ref tty if BINARY_OPERATORS.contains(&tty) => match self.next().token_type {
                        TokenType::Equal => self.binary_assign_statement(
                            Expressions::Value(Value::Identifier(current.value), current.span),
                            next.token_type,
                            current.span,
                            expr_arena,
                            stmt_arena,
                        ),
                        TokenType::Plus | TokenType::Minus => {
                            let span_start = next.span.0;
                            let span_end = self.current().span.1;
                            let (op1, op2) = (next.token_type, self.current().token_type);

                            if op1 != op2 {
                                return Err(ParserError::UnknownExpression {
                                    exception: "unknown variation of increment/decrement found"
                                        .to_string(),
                                    help:
                                        "Consider using right increment/decrement syntax: a++ / a--"
                                            .to_string(),
                                    src: self.source.clone(),
                                    span: error::position_to_span((span_start, span_end)),
                                });
                            }

                            let _ = self.next();
                            self.skip_eos();

                            Ok(Statements::BinaryAssignStatement {
                                object: Expressions::Value(
                                    Value::Identifier(current.value),
                                    current.span,
                                ),
                                operand: op1,
                                value: Expressions::Value(
                                    Value::Integer(1),
                                    (current.span.0, span_end),
                                ),
                                span: (current.span.0, span_end),
                            })
                        }
                        _ => {
                            Err(ParserError::UnknownExpression {
                                exception: "unknown binary operation in statement found"
                                    .to_string(),
                                help: "Maybe you wanted to add assign operator?".to_string(),
                                src: self.source.clone(),
                                span: error::position_to_span((
                                    current.span.0,
                                    self.current().span.1,
                                )),
                            })
                        }
                    },
                    END_STATEMENT => Ok(Statements::Expression(Expressions::Value(
                        Value::Identifier(current.value),
                        current.span,
                    ))),
                    _ => {
                        Err(ParserError::UnknownExpression {
                            exception: "unknown expression found after identifier".to_string(),
                            help: String::new(),
                            src: self.source.clone(),
                            span: error::position_to_span((current.span.0, next.span.1)),
                        })
                    }
                }
            }
            TokenType::EOF => {
                self.eof = true;
                Ok(Statements::None)
            }
            _ => Ok(Statements::Expression(self.expression(expr_arena, stmt_arena))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_basic_type_test() {
        let mut parser = Parser::new("", "");

        [
            ("i8", Type::I8),
            ("i16", Type::I16),
            ("i32", Type::I32),
            ("i64", Type::I64),
            ("u8", Type::U8),
            ("u16", Type::U16),
            ("u32", Type::U32),
            ("u64", Type::U64),
            ("usize", Type::USIZE),
            ("f32", Type::F32),
            ("f64", Type::F64),
            ("bool", Type::Bool),
            ("char", Type::Char),
            ("void", Type::Void),
        ]
        .into_iter()
        .for_each(|(typ, exp)| {
            assert_eq!(parser.get_basic_type(typ, (0, 0)), exp);
        });
    }

    mod expressions {
        use super::*;

#[test]
fn binary_expression() {
    const SRC: &str = "let a = 5 + 2;";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Binary {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                assert_eq!(operand, TokenType::Plus);

                if let Expressions::Value(Value::Integer(5), _) = *lhs {
                } else {
                    panic!("Wrong LHS found")
                };
                if let Expressions::Value(Value::Integer(2), _) = *rhs {
                } else {
                    panic!("Wrong LHS found")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn binary_advanced_expression() {
    const SRC: &str = "let a = 2 + 2 * 2;";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Binary {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                assert_eq!(operand, TokenType::Plus);

                if let Expressions::Value(Value::Integer(2), _) = *lhs {
                } else {
                    panic!("Wrong LHS found")
                };
                if let Expressions::Binary {
                    operand,
                    lhs,
                    rhs,
                    span: _,
                } = *rhs
                {
                    assert_eq!(operand, TokenType::Multiply);

                    if let Expressions::Value(Value::Integer(2), _) = *lhs {
                    } else {
                        panic!("Wrong LHS found")
                    };
                    if let Expressions::Value(Value::Integer(2), _) = *rhs {
                    } else {
                        panic!("Wrong LHS found")
                    };
                } else {
                    panic!("Wrong LHS found")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn unary_negative_expression() {
    const SRC: &str = "let a = -2;";
    const FILENAME: &str = "test.dn";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Unary {
                operand,
                object,
                span: _,
            } => {
                assert_eq!(operand, TokenType::Minus);

                if let Expressions::Value(Value::Integer(2), _) = *object {
                } else {
                    panic!("Wrong object expression found")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn unary_not_expression() {
    const SRC: &str = "let a = !2;";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Unary {
                operand,
                object,
                span: _,
            } => {
                assert_eq!(operand, TokenType::Not);

                if let Expressions::Value(Value::Integer(2), _) = *object {
                } else {
                    panic!("Wrong object expression found")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn boolean_eq_expression() {
    const SRC: &str = "let a = 1 == 1;";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Boolean {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                assert_eq!(operand, TokenType::Eq);

                if let Expressions::Value(Value::Integer(1), _) = *lhs {
                } else {
                    panic!("Wrong object expression found")
                };
                if let Expressions::Value(Value::Integer(1), _) = *rhs {
                } else {
                    panic!("Wrong object expression found")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn boolean_ne_expression() {
    const SRC: &str = "let a = 1 != 1;";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Boolean {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                assert_eq!(operand, TokenType::Ne);

                if let Expressions::Value(Value::Integer(1), _) = *lhs {
                } else {
                    panic!("Wrong object expression found")
                };
                if let Expressions::Value(Value::Integer(1), _) = *rhs {
                } else {
                    panic!("Wrong object expression found")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn boolean_bt_expression() {
    const SRC: &str = "let a = 1 > 1;";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Boolean {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                assert_eq!(operand, TokenType::Bt);

                if let Expressions::Value(Value::Integer(1), _) = *lhs {
                } else {
                    panic!("Wrong object expression found")
                };
                if let Expressions::Value(Value::Integer(1), _) = *rhs {
                } else {
                    panic!("Wrong object expression found")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn boolean_lt_expression() {
    const SRC: &str = "let a = 1 < 1;";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Boolean {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                assert_eq!(operand, TokenType::Lt);

                if let Expressions::Value(Value::Integer(1), _) = *lhs {
                } else {
                    panic!("Wrong object expression found")
                };
                if let Expressions::Value(Value::Integer(1), _) = *rhs {
                } else {
                    panic!("Wrong object expression found")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn boolean_advanced_expression() {
    const SRC: &str = "let a = 1 == 1 && 0 != 5;";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Boolean {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                assert_eq!(operand, TokenType::And);

                if let Expressions::Boolean {
                    operand,
                    lhs,
                    rhs,
                    span: _,
                } = *lhs
                {
                    assert_eq!(operand, TokenType::Eq);

                    if let Expressions::Value(Value::Integer(1), _) = *lhs {
                    } else {
                        panic!("Wrong object expression found")
                    };
                    if let Expressions::Value(Value::Integer(1), _) = *rhs {
                    } else {
                        panic!("Wrong object expression found")
                    };
                } else {
                    panic!("Wrong boolean expression found");
                }

                if let Expressions::Boolean {
                    operand,
                    lhs,
                    rhs,
                    span: _,
                } = *rhs
                {
                    assert_eq!(operand, TokenType::Ne);

                    if let Expressions::Value(Value::Integer(0), _) = *lhs {
                    } else {
                        panic!("Wrong object expression found")
                    };
                    if let Expressions::Value(Value::Integer(5), _) = *rhs {
                    } else {
                        panic!("Wrong object expression found")
                    };
                } else {
                    panic!("Wrong boolean expression found");
                }
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn bitwise_expression() {
    const SRC: &str = "let a = 5 << 2;";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Bitwise {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                assert_eq!(operand, TokenType::LShift);

                if let Expressions::Value(Value::Integer(5), _) = *lhs {
                } else {
                    panic!("Wrong object expression found")
                };
                if let Expressions::Value(Value::Integer(2), _) = *rhs {
                } else {
                    panic!("Wrong object expression found")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn argument_expression() {
    const SRC: &str = "let a = some_arg: i32";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Argument {
                name,
                r#type,
                span: _,
            } => {
                assert_eq!(name, "some_arg");
                assert_eq!(r#type, Type::I32);
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn argument_advanced_expression() {
    const SRC: &str = "let a = some_arg: *[i32; 5]";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Argument {
                name,
                r#type,
                span: _,
            } => {
                assert_eq!(name, "some_arg");
                assert_eq!(
                    r#type,
                    Type::Pointer(Box::new(Type::Array(Box::new(Type::I32), 5)))
                );
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn subelement_expression() {
    const SRC: &str = "let a = some_struct.field";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::SubElement {
                head,
                subelements,
                span: _,
            } => {
                if let Expressions::Value(Value::Identifier(id), _) = *head {
                    assert_eq!(id, "some_struct");
                } else {
                    panic!("Wrong subelement expr head found");
                }

                let mut subs = subelements.into_iter();
                if let Some(Expressions::Value(Value::Identifier(id), _)) = subs.next() {
                    assert_eq!(id, "field");
                } else {
                    panic!("Wrong subelement in subelement expr found")
                }
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn subelement_advanced_expression() {
    const SRC: &str = "let a = some_struct.field.method()";
    const FILENAME: &str = "test.enpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::SubElement {
                head,
                subelements,
                span: _,
            } => {

                if let Expressions::Value(Value::Identifier(id), _) = *head {
                    assert_eq!(id, "some_struct");
                } else {
                    panic!("Wrong subelement expr head found");
                }

                let mut subs = subelements.into_iter();

                if let Some(Expressions::Value(Value::Identifier(id), _)) = subs.next() {
                    assert_eq!(id, "field");
                } else {
                    // panic!("Wrong subelement in subelement expr found")
                }

                if let Some(Expressions::FnCall {
                    name,
                    arguments,
                    span: _,
                }) = subs.next()
                {
                    assert_eq!(name, "method");
                    assert!(arguments.is_empty());
                } else {
                    // panic!("Wrong subelement in subelement expr found")
                }
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn fncall_expression() {
    const SRC: &str = "let a = call_me()";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::FnCall {
                name,
                arguments,
                span: _,
            } => {
                assert_eq!(name, "call_me");
                assert!(arguments.is_empty());
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn fncall_advanced_expression() {
    const SRC: &str = "let a = call_me(1, id, 1.0)";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::FnCall {
                name,
                arguments,
                span: _,
            } => {
                assert_eq!(name, "call_me");

                let mut args = arguments.into_iter();

                if let Some(Expressions::Value(Value::Integer(int), _)) = args.next() {
                    assert_eq!(int, 1)
                } else {
                    panic!("Argument does not matches expected")
                };
                if let Some(Expressions::Value(Value::Identifier(id), _)) = args.next() {
                    assert_eq!(id, "id")
                } else {
                    panic!("Argument does not matches expected")
                };
                if let Some(Expressions::Value(Value::Float(fl), _)) = args.next() {
                    assert_eq!(fl, 1.0)
                } else {
                    panic!("Argument does not matches expected")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn reference_expression() {
    const SRC: &str = "let a = &b";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Reference { object, span: _ } => {
                if let Expressions::Value(Value::Identifier(id), _) = *object {
                    assert_eq!(id, "b")
                } else {
                    panic!("Ref object doesn't matches expected")
                }
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn reference_advanced_expression() {
    const SRC: &str = "let a = &(b)";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Reference { object, span: _ } => {
                if let Expressions::Value(Value::Identifier(id), _) = *object {
                    assert_eq!(id, "b")
                } else {
                    panic!("Ref object doesn't matches expected")
                }
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn dereference_expression() {
    const SRC: &str = "let a = *b";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Dereference { object, span: _ } => {
                if let Expressions::Value(Value::Identifier(id), _) = *object {
                    assert_eq!(id, "b")
                } else {
                    panic!("Ref object doesn't matches expected")
                }
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn dereference_advanced_expression() {
    const SRC: &str = "let a = **b";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Dereference { object, span: _ } => {
                if let Expressions::Dereference { object, span: _ } = *object {
                    if let Expressions::Value(Value::Identifier(id), _) = *object {
                        assert_eq!(id, "b");
                    } else {
                        panic!("Double dereferenced object isn't identifier")
                    }
                } else {
                    panic!("First level dereference object isn't dereference expr")
                }
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn array_expression() {
    const SRC: &str = "let a = [1, 2, 3]";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Array {
                values,
                len,
                span: _,
            } => {
                assert_eq!(len, 3);

                let mut values = values.into_iter();
                if let Some(Expressions::Value(Value::Integer(1), _)) = values.next() {
                } else {
                    panic!("Arg #1 is wrong")
                }
                if let Some(Expressions::Value(Value::Integer(2), _)) = values.next() {
                } else {
                    panic!("Arg #2 is wrong")
                }
                if let Some(Expressions::Value(Value::Integer(3), _)) = values.next() {
                } else {
                    panic!("Arg #3 is wrong")
                }
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn tuple_expression() {
    const SRC: &str = "let a = (1, 5, 4)";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Tuple { values, span: _ } => {
                assert_eq!(values.len(), 3);

                let mut values = values.into_iter();
                if let Some(Expressions::Value(Value::Integer(1), _)) = values.next() {
                } else {
                    panic!("Argument is wrong")
                };
                if let Some(Expressions::Value(Value::Integer(5), _)) = values.next() {
                } else {
                    panic!("Argument is wrong")
                };
                if let Some(Expressions::Value(Value::Integer(4), _)) = values.next() {
                } else {
                    panic!("Argument is wrong")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn tuple_advanced_expression() {
    const SRC: &str = "let a = (1, 2.0, \"hello\")";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Tuple { values, span: _ } => {
                assert_eq!(values.len(), 3);

                let mut values = values.into_iter();
                if let Some(Expressions::Value(Value::Integer(1), _)) = values.next() {
                } else {
                    panic!("Argument is wrong")
                };
                if let Some(Expressions::Value(Value::Float(2.0), _)) = values.next() {
                } else {
                    panic!("Argument is wrong")
                };
                if let Some(Expressions::Value(Value::String(str), _)) = values.next() {
                    assert_eq!(*str, "hello")
                } else {
                    panic!("Argument is wrong")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn slice_expression() {
    const SRC: &str = "let a = b[0]";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Slice {
                object,
                index,
                span: _,
            } => {
                if let Expressions::Value(Value::Identifier(id), _) = *object {
                    assert_eq!(id, "b");
                    if let Expressions::Value(Value::Integer(0), _) = *index {
                    } else {
                        panic!("Wrong index on slice")
                    }
                }
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn struct_expression() {
    const SRC: &str = "let a = Person { .age = 32, .name = \"John\", .money = 333.12 };";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Struct {
                name,
                fields,
                span: _,
            } => {
                assert_eq!(name, "Person");
                assert!(fields.contains_key("age"));
                assert!(fields.contains_key("name"));
                assert!(fields.contains_key("money"));
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}
    }
    mod statements {
        use super::*;

#[test]
fn assign_statement() {
    const SRC: &str = "some_var = 5;";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::AssignStatement {
            object,
            value,
            span: _,
        }) => {
            if let Expressions::Value(Value::Identifier(identifier), _) = object {
                assert_eq!(*identifier, "some_var");
            } else {
                panic!("Wrong obj expr parsed")
            }

            if let Expressions::Value(Value::Integer(5), _) = value {
            } else {
                panic!("Wrong value expr found")
            };
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn binary_assign_statement() {
    const SRC: &str = "some_var += 5;";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::BinaryAssignStatement {
            object,
            operand,
            value,
            span: _,
        }) => {
            if let Expressions::Value(Value::Identifier(identifier), _) = object {
                assert_eq!(*identifier, "some_var");
            }

            assert_eq!(*operand, TokenType::Plus);

            if let Expressions::Value(Value::Integer(5), _) = value {
            } else {
                panic!("Wrong value expr found")
            };
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn deref_assign_statement() {
    const SRC: &str = "*ptr = 5;";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::DerefAssignStatement {
            object,
            value,
            span: _,
        }) => {
            if let Expressions::Value(Value::Identifier(identifier), _) = object {
                assert_eq!(*identifier, "ptr");
            }

            if let Expressions::Value(Value::Integer(5), _) = value {
            } else {
                panic!("Wrong value expr found")
            };
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn slice_assign_statement() {
    const SRC: &str = "list[0] = 5;";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::SliceAssignStatement {
            object,
            index,
            value,
            span: _,
        }) => {
            if let Expressions::Value(Value::Identifier(identifier), _) = object {
                assert_eq!(*identifier, "list");
            }

            if let Expressions::Value(Value::Integer(0), _) = index {
            } else {
                panic!("Wrong index expr parsed")
            }
            if let Expressions::Value(Value::Integer(5), _) = value {
            } else {
                panic!("Wrong value expr parsed")
            };
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn field_assign_statement() {
    const SRC: &str = "some_struct.field = 12";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::FieldAssignStatement {
            object,
            value,
            span: _,
        }) => {
            if let Expressions::SubElement {
                head,
                subelements,
                span: _,
            } = object
            {
                if let Expressions::Value(Value::Identifier(id), _) = *head {
                    assert_eq!(*id, "some_struct")
                } else {
                    panic!("Wrong head expr found")
                };
                if let Some(Expressions::Value(Value::Identifier(field), _)) = subelements.first() {
                    assert_eq!(*field, "field");
                } else {
                    panic!("Wrong subelement expr found")
                }
            } else {
                panic!("Wrong value expr parsed")
            };
            if let Expressions::Value(Value::Integer(12), _) = value {
            } else {
                panic!("Wrong value expr parsed")
            };
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn annotation_statement() {
    const SRC: &str = "let var;";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::AnnotationStatement {
            identifier,
            datatype,
            value,
            span: _,
        }) => {
            assert_eq!(*identifier, "var");
            assert!(datatype.is_none());
            assert!(value.is_none());
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn annotation_statement_with_type() {
    const SRC: &str = "let var: i32;";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::AnnotationStatement {
            identifier,
            datatype,
            value,
            span: _,
        }) => {
            assert_eq!(*identifier, "var");
            assert!(datatype.is_some());
            assert!(value.is_none());

            assert_eq!(datatype.clone().unwrap(), Type::I32);
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn annotation_statement_with_value() {
    const SRC: &str = "let var = 15;";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::AnnotationStatement {
            identifier,
            datatype,
            value,
            span: _,
        }) => {
            assert_eq!(*identifier, "var");
            assert!(datatype.is_none());
            assert!(value.is_some());

            if let Some(Expressions::Value(Value::Integer(15), _)) = value {
            } else {
                panic!("Wrong value expr parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn annotation_statement_full() {
    const SRC: &str = "let var: usize = 15;";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::AnnotationStatement {
            identifier,
            datatype,
            value,
            span: _,
        }) => {
            assert_eq!(*identifier, "var");
            assert!(datatype.is_some());
            assert!(value.is_some());

            assert_eq!(datatype.clone().unwrap(), Type::USIZE);
            if let Some(Expressions::Value(Value::Integer(15), _)) = value {
            } else {
                panic!("Wrong value expr parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn function_define_statement() {
    const SRC: &str = "fn foo() {}";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::FunctionDefineStatement {
            name,
            datatype,
            arguments,
            block,
            public,
            span: _,
            header_span: _,
        }) => {
            assert_eq!(*name, "foo");
            assert_eq!(datatype, &Type::Void);
            assert!(arguments.is_empty());
            assert!(block.is_empty());
            assert!(!public);
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn function_define_statement_with_type() {
    const SRC: &str = "fn foo() usize {}";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::FunctionDefineStatement {
            name,
            datatype,
            arguments,
            block,
            public,
            span: _,
            header_span: _,
        }) => {
            assert_eq!(*name, "foo");
            assert_eq!(datatype, &Type::USIZE);
            assert!(arguments.is_empty());
            assert!(block.is_empty());
            assert!(!public);
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn function_define_statement_with_args() {
    const SRC: &str = "fn foo(a: i32, b: u64) usize {}";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::FunctionDefineStatement {
            name,
            datatype,
            arguments,
            block,
            public,
            span: _,
            header_span: _,
        }) => {
            assert_eq!(*name, "foo");
            assert_eq!(datatype, &Type::USIZE);
            assert!(block.is_empty());
            assert!(!arguments.is_empty());
            assert!(!public);

            if let Some((argname, argtype)) = arguments.first() {
                assert_eq!(*argname, "a");
                assert_eq!(argtype, &Type::I32);
            } else {
                panic!("Wrong argument expr parsed")
            }

            if let Some((argname, argtype)) = arguments.get(1) {
                assert_eq!(*argname, "b");
                assert_eq!(argtype, &Type::U64);
            } else {
                panic!("Wrong argument expr parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn function_define_statement_with_block() {
    const SRC: &str = "fn foo(a: i32, b: u64) { return 1; }";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::FunctionDefineStatement {
            name,
            datatype,
            arguments,
            block,
            public,
            span: _,
            header_span: _,
        }) => {
            assert_eq!(*name, "foo");
            assert_eq!(datatype, &Type::Void);
            assert!(!block.is_empty());
            assert!(!arguments.is_empty());
            assert!(!public);

            if let Some((argname, argtype)) = arguments.first() {
                assert_eq!(*argname, "a");
                assert_eq!(argtype, &Type::I32);
            } else {
                panic!("Wrong argument expr parsed")
            }

            if let Some((argname, argtype)) = arguments.get(1) {
                assert_eq!(*argname, "b");
                assert_eq!(argtype, &Type::U64);
            } else {
                panic!("Wrong argument expr parsed")
            }

            if let Some(Statements::ReturnStatement { value, span: _ }) = block.first() {
                if let Expressions::Value(Value::Integer(1), _) = value {
                } else {
                    panic!("Wrong value in statement block parsed")
                }
            } else {
                panic!("Wrong statement parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn function_define_statement_public() {
    const SRC: &str = "pub fn foo(a: i32, b: u64) { return 1; }";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::FunctionDefineStatement {
            name,
            datatype,
            arguments,
            block,
            public,
            span: _,
            header_span: _,
        }) => {
            assert_eq!(*name, "foo");
            assert_eq!(datatype, &Type::Void);
            assert!(!block.is_empty());
            assert!(!arguments.is_empty());
            assert!(*public);

            if let Some((argname, argtype)) = arguments.first() {
                assert_eq!(*argname, "a");
                assert_eq!(argtype, &Type::I32);
            } else {
                panic!("Wrong argument expr parsed")
            }

            if let Some((argname, argtype)) = arguments.get(1) {
                assert_eq!(*argname, "b");
                assert_eq!(argtype, &Type::U64);
            } else {
                panic!("Wrong argument expr parsed")
            }

            if let Some(Statements::ReturnStatement { value, span: _ }) = block.first() {
                if let Expressions::Value(Value::Integer(1), _) = value {
                } else {
                    panic!("Wrong value in statement block parsed")
                }
            } else {
                panic!("Wrong statement parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn function_call_statement() {
    const SRC: &str = "foo()";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::FunctionCallStatement {
            name,
            arguments,
            span: _,
        }) => {
            assert_eq!(*name, "foo");
            assert!(arguments.is_empty());
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn function_call_advanced_statement() {
    const SRC: &str = "foo(1, 2)";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::FunctionCallStatement {
            name,
            arguments,
            span: _,
        }) => {
            assert_eq!(*name, "foo");
            assert!(!arguments.is_empty());

            if let Some(Expressions::Value(Value::Integer(1), _)) = arguments.first() {
            } else {
                panic!("Wrong #1 argument parsed")
            }
            if let Some(Expressions::Value(Value::Integer(2), _)) = arguments.get(1) {
            } else {
                panic!("Wrong #2 argument parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn struct_define_statement() {
    const SRC: &str = "struct Person { name: *char, age: u8 }";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::StructDefineStatement {
            name,
            fields,
            functions,
            public,
            span: _,
        }) => {
            assert_eq!(*name, "Person");
            assert!(!fields.is_empty());
            assert!(!public);
            assert!(functions.is_empty());

            if let Some(Type::Pointer(_)) = fields.get("name") {
            } else {
                panic!("Wrong argument parsed")
            }
            if let Some(Type::U8) = fields.get("age") {
            } else {
                panic!("Wrong argument parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn struct_define_with_fn_statement() {
    const SRC: &str = "struct Person { name: *char, age: u8, fn foo() {} }";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::StructDefineStatement {
            name,
            fields,
            functions,
            public,
            span: _,
        }) => {
            assert_eq!(*name, "Person");
            assert!(!fields.is_empty());
            assert!(!public);
            assert!(!functions.is_empty());

            if let Some(Type::Pointer(_)) = fields.get("name") {
            } else {
                panic!("Wrong argument parsed")
            }
            if let Some(Type::U8) = fields.get("age") {
            } else {
                panic!("Wrong argument parsed")
            }

            if let Some(Statements::FunctionDefineStatement {
                name,
                datatype,
                arguments: _,
                block: _,
                public: _,
                span: _,
                header_span: _,
            }) = functions.get("foo")
            {
                assert_eq!(*name, "foo");
                assert_eq!(datatype, &Type::Void);
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn struct_define_public_statement() {
    const SRC: &str = "pub struct Person { name: *char, age: u8, fn foo() {} }";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::StructDefineStatement {
            name,
            fields,
            functions,
            public,
            span: _,
        }) => {
            assert_eq!(*name, "Person");
            assert!(!fields.is_empty());
            assert!(!functions.is_empty());
            assert!(*public);

            if let Some(Type::Pointer(_)) = fields.get("name") {
            } else {
                panic!("Wrong argument parsed")
            }
            if let Some(Type::U8) = fields.get("age") {
            } else {
                panic!("Wrong argument parsed")
            }

            if let Some(Statements::FunctionDefineStatement {
                name,
                datatype,
                arguments: _,
                block: _,
                public: _,
                span: _,
                header_span: _,
            }) = functions.get("foo")
            {
                assert_eq!(*name, "foo");
                assert_eq!(datatype, &Type::Void);
            } else {
                panic!("Wrong function define stmt parsed!")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn enum_define_statement() {
    const SRC: &str = "enum ABC { A, B, C }";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::EnumDefineStatement {
            name,
            fields,
            functions,
            public,
            span: _,
        }) => {
            assert_eq!(*name, "ABC");
            assert!(!fields.is_empty());
            assert!(!public);
            assert!(functions.is_empty());

            if let Some(&"A") = fields.first() {
            } else {
                panic!("Wrong field parsed")
            };
            if let Some(&"B") = fields.get(1) {
            } else {
                panic!("Wrong field parsed")
            };
            if let Some(&"C") = fields.get(2) {
            } else {
                panic!("Wrong field parsed")
            };
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn enum_define_with_fn_statement() {
    const SRC: &str = "enum ABC { A, B, C, fn foo() {} }";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::EnumDefineStatement {
            name,
            fields,
            functions,
            public,
            span: _,
        }) => {
            assert_eq!(*name, "ABC");
            assert!(!fields.is_empty());
            assert!(!public);
            assert!(!functions.is_empty());

            if let Some(&"A") = fields.first() {
            } else {
                panic!("Wrong field parsed")
            };
            if let Some(&"B") = fields.get(1) {
            } else {
                panic!("Wrong field parsed")
            };
            if let Some(&"C") = fields.get(2) {
            } else {
                panic!("Wrong field parsed")
            };

            if let Some(Statements::FunctionDefineStatement {
                name,
                datatype,
                arguments: _,
                block: _,
                public: _,
                span: _,
                header_span: _,
            }) = functions.get("foo")
            {
                assert_eq!(*name, "foo");
                assert_eq!(datatype, &Type::Void);
            } else {
                panic!("Wrong function define stmt parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn enum_define_pub_statement() {
    const SRC: &str = "pub enum ABC { A, B, C }";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::EnumDefineStatement {
            name,
            fields,
            functions,
            public,
            span: _,
        }) => {
            assert_eq!(*name, "ABC");
            assert!(!fields.is_empty());
            assert!(*public);
            assert!(functions.is_empty());

            if let Some(&"A") = fields.first() {
            } else {
                panic!("Wrong field parsed")
            };
            if let Some(&"B") = fields.get(1) {
            } else {
                panic!("Wrong field parsed")
            };
            if let Some(&"C") = fields.get(2) {
            } else {
                panic!("Wrong field parsed")
            };
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn typedef_statement() {
    const SRC: &str = "typedef int i32";
    const FILENAME: &str = "test.dn";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::TypedefStatement {
            alias,
            datatype,
            span: _,
        }) => {
            assert_eq!(*alias, "int");
            assert_eq!(datatype, &Type::I32);
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn typedef_advanced_statement() {
    const SRC: &str = "typedef array_ptr *[i32; 5]";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::TypedefStatement {
            alias,
            datatype,
            span: _,
        }) => {
            assert_eq!(*alias, "array_ptr");
            assert_eq!(
                datatype,
                &Type::Pointer(Box::new(Type::Array(Box::new(Type::I32), 5)))
            );
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn if_statement() {
    const SRC: &str = "if true {};";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::IfStatement {
            condition,
            then_block,
            else_block,
            span: _,
        }) => {
            assert!(else_block.is_none());
            assert!(then_block.is_empty());

            if let Expressions::Value(Value::Boolean(true), _) = condition {
            } else {
                panic!("Wrong condition expr parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn if_else_statement() {
    const SRC: &str = "if true {} else {};";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::IfStatement {
            condition,
            then_block,
            else_block,
            span: _,
        }) => {
            assert!(else_block.is_some());
            assert!(then_block.is_empty());

            if let Expressions::Value(Value::Boolean(true), _) = condition {
            } else {
                panic!("Wrong condition expr parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn while_statement() {
    const SRC: &str = "while true {}";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::WhileStatement {
            condition,
            block: _,
            span: _,
        }) => {
            if let Expressions::Value(Value::Boolean(true), _) = condition {
            } else {
                panic!("Wrong condition expr parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn for_statement() {
    const SRC: &str = "for i = 5 {}";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::ForStatement {
            binding,
            iterator,
            block: _,
            span: _,
        }) => {
            assert_eq!(*binding, "i");
            if let Expressions::Value(Value::Integer(5), _) = iterator {
            } else {
                panic!("Wrong iterator obj parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn import_statement() {
    const SRC: &str = "import \"module.dn\"";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::ImportStatement { path, span: _ }) => {
            if let Expressions::Value(Value::String(str), _) = path {
                assert_eq!(*str, "module.dn")
            } else {
                panic!("Wrong import object expr parsed")
            };
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn break_statement() {
    const SRC: &str = "break";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::BreakStatements { span: _ }) => {}
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn return_statement() {
    const SRC: &str = "return 15;";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    match ast.first() {
        Some(Statements::ReturnStatement { value, span: _ }) => {
            if let Expressions::Value(Value::Integer(15), _) = value {
            } else {
                panic!("Wrong return expr parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}
    }
    mod values {
        use super::*;

#[test]
fn basic_values() {
    const SRC: &str = "123; 5.0; 'a'; \"some\"; true";
    const FILENAME: &str = "test.genpay";

    let mut parser = Parser::new(SRC, FILENAME);
    let expr_arena = Bump::new();
    let stmt_arena = Bump::new();
    let (ast, _) = parser.parse(&expr_arena, &stmt_arena).unwrap();

    let mut ast_iter = ast.into_iter();

    if let Some(Statements::Expression(Expressions::Value(Value::Integer(_), _))) = ast_iter.next()
    {
    } else {
        panic!("Test failure for: {:?}", ast_iter.next().unwrap())
    }

    if let Some(Statements::Expression(Expressions::Value(Value::Float(_), _))) = ast_iter.next() {
    } else {
        panic!("Test failure for: {:?}", ast_iter.nth_back(0).unwrap())
    }

    if let Some(Statements::Expression(Expressions::Value(Value::Char(_), _))) = ast_iter.next() {
    } else {
        panic!("Test failure for: {:?}", ast_iter.nth_back(0).unwrap())
    }

    if let Some(Statements::Expression(Expressions::Value(Value::String(_), _))) = ast_iter.next() {
    } else {
        panic!("Test failure for: {:?}", ast_iter.nth_back(0).unwrap())
    }

    if let Some(Statements::Expression(Expressions::Value(Value::Boolean(_), _))) = ast_iter.next()
    {
    } else {
        panic!("Test failure for: {:?}", ast_iter.nth_back(0).unwrap())
    }
}
    }
}
