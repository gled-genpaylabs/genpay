use genpay_lexer::token::Token;
use genpay_lexer::Lexer;

pub mod error;
pub mod expressions;
pub mod statements;
pub mod types;
pub mod value;

use crate::error::ParserError;
use crate::expressions::Expressions;
use crate::statements::Statements;
use crate::types::Type;
use miette::NamedSource;

pub type Program<'a> = Vec<Statements<'a>>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token<'a>,
    peek_token: Token<'a>,
    source: NamedSource<String>,
    errors: Vec<ParserError<'a>>,
}

#[derive(PartialEq, PartialOrd, Debug, Clone, Copy)]
enum Precedence {
    LOWEST,
    ASSIGN,
    LOGICALOR,
    LOGICALAND,
    BITWISEOR,
    BITWISEXOR,
    BITWISEAND,
    EQUALS,
    LESSGREATER,
    SHIFT,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
    INDEX,
    MEMBER,
}

use genpay_lexer::token_type::TokenType;

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, filename: &'a str) -> Self {
        let mut lexer = Lexer::new(source);
        let current_token = match lexer.next() {
            Some(Ok(token)) => token,
            _ => Token {
                value: "",
                token_type: TokenType::EOF,
                span: (0, 0),
            },
        };
        let peek_token = match lexer.next() {
            Some(Ok(token)) => token,
            _ => Token {
                value: "",
                token_type: TokenType::EOF,
                span: (0, 0),
            },
        };
        Self {
            lexer,
            current_token,
            peek_token,
            source: NamedSource::new(filename, source.to_string()),
            errors: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> Result<Program<'a>, Vec<ParserError<'a>>> {
        let mut program: Program = Vec::new();

        while self.current_token.token_type != TokenType::EOF {
            if let Some(statement) = self.parse_statement() {
                program.push(statement);
            }
            self.next_token();
        }

        if self.errors.is_empty() {
            Ok(program)
        } else {
            Err(self.errors.clone())
        }
    }

    fn parse_statement(&mut self) -> Option<Statements<'a>> {
        match self.current_token.token_type {
            TokenType::Keyword if self.current_token.value == "let" => self.parse_let_statement(),
            TokenType::Keyword if self.current_token.value == "return" => {
                self.parse_return_statement()
            }
            TokenType::Keyword if self.current_token.value == "if" => self.parse_if_statement(),
            TokenType::Keyword if self.current_token.value == "while" => {
                self.parse_while_statement()
            }
            TokenType::Keyword if self.current_token.value == "for" => self.parse_for_statement(),
            TokenType::Keyword if self.current_token.value == "fn" => self.parse_fn_statement(),
            TokenType::Keyword if self.current_token.value == "struct" => {
                self.parse_struct_definition()
            }
            TokenType::Keyword if self.current_token.value == "enum" => {
                self.parse_enum_definition()
            }
            TokenType::Keyword if self.current_token.value == "import" => {
                self.parse_import_statement()
            }
            TokenType::Keyword if self.current_token.value == "include" => {
                self.parse_include_statement()
            }
            TokenType::Keyword if self.current_token.value == "extern" => {
                self.parse_extern_statement()
            }
            TokenType::LBrace => self.parse_block_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Statements<'a>> {
        let expression = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token.token_type == TokenType::Semicolon {
            self.next_token();
        }

        Some(Statements::Expression(expression))
    }

    fn parse_return_statement(&mut self) -> Option<Statements<'a>> {
        let span_start = self.current_token.span.0;
        self.next_token(); // consume 'return'

        let value = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token.token_type == TokenType::Semicolon {
            self.next_token();
        }

        let span_end = self.current_token.span.1;

        Some(Statements::ReturnStatement {
            value,
            span: (span_start, span_end),
        })
    }

    fn parse_block_statement(&mut self) -> Option<Statements<'a>> {
        let span_start = self.current_token.span.0;
        self.next_token(); // consume '{'

        let mut statements = vec![];
        while self.current_token.token_type != TokenType::RBrace
            && self.current_token.token_type != TokenType::EOF
        {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();
        }

        let span_end = self.current_token.span.1;

        Some(Statements::ScopeStatement {
            block: statements,
            span: (span_start, span_end),
        })
    }

    fn parse_if_statement(&mut self) -> Option<Statements<'a>> {
        let span_start = self.current_token.span.0;
        self.next_token(); // consume 'if'

        let condition = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token.token_type != TokenType::LBrace {
            self.errors.push(ParserError {
                exception: "Expected '{' after if condition",
                help: "Expected '{'",
                src: self.source.clone(),
                span: self.peek_token.span,
            });
            return None;
        }
        self.next_token();

        let then_block = self.parse_block_statement()?;

        let mut else_block = None;
        if self.peek_token.token_type == TokenType::Keyword && self.peek_token.value == "else" {
            self.next_token();
            self.next_token();
            else_block = self.parse_block_statement();
        }

        let span_end = self.current_token.span.1;

        Some(Statements::IfStatement {
            condition,
            then_block: match then_block {
                Statements::ScopeStatement { block, .. } => block,
                _ => return None,
            },
            else_block: match else_block {
                Some(Statements::ScopeStatement { block, .. }) => Some(block),
                None => None,
                _ => return None,
            },
            span: (span_start, span_end),
        })
    }

    fn parse_while_statement(&mut self) -> Option<Statements<'a>> {
        let span_start = self.current_token.span.0;
        self.next_token(); // consume 'while'

        let condition = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token.token_type != TokenType::LBrace {
            self.errors.push(ParserError {
                exception: "Expected '{' after while condition",
                help: "Expected '{'",
                src: self.source.clone(),
                span: self.peek_token.span,
            });
            return None;
        }
        self.next_token();

        let block = self.parse_block_statement()?;

        let span_end = self.current_token.span.1;

        Some(Statements::WhileStatement {
            condition,
            block: match block {
                Statements::ScopeStatement { block, .. } => block,
                _ => return None,
            },
            span: (span_start, span_end),
        })
    }

    fn parse_for_statement(&mut self) -> Option<Statements<'a>> {
        let span_start = self.current_token.span.0;
        self.next_token(); // consume 'for'

        if self.current_token.token_type != TokenType::Identifier {
            self.errors.push(ParserError {
                exception: "Expected identifier after 'for'",
                help: "Expected identifier",
                src: self.source.clone(),
                span: self.current_token.span,
            });
            return None;
        }
        let binding = self.current_token.value;
        self.next_token();

        if self.current_token.token_type != TokenType::Equal {
            self.errors.push(ParserError {
                exception: "Expected '=' after identifier in for loop",
                help: "Expected '='",
                src: self.source.clone(),
                span: self.current_token.span,
            });
            return None;
        }
        self.next_token();

        let iterator = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token.token_type != TokenType::LBrace {
            self.errors.push(ParserError {
                exception: "Expected '{' after for loop iterator",
                help: "Expected '{'",
                src: self.source.clone(),
                span: self.peek_token.span,
            });
            return None;
        }
        self.next_token();

        let block = self.parse_block_statement()?;

        let span_end = self.current_token.span.1;

        Some(Statements::ForStatement {
            binding,
            iterator,
            block: match block {
                Statements::ScopeStatement { block, .. } => block,
                _ => return None,
            },
            span: (span_start, span_end),
        })
    }

    fn parse_fn_statement(&mut self) -> Option<Statements<'a>> {
        let span_start = self.current_token.span.0;
        let public = self.current_token.value == "pub";
        if public {
            self.next_token();
        }
        self.next_token(); // consume 'fn'

        if self.current_token.token_type != TokenType::Identifier {
            self.errors.push(ParserError {
                exception: "Expected function name",
                help: "Expected identifier",
                src: self.source.clone(),
                span: self.current_token.span,
            });
            return None;
        }
        let name = self.current_token.value;
        self.next_token();

        if self.current_token.token_type != TokenType::LParen {
            self.errors.push(ParserError {
                exception: "Expected '(' after function name",
                help: "Expected '('",
                src: self.source.clone(),
                span: self.current_token.span,
            });
            return None;
        }
        self.next_token();

        let (arguments, is_var_args) = self.parse_function_parameters()?;

        let mut return_type = Type::Void;
        if self.current_token.token_type != TokenType::LBrace {
            if let Some(rt) = self.parse_type() {
                return_type = rt;
            }
        }

        if self.current_token.token_type != TokenType::LBrace {
            self.errors.push(ParserError {
                exception: "Expected '{' before function body",
                help: "Expected '{'",
                src: self.source.clone(),
                span: self.current_token.span,
            });
            return None;
        }

        let header_span = (span_start, self.current_token.span.0);
        let block = self.parse_block_statement()?;
        let span_end = self.current_token.span.1;

        Some(Statements::FunctionDefineStatement {
            name,
            datatype: return_type,
            arguments,
            block: match block {
                Statements::ScopeStatement { block, .. } => block,
                _ => return None,
            },
            public,
            is_var_args,
            span: (span_start, span_end),
            header_span,
        })
    }

    fn parse_function_parameters(&mut self) -> Option<(Vec<(&'a str, Type<'a>)>, bool)> {
        let mut params = Vec::new();
        let mut is_var_args = false;

        if self.current_token.token_type == TokenType::RParen {
            self.next_token();
            return Some((params, is_var_args));
        }

        if self.current_token.token_type != TokenType::Identifier {
            self.errors.push(ParserError {
                exception: "Expected parameter name",
                help: "Expected identifier",
                src: self.source.clone(),
                span: self.current_token.span,
            });
            return None;
        }
        let name = self.current_token.value;
        self.next_token();

        if self.current_token.token_type != TokenType::DoubleDots {
            self.errors.push(ParserError {
                exception: "Expected ':' after parameter name",
                help: "Expected ':'",
                src: self.source.clone(),
                span: self.current_token.span,
            });
            return None;
        }
        self.next_token();

        let param_type = self.parse_type()?;
        params.push((name, param_type));

        while self.current_token.token_type == TokenType::Comma {
            self.next_token();

            if self.current_token.token_type == TokenType::Dot {
                if self.peek_token.token_type == TokenType::Dot {
                    self.next_token();
                    if self.peek_token.token_type == TokenType::Dot {
                        self.next_token();
                        is_var_args = true;
                        break;
                    }
                }
            }

            if self.current_token.token_type != TokenType::Identifier {
                self.errors.push(ParserError {
                    exception: "Expected parameter name",
                    help: "Expected identifier",
                    src: self.source.clone(),
                    span: self.current_token.span,
                });
                return None;
            }
            let name = self.current_token.value;
            self.next_token();

            if self.current_token.token_type != TokenType::DoubleDots {
                self.errors.push(ParserError {
                    exception: "Expected ':' after parameter name",
                    help: "Expected ':'",
                    src: self.source.clone(),
                    span: self.current_token.span,
                });
                return None;
            }
            self.next_token();

            let param_type = self.parse_type()?;
            params.push((name, param_type));
        }

        if self.current_token.token_type != TokenType::RParen {
            self.errors.push(ParserError {
                exception: "Expected ')' after parameters",
                help: "Expected ')'",
                src: self.source.clone(),
                span: self.current_token.span,
            });
            return None;
        }
        self.next_token();

        Some((params, is_var_args))
    }

    fn parse_import_statement(&mut self) -> Option<Statements<'a>> {
        let span_start = self.current_token.span.0;
        self.next_token(); // consume 'import'

        let path = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token.token_type == TokenType::Semicolon {
            self.next_token();
        }

        let span_end = self.current_token.span.1;

        Some(Statements::ImportStatement {
            path,
            span: (span_start, span_end),
        })
    }

    fn parse_include_statement(&mut self) -> Option<Statements<'a>> {
        let span_start = self.current_token.span.0;
        self.next_token(); // consume 'include'

        let path = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token.token_type == TokenType::Semicolon {
            self.next_token();
        }

        let span_end = self.current_token.span.1;

        Some(Statements::IncludeStatement {
            path,
            span: (span_start, span_end),
        })
    }

    fn parse_extern_statement(&mut self) -> Option<Statements<'a>> {
        let span_start = self.current_token.span.0;
        self.next_token(); // consume 'extern'

        if self.current_token.token_type != TokenType::String {
            self.errors.push(ParserError {
                exception: "Expected extern type string",
                help: "Expected string literal",
                src: self.source.clone(),
                span: self.current_token.span,
            });
            return None;
        }
        let extern_type = self.current_token.value;
        self.next_token();

        let public =
            self.current_token.token_type == TokenType::Keyword && self.current_token.value == "pub";
        if public {
            self.next_token();
        }

        let fn_statement = self.parse_fn_statement()?;

        if let Statements::FunctionDefineStatement {
            name,
            arguments,
            datatype,
            is_var_args,
            ..
        } = fn_statement
        {
            let span_end = self.current_token.span.1;
            Some(Statements::ExternStatement {
                identifier: name,
                arguments: arguments.into_iter().map(|(_, t)| t).collect(),
                return_type: datatype,
                extern_type,
                is_var_args,
                public,
                span: (span_start, span_end),
            })
        } else {
            None
        }
    }

    fn parse_struct_definition(&mut self) -> Option<Statements<'a>> {
        let span_start = self.current_token.span.0;
        let public = self.current_token.value == "pub";
        if public {
            self.next_token();
        }
        self.next_token(); // consume 'struct'

        if self.current_token.token_type != TokenType::Identifier {
            self.errors.push(ParserError {
                exception: "Expected struct name",
                help: "Expected identifier",
                src: self.source.clone(),
                span: self.current_token.span,
            });
            return None;
        }
        let name = self.current_token.value;
        self.next_token();

        if self.current_token.token_type != TokenType::LBrace {
            self.errors.push(ParserError {
                exception: "Expected '{' after struct name",
                help: "Expected '{'",
                src: self.source.clone(),
                span: self.current_token.span,
            });
            return None;
        }
        self.next_token();

        let mut fields = indexmap::IndexMap::new();
        let mut functions = indexmap::IndexMap::new();

        while self.current_token.token_type != TokenType::RBrace && self.current_token.token_type != TokenType::EOF {
            if self.current_token.token_type == TokenType::Keyword && self.current_token.value == "fn" {
                let function = self.parse_fn_statement()?;
                if let Statements::FunctionDefineStatement { name, .. } = function.clone() {
                    functions.insert(name, function);
                }
            } else if self.current_token.token_type == TokenType::Identifier {
                let field_name = self.current_token.value;
                self.next_token();

                if self.current_token.token_type != TokenType::DoubleDots {
                    self.errors.push(ParserError {
                        exception: "Expected ':' after field name",
                        help: "Expected ':'",
                        src: self.source.clone(),
                        span: self.current_token.span,
                    });
                    return None;
                }
                self.next_token();

                let field_type = self.parse_type()?;
                fields.insert(field_name, field_type);
            }

            if self.current_token.token_type == TokenType::Comma {
                self.next_token();
            } else if self.current_token.token_type != TokenType::RBrace {
                self.errors.push(ParserError {
                    exception: "Expected ',' or '}' after field",
                    help: "Expected ',' or '}'",
                    src: self.source.clone(),
                    span: self.current_token.span,
                });
                return None;
            }
        }

        let span_end = self.current_token.span.1;

        Some(Statements::StructDefineStatement {
            name,
            fields,
            functions,
            public,
            span: (span_start, span_end),
        })
    }

    fn parse_enum_definition(&mut self) -> Option<Statements<'a>> {
        let span_start = self.current_token.span.0;
        let public = self.current_token.value == "pub";
        if public {
            self.next_token();
        }
        self.next_token(); // consume 'enum'

        if self.current_token.token_type != TokenType::Identifier {
            self.errors.push(ParserError {
                exception: "Expected enum name",
                help: "Expected identifier",
                src: self.source.clone(),
                span: self.current_token.span,
            });
            return None;
        }
        let name = self.current_token.value;
        self.next_token();

        if self.current_token.token_type != TokenType::LBrace {
            self.errors.push(ParserError {
                exception: "Expected '{' after enum name",
                help: "Expected '{'",
                src: self.source.clone(),
                span: self.current_token.span,
            });
            return None;
        }
        self.next_token();

        let mut fields = vec![];
        let mut functions = indexmap::IndexMap::new();

        while self.current_token.token_type != TokenType::RBrace && self.current_token.token_type != TokenType::EOF {
            if self.current_token.token_type == TokenType::Keyword && self.current_token.value == "fn" {
                let function = self.parse_fn_statement()?;
                if let Statements::FunctionDefineStatement { name, .. } = function.clone() {
                    functions.insert(name, function);
                }
            } else if self.current_token.token_type == TokenType::Identifier {
                let field_name = self.current_token.value;
                fields.push(field_name);
                self.next_token();
            }

            if self.current_token.token_type == TokenType::Comma {
                self.next_token();
            } else if self.current_token.token_type != TokenType::RBrace {
                self.errors.push(ParserError {
                    exception: "Expected ',' or '}' after variant",
                    help: "Expected ',' or '}'",
                    src: self.source.clone(),
                    span: self.current_token.span,
                });
                return None;
            }
        }

        let span_end = self.current_token.span.1;

        Some(Statements::EnumDefineStatement {
            name,
            fields,
            functions,
            public,
            span: (span_start, span_end),
        })
    }

    fn parse_let_statement(&mut self) -> Option<Statements<'a>> {
        let span_start = self.current_token.span.0;
        self.next_token(); // consume 'let'

        if self.current_token.token_type != TokenType::Identifier {
            self.errors.push(ParserError {
                exception: "Expected identifier after 'let'",
                help: "Expected identifier",
                src: self.source.clone(),
                span: self.current_token.span,
            });
            return None;
        }
        let identifier = self.current_token.value;
        self.next_token();

        let mut datatype = None;
        if self.current_token.token_type == TokenType::DoubleDots {
            self.next_token();
            datatype = self.parse_type();
        }

        let mut value = None;
        if self.current_token.token_type == TokenType::Equal {
            self.next_token();
            value = self.parse_expression(Precedence::LOWEST);
        }

        if self.peek_token.token_type == TokenType::Semicolon {
            self.next_token();
        }

        let span_end = self.current_token.span.1;

        Some(Statements::AnnotationStatement {
            identifier,
            datatype,
            value,
            span: (span_start, span_end),
        })
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        match self.lexer.next() {
            Some(Ok(token)) => self.peek_token = token,
            Some(Err(_)) => {
                // TODO: Handle lexer error
            }
            None => {
                self.peek_token = Token {
                    value: "",
                    token_type: TokenType::EOF,
                    span: (0, 0),
                };
            }
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expressions<'a>> {
        let mut left_expr = match self.parse_prefix() {
            Ok(expr) => expr,
            Err(_) => return None,
        };

        while precedence < self.peek_precedence() {
            self.next_token();
            left_expr = self.parse_infix(left_expr.clone())?;
        }

        Some(left_expr)
    }

    fn parse_prefix(&mut self) -> Result<Expressions<'a>, ()> {
        match self.current_token.token_type {
            TokenType::Number => self.parse_integer_literal().ok_or(()),
            TokenType::FloatNumber => self.parse_float_literal().ok_or(()),
            TokenType::String => self.parse_string_literal().ok_or(()),
            TokenType::Boolean => self.parse_boolean_literal().ok_or(()),
            TokenType::Identifier => self.parse_identifier().ok_or(()),
            TokenType::Minus | TokenType::Not | TokenType::Ampersand | TokenType::Multiply => {
                self.parse_prefix_expression().ok_or(())
            }
            TokenType::LParen => self.parse_grouped_expression().ok_or(()),
            TokenType::LBrack => self.parse_array_literal().ok_or(()),
            _ => Err(()),
        }
    }

    fn parse_integer_literal(&mut self) -> Option<Expressions<'a>> {
        let value = match self.current_token.value.parse::<i64>() {
            Ok(v) => v,
            Err(_) => {
                self.errors.push(ParserError {
                    exception: "Could not parse integer",
                    help: "Invalid integer literal",
                    src: self.source.clone(),
                    span: self.current_token.span,
                });
                return None;
            }
        };

        Some(Expressions::Value(
            crate::value::Value::Integer(value),
            self.current_token.span,
        ))
    }

    fn parse_float_literal(&mut self) -> Option<Expressions<'a>> {
        let value = match self.current_token.value.parse::<f64>() {
            Ok(v) => v,
            Err(_) => {
                self.errors.push(ParserError {
                    exception: "Could not parse float",
                    help: "Invalid float literal",
                    src: self.source.clone(),
                    span: self.current_token.span,
                });
                return None;
            }
        };

        Some(Expressions::Value(
            crate::value::Value::Float(value),
            self.current_token.span,
        ))
    }

    fn parse_string_literal(&mut self) -> Option<Expressions<'a>> {
        Some(Expressions::Value(
            crate::value::Value::String(self.current_token.value),
            self.current_token.span,
        ))
    }

    fn parse_boolean_literal(&mut self) -> Option<Expressions<'a>> {
        let value = self.current_token.value == "true";
        Some(Expressions::Value(
            crate::value::Value::Boolean(value),
            self.current_token.span,
        ))
    }

    fn parse_identifier(&mut self) -> Option<Expressions<'a>> {
        if self.peek_token.token_type == TokenType::LBrace {
            return self.parse_struct_literal();
        }

        Some(Expressions::Value(
            crate::value::Value::Identifier(self.current_token.value),
            self.current_token.span,
        ))
    }

    fn parse_prefix_expression(&mut self) -> Option<Expressions<'a>> {
        let operator = self.current_token.value;
        let span_start = self.current_token.span.0;

        self.next_token();

        let right = self.parse_expression(Precedence::PREFIX);

        right.map(|right_expr| {
            let span_end = right_expr.get_span().1;
            match operator {
                "&" => Expressions::Reference {
                    object: Box::new(right_expr),
                    span: (span_start, span_end),
                },
                "*" => Expressions::Dereference {
                    object: Box::new(right_expr),
                    span: (span_start, span_end),
                },
                _ => Expressions::Unary {
                    operand: operator,
                    object: Box::new(right_expr),
                    span: (span_start, span_end),
                },
            }
        })
    }

    fn parse_infix(&mut self, left: Expressions<'a>) -> Option<Expressions<'a>> {
        match self.current_token.token_type {
            TokenType::Plus
            | TokenType::Minus
            | TokenType::Multiply
            | TokenType::Divide
            | TokenType::Modulus => self.parse_infix_expression(left),
            TokenType::Eq
            | TokenType::Ne
            | TokenType::Lt
            | TokenType::Bt
            | TokenType::Leq
            | TokenType::Beq => self.parse_infix_expression(left),
            TokenType::And
            | TokenType::Or
            | TokenType::LShift
            | TokenType::RShift
            | TokenType::Ampersand
            | TokenType::Verbar
            | TokenType::Xor => self.parse_infix_expression(left),
            TokenType::LParen => self.parse_call_expression(left),
            TokenType::LBrack => self.parse_slice_expression(left),
            TokenType::Dot => self.parse_sub_element_expression(left),
            TokenType::Not => self.parse_macro_call_expression(left),
            _ => None,
        }
    }

    fn parse_infix_expression(&mut self, left: Expressions<'a>) -> Option<Expressions<'a>> {
        let precedence = self.current_precedence();
        let operator = self.current_token.value;
        self.next_token();
        let right = self.parse_expression(precedence)?;

        let span = (left.get_span().0, right.get_span().1);

        let expression = match operator {
            "+" | "-" | "*" | "/" | "%" => Expressions::Binary {
                lhs: Box::new(left),
                operand: operator,
                rhs: Box::new(right),
                span,
            },
            "==" | "!=" | "<" | ">" | "<=" | ">=" => Expressions::Boolean {
                lhs: Box::new(left),
                operand: operator,
                rhs: Box::new(right),
                span,
            },
            "&" | "|" | "^" | "<<" | ">>" => Expressions::Bitwise {
                lhs: Box::new(left),
                operand: operator,
                rhs: Box::new(right),
                span,
            },
            _ => return None,
        };

        Some(expression)
    }

    fn parse_grouped_expression(&mut self) -> Option<Expressions<'a>> {
        self.next_token(); // consume '('

        if self.current_token.token_type == TokenType::RParen {
            // Empty tuple
            return Some(Expressions::Tuple {
                values: vec![],
                span: (
                    self.current_token.span.0 - 1,
                    self.current_token.span.1,
                ),
            });
        }

        let mut expressions = vec![];
        expressions.push(self.parse_expression(Precedence::LOWEST)?);

        while self.peek_token.token_type == TokenType::Comma {
            self.next_token();
            self.next_token();
            expressions.push(self.parse_expression(Precedence::LOWEST)?);
        }

        if self.peek_token.token_type != TokenType::RParen {
            self.errors.push(ParserError {
                exception: "Expected ')' after expression",
                help: "Expected ')'",
                src: self.source.clone(),
                span: self.peek_token.span,
            });
            return None;
        }

        self.next_token();

        if expressions.len() == 1 && self.current_token.token_type != TokenType::Comma {
            return Some(expressions.remove(0));
        }

        let span_start = expressions.first().unwrap().get_span().0;
        let span_end = expressions.last().unwrap().get_span().1;

        Some(Expressions::Tuple {
            values: expressions,
            span: (span_start, span_end),
        })
    }

    fn parse_call_expression(&mut self, function: Expressions<'a>) -> Option<Expressions<'a>> {
        let name = match function {
            Expressions::Value(crate::value::Value::Identifier(name), _) => name,
            _ => {
                self.errors.push(ParserError {
                    exception: "Expected function name",
                    help: "Expected identifier",
                    src: self.source.clone(),
                    span: function.get_span(),
                });
                return None;
            }
        };

        let arguments = self.parse_expression_list(TokenType::RParen)?;

        Some(Expressions::FnCall {
            name,
            arguments,
            span: (function.get_span().0, self.current_token.span.1),
        })
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Option<Vec<Expressions<'a>>> {
        let mut list = Vec::new();

        if self.peek_token.token_type == end {
            self.next_token();
            return Some(list);
        }

        self.next_token();
        list.push(self.parse_expression(Precedence::LOWEST)?);

        while self.peek_token.token_type == TokenType::Comma {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Precedence::LOWEST)?);
        }

        if self.peek_token.token_type != end {
            self.errors.push(ParserError {
                exception: "Expected end of list",
                help: "Expected token",
                src: self.source.clone(),
                span: self.peek_token.span,
            });
            return None;
        }

        self.next_token();

        Some(list)
    }

    fn parse_array_literal(&mut self) -> Option<Expressions<'a>> {
        let span_start = self.current_token.span.0;
        let values = self.parse_expression_list(TokenType::RBrack)?;
        let len = values.len();
        let span_end = self.current_token.span.1;
        Some(Expressions::Array {
            values,
            len,
            span: (span_start, span_end),
        })
    }

    fn peek_precedence(&self) -> Precedence {
        self.get_precedence(&self.peek_token.token_type)
    }

    fn current_precedence(&self) -> Precedence {
        self.get_precedence(&self.current_token.token_type)
    }

    fn parse_struct_literal(&mut self) -> Option<Expressions<'a>> {
        let name = self.current_token.value;
        let span_start = self.current_token.span.0;

        self.next_token(); // consume identifier
        self.next_token(); // consume '{'

        let mut fields = std::collections::HashMap::new();

        while self.current_token.token_type != TokenType::RBrace {
            if self.current_token.token_type != TokenType::Dot {
                self.errors.push(ParserError {
                    exception: "Expected '.' before field name",
                    help: "Expected '.'",
                    src: self.source.clone(),
                    span: self.current_token.span,
                });
                return None;
            }
            self.next_token();

            if self.current_token.token_type != TokenType::Identifier {
                self.errors.push(ParserError {
                    exception: "Expected field name",
                    help: "Expected identifier",
                    src: self.source.clone(),
                    span: self.current_token.span,
                });
                return None;
            }
            let field_name = self.current_token.value;
            self.next_token();

            if self.current_token.token_type != TokenType::Equal {
                self.errors.push(ParserError {
                    exception: "Expected '=' after field name",
                    help: "Expected '='",
                    src: self.source.clone(),
                    span: self.current_token.span,
                });
                return None;
            }
            self.next_token();

            let value = self.parse_expression(Precedence::LOWEST)?;
            fields.insert(field_name, value);

            if self.peek_token.token_type == TokenType::RBrace {
                self.next_token();
                break;
            }

            if self.peek_token.token_type != TokenType::Comma {
                self.errors.push(ParserError {
                    exception: "Expected ',' or '}' after field",
                    help: "Expected ',' or '}'",
                    src: self.source.clone(),
                    span: self.peek_token.span,
                });
                return None;
            }
            self.next_token();
            self.next_token();
        }

        let span_end = self.current_token.span.1;

        Some(Expressions::Struct {
            name,
            fields,
            span: (span_start, span_end),
        })
    }

    fn parse_macro_call_expression(&mut self, macro_expr: Expressions<'a>) -> Option<Expressions<'a>> {
        let name = match macro_expr {
            Expressions::Value(crate::value::Value::Identifier(name), _) => name,
            _ => {
                self.errors.push(ParserError {
                    exception: "Expected macro name",
                    help: "Expected identifier",
                    src: self.source.clone(),
                    span: macro_expr.get_span(),
                });
                return None;
            }
        };

        if self.peek_token.token_type != TokenType::LParen {
            self.errors.push(ParserError {
                exception: "Expected '(' after macro name",
                help: "Expected '('",
                src: self.source.clone(),
                span: self.peek_token.span,
            });
            return None;
        }
        self.next_token();

        let arguments = self.parse_expression_list(TokenType::RParen)?;

        Some(Expressions::MacroCall {
            name,
            arguments,
            span: (macro_expr.get_span().0, self.current_token.span.1),
        })
    }

    fn parse_sub_element_expression(&mut self, left: Expressions<'a>) -> Option<Expressions<'a>> {
        self.next_token(); // Consume the dot

        let mut subelements = vec![];
        if let Some(expr) = self.parse_expression(Precedence::MEMBER) {
            subelements.push(expr);
        } else {
            return None; // or handle error
        }

        if subelements.is_empty() {
            return None;
        }
        let span_end = subelements.last().unwrap().get_span().1;

        Some(Expressions::SubElement {
            head: Box::new(left.clone()),
            subelements,
            span: (left.get_span().0, span_end),
        })
    }

    fn parse_slice_expression(&mut self, left: Expressions<'a>) -> Option<Expressions<'a>> {
        let span_start = left.get_span().0;
        self.next_token();
        let index = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token.token_type != TokenType::RBrack {
            self.errors.push(ParserError {
                exception: "Expected ']' after slice index",
                help: "Expected ']'",
                src: self.source.clone(),
                span: self.peek_token.span,
            });
            return None;
        }
        self.next_token();

        let span_end = self.current_token.span.1;

        Some(Expressions::Slice {
            object: Box::new(left),
            index: Box::new(index),
            span: (span_start, span_end),
        })
    }

    fn get_precedence(&self, token_type: &TokenType) -> Precedence {
        match token_type {
            TokenType::Equal => Precedence::ASSIGN,
            TokenType::Plus | TokenType::Minus => Precedence::SUM,
            TokenType::Divide | TokenType::Multiply | TokenType::Modulus => Precedence::PRODUCT,
            TokenType::Eq | TokenType::Ne => Precedence::EQUALS,
            TokenType::Lt | TokenType::Bt | TokenType::Leq | TokenType::Beq => Precedence::LESSGREATER,
            TokenType::And => Precedence::LOGICALAND,
            TokenType::Or => Precedence::LOGICALOR,
            TokenType::LShift | TokenType::RShift => Precedence::SHIFT,
            TokenType::Ampersand => Precedence::BITWISEAND,
            TokenType::Verbar => Precedence::BITWISEOR,
            TokenType::Xor => Precedence::BITWISEXOR,
            TokenType::LParen => Precedence::CALL,
            TokenType::LBrack => Precedence::INDEX,
            TokenType::Dot => Precedence::MEMBER,
            _ => Precedence::LOWEST,
        }
    }

    fn parse_type(&mut self) -> Option<Type<'a>> {
        self.parse_prefix_type()
    }

    fn parse_prefix_type(&mut self) -> Option<Type<'a>> {
        match self.current_token.token_type {
            TokenType::Multiply => {
                self.next_token();
                let inner_type = self.parse_type()?;
                Some(Type::Pointer(Box::new(inner_type)))
            }
            TokenType::Type => {
                let type_name = self.current_token.value;
                self.next_token();
                match type_name {
                    "i8" => Some(Type::I8),
                    "i16" => Some(Type::I16),
                    "i32" => Some(Type::I32),
                    "i64" => Some(Type::I64),
                    "u8" => Some(Type::U8),
                    "u16" => Some(Type::U16),
                    "u32" => Some(Type::U32),
                    "u64" => Some(Type::U64),
                    "usize" => Some(Type::USIZE),
                    "f32" => Some(Type::F32),
                    "f64" => Some(Type::F64),
                    "bool" => Some(Type::Bool),
                    "char" => Some(Type::Char),
                    "void" => Some(Type::Void),
                    _ => unreachable!(), // Should be handled by lexer
                }
            }
            TokenType::Identifier => {
                let alias = self.current_token.value;
                self.next_token();
                Some(Type::Alias(alias))
            }
            TokenType::LBrack => self.parse_array_type_syntax(),
            TokenType::LParen => self.parse_tuple_type(),
            _ => {
                self.errors.push(ParserError {
                    exception: "Invalid type",
                    help: "Expected a type",
                    src: self.source.clone(),
                    span: self.current_token.span,
                });
                None
            }
        }
    }

    fn parse_array_type_syntax(&mut self) -> Option<Type<'a>> {
        self.next_token(); // Consume '['

        if self.current_token.token_type == TokenType::RBrack {
            // Dynamic array: []T
            self.next_token(); // Consume ']'
            let inner_type = self.parse_type()?;
            return Some(Type::DynamicArray(Box::new(inner_type)));
        }

        // Sized array: [T; size]
        let inner_type = self.parse_type()?;

        if self.current_token.token_type != TokenType::Semicolon {
            self.errors.push(ParserError {
                exception: "Expected ';' in array type",
                help: "Expected ';'",
                src: self.source.clone(),
                span: self.current_token.span,
            });
            return None;
        }
        self.next_token(); // Consume ';'

        if self.current_token.token_type != TokenType::Number {
            self.errors.push(ParserError {
                exception: "Expected number for array size",
                help: "Expected an integer literal",
                src: self.source.clone(),
                span: self.current_token.span,
            });
            return None;
        }
        let size = self.current_token.value.parse().unwrap_or(0);
        self.next_token(); // Consume size

        if self.current_token.token_type != TokenType::RBrack {
            self.errors.push(ParserError {
                exception: "Expected ']' at the end of array type",
                help: "Expected ']'",
                src: self.source.clone(),
                span: self.current_token.span,
            });
            return None;
        }
        self.next_token(); // Consume ']'

        Some(Type::Array(Box::new(inner_type), size))
    }

    fn parse_tuple_type(&mut self) -> Option<Type<'a>> {
        self.next_token(); // Consume '('
        let mut types = Vec::new();

        if self.current_token.token_type == TokenType::RParen {
            self.next_token(); // Consume ')'
            return Some(Type::Tuple(types));
        }

        types.push(self.parse_type()?);

        while self.peek_token.token_type == TokenType::Comma {
            self.next_token(); // Consume ','
            self.next_token();
            types.push(self.parse_type()?);
        }

        if self.peek_token.token_type != TokenType::RParen {
            self.errors.push(ParserError {
                exception: "Expected ')' at the end of tuple type",
                help: "Expected ')'",
                src: self.source.clone(),
                span: self.peek_token.span,
            });
            return None;
        }
        self.next_token(); // Consume ')'

        Some(Type::Tuple(types))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_statement() {
        let input = "let x = 5;";
        let mut parser = Parser::new(input, "test.pay");
        let program = parser.parse().unwrap();

        assert_eq!(program.len(), 1);
        let statement = &program[0];

        if let Statements::AnnotationStatement {
            identifier,
            value,
            ..
        } = statement
        {
            assert_eq!(*identifier, "x");
            if let Some(Expressions::Value(crate::value::Value::Integer(5), _)) = value {
                // pass
            } else {
                panic!("Wrong value for x");
            }
        } else {
            panic!("Wrong statement type");
        }
    }

    #[test]
    fn test_return_statement() {
        let input = "return 5;";
        let mut parser = Parser::new(input, "test.pay");
        let program = parser.parse().unwrap();

        assert_eq!(program.len(), 1);
        let statement = &program[0];

        if let Statements::ReturnStatement { value, .. } = statement {
            if let Expressions::Value(crate::value::Value::Integer(5), _) = value {
                // pass
            } else {
                panic!("Wrong value for return");
            }
        } else {
            panic!("Wrong statement type");
        }
    }

    #[test]
    fn test_infix_expression() {
        let input = "5 + 5;";
        let mut parser = Parser::new(input, "test.pay");
        let program = parser.parse().unwrap();

        assert_eq!(program.len(), 1);
        let statement = &program[0];

        if let Statements::Expression(Expressions::Binary { lhs, operand, rhs, .. }) = statement {
            assert_eq!(*operand, "+");
            if let Expressions::Value(crate::value::Value::Integer(5), _) = **lhs {
                // pass
            } else {
                panic!("Wrong lhs value");
            }
            if let Expressions::Value(crate::value::Value::Integer(5), _) = **rhs {
                // pass
            } else {
                panic!("Wrong rhs value");
            }
        } else {
            panic!("Wrong statement type");
        }
    }

    #[test]
    fn test_operator_precedence() {
        let input = "-a * b";
        let mut parser = Parser::new(input, "test.pay");
        let program = parser.parse().unwrap();
        assert_eq!(program.len(), 1);
        let statement = &program[0];

        if let Statements::Expression(Expressions::Binary { lhs, operand, rhs, .. }) = statement {
            assert_eq!(*operand, "*");
            if let Expressions::Unary { operand, .. } = &**lhs {
                assert_eq!(*operand, "-");
            } else {
                panic!("lhs is not unary expression");
            }
            if let Expressions::Value(crate::value::Value::Identifier("b"), _) = &**rhs {
                // pass
            } else {
                panic!("rhs is not identifier");
            }
        } else {
            panic!("Wrong statement type");
        }
    }

    #[test]
    fn test_function_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let mut parser = Parser::new(input, "test.pay");
        let program = parser.parse().unwrap();

        assert_eq!(program.len(), 1);
        let statement = &program[0];

        if let Statements::Expression(Expressions::FnCall { name, arguments, .. }) = statement {
            assert_eq!(*name, "add");
            assert_eq!(arguments.len(), 3);
        } else {
            panic!("Wrong statement type");
        }
    }

    #[test]
    fn test_array_literal() {
        let input = "[1, 2 * 2, 3 + 3]";
        let mut parser = Parser::new(input, "test.pay");
        let program = parser.parse().unwrap();

        assert_eq!(program.len(), 1);
        let statement = &program[0];

        if let Statements::Expression(Expressions::Array { values, .. }) = statement {
            assert_eq!(values.len(), 3);
        } else {
            panic!("Wrong statement type");
        }
    }

    #[test]
    fn test_slice_expression() {
        let input = "myArray[1 + 1]";
        let mut parser = Parser::new(input, "test.pay");
        let program = parser.parse().unwrap();

        assert_eq!(program.len(), 1);
        let statement = &program[0];

        if let Statements::Expression(Expressions::Slice { object, index, .. }) = statement {
            if let Expressions::Value(crate::value::Value::Identifier("myArray"), _) = &**object {
                // pass
            } else {
                panic!("Wrong object in slice expression");
            }

            if let Expressions::Binary { .. } = &**index {
                // pass
            } else {
                panic!("Wrong index in slice expression");
            }
        } else {
            panic!("Wrong statement type");
        }
    }

    #[test]
    fn test_struct_literal() {
        let input = "MyStruct { .a = 1, .b = 2 }";
        let mut parser = Parser::new(input, "test.pay");
        let program = parser.parse().unwrap();

        assert_eq!(program.len(), 1);
        let statement = &program[0];

        if let Statements::Expression(Expressions::Struct { name, fields, .. }) = statement {
            assert_eq!(*name, "MyStruct");
            assert_eq!(fields.len(), 2);
        } else {
            panic!("Wrong statement type");
        }
    }

    #[test]
    fn test_if_else_statement() {
        let input = "if (x < y) { x } else { y }";
        let mut parser = Parser::new(input, "test.pay");
        let program = parser.parse().unwrap();

        assert_eq!(program.len(), 1);
        let statement = &program[0];

        if let Statements::IfStatement {
            condition,
            then_block,
            else_block,
            ..
        } = statement
        {
            if let Expressions::Boolean { .. } = condition {
                // pass
            } else {
                panic!("Wrong condition type");
            }
            assert_eq!(then_block.len(), 1);
            assert!(else_block.is_some());
            assert_eq!(else_block.clone().unwrap().len(), 1);
        } else {
            panic!("Wrong statement type");
        }
    }

    #[test]
    fn test_while_statement() {
        let input = "while (x < y) { x = x + 1 }";
        let mut parser = Parser::new(input, "test.pay");
        let program = parser.parse().unwrap();

        assert_eq!(program.len(), 1);
        let statement = &program[0];

        if let Statements::WhileStatement {
            condition,
            block,
            ..
        } = statement
        {
            if let Expressions::Boolean { .. } = condition {
                // pass
            } else {
                panic!("Wrong condition type");
            }
            assert_eq!(block.len(), 1);
        } else {
            panic!("Wrong statement type");
        }
    }

    #[test]
    fn test_for_statement() {
        let input = "for i = 0 { x }";
        let mut parser = Parser::new(input, "test.pay");
        let program = parser.parse().unwrap();

        assert_eq!(program.len(), 1);
        let statement = &program[0];

        if let Statements::ForStatement {
            binding,
            iterator,
            block,
            ..
        } = statement
        {
            assert_eq!(*binding, "i");
            if let Expressions::Value(crate::value::Value::Integer(0), _) = iterator {
                // pass
            } else {
                panic!("Wrong iterator type");
            }
            assert_eq!(block.len(), 1);
        } else {
            panic!("Wrong statement type");
        }
    }

    #[test]
    fn test_fn_definition() {
        let input = "fn add(a: i32, b: i32) i32 { return a + b; }";
        let mut parser = Parser::new(input, "test.pay");
        let program = parser.parse().unwrap();

        assert_eq!(program.len(), 1);
        let statement = &program[0];

        if let Statements::FunctionDefineStatement {
            name,
            arguments,
            datatype,
            block,
            ..
        } = statement
        {
            assert_eq!(*name, "add");
            assert_eq!(arguments.len(), 2);
            if let Type::I32 = datatype {
                // pass
            } else {
                panic!("Wrong return type");
            }
            assert_eq!(block.len(), 1);
        } else {
            panic!("Wrong statement type");
        }
    }

    #[test]
    fn test_struct_definition() {
        let input = "struct Point { x: i32, y: i32 }";
        let mut parser = Parser::new(input, "test.pay");
        let program = parser.parse().unwrap();

        assert_eq!(program.len(), 1);
        let statement = &program[0];

        if let Statements::StructDefineStatement { name, fields, .. } = statement {
            assert_eq!(*name, "Point");
            assert_eq!(fields.len(), 2);
        } else {
            panic!("Wrong statement type");
        }
    }

    #[test]
    fn test_enum_definition() {
        let input = "enum Color { Red, Green, Blue }";
        let mut parser = Parser::new(input, "test.pay");
        let program = parser.parse().unwrap();

        assert_eq!(program.len(), 1);
        let statement = &program[0];

        if let Statements::EnumDefineStatement { name, fields, .. } = statement {
            assert_eq!(*name, "Color");
            assert_eq!(fields.len(), 3);
        } else {
            panic!("Wrong statement type");
        }
    }
}
