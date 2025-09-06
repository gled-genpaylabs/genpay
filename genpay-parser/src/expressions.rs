use crate::{
    BINARY_OPERATORS, BITWISE_OPERATORS, BOOLEAN_OPERATORS, PRECEDENCE_BOOLEAN_OPERATORS,
    PRECEDENCE_BOOLEAN_OPERATORS, Parser,
    error::{self, ParserError},
    statements::Statements,
    types::Type,
    value::Value,
};
use bumpalo::collections::Vec as BumpVec;
use genpay_lexer::token_type::TokenType;
use std::collections::HashMap;

pub trait Spannable {
    fn span(&self) -> (usize, usize);
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expressions<'a> {
    /// `OBJECT BINOP EXPRESSION`
    Binary {
        operand: TokenType,
        lhs: &'a Expressions<'a>,
        rhs: &'a Expressions<'a>,
        span: (usize, usize),
    },
    /// `UNOP OBJECT`
    Unary {
        operand: TokenType,
        object: &'a Expressions<'a>,
        span: (usize, usize),
    },

    /// `OBJECT BOOLOP EXPRESSION`
    Boolean {
        operand: TokenType,
        lhs: &'a Expressions<'a>,
        rhs: &'a Expressions<'a>,
        span: (usize, usize),
    },
    /// `OBJECT BITOP EXPRESSION`
    Bitwise {
        operand: TokenType,
        lhs: &'a Expressions<'a>,
        rhs: &'a Expressions<'a>,
        span: (usize, usize),
    },

    /// `IDENTIFIER: TYPE`
    Argument {
        name: &'a str,
        r#type: Type<'a>,
        span: (usize, usize),
    },
    /// `OBJECT.SUBELEMENT_1.SUBELEMENT_2`
    SubElement {
        head: &'a Expressions<'a>,
        subelements: BumpVec<'a, Expressions<'a>>,
        span: (usize, usize),
    },

    /// `IDENTIFIER ( EXPRESSION, EXPRESSION, ... )`
    FnCall {
        name: &'a str,
        arguments: BumpVec<'a, Expressions<'a>>,
        span: (usize, usize),
    },
    /// `IDENTIFIER! ( EXPRESSION, EXPRESSION, ... )`
    MacroCall {
        name: &'a str,
        arguments: BumpVec<'a, Expressions<'a>>,
        span: (usize, usize),
    },

    /// `&EXPRESSION`
    Reference {
        object: &'a Expressions<'a>,
        span: (usize, usize),
    },

    /// `*EXPRESSION`
    Dereference {
        object: &'a Expressions<'a>,
        span: (usize, usize),
    },

    /// `[EXPRESSION, EXPRESSION, ...]`
    Array {
        values: BumpVec<'a, Expressions<'a>>,
        len: usize,
        span: (usize, usize),
    },
    /// `(EXPRESSION, EXPRESSION, ...)`
    Tuple {
        values: &'a [Expressions<'a>],
        span: (usize, usize),
    },
    /// `OBJECT[EXPRESSION]`
    Slice {
        object: &'a Expressions<'a>,
        index: &'a Expressions<'a>,
        span: (usize, usize),
    },
    /// `IDENTIFIER { .IDENTIFIER = EXPRESSION, .IDENTIFIER = EXPRESSION }`
    Struct {
        name: &'a str,
        fields: HashMap<&'a str, Expressions<'a>>,
        span: (usize, usize),
    },
    /// `{ STATEMENTS }`
    Scope {
        block: &'a [Statements<'a>],
        span: (usize, usize),
    },

    Value(Value<'a>, (usize, usize)),
    None,
}

impl<'a> Spannable for Expressions<'a> {
    fn span(&self) -> (usize, usize) {
        match self {
            Expressions::Binary { span, .. } => *span,
            Expressions::Boolean { span, .. } => *span,
            Expressions::Bitwise { span, .. } => *span,
            Expressions::Argument { span, .. } => *span,
            Expressions::SubElement { span, .. } => *span,
            Expressions::FnCall { span, .. } => *span,
            Expressions::MacroCall { span, .. } => *span,
            Expressions::Reference { span, .. } => *span,
            Expressions::Dereference { span, .. } => *span,
            Expressions::Array { span, .. } => *span,
            Expressions::Tuple { span, .. } => *span,
            Expressions::Slice { span, .. } => *span,
            Expressions::Struct { span, .. } => *span,
            Expressions::Unary { span, .. } => *span,
            Expressions::Scope { span, .. } => *span,
            Expressions::Value(_, span) => *span,
            Expressions::None => (0, 0),
        }
    }
}


use bumpalo::Bump;

impl<'a> Parser<'a> {
    pub fn subelement_expression(
        &mut self,
        head: Expressions<'a>,
        separator: TokenType,
        expr_arena: &'a Bump,
        stmt_arena: &'a Bump,
    ) -> Expressions<'a> {
        let head_span = head.span();
        let head = expr_arena.alloc(head);
        let mut subelements = BumpVec::new_in(expr_arena);
        let mut end_span = head_span.1;

        loop {
            if !self.expect(separator.clone()) {
                break;
            }
            let _ = self.next();

            let term = self.term(expr_arena, stmt_arena);
            end_span = term.span().1;
            subelements.push(term);
        }

        Expressions::SubElement {
            head,
            subelements,
            span: (head_span.0, end_span),
        }
    }

    pub fn binary_expression(&mut self, node: Expressions<'a>, expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Expressions<'a> {
        let node_span = node.span();
        let current = self.current();

        match current.token_type {
            ref tty if BINARY_OPERATORS.contains(&tty) => {
                let _ = self.next();

                let lhs = node;
                let rhs = self.expression(expr_arena, stmt_arena);
                let span_end = rhs.span().1;

                if PRECEDENCE_BOOLEAN_OPERATORS.contains(&tty) {
                    let new_node = rhs.clone();
                    let old_lhs = lhs.clone();

                    if let Expressions::Binary {
                        operand,
                        lhs,
                        rhs,
                        span,
                    } = new_node
                    {
                        let lhs_new = expr_arena.alloc(old_lhs);
                        let rhs_new = lhs;

                        let precedence_node = Expressions::Binary {
                            operand: current.token_type.clone(),
                            lhs: lhs_new,
                            rhs: rhs_new,
                            span,
                        };

                        return Expressions::Binary {
                            operand,
                            lhs: expr_arena.alloc(precedence_node),
                            rhs,
                            span: (node_span.0, span_end),
                        };
                    }
                }

                Expressions::Binary {
                    operand: current.token_type,
                    lhs: expr_arena.alloc(lhs),
                    rhs: expr_arena.alloc(rhs),
                    span: (node_span.0, span_end),
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn boolean_expression(&mut self, node: Expressions<'a>, expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Expressions<'a> {
        // FIXME: Expressions like `true || false` returns error "Undefined term found"

        let node_span = node.span();
        let current = self.current();

        match current.token_type {
            ref op if PRECEDENCE_BOOLEAN_OPERATORS.contains(&op) => node,
            ref op if BOOLEAN_OPERATORS.contains(&op) => {
                let _ = self.next();

                let lhs = node.clone();
                let rhs = self.expression(expr_arena, stmt_arena);
                let span_end = rhs.span().1;

                if PRECEDENCE_BOOLEAN_OPERATORS.contains(&self.current().token_type) {
                    let operand = self.current().token_type.clone();
                    let lhs_node = Expressions::Boolean {
                        operand: current.token_type.clone(),
                        lhs: expr_arena.alloc(lhs),
                        rhs: expr_arena.alloc(rhs),
                        span: (current.span.0, self.current().span.1),
                    };

                    let _ = self.next();
                    let rhs_node = self.expression(expr_arena, stmt_arena);

                    return Expressions::Boolean {
                        operand,
                        lhs: expr_arena.alloc(lhs_node),
                        rhs: expr_arena.alloc(rhs_node),
                        span: (node_span.0, span_end),
                    };
                }

                Expressions::Boolean {
                    operand: current.token_type,
                    lhs: expr_arena.alloc(lhs),
                    rhs: expr_arena.alloc(rhs),
                    span: (node_span.0, span_end),
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn bitwise_expression(&mut self, node: Expressions<'a>, expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Expressions<'a> {
        let node_span = node.span();
        let current = self.current();

        match current.token_type {
            ref tty if BITWISE_OPERATORS.contains(&tty) => {
                let _ = self.next();

                let lhs = expr_arena.alloc(node);
                let rhs = expr_arena.alloc(self.expression(expr_arena, stmt_arena));
                let span_end = rhs.span().1;

                Expressions::Bitwise {
                    operand: current.token_type,
                    lhs,
                    rhs,
                    span: (node_span.0, span_end),
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn call_expression(&mut self, fname: &'a str, span: (usize, usize), expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Expressions<'a> {
        match self.current().token_type {
            TokenType::Identifier => {
                let _ = self.next();
            }
            TokenType::LParen => {}
            _ => {
                self.error(ParserError::UnknownExpression {
                    exception: "unknown call expression syntax".to_string(),
                    help: "Consider using right syntax: `identifier(value, ...)".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span((span.0, self.current().span.1)),
                });

                return Expressions::None;
            }
        };

        let arguments =
            self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma, expr_arena, stmt_arena);

        let span_end = if let Some(last_arg) = arguments.last() {
            last_arg.span().1
        } else {
            self.current().span.0
        };

        Expressions::FnCall {
            name: fname,
            arguments,
            span: (span.0, span_end),
        }
    }

    pub fn macrocall_expression(&mut self, name: &'a str, span: (usize, usize), expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Expressions<'a> {
        if self.expect(TokenType::Not) {
            let _ = self.next();
        }

        let arguments =
            self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma, expr_arena, stmt_arena);

        let span_end = if let Some(last_arg) = arguments.last() {
            last_arg.span().1
        } else {
            self.current().span.0
        };

        Expressions::MacroCall {
            name,
            arguments,
            span: (span.0, span_end),
        }
    }

    pub fn slice_expression(&mut self, expr: Expressions<'a>, expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Expressions<'a> {
        if let TokenType::LBrack = self.current().token_type {
            let _ = self.next();
        }

        let object = expr_arena.alloc(expr.clone());
        let index = expr_arena.alloc(self.expression(expr_arena, stmt_arena));

        if self.current().token_type != TokenType::RBrack {
            self.error(ParserError::UnclosedExpression {
                exception: "unclosed slice index".to_string(),
                help: "Close slice index with brackets".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((
                    expr.span().0,
                    self.current().span.1,
                )),
            });

            return Expressions::None;
        }

        let _ = self.next();
        let span_end = self.current().span.1;
        Expressions::Slice {
            object,
            index,
            span: (expr.span().0, span_end),
        }
    }

    pub fn struct_expression(&mut self, name: &'a str, expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Expressions<'a> {
        let span_start = self.current().span.0;
        if self.expect(TokenType::Identifier) {
            let _ = self.next();
        }

        if !self.expect(TokenType::LBrace) {
            self.error(ParserError::SyntaxError {
                exception: "expected curly brackets for structure initialization".to_string(),
                help: "Use syntax: `identifier { .field = value, ... }`".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });

            self.skip_statement();
            return Expressions::None;
        };

        let _ = self.next();
        let mut fields = HashMap::new();

        while !self.expect(TokenType::RBrace) {
            if !self.expect(TokenType::Dot) {
                self.error(ParserError::SyntaxError {
                    exception: "wrong field initialization syntax".to_string(),
                    help: "Use syntax: `identifier { .field = value, ... }`".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span(self.current().span),
                });

                while !self.expect(TokenType::RBrace)
                    && !self.expect(TokenType::Semicolon)
                    && !self.expect(TokenType::EOF)
                {
                    let _ = self.next();
                }
                break;
            }

            let _ = self.next();
            if !self.expect(TokenType::Identifier) {
                self.error(ParserError::SyntaxError {
                    exception: "wrong field initialization syntax".to_string(),
                    help: "Use syntax: `identifier { .field = value, ... }`".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span(self.current().span),
                });

                while !self.expect(TokenType::RBrace)
                    && !self.expect(TokenType::Semicolon)
                    && !self.expect(TokenType::EOF)
                {
                    let _ = self.next();
                }
                break;
            }

            let id = self.current().value;
            let _ = self.next();

            if !self.expect(TokenType::Equal) {
                self.error(ParserError::SyntaxError {
                    exception: "wrong field initialization syntax".to_string(),
                    help: "Use syntax: `identifier { .field = value, ... }`".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span(self.current().span),
                });

                while !self.expect(TokenType::RBrace)
                    && !self.expect(TokenType::Semicolon)
                    && !self.expect(TokenType::EOF)
                {
                    let _ = self.next();
                }
                break;
            }

            let _ = self.next();
            let value = self.expression(expr_arena, stmt_arena);

            if !self.expect(TokenType::Comma) && !self.expect(TokenType::RBrace) {
                self.error(ParserError::SyntaxError {
                    exception: "values must be separated by commas".to_string(),
                    help: "Separate fields with commas".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span(self.current().span),
                });

                while !self.expect(TokenType::RBrace)
                    && !self.expect(TokenType::Semicolon)
                    && !self.expect(TokenType::EOF)
                {
                    let _ = self.next();
                }
                break;
            }

            if !self.expect(TokenType::RBrace) {
                let _ = self.next();
            }
            fields.insert(id, value);
        }

        if self.expect(TokenType::RBrace) {
            let _ = self.next();
        }

        let span = (span_start, self.current().span.1);
        Expressions::Struct { name, fields, span }
    }

    pub fn expressions_enum(
        &mut self,
        start: TokenType,
        end: TokenType,
        separator: TokenType,
        expr_arena: &'a Bump,
        stmt_arena: &'a Bump,
    ) -> BumpVec<'a, Expressions<'a>> {
        if self.expect(start) {
            let _ = self.next();
        } else if self.expect(end.clone()) {
            let _ = self.next();
            return BumpVec::new_in(expr_arena);
        }

        let mut output = BumpVec::new_in(expr_arena);

        loop {
            if self.expect(end.clone()) {
                break;
            }

            output.push(self.expression(expr_arena, stmt_arena));

            if self.expect(separator.clone()) {
                let _ = self.next();
            } else if !self.expect(end.clone()) {
                break;
            }
        }

        if self.expect(end) {
            let _ = self.next();
        }

        output
    }
}
