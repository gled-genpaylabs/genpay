//! # Expressions
//! Each expression in Deen has its own syntax (such as Statements). <br/>
//! _To see syntax rules for every Expression, check the [`Expressions`] enum_
//!
//! **Expression** is a syntactic entity in programming language that combines types and values
//! into single instance. <br/>
//! Expressions can be components of [`Statements`]. <br/>
//! Read: <https://en.wikipedia.org/wiki/Expression_(computer_science)>

use crate::{
    BINARY_OPERATORS, BITWISE_OPERATORS, BOOLEAN_OPERATORS, PRIORITY_BINARY_OPERATORS,
    PRIORITY_BOOLEAN_OPERATORS, Parser,
    error::{self, ParserError},
    statements::Statements,
    types::Type,
    value::Value,
};
use genpay_lexer::token_type::TokenType;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Expressions<'s> {
    /// `OBJECT BINOP EXPRESSION`
    Binary {
        operand: &'s str,
        lhs: Box<Expressions<'s>>,
        rhs: Box<Expressions<'s>>,
        span: (usize, usize),
    },
    /// `UNOP OBJECT`
    Unary {
        operand: &'s str,
        object: Box<Expressions<'s>>,
        span: (usize, usize),
    },

    /// `OBJECT BOOLOP EXPRESSION`
    Boolean {
        operand: &'s str,
        lhs: Box<Expressions<'s>>,
        rhs: Box<Expressions<'s>>,
        span: (usize, usize),
    },
    /// `OBJECT BITOP EXPRESSION`
    Bitwise {
        operand: &'s str,
        lhs: Box<Expressions<'s>>,
        rhs: Box<Expressions<'s>>,
        span: (usize, usize),
    },

    /// `IDENTIFIER: TYPE`
    Argument {
        name: &'s str,
        r#type: Type<'s>,
        span: (usize, usize),
    },
    /// `OBJECT.SUBELEMENT_1.SUBELEMENT_2`
    SubElement {
        head: Box<Expressions<'s>>,
        subelements: Vec<Expressions<'s>>,
        span: (usize, usize),
    },

    /// `IDENTIFIER ( EXPRESSION, EXPRESSION, ... )`
    FnCall {
        name: &'s str,
        arguments: Vec<Expressions<'s>>,
        span: (usize, usize),
    },
    /// `IDENTIFIER! ( EXPRESSION, EXPRESSION, ... )`
    MacroCall {
        name: &'s str,
        arguments: Vec<Expressions<'s>>,
        span: (usize, usize),
    },

    /// `&EXPRESSION`
    Reference {
        object: Box<Expressions<'s>>,
        span: (usize, usize),
    },

    /// `*EXPRESSION`
    Dereference {
        object: Box<Expressions<'s>>,
        span: (usize, usize),
    },

    /// `[EXPRESSION, EXPRESSION, ...]`
    Array {
        values: Vec<Expressions<'s>>,
        len: usize,
        span: (usize, usize),
    },
    /// `(EXPRESSION, EXPRESSION, ...)`
    Tuple {
        values: Box<[Expressions<'s>]>,
        span: (usize, usize),
    },
    /// `OBJECT[EXPRESSION]`
    Slice {
        object: Box<Expressions<'s>>,
        index: Box<Expressions<'s>>,
        span: (usize, usize),
    },
    /// `IDENTIFIER { .IDENTIFIER = EXPRESSION, .IDENTIFIER = EXPRESSION }`
    Struct {
        name: &'s str,
        fields: HashMap<&'s str, Expressions<'s>>,
        span: (usize, usize),
    },
    /// `{ STATEMENTS }`
    Scope {
        block: Box<[Statements<'s>]>,
        span: (usize, usize),
    },

    Value(Value<'s>, (usize, usize)),
    None,
}

impl<'s> Parser<'s> {
    #[inline]
    pub fn get_span_expression(expr: &Expressions<'s>) -> (usize, usize) {
        match expr {
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

    #[inline]
    pub fn span_expression(&self, expr: Expressions<'s>) -> (usize, usize) {
        Self::get_span_expression(&expr)
    }
}

impl<'s> Parser<'s> {
    pub fn subelement_expression(
        &mut self,
        head: Expressions<'s>,
        separator: TokenType,
    ) -> Expressions<'s> {
        let head_span = Self::get_span_expression(&head);
        let head = Box::new(head);
        let mut subelements = Vec::new();
        let mut end_span = head_span.1;

        loop {
            if !self.expect(separator.clone()) {
                break;
            }
            let _ = self.next();

            let term = self.term();
            end_span = Self::get_span_expression(&term).1;
            subelements.push(term);
        }

        Expressions::SubElement {
            head,
            subelements,
            span: (head_span.0, end_span),
        }
    }

    pub fn binary_expression(&mut self, node: Expressions<'s>) -> Expressions<'s> {
        let node_span = Self::get_span_expression(&node);
        let current = self.current();

        match current.token_type {
            tty if BINARY_OPERATORS.contains(&tty) => {
                let _ = self.next();

                let lhs = node;
                let rhs = self.expression();
                let span_end = Self::get_span_expression(&rhs).1;

                if PRIORITY_BINARY_OPERATORS.contains(&tty) {
                    let new_node = rhs.clone();
                    let old_lhs = lhs.clone();

                    if let Expressions::Binary {
                        operand,
                        lhs,
                        rhs,
                        span,
                    } = new_node
                    {
                        let lhs_new = Box::new(old_lhs);
                        let rhs_new = lhs;

                        let priority_node = Expressions::Binary {
                            operand: current.value,
                            lhs: lhs_new,
                            rhs: rhs_new,
                            span,
                        };

                        return Expressions::Binary {
                            operand,
                            lhs: Box::new(priority_node),
                            rhs,
                            span: (node_span.0, span_end),
                        };
                    }
                }

                Expressions::Binary {
                    operand: current.value,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    span: (node_span.0, span_end),
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn boolean_expression(&mut self, node: Expressions<'s>) -> Expressions<'s> {
        // FIXME: Expressions like `true || false` returns error "Undefined term found"

        let node_span = Self::get_span_expression(&node);
        let current = self.current();

        match current.token_type {
            op if PRIORITY_BOOLEAN_OPERATORS.contains(&op) => node,
            op if BOOLEAN_OPERATORS.contains(&op) => {
                let _ = self.next();

                let lhs = node.clone();
                let rhs = self.expression();
                let span_end = Self::get_span_expression(&rhs).1;

                if PRIORITY_BOOLEAN_OPERATORS.contains(&self.current().token_type) {
                    let operand = self.current().value;
                    let lhs_node = Expressions::Boolean {
                        operand: current.value,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        span: (current.span.0, self.current().span.1),
                    };

                    let _ = self.next();
                    let rhs_node = self.expression();

                    return Expressions::Boolean {
                        operand,
                        lhs: Box::new(lhs_node),
                        rhs: Box::new(rhs_node),
                        span: (node_span.0, span_end),
                    };
                }

                Expressions::Boolean {
                    operand: current.value,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    span: (node_span.0, span_end),
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn bitwise_expression(&mut self, node: Expressions<'s>) -> Expressions<'s> {
        let node_span = Self::get_span_expression(&node);
        let current = self.current();

        match current.token_type {
            tty if BITWISE_OPERATORS.contains(&tty) => {
                let _ = self.next();

                let lhs = Box::new(node);
                let rhs = Box::new(self.expression());
                let span_end = Self::get_span_expression(&rhs).1;

                Expressions::Bitwise {
                    operand: current.value,
                    lhs,
                    rhs,
                    span: (node_span.0, span_end),
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn call_expression(&mut self, fname: &'s str, span: (usize, usize)) -> Expressions<'s> {
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
            self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma);

        let span_end = if let Some(last_arg) = arguments.last() {
            Self::get_span_expression(last_arg).1
        } else {
            self.current().span.0
        };

        Expressions::FnCall {
            name: fname,
            arguments,
            span: (span.0, span_end),
        }
    }

    pub fn macrocall_expression(&mut self, name: &'s str, span: (usize, usize)) -> Expressions<'s> {
        if self.expect(TokenType::Not) {
            let _ = self.next();
        }

        let arguments =
            self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma);

        let span_end = if let Some(last_arg) = arguments.last() {
            Self::get_span_expression(last_arg).1
        } else {
            self.current().span.0
        };

        Expressions::MacroCall {
            name,
            arguments,
            span: (span.0, span_end),
        }
    }

    pub fn slice_expression(&mut self, expr: Expressions<'s>) -> Expressions<'s> {
        if let TokenType::LBrack = self.current().token_type {
            let _ = self.next();
        }

        let object = Box::new(expr.clone());
        let index = Box::new(self.expression());

        if self.current().token_type != TokenType::RBrack {
            self.error(ParserError::UnclosedExpression {
                exception: "unclosed slice index".to_string(),
                help: "Close slice index with brackets".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((
                    self.span_expression(expr).0,
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
            span: (self.span_expression(expr).0, span_end),
        }
    }

    pub fn struct_expression(&mut self, name: &'s str) -> Expressions<'s> {
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
            let value = self.expression();

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
    ) -> Vec<Expressions<'s>> {
        if self.expect(start) {
            let _ = self.next();
        } else if self.expect(end.clone()) {
            let _ = self.next();
            return Vec::new();
        }

        let mut output = Vec::new();

        loop {
            if self.expect(end.clone()) {
                break;
            }

            output.push(self.expression());

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
