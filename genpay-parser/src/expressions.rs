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

#[derive(Debug, Clone)]
pub enum Expressions<'a> {
    /// `OBJECT BINOP EXPRESSION`
    Binary {
        operand: &'a str,
        lhs: &'a Expressions<'a>,
        rhs: &'a Expressions<'a>,
        span: (usize, usize),
    },
    /// `UNOP OBJECT`
    Unary {
        operand: &'a str,
        object: &'a Expressions<'a>,
        span: (usize, usize),
    },

    /// `OBJECT BOOLOP EXPRESSION`
    Boolean {
        operand: &'a str,
        lhs: &'a Expressions<'a>,
        rhs: &'a Expressions<'a>,
        span: (usize, usize),
    },
    /// `OBJECT BITOP EXPRESSION`
    Bitwise {
        operand: &'a str,
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
        subelements: Vec<Expressions<'a>>,
        span: (usize, usize),
    },

    /// `IDENTIFIER ( EXPRESSION, EXPRESSION, ... )`
    FnCall {
        name: &'a str,
        arguments: Vec<Expressions<'a>>,
        span: (usize, usize),
    },
    /// `IDENTIFIER! ( EXPRESSION, EXPRESSION, ... )`
    MacroCall {
        name: &'a str,
        arguments: Vec<Expressions<'a>>,
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
        values: Vec<Expressions<'a>>,
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

impl<'a> PartialEq for Expressions<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Expressions::Binary {
                    operand: o1,
                    lhs: lhs1,
                    rhs: rhs1,
                    ..
                },
                Expressions::Binary {
                    operand: o2,
                    lhs: lhs2,
                    rhs: rhs2,
                    ..
                },
            ) => o1 == o2 && lhs1 == lhs2 && rhs1 == rhs2,
            (
                Expressions::Unary {
                    operand: o1,
                    object: obj1,
                    ..
                },
                Expressions::Unary {
                    operand: o2,
                    object: obj2,
                    ..
                },
            ) => o1 == o2 && obj1 == obj2,
            (
                Expressions::Boolean {
                    operand: o1,
                    lhs: lhs1,
                    rhs: rhs1,
                    ..
                },
                Expressions::Boolean {
                    operand: o2,
                    lhs: lhs2,
                    rhs: rhs2,
                    ..
                },
            ) => o1 == o2 && lhs1 == lhs2 && rhs1 == rhs2,
            (
                Expressions::Bitwise {
                    operand: o1,
                    lhs: lhs1,
                    rhs: rhs1,
                    ..
                },
                Expressions::Bitwise {
                    operand: o2,
                    lhs: lhs2,
                    rhs: rhs2,
                    ..
                },
            ) => o1 == o2 && lhs1 == lhs2 && rhs1 == rhs2,
            (
                Expressions::Argument {
                    name: n1,
                    r#type: t1,
                    ..
                },
                Expressions::Argument {
                    name: n2,
                    r#type: t2,
                    ..
                },
            ) => n1 == n2 && t1 == t2,
            (
                Expressions::SubElement {
                    head: h1,
                    subelements: s1,
                    ..
                },
                Expressions::SubElement {
                    head: h2,
                    subelements: s2,
                    ..
                },
            ) => *h1 == *h2 && s1 == s2,
            (
                Expressions::FnCall {
                    name: n1,
                    arguments: a1,
                    ..
                },
                Expressions::FnCall {
                    name: n2,
                    arguments: a2,
                    ..
                },
            ) => n1 == n2 && a1 == a2,
            (
                Expressions::MacroCall {
                    name: n1,
                    arguments: a1,
                    ..
                },
                Expressions::MacroCall {
                    name: n2,
                    arguments: a2,
                    ..
                },
            ) => n1 == n2 && a1 == a2,
            (Expressions::Reference { object: o1, .. }, Expressions::Reference { object: o2, .. }) => {
                o1 == o2
            }
            (
                Expressions::Dereference { object: o1, .. },
                Expressions::Dereference { object: o2, .. },
            ) => o1 == o2,
            (
                Expressions::Array {
                    values: v1,
                    len: l1,
                    ..
                },
                Expressions::Array {
                    values: v2,
                    len: l2,
                    ..
                },
            ) => l1 == l2 && v1 == v2,
            (Expressions::Tuple { values: v1, .. }, Expressions::Tuple { values: v2, .. }) => v1 == v2,
            (
                Expressions::Slice {
                    object: o1,
                    index: i1,
                    ..
                },
                Expressions::Slice {
                    object: o2,
                    index: i2,
                    ..
                },
            ) => o1 == o2 && i1 == i2,
            (
                Expressions::Struct {
                    name: n1,
                    fields: f1,
                    ..
                },
                Expressions::Struct {
                    name: n2,
                    fields: f2,
                    ..
                },
            ) => n1 == n2 && f1 == f2,
            (Expressions::Scope { block: b1, .. }, Expressions::Scope { block: b2, .. }) => b1 == b2,
            (Expressions::Value(v1, _), Expressions::Value(v2, _)) => v1 == v2,
            (Expressions::None, Expressions::None) => true,
            _ => false,
        }
    }
}

impl<'a> Parser<'a> {
    #[inline]
    pub fn get_span_expression(expr: &Expressions<'a>) -> (usize, usize) {
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
    pub fn span_expression(&self, expr: &Expressions<'a>) -> (usize, usize) {
        Self::get_span_expression(expr)
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
        let head_span = Self::get_span_expression(&head);
        let head = expr_arena.alloc(head);
        let mut subelements = Vec::new();
        let mut end_span = head_span.1;

        loop {
            if !self.expect(separator.clone()) {
                break;
            }
            let _ = self.next();

            let term = self.term(expr_arena, stmt_arena);
            end_span = Self::get_span_expression(&term).1;
            subelements.push(term);
        }

        Expressions::SubElement {
            head,
            subelements,
            span: (head_span.0, end_span),
        }
    }

    pub fn binary_expression(&mut self, node: Expressions<'a>, expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Expressions<'a> {
        let node_span = Self::get_span_expression(&node);
        let current = self.current();

        match current.token_type {
            tty if BINARY_OPERATORS.contains(&tty) => {
                let _ = self.next();

                let lhs = node;
                let rhs = self.expression(expr_arena, stmt_arena);
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
                        let lhs_new = expr_arena.alloc(old_lhs);
                        let rhs_new = lhs;

                        let priority_node = Expressions::Binary {
                            operand: current.value,
                            lhs: lhs_new,
                            rhs: rhs_new,
                            span,
                        };

                        return Expressions::Binary {
                            operand,
                            lhs: expr_arena.alloc(priority_node),
                            rhs,
                            span: (node_span.0, span_end),
                        };
                    }
                }

                Expressions::Binary {
                    operand: current.value,
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

        let node_span = Self::get_span_expression(&node);
        let current = self.current();

        match current.token_type {
            op if PRIORITY_BOOLEAN_OPERATORS.contains(&op) => node,
            op if BOOLEAN_OPERATORS.contains(&op) => {
                let _ = self.next();

                let lhs = node.clone();
                let rhs = self.expression(expr_arena, stmt_arena);
                let span_end = Self::get_span_expression(&rhs).1;

                if PRIORITY_BOOLEAN_OPERATORS.contains(&self.current().token_type) {
                    let operand = self.current().value;
                    let lhs_node = Expressions::Boolean {
                        operand: current.value,
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
                    operand: current.value,
                    lhs: expr_arena.alloc(lhs),
                    rhs: expr_arena.alloc(rhs),
                    span: (node_span.0, span_end),
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn bitwise_expression(&mut self, node: Expressions<'a>, expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Expressions<'a> {
        let node_span = Self::get_span_expression(&node);
        let current = self.current();

        match current.token_type {
            tty if BITWISE_OPERATORS.contains(&tty) => {
                let _ = self.next();

                let lhs = expr_arena.alloc(node);
                let rhs = expr_arena.alloc(self.expression(expr_arena, stmt_arena));
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

    pub fn macrocall_expression(&mut self, name: &'a str, span: (usize, usize), expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Expressions<'a> {
        if self.expect(TokenType::Not) {
            let _ = self.next();
        }

        let arguments =
            self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma, expr_arena, stmt_arena);

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
                    self.span_expression(&expr).0,
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
            span: (self.span_expression(&expr).0, span_end),
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
    ) -> Vec<Expressions<'a>> {
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
