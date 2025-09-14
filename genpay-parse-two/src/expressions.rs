use crate::{statements::Statements, types::Type, value::Value};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Expressions<'a> {
    /// `OBJECT BINOP EXPRESSION`
    Binary {
        operand: &'a str,
        lhs: Box<Expressions<'a>>,
        rhs: Box<Expressions<'a>>,
        span: (usize, usize),
    },
    /// `UNOP OBJECT`
    Unary {
        operand: &'a str,
        object: Box<Expressions<'a>>,
        span: (usize, usize),
    },

    /// `OBJECT BOOLOP EXPRESSION`
    Boolean {
        operand: &'a str,
        lhs: Box<Expressions<'a>>,
        rhs: Box<Expressions<'a>>,
        span: (usize, usize),
    },
    /// `OBJECT BITOP EXPRESSION`
    Bitwise {
        operand: &'a str,
        lhs: Box<Expressions<'a>>,
        rhs: Box<Expressions<'a>>,
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
        head: Box<Expressions<'a>>,
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
        object: Box<Expressions<'a>>,
        span: (usize, usize),
    },

    /// `*EXPRESSION`
    Dereference {
        object: Box<Expressions<'a>>,
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
        values: Vec<Expressions<'a>>,
        span: (usize, usize),
    },
    /// `OBJECT[EXPRESSION]`
    Slice {
        object: Box<Expressions<'a>>,
        index: Box<Expressions<'a>>,
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
        block: Vec<Statements<'a>>,
        span: (usize, usize),
    },

    Value(Value<'a>, (usize, usize)),
    None,
}

impl<'a> Expressions<'a> {
    pub fn get_span(&self) -> (usize, usize) {
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
