use crate::{statements::Statements, types::Type, value::Value};
use std::{collections::HashMap, string::String};

#[derive(Debug, Clone, PartialEq)]
pub enum Expressions {
    /// `OBJECT BINOP EXPRESSION`
    Binary {
        operand: String,
        lhs: Box<Expressions>,
        rhs: Box<Expressions>,
        span: (usize, usize),
    },
    /// `UNOP OBJECT`
    Unary {
        operand: String,
        object: Box<Expressions>,
        span: (usize, usize),
    },

    /// `OBJECT BOOLOP EXPRESSION`
    Boolean {
        operand: String,
        lhs: Box<Expressions>,
        rhs: Box<Expressions>,
        span: (usize, usize),
    },
    /// `OBJECT BITOP EXPRESSION`
    Bitwise {
        operand: String,
        lhs: Box<Expressions>,
        rhs: Box<Expressions>,
        span: (usize, usize),
    },

    /// `IDENTIFIER: TYPE`
    Argument {
        name: String,
        r#type: Type,
        span: (usize, usize),
    },
    /// `OBJECT.SUBELEMENT_1.SUBELEMENT_2`
    SubElement {
        head: Box<Expressions>,
        subelements: Vec<Expressions>,
        span: (usize, usize),
    },

    /// `IDENTIFIER ( EXPRESSION, EXPRESSION, ... )`
    FnCall {
        name: String,
        arguments: Vec<Expressions>,
        span: (usize, usize),
    },
    /// `IDENTIFIER! ( EXPRESSION, EXPRESSION, ... )`
    MacroCall {
        name: String,
        arguments: Vec<Expressions>,
        span: (usize, usize),
    },

    /// `&EXPRESSION`
    Reference {
        object: Box<Expressions>,
        span: (usize, usize),
    },

    /// `*EXPRESSION`
    Dereference {
        object: Box<Expressions>,
        span: (usize, usize),
    },

    /// `[EXPRESSION, EXPRESSION, ...]`
    Array {
        values: Vec<Expressions>,
        len: usize,
        span: (usize, usize),
    },
    /// `(EXPRESSION, EXPRESSION, ...)`
    Tuple {
        values: Vec<Expressions>,
        span: (usize, usize),
    },
    /// `OBJECT[EXPRESSION]`
    Slice {
        object: Box<Expressions>,
        index: Box<Expressions>,
        span: (usize, usize),
    },
    /// `IDENTIFIER { .IDENTIFIER = EXPRESSION, .IDENTIFIER = EXPRESSION }`
    Struct {
        name: String,
        fields: HashMap<String, Expressions>,
        span: (usize, usize),
    },
    /// `{ STATEMENTS }`
    Scope {
        block: Vec<Statements>,
        span: (usize, usize),
    },

    Value(Value, (usize, usize)),
    None,
}

impl Expressions {
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
