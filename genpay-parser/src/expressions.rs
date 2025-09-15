use crate::{statements::Statements, types::Type, value::Value};
use bumpalo::collections::Vec;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Expressions<'bump> {
    /// `OBJECT BINOP EXPRESSION`
    Binary {
        operand: &'bump str,
        lhs: &'bump Expressions<'bump>,
        rhs: &'bump Expressions<'bump>,
        span: (usize, usize),
    },
    /// `UNOP OBJECT`
    Unary {
        operand: &'bump str,
        object: &'bump Expressions<'bump>,
        span: (usize, usize),
    },

    /// `OBJECT BOOLOP EXPRESSION`
    Boolean {
        operand: &'bump str,
        lhs: &'bump Expressions<'bump>,
        rhs: &'bump Expressions<'bump>,
        span: (usize, usize),
    },
    /// `OBJECT BITOP EXPRESSION`
    Bitwise {
        operand: &'bump str,
        lhs: &'bump Expressions<'bump>,
        rhs: &'bump Expressions<'bump>,
        span: (usize, usize),
    },

    /// `IDENTIFIER: TYPE`
    Argument {
        name: &'bump str,
        r#type: Type<'bump>,
        span: (usize, usize),
    },
    /// `OBJECT.SUBELEMENT_1.SUBELEMENT_2`
    SubElement {
        head: &'bump Expressions<'bump>,
        subelements: Vec<'bump, Expressions<'bump>>,
        span: (usize, usize),
    },

    /// `IDENTIFIER ( EXPRESSION, EXPRESSION, ... )`
    FnCall {
        name: &'bump str,
        arguments: Vec<'bump, Expressions<'bump>>,
        span: (usize, usize),
    },
    /// `IDENTIFIER! ( EXPRESSION, EXPRESSION, ... )`
    MacroCall {
        name: &'bump str,
        arguments: Vec<'bump, Expressions<'bump>>,
        span: (usize, usize),
    },

    /// `&EXPRESSION`
    Reference {
        object: &'bump Expressions<'bump>,
        span: (usize, usize),
    },

    /// `*EXPRESSION`
    Dereference {
        object: &'bump Expressions<'bump>,
        span: (usize, usize),
    },

    /// `[EXPRESSION, EXPRESSION, ...]`
    Array {
        values: Vec<'bump, Expressions<'bump>>,
        len: usize,
        span: (usize, usize),
    },
    /// `(EXPRESSION, EXPRESSION, ...)`
    Tuple {
        values: Vec<'bump, Expressions<'bump>>,
        span: (usize, usize),
    },
    /// `OBJECT[EXPRESSION]`
    Slice {
        object: &'bump Expressions<'bump>,
        index: &'bump Expressions<'bump>,
        span: (usize, usize),
    },
    /// `IDENTIFIER { .IDENTIFIER = EXPRESSION, .IDENTIFIER = EXPRESSION }`
    Struct {
        name: &'bump str,
        fields: HashMap<&'bump str, Expressions<'bump>>,
        span: (usize, usize),
    },
    /// `{ STATEMENTS }`
    Scope {
        block: Vec<'bump, Statements<'bump>>,
        span: (usize, usize),
    },

    Value(Value<'bump>, (usize, usize)),
    None,
}

impl<'bump> Expressions<'bump> {
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
