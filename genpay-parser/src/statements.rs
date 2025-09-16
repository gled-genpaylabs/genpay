use crate::{expressions::Expressions, types::Type};
use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Statements {
    /// `OBJECT = EXPRESSION`
    AssignStatement {
        object: Expressions,
        value: Expressions,
        span: (usize, usize),
    },

    /// `OBJECT BINOP= EXPRESSION`
    BinaryAssignStatement {
        object: Expressions,
        operand: String,
        value: Expressions,
        span: (usize, usize),
    },

    /// `*OBJECT = EXPRESSION`
    DerefAssignStatement {
        object: Expressions,
        value: Expressions,
        span: (usize, usize),
    },

    /// `OBJECT[EXPRESSION] = EXPRESSION`
    SliceAssignStatement {
        object: Expressions,
        index: Expressions,
        value: Expressions,
        span: (usize, usize),
    },

    /// `OBJECT.FIELD= EXPRESSION`
    FieldAssignStatement {
        object: Expressions,
        value: Expressions,
        span: (usize, usize),
    },

    /// `let IDENTIFIER = EXPRESSION`
    AnnotationStatement {
        identifier: String,
        datatype: Option<Type>,
        value: Option<Expressions>,
        span: (usize, usize),
    },

    /// `pub/NOTHING fn IDENTIFIER ( IDENTIFIER: TYPE, IDENTIFIER: TYPE, ... ) TYPE/NOTHING { STATEMENTS }`
    FunctionDefineStatement {
        name: String,
        datatype: Type,
        arguments: Vec<(String, Type)>,
        block: Vec<Statements>,
        public: bool,
        is_var_args: bool,
        span: (usize, usize),
        header_span: (usize, usize),
    },
    /// `NAME ( EXPRESSION, EXPRESSION, ... )`
    FunctionCallStatement {
        name: String,
        arguments: Vec<Expressions>,
        span: (usize, usize),
    },

    /// `MACRONAME! ( EXPRESSION, EXPRESSION, ... )`
    MacroCallStatement {
        name: String,
        arguments: Vec<Expressions>,
        span: (usize, usize),
    },

    /// ```text
    /// struct IDENTIFIER {
    ///     IDENTIFIER: TYPE,
    ///     ...,
    ///
    ///     (FunctionDefineStatement)
    /// }
    /// ```
    StructDefineStatement {
        name: String,
        fields: BTreeMap<String, Type>,
        functions: BTreeMap<String, Vec<Statements>>,
        public: bool,
        span: (usize, usize),
    },

    /// ```text
    /// enum IDENTIFIER {
    ///     IDENTIFIER,
    ///     ...,
    ///
    ///     (FunctionDefineStatement)
    /// }
    /// ```
    EnumDefineStatement {
        name: String,
        fields: Vec<String>,
        functions: BTreeMap<String, Vec<Statements>>,
        public: bool,
        span: (usize, usize),
    },

    /// `typedef IDENTIFIER TYPE`
    TypedefStatement {
        alias: String,
        datatype: Type,
        span: (usize, usize),
    },

    /// `if EXPRESSION { STATEMENTS } else { STATEMENTS }`
    IfStatement {
        condition: Expressions,
        then_block: Vec<Statements>,
        else_block: Option<Vec<Statements>>,
        span: (usize, usize),
    },

    /// `while EXPRESSION { STATEMENTS }`
    WhileStatement {
        condition: Expressions,
        block: Vec<Statements>,
        span: (usize, usize),
    },

    /// `for IDENTIFIER = OBJECT { STATEMENTS }`
    ForStatement {
        binding: String,
        iterator: Expressions,
        block: Vec<Statements>,
        span: (usize, usize),
    },

    /// `import "PATH"`
    ImportStatement {
        path: Expressions,
        span: (usize, usize),
    },

    /// `include "PATH"`
    IncludeStatement {
        path: Expressions,
        span: (usize, usize),
    },

    /// `extern "EXT_TYPE" pub/NOTHING fn IDENTIFIER ( TYPE, TYPE, ... ) TYPE/NOTHING`
    ExternStatement {
        identifier: String,
        arguments: Vec<Type>,
        return_type: Type,
        extern_type: String,
        is_var_args: bool,
        public: bool,
        span: (usize, usize),
    },

    /// `_extern_declare IDENTIFIER EXPRESSION`
    ExternDeclareStatement {
        identifier: String,
        datatype: Type,
        span: (usize, usize),
    },

    /// `_link_c "PATH"`
    LinkCStatement {
        path: Expressions,
        span: (usize, usize),
    },

    /// `break`
    BreakStatements {
        span: (usize, usize),
    },

    /// `return EXPRESSION`
    ReturnStatement {
        value: Expressions,
        span: (usize, usize),
    },

    /// `{ STATEMENTS }`
    ScopeStatement {
        block: Vec<Statements>,
        span: (usize, usize),
    },

    Expression(Expressions),
    None,
}

impl Statements {
    pub fn get_span(&self) -> (usize, usize) {
        match self {
            Statements::AssignStatement { span, .. } => *span,
            Statements::BinaryAssignStatement { span, .. } => *span,
            Statements::DerefAssignStatement { span, .. } => *span,
            Statements::SliceAssignStatement { span, .. } => *span,
            Statements::FieldAssignStatement { span, .. } => *span,
            Statements::AnnotationStatement { span, .. } => *span,
            Statements::FunctionDefineStatement { span, .. } => *span,
            Statements::FunctionCallStatement { span, .. } => *span,
            Statements::MacroCallStatement { span, .. } => *span,
            Statements::StructDefineStatement { span, .. } => *span,
            Statements::EnumDefineStatement { span, .. } => *span,
            Statements::TypedefStatement { span, .. } => *span,
            Statements::IfStatement { span, .. } => *span,
            Statements::WhileStatement { span, .. } => *span,
            Statements::ForStatement { span, .. } => *span,
            Statements::ImportStatement { span, .. } => *span,
            Statements::IncludeStatement { span, .. } => *span,
            Statements::ExternDeclareStatement { span, .. } => *span,
            Statements::LinkCStatement { span, .. } => *span,
            Statements::ExternStatement { span, .. } => *span,
            Statements::BreakStatements { span, .. } => *span,
            Statements::ReturnStatement { value: _, span } => *span,
            Statements::ScopeStatement { span, .. } => *span,
            Statements::Expression(expr) => expr.get_span(),
            Statements::None => (0, 0),
        }
    }
}
