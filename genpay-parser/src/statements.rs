use crate::{expressions::Expressions, types::Type};
use bumpalo::collections::Vec;
use indexmap::IndexMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Statements<'bump> {
    /// `OBJECT = EXPRESSION`
    AssignStatement {
        object: Expressions<'bump>,
        value: Expressions<'bump>,
        span: (usize, usize),
    },

    /// `OBJECT BINOP= EXPRESSION`
    BinaryAssignStatement {
        object: Expressions<'bump>,
        operand: &'bump str,
        value: Expressions<'bump>,
        span: (usize, usize),
    },

    /// `*OBJECT = EXPRESSION`
    DerefAssignStatement {
        object: Expressions<'bump>,
        value: Expressions<'bump>,
        span: (usize, usize),
    },

    /// `OBJECT[EXPRESSION] = EXPRESSION`
    SliceAssignStatement {
        object: Expressions<'bump>,
        index: Expressions<'bump>,
        value: Expressions<'bump>,
        span: (usize, usize),
    },

    /// `OBJECT.FIELD= EXPRESSION`
    FieldAssignStatement {
        object: Expressions<'bump>,
        value: Expressions<'bump>,
        span: (usize, usize),
    },

    /// `let IDENTIFIER = EXPRESSION`
    AnnotationStatement {
        identifier: &'bump str,
        datatype: Option<Type<'bump>>,
        value: Option<Expressions<'bump>>,
        span: (usize, usize),
    },

    /// `pub/NOTHING fn IDENTIFIER ( IDENTIFIER: TYPE, IDENTIFIER: TYPE, ... ) TYPE/NOTHING { STATEMENTS }`
    FunctionDefineStatement {
        name: &'bump str,
        datatype: Type<'bump>,
        arguments: Vec<'bump, (&'bump str, Type<'bump>)>,
        block: Vec<'bump, Statements<'bump>>,
        public: bool,
        is_var_args: bool,
        span: (usize, usize),
        header_span: (usize, usize),
    },
    /// `NAME ( EXPRESSION, EXPRESSION, ... )`
    FunctionCallStatement {
        name: &'bump str,
        arguments: Vec<'bump, Expressions<'bump>>,
        span: (usize, usize),
    },

    /// `MACRONAME! ( EXPRESSION, EXPRESSION, ... )`
    MacroCallStatement {
        name: &'bump str,
        arguments: Vec<'bump, Expressions<'bump>>,
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
        name: &'bump str,
        fields: IndexMap<&'bump str, Type<'bump>>,
        functions: IndexMap<&'bump str, Vec<'bump, Statements<'bump>>>,
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
        name: &'bump str,
        fields: Vec<'bump, &'bump str>,
        functions: IndexMap<&'bump str, Vec<'bump, Statements<'bump>>>,
        public: bool,
        span: (usize, usize),
    },

    /// `typedef IDENTIFIER TYPE`
    TypedefStatement {
        alias: &'bump str,
        datatype: Type<'bump>,
        span: (usize, usize),
    },

    /// `if EXPRESSION { STATEMENTS } else { STATEMENTS }`
    IfStatement {
        condition: Expressions<'bump>,
        then_block: Vec<'bump, Statements<'bump>>,
        else_block: Option<Vec<'bump, Statements<'bump>>>,
        span: (usize, usize),
    },

    /// `while EXPRESSION { STATEMENTS }`
    WhileStatement {
        condition: Expressions<'bump>,
        block: Vec<'bump, Statements<'bump>>,
        span: (usize, usize),
    },

    /// `for IDENTIFIER = OBJECT { STATEMENTS }`
    ForStatement {
        binding: &'bump str,
        iterator: Expressions<'bump>,
        block: Vec<'bump, Statements<'bump>>,
        span: (usize, usize),
    },

    /// `import "PATH"`
    ImportStatement {
        path: Expressions<'bump>,
        span: (usize, usize),
    },

    /// `include "PATH"`
    IncludeStatement {
        path: Expressions<'bump>,
        span: (usize, usize),
    },

    /// `extern "EXT_TYPE" pub/NOTHING fn IDENTIFIER ( TYPE, TYPE, ... ) TYPE/NOTHING`
    ExternStatement {
        identifier: &'bump str,
        arguments: Vec<'bump, Type<'bump>>,
        return_type: Type<'bump>,
        extern_type: &'bump str,
        is_var_args: bool,
        public: bool,
        span: (usize, usize),
    },

    /// `_extern_declare IDENTIFIER EXPRESSION`
    ExternDeclareStatement {
        identifier: &'bump str,
        datatype: Type<'bump>,
        span: (usize, usize),
    },

    /// `_link_c "PATH"`
    LinkCStatement {
        path: Expressions<'bump>,
        span: (usize, usize),
    },

    /// `break`
    BreakStatements {
        span: (usize, usize),
    },

    /// `return EXPRESSION`
    ReturnStatement {
        value: Expressions<'bump>,
        span: (usize, usize),
    },

    /// `{ STATEMENTS }`
    ScopeStatement {
        block: Vec<'bump, Statements<'bump>>,
        span: (usize, usize),
    },

    Expression(Expressions<'bump>),
    None,
}

impl<'bump> Statements<'bump> {
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
