use crate::{expressions::Expressions, types::Type};
use indexmap::IndexMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Statements<'a> {
    /// `OBJECT = EXPRESSION`
    AssignStatement {
        object: Expressions<'a>,
        value: Expressions<'a>,
        span: (usize, usize),
    },

    /// `OBJECT BINOP= EXPRESSION`
    BinaryAssignStatement {
        object: Expressions<'a>,
        operand: &'a str,
        value: Expressions<'a>,
        span: (usize, usize),
    },

    /// `*OBJECT = EXPRESSION`
    DerefAssignStatement {
        object: Expressions<'a>,
        value: Expressions<'a>,
        span: (usize, usize),
    },

    /// `OBJECT[EXPRESSION] = EXPRESSION`
    SliceAssignStatement {
        object: Expressions<'a>,
        index: Expressions<'a>,
        value: Expressions<'a>,
        span: (usize, usize),
    },

    /// `OBJECT.FIELD= EXPRESSION`
    FieldAssignStatement {
        object: Expressions<'a>,
        value: Expressions<'a>,
        span: (usize, usize),
    },

    /// `let IDENTIFIER = EXPRESSION`
    AnnotationStatement {
        identifier: &'a str,
        datatype: Option<Type<'a>>,
        value: Option<Expressions<'a>>,
        span: (usize, usize),
    },

    /// `pub/NOTHING fn IDENTIFIER ( IDENTIFIER: TYPE, IDENTIFIER: TYPE, ... ) TYPE/NOTHING { STATEMENTS }`
    FunctionDefineStatement {
        name: &'a str,
        datatype: Type<'a>,
        arguments: Vec<(&'a str, Type<'a>)>,
        block: Vec<Statements<'a>>,
        public: bool,
        is_var_args: bool,
        span: (usize, usize),
        header_span: (usize, usize),
    },
    /// `NAME ( EXPRESSION, EXPRESSION, ... )`
    FunctionCallStatement {
        name: &'a str,
        arguments: Vec<Expressions<'a>>,
        span: (usize, usize),
    },

    /// `MACRONAME! ( EXPRESSION, EXPRESSION, ... )`
    MacroCallStatement {
        name: &'a str,
        arguments: Vec<Expressions<'a>>,
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
        name: &'a str,
        fields: IndexMap<&'a str, Type<'a>>,
        functions: IndexMap<&'a str, Statements<'a>>,
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
        name: &'a str,
        fields: Vec<&'a str>,
        functions: IndexMap<&'a str, Statements<'a>>,
        public: bool,
        span: (usize, usize),
    },

    /// `typedef IDENTIFIER TYPE`
    TypedefStatement {
        alias: &'a str,
        datatype: Type<'a>,
        span: (usize, usize),
    },

    /// `if EXPRESSION { STATEMENTS } else { STATEMENTS }`
    IfStatement {
        condition: Expressions<'a>,
        then_block: Vec<Statements<'a>>,
        else_block: Option<Vec<Statements<'a>>>,
        span: (usize, usize),
    },

    /// `while EXPRESSION { STATEMENTS }`
    WhileStatement {
        condition: Expressions<'a>,
        block: Vec<Statements<'a>>,
        span: (usize, usize),
    },

    /// `for IDENTIFIER = OBJECT { STATEMENTS }`
    ForStatement {
        binding: &'a str,
        iterator: Expressions<'a>,
        block: Vec<Statements<'a>>,
        span: (usize, usize),
    },

    /// `import "PATH"`
    ImportStatement {
        path: Expressions<'a>,
        span: (usize, usize),
    },

    /// `include "PATH"`
    IncludeStatement {
        path: Expressions<'a>,
        span: (usize, usize),
    },

    /// `extern "EXT_TYPE" pub/NOTHING fn IDENTIFIER ( TYPE, TYPE, ... ) TYPE/NOTHING`
    ExternStatement {
        identifier: &'a str,
        arguments: Vec<Type<'a>>,
        return_type: Type<'a>,
        extern_type: &'a str,
        is_var_args: bool,
        public: bool,
        span: (usize, usize),
    },

    /// `_extern_declare IDENTIFIER EXPRESSION`
    ExternDeclareStatement {
        identifier: &'a str,
        datatype: Type<'a>,
        span: (usize, usize),
    },

    /// `_link_c "PATH"`
    LinkCStatement {
        path: Expressions<'a>,
        span: (usize, usize),
    },

    /// `break`
    BreakStatements {
        span: (usize, usize),
    },

    /// `return EXPRESSION`
    ReturnStatement {
        value: Expressions<'a>,
        span: (usize, usize),
    },

    /// `{ STATEMENTS }`
    ScopeStatement {
        block: Vec<Statements<'a>>,
        span: (usize, usize),
    },

    Expression(Expressions<'a>),
    None,
}
