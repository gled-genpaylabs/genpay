use crate::{
    END_STATEMENT, Parser,
    error::{self, ParserError},
    expressions::{Expressions, Spannable},
    types::Type,
    value::Value,
};
use genpay_lexer::token_type::TokenType;
use std::collections::BTreeMap;

impl<'a> Spannable for Statements<'a> {
    fn span(&self) -> (usize, usize) {
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
            Statements::ExternStatement { span, .. } => *span,
            Statements::ExternDeclareStatement { span, .. } => *span,
            Statements::LinkCStatement { span, .. } => *span,
            Statements::BreakStatements { span } => *span,
            Statements::ReturnStatement { span, .. } => *span,
            Statements::ScopeStatement { span, .. } => *span,
            Statements::Expression(expr) => expr.span(),
            Statements::None => (0, 0),
        }
    }
}

#[derive(Debug, Clone)]
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
        operand: TokenType,
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
        fields: BTreeMap<&'a str, Type<'a>>,
        functions: BTreeMap<&'a str, Statements<'a>>,
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
        functions: BTreeMap<&'a str, Statements<'a>>,
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

impl<'a> PartialEq for Statements<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Statements::AssignStatement {
                    object: o1,
                    value: v1,
                    ..
                },
                Statements::AssignStatement {
                    object: o2,
                    value: v2,
                    ..
                },
            ) => o1 == o2 && v1 == v2,
            (
                Statements::BinaryAssignStatement {
                    object: o1,
                    operand: op1,
                    value: v1,
                    ..
                },
                Statements::BinaryAssignStatement {
                    object: o2,
                    operand: op2,
                    value: v2,
                    ..
                },
            ) => o1 == o2 && op1 == op2 && v1 == v2,
            (
                Statements::DerefAssignStatement {
                    object: o1,
                    value: v1,
                    ..
                },
                Statements::DerefAssignStatement {
                    object: o2,
                    value: v2,
                    ..
                },
            ) => o1 == o2 && v1 == v2,
            (
                Statements::SliceAssignStatement {
                    object: o1,
                    index: i1,
                    value: v1,
                    ..
                },
                Statements::SliceAssignStatement {
                    object: o2,
                    index: i2,
                    value: v2,
                    ..
                },
            ) => o1 == o2 && i1 == i2 && v1 == v2,
            (
                Statements::FieldAssignStatement {
                    object: o1,
                    value: v1,
                    ..
                },
                Statements::FieldAssignStatement {
                    object: o2,
                    value: v2,
                    ..
                },
            ) => o1 == o2 && v1 == v2,
            (
                Statements::AnnotationStatement {
                    identifier: i1,
                    datatype: d1,
                    value: v1,
                    ..
                },
                Statements::AnnotationStatement {
                    identifier: i2,
                    datatype: d2,
                    value: v2,
                    ..
                },
            ) => i1 == i2 && d1 == d2 && v1 == v2,
            (
                Statements::FunctionDefineStatement {
                    name: n1,
                    datatype: d1,
                    arguments: a1,
                    block: b1,
                    public: p1,
                    ..
                },
                Statements::FunctionDefineStatement {
                    name: n2,
                    datatype: d2,
                    arguments: a2,
                    block: b2,
                    public: p2,
                    ..
                },
            ) => n1 == n2 && d1 == d2 && a1 == a2 && b1 == b2 && p1 == p2,
            (
                Statements::FunctionCallStatement {
                    name: n1,
                    arguments: a1,
                    ..
                },
                Statements::FunctionCallStatement {
                    name: n2,
                    arguments: a2,
                    ..
                },
            ) => n1 == n2 && a1 == a2,
            (
                Statements::MacroCallStatement {
                    name: n1,
                    arguments: a1,
                    ..
                },
                Statements::MacroCallStatement {
                    name: n2,
                    arguments: a2,
                    ..
                },
            ) => n1 == n2 && a1 == a2,
            (
                Statements::StructDefineStatement {
                    name: n1,
                    fields: f1,
                    functions: func1,
                    public: p1,
                    ..
                },
                Statements::StructDefineStatement {
                    name: n2,
                    fields: f2,
                    functions: func2,
                    public: p2,
                    ..
                },
            ) => n1 == n2 && f1 == f2 && func1 == func2 && p1 == p2,
            (
                Statements::EnumDefineStatement {
                    name: n1,
                    fields: f1,
                    functions: func1,
                    public: p1,
                    ..
                },
                Statements::EnumDefineStatement {
                    name: n2,
                    fields: f2,
                    functions: func2,
                    public: p2,
                    ..
                },
            ) => n1 == n2 && f1 == f2 && func1 == func2 && p1 == p2,
            (
                Statements::TypedefStatement {
                    alias: a1,
                    datatype: d1,
                    ..
                },
                Statements::TypedefStatement {
                    alias: a2,
                    datatype: d2,
                    ..
                },
            ) => a1 == a2 && d1 == d2,
            (
                Statements::IfStatement {
                    condition: c1,
                    then_block: t1,
                    else_block: e1,
                    ..
                },
                Statements::IfStatement {
                    condition: c2,
                    then_block: t2,
                    else_block: e2,
                    ..
                },
            ) => c1 == c2 && t1 == t2 && e1 == e2,
            (
                Statements::WhileStatement {
                    condition: c1,
                    block: b1,
                    ..
                },
                Statements::WhileStatement {
                    condition: c2,
                    block: b2,
                    ..
                },
            ) => c1 == c2 && b1 == b2,
            (
                Statements::ForStatement {
                    binding: b1,
                    iterator: i1,
                    block: bl1,
                    ..
                },
                Statements::ForStatement {
                    binding: b2,
                    iterator: i2,
                    block: bl2,
                    ..
                },
            ) => b1 == b2 && i1 == i2 && bl1 == bl2,
            (Statements::ImportStatement { path: p1, .. }, Statements::ImportStatement { path: p2, .. }) => {
                p1 == p2
            }
            (
                Statements::IncludeStatement { path: p1, .. },
                Statements::IncludeStatement { path: p2, .. },
            ) => p1 == p2,
            (
                Statements::ExternStatement {
                    identifier: i1,
                    arguments: a1,
                    return_type: r1,
                    extern_type: e1,
                    is_var_args: iv1,
                    public: p1,
                    ..
                },
                Statements::ExternStatement {
                    identifier: i2,
                    arguments: a2,
                    return_type: r2,
                    extern_type: e2,
                    is_var_args: iv2,
                    public: p2,
                    ..
                },
            ) => {
                i1 == i2 && a1 == a2 && r1 == r2 && e1 == e2 && iv1 == iv2 && p1 == p2
            }
            (
                Statements::ExternDeclareStatement {
                    identifier: i1,
                    datatype: d1,
                    ..
                },
                Statements::ExternDeclareStatement {
                    identifier: i2,
                    datatype: d2,
                    ..
                },
            ) => i1 == i2 && d1 == d2,
            (Statements::LinkCStatement { path: p1, .. }, Statements::LinkCStatement { path: p2, .. }) => {
                p1 == p2
            }
            (Statements::BreakStatements { .. }, Statements::BreakStatements { .. }) => true,
            (Statements::ReturnStatement { value: v1, .. }, Statements::ReturnStatement { value: v2, .. }) => {
                v1 == v2
            }
            (
                Statements::ScopeStatement { block: b1, .. },
                Statements::ScopeStatement { block: b2, .. },
            ) => b1 == b2,
            (Statements::Expression(e1), Statements::Expression(e2)) => e1 == e2,
            (Statements::None, Statements::None) => true,
            _ => false,
        }
    }
}

use bumpalo::Bump;

impl<'a> Parser<'a> {
    pub fn annotation_statement(&mut self, expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Statements<'a> {
        let span_start = self.current().span.0;

        if self.current().value == "let" {
            let _ = self.next();
        }

        if !self.expect(TokenType::Identifier) {
            self.error(ParserError::SyntaxError {
                exception: "identifier expected after `let` keyword".to_string(),
                help: "Add identifier after `let` keyword".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });

            self.skip_statement();
            return Statements::None;
        }

        let id = self.current().value;
        let mut datatype = None;

        if self.next().token_type == TokenType::DoubleDots {
            let _ = self.next();
            datatype = Some(self.parse_type());
        }

        match self.current().token_type {
            TokenType::Equal => {
                let _ = self.next();
                let value = self.expression(expr_arena, stmt_arena);

                self.skip_eos();

                Statements::AnnotationStatement {
                    identifier: id,
                    datatype,
                    value: Some(value.clone()),
                    span: (span_start, value.span().1),
                }
            }
            END_STATEMENT => {
                let span_end = self.current().span.1;
                self.skip_eos();

                Statements::AnnotationStatement {
                    identifier: id,
                    datatype,
                    value: None,
                    span: (span_start, span_end),
                }
            }
            _ => {
                self.error(ParserError::SyntaxError {
                    exception: "expected `=` or `;` after variable declaration".to_string(),
                    help: "Consider adding assign operator or semicolon after identifier"
                        .to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span((span_start, self.current().span.1)),
                });

                Statements::None
            }
        }
    }

    pub fn import_statement(&mut self, expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Statements<'a> {
        let span_start = self.current().span.0;
        if self.current().token_type == TokenType::Keyword {
            let _ = self.next();
        }

        let path = self.expression(expr_arena, stmt_arena);
        self.position -= 1;

        let span_end = self.current().span.1;
        let _ = self.next();
        self.skip_eos();

        if let Expressions::Value(Value::String(_), _) = path {
            Statements::ImportStatement {
                path,
                span: (span_start, span_end),
            }
        } else {
            self.error(ParserError::SyntaxError {
                exception: "unknown import syntax found".to_string(),
                help: "Provide string with module path after `import` keyword".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, span_end)),
            });

            Statements::None
        }
    }

    pub fn include_statement(&mut self, expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Statements<'a> {
        let span_start = self.current().span.0;
        if self.current().token_type == TokenType::Keyword {
            let _ = self.next();
        }

        let path = self.expression(expr_arena, stmt_arena);
        let span_end = path.span().1;

        self.skip_eos();

        if let Expressions::Value(Value::String(_), _) = path {
            Statements::IncludeStatement {
                path,
                span: (span_start, span_end),
            }
        } else {
            self.error(ParserError::SyntaxError {
                exception: "unknown include syntax found".to_string(),
                help: "Provide string with module path after `import` keyword".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, span_end)),
            });

            Statements::None
        }
    }

    pub fn if_statement(&mut self, expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Statements<'a> {
        let span_start = self.current().span.0;
        if self.current().token_type == TokenType::Keyword {
            let _ = self.next();
        }

        let condition = self.expression(expr_arena, stmt_arena);

        if self.current().token_type != TokenType::LBrace {
            self.position -= 1;
            self.error(ParserError::SyntaxError {
                exception: "expected new block after condition".to_string(),
                help: "Consider opening new statements block".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });

            self.skip_statement();
            return Statements::None;
        }

        let _ = self.next();
        let mut span_block_start = self.current().span.0;
        let mut then_block = Vec::new();

        while self.current().token_type != TokenType::RBrace {
            if self.current().token_type == TokenType::EOF {
                self.error(ParserError::UnclosedExpression {
                    exception: "statements block end not found".to_string(),
                    help: "Consider adding block end after statements: `}`".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span((span_block_start, self.current().span.1)),
                });

                return Statements::None;
            }

            then_block.push(self.statement(expr_arena, stmt_arena));
            span_block_start = self.current().span.0;
        }

        let span_end = self.current().span.1;
        if self.expect(TokenType::RBrace) {
            let _ = self.next();
        }

        match self.current().token_type {
            TokenType::Keyword => {
                if self.current().value != "else" {
                    self.skip_eos();
                    return Statements::IfStatement {
                        condition,
                        then_block,
                        else_block: None,
                        span: (span_start, span_end),
                    };
                }

                let mut else_span_start = self.current().span.0;
                let _ = self.next();

                match self.current().token_type {
                    TokenType::Keyword => {
                        if self.current().value != "if" {
                            self.error(ParserError::UnknownExpression {
                                exception: "unexpected keyword found after `else`".to_string(),
                                help: "Consider opening new block, or using `if else` bundle"
                                    .to_string(),
                                src: self.source.clone(),
                                span: error::position_to_span((
                                    else_span_start,
                                    self.current().span.1,
                                )),
                            });

                            return Statements::None;
                        }

                        let stmt = self.if_statement(expr_arena, stmt_arena);
                        let span_end = self.current().span.1;
                        return Statements::IfStatement {
                            condition,
                            then_block,
                            else_block: Some(vec![stmt]),
                            span: (span_start, span_end),
                        };
                    }
                    TokenType::LBrace => {}
                    _ => {
                        self.error(ParserError::SyntaxError {
                            exception: "new block expected after `else` keyword".to_string(),
                            help: "Open new statements block with curly brackets".to_string(),
                            src: self.source.clone(),
                            span: error::position_to_span((else_span_start, self.current().span.1)),
                        });

                        return Statements::None;
                    }
                }

                let _ = self.next();

                let mut else_block = Vec::new();
                else_span_start = self.current().span.0;

                while self.current().token_type != TokenType::RBrace {
                    if self.current().token_type == TokenType::EOF {
                        self.error(ParserError::UnclosedExpression {
                            exception: "statements block end not found".to_string(),
                            help: "Consider adding block end after statements: `}`".to_string(),
                            src: self.source.clone(),
                            span: error::position_to_span((else_span_start, self.current().span.1)),
                        });

                        return Statements::None;
                    }

                    else_block.push(self.statement(expr_arena, stmt_arena));
                    else_span_start = self.current().span.0;
                }

                if self.expect(TokenType::RBrace) {
                    let _ = self.next();
                }

                let span_end = self.current().span.1;
                self.skip_eos();
                Statements::IfStatement {
                    condition,
                    then_block,
                    else_block: Some(else_block),
                    span: (span_start, span_end),
                }
            }
            _ => {
                let span_end = self.current().span.1;
                self.skip_eos();
                Statements::IfStatement {
                    condition,
                    then_block,
                    else_block: None,
                    span: (span_start, span_end),
                }
            }
        }
    }

    pub fn while_statement(&mut self, expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Statements<'a> {
        let span_start = self.current().span.0;
        if let TokenType::Keyword = self.current().token_type {
            let _ = self.next();
        }

        let condition = self.expression(expr_arena, stmt_arena);

        if self.current().token_type != TokenType::LBrace {
            self.error(ParserError::SyntaxError {
                exception: "expected new block after condition".to_string(),
                help: "Consider opening new block after condition".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });

            return Statements::None;
        }

        let _ = self.next();
        let mut span_block_start = self.current().span.0;
        let mut block = Vec::new();

        while !self.expect(TokenType::RBrace) {
            if self.current().token_type == TokenType::EOF {
                self.error(ParserError::UnclosedExpression {
                    exception: "statements block end not found".to_string(),
                    help: "Consider adding block end after statements: `}`".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span((span_block_start, self.current().span.1)),
                });

                return Statements::None;
            }

            block.push(self.statement(expr_arena, stmt_arena));
            span_block_start = self.current().span.0;
        }

        if self.expect(TokenType::RBrace) {
            let _ = self.next();
        }

        self.skip_eos();
        Statements::WhileStatement {
            condition,
            block,
            span: (span_start, self.current().span.1),
        }
    }

    pub fn for_statement(&mut self, expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Statements<'a> {
        let span_start = self.current().span.0;
        if let TokenType::Keyword = self.current().token_type {
            let _ = self.next();
        }
        let binding = self.current().value;
        let _ = self.next();

        if !self.expect(TokenType::Equal) {
            self.error(ParserError::SyntaxError {
                exception: "expected binding for iterator in loop".to_string(),
                help: "Use right syntax: `for BINDING = ITERATOR {}`".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });

            let _ = self.next();
            return Statements::None;
        }

        let _ = self.next();
        let iterator = self.expression(expr_arena, stmt_arena);

        if self.current().token_type != TokenType::LBrace {
            self.error(ParserError::SyntaxError {
                exception: "expected new block after condition".to_string(),
                help: "Consider opening new block after condition".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });

            return Statements::None;
        }

        let _ = self.next();
        let mut span_block_start = self.current().span.0;
        let mut block = Vec::new();

        while !self.expect(TokenType::RBrace) {
            if self.current().token_type == TokenType::EOF {
                self.error(ParserError::UnclosedExpression {
                    exception: "statements block end not found".to_string(),
                    help: "Consider adding block end after statements: `}`".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span((span_block_start, self.current().span.1)),
                });

                return Statements::None;
            }

            block.push(self.statement(expr_arena, stmt_arena));
            span_block_start = self.current().span.0;
        }

        let span_end = self.current().span.1;
        if self.expect(TokenType::RBrace) {
            let _ = self.next();
        }

        self.skip_eos();
        Statements::ForStatement {
            binding,
            iterator,
            block,
            span: (span_start, span_end),
        }
    }

    pub fn fn_statement(&mut self, expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Statements<'a> {
        let span_start = self.current().span.0;
        if let TokenType::Keyword = self.current().token_type {
            let _ = self.next();
        }

        let identifier = self.current().value;

        let _ = self.next();
        let arguments =
            self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma, expr_arena, stmt_arena);

        let arguments_tuples: Vec<(&str, Type)> = arguments
            .iter()
            .map(|arg| {
                if let Expressions::Argument {
                    name,
                    r#type,
                    span: _,
                } = arg
                {
                    (*name, r#type.clone())
                } else {
                    // okay lets just skip this piece of code.
                    // I've made a mistake creating this embedded code, but we all make mistakes.
                    // Anyways I just wanted short code to unwrap and compare identifier inside
                    // embedded boxed expressions blocks, so...

                    if let Expressions::Reference { object, span: _ } = arg {
                        if let Expressions::Value(Value::Identifier(id), _) = **object {
                            if id == "self" {
                                return ("self", Type::SelfRef);
                            }
                        }
                    }

                    self.error(ParserError::DeclarationError {
                        exception: "unexpected argument declaration found".to_string(),
                        help: "Use right arguments syntax: `identifier: type`".to_string(),
                        src: self.source.clone(),
                        span: error::position_to_span(arg.span()),
                    });

                    ("", Type::Void)
                }
            })
            .collect();

        let mut datatype = Type::Void;
        if !self.expect(TokenType::LBrace) {
            datatype = self.parse_type();
        }

        if !self.expect(TokenType::LBrace) {
            self.error(ParserError::SyntaxError {
                exception: "expected new block after function declaration".to_string(),
                help: "Open new statements block with curly brackets".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });

            return Statements::None;
        }

        let header_span = (span_start, self.current().span.1);

        let _ = self.next();
        let mut span_block_start = self.current().span.0;
        let mut block = Vec::new();

        while !self.expect(TokenType::RBrace) {
            if self.current().token_type == TokenType::EOF {
                self.error(ParserError::UnclosedExpression {
                    exception: "statements block end not found".to_string(),
                    help: "Consider adding block end after statements: `}`".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span((span_block_start, self.current().span.1)),
                });

                return Statements::None;
            }

            block.push(self.statement(expr_arena, stmt_arena));
            span_block_start = self.current().span.0;
        }

        let span_end = self.current().span.0 + 1;
        if self.expect(TokenType::RBrace) {
            let _ = self.next();
        }

        // self.skip_eos();
        Statements::FunctionDefineStatement {
            name: identifier,
            datatype,
            arguments: arguments_tuples,
            block,
            public: false,
            span: (span_start, span_end),
            header_span,
        }
    }

    pub fn return_statement(&mut self, expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Statements<'a> {
        let span_start = self.current().span.0;
        if self.expect(TokenType::Keyword) {
            let _ = self.next();
        }

        let return_expr = if self.expect(TokenType::Semicolon) {
            let _ = self.next();
            Expressions::Value(Value::Void, self.current().span)
        } else {
            self.expression(expr_arena, stmt_arena)
        };

        let end_span = return_expr.span().1;
        Statements::ReturnStatement {
            value: return_expr,
            span: (span_start, end_span),
        }
    }

    pub fn break_statement(&mut self, _expr_arena: &'a Bump, _stmt_arena: &'a Bump) -> Statements<'a> {
        let span = self.current().span;
        if self.expect(TokenType::Keyword) {
            let _ = self.next();
        }

        self.skip_eos();

        Statements::BreakStatements { span }
    }

    pub fn assign_statement(&mut self, object: Expressions<'a>, span: (usize, usize), expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Statements<'a> {
        if self.expect(TokenType::Equal) {
            let _ = self.next();
        }

        let value = self.expression(expr_arena, stmt_arena);
        let end_span = value.span().1;
        self.skip_eos();

        Statements::AssignStatement {
            object,
            value,
            span: (span.0, end_span),
        }
    }

    pub fn binary_assign_statement(
        &mut self,
        object: Expressions<'a>,
        op: TokenType,
        span: (usize, usize),
        expr_arena: &'a Bump,
        stmt_arena: &'a Bump,
    ) -> Statements<'a> {
        if self.expect(TokenType::Equal) {
            let _ = self.next();
        }

        let value = self.expression(expr_arena, stmt_arena);
        let end_span = value.span().1;
        self.skip_eos();

        Statements::BinaryAssignStatement {
            operand: op,
            object,
            value,
            span: (span.0, end_span),
        }
    }

    pub fn slice_assign_statement(
        &mut self,
        object: Expressions<'a>,
        span: (usize, usize),
        expr_arena: &'a Bump,
        stmt_arena: &'a Bump,
    ) -> Statements<'a> {
        if !self.expect(TokenType::LBrack) {
            // This should be an error, but for now we assume it's there
        }
        let _ = self.next(); // consume '['

        let index_expr = self.expression(expr_arena, stmt_arena);

        if !self.expect(TokenType::RBrack) {
            self.error(ParserError::UnclosedExpression {
                exception: "unclosed brackets in slice".to_string(),
                help: "Close slice index with brackets".to_string(),
                src: self.source.clone(),
                span: error::position_to_span(self.current().span),
            });
            return Statements::None;
        }
        let _ = self.next(); // consume ']'

        if !self.expect(TokenType::Equal) {
            self.error(ParserError::SyntaxError {
                exception: "expected assign operator after slice".to_string(),
                help: "Add assign operator after brackets".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span.0, self.current().span.1)),
            });
            self.skip_statement();
            return Statements::None;
        }
        let _ = self.next(); // consume '='

        let value_expr = self.expression(expr_arena, stmt_arena);
        let end_span = value_expr.span().1;
        self.skip_eos();

        Statements::SliceAssignStatement {
            object,
            index: index_expr,
            value: value_expr,
            span: (span.0, end_span),
        }
    }

    pub fn call_statement(&mut self, id: &'a str, span: (usize, usize), expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Statements<'a> {
        match self.current().token_type {
            TokenType::Identifier => {
                let _ = self.next();
            }
            TokenType::LParen => {}
            _ => {
                self.error(ParserError::UnknownExpression {
                    exception: "unknown call statement syntax".to_string(),
                    help: "Consider using right syntax: `identifier(value, ...)".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span((span.0, self.current().span.1)),
                });

                return Statements::None;
            }
        };

        let arguments =
            self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma, expr_arena, stmt_arena);

        let span_end = if let Some(last_arg) = arguments.last() {
            last_arg.span().1
        } else {
            self.current().span.0
        };

        self.skip_eos();

        Statements::FunctionCallStatement {
            name: id,
            arguments,
            span: (span.0, span_end),
        }
    }

    pub fn macrocall_statement(&mut self, id: &'a str, span: (usize, usize), expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Statements<'a> {
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

        self.skip_eos();

        Statements::MacroCallStatement {
            name: id,
            arguments,
            span: (span.0, span_end),
        }
    }

    pub fn struct_statement(&mut self, expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Statements<'a> {
        let span_start = self.current().span.0;
        if let TokenType::Keyword = self.current().token_type {
            let _ = self.next();
        }

        if !self.expect(TokenType::Identifier) {
            self.error(ParserError::SyntaxError {
                exception: "structure identifier not found".to_string(),
                help: "Add identifier after `struct` keyword".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });

            return Statements::None;
        }

        let name = self.current().value;
        let _ = self.next();

        if !self.expect(TokenType::LBrace) {
            self.error(ParserError::SyntaxError {
                exception: "expected new block after identifier".to_string(),
                help: "Consider opening new statements block with curly brackets".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });

            return Statements::None;
        }

        let _ = self.next();
        let mut fields = BTreeMap::new();
        let mut functions = BTreeMap::new();

        let mut method_mode = false;
        let mut mode_reported = false;

        while !self.expect(TokenType::RBrace) {
            match self.current().token_type {
                TokenType::RBrace => break,
                TokenType::Keyword => {
                    if self.current().value != "fn" {
                        self.error(ParserError::DeclarationError {
                            exception: "unknown keyword in declaration found".to_string(),
                            help: String::new(),
                            src: self.source.clone(),
                            span: error::position_to_span(self.current().span),
                        });

                        return Statements::None;
                    }

                    method_mode = true;

                    let stmt = self.fn_statement(expr_arena, stmt_arena);

                    if let Statements::FunctionDefineStatement {
                        name,
                        datatype: _,
                        arguments: _,
                        public: _,
                        block: _,
                        span: _,
                        header_span: _,
                    } = &stmt
                    {
                        functions.insert(*name, stmt.clone());
                    } else {
                        unreachable!()
                    }

                    if self.expect(TokenType::Comma) {
                        let _ = self.next();
                    }
                }
                TokenType::Identifier => {
                    if method_mode && !mode_reported {
                        self.error(ParserError::DeclarationError {
                            exception: "fields after methods are not allowed".to_string(),
                            help: "Move fields before methods".to_string(),
                            src: self.source.clone(),
                            span: error::position_to_span(self.current().span),
                        });

                        mode_reported = true;
                    }

                    let name = self.current().value;
                    let span = self.current().span;
                    let _ = self.next();

                    if !self.expect(TokenType::DoubleDots) {
                        self.error(ParserError::DeclarationError {
                            exception: "unknown field declaration syntax".to_string(),
                            help: "Follow this syntax: `field: type`".to_string(),
                            src: self.source.clone(),
                            span: error::position_to_span((span.0, self.current().span.1)),
                        });

                        return Statements::None;
                    }

                    let _ = self.next();
                    let field_type = self.parse_type();

                    self.position -= 1;
                    let extra_span = self.current().span;
                    self.position += 1;

                    if !self.expect(TokenType::Comma) && !self.expect(TokenType::RBrace) {
                        self.error(ParserError::SyntaxError {
                            exception: "expected comma".to_string(),
                            help: "Separate fields and methods with commas".to_string(),
                            src: self.source.clone(),
                            span: error::position_to_span((extra_span.1, extra_span.1)),
                        });
                    }

                    if self.expect(TokenType::Comma) {
                        let _ = self.next();
                    }

                    if fields.contains_key(name) {
                        self.error(ParserError::DeclarationError {
                            exception: format!("field `{name}` defined multiple times"),
                            help: "Remove field duplicate".to_string(),
                            src: self.source.clone(),
                            span: error::position_to_span(span),
                        });
                    }

                    fields.insert(name, field_type);
                }
                TokenType::Semicolon => {
                    self.error(ParserError::SyntaxError {
                        exception: "use commas instead for separation".to_string(),
                        help: "Replace semicolons with commas".to_string(),
                        src: self.source.clone(),
                        span: error::position_to_span(self.current().span),
                    });

                    let _ = self.next();
                }
                _ => {
                    self.error(ParserError::UnknownExpression {
                        exception: "unknown expression at the struct declaration".to_string(),
                        help: String::new(),
                        src: self.source.clone(),
                        span: error::position_to_span(self.current().span),
                    });

                    return Statements::None;
                }
            }
        }

        if self.expect(TokenType::RBrace) {
            let _ = self.next();
        }
        self.skip_eos();

        Statements::StructDefineStatement {
            name,
            fields,
            functions,
            public: false,
            span: (span_start, self.current().span.1),
        }
    }

    pub fn enum_statement(&mut self, expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Statements<'a> {
        let span_start = self.current().span.0;
        if let TokenType::Keyword = self.current().token_type {
            let _ = self.next();
        }

        if !self.expect(TokenType::Identifier) {
            self.error(ParserError::SyntaxError {
                exception: "identifier expected for enumeration name".to_string(),
                help: "Add identifier after `enum` keyword".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });

            return Statements::None;
        }

        let name = self.current().value;
        let _ = self.next();

        if !self.expect(TokenType::LBrace) {
            self.error(ParserError::SyntaxError {
                exception: "expected new block after identifier".to_string(),
                help: "Open new statements block after identifier".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });

            return Statements::None;
        }
        let _ = self.next();

        let mut fields = Vec::new();
        let mut functions = BTreeMap::new();

        while !self.expect(TokenType::RBrace) {
            match self.current().token_type {
                TokenType::RBrace => break,
                TokenType::Keyword => {
                    if self.current().value != "fn" {
                        self.error(ParserError::DeclarationError {
                            exception: "unknown keyword in declaration found".to_string(),
                            help: String::new(),
                            src: self.source.clone(),
                            span: error::position_to_span(self.current().span),
                        });

                        return Statements::None;
                    }

                    let stmt = self.fn_statement(expr_arena, stmt_arena);

                    if let Statements::FunctionDefineStatement {
                        name,
                        datatype: _,
                        arguments: _,
                        public: _,
                        block: _,
                        span: _,
                        header_span: _,
                    } = &stmt
                    {
                        functions.insert(name.to_owned(), stmt.clone());
                    } else {
                        unreachable!()
                    }

                    if self.expect(TokenType::Comma) {
                        let _ = self.next();
                    }
                }
                TokenType::Identifier => {
                    let name = self.current().value;
                    let extra_span = self.current().span;
                    let _ = self.next();

                    if !self.expect(TokenType::Comma) && !self.expect(TokenType::RBrace) {
                        self.error(ParserError::SyntaxError {
                            exception: "expected comma".to_string(),
                            help: "Separate fields and methods with commas".to_string(),
                            src: self.source.clone(),
                            span: error::position_to_span((extra_span.1, extra_span.1)),
                        });
                    }

                    if self.expect(TokenType::Comma) {
                        let _ = self.next();
                    }

                    fields.push(name);
                }
                _ => {
                    self.error(ParserError::UnknownExpression {
                        exception: "unknown expression at the struct declaration".to_string(),
                        help: String::new(),
                        src: self.source.clone(),
                        span: error::position_to_span(self.current().span),
                    });

                    return Statements::None;
                }
            }
        }

        if self.expect(TokenType::RBrace) {
            let _ = self.next();
        }
        self.skip_eos();

        Statements::EnumDefineStatement {
            name,
            fields,
            functions,
            public: false,
            span: (span_start, self.current().span.1),
        }
    }

    pub fn typedef_statement(&mut self, _expr_arena: &'a Bump, _stmt_arena: &'a Bump) -> Statements<'a> {
        let span_start = self.current().span.0;
        if self.expect(TokenType::Keyword) {
            let _ = self.next();
        }

        if !self.expect(TokenType::Identifier) {
            self.error(ParserError::SyntaxError {
                exception: "expected alias for typedef".to_string(),
                help: "Use right `typedef` syntax: \"typedef ALIAS TYPE\"".to_string(),
                src: self.source.clone(),
                span: error::position_to_span(self.current().span),
            });
        };

        let alias = self.current().value;
        let _ = self.next();

        let datatype = self.parse_type();
        let span_end = self.current().span.1;
        self.skip_eos();

        Statements::TypedefStatement {
            alias,
            datatype,
            span: (span_start, span_end),
        }
    }

    pub fn extern_declare_statement(&mut self, _expr_arena: &'a Bump, _stmt_arena: &'a Bump) -> Statements<'a> {
        let span_start = self.current().span.0;
        if self.expect(TokenType::Keyword) {
            let _ = self.next();
        }

        if !self.expect(TokenType::Identifier) {
            self.error(ParserError::SyntaxError {
                exception: "expected identifier for extern-declare".to_string(),
                help: "Use right `extern declare` syntax: \"__extern_declare IDENTIFIER TYPE\""
                    .to_string(),
                src: self.source.clone(),
                span: error::position_to_span(self.current().span),
            });
        }

        let identifier = self.current().value;
        let _ = self.next();

        let datatype = self.parse_type();
        let span_end = self.current().span.1;

        self.skip_eos();
        let span = (span_start, span_end);

        Statements::ExternDeclareStatement {
            identifier,
            datatype,
            span,
        }
    }

    pub fn link_c_statement(&mut self, expr_arena: &'a Bump, stmt_arena: &'a Bump) -> Statements<'a> {
        let span_start = self.current().span.0;
        if self.expect(TokenType::Keyword) {
            let _ = self.next();
        }

        let path = self.expression(expr_arena, stmt_arena);
        let span_end = path.span().1;

        if let Expressions::Value(Value::String(_), _) = path {
            Statements::LinkCStatement {
                path,
                span: (span_start, span_end),
            }
        } else {
            self.error(ParserError::SyntaxError {
                exception: "expected string path".to_string(),
                help: "Consider using string constant after keyword".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, span_end)),
            });

            Statements::LinkCStatement {
                path,
                span: (span_start, span_end),
            }
        }
    }

    pub fn extern_statement(&mut self, _expr_arena: &'a Bump, _stmt_arena: &'a Bump) -> Statements<'a> {
        let span_start = self.current().span.0;
        if self.expect(TokenType::Keyword) {
            let _ = self.next();
        }

        if !self.expect(TokenType::String) {
            self.error(ParserError::SyntaxError {
                exception: "expected extern type".to_string(),
                help: "Provide string constant with extern type".to_string(),
                src: self.source.clone(),
                span: error::position_to_span(self.current().span),
            });
        }

        let extern_type = self.current().value;
        let _ = self.next();

        let public = self.expect(TokenType::Keyword) && self.current().value == "pub";
        if public {
            let _ = self.next();
        }

        if !self.expect(TokenType::Keyword) && self.current().value != "fn" {
            self.error(ParserError::UnsupportedExpression {
                exception: "unsupported extern statement found".to_string(),
                help: "Extern statement support only functions declarations".to_string(),
                src: self.source.clone(),
                span: error::position_to_span(self.current().span),
            });

            self.skip_statement();
        }

        let _ = self.next();
        let identifier = if self.expect(TokenType::Identifier) {
            self.current().value
        } else {
            self.error(ParserError::SyntaxError {
                exception: "function identifier expected".to_string(),
                help: "Add identifier after `fn` keyword".to_string(),
                src: self.source.clone(),
                span: error::position_to_span(self.current().span),
            });

            "undefined"
        };

        let _ = self.next();
        if !self.expect(TokenType::LParen) {
            self.error(ParserError::SyntaxError {
                exception: "expected arguments types block".to_string(),
                help: "Use syntax: `extern fn IDENTIFIER ( TYPE, ... ) TYPE`".to_string(),
                src: self.source.clone(),
                span: error::position_to_span(self.current().span),
            });
        }

        let mut arguments = Vec::new();
        let mut is_var_args = false;
        let _ = self.next();

        while !self.expect(TokenType::RParen) {
            if self.expect(TokenType::Dot) {
                if self.next().token_type == TokenType::Dot
                    && self.next().token_type == TokenType::Dot
                {
                    is_var_args = true;
                    let _ = self.next();
                } else {
                    self.position -= 1;

                    self.error(ParserError::DeclarationError {
                        exception: "unknown argument declaration syntax".to_string(),
                        help: String::new(),
                        src: self.source.clone(),
                        span: error::position_to_span(self.current().span),
                    });

                    break;
                }
                continue;
            }

            if self.expect(TokenType::RParen) {
                break;
            }
            if self.expect(TokenType::Semicolon) {
                break;
            }
            if self.expect(TokenType::Comma) {
                let _ = self.next();
                continue;
            }

            arguments.push(self.parse_type());
        }

        if self.expect(TokenType::RParen) {
            let _ = self.next();
        }

        let mut return_type = Type::Void;
        if !self.expect(TokenType::Semicolon) {
            return_type = self.parse_type();
        }

        let span_end = self.current().span.1;
        self.skip_eos();

        Statements::ExternStatement {
            identifier,
            arguments,
            return_type,
            extern_type,
            public,
            is_var_args,
            span: (span_start, span_end),
        }
    }
}
