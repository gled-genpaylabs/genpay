use crate::{
    END_STATEMENT, Parser,
    error::{self, ParserError},
    expressions::{Expressions, Spannable},
    types::Type,
    value::Value,
};
use bumpalo::collections::{CollectIn, Vec as BumpVec};
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
        arguments: BumpVec<'a, (&'a str, Type<'a>)>,
        block: BumpVec<'a, Statements<'a>>,
        public: bool,
        span: (usize, usize),
        header_span: (usize, usize),
    },
    /// `NAME ( EXPRESSION, EXPRESSION, ... )`
    FunctionCallStatement {
        name: &'a str,
        arguments: BumpVec<'a, Expressions<'a>>,
        span: (usize, usize),
    },

    /// `MACRONAME! ( EXPRESSION, EXPRESSION, ... )`
    MacroCallStatement {
        name: &'a str,
        arguments: BumpVec<'a, Expressions<'a>>,
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
        fields: BumpVec<'a, &'a str>,
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
        then_block: BumpVec<'a, Statements<'a>>,
        else_block: Option<BumpVec<'a, Statements<'a>>>,
        span: (usize, usize),
    },

    /// `while EXPRESSION { STATEMENTS }`
    WhileStatement {
        condition: Expressions<'a>,
        block: BumpVec<'a, Statements<'a>>,
        span: (usize, usize),
    },

    /// `for IDENTIFIER = OBJECT { STATEMENTS }`
    ForStatement {
        binding: &'a str,
        iterator: Expressions<'a>,
        block: BumpVec<'a, Statements<'a>>,
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
        arguments: BumpVec<'a, Type<'a>>,
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
        block: BumpVec<'a, Statements<'a>>,
        span: (usize, usize),
    },

    Expression(Expressions<'a>),
    None,
}


use bumpalo::Bump;

impl<'a> Parser<'a> {
    pub fn annotation_statement(&mut self, arena: &'a Bump) -> Result<Statements<'a>, ParserError> {
        let span_start = self.current().span.0;

        if self.current().value == "let" {
            let _ = self.next();
        }

        if !self.expect(TokenType::Identifier) {
            return Err(ParserError::SyntaxError {
                exception: "identifier expected after `let` keyword".to_string(),
                help: "Add identifier after `let` keyword".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });
        }

        let id = self.current().value;
        let mut datatype = None;

        if self.next().token_type == TokenType::DoubleDots {
            let _ = self.next();
            datatype = Some(self.parse_type(arena));
        }

        match self.current().token_type {
            TokenType::Equal => {
                let _ = self.next();
                let value = self.expression(arena);

                self.skip_eos();

                Ok(Statements::AnnotationStatement {
                    identifier: id,
                    datatype,
                    value: Some(value.clone()),
                    span: (span_start, value.span().1),
                })
            }
            END_STATEMENT => {
                let span_end = self.current().span.1;
                self.skip_eos();

                Ok(Statements::AnnotationStatement {
                    identifier: id,
                    datatype,
                    value: None,
                    span: (span_start, span_end),
                })
            }
            _ => {
                Err(ParserError::SyntaxError {
                    exception: "expected `=` or `;` after variable declaration".to_string(),
                    help: "Consider adding assign operator or semicolon after identifier"
                        .to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span((span_start, self.current().span.1)),
                })
            }
        }
    }

    pub fn import_statement(&mut self, arena: &'a Bump) -> Result<Statements<'a>, ParserError> {
        let span_start = self.current().span.0;
        if self.current().token_type == TokenType::Keyword {
            let _ = self.next();
        }

        let path = self.expression(arena);
        self.position -= 1;

        let span_end = self.current().span.1;
        let _ = self.next();
        self.skip_eos();

        if let Expressions::Value(Value::String(_), _) = path {
            Ok(Statements::ImportStatement {
                path,
                span: (span_start, span_end),
            })
        } else {
            Err(ParserError::SyntaxError {
                exception: "unknown import syntax found".to_string(),
                help: "Provide string with module path after `import` keyword".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, span_end)),
            })
        }
    }

    pub fn include_statement(&mut self, arena: &'a Bump) -> Result<Statements<'a>, ParserError> {
        let span_start = self.current().span.0;
        if self.current().token_type == TokenType::Keyword {
            let _ = self.next();
        }

        let path = self.expression(arena);
        let span_end = path.span().1;

        self.skip_eos();

        if let Expressions::Value(Value::String(_), _) = path {
            Ok(Statements::IncludeStatement {
                path,
                span: (span_start, span_end),
            })
        } else {
            Err(ParserError::SyntaxError {
                exception: "unknown include syntax found".to_string(),
                help: "Provide string with module path after `import` keyword".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, span_end)),
            })
        }
    }

    pub fn if_statement(&mut self, arena: &'a Bump) -> Result<Statements<'a>, ParserError> {
        let span_start = self.current().span.0;
        if self.current().token_type == TokenType::Keyword {
            let _ = self.next();
        }

        let condition = self.expression(arena);

        if self.current().token_type != TokenType::LBrace {
            self.position -= 1;
            return Err(ParserError::SyntaxError {
                exception: "expected new block after condition".to_string(),
                help: "Consider opening new statements block".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });
        }

        let _ = self.next();
        let mut span_block_start = self.current().span.0;
        let mut then_block = BumpVec::new_in(arena);

        while self.current().token_type != TokenType::RBrace {
            if self.current().token_type == TokenType::EOF {
                return Err(ParserError::UnclosedExpression {
                    exception: "statements block end not found".to_string(),
                    help: "Consider adding block end after statements: `}`".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span((span_block_start, self.current().span.1)),
                });
            }

            then_block.push(self.statement(arena)?);
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
                    return Ok(Statements::IfStatement {
                        condition,
                        then_block,
                        else_block: None,
                        span: (span_start, span_end),
                    });
                }

                let mut else_span_start = self.current().span.0;
                let _ = self.next();

                match self.current().token_type {
                    TokenType::Keyword => {
                        if self.current().value != "if" {
                            return Err(ParserError::UnknownExpression {
                                exception: "unexpected keyword found after `else`".to_string(),
                                help: "Consider opening new block, or using `if else` bundle"
                                    .to_string(),
                                src: self.source.clone(),
                                span: error::position_to_span((
                                    else_span_start,
                                    self.current().span.1,
                                )),
                            });
                        }

                        let stmt = self.if_statement(arena)?;
                        let span_end = self.current().span.1;
                        let mut else_block = BumpVec::new_in(arena);
                        else_block.push(stmt);
                        return Ok(Statements::IfStatement {
                            condition,
                            then_block,
                            else_block: Some(else_block),
                            span: (span_start, span_end),
                        });
                    }
                    TokenType::LBrace => {}
                    _ => {
                        return Err(ParserError::SyntaxError {
                            exception: "new block expected after `else` keyword".to_string(),
                            help: "Open new statements block with curly brackets".to_string(),
                            src: self.source.clone(),
                            span: error::position_to_span((else_span_start, self.current().span.1)),
                        });
                    }
                }

                let _ = self.next();

                let mut else_block = BumpVec::new_in(arena);
                else_span_start = self.current().span.0;

                while self.current().token_type != TokenType::RBrace {
                    if self.current().token_type == TokenType::EOF {
                        return Err(ParserError::UnclosedExpression {
                            exception: "statements block end not found".to_string(),
                            help: "Consider adding block end after statements: `}`".to_string(),
                            src: self.source.clone(),
                            span: error::position_to_span((else_span_start, self.current().span.1)),
                        });
                    }

                    else_block.push(self.statement(arena)?);
                    else_span_start = self.current().span.0;
                }

                if self.expect(TokenType::RBrace) {
                    let _ = self.next();
                }

                let span_end = self.current().span.1;
                self.skip_eos();
                Ok(Statements::IfStatement {
                    condition,
                    then_block,
                    else_block: Some(else_block),
                    span: (span_start, span_end),
                })
            }
            _ => {
                let span_end = self.current().span.1;
                self.skip_eos();
                Ok(Statements::IfStatement {
                    condition,
                    then_block,
                    else_block: None,
                    span: (span_start, span_end),
                })
            }
        }
    }

    pub fn while_statement(&mut self, arena: &'a Bump) -> Result<Statements<'a>, ParserError> {
        let span_start = self.current().span.0;
        if let TokenType::Keyword = self.current().token_type {
            let _ = self.next();
        }

        let condition = self.expression(arena);

        if self.current().token_type != TokenType::LBrace {
            return Err(ParserError::SyntaxError {
                exception: "expected new block after condition".to_string(),
                help: "Consider opening new block after condition".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });
        }

        let _ = self.next();
        let mut span_block_start = self.current().span.0;
        let mut block = BumpVec::new_in(arena);

        while !self.expect(TokenType::RBrace) {
            if self.current().token_type == TokenType::EOF {
                return Err(ParserError::UnclosedExpression {
                    exception: "statements block end not found".to_string(),
                    help: "Consider adding block end after statements: `}`".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span((span_block_start, self.current().span.1)),
                });
            }

            block.push(self.statement(arena)?);
            span_block_start = self.current().span.0;
        }

        if self.expect(TokenType::RBrace) {
            let _ = self.next();
        }

        self.skip_eos();
        Ok(Statements::WhileStatement {
            condition,
            block,
            span: (span_start, self.current().span.1),
        })
    }

    pub fn for_statement(&mut self, arena: &'a Bump) -> Result<Statements<'a>, ParserError> {
        let span_start = self.current().span.0;
        if let TokenType::Keyword = self.current().token_type {
            let _ = self.next();
        }
        let binding = self.current().value;
        let _ = self.next();

        if !self.expect(TokenType::Equal) {
            return Err(ParserError::SyntaxError {
                exception: "expected binding for iterator in loop".to_string(),
                help: "Use right syntax: `for BINDING = ITERATOR {}`".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });
        }

        let _ = self.next();
        let iterator = self.expression(arena);

        if self.current().token_type != TokenType::LBrace {
            return Err(ParserError::SyntaxError {
                exception: "expected new block after condition".to_string(),
                help: "Consider opening new block after condition".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });
        }

        let _ = self.next();
        let mut span_block_start = self.current().span.0;
        let mut block = BumpVec::new_in(arena);

        while !self.expect(TokenType::RBrace) {
            if self.current().token_type == TokenType::EOF {
                return Err(ParserError::UnclosedExpression {
                    exception: "statements block end not found".to_string(),
                    help: "Consider adding block end after statements: `}`".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span((span_block_start, self.current().span.1)),
                });
            }

            block.push(self.statement(arena)?);
            span_block_start = self.current().span.0;
        }

        let span_end = self.current().span.1;
        if self.expect(TokenType::RBrace) {
            let _ = self.next();
        }

        self.skip_eos();
        Ok(Statements::ForStatement {
            binding,
            iterator,
            block,
            span: (span_start, span_end),
        })
    }

    pub fn fn_statement(&mut self, arena: &'a Bump) -> Result<Statements<'a>, ParserError> {
        let span_start = self.current().span.0;
        let mut public = false;
        if self.current().value == "pub" {
            public = true;
            let _ = self.next();
        }

        if let TokenType::Keyword = self.current().token_type {
            if self.current().value == "fn" {
                let _ = self.next();
            }
        }

        let identifier = self.current().value;

        let _ = self.next();
        let arguments =
            self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma, arena);

        let arguments_tuples: BumpVec<(&str, Type)> = arguments
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
            .collect_in(arena);

        let mut datatype = Type::Void;
        if !self.expect(TokenType::LBrace) {
            datatype = self.parse_type(arena);
        }

        if !self.expect(TokenType::LBrace) {
            return Err(ParserError::SyntaxError {
                exception: "expected new block after function declaration".to_string(),
                help: "Open new statements block with curly brackets".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });
        }

        let header_span = (span_start, self.current().span.1);

        let _ = self.next();
        let mut span_block_start = self.current().span.0;
        let mut block = BumpVec::new_in(arena);

        while !self.expect(TokenType::RBrace) {
            if self.current().token_type == TokenType::EOF {
                return Err(ParserError::UnclosedExpression {
                    exception: "statements block end not found".to_string(),
                    help: "Consider adding block end after statements: `}`".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span((span_block_start, self.current().span.1)),
                });
            }

            block.push(self.statement(arena)?);
            span_block_start = self.current().span.0;
        }

        let span_end = self.current().span.0 + 1;
        if self.expect(TokenType::RBrace) {
            let _ = self.next();
        }

        Ok(Statements::FunctionDefineStatement {
            name: identifier,
            datatype,
            arguments: arguments_tuples,
            block,
            public,
            span: (span_start, span_end),
            header_span,
        })
    }

    pub fn return_statement(&mut self, arena: &'a Bump) -> Result<Statements<'a>, ParserError> {
        let span_start = self.current().span.0;
        if self.expect(TokenType::Keyword) {
            let _ = self.next();
        }

        let return_expr = if self.expect(TokenType::Semicolon) {
            let _ = self.next();
            Expressions::Value(Value::Void, self.current().span)
        } else {
            self.expression(arena)
        };

        let end_span = return_expr.span().1;
        Ok(Statements::ReturnStatement {
            value: return_expr,
            span: (span_start, end_span),
        })
    }

    pub fn break_statement(&mut self, _arena: &'a Bump) -> Result<Statements<'a>, ParserError> {
        let span = self.current().span;
        if self.expect(TokenType::Keyword) {
            let _ = self.next();
        }

        self.skip_eos();

        Ok(Statements::BreakStatements { span })
    }

    pub fn assign_statement(&mut self, object: Expressions<'a>, span: (usize, usize), arena: &'a Bump) -> Result<Statements<'a>, ParserError> {
        if self.expect(TokenType::Equal) {
            let _ = self.next();
        }

        let value = self.expression(arena);
        let end_span = value.span().1;
        self.skip_eos();

        Ok(Statements::AssignStatement {
            object,
            value,
            span: (span.0, end_span),
        })
    }

    pub fn binary_assign_statement(
        &mut self,
        object: Expressions<'a>,
        op: TokenType,
        span: (usize, usize),
        arena: &'a Bump,
    ) -> Result<Statements<'a>, ParserError> {
        if self.expect(TokenType::Equal) {
            let _ = self.next();
        }

        let value = self.expression(arena);
        let end_span = value.span().1;
        self.skip_eos();

        Ok(Statements::BinaryAssignStatement {
            operand: op,
            object,
            value,
            span: (span.0, end_span),
        })
    }

    pub fn slice_assign_statement(
        &mut self,
        object: Expressions<'a>,
        span: (usize, usize),
        arena: &'a Bump,
    ) -> Result<Statements<'a>, ParserError> {
        if !self.expect(TokenType::LBrack) {
        }
        let _ = self.next();

        let index_expr = self.expression(arena);

        if !self.expect(TokenType::RBrack) {
            return Err(ParserError::UnclosedExpression {
                exception: "unclosed brackets in slice".to_string(),
                help: "Close slice index with brackets".to_string(),
                src: self.source.clone(),
                span: error::position_to_span(self.current().span),
            });
        }
        let _ = self.next();

        if !self.expect(TokenType::Equal) {
            return Err(ParserError::SyntaxError {
                exception: "expected assign operator after slice".to_string(),
                help: "Add assign operator after brackets".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span.0, self.current().span.1)),
            });
        }
        let _ = self.next();

        let value_expr = self.expression(arena);
        let end_span = value_expr.span().1;
        self.skip_eos();

        Ok(Statements::SliceAssignStatement {
            object,
            index: index_expr,
            value: value_expr,
            span: (span.0, end_span),
        })
    }

    pub fn call_statement(&mut self, id: &'a str, span: (usize, usize), arena: &'a Bump) -> Result<Statements<'a>, ParserError> {
        match self.current().token_type {
            TokenType::Identifier => {
                let _ = self.next();
            }
            TokenType::LParen => {}
            _ => {
                return Err(ParserError::UnknownExpression {
                    exception: "unknown call statement syntax".to_string(),
                    help: "Consider using right syntax: `identifier(value, ...)".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span((span.0, self.current().span.1)),
                });
            }
        };

        let arguments =
            self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma, arena);

        let span_end = if let Some(last_arg) = arguments.last() {
            last_arg.span().1
        } else {
            self.current().span.0
        };

        self.skip_eos();

        Ok(Statements::FunctionCallStatement {
            name: id,
            arguments,
            span: (span.0, span_end),
        })
    }

    pub fn macrocall_statement(&mut self, id: &'a str, span: (usize, usize), arena: &'a Bump) -> Result<Statements<'a>, ParserError> {
        if self.expect(TokenType::Not) {
            let _ = self.next();
        }

        let arguments =
            self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma, arena);

        let span_end = if let Some(last_arg) = arguments.last() {
            last_arg.span().1
        } else {
            self.current().span.0
        };

        self.skip_eos();

        Ok(Statements::MacroCallStatement {
            name: id,
            arguments,
            span: (span.0, span_end),
        })
    }

    pub fn struct_statement(&mut self, arena: &'a Bump) -> Result<Statements<'a>, ParserError> {
        let span_start = self.current().span.0;
        let mut public = false;
        if self.current().value == "pub" {
            public = true;
            let _ = self.next();
        }
        if let TokenType::Keyword = self.current().token_type {
            if self.current().value == "struct" {
                let _ = self.next();
            }
        }

        if !self.expect(TokenType::Identifier) {
            return Err(ParserError::SyntaxError {
                exception: "structure identifier not found".to_string(),
                help: "Add identifier after `struct` keyword".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });
        }

        let name = self.current().value;
        let _ = self.next();

        if !self.expect(TokenType::LBrace) {
            return Err(ParserError::SyntaxError {
                exception: "expected new block after identifier".to_string(),
                help: "Consider opening new statements block with curly brackets".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });
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
                        return Err(ParserError::DeclarationError {
                            exception: "unknown keyword in declaration found".to_string(),
                            help: String::new(),
                            src: self.source.clone(),
                            span: error::position_to_span(self.current().span),
                        });
                    }

                    method_mode = true;

                    let stmt = self.fn_statement(arena)?;

                    if let Statements::FunctionDefineStatement {
                        name,
                        ..
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
                        let err = ParserError::DeclarationError {
                            exception: "fields after methods are not allowed".to_string(),
                            help: "Move fields before methods".to_string(),
                            src: self.source.clone(),
                            span: error::position_to_span(self.current().span),
                        };
                        self.error(err.clone());
                        mode_reported = true;
                    }

                    let name = self.current().value;
                    let span = self.current().span;
                    let _ = self.next();

                    if !self.expect(TokenType::DoubleDots) {
                        return Err(ParserError::DeclarationError {
                            exception: "unknown field declaration syntax".to_string(),
                            help: "Follow this syntax: `field: type`".to_string(),
                            src: self.source.clone(),
                            span: error::position_to_span((span.0, self.current().span.1)),
                        });
                    }

                    let _ = self.next();
                    let field_type = self.parse_type(arena);

                    let extra_span = if let Some(prev) = self.previous() {
                        prev.span
                    } else {
                        self.current().span
                    };

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
                    return Err(ParserError::UnknownExpression {
                        exception: "unknown expression at the struct declaration".to_string(),
                        help: String::new(),
                        src: self.source.clone(),
                        span: error::position_to_span(self.current().span),
                    });
                }
            }
        }

        if self.expect(TokenType::RBrace) {
            let _ = self.next();
        }
        self.skip_eos();

        Ok(Statements::StructDefineStatement {
            name,
            fields,
            functions,
            public,
            span: (span_start, self.current().span.1),
        })
    }

    pub fn enum_statement(&mut self, arena: &'a Bump) -> Result<Statements<'a>, ParserError> {
        let span_start = self.current().span.0;
        let mut public = false;
        if self.current().value == "pub" {
            public = true;
            let _ = self.next();
        }
        if let TokenType::Keyword = self.current().token_type {
            if self.current().value == "enum" {
                let _ = self.next();
            }
        }

        if !self.expect(TokenType::Identifier) {
            return Err(ParserError::SyntaxError {
                exception: "identifier expected for enumeration name".to_string(),
                help: "Add identifier after `enum` keyword".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });
        }

        let name = self.current().value;
        let _ = self.next();

        if !self.expect(TokenType::LBrace) {
            return Err(ParserError::SyntaxError {
                exception: "expected new block after identifier".to_string(),
                help: "Open new statements block after identifier".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });
        }
        let _ = self.next();

        let mut fields = BumpVec::new_in(arena);
        let mut functions = BTreeMap::new();

        while !self.expect(TokenType::RBrace) {
            match self.current().token_type {
                TokenType::RBrace => break,
                TokenType::Keyword => {
                    if self.current().value != "fn" {
                        return Err(ParserError::DeclarationError {
                            exception: "unknown keyword in declaration found".to_string(),
                            help: String::new(),
                            src: self.source.clone(),
                            span: error::position_to_span(self.current().span),
                        });
                    }

                    let stmt = self.fn_statement(arena)?;

                    if let Statements::FunctionDefineStatement {
                        name,
                        ..
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
                    return Err(ParserError::UnknownExpression {
                        exception: "unknown expression at the struct declaration".to_string(),
                        help: String::new(),
                        src: self.source.clone(),
                        span: error::position_to_span(self.current().span),
                    });
                }
            }
        }

        if self.expect(TokenType::RBrace) {
            let _ = self.next();
        }
        self.skip_eos();

        Ok(Statements::EnumDefineStatement {
            name,
            fields,
            functions,
            public,
            span: (span_start, self.current().span.1),
        })
    }

    pub fn typedef_statement(&mut self, arena: &'a Bump) -> Result<Statements<'a>, ParserError> {
        let span_start = self.current().span.0;
        if self.expect(TokenType::Keyword) {
            let _ = self.next();
        }

        if !self.expect(TokenType::Identifier) {
            return Err(ParserError::SyntaxError {
                exception: "expected alias for typedef".to_string(),
                help: "Use right `typedef` syntax: \"typedef ALIAS TYPE\"".to_string(),
                src: self.source.clone(),
                span: error::position_to_span(self.current().span),
            });
        };

        let alias = self.current().value;
        let _ = self.next();

        let datatype = self.parse_type(arena);
        let span_end = self.current().span.1;
        self.skip_eos();

        Ok(Statements::TypedefStatement {
            alias,
            datatype,
            span: (span_start, span_end),
        })
    }

    pub fn extern_declare_statement(&mut self, arena: &'a Bump) -> Result<Statements<'a>, ParserError> {
        let span_start = self.current().span.0;
        if self.expect(TokenType::Keyword) {
            let _ = self.next();
        }

        if !self.expect(TokenType::Identifier) {
            return Err(ParserError::SyntaxError {
                exception: "expected identifier for extern-declare".to_string(),
                help: "Use right `extern declare` syntax: \"__extern_declare IDENTIFIER TYPE\""
                    .to_string(),
                src: self.source.clone(),
                span: error::position_to_span(self.current().span),
            });
        }

        let identifier = self.current().value;
        let _ = self.next();

        let datatype = self.parse_type(arena);
        let span_end = self.current().span.1;

        self.skip_eos();
        let span = (span_start, span_end);

        Ok(Statements::ExternDeclareStatement {
            identifier,
            datatype,
            span,
        })
    }

    pub fn link_c_statement(&mut self, arena: &'a Bump) -> Result<Statements<'a>, ParserError> {
        let span_start = self.current().span.0;
        if self.expect(TokenType::Keyword) {
            let _ = self.next();
        }

        let path = self.expression(arena);
        let span_end = path.span().1;

        if let Expressions::Value(Value::String(_), _) = path {
            Ok(Statements::LinkCStatement {
                path,
                span: (span_start, span_end),
            })
        } else {
            Err(ParserError::SyntaxError {
                exception: "expected string path".to_string(),
                help: "Consider using string constant after keyword".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, span_end)),
            })
        }
    }

    pub fn extern_statement(&mut self, arena: &'a Bump) -> Result<Statements<'a>, ParserError> {
        let span_start = self.current().span.0;
        if self.expect(TokenType::Keyword) {
            let _ = self.next();
        }

        if !self.expect(TokenType::String) {
            return Err(ParserError::SyntaxError {
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
            return Err(ParserError::UnsupportedExpression {
                exception: "unsupported extern statement found".to_string(),
                help: "Extern statement support only functions declarations".to_string(),
                src: self.source.clone(),
                span: error::position_to_span(self.current().span),
            });
        }

        let _ = self.next();
        let identifier = if self.expect(TokenType::Identifier) {
            self.current().value
        } else {
            "undefined"
        };

        if identifier == "undefined" {
             return Err(ParserError::SyntaxError {
                exception: "function identifier expected".to_string(),
                help: "Add identifier after `fn` keyword".to_string(),
                src: self.source.clone(),
                span: error::position_to_span(self.current().span),
            });
        }

        let _ = self.next();
        if !self.expect(TokenType::LParen) {
            return Err(ParserError::SyntaxError {
                exception: "expected arguments types block".to_string(),
                help: "Use syntax: `extern fn IDENTIFIER ( TYPE, ... ) TYPE`".to_string(),
                src: self.source.clone(),
                span: error::position_to_span(self.current().span),
            });
        }

        let mut arguments = BumpVec::new_in(arena);
        let mut is_var_args = false;
        let _ = self.next();

        while !self.expect(TokenType::RParen) {
            if self.current().token_type == TokenType::Dot {
                if let (Some(p1), Some(p2)) = (self.peek(), self.peek_nth(2)) {
                    if p1.token_type == TokenType::Dot && p2.token_type == TokenType::Dot {
                        is_var_args = true;
                        let _ = self.next();
                        let _ = self.next();
                        let _ = self.next();
                        continue;
                    }
                }
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

            arguments.push(self.parse_type(arena));
        }

        if self.expect(TokenType::RParen) {
            let _ = self.next();
        }

        let mut return_type = Type::Void;
        if !self.expect(TokenType::Semicolon) {
            return_type = self.parse_type(arena);
        }

        let span_end = self.current().span.1;
        self.skip_eos();

        Ok(Statements::ExternStatement {
            identifier,
            arguments,
            return_type,
            extern_type,
            public,
            is_var_args,
            span: (span_start, span_end),
        })
    }
}
