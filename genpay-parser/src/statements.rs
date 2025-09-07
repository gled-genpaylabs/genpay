use crate::{
    END_STATEMENT, Parser,
    error::{self, ParserError},
    expressions::Expressions,
    types::Type,
    value::Value,
};
use genpay_lexer::token_type::TokenType;
use indexmap::IndexMap;

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
        fields: IndexMap<String, Type>,
        functions: IndexMap<String, Statements>,
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
        functions: IndexMap<String, Statements>,
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

impl Parser {
    #[inline]
    pub fn get_span_statement(stmt: &Statements) -> (usize, usize) {
        match stmt {
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
            Statements::Expression(expr) => Self::get_span_expression(expr),
            Statements::None => (0, 0), // или можно вернуть Option<(usize, usize)>
        }
    }
}

#[cfg(test)]
mod tests {
    use genpay_lexer::Lexer;
    use crate::{
        Parser, expressions::Expressions, statements::Statements, types::Type, value::Value,
    };

    #[test]
    fn assign_statement() {
        const SRC: &str = "some_var = 5;";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::AssignStatement {
                object,
                value,
                span: _,
            }) => {
                if let Expressions::Value(Value::Identifier(identifier), _) = object {
                    assert_eq!(identifier, "some_var");
                } else {
                    panic!("Wrong obj expr parsed")
                }

                if let Expressions::Value(Value::Integer(5), _) = value {
                } else {
                    panic!("Wrong value expr found")
                };
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn binary_assign_statement() {
        const SRC: &str = "some_var += 5;";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::BinaryAssignStatement {
                object,
                operand,
                value,
                span: _,
            }) => {
                if let Expressions::Value(Value::Identifier(identifier), _) = object {
                    assert_eq!(identifier, "some_var");
                }

                assert_eq!(operand, "+");

                if let Expressions::Value(Value::Integer(5), _) = value {
                } else {
                    panic!("Wrong value expr found")
                };
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn deref_assign_statement() {
        const SRC: &str = "*ptr = 5;";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::DerefAssignStatement {
                object,
                value,
                span: _,
            }) => {
                if let Expressions::Value(Value::Identifier(identifier), _) = object {
                    assert_eq!(identifier, "ptr");
                }

                if let Expressions::Value(Value::Integer(5), _) = value {
                } else {
                    panic!("Wrong value expr found")
                };
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn slice_assign_statement() {
        const SRC: &str = "list[0] = 5;";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::SliceAssignStatement {
                object,
                index,
                value,
                span: _,
            }) => {
                if let Expressions::Value(Value::Identifier(identifier), _) = object {
                    assert_eq!(identifier, "list");
                }

                if let Expressions::Value(Value::Integer(0), _) = index {
                } else {
                    panic!("Wrong index expr parsed")
                }
                if let Expressions::Value(Value::Integer(5), _) = value {
                } else {
                    panic!("Wrong value expr parsed")
                };
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn field_assign_statement() {
        const SRC: &str = "some_struct.field = 12";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::FieldAssignStatement {
                object,
                value,
                span: _,
            }) => {
                if let Expressions::SubElement {
                    head,
                    subelements,
                    span: _,
                } = object
                {
                    if let Expressions::Value(Value::Identifier(id), _) = *head.clone() {
                        assert_eq!(id, "some_struct")
                    } else {
                        panic!("Wrong head expr found")
                    };
                    if let Some(Expressions::Value(Value::Identifier(field), _)) = subelements.first() {
                        assert_eq!(field, "field");
                    } else {
                        panic!("Wrong subelement expr found")
                    }
                } else {
                    panic!("Wrong value expr parsed")
                };
                if let Expressions::Value(Value::Integer(12), _) = value {
                } else {
                    panic!("Wrong value expr parsed")
                };
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn annotation_statement() {
        const SRC: &str = "let var;";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::AnnotationStatement {
                identifier,
                datatype,
                value,
                span: _,
            }) => {
                assert_eq!(identifier, "var");
                assert!(datatype.is_none());
                assert!(value.is_none());
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn annotation_statement_with_type() {
        const SRC: &str = "let var: i32;";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::AnnotationStatement {
                identifier,
                datatype,
                value,
                span: _,
            }) => {
                assert_eq!(identifier, "var");
                assert!(datatype.is_some());
                assert!(value.is_none());

                assert_eq!(datatype.clone().unwrap(), Type::I32);
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn annotation_statement_with_value() {
        const SRC: &str = "let var = 15;";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::AnnotationStatement {
                identifier,
                datatype,
                value,
                span: _,
            }) => {
                assert_eq!(identifier, "var");
                assert!(datatype.is_none());
                assert!(value.is_some());

                if let Some(Expressions::Value(Value::Integer(15), _)) = value {
                } else {
                    panic!("Wrong value expr parsed")
                }
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn annotation_statement_full() {
        const SRC: &str = "let var: usize = 15;";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::AnnotationStatement {
                identifier,
                datatype,
                value,
                span: _,
            }) => {
                assert_eq!(identifier, "var");
                assert!(datatype.is_some());
                assert!(value.is_some());

                assert_eq!(datatype.clone().unwrap(), Type::USIZE);
                if let Some(Expressions::Value(Value::Integer(15), _)) = value {
                } else {
                    panic!("Wrong value expr parsed")
                }
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn function_define_statement() {
        const SRC: &str = "fn foo() {}";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::FunctionDefineStatement {
                name,
                datatype,
                arguments,
                block,
                public,
                span: _,
                header_span: _,
            }) => {
                assert_eq!(name, "foo");
                assert_eq!(datatype, &Type::Void);
                assert!(arguments.is_empty());
                assert!(block.is_empty());
                assert!(!public);
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn function_define_statement_with_type() {
        const SRC: &str = "fn foo() usize {}";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::FunctionDefineStatement {
                name,
                datatype,
                arguments,
                block,
                public,
                span: _,
                header_span: _,
            }) => {
                assert_eq!(name, "foo");
                assert_eq!(datatype, &Type::USIZE);
                assert!(arguments.is_empty());
                assert!(block.is_empty());
                assert!(!public);
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn function_define_statement_with_args() {
        const SRC: &str = "fn foo(a: i32, b: u64) usize {}";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::FunctionDefineStatement {
                name,
                datatype,
                arguments,
                block,
                public,
                span: _,
                header_span: _,
            }) => {
                assert_eq!(name, "foo");
                assert_eq!(datatype, &Type::USIZE);
                assert!(block.is_empty());
                assert!(!arguments.is_empty());
                assert!(!public);

                if let Some((argname, argtype)) = arguments.first() {
                    assert_eq!(argname, "a");
                    assert_eq!(argtype, &Type::I32);
                } else {
                    panic!("Wrong argument expr parsed")
                }

                if let Some((argname, argtype)) = arguments.get(1) {
                    assert_eq!(argname, "b");
                    assert_eq!(argtype, &Type::U64);
                } else {
                    panic!("Wrong argument expr parsed")
                }
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn function_define_statement_with_block() {
        const SRC: &str = "fn foo(a: i32, b: u64) { return 1; }";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::FunctionDefineStatement {
                name,
                datatype,
                arguments,
                block,
                public,
                span: _,
                header_span: _,
            }) => {
                assert_eq!(name, "foo");
                assert_eq!(datatype, &Type::Void);
                assert!(!block.is_empty());
                assert!(!arguments.is_empty());
                assert!(!public);

                if let Some((argname, argtype)) = arguments.first() {
                    assert_eq!(argname, "a");
                    assert_eq!(argtype, &Type::I32);
                } else {
                    panic!("Wrong argument expr parsed")
                }

                if let Some((argname, argtype)) = arguments.get(1) {
                    assert_eq!(argname, "b");
                    assert_eq!(argtype, &Type::U64);
                } else {
                    panic!("Wrong argument expr parsed")
                }

                if let Some(Statements::ReturnStatement { value, span: _ }) = block.first() {
                    if let Expressions::Value(Value::Integer(1), _) = value {
                    } else {
                        panic!("Wrong value in statement block parsed")
                    }
                } else {
                    panic!("Wrong statement parsed")
                }
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn function_define_statement_public() {
        const SRC: &str = "pub fn foo(a: i32, b: u64) { return 1; }";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::FunctionDefineStatement {
                name,
                datatype,
                arguments,
                block,
                public,
                span: _,
                header_span: _,
            }) => {
                assert_eq!(name, "foo");
                assert_eq!(datatype, &Type::Void);
                assert!(!block.is_empty());
                assert!(!arguments.is_empty());
                assert!(public);

                if let Some((argname, argtype)) = arguments.first() {
                    assert_eq!(argname, "a");
                    assert_eq!(argtype, &Type::I32);
                } else {
                    panic!("Wrong argument expr parsed")
                }

                if let Some((argname, argtype)) = arguments.get(1) {
                    assert_eq!(argname, "b");
                    assert_eq!(argtype, &Type::U64);
                } else {
                    panic!("Wrong argument expr parsed")
                }

                if let Some(Statements::ReturnStatement { value, span: _ }) = block.first() {
                    if let Expressions::Value(Value::Integer(1), _) = value {
                    } else {
                        panic!("Wrong value in statement block parsed")
                    }
                } else {
                    panic!("Wrong statement parsed")
                }
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn function_call_statement() {
        const SRC: &str = "foo()";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::FunctionCallStatement {
                name,
                arguments,
                span: _,
            }) => {
                assert_eq!(name, "foo");
                assert!(arguments.is_empty());
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn function_call_advanced_statement() {
        const SRC: &str = "foo(1, 2)";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::FunctionCallStatement {
                name,
                arguments,
                span: _,
            }) => {
                assert_eq!(name, "foo");
                assert!(!arguments.is_empty());

                if let Some(Expressions::Value(Value::Integer(1), _)) = arguments.first() {
                } else {
                    panic!("Wrong #1 argument parsed")
                }
                if let Some(Expressions::Value(Value::Integer(2), _)) = arguments.get(1) {
                } else {
                    panic!("Wrong #2 argument parsed")
                }
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn struct_define_statement() {
        const SRC: &str = "struct Person { name: *char, age: u8 }";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::StructDefineStatement {
                name,
                fields,
                functions,
                public,
                span: _,
            }) => {
                assert_eq!(name, "Person");
                assert!(!fields.is_empty());
                assert!(!public);
                assert!(functions.is_empty());

                if let Some(Type::Pointer(_)) = fields.get("name") {
                } else {
                    panic!("Wrong argument parsed")
                }
                if let Some(Type::U8) = fields.get("age") {
                } else {
                    panic!("Wrong argument parsed")
                }
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn struct_define_with_fn_statement() {
        const SRC: &str = "struct Person { name: *char, age: u8, fn foo() {} }";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::StructDefineStatement {
                name,
                fields,
                functions,
                public,
                span: _,
            }) => {
                assert_eq!(name, "Person");
                assert!(!fields.is_empty());
                assert!(!public);
                assert!(!functions.is_empty());

                if let Some(Type::Pointer(_)) = fields.get("name") {
                } else {
                    panic!("Wrong argument parsed")
                }
                if let Some(Type::U8) = fields.get("age") {
                } else {
                    panic!("Wrong argument parsed")
                }

                if let Some(Statements::FunctionDefineStatement {
                    name,
                    datatype,
                    arguments: _,
                    block: _,
                    public: _,
                    span: _,
                    header_span: _,
                }) = functions.get("foo")
                {
                    assert_eq!(name, "foo");
                    assert_eq!(datatype, &Type::Void);
                }
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn struct_define_public_statement() {
        const SRC: &str = "pub struct Person { name: *char, age: u8, fn foo() {} }";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::StructDefineStatement {
                name,
                fields,
                functions,
                public,
                span: _,
            }) => {
                assert_eq!(name, "Person");
                assert!(!fields.is_empty());
                assert!(!functions.is_empty());
                assert!(public);

                if let Some(Type::Pointer(_)) = fields.get("name") {
                } else {
                    panic!("Wrong argument parsed")
                }
                if let Some(Type::U8) = fields.get("age") {
                } else {
                    panic!("Wrong argument parsed")
                }

                if let Some(Statements::FunctionDefineStatement {
                    name,
                    datatype,
                    arguments: _,
                    block: _,
                    public: _,
                    span: _,
                    header_span: _,
                }) = functions.get("foo")
                {
                    assert_eq!(name, "foo");
                    assert_eq!(datatype, &Type::Void);
                } else {
                    panic!("Wrong function define stmt parsed!")
                }
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn enum_define_statement() {
        const SRC: &str = "enum ABC { A, B, C }";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::EnumDefineStatement {
                name,
                fields,
                functions,
                public,
                span: _,
            }) => {
                assert_eq!(name, "ABC");
                assert!(!fields.is_empty());
                assert!(!public);
                assert!(functions.is_empty());

                if let Some("A") = fields.first().map(|x| x.as_str()) {
                } else {
                    panic!("Wrong field parsed")
                };
                if let Some("B") = fields.get(1).map(|x| x.as_str()) {
                } else {
                    panic!("Wrong field parsed")
                };
                if let Some("C") = fields.get(2).map(|x| x.as_str()) {
                } else {
                    panic!("Wrong field parsed")
                };
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn enum_define_with_fn_statement() {
        const SRC: &str = "enum ABC { A, B, C, fn foo() {} }";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::EnumDefineStatement {
                name,
                fields,
                functions,
                public,
                span: _,
            }) => {
                assert_eq!(name, "ABC");
                assert!(!fields.is_empty());
                assert!(!public);
                assert!(!functions.is_empty());

                if let Some("A") = fields.first().map(|x| x.as_str()) {
                } else {
                    panic!("Wrong field parsed")
                };
                if let Some("B") = fields.get(1).map(|x| x.as_str()) {
                } else {
                    panic!("Wrong field parsed")
                };
                if let Some("C") = fields.get(2).map(|x| x.as_str()) {
                } else {
                    panic!("Wrong field parsed")
                };

                if let Some(Statements::FunctionDefineStatement {
                    name,
                    datatype,
                    arguments: _,
                    block: _,
                    public: _,
                    span: _,
                    header_span: _,
                }) = functions.get("foo")
                {
                    assert_eq!(name, "foo");
                    assert_eq!(datatype, &Type::Void);
                } else {
                    panic!("Wrong function define stmt parsed")
                }
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn enum_define_pub_statement() {
        const SRC: &str = "pub enum ABC { A, B, C }";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::EnumDefineStatement {
                name,
                fields,
                functions,
                public,
                span: _,
            }) => {
                assert_eq!(name, "ABC");
                assert!(!fields.is_empty());
                assert!(public);
                assert!(functions.is_empty());

                if let Some("A") = fields.first().map(|x| x.as_str()) {
                } else {
                    panic!("Wrong field parsed")
                };
                if let Some("B") = fields.get(1).map(|x| x.as_str()) {
                } else {
                    panic!("Wrong field parsed")
                };
                if let Some("C") = fields.get(2).map(|x| x.as_str()) {
                } else {
                    panic!("Wrong field parsed")
                };
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn typedef_statement() {
        const SRC: &str = "typedef int i32";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::TypedefStatement {
                alias,
                datatype,
                span: _,
            }) => {
                assert_eq!(alias, "int");
                assert_eq!(datatype, &Type::I32);
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn typedef_advanced_statement() {
        const SRC: &str = "typedef array_ptr *[i32; 5]";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::TypedefStatement {
                alias,
                datatype,
                span: _,
            }) => {
                assert_eq!(alias, "array_ptr");
                assert_eq!(
                    datatype,
                    &Type::Pointer(Box::new(Type::Array(Box::new(Type::I32), 5)))
                );
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn if_statement() {
        const SRC: &str = "if true {};";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::IfStatement {
                condition,
                then_block,
                else_block,
                span: _,
            }) => {
                assert!(else_block.is_none());
                assert!(then_block.is_empty());

                if let Expressions::Value(Value::Boolean(true), _) = condition {
                } else {
                    panic!("Wrong condition expr parsed")
                }
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn if_else_statement() {
        const SRC: &str = "if true {} else {};";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::IfStatement {
                condition,
                then_block,
                else_block,
                span: _,
            }) => {
                assert!(else_block.is_some());
                assert!(then_block.is_empty());

                if let Expressions::Value(Value::Boolean(true), _) = condition {
                } else {
                    panic!("Wrong condition expr parsed")
                }
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn while_statement() {
        const SRC: &str = "while true {}";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::WhileStatement {
                condition,
                block: _,
                span: _,
            }) => {
                if let Expressions::Value(Value::Boolean(true), _) = condition {
                } else {
                    panic!("Wrong condition expr parsed")
                }
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn for_statement() {
        const SRC: &str = "for i = 5 {}";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::ForStatement {
                binding,
                iterator,
                block: _,
                span: _,
            }) => {
                assert_eq!(binding, "i");
                if let Expressions::Value(Value::Integer(5), _) = iterator {
                } else {
                    panic!("Wrong iterator obj parsed")
                }
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn import_statement() {
        const SRC: &str = "import \"module.pay\"";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::ImportStatement { path, span: _ }) => {
                if let Expressions::Value(Value::String(str), _) = path {
                    assert_eq!(str, "module.pay")
                } else {
                    panic!("Wrong import object expr parsed")
                };
            }
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn break_statement() {
        const SRC: &str = "break";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::BreakStatements { span: _ }) => {}
            _ => panic!("Wrong statement parsed"),
        }
    }

    #[test]
    fn return_statement() {
        const SRC: &str = "return 15;";
        const FILENAME: &str = "test.pay";

        let mut lexer = Lexer::new(SRC, "test.pay");
        let (tokens, _) = lexer.tokenize().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        match ast.first() {
            Some(Statements::ReturnStatement { value, span: _ }) => {
                if let Expressions::Value(Value::Integer(15), _) = value {
                } else {
                    panic!("Wrong return expr parsed")
                }
            }
            _ => panic!("Wrong statement parsed"),
        }
    }
}

impl Parser {
    pub fn annotation_statement(&mut self) -> Statements {
        let span_start = self.current().span.0;

        if self.current().value == *"let" {
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
                let value = self.expression();

                self.skip_eos();

                Statements::AnnotationStatement {
                    identifier: id,
                    datatype,
                    value: Some(value.clone()),
                    span: (span_start, self.span_expression(value).1),
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

    pub fn import_statement(&mut self) -> Statements {
        let span_start = self.current().span.0;
        if self.current().token_type == TokenType::Keyword {
            let _ = self.next();
        }

        let path = self.expression();
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

    pub fn include_statement(&mut self) -> Statements {
        let span_start = self.current().span.0;
        if self.current().token_type == TokenType::Keyword {
            let _ = self.next();
        }

        let path = self.expression();
        let span_end = Self::get_span_expression(&path).1;

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

    pub fn if_statement(&mut self) -> Statements {
        let span_start = self.current().span.0;
        if self.current().token_type == TokenType::Keyword {
            let _ = self.next();
        }

        let condition = self.expression();

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

            then_block.push(self.statement());
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
                        if self.current().value != *"if" {
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

                        let stmt = self.if_statement();
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

                    else_block.push(self.statement());
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

    pub fn while_statement(&mut self) -> Statements {
        let span_start = self.current().span.0;
        if let TokenType::Keyword = self.current().token_type {
            let _ = self.next();
        }

        let condition = self.expression();

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

            block.push(self.statement());
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

    pub fn for_statement(&mut self) -> Statements {
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
        let iterator = self.expression();

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

            block.push(self.statement());
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

    pub fn fn_statement(&mut self) -> Statements {
        let span_start = self.current().span.0;
        if let TokenType::Keyword = self.current().token_type {
            let _ = self.next();
        }

        let identifier = self.current().value;

        let _ = self.next();
        let arguments =
            self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma);

        let arguments_tuples = arguments
            .iter()
            .map(|arg| {
                if let Expressions::Argument {
                    name,
                    r#type,
                    span: _,
                } = arg
                {
                    (name.clone(), r#type.clone())
                } else {
                    // okay lets just skip this piece of code.
                    // I've made a mistake creating this embedded code, but we all make mistakes.
                    // Anyways I just wanted short code to unwrap and compare identifier inside
                    // embedded boxed expressions blocks, so...

                    if let Expressions::Reference { object, span: _ } = arg {
                        if let Expressions::Value(Value::Identifier(id), _) = *object.clone() {
                            if id == "self" {
                                return (id, Type::SelfRef);
                            }
                        }
                    }

                    self.error(ParserError::DeclarationError {
                        exception: "unexpected argument declaration found".to_string(),
                        help: "Use right arguments syntax: `identifier: type`".to_string(),
                        src: self.source.clone(),
                        span: error::position_to_span(self.span_expression(arg.clone())),
                    });

                    (String::new(), Type::Void)
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

            block.push(self.statement());
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

    pub fn return_statement(&mut self) -> Statements {
        let span_start = self.current().span.0;
        if self.expect(TokenType::Keyword) {
            let _ = self.next();
        }

        let return_expr = if self.expect(TokenType::Semicolon) {
            let _ = self.next();
            Expressions::Value(Value::Void, self.current().span)
        } else {
            self.expression()
        };

        Statements::ReturnStatement {
            value: return_expr.clone(),
            span: (span_start, self.span_expression(return_expr).1),
        }
    }

    pub fn break_statement(&mut self) -> Statements {
        let span = self.current().span;
        if self.expect(TokenType::Keyword) {
            let _ = self.next();
        }

        self.skip_eos();

        Statements::BreakStatements { span }
    }

    pub fn assign_statement(&mut self, object: Expressions, span: (usize, usize)) -> Statements {
        if self.expect(TokenType::Equal) {
            let _ = self.next();
        }

        let value = self.expression();
        let span_end = self.current().span.1;

        Statements::AssignStatement {
            object,
            value,
            span: (span.0, span_end - 3),
        }
    }

    pub fn binary_assign_statement(
        &mut self,
        object: Expressions,
        op: String,
        span: (usize, usize),
    ) -> Statements {
        if self.expect(TokenType::Equal) {
            let _ = self.next();
        }

        let value = self.expression();
        let span_end = self.current().span.1;
        self.skip_eos();

        Statements::BinaryAssignStatement {
            operand: op,
            object,
            value,
            span: (span.0, span_end - 3),
        }
    }

    pub fn slice_assign_statement(
        &mut self,
        object: Expressions,
        span: (usize, usize),
    ) -> Statements {
        let brackets_span_start = self.current().span.0;
        if self.expect(TokenType::LBrack) {
            let _ = self.next();
        }

        let ind = self.expression();
        let brackets_span_end = self.current().span.1;

        if !self.expect(TokenType::RBrack) {
            self.error(ParserError::UnclosedExpression {
                exception: "unclosed brackets in slice".to_string(),
                help: "Close slice index with brackets".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((brackets_span_start, brackets_span_end)),
            });

            return Statements::None;
        }

        let _ = self.next();
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

        let _ = self.next();
        let val = self.expression();
        self.skip_eos();

        let span_end = self.current().span.1;

        Statements::SliceAssignStatement {
            object,
            index: ind,
            value: val,
            span: (span.0, span_end),
        }
    }

    pub fn call_statement(&mut self, id: String, span: (usize, usize)) -> Statements {
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
            self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma);

        self.position -= 1;
        let span_end = self.current().span.1;
        self.position += 1;

        self.skip_eos();

        Statements::FunctionCallStatement {
            name: id,
            arguments,
            span: (span.0, span_end),
        }
    }

    pub fn macrocall_statement(&mut self, id: String, span: (usize, usize)) -> Statements {
        if self.expect(TokenType::Not) {
            let _ = self.next();
        }

        let arguments =
            self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma);

        self.position -= 1;
        let span_end = self.current().span.1;
        self.position += 1;

        self.skip_eos();

        Statements::MacroCallStatement {
            name: id,
            arguments,
            span: (span.0, span_end),
        }
    }

    pub fn struct_statement(&mut self) -> Statements {
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
        let mut fields = IndexMap::new();
        let mut functions = IndexMap::new();

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

                    let stmt = self.fn_statement();

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
                        functions.insert(name.to_owned(), stmt);
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

                    if fields.contains_key(&name) {
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

    pub fn enum_statement(&mut self) -> Statements {
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
        let mut functions = IndexMap::new();

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

                    let stmt = self.fn_statement();

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
                        functions.insert(name.to_owned(), stmt);
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

    pub fn typedef_statement(&mut self) -> Statements {
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

    pub fn extern_declare_statement(&mut self) -> Statements {
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

    pub fn link_c_statement(&mut self) -> Statements {
        let span_start = self.current().span.0;
        if self.expect(TokenType::Keyword) {
            let _ = self.next();
        }

        let path = self.expression();
        let span_end = Self::get_span_expression(&path).1;

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

    pub fn extern_statement(&mut self) -> Statements {
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

            "undefined".to_string()
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
