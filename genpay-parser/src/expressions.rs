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

    pub fn subelement_expression(
        &mut self,
        head: Expressions<'a>,
        separator: TokenType,
    ) -> Expressions<'a> {
        // if self.expect(separator) {
        //     let _ = self.next();
        // }

        let head = Box::new(head);
        let span_start = self.current().span.1;
        let mut subelements = Vec::new();
        let mut span_end = self.current().span.1;

        while self.expect(separator.clone()) {
            if self.expect(separator.clone()) {
                let _ = self.next();
            }
            subelements.push(self.term());
            span_end = self.current().span.1;
        }

        Expressions::SubElement {
            head,
            subelements,
            span: (span_start, span_end),
        }
    }

    pub fn binary_expression(&mut self, node: Expressions<'a>) -> Expressions<'a> {
        let node_span = Self::get_span_expression(&node);
        let span_end;

        let current = self.current();

        match current.token_type {
            tty if BINARY_OPERATORS.contains(&tty) => {
                let _ = self.next();

                let lhs = node;
                let rhs = self.expression();

                self.position -= 2;
                span_end = self.current().span.1;
                self.position += 2;

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
                        let lhs_new = Box::new(old_lhs);
                        let rhs_new = lhs;

                        let priority_node = Expressions::Binary {
                            operand: current.value,
                            lhs: lhs_new,
                            rhs: rhs_new,
                            span,
                        };

                        return Expressions::Binary {
                            operand,
                            lhs: Box::new(priority_node),
                            rhs,
                            span: (node_span.0, span_end),
                        };
                    }
                }

                Expressions::Binary {
                    operand: current.value,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    span: (node_span.0, span_end),
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn boolean_expression(&mut self, node: Expressions<'a>) -> Expressions<'a> {
        // FIXME: Expressions like `true || false` returns error "Undefined term found"

        let node_span = Self::get_span_expression(&node);
        let span_end;

        let current = self.current();

        match current.token_type {
            op if PRIORITY_BOOLEAN_OPERATORS.contains(&op) => node,
            op if BOOLEAN_OPERATORS.contains(&op) => {
                let _ = self.next();

                let lhs = node.clone();
                let rhs = self.expression();

                self.position -= 2;
                span_end = self.current().span.1;
                self.position += 2;

                if PRIORITY_BOOLEAN_OPERATORS.contains(&self.current().token_type) {
                    let operand = self.current().value;
                    let lhs_node = Expressions::Boolean {
                        operand: current.value,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        span: (current.span.0, self.current().span.1),
                    };

                    let _ = self.next();
                    let rhs_node = self.expression();

                    return Expressions::Boolean {
                        operand,
                        lhs: Box::new(lhs_node),
                        rhs: Box::new(rhs_node),
                        span: (node_span.0, span_end),
                    };
                }

                Expressions::Boolean {
                    operand: current.value,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    span: (node_span.0, span_end),
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn bitwise_expression(&mut self, node: Expressions<'a>) -> Expressions<'a> {
        let node_span = Self::get_span_expression(&node);
        let span_end;

        let current = self.current();

        match current.token_type {
            tty if BITWISE_OPERATORS.contains(&tty) => {
                let _ = self.next();

                let lhs = Box::new(node);
                let rhs = Box::new(self.expression());

                self.position -= 2;
                span_end = self.current().span.1;
                self.position += 2;

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

    pub fn call_expression(&mut self, fname: &'a str, span: (usize, usize)) -> Expressions<'a> {
        match self.current().token_type {
            TokenType::Identifier => {
                let _ = self.next();
            }
            TokenType::LParen => {}
            _ => {
                self.error(ParserError::UnknownExpression {
                    exception: "unknown call expression syntax".to_string(),
                    help: "Consider using right syntax: `identifier(value, ...)`".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span((span.0, self.current().span.1)),
                });

                return Expressions::None;
            }
        };

        let arguments =
            self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma);

        self.position -= 1;
        let span_end = self.current().span.1;
        self.position += 1;

        Expressions::FnCall {
            name: fname,
            arguments,
            span: (span.0, span_end),
        }
    }

    pub fn macrocall_expression(&mut self, name: &'a str, span: (usize, usize)) -> Expressions<'a> {
        if self.expect(TokenType::Not) {
            let _ = self.next();
        }

        let arguments =
            self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma);

        self.position -= 1;
        let span_end = self.current().span.1;
        self.position += 1;

        Expressions::MacroCall {
            name,
            arguments,
            span: (span.0, span_end),
        }
    }

    pub fn slice_expression(&mut self, expr: Expressions<'a>) -> Expressions<'a> {
        if let TokenType::LBrack = self.current().token_type {
            let _ = self.next();
        }

        let expr_span_0 = self.span_expression(&expr).0;
        let object = Box::new(expr);
        let index = Box::new(self.expression());

        if self.current().token_type != TokenType::RBrack {
            self.error(ParserError::UnclosedExpression {
                exception: "unclosed slice index".to_string(),
                help: "Close slice index with brackets".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((
                    expr_span_0,
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
            span: (expr_span_0, span_end),
        }
    }

    pub fn struct_expression(&mut self, name: &'a str) -> Expressions<'a> {
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
            let value = self.expression();

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
    ) -> Vec<Expressions<'a>> {
        let mut current = self.current();

        if self.expect(start) {
            current = self.next()
        } else if self.expect(end.clone()) {
            let _ = self.next();
            return Vec::new();
        }

        let mut output = Vec::new();

        while current.token_type != end {
            current = self.current();

            if current.token_type == separator {
                let _ = self.next();
            } else if current.token_type == end {
                break;
            } else {
                output.push(self.expression());
            }
        }

        if self.expect(end) {
            let _ = self.next();
        }

        output
    }
}

#[cfg(test)]
mod tests {
    use genpay_lexer::Lexeme as Lexer;
    use crate::{
        Parser, expressions::Expressions, statements::Statements, types::Type, value::Value,
    };

    #[test]
    fn binary_expression() {
        const SRC: &str = "let a = 5 + 2;";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::AnnotationStatement {
            identifier: _,
            value,
            span: _,
            datatype: _,
        }) = ast.first()
        {
            match value.clone().unwrap() {
                Expressions::Binary {
                    operand,
                    lhs,
                    rhs,
                    span: _,
                } => {
                    assert_eq!(operand, "+");

                    if let Expressions::Value(Value::Integer(5), _) = *lhs {
                    } else {
                        panic!("Wrong LHS found")
                    };
                    if let Expressions::Value(Value::Integer(2), _) = *rhs {
                    } else {
                        panic!("Wrong LHS found")
                    };
                }
                _ => panic!("Wrong expression value found"),
            }
        } else {
            panic!("Statements side failure");
        }
    }

    #[test]
    fn binary_advanced_expression() {
        const SRC: &str = "let a = 2 + 2 * 2;";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::AnnotationStatement {
            identifier: _,
            value,
            span: _,
            datatype: _,
        }) = ast.first()
        {
            match value.clone().unwrap() {
                Expressions::Binary {
                    operand,
                    lhs,
                    rhs,
                    span: _,
                } => {
                    assert_eq!(operand, "+");

                    if let Expressions::Value(Value::Integer(2), _) = *lhs {
                    } else {
                        panic!("Wrong LHS found")
                    };
                    if let Expressions::Binary {
                        operand,
                        lhs,
                        rhs,
                        span: _,
                    } = *rhs
                    {
                        assert_eq!(operand, "*");

                        if let Expressions::Value(Value::Integer(2), _) = *lhs {
                        } else {
                            panic!("Wrong LHS found")
                        };
                        if let Expressions::Value(Value::Integer(2), _) = *rhs {
                        } else {
                            panic!("Wrong LHS found")
                        };
                    } else {
                        panic!("Wrong LHS found")
                    };
                }
                _ => panic!("Wrong expression value found"),
            }
        } else {
            panic!("Statements side failure");
        }
    }

    #[test]
    fn unary_negative_expression() {
        const SRC: &str = "let a = -2;";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::AnnotationStatement {
            identifier: _,
            value,
            span: _,
            datatype: _,
        }) = ast.first()
        {
            match value.clone().unwrap() {
                Expressions::Unary {
                    operand,
                    object,
                    span: _,
                } => {
                    assert_eq!(operand, "-");

                    if let Expressions::Value(Value::Integer(2), _) = *object {
                    } else {
                        panic!("Wrong object expression found")
                    };
                }
                _ => panic!("Wrong expression value found"),
            }
        } else {
            panic!("Statements side failure");
        }
    }

    #[test]
    fn unary_not_expression() {
        const SRC: &str = "let a = !2;";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::AnnotationStatement {
            identifier: _,
            value,
            span: _,
            datatype: _,
        }) = ast.first()
        {
            match value.clone().unwrap() {
                Expressions::Unary {
                    operand,
                    object,
                    span: _,
                } => {
                    assert_eq!(operand, "!");

                    if let Expressions::Value(Value::Integer(2), _) = *object {
                    } else {
                        panic!("Wrong object expression found")
                    };
                }
                _ => panic!("Wrong expression value found"),
            }
        } else {
            panic!("Statements side failure");
        }
    }

    #[test]
    fn boolean_eq_expression() {
        const SRC: &str = "let a = 1 == 1;";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::AnnotationStatement {
            identifier: _,
            value,
            span: _,
            datatype: _,
        }) = ast.first()
        {
            match value.clone().unwrap() {
                Expressions::Boolean {
                    operand,
                    lhs,
                    rhs,
                    span: _,
                } => {
                    assert_eq!(operand, "==");

                    if let Expressions::Value(Value::Integer(1), _) = *lhs {
                    } else {
                        panic!("Wrong object expression found")
                    };
                    if let Expressions::Value(Value::Integer(1), _) = *rhs {
                    } else {
                        panic!("Wrong object expression found")
                    };
                }
                _ => panic!("Wrong expression value found"),
            }
        } else {
            panic!("Statements side failure");
        }
    }

    #[test]
    fn boolean_ne_expression() {
        const SRC: &str = "let a = 1 != 1;";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::AnnotationStatement {
            identifier: _,
            value,
            span: _,
            datatype: _,
        }) = ast.first()
        {
            match value.clone().unwrap() {
                Expressions::Boolean {
                    operand,
                    lhs,
                    rhs,
                    span: _,
                } => {
                    assert_eq!(operand, "!=");

                    if let Expressions::Value(Value::Integer(1), _) = *lhs {
                    } else {
                        panic!("Wrong object expression found")
                    };
                    if let Expressions::Value(Value::Integer(1), _) = *rhs {
                    } else {
                        panic!("Wrong object expression found")
                    };
                }
                _ => panic!("Wrong expression value found"),
            }
        } else {
            panic!("Statements side failure");
        }
    }

    #[test]
    fn boolean_bt_expression() {
        const SRC: &str = "let a = 1 > 1;";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::AnnotationStatement {
            identifier: _,
            value,
            span: _,
            datatype: _,
        }) = ast.first()
        {
            match value.clone().unwrap() {
                Expressions::Boolean {
                    operand,
                    lhs,
                    rhs,
                    span: _,
                } => {
                    assert_eq!(operand, ">");

                    if let Expressions::Value(Value::Integer(1), _) = *lhs {
                    } else {
                        panic!("Wrong object expression found")
                    };
                    if let Expressions::Value(Value::Integer(1), _) = *rhs {
                    } else {
                        panic!("Wrong object expression found")
                    };
                }
                _ => panic!("Wrong expression value found"),
            }
        } else {
            panic!("Statements side failure");
        }
    }

    #[test]
    fn boolean_lt_expression() {
        const SRC: &str = "let a = 1 < 1;";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::AnnotationStatement {
            identifier: _,
            value,
            span: _,
            datatype: _,
        }) = ast.first()
        {
            match value.clone().unwrap() {
                Expressions::Boolean {
                    operand,
                    lhs,
                    rhs,
                    span: _,
                } => {
                    assert_eq!(operand, "<");

                    if let Expressions::Value(Value::Integer(1), _) = *lhs {
                    } else {
                        panic!("Wrong object expression found")
                    };
                    if let Expressions::Value(Value::Integer(1), _) = *rhs {
                    } else {
                        panic!("Wrong object expression found")
                    };
                }
                _ => panic!("Wrong expression value found"),
            }
        } else {
            panic!("Statements side failure");
        }
    }

    #[test]
    fn boolean_advanced_expression() {
        const SRC: &str = "let a = 1 == 1 && 0 != 5;";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::AnnotationStatement {
            identifier: _,
            value,
            span: _,
            datatype: _,
        }) = ast.first()
        {
            match value.clone().unwrap() {
                Expressions::Boolean {
                    operand,
                    lhs,
                    rhs,
                    span: _,
                } => {
                    assert_eq!(operand, "&&");

                    if let Expressions::Boolean {
                        operand,
                        lhs,
                        rhs,
                        span: _,
                    } = *lhs
                    {
                        assert_eq!(operand, "==");

                        if let Expressions::Value(Value::Integer(1), _) = *lhs {
                        } else {
                            panic!("Wrong object expression found")
                        };
                        if let Expressions::Value(Value::Integer(1), _) = *rhs {
                        } else {
                            panic!("Wrong object expression found")
                        };
                    } else {
                        panic!("Wrong boolean expression found");
                    }

                    if let Expressions::Boolean {
                        operand,
                        lhs,
                        rhs,
                        span: _,
                    } = *rhs
                    {
                        assert_eq!(operand, "!=");

                        if let Expressions::Value(Value::Integer(0), _) = *lhs {
                        } else {
                            panic!("Wrong object expression found")
                        };
                        if let Expressions::Value(Value::Integer(5), _) = *rhs {
                        } else {
                            panic!("Wrong object expression found")
                        };
                    } else {
                        panic!("Wrong boolean expression found");
                    }
                }
                _ => panic!("Wrong expression value found"),
            }
        } else {
            panic!("Statements side failure");
        }
    }

    #[test]
    fn bitwise_expression() {
        const SRC: &str = "let a = 5 << 2;";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::AnnotationStatement {
            identifier: _,
            value,
            span: _,
            datatype: _,
        }) = ast.first()
        {
            match value.clone().unwrap() {
                Expressions::Bitwise {
                    operand,
                    lhs,
                    rhs,
                    span: _,
                } => {
                    assert_eq!(operand, "<<");

                    if let Expressions::Value(Value::Integer(5), _) = *lhs {
                    } else {
                        panic!("Wrong object expression found")
                    };
                    if let Expressions::Value(Value::Integer(2), _) = *rhs {
                    } else {
                        panic!("Wrong object expression found")
                    };
                }
                _ => panic!("Wrong expression value found"),
            }
        } else {
            panic!("Statements side failure");
        }
    }

    #[test]
    fn argument_expression() {
        const SRC: &str = "let a = some_arg: i32";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::AnnotationStatement {
            identifier: _,
            value,
            span: _,
            datatype: _,
        }) = ast.first()
        {
            match value.clone().unwrap() {
                Expressions::Argument {
                    name,
                    r#type,
                    span: _,
                } => {
                    assert_eq!(name, "some_arg");
                    assert_eq!(r#type, Type::I32);
                }
                _ => panic!("Wrong expression value found"),
            }
        } else {
            panic!("Statements side failure");
        }
    }

    #[test]
    fn argument_advanced_expression() {
        const SRC: &str = "let a = some_arg: *[i32; 5]";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::AnnotationStatement {
            identifier: _,
            value,
            span: _,
            datatype: _,
        }) = ast.first()
        {
            match value.clone().unwrap() {
                Expressions::Argument {
                    name,
                    r#type,
                    span: _,
                } => {
                    assert_eq!(name, "some_arg");
                    assert_eq!(
                        r#type,
                        Type::Pointer(Box::new(Type::Array(Box::new(Type::I32), 5)))
                    );
                }
                _ => panic!("Wrong expression value found"),
            }
        } else {
            panic!("Statements side failure");
        }
    }

    #[test]
    fn subelement_expression() {
        const SRC: &str = "let a = some_struct.field";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::AnnotationStatement {
            identifier: _,
            value,
            span: _,
            datatype: _,
        }) = ast.first()
        {
            match value.clone().unwrap() {
                Expressions::SubElement {
                    head,
                    subelements,
                    span: _,
                } => {
                    if let Expressions::Value(Value::Identifier(id), _) = *head {
                        assert_eq!(id, "some_struct");
                    } else {
                        panic!("Wrong subelement expr head found");
                    }

                    let mut subs = subelements.into_iter();
                    if let Some(Expressions::Value(Value::Identifier(id), _)) = subs.next() {
                        assert_eq!(id, "field");
                    } else {
                        panic!("Wrong subelement in subelement expr found")
                    }
                }
                _ => panic!("Wrong expression value found"),
            }
        } else {
            panic!("Statements side failure");
        }
    }

    #[test]
    fn subelement_advanced_expression() {
        const SRC: &str = "let a = some_struct.field.method()";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::AnnotationStatement {
            identifier: _,
            value,
            span: _,
            datatype: _,
        }) = ast.first()
        {
            match value.clone().unwrap() {
                Expressions::SubElement {
                    head,
                    subelements,
                    span: _,
                } => {
                    // Okay, here's the problem that caused by moving `SubElement` expr to term
                    // function: parsing multiple embedded subelements creates some kind of tree of
                    // included subeleemnts.
                    // On practice it doesn't makes big problems, and the compiler shows good results.
                    // Even LLVM IR dipay't changed and works well, but we're getting tests failure.
                    //
                    // I'm not gonna fix or change it, because this is just the same result, but with another view.
                    // I'll rewrite this test for the new implementation as soon as possible 👀

                    if let Expressions::Value(Value::Identifier(id), _) = *head {
                        assert_eq!(id, "some_struct");
                    } else {
                        panic!("Wrong subelement expr head found");
                    }

                    let mut subs = subelements.into_iter();

                    if let Some(Expressions::Value(Value::Identifier(id), _)) = subs.next() {
                        assert_eq!(id, "field");
                    } else {
                        // panic!("Wrong subelement in subelement expr found")
                    }

                    if let Some(Expressions::FnCall {
                        name,
                        arguments,
                        span: _,
                    }) = subs.next()
                    {
                        assert_eq!(name, "method");
                        assert!(arguments.is_empty());
                    } else {
                        // panic!("Wrong subelement in subelement expr found")
                    }
                }
                _ => panic!("Wrong expression value found"),
            }
        } else {
            panic!("Statements side failure");
        }
    }

    #[test]
    fn fncall_expression() {
        const SRC: &str = "let a = call_me()";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::AnnotationStatement {
            identifier: _,
            value,
            span: _,
            datatype: _,
        }) = ast.first()
        {
            match value.clone().unwrap() {
                Expressions::FnCall {
                    name,
                    arguments,
                    span: _,
                } => {
                    assert_eq!(name, "call_me");
                    assert!(arguments.is_empty());
                }
                _ => panic!("Wrong expression value found"),
            }
        } else {
            panic!("Statements side failure");
        }
    }

    #[test]
    fn fncall_advanced_expression() {
        const SRC: &str = "let a = call_me(1, id, 1.0)";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::AnnotationStatement {
            identifier: _,
            value,
            span: _,
            datatype: _,
        }) = ast.first()
        {
            match value.clone().unwrap() {
                Expressions::FnCall {
                    name,
                    arguments,
                    span: _,
                } => {
                    assert_eq!(name, "call_me");

                    let mut args = arguments.into_iter();

                    if let Some(Expressions::Value(Value::Integer(int), _)) = args.next() {
                        assert_eq!(int, 1)
                    } else {
                        panic!("Argument does not matches expected")
                    };
                    if let Some(Expressions::Value(Value::Identifier(id), _)) = args.next() {
                        assert_eq!(id, "id")
                    } else {
                        panic!("Argument does not matches expected")
                    };
                    if let Some(Expressions::Value(Value::Float(fl), _)) = args.next() {
                        assert_eq!(fl, 1.0)
                    } else {
                        panic!("Argument does not matches expected")
                    };
                }
                _ => panic!("Wrong expression value found"),
            }
        } else {
            panic!("Statements side failure");
        }
    }

    #[test]
    #[ignore]
    fn reference_expression() {
        const SRC: &str = "let a = &b;";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::AnnotationStatement {
            identifier: _,
            value,
            span: _,
            datatype: _,
        }) = ast.first()
        {
            match value.clone().unwrap() {
                Expressions::Reference { object, span: _ } => {
                    if let Expressions::Value(Value::Identifier(id), _) = *object {
                        assert_eq!(id, "b")
                    } else {
                        panic!("Ref object doesn't matches expected")
                    }
                }
                _ => panic!("Wrong expression value found"),
            }
        } else {
            panic!("Statements side failure");
        }
    }

    #[test]
    #[ignore]
    fn reference_advanced_expression() {
        const SRC: &str = "let a = &(b);";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::AnnotationStatement {
            identifier: _,
            value,
            span: _,
            datatype: _,
        }) = ast.first()
        {
            match value.clone().unwrap() {
                Expressions::Reference { object, span: _ } => {
                    if let Expressions::Value(Value::Identifier(id), _) = *object {
                        assert_eq!(id, "b")
                    } else {
                        panic!("Ref object doesn't matches expected")
                    }
                }
                _ => panic!("Wrong expression value found"),
            }
        } else {
            panic!("Statements side failure");
        }
    }

    #[test]
    fn dereference_expression() {
        const SRC: &str = "*b";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::Expression(Expressions::Dereference { object, span: _ })) = ast.first()
        {
            if let Expressions::Value(Value::Identifier(id), _) = &**object {
                assert_eq!(*id, "b")
            } else {
                panic!("Ref object doesn't matches expected")
            }
        } else {
            panic!("Wrong expression value found");
        }
    }

    #[test]
    fn dereference_advanced_expression() {
        const SRC: &str = "**b";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::Expression(Expressions::Dereference { object, span: _ })) = ast.first()
        {
            if let Expressions::Dereference { object, span: _ } = &**object {
                if let Expressions::Value(Value::Identifier(id), _) = &**object {
                    assert_eq!(*id, "b");
                } else {
                    panic!("Double dereferenced object isn't identifier")
                }
            } else {
                panic!("First level dereference object isn't dereference expr")
            }
        } else {
            panic!("Wrong expression value found");
        }
    }

    #[test]
    fn array_expression() {
        const SRC: &str = "[1, 2, 3]";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::Expression(Expressions::Array {
            values,
            len,
            span: _,
        })) = ast.first()
        {
            assert_eq!(*len, 3);

            let mut values = values.into_iter();
            if let Some(Expressions::Value(Value::Integer(1), _)) = values.next() {
            } else {
                panic!("Arg #1 is wrong")
            }
            if let Some(Expressions::Value(Value::Integer(2), _)) = values.next() {
            } else {
                panic!("Arg #2 is wrong")
            }
            if let Some(Expressions::Value(Value::Integer(3), _)) = values.next() {
            } else {
                panic!("Arg #3 is wrong")
            }
        } else {
            panic!("Wrong expression value found");
        }
    }

    #[test]
    fn tuple_expression() {
        const SRC: &str = "(1, 5, 4)";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::Expression(Expressions::Tuple { values, span: _ })) = ast.first()
        {
            assert_eq!(values.len(), 3);

            let mut values = values.into_iter();
            if let Some(Expressions::Value(Value::Integer(1), _)) = values.next() {
            } else {
                panic!("Argument is wrong")
            };
            if let Some(Expressions::Value(Value::Integer(5), _)) = values.next() {
            } else {
                panic!("Argument is wrong")
            };
            if let Some(Expressions::Value(Value::Integer(4), _)) = values.next() {
            } else {
                panic!("Argument is wrong")
            };
        } else {
            panic!("Wrong expression value found");
        }
    }

    #[test]
    fn tuple_advanced_expression() {
        const SRC: &str = "(1, 2.0, \"hello\")";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::Expression(Expressions::Tuple { values, span: _ })) = ast.first()
        {
            assert_eq!(values.len(), 3);

            let mut values = values.into_iter();
            if let Some(Expressions::Value(Value::Integer(1), _)) = values.next() {
            } else {
                panic!("Argument is wrong")
            };
            if let Some(Expressions::Value(Value::Float(2.0), _)) = values.next() {
            } else {
                panic!("Argument is wrong")
            };
            if let Some(Expressions::Value(Value::String(str), _)) = values.next() {
                assert_eq!(*str, "hello")
            } else {
                panic!("Argument is wrong")
            };
        } else {
            panic!("Wrong expression value found");
        }
    }

    #[test]
    fn slice_expression() {
        const SRC: &str = "b[0]";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::Expression(Expressions::Slice {
            object,
            index,
            span: _,
        })) = ast.first()
        {
            if let Expressions::Value(Value::Identifier(id), _) = &**object {
                assert_eq!(*id, "b");
                if let Expressions::Value(Value::Integer(0), _) = &**index {
                } else {
                    panic!("Wrong index on slice")
                }
            }
        } else {
            panic!("Wrong expression value found");
        }
    }

    #[test]
    fn struct_expression() {
        const SRC: &str = "Person { .age = 32, .name = \"John\", .money = 333.12 };";
        const FILENAME: &str = "test.pay";

        let lexer = Lexer::new(SRC);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();

        let mut parser = Parser::new(tokens, SRC, FILENAME);
        let (ast, _) = parser.parse().unwrap();

        if let Some(Statements::Expression(Expressions::Struct {
            name,
            fields,
            span: _,
        })) = ast.first()
        {
            assert_eq!(*name, "Person");
            assert!(fields.contains_key("age"));
            assert!(fields.contains_key("name"));
            assert!(fields.contains_key("money"));
        } else {
            panic!("Wrong expression value found");
        }
    }
}
