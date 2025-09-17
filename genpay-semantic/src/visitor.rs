use crate::{Analyzer, Scope, SemanticError};
use genpay_parser::{expressions::Expressions, statements::Statements, types::Type};
use std::collections::BTreeMap;
use std::path::PathBuf;

impl Analyzer {
    pub fn visit_statement(&mut self, statement: &Statements) {
        match statement {
            Statements::AnnotationStatement {
                identifier,
                datatype,
                value,
                span,
            } => {
                if self.scope.variables.contains_key(identifier) {
                    self.error(SemanticError::RedefinitionError {
                        exception: format!("Variable `{}` is already defined in this scope.", identifier),
                        help: Some("Consider using a different name.".to_string()),
                        src: self.source.clone(),
                        span: (*span).into(),
                    });
                    return;
                }

                let (var_type, initialized) = if let Some(expr) = value {
                    let expr_type = self.visit_expression(expr, datatype.clone());
                    if let Some(dt) = datatype {
                        if *dt != expr_type {
                            self.error(SemanticError::TypesMismatch {
                                exception: format!("Expected type `{}`, but found type `{}`.", dt, expr_type),
                                help: None,
                                src: self.source.clone(),
                                span: expr.get_span().into(),
                            });
                        }
                    }
                    (expr_type, true)
                } else {
                    (datatype.clone().unwrap_or(Type::Void), false)
                };

                if var_type == Type::Void {
                    self.error(SemanticError::SemanticalError {
                        exception: "Variable declaration must have a type or an initial value.".to_string(),
                        help: Some(format!("Consider adding a type annotation, like `let {}: <type>;` or an initial value `let {} = <value>;`", identifier, identifier)),
                        src: self.source.clone(),
                        span: (*span).into(),
                    });
                    return;
                }

                self.scope
                    .add_var(identifier.clone(), var_type, initialized, *span);
            }
            Statements::Expression(expression) => {
                self.visit_expression(expression, None);
            }
            Statements::FunctionDefineStatement {
                name,
                datatype,
                arguments,
                block,
                public,
                is_var_args,
                span: _,
                header_span: _,
            } => {
                let func_type = Type::Function(
                    arguments.iter().map(|(_, t)| t.clone()).collect(),
                    Box::new(datatype.clone()),
                    *is_var_args,
                );

                if let Err(err) = self.scope.add_fn(name.clone(), func_type.clone(), *public) {
                    self.error(SemanticError::RedefinitionError {
                        exception: err,
                        help: None,
                        src: self.source.clone(),
                        span: (0, 0).into(),
                    });
                    return;
                }

                let mut func_scope = Scope::new();
                func_scope.parent = Some(Box::new(self.scope.clone()));
                func_scope.expected = datatype.clone();

                for (arg_name, arg_type) in arguments {
                    func_scope.add_var(arg_name.clone(), arg_type.clone(), true, (0, 0));
                }

                let original_scope = self.scope.clone();
                self.scope = func_scope;

                for stmt in block {
                    self.visit_statement(stmt);
                }

                self.scope = original_scope;
            }
            Statements::ReturnStatement { value, span } => {
                let ret_type = self.visit_expression(value, Some(self.scope.expected.clone()));

                // Allow i64 literals to be cast to i32 in return statements.
                let is_i64_to_i32_cast =
                    ret_type == Type::I64 && self.scope.expected == Type::I32;

                if ret_type != self.scope.expected && !is_i64_to_i32_cast {
                    self.error(SemanticError::TypesMismatch {
                        exception: format!(
                            "Expected return type `{}`, but found type `{}`.",
                            self.scope.expected, ret_type
                        ),
                        help: None,
                        src: self.source.clone(),
                        span: (*span).into(),
                    });
                }
            }
            Statements::IfStatement {
                condition,
                then_block,
                else_block,
                span: _,
            } => {
                let cond_type = self.visit_expression(condition, Some(Type::Bool));
                if cond_type != Type::Bool {
                    self.error(SemanticError::TypesMismatch {
                        exception: format!(
                            "Expected a boolean condition, but found type `{}`.",
                            cond_type
                        ),
                        help: None,
                        src: self.source.clone(),
                        span: condition.get_span().into(),
                    });
                }

                let mut then_scope = Scope::new();
                then_scope.parent = Some(Box::new(self.scope.clone()));
                let original_scope = self.scope.clone();
                self.scope = then_scope;
                for stmt in then_block {
                    self.visit_statement(stmt);
                }
                self.scope = original_scope;

                if let Some(else_block) = else_block {
                    let mut else_scope = Scope::new();
                    else_scope.parent = Some(Box::new(self.scope.clone()));
                    let original_scope = self.scope.clone();
                    self.scope = else_scope;
                    for stmt in else_block {
                        self.visit_statement(stmt);
                    }
                    self.scope = original_scope;
                }
            }
            Statements::WhileStatement {
                condition,
                block,
                span: _,
            } => {
                let cond_type = self.visit_expression(condition, Some(Type::Bool));
                if cond_type != Type::Bool {
                    self.error(SemanticError::TypesMismatch {
                        exception: format!(
                            "Expected a boolean condition, but found type `{}`.",
                            cond_type
                        ),
                        help: None,
                        src: self.source.clone(),
                        span: condition.get_span().into(),
                    });
                }

                let mut while_scope = Scope::new();
                while_scope.parent = Some(Box::new(self.scope.clone()));
                while_scope.is_loop = true;
                let original_scope = self.scope.clone();
                self.scope = while_scope;
                for stmt in block {
                    self.visit_statement(stmt);
                }
                self.scope = original_scope;
            }
            Statements::ForStatement { .. } => {
                // TODO: Implement for loops
            }
            Statements::StructDefineStatement {
                name,
                fields,
                functions: _,
                public,
                span,
            } => {
                let struct_type = Type::Struct(fields.clone(), BTreeMap::new());

                if let Err(err) = self.scope.add_struct(name.clone(), struct_type, *public) {
                    self.error(SemanticError::RedefinitionError {
                        exception: err,
                        help: None,
                        src: self.source.clone(),
                        span: (*span).into(),
                    });
                }
            }
            Statements::EnumDefineStatement {
                name,
                fields,
                functions: _,
                public,
                span,
            } => {
                let enum_type = Type::Enum(fields.clone(), BTreeMap::new());

                if let Err(err) = self.scope.add_enum(name.clone(), enum_type, *public) {
                    self.error(SemanticError::RedefinitionError {
                        exception: err,
                        help: None,
                        src: self.source.clone(),
                        span: (*span).into(),
                    });
                }
            }
            Statements::AssignStatement {
                object,
                value,
                span,
            } => {
                let left_type = self.visit_expression(object, None);
                let right_type = self.visit_expression(value, Some(left_type.clone()));

                if left_type != right_type {
                    self.error(SemanticError::TypesMismatch {
                        exception: format!(
                            "Expected type `{}`, but found type `{}`.",
                            left_type, right_type
                        ),
                        help: None,
                        src: self.source.clone(),
                        span: (*span).into(),
                    });
                }

                // TODO: check for mutability
            }
            Statements::ScopeStatement { block, .. } => {
                let mut scope = Scope::new();
                scope.parent = Some(Box::new(self.scope.clone()));
                let original_scope = self.scope.clone();
                self.scope = scope;
                for stmt in block {
                    self.visit_statement(stmt);
                }
                self.scope = original_scope;
            }
            Statements::BreakStatements { span } => {
                if !self.scope.is_loop() {
                    self.error(SemanticError::UnsupportedExpression {
                        exception: "Cannot use `break` outside of a loop.".to_string(),
                        help: None,
                        src: self.source.clone(),
                        span: (*span).into(),
                    });
                }
            }
            Statements::TypedefStatement {
                alias,
                datatype,
                span,
            } => {
                if let Err(err) = self.scope.add_typedef(alias.clone(), datatype.clone()) {
                    self.error(SemanticError::RedefinitionError {
                        exception: err,
                        help: None,
                        src: self.source.clone(),
                        span: (*span).into(),
                    });
                }
            }
            Statements::MacroCallStatement {
                name,
                arguments,
                span,
            } => {
                self.verify_macrocall(name, arguments, span);
            }
            _ => {}
        }
    }

    pub fn visit_expression(&mut self, expression: &Expressions, _expected_type: Option<Type>) -> Type {
        match expression {
            Expressions::Value(value, span) => match value {
                genpay_parser::value::Value::Integer(_) => Type::I64,
                genpay_parser::value::Value::Float(_) => Type::F64,
                genpay_parser::value::Value::String(_) => Type::String,
                genpay_parser::value::Value::Char(_) => Type::Char,
                genpay_parser::value::Value::Boolean(_) => Type::Bool,
                genpay_parser::value::Value::Identifier(name) => {
                    if let Some(var) = self.scope.get_var(name) {
                        var.datatype
                    } else {
                        self.error(SemanticError::UnresolvedName {
                            exception: format!("Variable `{}` not found in this scope.", name),
                            help: None,
                            src: self.source.clone(),
                            span: (*span).into(),
                        });
                        Type::Void
                    }
                }
                genpay_parser::value::Value::Null => Type::Null,
                genpay_parser::value::Value::Void => Type::Void,
                _ => todo!(),
            },
            Expressions::Unary {
                operand,
                object,
                span,
            } => {
                let obj_type = self.visit_expression(object, None);
                match operand.as_str() {
                    "-" => {
                        if !Analyzer::is_integer(&obj_type) && !Analyzer::is_float(&obj_type) {
                            self.error(SemanticError::TypesMismatch {
                                exception: format!(
                                    "Cannot apply unary operator `-` to type `{}`.",
                                    obj_type
                                ),
                                help: Some("The `-` operator can only be applied to integers and floats.".to_string()),
                                src: self.source.clone(),
                                span: (*span).into(),
                            });
                        }
                        obj_type
                    }
                    "!" => {
                        if obj_type != Type::Bool {
                            self.error(SemanticError::TypesMismatch {
                                exception: format!(
                                    "Cannot apply unary operator `!` to type `{}`.",
                                    obj_type
                                ),
                                help: Some("The `!` operator can only be applied to booleans.".to_string()),
                                src: self.source.clone(),
                                span: (*span).into(),
                            });
                        }
                        Type::Bool
                    }
                    _ => {
                        self.error(SemanticError::UnsupportedExpression {
                            exception: format!("Unsupported unary operator `{}`.", operand),
                            help: None,
                            src: self.source.clone(),
                            span: (*span).into(),
                        });
                        Type::Void
                    }
                }
            }
            Expressions::Binary {
                operand,
                lhs,
                rhs,
                span,
            } => {
                let left_type = self.visit_expression(lhs, None);
                let right_type = self.visit_expression(rhs, None);

                match operand.as_str() {
                    "+" | "-" | "*" | "/" | "%" => {
                        if Analyzer::is_integer(&left_type) && Analyzer::is_integer(&right_type) {
                            if Analyzer::integer_order(&left_type) > Analyzer::integer_order(&right_type) {
                                left_type
                            } else {
                                right_type
                            }
                        } else if Analyzer::is_float(&left_type) && Analyzer::is_float(&right_type) {
                            if Analyzer::float_order(&left_type) > Analyzer::float_order(&right_type) {
                                left_type
                            } else {
                                right_type
                            }
                        } else {
                            self.error(SemanticError::TypesMismatch {
                                exception: format!(
                                    "Cannot apply binary operator `{}` to types `{}` and `{}`.",
                                    operand, left_type, right_type
                                ),
                                help: Some("This operator can only be applied to numbers of the same type.".to_string()),
                                src: self.source.clone(),
                                span: (*span).into(),
                            });
                            Type::Void
                        }
                    }
                    "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                        if left_type != right_type {
                            self.error(SemanticError::TypesMismatch {
                                exception: format!(
                                    "Cannot apply binary operator `{}` to types `{}` and `{}`.",
                                    operand, left_type, right_type
                                ),
                                help: Some("Comparison operators can only be applied to values of the same type.".to_string()),
                                src: self.source.clone(),
                                span: (*span).into(),
                            });
                        }
                        Type::Bool
                    }
                    _ => {
                        self.error(SemanticError::UnsupportedExpression {
                            exception: format!("Unsupported binary operator `{}`.", operand),
                            help: None,
                            src: self.source.clone(),
                            span: (*span).into(),
                        });
                        Type::Void
                    }
                }
            }
            Expressions::FnCall {
                name,
                arguments,
                span,
            } => {
                if let Some(func_type) = self.scope.get_fn(name) {
                    if let Type::Function(args, ret_type, _) = func_type {
                        if arguments.len() != args.len() {
                            self.error(SemanticError::ArgumentException {
                                exception: format!(
                                    "Function `{}` expects {} arguments, but {} were provided.",
                                    name,
                                    args.len(),
                                    arguments.len()
                                ),
                                help: None,
                                src: self.source.clone(),
                                span: (*span).into(),
                            });
                        }

                        for (i, arg_expr) in arguments.iter().enumerate() {
                            let arg_type = self.visit_expression(arg_expr, Some(args[i].clone()));
                            if arg_type != args[i] {
                                self.error(SemanticError::TypesMismatch {
                                    exception: format!(
                                        "Expected argument of type `{}`, but found type `{}`.",
                                        args[i], arg_type
                                    ),
                                    help: None,
                                    src: self.source.clone(),
                                    span: arg_expr.get_span().into(),
                                });
                            }
                        }

                        *ret_type.clone()
                    } else {
                        self.error(SemanticError::SemanticalError {
                            exception: format!("`{}` is not a function.", name),
                            help: None,
                            src: self.source.clone(),
                            span: (*span).into(),
                        });
                        Type::Void
                    }
                } else {
                    self.error(SemanticError::UnresolvedName {
                        exception: format!("Function `{}` not found.", name),
                        help: None,
                        src: self.source.clone(),
                        span: (*span).into(),
                    });
                    Type::Void
                }
            }
            Expressions::Struct {
                name,
                fields,
                span,
            } => {
                if let Some(struct_type) = self.scope.get_struct(name) {
                    if let Type::Struct(defined_fields, _) = struct_type.clone()
                    {
                        if fields.len() != defined_fields.len() {
                            self.error(SemanticError::MissingFields {
                                exception: format!(
                                    "Struct `{}` expects {} fields, but {} were provided.",
                                    name,
                                    defined_fields.len(),
                                    fields.len()
                                ),
                                help: None,
                                src: self.source.clone(),
                                span: (*span).into(),
                            });
                        }

                        for (field_name, field_expr) in fields {
                            if let Some(defined_field_type) = defined_fields.get(field_name) {
                                let field_type = self.visit_expression(
                                    field_expr,
                                    Some(defined_field_type.clone()),
                                );
                                if field_type != *defined_field_type {
                                    self.error(SemanticError::TypesMismatch {
                                        exception: format!(
                                            "Expected field `{}` to be of type `{}`, but found type `{}`.",
                                            field_name, defined_field_type, field_type
                                        ),
                                        help: None,
                                        src: self.source.clone(),
                                        span: field_expr.get_span().into(),
                                    });
                                }
                            } else {
                                self.error(SemanticError::UnresolvedName {
                                    exception: format!(
                                        "Field `{}` not found on struct `{}`.",
                                        field_name, name
                                    ),
                                    help: None,
                                    src: self.source.clone(),
                                    span: field_expr.get_span().into(),
                                });
                            }
                        }
                        struct_type
                    } else {
                        unreachable!()
                    }
                } else {
                    self.error(SemanticError::UnresolvedName {
                        exception: format!("Struct `{}` not found.", name),
                        help: None,
                        src: self.source.clone(),
                        span: (*span).into(),
                    });
                    Type::Void
                }
            }
            Expressions::SubElement {
                head,
                subelements,
                span,
            } => {
                let mut current_type = self.visit_expression(head, None);
                for sub in subelements {
                    if let Expressions::Value(genpay_parser::value::Value::Identifier(name), _) = sub {
                        if let Type::Struct(fields, _) = current_type.clone() {
                            if let Some(field_type) = fields.get(name) {
                                current_type = field_type.clone();
                            } else {
                                self.error(SemanticError::UnresolvedName {
                                    exception: format!(
                                        "Field `{}` not found on type `{}`.",
                                        name, current_type
                                    ),
                                    help: None,
                                    src: self.source.clone(),
                                    span: sub.get_span().into(),
                                });
                                return Type::Void;
                            }
                        } else {
                            self.error(SemanticError::UnsupportedExpression {
                                exception: format!(
                                    "Cannot access field on non-struct type `{}`.",
                                    current_type
                                ),
                                help: None,
                                src: self.source.clone(),
                                span: (*span).into(),
                            });
                            return Type::Void;
                        }
                    } else {
                        // TODO: handle method calls
                    }
                }
                current_type
            }
            Expressions::Array { values, .. } => {
                if values.is_empty() {
                    return Type::Array(Box::new(Type::Void), 0);
                }

                let first_type = self.visit_expression(&values[0], None);
                for value in values.iter().skip(1) {
                    let current_type = self.visit_expression(value, Some(first_type.clone()));
                    if current_type != first_type {
                        self.error(SemanticError::TypesMismatch {
                            exception: format!(
                                "Array elements must have the same type. Expected `{}` but found `{}`.",
                                first_type, current_type
                            ),
                            help: None,
                            src: self.source.clone(),
                            span: value.get_span().into(),
                        });
                    }
                }
                Type::Array(Box::new(first_type), values.len())
            }
            Expressions::Tuple { values, .. } => {
                let types = values
                    .iter()
                    .map(|v| self.visit_expression(v, None))
                    .collect();
                Type::Tuple(types)
            }
            Expressions::Slice {
                object,
                index,
                span,
            } => {
                let obj_type = self.visit_expression(object, None);
                let index_type = self.visit_expression(index, Some(Type::I64));

                if !Analyzer::is_integer(&index_type) {
                    self.error(SemanticError::TypesMismatch {
                        exception: format!(
                            "Array index must be an integer, but found `{}`.",
                            index_type
                        ),
                        help: None,
                        src: self.source.clone(),
                        span: index.get_span().into(),
                    });
                }

                if let Type::Array(elem_type, _) = obj_type {
                    *elem_type
                } else {
                    self.error(SemanticError::TypesMismatch {
                        exception: format!("Cannot index non-array type `{}`.", obj_type),
                        help: None,
                        src: self.source.clone(),
                        span: (*span).into(),
                    });
                    Type::Void
                }
            }
            Expressions::Reference { object, .. } => {
                let obj_type = self.visit_expression(object, None);
                Type::Pointer(Box::new(obj_type))
            }
            Expressions::Dereference { object, span } => {
                let obj_type = self.visit_expression(object, None);
                if let Type::Pointer(elem_type) = obj_type {
                    *elem_type
                } else {
                    self.error(SemanticError::TypesMismatch {
                        exception: format!("Cannot dereference non-pointer type `{}`.", obj_type),
                        help: None,
                        src: self.source.clone(),
                        span: (*span).into(),
                    });
                    Type::Void
                }
            }
            _ => todo!(),
        }
    }

    // Stubbed helper methods
    pub fn unwrap_alias(&self, a_type: &Type) -> Result<Type, String> {
        if let Type::Alias(alias) = a_type {
            if let Some(typ) = self.scope.get_typedef(alias) {
                self.unwrap_alias(&typ)
            } else {
                Err(format!("Type alias `{}` not found.", alias))
            }
        } else {
            Ok(a_type.clone())
        }
    }

    pub fn is_integer(a_type: &Type) -> bool {
        matches!(
            a_type,
            Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::U8 | Type::U16 | Type::U32 | Type::U64
        )
    }

    pub fn is_float(a_type: &Type) -> bool {
        matches!(a_type, Type::F32 | Type::F64)
    }

    pub fn integer_order(a_type: &Type) -> u8 {
        match a_type {
            Type::I8 => 1,
            Type::U8 => 2,
            Type::I16 => 3,
            Type::U16 => 4,
            Type::I32 => 5,
            Type::U32 => 6,
            Type::I64 => 7,
            Type::U64 => 8,
            _ => 0,
        }
    }

    pub fn float_order(a_type: &Type) -> u8 {
        match a_type {
            Type::F32 => 1,
            Type::F64 => 2,
            _ => 0,
        }
    }

    pub fn is_unsigned_integer(a_type: &Type) -> bool {
        matches!(
            a_type,
            Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::USIZE
        )
    }

    pub fn unsigned_to_signed_integer(a_type: &Type) -> Type {
        match a_type {
            Type::U8 => Type::I8,
            Type::U16 => Type::I16,
            Type::U32 => Type::I32,
            Type::U64 => Type::I64,
            Type::USIZE => Type::I64,
            _ => a_type.clone(),
        }
    }

    pub fn verify_macrocall(
        &mut self,
        name: &str,
        arguments: &[Expressions],
        span: &(usize, usize),
    ) -> Type {
        match name {
            "println" => {
                for arg in arguments {
                    self.visit_expression(arg, None);
                }
                Type::Void
            }
            _ => {
                self.error(SemanticError::UnresolvedName {
                    exception: format!("Macro `{}` not found.", name),
                    help: None,
                    src: self.source.clone(),
                    span: (*span).into(),
                });
                Type::Void
            }
        }
    }

    pub fn expand_library_path(&self, path: &str, is_module: bool) -> Result<PathBuf, String> {
        todo!()
    }

    pub fn verify_cast(&self, from_type: &Type, target_type: &Type) -> Result<(), String> {
        todo!()
    }
}
