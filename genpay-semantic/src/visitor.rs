use crate::Analyzer;
use genpay_parser::{expressions::Expressions, statements::Statements, types::Type};
use std::path::PathBuf;

impl Analyzer {
    pub fn visit_statement(&mut self, statement: &Statements) {
        match statement {
            _ => todo!(),
        }
    }

    pub fn visit_expression(&mut self, expression: &Expressions, expected_type: Option<Type>) -> Type {
        match expression {
            _ => todo!(),
        }
    }

    // Stubbed helper methods
    pub fn unwrap_alias(&self, a_type: &Type) -> Result<Type, String> {
        todo!()
    }

    pub fn is_integer(a_type: &Type) -> bool {
        todo!()
    }

    pub fn is_float(a_type: &Type) -> bool {
        todo!()
    }

    pub fn integer_order(a_type: &Type) -> u8 {
        todo!()
    }

    pub fn float_order(a_type: &Type) -> u8 {
        todo!()
    }

    pub fn verify_macrocall(&mut self, name: &str, arguments: &[Expressions], span: &(usize, usize)) -> Type {
        todo!()
    }

    pub fn expand_library_path(&self, path: &str, is_module: bool) -> Result<PathBuf, String> {
        todo!()
    }

    pub fn verify_cast(&self, from_type: &Type, target_type: &Type) -> Result<(), String> {
        todo!()
    }
}
