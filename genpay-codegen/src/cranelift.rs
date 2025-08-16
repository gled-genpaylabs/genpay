#![cfg(feature = "cranelift")]

use crate::CodeGenBackend;
use genpay_parser::{expressions::Expressions, statements::Statements, types::Type, value::Value};

pub struct CraneliftBackend {}

impl CraneliftBackend {
    pub fn new() -> Self {
        Self {}
    }
}

impl<'ctx> CodeGenBackend<'ctx> for CraneliftBackend {
    type BackendValue = (); // Placeholder

    fn compile_statement(&mut self, _statement: Statements<'ctx>, _prefix: Option<&'ctx str>) {
        unimplemented!("Cranelift backend is not yet implemented");
    }

    fn compile_expression(
        &mut self,
        _expression: Expressions<'ctx>,
        _expected: Option<Type<'ctx>>,
    ) -> (Type<'ctx>, Self::BackendValue) {
        unimplemented!("Cranelift backend is not yet implemented");
    }

    fn compile_value(
        &mut self,
        _value: Value<'ctx>,
        _expected: Option<Type<'ctx>>,
    ) -> (Type<'ctx>, Self::BackendValue) {
        unimplemented!("Cranelift backend is not yet implemented");
    }
}
