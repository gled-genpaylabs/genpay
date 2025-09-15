use genpay_parser::types::Type;
use bumpalo::collections::Vec;

#[derive(Debug, Clone)]
pub struct Function<'ctx, 'bump> {
    pub datatype: Type<'bump>,
    pub value: inkwell::values::FunctionValue<'ctx>,
    pub arguments: Vec<'bump, Type<'bump>>,
    pub called: bool,
}
