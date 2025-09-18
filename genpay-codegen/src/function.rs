use genpay_parser::types::Type;

#[derive(Debug, Clone)]
pub struct Function<'ctx> {
    pub datatype: Type<'ctx>,
    pub value: inkwell::values::FunctionValue<'ctx>,
    pub arguments: Vec<Type<'ctx>>,
    pub called: bool,
}
