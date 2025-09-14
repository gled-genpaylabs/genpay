use genpay_parse_two::types::Type;

#[derive(Debug, Clone)]
pub struct Function<'ctx> {
    pub datatype: Type,
    pub value: inkwell::values::FunctionValue<'ctx>,
    pub arguments: Vec<Type>,
    pub called: bool,
}
