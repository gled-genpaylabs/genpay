use crate::function::Function;
use genpay_parser::types::Type;
use inkwell::types::BasicTypeEnum;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Structure<'ctx> {
    pub fields: HashMap<&'ctx str, Field<'ctx>>,
    pub functions: HashMap<&'ctx str, Function<'ctx>>,
    pub llvm_type: BasicTypeEnum<'ctx>,
}

#[derive(Debug, Clone)]
pub struct Field<'ctx> {
    pub name: &'ctx str,
    pub nth: u32,
    pub datatype: Type<'ctx>,
    pub llvm_type: BasicTypeEnum<'ctx>,
}
