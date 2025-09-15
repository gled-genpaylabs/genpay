use crate::function::Function;
use genpay_parser::types::Type;
use inkwell::types::BasicTypeEnum;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Structure<'ctx, 'bump> {
    pub fields: HashMap<&'bump str, Field<'ctx, 'bump>>,
    pub functions: HashMap<&'bump str, Function<'ctx, 'bump>>,
    pub llvm_type: BasicTypeEnum<'ctx>,
}

#[derive(Debug, Clone)]
pub struct Field<'ctx, 'bump> {
    pub name: &'bump str,
    pub nth: u32,
    pub datatype: Type<'bump>,
    pub llvm_type: BasicTypeEnum<'ctx>,
}
