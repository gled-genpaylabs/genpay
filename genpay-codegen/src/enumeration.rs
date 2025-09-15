use crate::function::Function;
use inkwell::types::BasicTypeEnum;
use std::collections::HashMap;
use bumpalo::collections::Vec;

#[derive(Debug, Clone)]
pub struct Enumeration<'ctx, 'bump> {
    pub fields: Vec<'bump, &'bump str>,
    pub functions: HashMap<&'bump str, Function<'ctx, 'bump>>,
    pub llvm_type: BasicTypeEnum<'ctx>,
}
