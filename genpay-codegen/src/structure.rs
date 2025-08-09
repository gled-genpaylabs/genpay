use crate::function::Function;
use genpay_parser::types::Type;
use llvm_sys::prelude::LLVMTypeRef;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Structure {
    pub fields: HashMap<String, Field>,
    pub functions: HashMap<String, Function>,
    pub llvm_type: LLVMTypeRef,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub nth: u32,
    pub datatype: Type,
    pub llvm_type: LLVMTypeRef,
}
