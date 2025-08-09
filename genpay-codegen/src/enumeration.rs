use llvm_sys::prelude::LLVMTypeRef;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Enumeration {
    pub fields: Vec<String>,
    pub functions: HashMap<String, crate::function::Function>,
    pub llvm_type: LLVMTypeRef,
}
