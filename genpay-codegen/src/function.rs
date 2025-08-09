use genpay_parser::types::Type;
use llvm_sys::prelude::{LLVMValueRef, LLVMTypeRef};

#[derive(Debug, Clone)]
pub struct Function {
    pub datatype: Type,
    pub fn_type: LLVMTypeRef,
    pub value: LLVMValueRef,
    pub arguments: Vec<Type>,
    pub called: bool,
}
