use genpay_parser::types::Type;
use llvm_sys::prelude::{LLVMTypeRef, LLVMValueRef};

#[derive(Debug, Clone)]
pub struct Variable {
    pub datatype: Type,
    pub llvm_type: LLVMTypeRef,
    pub ptr: LLVMValueRef,
    pub no_drop: bool,
    pub global: bool,
}
