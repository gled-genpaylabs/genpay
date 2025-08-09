use llvm_sys::{
    core::*,
    target::*,
    target_machine::*,
    prelude::*,
};

pub struct ObjectCompiler;

impl ObjectCompiler {
    pub fn compile_module(module: LLVMModuleRef, name: &str) {
        unsafe {
            LLVM_InitializeAllTargetInfos();
            LLVM_InitializeAllTargets();
            LLVM_InitializeAllTargetMCs();
            LLVM_InitializeAllAsmPrinters();
            LLVM_InitializeAllAsmParsers();

            let target_triple = LLVMGetDefaultTargetTriple();
            let mut target = std::ptr::null_mut();
            let mut error = std::ptr::null_mut();

            if LLVMGetTargetFromTriple(target_triple, &mut target, &mut error) != 0 {
                let error_message = std::ffi::CStr::from_ptr(error).to_str().unwrap().to_owned();
                LLVMDisposeMessage(error);
                panic!("Failed to get target from triple: {}", error_message);
            }

            let target_machine = LLVMCreateTargetMachine(
                target,
                target_triple,
                std::ffi::CString::new("generic").unwrap().as_ptr(),
                std::ffi::CString::new("").unwrap().as_ptr(),
                LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
                LLVMRelocMode::LLVMRelocPIC,
                LLVMCodeModel::LLVMCodeModelDefault,
            );

            let output_name = format!("{}.o", name);
            let output_name_c = std::ffi::CString::new(output_name).unwrap();
            let mut error = std::ptr::null_mut();

            if LLVMTargetMachineEmitToFile(
                target_machine,
                module,
                output_name_c.as_ptr() as *mut _,
                LLVMCodeGenFileType::LLVMObjectFile,
                &mut error,
            ) != 0
            {
                let error_message = std::ffi::CStr::from_ptr(error).to_str().unwrap().to_owned();
                LLVMDisposeMessage(error);
                panic!("Failed to emit to file: {}", error_message);
            }

            LLVMDisposeTargetMachine(target_machine);
        }
    }
}
