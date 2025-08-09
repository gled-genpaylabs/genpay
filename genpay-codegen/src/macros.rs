use genpay_parser::{expressions::Expressions, types::Type, value::Value};
use llvm_sys::core::*;
use llvm_sys::prelude::*;

use crate::CodeGen;

pub trait StandartMacros {
    fn build_macro_call(
        &mut self,
        id: &str,
        arguments: Vec<Expressions>,
    ) -> (Type, LLVMValueRef);
}

impl StandartMacros for CodeGen {
    fn build_macro_call(
        &mut self,
        id: &str,
        arguments: Vec<Expressions>,
    ) -> (Type, LLVMValueRef) {
        match id {
            "print" | "println" => {
                let mut literal =
                    if let Some(Expressions::Value(Value::String(str), _)) = arguments.first() {
                        str.clone()
                    } else {
                        String::default()
                    };
                let compiled_args = arguments
                    .iter()
                    .skip(1)
                    .map(|expr| self.compile_expression(expr.clone(), None))
                    .collect::<Vec<(Type, LLVMValueRef)>>();

                let format_specifiers = compiled_args
                    .iter()
                    .map(|(typ, _)| self.type_specifier(typ))
                    .collect::<Vec<String>>();

                format_specifiers.iter().for_each(|spec| {
                    if let Some(position) = literal.find("{}") {
                        literal.replace_range(position..position + 2, &spec);
                    }
                });

                if id.contains("ln") {
                    literal.push('\n')
                }
                let printf_name = std::ffi::CString::new("printf").unwrap();
                let printf_fn = unsafe {
                    let function = LLVMGetNamedFunction(self.module, printf_name.as_ptr());
                    if function == std::ptr::null_mut() {
                        let fn_type = LLVMFunctionType(LLVMInt32TypeInContext(self.context), [LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)].as_mut_ptr(), 1, 1);
                        LLVMAddFunction(self.module, printf_name.as_ptr(), fn_type)
                    } else {
                        function
                    }
                };

                let format_values = compiled_args
                    .into_iter()
                    .map(|arg| match arg.0.clone() {
                        Type::Bool => {
                            let (_true, _false) = self.booleans_strings();
                            let name = std::ffi::CString::new("").unwrap();
                            unsafe { LLVMBuildSelect(self.builder, arg.1, _true, _false, name.as_ptr()) }
                        }
                        Type::F32 => {
                            let name = std::ffi::CString::new("").unwrap();
                            unsafe { LLVMBuildFPExt(self.builder, arg.1, LLVMDoubleTypeInContext(self.context), name.as_ptr()) }
                        }
                        Type::Alias(alias) => {
                            let alias_type = self.get_alias_type(arg.0.clone(), None).unwrap();

                            match alias_type {
                                "struct" => {
                                    let display_function = self
                                        .scope
                                        .get_function(format!(
                                            "{}_{}__{}",
                                            alias_type, alias, "display"
                                        ))
                                        .unwrap();

                                    // NOTE: Method `display(&self) *char` won`t give mutability of
                                    // structure, it will only provide tool to use struct in basic
                                    // output.
                                    let self_val: LLVMValueRef =
                                        if self.is_a_pointer_value(arg.1) {
                                            arg.1
                                        } else {
                                            let alloca_name = std::ffi::CString::new("").unwrap();
                                            let alloca = unsafe { LLVMBuildAlloca(self.builder, LLVMTypeOf(arg.1), alloca_name.as_ptr()) };
                                            unsafe { LLVMBuildStore(self.builder, arg.1, alloca) };
                                            alloca
                                        };

                                    let call_name = std::ffi::CString::new("").unwrap();
                                    let output: LLVMValueRef = unsafe { LLVMBuildCall2(self.builder, display_function.fn_type, display_function.value, [self_val].as_mut_ptr(), 1, call_name.as_ptr()) };
                                    output
                                }
                                "enum" => arg.1,
                                _ => unreachable!(),
                            }
                        }
                        _ => arg.1,
                    })
                    .collect::<Vec<LLVMValueRef>>();

                let literal_c_string = std::ffi::CString::new(literal).unwrap();
                let global_name = std::ffi::CString::new("").unwrap();
                let global_literal: LLVMValueRef = unsafe { LLVMBuildGlobalStringPtr(self.builder, literal_c_string.as_ptr(), global_name.as_ptr()) };
                let mut call_arguments = [vec![global_literal], format_values].concat();

                let call_name = std::ffi::CString::new("").unwrap();
                unsafe { LLVMBuildCall2(self.builder, LLVMGetCalledFunctionType(printf_fn), printf_fn, call_arguments.as_mut_ptr(), call_arguments.len() as u32, call_name.as_ptr()) };

                (Type::Void, unsafe { LLVMConstInt(LLVMInt1TypeInContext(self.context), 0, 0) })
            }
            "format" => {
                let mut literal =
                    if let Some(Expressions::Value(Value::String(str), _)) = arguments.first() {
                        str.clone()
                    } else {
                        String::default()
                    };
                let compiled_args = arguments
                    .iter()
                    .skip(1)
                    .map(|expr| self.compile_expression(expr.clone(), None))
                    .collect::<Vec<(Type, LLVMValueRef)>>();

                let format_specifiers = compiled_args
                    .iter()
                    .map(|(typ, _)| self.type_specifier(typ))
                    .collect::<Vec<String>>();

                format_specifiers.into_iter().for_each(|spec| {
                    if let Some(position) = literal.find("{}") {
                        literal.replace_range(position..position + 2, &spec);
                    }
                });

                if id.contains("ln") {
                    literal.push('\n')
                }
                let snprintf_name = std::ffi::CString::new("snprintf").unwrap();
                let snprintf_fn = unsafe {
                    let function = LLVMGetNamedFunction(self.module, snprintf_name.as_ptr());
                    if function == std::ptr::null_mut() {
                        let fn_type = LLVMFunctionType(LLVMInt32TypeInContext(self.context), [LLVMPointerType(LLVMInt8TypeInContext(self.context), 0), LLVMInt64TypeInContext(self.context), LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)].as_mut_ptr(), 3, 1);
                        LLVMAddFunction(self.module, snprintf_name.as_ptr(), fn_type)
                    } else {
                        function
                    }
                };

                let format_values = compiled_args
                    .into_iter()
                    .map(|arg| match arg.0.clone() {
                        Type::Bool => {
                            let (_true, _false) = self.booleans_strings();
                            let name = std::ffi::CString::new("").unwrap();
                            unsafe { LLVMBuildSelect(self.builder, arg.1, _true, _false, name.as_ptr()) }
                        }
                        Type::F32 => {
                            let name = std::ffi::CString::new("").unwrap();
                            unsafe { LLVMBuildFPExt(self.builder, arg.1, LLVMDoubleTypeInContext(self.context), name.as_ptr()) }
                        }
                        Type::Alias(alias) => {
                            let alias_type = self.get_alias_type(arg.0.clone(), None).unwrap();

                            match alias_type {
                                "struct" => {
                                    let display_function = self
                                        .scope
                                        .get_function(format!(
                                            "{}_{}__{}",
                                            alias_type, alias, "display"
                                        ))
                                        .unwrap();

                                    let self_val: LLVMValueRef = arg.1;

                                    let call_name = std::ffi::CString::new("").unwrap();
                                    let output: LLVMValueRef = unsafe { LLVMBuildCall2(self.builder, display_function.fn_type, display_function.value, [self_val].as_mut_ptr(), 1, call_name.as_ptr()) };
                                    output
                                }
                                "enum" => arg.1,
                                _ => unreachable!(),
                            }
                        }
                        _ => arg.1,
                    })
                    .collect::<Vec<LLVMValueRef>>();

                let literal_c_string = std::ffi::CString::new(literal).unwrap();
                let global_name = std::ffi::CString::new("").unwrap();
                let global_literal: LLVMValueRef = unsafe { LLVMBuildGlobalStringPtr(self.builder, literal_c_string.as_ptr(), global_name.as_ptr()) };

                // first call for output size
                let mut first_call_args = [
                    vec![
                        unsafe { LLVMConstNull(LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)) },
                        unsafe { LLVMConstInt(LLVMInt64TypeInContext(self.context), 0, 0) },
                        global_literal,
                    ],
                    format_values.clone(),
                ]
                .concat();

                let call_name = std::ffi::CString::new("").unwrap();
                let length = unsafe { LLVMBuildCall2(self.builder, LLVMGetCalledFunctionType(snprintf_fn), snprintf_fn, first_call_args.as_mut_ptr(), first_call_args.len() as u32, call_name.as_ptr()) };

                let buffer = {
                    let add_name = std::ffi::CString::new("").unwrap();
                    let buffer_size = unsafe { LLVMBuildAdd(self.builder, length, LLVMConstInt(LLVMInt32TypeInContext(self.context), 1, 0), add_name.as_ptr()) };

                    let alloca_name = std::ffi::CString::new("").unwrap();
                    unsafe { LLVMBuildArrayAlloca(self.builder, LLVMInt8TypeInContext(self.context), buffer_size, alloca_name.as_ptr()) }
                };

                // second call for the final format
                let sprintf_name = std::ffi::CString::new("sprintf").unwrap();
                let sprintf_fn = unsafe {
                    let function = LLVMGetNamedFunction(self.module, sprintf_name.as_ptr());
                    if function == std::ptr::null_mut() {
                        let fn_type = LLVMFunctionType(LLVMInt32TypeInContext(self.context), [LLVMPointerType(LLVMInt8TypeInContext(self.context), 0), LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)].as_mut_ptr(), 2, 1);
                        LLVMAddFunction(self.module, sprintf_name.as_ptr(), fn_type)
                    } else {
                        function
                    }
                };

                let mut call_arguments = [vec![buffer, global_literal], format_values].concat();

                let call_name = std::ffi::CString::new("").unwrap();
                unsafe { LLVMBuildCall2(self.builder, LLVMGetCalledFunctionType(sprintf_fn), sprintf_fn, call_arguments.as_mut_ptr(), call_arguments.len() as u32, call_name.as_ptr()) };

                (Type::Pointer(Box::new(Type::Char)), buffer)
            }
            "panic" => {
                let (mut literal, call_line) =
                    if let Some(Expressions::Value(Value::String(str), span)) = arguments.first() {
                        (str.clone(), self.get_source_line(span.0))
                    } else {
                        (String::default(), 0)
                    };

                let compiled_args = arguments
                    .iter()
                    .skip(1)
                    .map(|expr| self.compile_expression(expr.clone(), None))
                    .collect::<Vec<(Type, LLVMValueRef)>>();

                let format_specifiers = compiled_args
                    .iter()
                    .map(|(typ, _)| self.type_specifier(typ))
                    .collect::<Vec<String>>();

                format_specifiers.into_iter().for_each(|spec| {
                    if let Some(position) = literal.find("{}") {
                        literal.replace_range(position..position + 2, &spec);
                    }
                });

                if id.contains("ln") {
                    literal.push('\n')
                }

                let format_values = compiled_args
                    .into_iter()
                    .map(|arg| match arg.0.clone() {
                        Type::Bool => {
                            let (_true, _false) = self.booleans_strings();
                            let name = std::ffi::CString::new("").unwrap();
                            unsafe { LLVMBuildSelect(self.builder, arg.1, _true, _false, name.as_ptr()) }
                        }
                        Type::Alias(alias) => {
                            let alias_type = self.get_alias_type(arg.0.clone(), None).unwrap();

                            match alias_type {
                                "struct" => {
                                    let display_function = self
                                        .scope
                                        .get_function(format!(
                                            "{}_{}__{}",
                                            alias_type, alias, "display"
                                        ))
                                        .unwrap();

                                    let self_val: LLVMValueRef = arg.1;

                                    let call_name = std::ffi::CString::new("").unwrap();
                                    let output: LLVMValueRef = unsafe { LLVMBuildCall2(self.builder, display_function.fn_type, display_function.value, [self_val].as_mut_ptr(), 1, call_name.as_ptr()) };
                                    output
                                }
                                "enum" => arg.1,
                                _ => unreachable!(),
                            }
                        }
                        _ => arg.1,
                    })
                    .collect::<Vec<LLVMValueRef>>();

                self.build_panic(literal, format_values, call_line);
                (Type::Void, unsafe { LLVMConstInt(LLVMInt1TypeInContext(self.context), 0, 0) })
            }
            "sizeof" => {
                let instance = arguments.first().unwrap();
                let basic_type = {
                    if let Expressions::Argument {
                        name: _,
                        r#type,
                        span: _,
                    } = instance
                    {
                        self.get_basic_type(r#type.clone())
                    } else {
                        unsafe { LLVMTypeOf(self.compile_expression(instance.clone(), None).1) }
                    }
                };

                (Type::USIZE, unsafe { LLVMSizeOf(basic_type) })
            }
            "cast" => {
                let from = arguments.first().unwrap().clone();
                let to = arguments.get(1).unwrap().clone();

                let from_value = self.compile_expression(from, None);
                let to_type = self.compile_expression(to, None);
                let target_basic_type = self.get_basic_type(to_type.0.clone());

                match (&from_value.0, &to_type.0) {
                    (from, to) if from == to => from_value,

                    (from, to)
                        if genpay_semantic::Analyzer::is_integer(from)
                            && genpay_semantic::Analyzer::is_integer(to)
                            || from == &Type::Char
                            || to == &Type::Char
                            || from == &Type::Bool
                            || to == &Type::Bool =>
                    {
                        let unsigned = genpay_semantic::Analyzer::is_unsigned_integer(to);

                        let from_order = genpay_semantic::Analyzer::integer_order(from);
                        let to_order = genpay_semantic::Analyzer::integer_order(to);

                        let name = std::ffi::CString::new("").unwrap();
                        let value = if from_order > to_order {
                            // truncating
                            unsafe { LLVMBuildTrunc(self.builder, from_value.1, target_basic_type, name.as_ptr()) }
                        } else if from_order < to_order {
                            // extending
                            if unsigned {
                                unsafe { LLVMBuildZExt(self.builder, from_value.1, target_basic_type, name.as_ptr()) }
                            } else {
                                unsafe { LLVMBuildSExt(self.builder, from_value.1, target_basic_type, name.as_ptr()) }
                            }
                        } else {
                            // casting
                            unsafe { LLVMBuildBitCast(self.builder, from_value.1, target_basic_type, name.as_ptr()) }
                        };

                        (to_type.0, value)
                    }

                    (from, to)
                        if genpay_semantic::Analyzer::is_float(from)
                            && genpay_semantic::Analyzer::is_float(to) =>
                    {
                        let from_order = genpay_semantic::Analyzer::float_order(from);
                        let to_order = genpay_semantic::Analyzer::float_order(to);

                        let name = std::ffi::CString::new("").unwrap();
                        let value = if from_order > to_order {
                            // truncating
                            unsafe { LLVMBuildFPTrunc(self.builder, from_value.1, target_basic_type, name.as_ptr()) }
                        } else {
                            // extending
                            unsafe { LLVMBuildFPExt(self.builder, from_value.1, target_basic_type, name.as_ptr()) }
                        };

                        (to_type.0, value)
                    }

                    (from, to)
                        if genpay_semantic::Analyzer::is_float(from)
                            && genpay_semantic::Analyzer::is_integer(to) =>
                    {
                        let unsigned = genpay_semantic::Analyzer::is_unsigned_integer(to);

                        let name = std::ffi::CString::new("").unwrap();
                        let value = if unsigned {
                            unsafe { LLVMBuildFPToUI(self.builder, from_value.1, target_basic_type, name.as_ptr()) }
                        } else {
                            unsafe { LLVMBuildFPToSI(self.builder, from_value.1, target_basic_type, name.as_ptr()) }
                        };

                        (to_type.0, value)
                    }

                    (from, to)
                        if genpay_semantic::Analyzer::is_integer(from)
                            && genpay_semantic::Analyzer::is_float(to) =>
                    {
                        let unsigned = genpay_semantic::Analyzer::is_unsigned_integer(from);

                        let name = std::ffi::CString::new("").unwrap();
                        let value = if unsigned {
                            unsafe { LLVMBuildUIToFP(self.builder, from_value.1, target_basic_type, name.as_ptr()) }
                        } else {
                            unsafe { LLVMBuildSIToFP(self.builder, from_value.1, target_basic_type, name.as_ptr()) }
                        };

                        (to_type.0, value)
                    }

                    _ => panic!(
                        "Unimplemented cast type catched: `{}` -> `{}`",
                        from_value.0, to_type.0
                    ),
                }
            }
            _ => {
                panic!("Provided macros is under development stage: `{id}!()`")
            }
        }
    }
}
