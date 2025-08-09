use crate::{
    enumeration::Enumeration,
    function::Function,
    macros::StandartMacros,
    scope::Scope,
    structure::{Field, Structure},
    variable::Variable,
};
use genpay_parser::{expressions::Expressions, statements::Statements, types::Type, value::Value};
use llvm_sys::{
    core::*,
    prelude::*,
    target::{LLVM_InitializeAllTargetInfos, LLVM_InitializeAllTargets, LLVM_InitializeAllTargetMCs, LLVM_InitializeAllAsmPrinters, LLVM_InitializeAllAsmParsers},
    target_machine::*,
    analysis::{LLVMVerifyFunction, LLVMVerifierFailureAction},
    LLVMTypeKind,
};

use genpay_semantic::symtable::SymbolTable;
use std::collections::HashMap;

mod enumeration;
mod function;
mod macros;
mod scope;
mod structure;
mod variable;

pub struct CodeGen {
    source: String,

    context: LLVMContextRef,
    builder: LLVMBuilderRef,
    module: LLVMModuleRef,

    scope: Box<Scope>,
    function: Option<LLVMValueRef>,
    breaks: Vec<LLVMBasicBlockRef>,
    booleans_strings: Option<(LLVMValueRef, LLVMValueRef)>,

    symtable: SymbolTable,
    imports: HashMap<String, ModuleContent>,
    includes: Vec<String>,
}

impl CodeGen {
    pub fn enter_scope(&mut self, mut scope: Scope) {
        let parent = self.scope.to_owned();
        scope.parent = Some(parent);

        self.scope = Box::new(scope);
    }

    pub fn enter_new_scope(&mut self) {
        let new_scope = Scope::new();
        self.enter_scope(new_scope);
    }

    pub fn cleanup_variables(&mut self) {
        let scope_variables = self.scope.stricted_variables();

        scope_variables.into_iter().for_each(|(name, var)| {
            match var.datatype.clone() {
                Type::Alias(alias) => {
                    if matches!(self.get_alias_type(var.datatype.clone(), None), Some("struct")) && name != "self" && !var.no_drop {
                        let structure = self.scope.get_struct(&alias).unwrap();

                        if let Some(destructor) = structure.functions.get("drop") {
                            let called = self.scope.get_function(format!("struct_{alias}__drop")).unwrap().called;
                            if destructor.arguments == vec![Type::Pointer(Box::new(var.datatype))] && !called {
                                let call_name = std::ffi::CString::new("").unwrap();
                                unsafe {
                                    LLVMBuildCall2(
                                        self.builder,
                                        destructor.fn_type,
                                        destructor.value,
                                        [var.ptr].as_mut_ptr(),
                                        1,
                                        call_name.as_ptr(),
                                    )
                                };
                            }
                        }
                    }
                }
                Type::Struct(_, _) => {
                    panic!("Something went wrong, developer got brainrot, please report that on Github Issue")
                },
                _ => {}
            }
        })
    }

    pub fn exit_scope_raw(&mut self) {
        if let Some(parent) = self.scope.parent.to_owned() {
            self.scope = parent;
        }
    }

    pub fn exit_scope(&mut self) {
        if let Some(parent) = self.scope.parent.to_owned() {
            let insert_block = unsafe { LLVMGetInsertBlock(self.builder) };

            if unsafe { LLVMGetBasicBlockTerminator(insert_block) } != std::ptr::null_mut() {
                let terminator = unsafe { LLVMGetBasicBlockTerminator(insert_block) };
                unsafe { LLVMPositionBuilderBefore(self.builder, terminator) };
                self.cleanup_variables();

                unsafe { LLVMPositionBuilderAtEnd(self.builder, insert_block) };
            } else {
                self.cleanup_variables();
            }

            self.scope = parent;
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModuleContent {
    pub functions: HashMap<String, Function>,
    pub structures: HashMap<String, Structure>,
    pub enumerations: HashMap<String, Enumeration>,
}

impl CodeGen {
    pub fn create_context() -> LLVMContextRef {
        unsafe {
            LLVM_InitializeAllTargetInfos();
            LLVM_InitializeAllTargets();
            LLVM_InitializeAllTargetMCs();
            LLVM_InitializeAllAsmPrinters();
            LLVM_InitializeAllAsmParsers();
            LLVMContextCreate()
        }
    }

    pub fn new(
        context: LLVMContextRef,
        module_name: &str,
        module_source: &str,
        symtable: SymbolTable,
    ) -> Self {
        unsafe {
            let module_name = std::ffi::CString::new(module_name).unwrap();
            let module = LLVMModuleCreateWithNameInContext(module_name.as_ptr(), context);
            let builder = LLVMCreateBuilderInContext(context);

            let source_file_name = std::ffi::CString::new(format!("{}.genpay", module_name.to_str().unwrap())).unwrap();
            LLVMSetSourceFileName(module, source_file_name.as_ptr(), source_file_name.to_bytes().len());

            let target_triple = LLVMGetDefaultTargetTriple();
            LLVMSetTarget(module, target_triple);

            let ident_key = std::ffi::CString::new("ident").unwrap();
            let ident_value = std::ffi::CString::new(format!(
                "genpay compiler v{} {}",
                env!("CARGO_PKG_VERSION"),
                env!("GIT_HASH").chars().take(8).collect::<String>()
            )).unwrap();
            let metadata_node = LLVMMDStringInContext2(context, ident_value.as_ptr(), ident_value.to_bytes().len());
            let metadata = LLVMMDNodeInContext2(context, [metadata_node].as_mut_ptr(), 1);
            LLVMAddNamedMetadataOperand(module, ident_key.as_ptr(), LLVMMetadataAsValue(context, metadata));

            Self {
                source: module_source.to_owned(),
                context,
                builder,
                module,
                scope: Box::new(Scope::new()),
                function: None,
                breaks: vec![],
                booleans_strings: None,
                symtable,
                imports: HashMap::new(),
                includes: Vec::new(),
            }
        }
    }

    pub fn compile(
        &mut self,
        statements: Vec<Statements>,
        prefix: Option<String>,
    ) -> (LLVMModuleRef, ModuleContent) {
        // let pre_statements = statements
        //     .iter()
        //     .filter(|stmt| match stmt {
        //         Statements::StructDefineStatement {
        //             name: _,
        //             fields: _,
        //             functions: _,
        //             public: _,
        //             span: _,
        //         } => true,
        //         Statements::EnumDefineStatement {
        //             name: _,
        //             fields: _,
        //             functions: _,
        //             public: _,
        //             span: _,
        //         } => true,
        //         Statements::TypedefStatement {
        //             alias: _,
        //             datatype: _,
        //             span: _,
        //         } => true,
        //         Statements::ImportStatement { path: _, span: _ } => true,
        //         Statements::FunctionDefineStatement {
        //             name,
        //             datatype: _,
        //             arguments: _,
        //             block: _,
        //             public: _,
        //             span: _,
        //             header_span: _,
        //         } => name != "main",
        //         _ => false,
        //     })
        //     .collect::<Vec<&Statements>>();
        //
        // let after_statements = statements
        //     .iter()
        //     .filter(|stmt| !pre_statements.contains(stmt));
        //
        // pre_statements
        //     .clone()
        //     .into_iter()
        //     .for_each(|stmt| self.compile_statement(stmt.clone(), prefix.clone()));
        // after_statements
        //     .into_iter()
        //     .for_each(|stmt| self.compile_statement(stmt.clone(), prefix.clone()));

        statements
            .into_iter()
            .for_each(|stmt| self.compile_statement(stmt, prefix.clone()));

        let module_content = {
            let functions = self.scope.stricted_functions();
            let structures = self.scope.stricted_structs();
            let enumerations = self.scope.stricted_enums();

            ModuleContent {
                functions,
                structures,
                enumerations,
            }
        };

        (self.module, module_content)
    }
}

impl CodeGen {
    fn compile_statement(&mut self, statement: Statements, prefix: Option<String>) {
        match statement {
            Statements::AssignStatement {
                object,
                value,
                span: _,
            } => {
                if let Expressions::Value(Value::Identifier(identifier), _) = object {
                    let var = self.scope.get_variable(&identifier).unwrap();
                    let compiled_value = self.compile_expression(value, Some(var.datatype));

                    unsafe { LLVMBuildStore(self.builder, compiled_value.1, var.ptr) };
                } else {
                    let ptr = self
                        .compile_expression(object, Some(Type::Pointer(Box::new(Type::Undefined))));
                    let value = self.compile_expression(value, None);

                    unsafe { LLVMBuildStore(self.builder, value.1, ptr.1) };
                }
            }
            Statements::BinaryAssignStatement {
                object,
                operand,
                value,
                span,
            } => {
                let stmt = Statements::AssignStatement {
                    object: object.clone(),
                    value: Expressions::Binary {
                        operand,
                        lhs: Box::new(object),
                        rhs: Box::new(value),
                        span: (0, 0),
                    },
                    span,
                };

                self.compile_statement(stmt, prefix);
            }
            Statements::DerefAssignStatement {
                object,
                value,
                span: _,
            } => {
                let (instance_type, instance_ptr) = self.compile_expression(object.clone(), None);

                match instance_type {
                    Type::Pointer(ptr_type) => {
                        let compiled_value = self.compile_expression(value, Some(*ptr_type));

                        unsafe { LLVMBuildStore(self.builder, compiled_value.1, instance_ptr) };
                    }

                    Type::Alias(alias) => {
                        let instance_ptr = self
                            .compile_expression(
                                object,
                                Some(Type::Pointer(Box::new(Type::Undefined))),
                            )
                            .1;

                        let struct_type = self.scope.get_struct(alias).unwrap();
                        let deref_assign_fn = struct_type.functions.get("deref_assign").unwrap();

                        let compiled_value = self
                            .compile_expression(value, Some(deref_assign_fn.arguments[1].clone()));

                        let call_name = std::ffi::CString::new("@genpay_deref_assign_call").unwrap();
                        unsafe {
                            LLVMBuildCall2(
                                self.builder,
                                deref_assign_fn.fn_type,
                                deref_assign_fn.value,
                                [instance_ptr, compiled_value.1].as_mut_ptr(),
                                2,
                                call_name.as_ptr(),
                            )
                        };
                    }

                    _ => {
                        panic!("Non-pointer type handled: `{instance_type}`")
                    }
                }
            }
            Statements::SliceAssignStatement {
                object,
                index,
                value,
                span,
            } => {
                let obj = self.compile_expression(object.clone(), None);
                let idx = self.compile_expression(index, Some(Type::USIZE));

                match obj.0 {
                    Type::Array(item_type, len) => {
                        let obj_ptr = if self.is_a_pointer_value(obj.1) {
                            obj.1
                        } else {
                            self.compile_expression(
                                object,
                                Some(Type::Pointer(Box::new(Type::Undefined))),
                            ).1
                        };

                        let value = self.compile_expression(value, Some(*item_type.clone()));

                        // checking for the right index
                        let checker_block_name = std::ffi::CString::new("__idxcb").unwrap();
                        let checker_block = unsafe { LLVMAppendBasicBlockInContext(self.context, self.function.unwrap(), checker_block_name.as_ptr()) };
                        let error_block_name = std::ffi::CString::new("__idxcb_err").unwrap();
                        let error_block = unsafe { LLVMAppendBasicBlockInContext(self.context, self.function.unwrap(), error_block_name.as_ptr()) };
                        let ok_block_name = std::ffi::CString::new("__idxcb_ok").unwrap();
                        let ok_block = unsafe { LLVMAppendBasicBlockInContext(self.context, self.function.unwrap(), ok_block_name.as_ptr()) };

                        unsafe { LLVMBuildBr(self.builder, checker_block) };
                        unsafe { LLVMPositionBuilderAtEnd(self.builder, checker_block) };

                        let expected_basic_value = unsafe { LLVMConstInt(LLVMInt64TypeInContext(self.context), len as u64, 0) };
                        let provided_basic_value = idx.1;

                        let cmp_name = std::ffi::CString::new("").unwrap();
                        let cmp_value = unsafe { LLVMBuildICmp(self.builder, llvm_sys::LLVMIntPredicate::LLVMIntSLT, provided_basic_value, expected_basic_value, cmp_name.as_ptr()) };
                        unsafe { LLVMBuildCondBr(self.builder, cmp_value, ok_block, error_block) };

                        unsafe { LLVMPositionBuilderAtEnd(self.builder, error_block) };

                        self.build_panic(
                            "Array has len %ld, but index is %ld",
                            vec![expected_basic_value, provided_basic_value],
                            self.get_source_line(span.0),
                        );
                        self.build_branch(ok_block);
                        unsafe { LLVMPositionBuilderAtEnd(self.builder, ok_block) };

                        // getting ptr
                        let gep_name = std::ffi::CString::new("").unwrap();
                        let ptr = unsafe {
                            LLVMBuildInBoundsGEP2(
                                self.builder,
                                self.get_basic_type(*item_type),
                                obj_ptr,
                                [idx.1].as_mut_ptr(),
                                1,
                                gep_name.as_ptr(),
                            )
                        };

                        // storing value
                        unsafe { LLVMBuildStore(self.builder, value.1, ptr) };
                    }
                    Type::Pointer(ptr_type) => {
                        // compiling value and ptr
                        let value = self.compile_expression(value, Some(*ptr_type.clone()));
                        let gep_name = std::ffi::CString::new("").unwrap();
                        let ptr = unsafe {
                            LLVMBuildInBoundsGEP2(
                                self.builder,
                                self.get_basic_type(*ptr_type),
                                obj.1,
                                [idx.1].as_mut_ptr(),
                                1,
                                gep_name.as_ptr(),
                            )
                        };

                        // storing value
                        unsafe { LLVMBuildStore(self.builder, value.1, ptr) };
                    }

                    Type::Alias(alias) => {
                        let instance_ptr = self
                            .compile_expression(
                                object,
                                Some(Type::Pointer(Box::new(Type::Undefined))),
                            )
                            .1;

                        let struct_type = self.scope.get_struct(alias).unwrap();
                        let slice_assign_fn = struct_type.functions.get("slice_assign").unwrap();
                        let compiled_value = self
                            .compile_expression(value, Some(slice_assign_fn.arguments[2].clone()));

                        let call_name = std::ffi::CString::new("@genpay_slice_assign_call").unwrap();
                        unsafe {
                            LLVMBuildCall2(
                                self.builder,
                                slice_assign_fn.fn_type,
                                slice_assign_fn.value,
                                [instance_ptr, idx.1, compiled_value.1].as_mut_ptr(),
                                3,
                                call_name.as_ptr(),
                            )
                        };
                    }

                    _ => unreachable!(),
                }
            }
            Statements::FieldAssignStatement {
                object,
                value,
                span: _,
            } => {
                let compiled_object =
                    self.compile_expression(object, Some(Type::Pointer(Box::new(Type::Undefined))));
                let compiled_value = self.compile_expression(value, Some(compiled_object.0));

                unsafe { LLVMBuildStore(self.builder, compiled_value.1, compiled_object.1) };
            }

            Statements::AnnotationStatement {
                identifier,
                datatype,
                value,
                span: _,
            } => {
                let empty_binding = identifier == "_";

                match (datatype, value) {
                    (Some(datatype), Some(value)) => {
                        let value = self.compile_expression(value, Some(datatype.clone()));

                        if !empty_binding {
                            let id_c_string = std::ffi::CString::new(identifier.clone()).unwrap();
                            let alloca = unsafe { LLVMBuildAlloca(self.builder, LLVMTypeOf(value.1), id_c_string.as_ptr()) };

                            self.scope.set_variable(
                                identifier,
                                Variable {
                                    datatype,
                                    llvm_type: unsafe { LLVMTypeOf(value.1) },
                                    ptr: alloca,
                                    no_drop: false,
                                    global: false,
                                },
                            );
                            unsafe { LLVMBuildStore(self.builder, value.1, alloca) };
                        }
                    }
                    (Some(datatype), _) => {
                        let basic_type = self.get_basic_type(datatype.clone());

                        if !empty_binding {
                            let id_c_string = std::ffi::CString::new(identifier.clone()).unwrap();
                            let alloca = unsafe { LLVMBuildAlloca(self.builder, basic_type, id_c_string.as_ptr()) };

                            self.scope.set_variable(
                                identifier,
                                Variable {
                                    datatype,
                                    llvm_type: basic_type,
                                    ptr: alloca,
                                    no_drop: false,
                                    global: false,
                                },
                            );
                        }
                    }
                    (_, Some(value)) => {
                        let compiled_value = self.compile_expression(value, None);

                        if !empty_binding {
                            let id_c_string = std::ffi::CString::new(identifier.clone()).unwrap();
                            let alloca = unsafe { LLVMBuildAlloca(self.builder, LLVMTypeOf(compiled_value.1), id_c_string.as_ptr()) };

                            self.scope.set_variable(
                                identifier,
                                Variable {
                                    datatype: compiled_value.0,
                                    llvm_type: unsafe { LLVMTypeOf(compiled_value.1) },
                                    ptr: alloca,
                                    no_drop: false,
                                    global: false,
                                },
                            );
                            unsafe { LLVMBuildStore(self.builder, compiled_value.1, alloca) };
                        }
                    }
                    _ => unreachable!(),
                };
            }

            Statements::FunctionDefineStatement {
                name: raw_name,
                datatype,
                arguments,
                block,
                public: _,
                span: _,
                header_span: _,
            } => {
                let module_name = unsafe { std::ffi::CStr::from_ptr(LLVMGetModuleIdentifier(self.module, &mut 0)).to_str().unwrap() };
                let name = format!("{}{}", prefix.clone().unwrap_or_default(), raw_name,);

                let llvm_ir_name = format!(
                    "{}{}{}({})",
                    if module_name == "main" {
                        "".to_owned()
                    } else {
                        format!("{module_name}.")
                    },
                    prefix.clone().unwrap_or_default(),
                    raw_name,
                    arguments
                        .iter()
                        .map(|arg| arg.1.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                );

                let mut args: Vec<LLVMTypeRef> = Vec::new();
                arguments.iter().for_each(|arg| {
                    args.push(self.get_basic_type(arg.1.clone()));
                });

                let fn_type = self.get_fn_type(datatype.clone(), &mut args, false);
                let function_name = if name == "main" {
                    std::ffi::CString::new("main").unwrap()
                } else {
                    std::ffi::CString::new(llvm_ir_name).unwrap()
                };
                let function = unsafe { LLVMAddFunction(self.module, function_name.as_ptr(), fn_type) };
                let entry_name = std::ffi::CString::new("entry").unwrap();
                let entry = unsafe { LLVMAppendBasicBlockInContext(self.context, function, entry_name.as_ptr()) };

                let old_position = unsafe { LLVMGetInsertBlock(self.builder) };
                let old_function = self.function;
                unsafe { LLVMPositionBuilderAtEnd(self.builder, entry) };
                self.function = Some(function);

                self.enter_new_scope();

                arguments.iter().enumerate().for_each(|(index, arg)| {
                    let arg_name = arg.0.clone();
                    let arg_value = unsafe { LLVMGetParam(function, index as u32) };

                    let param_type = self.get_basic_type(arg.1.clone());
                    let param_alloca_name = std::ffi::CString::new("").unwrap();
                    let param_alloca = unsafe { LLVMBuildAlloca(self.builder, param_type, param_alloca_name.as_ptr()) };

                    unsafe { LLVMBuildStore(self.builder, arg_value, param_alloca) };

                    let arg_datatype = match arg.1 {
                        Type::SelfRef => {
                            let prefix = prefix.clone().unwrap();
                            let alias = prefix.replace("struct_", "").replace("enum_", "");
                            let alias = alias.split("__").collect::<Vec<&str>>()[0];

                            Type::Alias(alias.to_owned())
                        }
                        _ => arg.1.clone(),
                    };

                    self.scope.set_variable(
                        arg_name,
                        Variable {
                            datatype: arg_datatype,
                            llvm_type: param_type,
                            ptr: param_alloca,
                            no_drop: false,
                            global: false,
                        },
                    );
                });

                let typed_args = arguments.iter().map(|x| x.1.clone()).collect::<Vec<Type>>();
                self.scope.set_function(
                    name.clone(),
                    Function {
                        datatype: datatype.clone(),
                        fn_type,
                        value: function,
                        arguments: typed_args.clone(),
                        called: false,
                    },
                );
                block
                    .iter()
                    .for_each(|stmt| self.compile_statement(stmt.clone(), None));

                if datatype == Type::Void {
                    unsafe { LLVMBuildRetVoid(self.builder) };
                }

                if unsafe { LLVMVerifyFunction(function, LLVMVerifierFailureAction::LLVMPrintMessageAction) } != 0 {
                    let latest_block = unsafe { LLVMGetLastBasicBlock(function) };
                    let prev_pos = unsafe { LLVMGetInsertBlock(self.builder) };

                    unsafe { LLVMPositionBuilderAtEnd(self.builder, latest_block) };

                    if unsafe { LLVMGetLastInstruction(latest_block) } == std::ptr::null_mut() {
                        unsafe { LLVMBuildRetVoid(self.builder) };
                    }

                    unsafe { LLVMPositionBuilderAtEnd(self.builder, prev_pos) };
                }

                self.exit_scope();
                if old_position != std::ptr::null_mut() {
                    unsafe { LLVMPositionBuilderAtEnd(self.builder, old_position) };
                }

                self.function = old_function;

                self.scope.set_function(
                    name.clone(),
                    Function {
                        datatype: datatype.clone(),
                        fn_type,
                        value: function,
                        arguments: typed_args,
                        called: false,
                    },
                );
            }
            Statements::FunctionCallStatement {
                name,
                arguments,
                span: _,
            } => {
                {
                    let mut_fn = self.scope.get_mut_function(&name).unwrap();
                    mut_fn.called = true
                }

                let function = self.scope.get_function(&name).unwrap();
                let mut basic_args: Vec<LLVMValueRef> = Vec::new();

                let mut function_args = function.arguments.clone();

                if function_args.len() >= 2
                    && function_args[function_args.len() - 1] == Type::Void
                    && function_args[function_args.len() - 2] == Type::Void
                {
                    function_args.resize(arguments.len(), Type::Void);
                }

                arguments
                    .into_iter()
                    .zip(function_args)
                    .for_each(|(expr, expected)| {
                        basic_args.push(self.compile_expression(expr, Some(expected)).1);
                    });

                let call_name = std::ffi::CString::new("").unwrap();
                unsafe {
                    LLVMBuildCall2(
                        self.builder,
                        function.fn_type,
                        function.value,
                        basic_args.as_mut_ptr(),
                        basic_args.len() as u32,
                        call_name.as_ptr(),
                    )
                };
            }

            Statements::MacroCallStatement {
                name,
                arguments,
                span: _,
            } => {
                self.build_macro_call(&name, arguments);
            }

            Statements::StructDefineStatement {
                name: raw_name,
                fields,
                functions,
                public: _,
                span: _,
            } => {
                let name = format!("{}{}", prefix.clone().unwrap_or_default(), raw_name);
                let llvm_name = format!(
                    "{}.{}{}",
                    unsafe { std::ffi::CStr::from_ptr(LLVMGetModuleIdentifier(self.module, &mut 0)).to_str().unwrap() },
                    prefix.unwrap_or_default(),
                    raw_name
                );

                let llvm_name_c_string = std::ffi::CString::new(llvm_name).unwrap();
                let struct_type = unsafe { LLVMStructCreateNamed(self.context, llvm_name_c_string.as_ptr()) };
                let mut compiled_fields = Vec::new();

                fields.iter().for_each(|field| {
                    compiled_fields.push(Field {
                        name: field.0.to_owned(),
                        nth: compiled_fields.len() as u32,
                        datatype: field.1.to_owned(),
                        llvm_type: self.get_basic_type(field.1.to_owned()),
                    });
                });

                let mut basic_fields_types = compiled_fields
                    .iter()
                    .map(|field| field.llvm_type)
                    .collect::<Vec<LLVMTypeRef>>();
                unsafe { LLVMStructSetBody(struct_type, basic_fields_types.as_mut_ptr(), basic_fields_types.len() as u32, 0) };

                let mut fields_hashmap = HashMap::new();
                compiled_fields.into_iter().for_each(|field| {
                    fields_hashmap.insert(field.name.clone(), field);
                });

                self.scope.set_struct(
                    name.clone(),
                    Structure {
                        fields: fields_hashmap,
                        functions: HashMap::new(),
                        llvm_type: struct_type,
                    },
                );

                functions.iter().for_each(|(_, function_statement)| {
                    self.enter_new_scope();

                    self.compile_statement(
                        function_statement.to_owned(),
                        Some(format!("struct_{name}__")),
                    );

                    let (mut function_id, mut function_value) =
                        self.scope.stricted_functions().into_iter().last().unwrap();
                    self.exit_scope_raw();

                    if let Some(Type::SelfRef) = function_value.arguments.first() {
                        *function_value.arguments.first_mut().unwrap() =
                            Type::Pointer(Box::new(Type::Alias(name.clone())));
                    }
                    self.scope
                        .set_function(function_id.clone(), function_value.clone());

                    function_id = function_id.replace(&format!("struct_{name}__"), "");
                    self.scope
                        .get_mut_struct(&name)
                        .unwrap()
                        .functions
                        .insert(function_id, function_value);
                });
            }
            Statements::EnumDefineStatement {
                name,
                fields,
                functions,
                public: _,
                span: _,
            } => {
                let name = format!("{}{}", prefix.unwrap_or_default(), name);
                self.scope.set_enum(
                    name.clone(),
                    Enumeration {
                        fields,
                        functions: HashMap::new(),
                        llvm_type: unsafe { LLVMInt8TypeInContext(self.context) },
                    },
                );

                functions.iter().for_each(|(_, function_statement)| {
                    self.compile_statement(
                        function_statement.to_owned(),
                        Some(format!("enum_{name}__")),
                    );

                    self.enter_new_scope();

                    self.compile_statement(
                        function_statement.to_owned(),
                        Some(format!("enum_{name}__")),
                    );

                    let (mut function_id, function_value) =
                        self.scope.stricted_functions().into_iter().last().unwrap();
                    self.exit_scope_raw();

                    function_id = function_id.replace(&format!("struct_{name}__"), "");
                    self.scope
                        .get_mut_enum(&name)
                        .unwrap()
                        .functions
                        .insert(function_id, function_value);
                });
            }
            Statements::TypedefStatement {
                alias,
                datatype,
                span: _,
            } => {
                self.scope.set_typedef(alias, datatype);
            }

            Statements::IfStatement {
                condition,
                then_block,
                else_block,
                span: _,
            } => {
                let condition = self.compile_expression(condition, None);

                let then_basic_block_name = std::ffi::CString::new("__if_then").unwrap();
                let then_basic_block = unsafe { LLVMAppendBasicBlockInContext(self.context, self.function.unwrap(), then_basic_block_name.as_ptr()) };
                let else_basic_block = if else_block.is_some() {
                    let else_basic_block_name = std::ffi::CString::new("__if_else").unwrap();
                    Some(unsafe { LLVMAppendBasicBlockInContext(self.context, self.function.unwrap(), else_basic_block_name.as_ptr()) })
                } else {
                    None
                };
                let after_basic_block_name = std::ffi::CString::new("__if_after").unwrap();
                let after_basic_block = unsafe { LLVMAppendBasicBlockInContext(self.context, self.function.unwrap(), after_basic_block_name.as_ptr()) };

                unsafe { LLVMBuildCondBr(self.builder, condition.1, then_basic_block, else_basic_block.unwrap_or(after_basic_block)) };
                unsafe { LLVMPositionBuilderAtEnd(self.builder, then_basic_block) };

                self.enter_new_scope();

                then_block
                    .into_iter()
                    .for_each(|stmt| self.compile_statement(stmt, None));

                self.exit_scope();
                self.build_branch(after_basic_block);

                if let Some(else_basic_block) = else_basic_block {
                    self.enter_new_scope();

                    unsafe { LLVMPositionBuilderAtEnd(self.builder, else_basic_block) };
                    else_block
                        .unwrap()
                        .into_iter()
                        .for_each(|stmt| self.compile_statement(stmt, None));

                    self.exit_scope();
                    self.build_branch(after_basic_block);
                }

                unsafe { LLVMPositionBuilderAtEnd(self.builder, after_basic_block) };
            }
            Statements::WhileStatement {
                condition,
                block,
                span: _,
            } => {
                let condition_block_name = std::ffi::CString::new("__while_condition").unwrap();
                let condition_block = unsafe { LLVMAppendBasicBlockInContext(self.context, self.function.unwrap(), condition_block_name.as_ptr()) };
                let statements_block_name = std::ffi::CString::new("__while_block").unwrap();
                let statements_block = unsafe { LLVMAppendBasicBlockInContext(self.context, self.function.unwrap(), statements_block_name.as_ptr()) };
                let after_block_name = std::ffi::CString::new("__while_after").unwrap();
                let after_block = unsafe { LLVMAppendBasicBlockInContext(self.context, self.function.unwrap(), after_block_name.as_ptr()) };

                unsafe { LLVMBuildBr(self.builder, condition_block) };
                unsafe { LLVMPositionBuilderAtEnd(self.builder, condition_block) };
                self.breaks.push(after_block);

                let compiled_condition = self.compile_expression(condition, None);

                unsafe { LLVMBuildCondBr(self.builder, compiled_condition.1, statements_block, after_block) };

                self.enter_new_scope();

                unsafe { LLVMPositionBuilderAtEnd(self.builder, statements_block) };
                block
                    .into_iter()
                    .for_each(|statement| self.compile_statement(statement, prefix.clone()));

                self.exit_scope();

                unsafe { LLVMBuildBr(self.builder, condition_block) };
                unsafe { LLVMPositionBuilderAtEnd(self.builder, after_block) };
                let _ = self.breaks.pop();
            }
            Statements::ForStatement {
                binding,
                iterator,
                block,
                span,
            } => {
                // blocks definition
                let iterator_block_name = std::ffi::CString::new("for_iterator").unwrap();
                let iterator_block = unsafe { LLVMAppendBasicBlockInContext(self.context, self.function.unwrap(), iterator_block_name.as_ptr()) };
                let statements_block_name = std::ffi::CString::new("for_block").unwrap();
                let statements_block = unsafe { LLVMAppendBasicBlockInContext(self.context, self.function.unwrap(), statements_block_name.as_ptr()) };
                let after_block_name = std::ffi::CString::new("for_after").unwrap();
                let after_block = unsafe { LLVMAppendBasicBlockInContext(self.context, self.function.unwrap(), after_block_name.as_ptr()) };

                // binding initialization

                let mut compiled_iterator = self
                    .compile_expression(iterator, Some(Type::Pointer(Box::new(Type::Undefined))));

                if let Type::Pointer(ptr_type) = compiled_iterator.0 {
                    compiled_iterator.0 = *ptr_type.clone();

                    if let Type::Array(_, _) = *ptr_type {
                        let load_name = std::ffi::CString::new("").unwrap();
                        compiled_iterator.1 = unsafe { LLVMBuildLoad2(self.builder, LLVMPointerType(LLVMInt8TypeInContext(self.context), 0), compiled_iterator.1, load_name.as_ptr()) };
                    }
                }

                let binding_type = match compiled_iterator.0.clone() {
                    typ if genpay_semantic::Analyzer::is_integer(&typ) => typ,

                    Type::Array(typ, _) => *typ,
                    Type::DynamicArray(typ) => *typ,
                    Type::Alias(alias) => {
                        let iterator_fn = self
                            .scope
                            .get_function(format!("struct_{}__{}", alias, "iterate"))
                            .unwrap();
                        if let Type::Tuple(types) = iterator_fn.datatype {
                            types[0].clone()
                        } else {
                            unreachable!()
                        }
                    }

                    _ => unreachable!(),
                };

                let basic_binding_type = self.get_basic_type(binding_type.clone());
                let binding_name = std::ffi::CString::new(binding.clone()).unwrap();
                let binding_ptr = unsafe { LLVMBuildAlloca(self.builder, basic_binding_type, binding_name.as_ptr()) };

                self.enter_new_scope();

                self.scope.set_variable(
                    binding.clone(),
                    Variable {
                        datatype: binding_type,
                        llvm_type: basic_binding_type,
                        ptr: binding_ptr,
                        no_drop: false,
                        global: false,
                    },
                );
                self.breaks.push(after_block);

                match compiled_iterator.0.clone() {
                    typ if genpay_semantic::Analyzer::is_integer(&typ) => {
                        // runtime checker for negative number
                        if !genpay_semantic::Analyzer::is_unsigned_integer(&typ)
                            && unsafe { LLVMConstIntGetSExtValue(compiled_iterator.1) } < 0
                        {
                            let checker_block_name = std::ffi::CString::new("int_checker").unwrap();
                            let checker_block = unsafe { LLVMInsertBasicBlockInContext(self.context, LLVMGetInsertBlock(self.builder), checker_block_name.as_ptr()) };
                            let err_block_name = std::ffi::CString::new("int_integer_panic").unwrap();
                            let err_block = unsafe { LLVMInsertBasicBlockInContext(self.context, checker_block, err_block_name.as_ptr()) };
                            let ok_block_name = std::ffi::CString::new("int_checker_ok").unwrap();
                            let ok_block = unsafe { LLVMInsertBasicBlockInContext(self.context, err_block, ok_block_name.as_ptr()) };

                            unsafe { LLVMBuildBr(self.builder, checker_block) };
                            unsafe { LLVMPositionBuilderAtEnd(self.builder, checker_block) };

                            let zero = unsafe { LLVMConstNull(LLVMInt64TypeInContext(self.context)) };
                            let cmp_name = std::ffi::CString::new("").unwrap();
                            let cmp = unsafe { LLVMBuildICmp(self.builder, llvm_sys::LLVMIntPredicate::LLVMIntSGE, compiled_iterator.1, zero, cmp_name.as_ptr()) };

                            unsafe { LLVMBuildCondBr(self.builder, cmp, ok_block, err_block) };
                            unsafe { LLVMPositionBuilderAtEnd(self.builder, err_block) };

                            let specifier = self.type_specifier(&typ);
                            self.build_panic(
                                format!("Loop `for` handled negative number: {specifier}"),
                                vec![compiled_iterator.1],
                                self.get_source_line(span.0),
                            );

                            unsafe { LLVMPositionBuilderAtEnd(self.builder, ok_block) };
                        }

                        // initial binding value
                        unsafe { LLVMBuildStore(self.builder, LLVMConstNull(self.get_basic_type(typ.clone())), binding_ptr) };

                        // condition block
                        unsafe { LLVMBuildBr(self.builder, iterator_block) };
                        unsafe { LLVMPositionBuilderAtEnd(self.builder, iterator_block) };

                        let load_name = std::ffi::CString::new("").unwrap();
                        let binding_value = unsafe { LLVMBuildLoad2(self.builder, self.get_basic_type(typ), binding_ptr, load_name.as_ptr()) };
                        let iter_cmp_name = std::ffi::CString::new("").unwrap();
                        let iter_cmp = unsafe { LLVMBuildICmp(self.builder, llvm_sys::LLVMIntPredicate::LLVMIntSLT, binding_value, compiled_iterator.1, iter_cmp_name.as_ptr()) };

                        unsafe { LLVMBuildCondBr(self.builder, iter_cmp, statements_block, after_block) };
                    }

                    Type::Alias(alias) => {
                        // allocating iterator status
                        let status_varname = format!("@genpay_iterator_status_{}", &binding);

                        let iterator_fn = self
                            .scope
                            .get_function(format!("struct_{}__{}", alias, "iterate"))
                            .unwrap();
                        let status_varname_c_string = std::ffi::CString::new(status_varname.clone()).unwrap();
                        let iter_status_alloca = unsafe { LLVMBuildAlloca(self.builder, LLVMInt1TypeInContext(self.context), status_varname_c_string.as_ptr()) };

                        self.scope.set_variable(
                            status_varname,
                            Variable {
                                datatype: Type::Bool,
                                llvm_type: unsafe { LLVMInt1TypeInContext(self.context) },
                                ptr: iter_status_alloca,
                                no_drop: false,
                                global: false,
                            },
                        );

                        let iterator_struct: LLVMValueRef =
                            if self.is_a_pointer_value(compiled_iterator.1) {
                                compiled_iterator.1
                            } else {
                                let alloca_name = std::ffi::CString::new("").unwrap();
                                let alloca = unsafe { LLVMBuildAlloca(self.builder, LLVMTypeOf(compiled_iterator.1), alloca_name.as_ptr()) };
                                unsafe { LLVMBuildStore(self.builder, compiled_iterator.1, alloca) };
                                alloca
                            };

                        // calling iterator function
                        let call_name = std::ffi::CString::new("").unwrap();
                        let output = unsafe { LLVMBuildCall2(self.builder, iterator_fn.fn_type, iterator_fn.value, [iterator_struct].as_mut_ptr(), 1, call_name.as_ptr()) };

                        // allocating result
                        let tuple_alloca_name = std::ffi::CString::new("").unwrap();
                        let tuple_alloca = unsafe { LLVMBuildAlloca(self.builder, LLVMTypeOf(output), tuple_alloca_name.as_ptr()) };
                        unsafe { LLVMBuildStore(self.builder, output, tuple_alloca) };

                        // storing iterator status
                        let status_field_ptr_name = std::ffi::CString::new("").unwrap();
                        let status_field_ptr = unsafe { LLVMBuildStructGEP2(self.builder, LLVMTypeOf(output), tuple_alloca, 1, status_field_ptr_name.as_ptr()) };
                        let status_load_name = std::ffi::CString::new("").unwrap();
                        let status = unsafe { LLVMBuildLoad2(self.builder, LLVMInt1TypeInContext(self.context), status_field_ptr, status_load_name.as_ptr()) };

                        unsafe { LLVMBuildStore(self.builder, status, iter_status_alloca) };

                        // storing binding value
                        let binding_field_ptr_name = std::ffi::CString::new("").unwrap();
                        let binding_field_ptr = unsafe { LLVMBuildStructGEP2(self.builder, LLVMTypeOf(output), tuple_alloca, 0, binding_field_ptr_name.as_ptr()) };
                        let iter_value_load_name = std::ffi::CString::new("").unwrap();
                        let iter_value = unsafe { LLVMBuildLoad2(self.builder, basic_binding_type, binding_field_ptr, iter_value_load_name.as_ptr()) };

                        unsafe { LLVMBuildStore(self.builder, iter_value, binding_ptr) };

                        // condition block
                        unsafe { LLVMBuildBr(self.builder, iterator_block) };
                        unsafe { LLVMPositionBuilderAtEnd(self.builder, iterator_block) };

                        let status_load_name = std::ffi::CString::new("").unwrap();
                        let status = unsafe { LLVMBuildLoad2(self.builder, LLVMInt1TypeInContext(self.context), iter_status_alloca, status_load_name.as_ptr()) };
                        unsafe { LLVMBuildCondBr(self.builder, status, statements_block, after_block) };
                    }
                    Type::Array(arrtype, len) => {
                        // allocating iterator position
                        let iterator_position_varname =
                            format!("@genpay_iterator_position_{}", &binding);
                        let iterator_position_varname_c_string = std::ffi::CString::new(iterator_position_varname.clone()).unwrap();
                        let iterator_position_alloca = unsafe { LLVMBuildAlloca(self.builder, LLVMInt64TypeInContext(self.context), iterator_position_varname_c_string.as_ptr()) };

                        self.scope.set_variable(
                            iterator_position_varname,
                            Variable {
                                datatype: Type::USIZE,
                                llvm_type: unsafe { LLVMInt64TypeInContext(self.context) },
                                ptr: iterator_position_alloca,
                                no_drop: false,
                                global: false,
                            },
                        );

                        // assigning first element
                        let basic_type = self.get_basic_type(*arrtype);
                        let gep_name = std::ffi::CString::new("").unwrap();
                        let ptr = unsafe {
                            LLVMBuildGEP2(
                                self.builder,
                                basic_type,
                                compiled_iterator.1,
                                [LLVMConstInt(LLVMInt64TypeInContext(self.context), 0, 0)].as_mut_ptr(),
                                1,
                                gep_name.as_ptr(),
                            )
                        };

                        let load_name = std::ffi::CString::new("").unwrap();
                        let value = unsafe { LLVMBuildLoad2(self.builder, basic_type, ptr, load_name.as_ptr()) };
                        unsafe { LLVMBuildStore(self.builder, value, binding_ptr) };

                        // condition block
                        unsafe { LLVMBuildBr(self.builder, iterator_block) };
                        unsafe { LLVMPositionBuilderAtEnd(self.builder, iterator_block) };

                        let load_name = std::ffi::CString::new("").unwrap();
                        let iterator_position = unsafe { LLVMBuildLoad2(self.builder, LLVMInt64TypeInContext(self.context), iterator_position_alloca, load_name.as_ptr()) };
                        let array_len_value = unsafe { LLVMConstInt(LLVMInt64TypeInContext(self.context), len as u64, 0) };

                        let cmp_name = std::ffi::CString::new("").unwrap();
                        let cmp = unsafe { LLVMBuildICmp(self.builder, llvm_sys::LLVMIntPredicate::LLVMIntULT, iterator_position, array_len_value, cmp_name.as_ptr()) };

                        unsafe { LLVMBuildCondBr(self.builder, cmp, statements_block, after_block) };
                    }
                    Type::DynamicArray(_) => {}

                    _ => unreachable!(),
                }

                // statements block
                self.enter_new_scope();

                unsafe { LLVMPositionBuilderAtEnd(self.builder, statements_block) };
                block
                    .into_iter()
                    .for_each(|statement| self.compile_statement(statement, prefix.clone()));

                self.exit_scope();

                // making iteration
                match compiled_iterator.0 {
                    typ if genpay_semantic::Analyzer::is_integer(&typ) => {
                        let load_name = std::ffi::CString::new("itertmp").unwrap();
                        let current_value = unsafe { LLVMBuildLoad2(self.builder, basic_binding_type, binding_ptr, load_name.as_ptr()) };
                        let add_name = std::ffi::CString::new("iterinc").unwrap();
                        let incremented_value = unsafe { LLVMBuildAdd(self.builder, current_value, LLVMConstInt(basic_binding_type, 1, 0), add_name.as_ptr()) };

                        unsafe { LLVMBuildStore(self.builder, incremented_value, binding_ptr) };
                    }

                    Type::Alias(alias) => {
                        let status_varname = format!("@genpay_iterator_status_{}", &binding);
                        let status_alloca = self.scope.get_variable(status_varname).unwrap().ptr;

                        let iterator_fn = self
                            .scope
                            .get_function(format!("struct_{}__{}", alias, "iterate"))
                            .unwrap();
                        let iterator_struct: LLVMValueRef =
                            if self.is_a_pointer_value(compiled_iterator.1) {
                                compiled_iterator.1
                            } else {
                                let alloca_name = std::ffi::CString::new("").unwrap();
                                let alloca = unsafe { LLVMBuildAlloca(self.builder, LLVMTypeOf(compiled_iterator.1), alloca_name.as_ptr()) };
                                unsafe { LLVMBuildStore(self.builder, compiled_iterator.1, alloca) };
                                alloca
                            };

                        // calling iterator function
                        let call_name = std::ffi::CString::new("").unwrap();
                        let output = unsafe { LLVMBuildCall2(self.builder, iterator_fn.fn_type, iterator_fn.value, [iterator_struct].as_mut_ptr(), 1, call_name.as_ptr()) };

                        // allocating result
                        let tuple_alloca_name = std::ffi::CString::new("").unwrap();
                        let tuple_alloca = unsafe { LLVMBuildAlloca(self.builder, LLVMTypeOf(output), tuple_alloca_name.as_ptr()) };
                        unsafe { LLVMBuildStore(self.builder, output, tuple_alloca) };

                        // storing iterator status
                        let status_field_ptr_name = std::ffi::CString::new("").unwrap();
                        let status_field_ptr = unsafe { LLVMBuildStructGEP2(self.builder, LLVMTypeOf(output), tuple_alloca, 1, status_field_ptr_name.as_ptr()) };
                        let status_load_name = std::ffi::CString::new("").unwrap();
                        let status = unsafe { LLVMBuildLoad2(self.builder, LLVMInt1TypeInContext(self.context), status_field_ptr, status_load_name.as_ptr()) };

                        unsafe { LLVMBuildStore(self.builder, status, status_alloca) };

                        // storing binding value
                        let binding_field_ptr_name = std::ffi::CString::new("").unwrap();
                        let binding_field_ptr = unsafe { LLVMBuildStructGEP2(self.builder, LLVMTypeOf(output), tuple_alloca, 0, binding_field_ptr_name.as_ptr()) };
                        let iter_value_load_name = std::ffi::CString::new("").unwrap();
                        let iter_value = unsafe { LLVMBuildLoad2(self.builder, basic_binding_type, binding_field_ptr, iter_value_load_name.as_ptr()) };

                        unsafe { LLVMBuildStore(self.builder, iter_value, binding_ptr) };
                    }
                    Type::Array(arrtype, _) => {
                        // incrementing iterator position
                        let iterator_position_varname =
                            format!("@genpay_iterator_position_{}", &binding);
                        let iterator_position_ptr = self
                            .scope
                            .get_variable(iterator_position_varname)
                            .unwrap()
                            .ptr;

                        let load_name = std::ffi::CString::new("").unwrap();
                        let value = unsafe { LLVMBuildLoad2(self.builder, LLVMInt64TypeInContext(self.context), iterator_position_ptr, load_name.as_ptr()) };
                        let add_name = std::ffi::CString::new("").unwrap();
                        let incremented_value = unsafe { LLVMBuildAdd(self.builder, value, LLVMConstInt(LLVMInt64TypeInContext(self.context), 1, 0), add_name.as_ptr()) };

                        unsafe { LLVMBuildStore(self.builder, incremented_value, iterator_position_ptr) };

                        // storing value to the binding
                        let basic_type = self.get_basic_type(*arrtype);
                        let gep_name = std::ffi::CString::new("").unwrap();
                        let ptr = unsafe {
                            LLVMBuildGEP2(
                                self.builder,
                                basic_type,
                                compiled_iterator.1,
                                [incremented_value].as_mut_ptr(),
                                1,
                                gep_name.as_ptr(),
                            )
                        };

                        let load_name = std::ffi::CString::new("").unwrap();
                        let value = unsafe { LLVMBuildLoad2(self.builder, basic_type, ptr, load_name.as_ptr()) };
                        unsafe { LLVMBuildStore(self.builder, value, binding_ptr) };
                    }
                    Type::DynamicArray(_) => {}

                    _ => unreachable!(),
                }

                self.build_branch(iterator_block);

                // exit
                unsafe { LLVMPositionBuilderAtEnd(self.builder, after_block) };
                let _ = self.breaks.pop();

                self.exit_scope();
            }

            Statements::BreakStatements { span: _ } => {
                let break_block = self.breaks.last().cloned().unwrap();

                unsafe { LLVMBuildBr(self.builder, break_block) };
            }
            Statements::ReturnStatement { value, span: _ } => {
                let compiled_value = self.compile_expression(value, Some(Type::NoDrop));
                if compiled_value.0 != Type::Void {
                    unsafe { LLVMBuildRet(self.builder, compiled_value.1) };
                } else {
                    unsafe { LLVMBuildRetVoid(self.builder) };
                }
            }

            Statements::LinkCStatement { path: _, span: _ } => {}

            Statements::ExternDeclareStatement {
                identifier,
                datatype,
                span: _,
            } => {
                let basic_type = self.get_basic_type(datatype.clone());
                let id_c_string = std::ffi::CString::new(identifier.clone()).unwrap();
                let global = unsafe { LLVMAddGlobal(self.module, basic_type, id_c_string.as_ptr()) };
                unsafe { LLVMSetLinkage(global, llvm_sys::LLVMLinkage::LLVMExternalLinkage) };
                unsafe { LLVMSetAlignment(global, 8) };

                self.scope.set_variable(
                    identifier,
                    Variable {
                        datatype,
                        llvm_type: basic_type,
                        ptr: unsafe { LLVMConstNull(LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)) },
                        no_drop: true,
                        global: true,
                    },
                );
            }

            Statements::ExternStatement {
                identifier,
                arguments,
                return_type,
                extern_type: _,
                is_var_args,
                public: _,
                span: _,
            } => {
                if self.scope.get_function(&identifier).is_some() {
                    return;
                }

                let mut basic_arguments = arguments
                    .iter()
                    .map(|arg| self.get_basic_type(arg.clone()))
                    .collect::<Vec<LLVMTypeRef>>();

                let fn_type = self.get_fn_type(return_type.clone(), &mut basic_arguments, is_var_args);
                let id_c_string = std::ffi::CString::new(identifier.clone()).unwrap();
                let fn_value = unsafe {
                    let function = LLVMGetNamedFunction(self.module, id_c_string.as_ptr());
                    if function == std::ptr::null_mut() {
                        LLVMAddFunction(self.module, id_c_string.as_ptr(), fn_type)
                    } else {
                        function
                    }
                };

                // little hack cuz i dont wanna add `is_var_args` argument to function structure
                // and fix the whole code for the only one usage

                let mut arguments = arguments;
                arguments.push(Type::Void);
                arguments.push(Type::Void);

                // so if we have 2 void arguments at the end (which is impossible in basic code) - this is var args

                self.scope.set_function(
                    identifier,
                    Function {
                        datatype: return_type,
                        fn_type,
                        value: fn_value,
                        arguments,
                        called: false,
                    },
                )
            }

            Statements::ImportStatement { path, span: _ } => {
                let path = if let Expressions::Value(Value::String(path), _) = path {
                    path
                } else {
                    String::default()
                };
                let fname = std::path::Path::new(&path)
                    .file_name()
                    .map(|fname| fname.to_str().unwrap_or("$NONE"))
                    .unwrap();

                let module_name = fname
                    .split(".")
                    .nth(0)
                    .map(|n| n.to_string())
                    .unwrap_or(fname.replace(".dn", ""));

                let import = self
                    .symtable
                    .imports
                    .get(&module_name)
                    .cloned()
                    .unwrap_or_default();
                let mut codegen = Self::new(
                    self.context,
                    &module_name,
                    &import.source,
                    import.embedded_symtable.clone(),
                );

                let (_module, mut module_content) = codegen.compile(import.ast.clone(), None);

                module_content.functions.iter_mut().for_each(|(func_name, func)| {
                    let args_fmt = func
                        .arguments
                        .iter()
                        .map(|typ| typ.to_string())
                        .collect::<Vec<String>>();
                    let mut args = func
                        .arguments
                        .iter()
                        .map(|typ| self.get_basic_type(typ.clone()))
                        .collect::<Vec<LLVMTypeRef>>();
                    let fn_type = self.get_fn_type(func.datatype.clone(), &mut args, false);

                    let function_name = format!("{}.{}({})", &module_name, func_name, args_fmt.join(", "));
                    let function_name_c_string = std::ffi::CString::new(function_name).unwrap();
                    let declared_fn = unsafe { LLVMAddFunction(self.module, function_name_c_string.as_ptr(), fn_type) };
                    unsafe { LLVMSetLinkage(declared_fn, llvm_sys::LLVMLinkage::LLVMExternalLinkage) };
                    func.value = declared_fn;
                });

                self.imports.insert(module_name, module_content);
            }

            Statements::IncludeStatement { path, span: _ } => {
                let path = if let Expressions::Value(Value::String(path), _) = path {
                    path.replace("@", "")
                } else {
                    String::default()
                };

                let fname = std::path::Path::new(&path)
                    .file_name()
                    .map(|fname| fname.to_str().unwrap_or("$NONE"))
                    .unwrap();

                let module_name = fname
                    .split(".")
                    .nth(0)
                    .map(|n| n.to_string())
                    .unwrap_or(fname.replace(".dn", ""));

                if self.includes.contains(&module_name) {
                    return;
                };
                self.includes.push(module_name.clone());

                let include = self.symtable.included.get(&module_name).unwrap().clone();
                include
                    .ast
                    .iter()
                    .for_each(|stmt| self.compile_statement(stmt.clone(), None));
            }

            Statements::ScopeStatement { block, span: _ } => {
                self.enter_new_scope();
                block
                    .iter()
                    .for_each(|stmt| self.compile_statement(stmt.clone(), None));

                self.exit_scope();
            }

            Statements::Expression(expr) => {
                let _ = self.compile_expression(expr, None);
            }
            Statements::None => unreachable!(),
        }
    }

    fn compile_expression(
        &mut self,
        expression: Expressions,
        expected: Option<Type>,
    ) -> (Type, LLVMValueRef) {
        match expression {
            Expressions::Value(val, _) => self.compile_value(val, expected),
            Expressions::FnCall {
                name,
                arguments,
                span: _,
            } => {
                {
                    let mut_fn = self.scope.get_mut_function(&name).unwrap();
                    mut_fn.called = true
                }

                let function = self.scope.get_function(&name).unwrap();
                let mut args: Vec<LLVMValueRef> = Vec::new();
                arguments
                    .iter()
                    .zip(function.arguments)
                    .for_each(|(arg, fn_expected)| {
                        args.push(
                            self.compile_expression(arg.clone(), Some(fn_expected))
                                .1,
                        )
                    });

                let mut return_type = function.datatype;

                if let Type::Pointer(ref return_ptr_type) = return_type {
                    if let (Some(Type::Pointer(expected_ptr_type)), true) =
                        (expected.clone(), **return_ptr_type == Type::Void)
                    {
                        return_type = Type::Pointer(expected_ptr_type);
                    }
                }

                if genpay_semantic::Analyzer::is_integer(&return_type)
                    && (genpay_semantic::Analyzer::is_integer(
                        expected.as_ref().unwrap_or(&Type::Void),
                    ) || matches!(expected.as_ref().unwrap_or(&Type::Void), Type::Char))
                    && genpay_semantic::Analyzer::integer_order(expected.as_ref().unwrap())
                        <= genpay_semantic::Analyzer::integer_order(&return_type)
                {
                    return_type = expected.unwrap();
                }

                let call_name = std::ffi::CString::new("").unwrap();
                (
                    return_type,
                    unsafe {
                        LLVMBuildCall2(
                            self.builder,
                            function.fn_type,
                            function.value,
                            args.as_mut_ptr(),
                            args.len() as u32,
                            call_name.as_ptr(),
                        )
                    },
                )
            }

            Expressions::MacroCall {
                name,
                arguments,
                span: _,
            } => self.build_macro_call(&name, arguments),

            Expressions::Reference { object, span: _ } => match *object {
                Expressions::Value(Value::Identifier(id), _) => {
                    let var = self.scope.get_variable(&id).unwrap();

                    (
                        Type::Pointer(Box::new(var.datatype.clone())),
                        var.ptr,
                    )
                }
                _ => {
                    let value = self.compile_expression(
                        *object,
                        Some(Type::Pointer(Box::new(Type::Undefined))),
                    );
                    let alloca_name = std::ffi::CString::new("").unwrap();
                    let alloca = unsafe { LLVMBuildAlloca(self.builder, LLVMTypeOf(value.1), alloca_name.as_ptr()) };
                    unsafe { LLVMBuildStore(self.builder, value.1, alloca) };

                    (Type::Pointer(Box::new(value.0)), alloca)
                }
            },
            Expressions::Dereference { object, span: _ } => {
                let (datatype, ptr) = self.compile_expression(*object.clone(), None);

                match datatype {
                    Type::Pointer(ptr_type) => {
                        let basic_type = self.get_basic_type(*ptr_type.clone());
                        let load_name = std::ffi::CString::new("").unwrap();
                        let value = unsafe { LLVMBuildLoad2(self.builder, basic_type, ptr, load_name.as_ptr()) };

                        (*ptr_type, value)
                    }

                    Type::Alias(alias) => {
                        let ptr = self
                            .compile_expression(
                                *object,
                                Some(Type::Pointer(Box::new(Type::Undefined))),
                            )
                            .1;

                        let struct_type = self.scope.get_struct(alias).unwrap();
                        let deref_fn = struct_type.functions.get("deref").unwrap();

                        // calling deref struct
                        let call_name = std::ffi::CString::new("@genpay_deref_call").unwrap();
                        let call_result = unsafe {
                            LLVMBuildCall2(
                                self.builder,
                                deref_fn.fn_type,
                                deref_fn.value,
                                [ptr].as_mut_ptr(),
                                1,
                                call_name.as_ptr(),
                            )
                        };

                        (deref_fn.datatype.clone(), call_result)
                    }

                    _ => {
                        panic!(
                            "Semantical Analyzer missed dereference expression bug:\n- `{datatype}`:\n{ptr:#?}"
                        )
                    }
                }
            }

            Expressions::Unary {
                operand,
                object,
                span: _,
            } => {
                let object_value = self.compile_expression(*object, expected);

                if let Type::Alias(alias) = &object_value.0 {
                    let structure = self.scope.get_struct(alias).unwrap();
                    let unary_function = structure.functions.get("unary").unwrap();

                    let object_ptr = {
                        let alloca_name = std::ffi::CString::new("").unwrap();
                        let alloca = unsafe { LLVMBuildAlloca(self.builder, LLVMTypeOf(object_value.1), alloca_name.as_ptr()) };
                        unsafe { LLVMBuildStore(self.builder, object_value.1, alloca) };
                        alloca
                    };

                    let operand_c_string = std::ffi::CString::new(operand).unwrap();
                    let operand_global_name = std::ffi::CString::new("@genpay_operand").unwrap();
                    let operand_value = unsafe { LLVMBuildGlobalStringPtr(self.builder, operand_c_string.as_ptr(), operand_global_name.as_ptr()) };

                    let call_name = std::ffi::CString::new("@genpay_unop_call").unwrap();
                    let function_output = unsafe {
                        LLVMBuildCall2(
                            self.builder,
                            unary_function.fn_type,
                            unary_function.value,
                            [object_ptr, operand_value].as_mut_ptr(),
                            2,
                            call_name.as_ptr(),
                        )
                    };

                    return (unary_function.datatype.clone(), function_output);
                }

                match operand.as_str() {
                    "-" => match object_value.0 {
                        Type::I8
                        | Type::I16
                        | Type::I32
                        | Type::I64
                        | Type::U8
                        | Type::U16
                        | Type::U32
                        | Type::U64
                        | Type::USIZE => (
                            genpay_semantic::Analyzer::unsigned_to_signed_integer(&object_value.0),
                            unsafe { LLVMBuildNeg(self.builder, object_value.1, std::ffi::CString::new("").unwrap().as_ptr()) },
                        ),

                        Type::F32 | Type::F64 => (
                            object_value.0,
                            unsafe { LLVMBuildFNeg(self.builder, object_value.1, std::ffi::CString::new("").unwrap().as_ptr()) },
                        ),

                        _ => unreachable!(),
                    },

                    "!" => (
                        object_value.0,
                        unsafe { LLVMBuildNot(self.builder, object_value.1, std::ffi::CString::new("").unwrap().as_ptr()) },
                    ),

                    _ => unreachable!(),
                }
            }
            Expressions::Binary {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                let expected = expected.and_then(|typ| {
                    if let Type::Pointer(_) = typ {
                        None
                    } else {
                        Some(typ)
                    }
                });

                let lhs_value = self.compile_expression(*lhs.clone(), expected.clone());
                let rhs_value = self.compile_expression(*rhs.clone(), expected.clone());

                let senior_type = match lhs_value.0.clone() {
                    typ if genpay_semantic::Analyzer::is_integer(&typ) => {
                        if genpay_semantic::Analyzer::integer_order(&lhs_value.0)
                            > genpay_semantic::Analyzer::integer_order(&rhs_value.0)
                        {
                            lhs_value.0
                        } else {
                            rhs_value.0.clone()
                        }
                    }
                    typ if genpay_semantic::Analyzer::is_float(&typ) => {
                        if genpay_semantic::Analyzer::float_order(&lhs_value.0)
                            > genpay_semantic::Analyzer::float_order(&rhs_value.0)
                        {
                            lhs_value.0
                        } else {
                            rhs_value.0.clone()
                        }
                    }

                    typ if matches!((&typ, &rhs_value.0), (Type::Pointer(_), Type::Pointer(_))) => {
                        Type::USIZE
                    }
                    Type::Pointer(_) => lhs_value.0,

                    Type::Alias(alias) => Type::Alias(alias),

                    _ => panic!("Unreachable type found: {}", lhs_value.0.clone()),
                };

                let output = match senior_type.clone() {
                    typ if genpay_semantic::Analyzer::is_integer(&typ) => match operand.as_str() {
                        "+" => {
                            let name = std::ffi::CString::new("").unwrap();
                            if genpay_semantic::Analyzer::is_unsigned_integer(&typ) {
                                unsafe { LLVMBuildNSWAdd(self.builder, lhs_value.1, rhs_value.1, name.as_ptr()) }
                            } else {
                                unsafe { LLVMBuildAdd(self.builder, lhs_value.1, rhs_value.1, name.as_ptr()) }
                            }
                        }
                        "-" => {
                            let name = std::ffi::CString::new("").unwrap();
                            if genpay_semantic::Analyzer::is_unsigned_integer(&typ) {
                                unsafe { LLVMBuildNSWSub(self.builder, lhs_value.1, rhs_value.1, name.as_ptr()) }
                            } else {
                                unsafe { LLVMBuildSub(self.builder, lhs_value.1, rhs_value.1, name.as_ptr()) }
                            }
                        }
                        "*" => {
                            let name = std::ffi::CString::new("").unwrap();
                            if genpay_semantic::Analyzer::is_unsigned_integer(&typ) {
                                unsafe { LLVMBuildNSWMul(self.builder, lhs_value.1, rhs_value.1, name.as_ptr()) }
                            } else {
                                unsafe { LLVMBuildMul(self.builder, lhs_value.1, rhs_value.1, name.as_ptr()) }
                            }
                        }
                        "/" => {
                            let name = std::ffi::CString::new("").unwrap();
                            if genpay_semantic::Analyzer::is_unsigned_integer(&typ) {
                                unsafe { LLVMBuildUDiv(self.builder, lhs_value.1, rhs_value.1, name.as_ptr()) }
                            } else {
                                unsafe { LLVMBuildSDiv(self.builder, lhs_value.1, rhs_value.1, name.as_ptr()) }
                            }
                        }
                        "%" => {
                            let name = std::ffi::CString::new("").unwrap();
                            if genpay_semantic::Analyzer::is_unsigned_integer(&typ) {
                                unsafe { LLVMBuildURem(self.builder, lhs_value.1, rhs_value.1, name.as_ptr()) }
                            } else {
                                unsafe { LLVMBuildSRem(self.builder, lhs_value.1, rhs_value.1, name.as_ptr()) }
                            }
                        }

                        _ => panic!(
                            "Unsupported for codegen operator found! Please open issue on Github!"
                        ),
                    },
                    typ if genpay_semantic::Analyzer::is_float(&typ) => {
                        let name = std::ffi::CString::new("").unwrap();
                        match operand.as_str() {
                        "+" => unsafe { LLVMBuildFAdd(self.builder, lhs_value.1, rhs_value.1, name.as_ptr()) },
                        "-" => unsafe { LLVMBuildFSub(self.builder, lhs_value.1, rhs_value.1, name.as_ptr()) },
                        "*" => unsafe { LLVMBuildFMul(self.builder, lhs_value.1, rhs_value.1, name.as_ptr()) },
                        "/" => unsafe { LLVMBuildFDiv(self.builder, lhs_value.1, rhs_value.1, name.as_ptr()) },
                        _ => unreachable!(),
                    }},

                    Type::Pointer(ptr_type) => {
                        if matches!(rhs_value.0, Type::Pointer(_)) {
                            let name = std::ffi::CString::new("").unwrap();
                            let lhs_int = unsafe { LLVMBuildPtrToInt(self.builder, lhs_value.1, LLVMInt64TypeInContext(self.context), name.as_ptr()) };
                            let rhs_int = unsafe { LLVMBuildPtrToInt(self.builder, rhs_value.1, LLVMInt64TypeInContext(self.context), name.as_ptr()) };

                            let mut value = match operand.as_str() {
                                "+" => unsafe { LLVMBuildAdd(self.builder, lhs_int, rhs_int, name.as_ptr()) },
                                "-" => unsafe { LLVMBuildSub(self.builder, lhs_int, rhs_int, name.as_ptr()) },
                                _ => unreachable!(),
                            };

                            if matches!(expected, Some(Type::Pointer(_))) {
                                value = unsafe { LLVMBuildIntToPtr(self.builder, value, LLVMTypeOf(lhs_value.1), name.as_ptr()) };
                            }

                            value
                        } else {
                            let name = std::ffi::CString::new("").unwrap();
                            unsafe {
                                LLVMBuildInBoundsGEP2(
                                    self.builder,
                                    self.get_basic_type(*ptr_type.clone()),
                                    lhs_value.1,
                                    [rhs_value.1].as_mut_ptr(),
                                    1,
                                    name.as_ptr(),
                                )
                            }
                        }
                    }

                    Type::Alias(alias) => {
                        let structure = self.scope.get_struct(alias).unwrap();
                        let binary_function = structure.functions.get("binary").unwrap();

                        let left_ptr = self
                            .compile_expression(
                                *lhs.clone(),
                                Some(Type::Pointer(Box::new(Type::Undefined))),
                            )
                            .1;
                        let right_ptr = self
                            .compile_expression(
                                *rhs.clone(),
                                Some(Type::Pointer(Box::new(Type::Undefined))),
                            )
                            .1;

                        let operand_c_string = std::ffi::CString::new(operand).unwrap();
                        let operand_global_name = std::ffi::CString::new("@genpay_operand").unwrap();
                        let operand_value = unsafe { LLVMBuildGlobalStringPtr(self.builder, operand_c_string.as_ptr(), operand_global_name.as_ptr()) };

                        let call_name = std::ffi::CString::new("@genpay_binop_call").unwrap();
                        unsafe {
                            LLVMBuildCall2(
                                self.builder,
                                binary_function.fn_type,
                                binary_function.value,
                                [left_ptr, right_ptr, operand_value].as_mut_ptr(),
                                3,
                                call_name.as_ptr(),
                            )
                        }
                    }

                    _ => unreachable!(),
                };

                (senior_type, output)
            }
            Expressions::Boolean {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                let mut lhs_value = self.compile_expression(*lhs.clone(), expected.clone());
                let mut rhs_value =
                    self.compile_expression(*rhs.clone(), Some(lhs_value.0.clone()));

                if let Type::Alias(left) = &lhs_value.0
                    && let Type::Alias(right) = &rhs_value.0
                    && self.scope.get_enum(left).is_some()
                    && self.scope.get_enum(right).is_some()
                {
                    lhs_value.0 = Type::U8;
                    rhs_value.0 = Type::U8;
                }

                match operand.as_str() {
                    "&&" => {
                        let name = std::ffi::CString::new("").unwrap();
                        return (
                            Type::Bool,
                            unsafe { LLVMBuildAnd(self.builder, lhs_value.1, rhs_value.1, name.as_ptr()) },
                        );
                    }
                    "||" => {
                        let name = std::ffi::CString::new("").unwrap();
                        return (
                            Type::Bool,
                            unsafe { LLVMBuildOr(self.builder, lhs_value.1, rhs_value.1, name.as_ptr()) },
                        );
                    }
                    _ => {}
                }

                match lhs_value.0 {
                    typ if typ == Type::Null || rhs_value.0 == Type::Null => {
                        let leading_value = if typ == Type::Null {
                            rhs_value.1
                        } else {
                            lhs_value.1
                        };
                        let name = std::ffi::CString::new("").unwrap();
                        let result_value = if operand == "==" {
                            unsafe { LLVMBuildIsNull(self.builder, leading_value, name.as_ptr()) }
                        } else {
                            unsafe { LLVMBuildIsNotNull(self.builder, leading_value, name.as_ptr()) }
                        };

                        (Type::Bool, result_value)
                    }

                    typ if genpay_semantic::Analyzer::is_integer(&typ) || typ == Type::Char => {
                        let predicate = match operand.as_str() {
                            ">" => llvm_sys::LLVMIntPredicate::LLVMIntSGT,
                            "<" => llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                            "<=" | "=<" => llvm_sys::LLVMIntPredicate::LLVMIntSLE,
                            ">=" | "=>" => llvm_sys::LLVMIntPredicate::LLVMIntSGE,
                            "==" => llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                            "!=" => llvm_sys::LLVMIntPredicate::LLVMIntNE,
                            _ => unreachable!(),
                        };

                        let name = std::ffi::CString::new("").unwrap();
                        (
                            Type::Bool,
                            unsafe { LLVMBuildICmp(self.builder, predicate, lhs_value.1, rhs_value.1, name.as_ptr()) },
                        )
                    }

                    typ if genpay_semantic::Analyzer::is_float(&typ) => {
                        let predicate = match operand.as_str() {
                            ">" => llvm_sys::LLVMRealPredicate::LLVMRealOGT,
                            "<" => llvm_sys::LLVMRealPredicate::LLVMRealOLT,
                            "<=" | "=<" => llvm_sys::LLVMRealPredicate::LLVMRealOLE,
                            ">=" | "=>" => llvm_sys::LLVMRealPredicate::LLVMRealOGE,
                            "==" => llvm_sys::LLVMRealPredicate::LLVMRealOEQ,
                            "!=" => llvm_sys::LLVMRealPredicate::LLVMRealONE,
                            _ => unreachable!(),
                        };

                        let name = std::ffi::CString::new("").unwrap();
                        (
                            Type::Bool,
                            unsafe { LLVMBuildFCmp(self.builder, predicate, lhs_value.1, rhs_value.1, name.as_ptr()) },
                        )
                    }
                    Type::Pointer(ptr_type) if *ptr_type == Type::Char => {
                        let strcmp_name = std::ffi::CString::new("strcmp").unwrap();
                        let strcmp_fn = unsafe {
                            let function = LLVMGetNamedFunction(self.module, strcmp_name.as_ptr());
                            if function == std::ptr::null_mut() {
                                let fn_type = LLVMFunctionType(LLVMInt32TypeInContext(self.context), [LLVMPointerType(LLVMInt8TypeInContext(self.context), 0), LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)].as_mut_ptr(), 2, 0);
                                LLVMAddFunction(self.module, strcmp_name.as_ptr(), fn_type)
                            } else {
                                function
                            }
                        };

                        let call_name = std::ffi::CString::new("").unwrap();
                        let cmp_value = unsafe { LLVMBuildCall2(self.builder, LLVMGetCalledFunctionType(strcmp_fn), strcmp_fn, [lhs_value.1, rhs_value.1].as_mut_ptr(), 2, call_name.as_ptr()) };

                        let int_predicate = match operand.as_str() {
                            ">" => llvm_sys::LLVMIntPredicate::LLVMIntSGT,
                            "<" => llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                            "<=" | "=<" => llvm_sys::LLVMIntPredicate::LLVMIntSLE,
                            ">=" | "=>" => llvm_sys::LLVMIntPredicate::LLVMIntSGE,
                            "==" => llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                            "!=" => llvm_sys::LLVMIntPredicate::LLVMIntNE,
                            _ => unreachable!(),
                        };

                        let name = std::ffi::CString::new("").unwrap();
                        (
                            Type::Bool,
                            unsafe { LLVMBuildICmp(self.builder, int_predicate, cmp_value, LLVMConstNull(LLVMInt32TypeInContext(self.context)), name.as_ptr()) },
                        )
                    }

                    Type::Alias(alias) => {
                        // calling compare function
                        let structure = self.scope.get_struct(alias).unwrap();
                        let compare_function = structure.functions.get("compare").unwrap();

                        let left_ptr = self
                            .compile_expression(
                                *lhs.clone(),
                                Some(Type::Pointer(Box::new(Type::Undefined))),
                            )
                            .1;
                        let right_ptr = self
                            .compile_expression(
                                *rhs.clone(),
                                Some(Type::Pointer(Box::new(Type::Undefined))),
                            )
                            .1;

                        let call_name = std::ffi::CString::new("@genpay_cmp_call").unwrap();
                        let function_output = unsafe {
                            LLVMBuildCall2(
                                self.builder,
                                compare_function.fn_type,
                                compare_function.value,
                                [left_ptr, right_ptr].as_mut_ptr(),
                                2,
                                call_name.as_ptr(),
                            )
                        };

                        // comparing
                        let int_predicate = match operand.as_str() {
                            ">" => llvm_sys::LLVMIntPredicate::LLVMIntSGT,
                            "<" => llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                            "<=" | "=<" => llvm_sys::LLVMIntPredicate::LLVMIntSLE,
                            ">=" | "=>" => llvm_sys::LLVMIntPredicate::LLVMIntSGE,
                            "==" => llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                            "!=" => llvm_sys::LLVMIntPredicate::LLVMIntNE,
                            _ => unreachable!(),
                        };

                        let name = std::ffi::CString::new("").unwrap();
                        (
                            Type::Bool,
                            unsafe { LLVMBuildICmp(self.builder, int_predicate, LLVMConstNull(LLVMInt32TypeInContext(self.context)), function_output, name.as_ptr()) },
                        )
                    }

                    Type::Bool => {
                        let name = std::ffi::CString::new("").unwrap();
                        (
                            Type::Bool,
                            unsafe { LLVMBuildAnd(self.builder, lhs_value.1, rhs_value.1, name.as_ptr()) },
                        )
                    },

                    _ => panic!("Boolean catched: {} ? {}", lhs_value.0, rhs_value.0),
                }
            }
            Expressions::Bitwise {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                let left = self.compile_expression(*lhs, expected.clone());
                let right = self.compile_expression(*rhs, expected.clone());

                let _sign_extend = if genpay_semantic::Analyzer::is_unsigned_integer(&left.0) { 1 } else { 0 };
                let name = std::ffi::CString::new("").unwrap();
                let basic_value = match operand.as_str() {
                    "<<" => unsafe { LLVMBuildShl(self.builder, left.1, right.1, name.as_ptr()) },
                    ">>" => unsafe { LLVMBuildAShr(self.builder, left.1, right.1, name.as_ptr()) },
                    "&" => unsafe { LLVMBuildAnd(self.builder, left.1, right.1, name.as_ptr()) },
                    "|" => unsafe { LLVMBuildOr(self.builder, left.1, right.1, name.as_ptr()) },
                    "^" => unsafe { LLVMBuildXor(self.builder, left.1, right.1, name.as_ptr()) },
                    _ => unreachable!(),
                };

                (left.0, basic_value)
            }

            Expressions::SubElement {
                head,
                subelements,
                span: _,
            } => {
                let compiled_head =
                    self.compile_expression(*head, Some(Type::Pointer(Box::new(Type::Undefined))));

                let mut prev_val = compiled_head.1;
                let mut prev_type = compiled_head.0;

                // I know it looks kinda awful and terrible, but it works.
                // There's no way you can get double pointer to a struct in sub-element, so i just
                // placed self pointers into this shit.
                if let Type::Pointer(ptr_type) = prev_type.clone() {
                    if let Type::Pointer(ptr_type) = *ptr_type.clone() {
                        prev_type = *ptr_type;
                        let load_name = std::ffi::CString::new("").unwrap();
                        prev_val = unsafe { LLVMBuildLoad2(self.builder, LLVMPointerType(LLVMInt8TypeInContext(self.context), 0), prev_val, load_name.as_ptr()) };
                    }
                }

                subelements.iter().for_each(|sub| match sub {
                    Expressions::Value(Value::Identifier(field), _) => {
                        if let Type::Pointer(ptr_type) = prev_type.clone() {
                            prev_type = *ptr_type;
                        }

                        if let Type::Alias(alias) = prev_type.clone() {
                            let alias_type = self.get_alias_type(prev_type.clone(), None).unwrap();

                            match alias_type {
                                "struct" => {
                                    let structure = self.scope.get_struct(&alias).unwrap();
                                    let field = structure.fields.get(field).unwrap();

                                    let name = std::ffi::CString::new("").unwrap();
                                    let ptr = unsafe { LLVMBuildStructGEP2(self.builder, structure.llvm_type, prev_val, field.nth, name.as_ptr()) };

                                    let value = if let Some(Type::Pointer(ptr_type)) =
                                        expected.clone()
                                    {
                                        if *ptr_type == Type::Undefined {
                                            ptr
                                        } else {
                                            let load_name = std::ffi::CString::new("").unwrap();
                                            unsafe { LLVMBuildLoad2(self.builder, field.llvm_type, ptr, load_name.as_ptr()) }
                                        }
                                    } else {
                                        let load_name = std::ffi::CString::new("").unwrap();
                                        unsafe { LLVMBuildLoad2(self.builder, field.llvm_type, ptr, load_name.as_ptr()) }
                                    };

                                    prev_type = field.datatype.clone();
                                    prev_val = value;
                                }
                                "enum" => {
                                    let enumeration = self.scope.get_enum(&alias).unwrap();
                                    let idx =
                                        enumeration.fields.iter().position(|f| f == field).unwrap();
                                    let idx_value =
                                        unsafe { LLVMConstInt(LLVMInt8TypeInContext(self.context), idx as u64, 0) };

                                    prev_val = idx_value;
                                }

                                _ => unreachable!(),
                            }
                        }
                    }

                    Expressions::Value(Value::Integer(idx), _) => match prev_type.clone() {
                        Type::Tuple(types) => {
                            let field_type = types[*idx as usize].clone();
                            let field_basic_type = self.get_basic_type(field_type.clone());

                            let mut basic_types = types
                                .into_iter()
                                .map(|typ| self.get_basic_type(typ))
                                .collect::<Vec<LLVMTypeRef>>();
                            let tuple_type = unsafe { LLVMStructTypeInContext(self.context, basic_types.as_mut_ptr(), basic_types.len() as u32, 0) };

                            let name = std::ffi::CString::new("").unwrap();
                            let ptr = unsafe { LLVMBuildStructGEP2(self.builder, tuple_type, prev_val, *idx as u32, name.as_ptr()) };

                            let value = if let Some(Type::Pointer(ptr_type)) = expected.clone() {
                                if *ptr_type == Type::Undefined {
                                    ptr
                                } else {
                                    let load_name = std::ffi::CString::new("").unwrap();
                                    unsafe { LLVMBuildLoad2(self.builder, field_basic_type, ptr, load_name.as_ptr()) }
                                }
                            } else {
                                let load_name = std::ffi::CString::new("").unwrap();
                                unsafe { LLVMBuildLoad2(self.builder, field_basic_type, ptr, load_name.as_ptr()) }
                            };

                            prev_type = field_type;
                            prev_val = value;
                        }
                        _ => unreachable!(),
                    },

                    Expressions::FnCall {
                        name,
                        arguments,
                        span: _,
                    } => {
                        if let Type::Pointer(ptr_type) = prev_type.clone() {
                            prev_type = *ptr_type;
                        }

                        match prev_type.clone() {
                            Type::Alias(alias) => {
                                let alias_type =
                                    self.get_alias_type(prev_type.clone(), None).unwrap();

                                match alias_type {
                                    "struct" | "enum" => {
                                        let function = self
                                            .scope
                                            .get_function(format!("{alias_type}_{alias}__{name}"))
                                            .unwrap();

                                        {
                                            let mut_fn = self
                                                .scope
                                                .get_mut_function(format!(
                                                    "{alias_type}_{alias}__{name}"
                                                ))
                                                .unwrap();
                                            mut_fn.called = true;
                                        }

                                        let mut arguments = arguments
                                            .iter()
                                            .zip(function.arguments.clone())
                                            .map(|(arg, exp)| {
                                                self.compile_expression(arg.clone(), Some(exp))
                                                    .1
                                            })
                                            .collect::<Vec<LLVMValueRef>>();

                                        if let Some(Type::Pointer(ptr_type)) =
                                            function.arguments.first()
                                        {
                                            if let Type::Alias(arg_alias) = *ptr_type.clone() {
                                                if arg_alias == alias {
                                                    arguments.reverse();
                                                    arguments.push(prev_val);
                                                    arguments.reverse();
                                                }
                                            }
                                        }

                                        prev_type = function.datatype;
                                        let call_name = std::ffi::CString::new("").unwrap();
                                        prev_val = unsafe { LLVMBuildCall2(self.builder, function.fn_type, function.value, arguments.as_mut_ptr(), arguments.len() as u32, call_name.as_ptr()) };
                                    }
                                    _ => unreachable!(),
                                }
                            }
                            Type::ImportObject(import_name) => {
                                let module_content =
                                    self.imports.get(&import_name).unwrap().clone();
                                let function = module_content.functions.get(name).unwrap();

                                let mut arguments = arguments
                                    .iter()
                                    .zip(function.arguments.clone())
                                    .map(|(arg, exp)| {
                                        self.compile_expression(arg.clone(), Some(exp)).1
                                    })
                                    .collect::<Vec<LLVMValueRef>>();

                                prev_type = function.datatype.to_owned();
                                let call_name = std::ffi::CString::new("").unwrap();
                                prev_val = unsafe { LLVMBuildCall2(self.builder, function.fn_type, function.value, arguments.as_mut_ptr(), arguments.len() as u32, call_name.as_ptr()) };
                            }
                            _ => {
                                panic!(
                                    "FnCall `{}()` unreachable type got: `{}`",
                                    name, &prev_type
                                );
                            }
                        }
                    }

                    Expressions::Struct {
                        name,
                        fields,
                        span: _,
                    } => {
                        if let Type::ImportObject(import_name) = prev_type.clone() {
                            let structure = self
                                .imports
                                .get(&import_name)
                                .unwrap()
                                .structures
                                .get(name)
                                .unwrap()
                                .clone();
                            let alloca_name = std::ffi::CString::new(format!("struct.{name}.init")).unwrap();
                            let struct_alloca = unsafe { LLVMBuildAlloca(self.builder, structure.llvm_type, alloca_name.as_ptr()) };

                            for (field_name, field_expr) in fields {
                                let struct_field = structure.fields.get(field_name).unwrap();
                                let field_value = self.compile_expression(
                                    field_expr.clone(),
                                    Some(struct_field.datatype.clone()),
                                );

                                let gep_name = std::ffi::CString::new("").unwrap();
                                let field_ptr = unsafe { LLVMBuildStructGEP2(self.builder, structure.llvm_type, struct_alloca, struct_field.nth, gep_name.as_ptr()) };
                                unsafe { LLVMBuildStore(self.builder, field_value.1, field_ptr) };
                            }

                            let value = match expected {
                                Some(Type::Pointer(_)) => struct_alloca,
                                _ => {
                                    let load_name = std::ffi::CString::new("").unwrap();
                                    unsafe { LLVMBuildLoad2(self.builder, structure.llvm_type, struct_alloca, load_name.as_ptr()) }
                                },
                            };

                            prev_type = Type::Alias(format!("{import_name}.{name}"));
                            prev_val = value;
                        } else {
                            unreachable!()
                        }
                    }
                    _ => {
                        panic!("Unreachable expression found: {sub:?}")
                    }
                });

                (prev_type, prev_val)
            }
            Expressions::Scope { block, span: _ } => {
                let fn_type = self.get_fn_type(expected.clone().unwrap_or(Type::Void), &mut [], false);
                let fn_name = std::ffi::CString::new("__scope_wrap").unwrap();
                let scope_fn_value = unsafe { LLVMAddFunction(self.module, fn_name.as_ptr(), fn_type) };
                unsafe { LLVMSetLinkage(scope_fn_value, llvm_sys::LLVMLinkage::LLVMPrivateLinkage) };

                let entry_name = std::ffi::CString::new("entry").unwrap();
                let entry = unsafe { LLVMAppendBasicBlockInContext(self.context, scope_fn_value, entry_name.as_ptr()) };
                let current_position = unsafe { LLVMGetInsertBlock(self.builder) };

                unsafe { LLVMPositionBuilderAtEnd(self.builder, entry) };
                block
                    .iter()
                    .for_each(|stmt| self.compile_statement(stmt.to_owned(), None));

                unsafe { LLVMPositionBuilderAtEnd(self.builder, current_position) };
                let call_name = std::ffi::CString::new("").unwrap();
                let scope_result = unsafe { LLVMBuildCall2(self.builder, fn_type, scope_fn_value, [].as_mut_ptr(), 0, call_name.as_ptr()) };

                (expected.unwrap_or(Type::Void), scope_result)
            }

            Expressions::Array {
                values,
                len,
                span: _,
            } => {
                let expected_items_type = match expected {
                    Some(Type::Array(typ, _)) => Some(*typ),
                    _ => None,
                };

                let compiled_values = values
                    .into_iter()
                    .map(|val| self.compile_expression(val, expected_items_type.clone()))
                    .collect::<Vec<(Type, LLVMValueRef)>>();

                let arr_type = compiled_values[0].0.clone();
                let arr_basic_type = unsafe { LLVMTypeOf(compiled_values[0].1) };

                let alloca_name = std::ffi::CString::new("").unwrap();
                let arr_alloca = unsafe { LLVMBuildArrayAlloca(self.builder, arr_basic_type, LLVMConstInt(LLVMInt64TypeInContext(self.context), len as u64, 0), alloca_name.as_ptr()) };

                compiled_values
                    .into_iter()
                    .enumerate()
                    .for_each(|(ind, (_, basic_value))| {
                        let gep_name = std::ffi::CString::new("").unwrap();
                        let ptr = unsafe {
                            LLVMBuildInBoundsGEP2(
                                self.builder,
                                arr_basic_type,
                                arr_alloca,
                                [LLVMConstInt(LLVMInt64TypeInContext(self.context), ind as u64, 0)].as_mut_ptr(),
                                1,
                                gep_name.as_ptr(),
                            )
                        };
                        unsafe { LLVMBuildStore(self.builder, basic_value, ptr) };
                    });

                (Type::Array(Box::new(arr_type), len), arr_alloca)
            }
            Expressions::Tuple { values, span: _ } => {
                let mut expected_types = values.iter().map(|_| None).collect::<Vec<Option<Type>>>();
                if let Some(Type::Tuple(expectations)) = expected.clone() {
                    expected_types = expectations.into_iter().map(Some).collect();
                }

                let compiled_values = values
                    .into_iter()
                    .zip(expected_types)
                    .map(|(val, exp)| self.compile_expression(val, exp))
                    .collect::<Vec<(Type, LLVMValueRef)>>();
                let mut basic_types = compiled_values
                    .iter()
                    .map(|val| unsafe { LLVMTypeOf(val.1) })
                    .collect::<Vec<LLVMTypeRef>>();
                let tuple_type = unsafe { LLVMStructTypeInContext(self.context, basic_types.as_mut_ptr(), basic_types.len() as u32, 0) };

                let compiled_types = compiled_values
                    .iter()
                    .map(|(typ, _)| typ.clone())
                    .collect::<Vec<Type>>();
                let alloca_name = std::ffi::CString::new(format!(
                    "tuple__{}",
                    compiled_types
                        .iter()
                        .map(|typ| typ.to_string())
                        .collect::<Vec<String>>()
                        .join("_")
                )).unwrap();
                let alloca = unsafe { LLVMBuildAlloca(self.builder, tuple_type, alloca_name.as_ptr()) };

                compiled_values
                    .into_iter()
                    .enumerate()
                    .for_each(|(idx, (_, basic_val))| {
                        let gep_name = std::ffi::CString::new("").unwrap();
                        let ptr = unsafe { LLVMBuildStructGEP2(self.builder, tuple_type, alloca, idx as u32, gep_name.as_ptr()) };
                        unsafe { LLVMBuildStore(self.builder, basic_val, ptr) };
                    });

                let value = match expected {
                    Some(Type::Pointer(_)) => alloca,
                    _ => {
                        let load_name = std::ffi::CString::new("").unwrap();
                        unsafe { LLVMBuildLoad2(self.builder, tuple_type, alloca, load_name.as_ptr()) }
                    },
                };
                let tuple_datatype = Type::Tuple(compiled_types);

                (tuple_datatype, value)
            }
            Expressions::Slice {
                object,
                index,
                span,
            } => {
                let obj = self.compile_expression(*object.clone(), None);
                let idx = self.compile_expression(*index, Some(Type::USIZE));

                match obj.0 {
                    Type::Array(ret_type, len) => {
                        let obj_ptr = if self.is_a_pointer_value(obj.1) {
                            obj.1
                        } else {
                            self.compile_expression(
                                *object.clone(),
                                Some(Type::Pointer(Box::new(Type::Undefined))),
                            )
                            .1
                        };

                        // checking for the right index
                        let checker_block_name = std::ffi::CString::new("__idxcb").unwrap();
                        let checker_block = unsafe { LLVMAppendBasicBlockInContext(self.context, self.function.unwrap(), checker_block_name.as_ptr()) };
                        let error_block_name = std::ffi::CString::new("__idxcb_err").unwrap();
                        let error_block = unsafe { LLVMAppendBasicBlockInContext(self.context, self.function.unwrap(), error_block_name.as_ptr()) };
                        let ok_block_name = std::ffi::CString::new("__idxcb_ok").unwrap();
                        let ok_block = unsafe { LLVMAppendBasicBlockInContext(self.context, self.function.unwrap(), ok_block_name.as_ptr()) };

                        unsafe { LLVMBuildBr(self.builder, checker_block) };
                        unsafe { LLVMPositionBuilderAtEnd(self.builder, checker_block) };

                        let expected_basic_value = unsafe { LLVMConstInt(LLVMInt64TypeInContext(self.context), len as u64, 0) };
                        let provided_basic_value = idx.1;

                        let cmp_name = std::ffi::CString::new("").unwrap();
                        let cmp_value = unsafe { LLVMBuildICmp(self.builder, llvm_sys::LLVMIntPredicate::LLVMIntSLT, provided_basic_value, expected_basic_value, cmp_name.as_ptr()) };
                        unsafe { LLVMBuildCondBr(self.builder, cmp_value, ok_block, error_block) };

                        unsafe { LLVMPositionBuilderAtEnd(self.builder, error_block) };

                        self.build_panic(
                            "Array has len %ld, but index is %ld",
                            vec![expected_basic_value, provided_basic_value],
                            self.get_source_line(span.0),
                        );
                        self.build_branch(ok_block);

                        unsafe { LLVMPositionBuilderAtEnd(self.builder, ok_block) };

                        // getting value
                        let basic_ret_type = self.get_basic_type(*ret_type.clone());
                        let gep_name = std::ffi::CString::new("").unwrap();
                        let ptr = unsafe {
                            LLVMBuildInBoundsGEP2(
                                self.builder,
                                basic_ret_type,
                                obj_ptr,
                                [idx.1].as_mut_ptr(),
                                1,
                                gep_name.as_ptr(),
                            )
                        };

                        let load_name = std::ffi::CString::new("").unwrap();
                        let ret_value = unsafe { LLVMBuildLoad2(self.builder, basic_ret_type, ptr, load_name.as_ptr()) };
                        (*ret_type, ret_value)
                    }
                    Type::Pointer(ptr_type) => {
                        let basic_ret_type = self.get_basic_type(*ptr_type.clone());
                        let gep_name = std::ffi::CString::new("").unwrap();
                        let ptr = unsafe {
                            LLVMBuildInBoundsGEP2(
                                self.builder,
                                basic_ret_type,
                                obj.1,
                                [idx.1].as_mut_ptr(),
                                1,
                                gep_name.as_ptr(),
                            )
                        };

                        let load_name = std::ffi::CString::new("").unwrap();
                        let ret_value = unsafe { LLVMBuildLoad2(self.builder, basic_ret_type, ptr, load_name.as_ptr()) };
                        (*ptr_type, ret_value)
                    }

                    Type::Alias(alias) => {
                        // getting struct ptr and type
                        let ptr = self
                            .compile_expression(
                                *object,
                                Some(Type::Pointer(Box::new(Type::Undefined))),
                            )
                            .1;

                        let struct_type = self.scope.get_struct(alias).unwrap();
                        let slice_fn = struct_type.functions.get("slice").unwrap();

                        // calling slice function
                        let call_name = std::ffi::CString::new("@deen_slice_call").unwrap();
                        let call_result = unsafe {
                            LLVMBuildCall2(
                                self.builder,
                                slice_fn.fn_type,
                                slice_fn.value,
                                [ptr, idx.1].as_mut_ptr(),
                                2,
                                call_name.as_ptr(),
                            )
                        };

                        (slice_fn.datatype.clone(), call_result)
                    }

                    _ => unreachable!(),
                }
            }
            Expressions::Struct {
                name,
                fields,
                span: _,
            } => {
                let structure = self.scope.get_struct(&name).unwrap();
                let alloca_name = std::ffi::CString::new(format!("struct.{name}.init")).unwrap();
                let struct_alloca = unsafe { LLVMBuildAlloca(self.builder, structure.llvm_type, alloca_name.as_ptr()) };

                for (field_name, field_expr) in fields {
                    let struct_field = structure.fields.get(&field_name).unwrap();
                    let field_value =
                        self.compile_expression(field_expr, Some(struct_field.datatype.clone()));

                    let gep_name = std::ffi::CString::new("").unwrap();
                    let field_ptr = unsafe { LLVMBuildStructGEP2(self.builder, structure.llvm_type, struct_alloca, struct_field.nth, gep_name.as_ptr()) };

                    unsafe { LLVMBuildStore(self.builder, field_value.1, field_ptr) };
                }

                let value = match expected {
                    Some(Type::Pointer(_)) => struct_alloca,
                    _ => {
                        let load_name = std::ffi::CString::new("").unwrap();
                        unsafe { LLVMBuildLoad2(self.builder, structure.llvm_type, struct_alloca, load_name.as_ptr()) }
                    },
                };

                (Type::Alias(name), value)
            }

            Expressions::Argument {
                name: _,
                r#type,
                span: _,
            } => (r#type.clone(), unsafe { LLVMConstNull(self.get_basic_type(r#type.clone())) }),
            Expressions::None => unreachable!(),
        }
    }

    fn compile_value(
        &mut self,
        value: Value,
        expected: Option<Type>,
    ) -> (Type, LLVMValueRef) {
        match value {
            Value::Integer(int) => {
                if genpay_semantic::Analyzer::is_integer(&expected.clone().unwrap_or(Type::Void))
                    || expected == Some(Type::Char)
                {
                    let exp = if let Some(exp) = expected.clone() {
                        exp
                    } else {
                        unreachable!()
                    };

                    let (expected_type, signed) = match exp {
                        Type::I8 => (unsafe { LLVMInt8TypeInContext(self.context) }, 1),
                        Type::I16 => (unsafe { LLVMInt16TypeInContext(self.context) }, 1),
                        Type::I32 => (unsafe { LLVMInt32TypeInContext(self.context) }, 1),
                        Type::I64 => (unsafe { LLVMInt64TypeInContext(self.context) }, 1),

                        Type::U8 => (unsafe { LLVMInt8TypeInContext(self.context) }, 0),
                        Type::U16 => (unsafe { LLVMInt16TypeInContext(self.context) }, 0),
                        Type::U32 => (unsafe { LLVMInt32TypeInContext(self.context) }, 0),
                        Type::U64 => (unsafe { LLVMInt64TypeInContext(self.context) }, 0),
                        Type::USIZE => (unsafe { LLVMInt64TypeInContext(self.context) }, 0),

                        Type::Char => (unsafe { LLVMInt8TypeInContext(self.context) }, 0),

                        _ => {
                            panic!("Unreachable type expected: {exp}");
                        }
                    };

                    return (
                        exp,
                        unsafe { LLVMConstInt(expected_type, int as u64, signed) },
                    );
                }

                match int {
                    -2_147_483_648..=2_147_483_647 => (
                        Type::I32,
                        unsafe { LLVMConstInt(LLVMInt32TypeInContext(self.context), int as u64, 1) },
                    ),
                    -9_223_372_036_854_775_808..=9_223_372_036_854_775_807 => (
                        Type::I64,
                        unsafe { LLVMConstInt(LLVMInt64TypeInContext(self.context), int as u64, 1) },
                    ),
                }
            }
            Value::Float(float) => {
                if let Some(exp) = expected {
                    return match exp {
                        Type::F32 => (Type::F32, unsafe { LLVMConstReal(LLVMFloatTypeInContext(self.context), float) }),
                        Type::F64 => (Type::F64, unsafe { LLVMConstReal(LLVMDoubleTypeInContext(self.context), float) }),
                        _ => (Type::F64, unsafe { LLVMConstReal(LLVMDoubleTypeInContext(self.context), float) }),
                    };
                }

                (Type::F64, unsafe { LLVMConstReal(LLVMDoubleTypeInContext(self.context), float) })
            }

            Value::Char(ch) => (
                Type::Char,
                unsafe { LLVMConstInt(LLVMInt8TypeInContext(self.context), ch as u64, 0) },
            ),
            Value::String(str) => {
                let str_c_string = std::ffi::CString::new(str).unwrap();
                let global_value = unsafe { LLVMBuildGlobalStringPtr(self.builder, str_c_string.as_ptr(), str_c_string.as_ptr()) };
                (
                    Type::Pointer(Box::new(Type::Char)),
                    global_value,
                )
            }

            Value::Boolean(bool) => (
                Type::Bool,
                unsafe { LLVMConstInt(LLVMInt1TypeInContext(self.context), bool as u64, 0) },
            ),
            Value::Identifier(id) => {
                // current module objects
                if self.scope.get_struct(&id).is_some() {
                    return (Type::Alias(id), unsafe { LLVMConstNull(LLVMInt8TypeInContext(self.context)) });
                }
                if let Some(typedef) = self.scope.get_typedef(&id) {
                    return (typedef.clone(), unsafe { LLVMConstNull(LLVMInt8TypeInContext(self.context)) });
                }
                if self.scope.get_enum(&id).is_some() {
                    return (Type::Alias(id), unsafe { LLVMConstNull(LLVMInt8TypeInContext(self.context)) });
                }

                // seeking through imports
                if self.imports.contains_key(&id) {
                    return (
                        Type::ImportObject(id),
                        unsafe { LLVMConstNull(LLVMInt8TypeInContext(self.context)) },
                    );
                }

                let variable = self.scope.get_mut_variable(&id).unwrap(); // already checked by semantic analyzer

                if variable.global {
                    let id_c_string = std::ffi::CString::new(id).unwrap();
                    let global_value = unsafe { LLVMGetNamedGlobal(self.module, id_c_string.as_ptr()) };
                    return (variable.datatype.clone(), global_value);
                }

                if expected == Some(Type::NoDrop) {
                    variable.no_drop = true;
                }

                let load_name = std::ffi::CString::new("").unwrap();
                let value = match expected.clone() {
                    Some(Type::Pointer(ptr_type)) => {
                        if *ptr_type == Type::Undefined {
                            variable.ptr
                        } else {
                            unsafe { LLVMBuildLoad2(self.builder, variable.llvm_type, variable.ptr, load_name.as_ptr()) }
                        }
                    }
                    _ => unsafe { LLVMBuildLoad2(self.builder, variable.llvm_type, variable.ptr, load_name.as_ptr()) },
                };
                let datatype = match expected {
                    Some(Type::Pointer(ptr_type)) => {
                        if id == "self" {
                            Type::Pointer(Box::new(Type::Pointer(Box::new(
                                variable.datatype.clone(),
                            ))))
                        } else if *ptr_type == Type::Undefined {
                            Type::Pointer(Box::new(variable.datatype.clone()))
                        } else {
                            variable.datatype.clone()
                        }
                    }
                    _ => variable.datatype.clone(),
                };

                (datatype, value)
            }

            Value::Void => (Type::Void, unsafe { LLVMConstNull(LLVMInt1TypeInContext(self.context)) }),
            Value::Null => (
                Type::Null,
                unsafe { LLVMConstNull(LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)) },
            ),
            Value::Keyword(_) => unreachable!(),
        }
    }
}

impl CodeGen {
    fn is_a_pointer_value(&self, value: LLVMValueRef) -> bool {
        let type_of = unsafe { LLVMTypeOf(value) };
        let kind = unsafe { LLVMGetTypeKind(type_of) };
        kind == LLVMTypeKind::LLVMPointerTypeKind
    }

    fn build_panic(
        &mut self,
        message: impl std::convert::AsRef<str>,
        specifiers: Vec<LLVMValueRef>,
        call_line: usize,
    ) {
        let mut message = message.as_ref().to_owned();
        let panic_fn_name = std::ffi::CString::new("__genpay_panic").unwrap();
        let panic_fn = unsafe {
            let function = LLVMGetNamedFunction(self.module, panic_fn_name.as_ptr());
            if function == std::ptr::null_mut() {
                self.create_panic_function()
            } else {
                function
            }
        };

        if message.chars().last().unwrap_or(' ') != '\n' {
            message.push('\n')
        }

        let message_str = format!(
            "\n* Runtime Panic at `{}.genpay` <line {}>\n{}",
            unsafe { std::ffi::CStr::from_ptr(LLVMGetModuleIdentifier(self.module, &mut 0)).to_str().unwrap() },
            call_line,
            message
        );
        let message_c_string = std::ffi::CString::new(message_str).unwrap();
        let panic_formatter_name = std::ffi::CString::new("panic_formatter").unwrap();
        let message_ptr = unsafe { LLVMBuildGlobalStringPtr(self.builder, message_c_string.as_ptr(), panic_formatter_name.as_ptr()) };

        let mut args: Vec<LLVMValueRef> = [vec![message_ptr], specifiers].concat();
        let call_name = std::ffi::CString::new("").unwrap();
        unsafe { LLVMBuildCall2(self.builder, LLVMGetCalledFunctionType(panic_fn), panic_fn, args.as_mut_ptr(), args.len() as u32, call_name.as_ptr()) };
    }

    fn create_panic_function(&mut self) -> LLVMValueRef {
        let fn_type = unsafe { LLVMFunctionType(LLVMVoidTypeInContext(self.context), [LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)].as_mut_ptr(), 1, 1) };

        let fn_name = std::ffi::CString::new("__genpay_panic").unwrap();
        let fn_value = unsafe { LLVMAddFunction(self.module, fn_name.as_ptr(), fn_type) };
        unsafe { LLVMSetLinkage(fn_value, llvm_sys::LLVMLinkage::LLVMPrivateLinkage) };

        let entry_name = std::ffi::CString::new("entry").unwrap();
        let entry = unsafe { LLVMAppendBasicBlockInContext(self.context, fn_value, entry_name.as_ptr()) };
        let old_position = unsafe { LLVMGetInsertBlock(self.builder) };
        unsafe { LLVMPositionBuilderAtEnd(self.builder, entry) };

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
        let exit_name = std::ffi::CString::new("exit").unwrap();
        let exit_fn = unsafe {
            let function = LLVMGetNamedFunction(self.module, exit_name.as_ptr());
            if function == std::ptr::null_mut() {
                let fn_type = LLVMFunctionType(LLVMVoidTypeInContext(self.context), [LLVMInt32TypeInContext(self.context)].as_mut_ptr(), 1, 0);
                LLVMAddFunction(self.module, exit_name.as_ptr(), fn_type)
            } else {
                function
            }
        };

        let mut args: Vec<LLVMValueRef> = Vec::new();
        for i in 0..unsafe { LLVMCountParams(fn_value) } {
            args.push(unsafe { LLVMGetParam(fn_value, i) });
        }

        let call_name = std::ffi::CString::new("").unwrap();
        unsafe { LLVMBuildCall2(self.builder, LLVMGetCalledFunctionType(printf_fn), printf_fn, args.as_mut_ptr(), args.len() as u32, call_name.as_ptr()) };
        let exit_code = unsafe { LLVMConstInt(LLVMInt32TypeInContext(self.context), 1, 0) };
        unsafe { LLVMBuildCall2(self.builder, LLVMGetCalledFunctionType(exit_fn), exit_fn, [exit_code].as_mut_ptr(), 1, call_name.as_ptr()) };
        unsafe { LLVMBuildRetVoid(self.builder) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, old_position) };

        fn_value
    }

    #[inline]
    fn get_basic_type(&self, datatype: Type) -> LLVMTypeRef {
        unsafe {
            match datatype {
                Type::I8 => LLVMInt8TypeInContext(self.context),
                Type::I16 => LLVMInt16TypeInContext(self.context),
                Type::I32 => LLVMInt32TypeInContext(self.context),
                Type::I64 => LLVMInt64TypeInContext(self.context),

                Type::U8 => LLVMInt8TypeInContext(self.context),
                Type::U16 => LLVMInt16TypeInContext(self.context),
                Type::U32 => LLVMInt32TypeInContext(self.context),
                Type::U64 => LLVMInt64TypeInContext(self.context),
                Type::USIZE => LLVMIntTypeInContext(self.context, 64),

                Type::F32 => LLVMFloatTypeInContext(self.context),
                Type::F64 => LLVMDoubleTypeInContext(self.context),

                Type::Void => LLVMVoidTypeInContext(self.context),
                Type::Char => LLVMInt8TypeInContext(self.context),
                Type::Bool => LLVMInt1TypeInContext(self.context),

                Type::Pointer(_) => LLVMPointerType(LLVMInt8TypeInContext(self.context), 0),
                Type::Array(datatype, len) => {
                    LLVMArrayType2(self.get_basic_type(*datatype), len as u64)
                }
                Type::DynamicArray(_) => todo!(),

                Type::Tuple(types) => {
                    let mut basic_types = types
                        .into_iter()
                        .map(|typ| self.get_basic_type(typ))
                        .collect::<Vec<LLVMTypeRef>>();
                    LLVMStructTypeInContext(self.context, basic_types.as_mut_ptr(), basic_types.len() as u32, 0)
                }
                Type::Alias(alias) => {
                    if alias == "_" {
                        return LLVMInt1TypeInContext(self.context);
                    }

                    let struct_type = self.scope.get_struct(&alias);
                    let enum_type = self.scope.get_enum(&alias);
                    let typedef_type = self.scope.get_typedef(&alias);

                    if let Some(struct_type) = struct_type {
                        return struct_type.llvm_type;
                    };
                    if let Some(enum_type) = enum_type {
                        return enum_type.llvm_type;
                    };
                    if let Some(typedef_type) = typedef_type {
                        return self.get_basic_type(typedef_type.to_owned());
                    };

                    panic!("Compiler's semantic didn't catch undefined alias: `{alias}`")
                }

                Type::Function(_, _, _) => unreachable!(),
                Type::ImportObject(_) => unreachable!(),
                Type::Undefined => unreachable!(),
                Type::NoDrop => unreachable!(),
                Type::Struct(fields, _) => {
                    let mut basic_types = fields
                        .iter()
                        .map(|field| self.get_basic_type(field.1.clone()))
                        .collect::<Vec<LLVMTypeRef>>();
                    LLVMStructTypeInContext(self.context, basic_types.as_mut_ptr(), basic_types.len() as u32, 0)
                }
                Type::Enum(_, _) => LLVMInt16TypeInContext(self.context),
                Type::SelfRef => LLVMPointerType(LLVMInt8TypeInContext(self.context), 0),
                Type::Null => LLVMInt1TypeInContext(self.context),
            }
        }
    }

    fn get_fn_type(
        &self,
        datatype: Type,
        arguments: &mut [LLVMTypeRef],
        is_var_args: bool,
    ) -> LLVMTypeRef {
        unsafe {
            match datatype {
                Type::Void => LLVMFunctionType(LLVMVoidTypeInContext(self.context), arguments.as_mut_ptr(), arguments.len() as u32, is_var_args as i32),
                _ => LLVMFunctionType(self.get_basic_type(datatype), arguments.as_mut_ptr(), arguments.len() as u32, is_var_args as i32),
            }
        }
    }

    fn get_alias_type(&self, alias_type: Type, import_name: Option<&str>) -> Option<&str> {
        if let Type::Alias(alias) = alias_type {
            let struct_type = self.scope.get_struct(&alias);
            let enum_type = self.scope.get_enum(&alias);
            let typedef_type = self.scope.get_typedef(&alias);

            if struct_type.is_some() {
                return Some("struct");
            };
            if enum_type.is_some() {
                return Some("enum");
            };
            if typedef_type.is_some() {
                return Some("typedef");
            };

            if let Some(import_name) = import_name {
                let import = self.imports.get(import_name).unwrap();

                let struct_type = import.structures.get(&alias);
                let enum_type = import.enumerations.get(&alias);

                if struct_type.is_some() {
                    return Some("struct");
                };
                if enum_type.is_some() {
                    return Some("enum");
                };

                None
            } else {
                *self
                    .imports
                    .values()
                    .map(|import| {
                        let struct_type = import.structures.get(&alias);
                        let enum_type = import.enumerations.get(&alias);

                        if struct_type.is_some() {
                            return Some("struct");
                        };
                        if enum_type.is_some() {
                            return Some("enum");
                        };

                        None
                    })
                    .collect::<Vec<Option<&str>>>()
                    .first()
                    .unwrap()
            }
        } else {
            None
        }
    }

    fn get_source_line(&self, position: usize) -> usize {
        self.source
            .char_indices()
            .take_while(|&(pos, _)| pos < position)
            .filter(|&(_, chr)| chr == '\n')
            .count()
            + 1
    }

    fn build_branch(&mut self, block: LLVMBasicBlockRef) {
        if unsafe { LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(self.builder)) } == std::ptr::null_mut()
        {
            unsafe { LLVMBuildBr(self.builder, block) };
        }
    }

    fn type_specifier(&self, datatype: &Type) -> String {
        match datatype {
            Type::I8 => "%hhd",
            Type::I16 => "%hd",
            Type::I32 => "%d",
            Type::I64 => "%lld",

            Type::U8 => "%hhu",
            Type::U16 => "%hu",
            Type::U32 => "%u",
            Type::U64 => "%llu",

            Type::USIZE => "%zu",

            Type::F32 => "%f",
            Type::F64 => "%lf",

            Type::Char => "%c",
            Type::Pointer(ptr) => match **ptr {
                Type::Char => "%s",
                _ => "%p",
            },

            Type::Bool => "%s",
            Type::Enum(_, _) => "%d",

            Type::Alias(_) => {
                let alias_type = self.get_alias_type(datatype.clone(), None).unwrap();
                match alias_type {
                    "struct" => "%s",
                    "enum" => "%d",
                    _ => unreachable!(),
                }
            }
            _ => "%s",
        }
        .to_string()
    }

    fn booleans_strings(&mut self) -> (LLVMValueRef, LLVMValueRef) {
        if let Some(allocated_strings) = self.booleans_strings {
            return allocated_strings;
        }

        let true_c_string = std::ffi::CString::new("true").unwrap();
        let true_global_name = std::ffi::CString::new("@true").unwrap();
        let false_c_string = std::ffi::CString::new("false").unwrap();
        let false_global_name = std::ffi::CString::new("@false").unwrap();

        let strings = (
            unsafe { LLVMBuildGlobalStringPtr(self.builder, true_c_string.as_ptr(), true_global_name.as_ptr()) },
            unsafe { LLVMBuildGlobalStringPtr(self.builder, false_c_string.as_ptr(), false_global_name.as_ptr()) },
        );

        self.booleans_strings = Some(strings);
        strings
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use llvm_sys::LLVMLinkage;

    // #[test]
    // fn panic_function_test() {
    //     let ctx = CodeGen::create_context();
    //     let mut codegen = CodeGen::new(
    //         ctx,
    //         "",
    //         "",
    //         genpay_semantic::symtable::SymbolTable::default(),
    //     );

    //     let main_fn_name = std::ffi::CString::new("main").unwrap();
    //     let fn_type = unsafe { LLVMFunctionType(LLVMVoidTypeInContext(codegen.context), [].as_mut_ptr(), 0, 0) };
    //     let main_fn = unsafe { LLVMAddFunction(codegen.module, main_fn_name.as_ptr(), fn_type) };

    //     let entry_block_name = std::ffi::CString::new("entry").unwrap();
    //     let entry_block = unsafe { LLVMAppendBasicBlockInContext(codegen.context, main_fn, entry_block_name.as_ptr()) };
    //     unsafe { LLVMPositionBuilderAtEnd(codegen.builder, entry_block) };

    //     codegen.create_panic_function();
    // }

    #[test]
    fn boolean_strings_test() {
        let ctx = CodeGen::create_context();
        let mut codegen = CodeGen::new(
            ctx,
            "",
            "",
            genpay_semantic::symtable::SymbolTable::default(),
        );

        let main_fn_name = std::ffi::CString::new("main").unwrap();
        let fn_type = unsafe { LLVMFunctionType(LLVMVoidTypeInContext(codegen.context), [].as_mut_ptr(), 0, 0) };
        let main_fn = unsafe { LLVMAddFunction(codegen.module, main_fn_name.as_ptr(), fn_type) };

        let entry_block_name = std::ffi::CString::new("entry").unwrap();
        let entry_block = unsafe { LLVMAppendBasicBlockInContext(codegen.context, main_fn, entry_block_name.as_ptr()) };
        unsafe { LLVMPositionBuilderAtEnd(codegen.builder, entry_block) };

        let (true_str, false_str) = codegen.booleans_strings();
        let (true_str_2, false_str_2) = codegen.booleans_strings();

        assert_eq!(true_str, true_str_2);
        assert_eq!(false_str, false_str_2);
    }
}
