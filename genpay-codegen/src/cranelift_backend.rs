use cranelift::codegen::ir::StackSlot;
use cranelift::codegen::isa::TargetIsa;
use cranelift::codegen::settings;
use cranelift::codegen::Context;
use cranelift::prelude::{
    types, AbiParam, Configurable, EntityRef, FunctionBuilder, FunctionBuilderContext,
    InstBuilder, StackSlotData, StackSlotKind, Value, Variable,
};
use cranelift_module::{FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use genpay_parser::{expressions::Expressions, statements::Statements, value::Value as GenpayValue};
use std::collections::HashMap;
use std::sync::Arc;

pub struct CraneliftCodeGen {
    builder_context: FunctionBuilderContext,
    ctx: Context,
    module: ObjectModule,
    functions: HashMap<String, FuncId>,
    isa: Arc<dyn TargetIsa>,
}

struct FunctionCompiler<'a> {
    builder: FunctionBuilder<'a>,
    module: &'a mut ObjectModule,
    variables: HashMap<String, Variable>,
    variable_slots: Vec<StackSlot>,
    functions: &'a HashMap<String, FuncId>,
}

impl CraneliftCodeGen {
    pub fn new() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.enable("is_pic").unwrap();
        let isa = cranelift_native::builder()
            .unwrap_or_else(|msg| {
                panic!("host machine is not supported: {}", msg);
            })
            .finish(settings::Flags::new(flag_builder))
            .unwrap();

        let builder =
            ObjectBuilder::new(isa.clone(), "genpay-module", cranelift_module::default_libcall_names())
                .unwrap();
        let module = ObjectModule::new(builder);
        let builder_context = FunctionBuilderContext::new();
        let ctx = module.make_context();

        Self {
            builder_context,
            ctx,
            module,
            functions: HashMap::new(),
            isa,
        }
    }

    pub fn compile(mut self, ast: Vec<Statements>) -> Result<Vec<u8>, String> {
        for statement in ast {
            self.compile_statement(statement);
        }

        let product = self.module.finish();
        Ok(product.emit().unwrap())
    }

    fn compile_statement(&mut self, statement: Statements) {
        match statement {
            Statements::FunctionDefineStatement {
                name,
                datatype,
                arguments,
                block,
                ..
            } => {
                self.compile_function(name, datatype, arguments, block);
            }
            _ => { /* For now, ignore other statements */ }
        }
    }

    fn compile_function(
        &mut self,
        name: String,
        return_type: genpay_parser::types::Type,
        args: Vec<(String, genpay_parser::types::Type)>,
        body: Vec<Statements>,
    ) {
        self.ctx.func.signature.clear(self.isa.default_call_conv());

        for (_, arg_type) in &args {
            let cl_type = to_cranelift_type(arg_type.clone());
            self.ctx.func.signature.params.push(AbiParam::new(cl_type));
        }
        let cl_return_type = to_cranelift_type(return_type);
        self.ctx.func.signature.returns.push(AbiParam::new(cl_return_type));

        let func_id = self
            .module
            .declare_function(&name, Linkage::Export, &self.ctx.func.signature)
            .unwrap();
        self.functions.insert(name, func_id);

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        let arg_values = builder.block_params(entry_block).to_vec();

        let mut fn_compiler = FunctionCompiler {
            builder,
            module: &mut self.module,
            variables: HashMap::new(),
            variable_slots: Vec::new(),
            functions: &self.functions,
        };

        for ((arg_name, arg_type), arg_value) in args.iter().zip(arg_values.iter()) {
            let cl_type = to_cranelift_type(arg_type.clone());
            let slot = fn_compiler.builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                cl_type.bytes(),
            ));
            fn_compiler.builder.ins().stack_store(*arg_value, slot, 0);

            let var = Variable::new(fn_compiler.variables.len());
            fn_compiler.variables.insert(arg_name.clone(), var);
            fn_compiler.variable_slots.push(slot);
        }

        fn_compiler.builder.seal_block(entry_block);

        // For now, we only support a single return statement in the body.
        if let Some(Statements::ReturnStatement { value, .. }) = body.get(0) {
            let return_val = fn_compiler.compile_expression(value.clone());
            fn_compiler.builder.ins().return_(&[return_val]);
        }

        fn_compiler.builder.finalize();
        self.module.define_function(func_id, &mut self.ctx).unwrap();
        self.module.clear_context(&mut self.ctx);
    }
}

impl<'a> FunctionCompiler<'a> {
    fn compile_expression(
        &mut self,
        expr: Expressions,
    ) -> Value {
        match expr {
            Expressions::Value(GenpayValue::Integer(i), _) => {
                self.builder.ins().iconst(types::I32, i as i64)
            }
            Expressions::Value(GenpayValue::Identifier(name), _) => {
                let var = self.variables.get(&name).unwrap();
                let slot = self.variable_slots[var.index()];
                self.builder.ins().stack_load(types::I32, slot, 0)
            }
            Expressions::Binary {
                lhs,
                rhs,
                operand,
                ..
            } => {
                let left = self.compile_expression(*lhs);
                let right = self.compile_expression(*rhs);
                match operand.as_str() {
                    "+" => self.builder.ins().iadd(left, right),
                    _ => todo!("unsupported binary operator"),
                }
            }
            Expressions::FnCall {
                name,
                arguments,
                ..
            } => {
                let func_id = self.functions.get(&name).unwrap();
                let func_ref = self.module.declare_func_in_func(*func_id, &mut self.builder.func);

                let mut arg_values = Vec::new();
                for arg_expr in arguments {
                    arg_values.push(self.compile_expression(arg_expr));
                }

                let call = self.builder.ins().call(func_ref, &arg_values);
                self.builder.inst_results(call)[0]
            }
            _ => todo!("unsupported expression: {:?}", expr),
        }
    }
}

fn to_cranelift_type(ty: genpay_parser::types::Type) -> types::Type {
    match ty {
        genpay_parser::types::Type::I32 => types::I32,
        genpay_parser::types::Type::Void => types::I64, // Represent void as i64 for now, will be ignored
        _ => todo!("unsupported type: {:?}", ty),
    }
}
