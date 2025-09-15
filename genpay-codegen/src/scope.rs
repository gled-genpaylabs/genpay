use crate::{
    CodeGen, enumeration::Enumeration, function::Function, structure::Structure, variable::Variable,
};
use genpay_parser::types::Type;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Scope<'ctx, 'bump> {
    parent: Option<Box<Scope<'ctx, 'bump>>>,

    variables: HashMap<&'bump str, Variable<'ctx>>,
    functions: HashMap<&'bump str, Function<'ctx, 'bump>>,
    structures: HashMap<&'bump str, Structure<'ctx>>,
    enumerations: HashMap<&'bump str, Enumeration<'ctx>>,
    typedefs: HashMap<&'bump str, Type<'bump>>,
}

impl<'ctx, 'bump> Default for Scope<'ctx, 'bump> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'ctx, 'bump> Scope<'ctx, 'bump> {
    pub fn new() -> Self {
        Self {
            parent: None,

            variables: HashMap::new(),
            functions: HashMap::new(),
            structures: HashMap::new(),
            enumerations: HashMap::new(),
            typedefs: HashMap::new(),
        }
    }

    // variables
    pub fn set_variable(&mut self, id: &'bump str, object: Variable<'ctx>) {
        self.variables.insert(id, object);
    }

    pub fn get_variable(&self, id: &str) -> Option<Variable<'ctx>> {
        self.variables.get(id).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.get_variable(id))
        })
    }

    pub fn get_mut_variable(
        &mut self,
        id: &str,
    ) -> Option<&mut Variable<'ctx>> {
        self.variables.get_mut(id).or_else(|| {
            self.parent
                .as_mut()
                .and_then(|parent| parent.get_mut_variable(id))
        })
    }

    pub fn remove_variable(&mut self, id: &str) -> Option<Variable<'ctx>> {
        self.variables.remove(id)
    }

    // functions
    pub fn set_function(&mut self, id: &'bump str, object: Function<'ctx, 'bump>) {
        self.functions.insert(id, object);
    }

    pub fn get_function(&self, id: &str) -> Option<Function<'ctx, 'bump>> {
        self.functions.get(id).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.get_function(id))
        })
    }

    pub fn get_mut_function(
        &mut self,
        id: &str,
    ) -> Option<&mut Function<'ctx, 'bump>> {
        self.functions.get_mut(id).or_else(|| {
            self.parent
                .as_mut()
                .and_then(|parent| parent.get_mut_function(id))
        })
    }

    // structures
    pub fn set_struct(&mut self, id: &'bump str, object: Structure<'ctx>) {
        self.structures.insert(id, object);
    }

    pub fn get_struct(&self, id: &str) -> Option<Structure<'ctx>> {
        self.structures.get(id).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.get_struct(id))
        })
    }

    pub fn get_mut_struct(
        &mut self,
        id: &str,
    ) -> Option<&mut Structure<'ctx>> {
        self.structures.get_mut(id).or_else(|| {
            self.parent
                .as_mut()
                .and_then(|parent| parent.get_mut_struct(id))
        })
    }

    // enums
    pub fn set_enum(&mut self, id: &'bump str, object: Enumeration<'ctx>) {
        self.enumerations.insert(id, object);
    }

    pub fn get_enum(&self, id: &str) -> Option<Enumeration<'ctx>> {
        self.enumerations
            .get(id)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.get_enum(id)))
    }

    pub fn get_mut_enum(
        &mut self,
        id: &str,
    ) -> Option<&mut Enumeration<'ctx>> {
        self.enumerations.get_mut(id).or_else(|| {
            self.parent
                .as_mut()
                .and_then(|parent| parent.get_mut_enum(id))
        })
    }

    // typedefs
    pub fn set_typedef(&mut self, id: &'bump str, object: Type<'bump>) {
        self.typedefs.insert(id, object);
    }

    pub fn get_typedef(&self, id: &str) -> Option<Type<'bump>> {
        self.typedefs.get(id).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.get_typedef(id))
        })
    }

    // tech
    pub fn stricted_variables(&self) -> HashMap<&'bump str, Variable<'ctx>> {
        self.variables.clone()
    }

    pub fn stricted_functions(&self) -> HashMap<&'bump str, Function<'ctx, 'bump>> {
        self.functions.clone()
    }

    pub fn stricted_structs(&self) -> HashMap<&'bump str, Structure<'ctx>> {
        self.structures.clone()
    }

    pub fn stricted_enums(&self) -> HashMap<&'bump str, Enumeration<'ctx>> {
        self.enumerations.clone()
    }
}

impl<'ctx, 'bump> CodeGen<'ctx, 'bump> {
    pub fn enter_scope(&mut self, mut scope: Scope<'ctx, 'bump>) {
        let parent = self.scope.clone();
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
            match var.datatype {
                Type::Alias(alias) => {
                    if matches!(self.get_alias_type(var.datatype, None), Some("struct")) && name != "self" && !var.no_drop {
                        let structure = self.scope.get_struct(alias).unwrap();

                        if let Some(destructor) = structure.functions.get("drop") {
                            let called = self.scope.get_function(self.bump.alloc(format!("struct_{alias}__drop"))).unwrap().called;
                            let mut expected_args = bumpalo::collections::Vec::new_in(self.bump);
                            expected_args.push(Type::Pointer(self.bump.alloc(var.datatype)));
                            if destructor.arguments == expected_args && !called {
                                let _ = self.builder.build_call(
                                    destructor.value,
                                    &[
                                        var.ptr.into()
                                    ],
                                    ""
                                );
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
        if let Some(parent) = self.scope.parent.clone() {
            self.scope = parent;
        }
    }

    pub fn exit_scope(&mut self) {
        if let Some(parent) = self.scope.parent.clone() {
            let insert_block = self.builder.get_insert_block().unwrap();

            if let Some(instr) = insert_block.get_terminator() {
                self.builder.position_before(&instr);
                self.cleanup_variables();

                self.builder.position_at_end(insert_block);
            } else {
                self.cleanup_variables();
            }

            self.scope = parent;
        }
    }
}
