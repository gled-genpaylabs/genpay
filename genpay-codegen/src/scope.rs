use crate::{
    enumeration::Enumeration, function::Function, structure::Structure, variable::Variable,
};
use genpay_parser::types::Type;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Scope {
    pub parent: Option<Box<Scope>>,

    variables: HashMap<String, Variable>,
    functions: HashMap<String, Function>,
    structures: HashMap<String, Structure>,
    enumerations: HashMap<String, Enumeration>,
    typedefs: HashMap<String, Type>,
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

impl Scope {
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
    pub fn set_variable(&mut self, id: impl std::convert::AsRef<str>, object: Variable) {
        self.variables.insert(id.as_ref().into(), object);
    }

    pub fn get_variable(&self, id: impl std::convert::AsRef<str>) -> Option<Variable> {
        self.variables.get(id.as_ref()).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.get_variable(id))
        })
    }

    pub fn get_mut_variable(
        &mut self,
        id: impl std::convert::AsRef<str>,
    ) -> Option<&mut Variable> {
        self.variables.get_mut(id.as_ref()).or_else(|| {
            self.parent
                .as_mut()
                .and_then(|parent| parent.get_mut_variable(id))
        })
    }

    pub fn remove_variable(&mut self, id: impl std::convert::AsRef<str>) -> Option<Variable> {
        self.variables.remove(id.as_ref())
    }

    // functions
    pub fn set_function(&mut self, id: impl std::convert::AsRef<str>, object: Function) {
        self.functions.insert(id.as_ref().into(), object);
    }

    pub fn get_function(&self, id: impl std::convert::AsRef<str>) -> Option<Function> {
        self.functions.get(id.as_ref()).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.get_function(id))
        })
    }

    pub fn get_mut_function(
        &mut self,
        id: impl std::convert::AsRef<str>,
    ) -> Option<&mut Function> {
        self.functions.get_mut(id.as_ref()).or_else(|| {
            self.parent
                .as_mut()
                .and_then(|parent| parent.get_mut_function(id))
        })
    }

    // structures
    pub fn set_struct(&mut self, id: impl std::convert::AsRef<str>, object: Structure) {
        self.structures.insert(id.as_ref().into(), object);
    }

    pub fn get_struct(&self, id: impl std::convert::AsRef<str>) -> Option<Structure> {
        self.structures.get(id.as_ref()).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.get_struct(id))
        })
    }

    pub fn get_mut_struct(
        &mut self,
        id: impl std::convert::AsRef<str>,
    ) -> Option<&mut Structure> {
        self.structures.get_mut(id.as_ref()).or_else(|| {
            self.parent
                .as_mut()
                .and_then(|parent| parent.get_mut_struct(id))
        })
    }

    // enums
    pub fn set_enum(&mut self, id: impl std::convert::AsRef<str>, object: Enumeration) {
        self.enumerations.insert(id.as_ref().into(), object);
    }

    pub fn get_enum(&self, id: impl std::convert::AsRef<str>) -> Option<Enumeration> {
        self.enumerations
            .get(id.as_ref())
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.get_enum(id)))
    }

    pub fn get_mut_enum(
        &mut self,
        id: impl std::convert::AsRef<str>,
    ) -> Option<&mut Enumeration> {
        self.enumerations.get_mut(id.as_ref()).or_else(|| {
            self.parent
                .as_mut()
                .and_then(|parent| parent.get_mut_enum(id))
        })
    }

    // typedefs
    pub fn set_typedef(&mut self, id: impl std::convert::AsRef<str>, object: Type) {
        self.typedefs.insert(id.as_ref().into(), object);
    }

    pub fn get_typedef(&self, id: impl std::convert::AsRef<str>) -> Option<Type> {
        self.typedefs.get(id.as_ref()).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.get_typedef(id))
        })
    }

    // tech
    pub fn stricted_variables(&self) -> HashMap<String, Variable> {
        self.variables.to_owned()
    }

    pub fn stricted_functions(&self) -> HashMap<String, Function> {
        self.functions.to_owned()
    }

    pub fn stricted_structs(&self) -> HashMap<String, Structure> {
        self.structures.to_owned()
    }

    pub fn stricted_enums(&self) -> HashMap<String, Enumeration> {
        self.enumerations.to_owned()
    }
}
