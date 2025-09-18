use bumpalo::collections::Vec;
use genpay_parser::{statements::Statements, types::Type};
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

#[derive(Debug, Clone, Default)]
pub struct SymbolTable<'bump> {
    pub imports: HashMap<String, Import<'bump>>,
    pub included: HashMap<String, Include<'bump>>,
    pub linked: HashSet<PathBuf>,
}

#[derive(Debug, Clone)]
pub struct Import<'bump> {
    pub functions: HashMap<String, Type<'bump>>,
    pub structs: HashMap<String, Type<'bump>>,
    pub enums: HashMap<String, Type<'bump>>,

    pub embedded_symtable: SymbolTable<'bump>,
    pub source: String,
    pub ast: Vec<'bump, Statements<'bump>>,
}

#[derive(Debug, Clone)]
pub struct Include<'bump> {
    pub ast: Vec<'bump, Statements<'bump>>,
}

impl<'bump> Import<'bump> {
    pub fn new(ast: Vec<'bump, Statements<'bump>>, source: &str) -> Self {
        Self {
            functions: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),

            embedded_symtable: SymbolTable::default(),
            source: source.to_owned(),
            ast,
        }
    }

    pub fn add_fn(&mut self, name: String, typ: Type<'bump>) {
        self.functions.insert(name, typ);
    }

    pub fn add_struct(&mut self, name: String, typ: Type<'bump>) {
        self.structs.insert(name, typ);
    }

    pub fn add_enum(&mut self, name: String, typ: Type<'bump>) {
        self.enums.insert(name, typ);
    }

    pub fn get_struct(&self, name: impl std::convert::AsRef<str>) -> Option<Type<'bump>> {
        self.structs.get(name.as_ref()).cloned()
    }

    pub fn get_enum(&self, name: impl std::convert::AsRef<str>) -> Option<Type<'bump>> {
        self.enums.get(name.as_ref()).cloned()
    }

    pub fn get_fn(&self, name: impl std::convert::AsRef<str>) -> Option<Type<'bump>> {
        self.functions.get(name.as_ref()).cloned()
    }
}
