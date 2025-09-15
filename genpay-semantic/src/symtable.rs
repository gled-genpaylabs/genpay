use genpay_parser::{statements::Statements, types::Type};
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

#[derive(Debug, Clone)]
pub struct SymbolTable<'bump> {
    pub imports: HashMap<&'bump str, Import<'bump>>,
    pub included: HashMap<&'bump str, Include<'bump>>,
    pub linked: HashSet<PathBuf>,
}

impl<'bump> Default for SymbolTable<'bump> {
    fn default() -> Self {
        Self {
            imports: HashMap::new(),
            included: HashMap::new(),
            linked: HashSet::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Import<'bump> {
    pub functions: HashMap<&'bump str, Type<'bump>>,
    pub structs: HashMap<&'bump str, Type<'bump>>,
    pub enums: HashMap<&'bump str, Type<'bump>>,

    pub embedded_symtable: SymbolTable<'bump>,
    pub source: &'bump str,
    pub ast: bumpalo::collections::vec::Vec<'bump, Statements<'bump>>,
}

#[derive(Debug, Clone)]
pub struct Include<'bump> {
    pub ast: bumpalo::collections::vec::Vec<'bump, Statements<'bump>>,
}

impl<'bump> Import<'bump> {
    pub fn new(
        ast: bumpalo::collections::vec::Vec<'bump, Statements<'bump>>,
        source: &'bump str,
    ) -> Self {
        Self {
            functions: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),

            embedded_symtable: SymbolTable::default(),
            source,
            ast,
        }
    }

    pub fn add_fn(&mut self, name: &'bump str, typ: Type<'bump>) {
        self.functions.insert(name, typ);
    }

    pub fn add_struct(&mut self, name: &'bump str, typ: Type<'bump>) {
        self.structs.insert(name, typ);
    }

    pub fn add_enum(&mut self, name: &'bump str, typ: Type<'bump>) {
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
