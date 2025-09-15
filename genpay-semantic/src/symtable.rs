use genpay_parser::{statements::Statements, types::Type};
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

#[derive(Debug, Clone, Default)]
pub struct SymbolTable<'a> {
    pub imports: HashMap<String, Import<'a>>,
    pub included: HashMap<String, Include<'a>>,
    pub linked: HashSet<PathBuf>,
}

#[derive(Debug, Clone)]
pub struct Import<'a> {
    pub functions: HashMap<String, Type<'a>>,
    pub structs: HashMap<String, Type<'a>>,
    pub enums: HashMap<String, Type<'a>>,

    pub embedded_symtable: SymbolTable<'a>,
    pub source: String,
    pub ast: Vec<Statements<'a>>,
}

#[derive(Debug, Clone)]
pub struct Include<'a> {
    pub ast: Vec<Statements<'a>>,
}

impl<'a> Import<'a> {
    pub fn new(ast: Vec<Statements<'a>>, source: &str) -> Self {
        Self {
            functions: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),

            embedded_symtable: SymbolTable::default(),
            source: source.to_owned(),
            ast,
        }
    }

    pub fn add_fn(&mut self, name: String, typ: Type<'a>) {
        self.functions.insert(name, typ);
    }

    pub fn add_struct(&mut self, name: String, typ: Type<'a>) {
        self.structs.insert(name, typ);
    }

    pub fn add_enum(&mut self, name: String, typ: Type<'a>) {
        self.enums.insert(name, typ);
    }

    pub fn get_struct(&self, name: impl std::convert::AsRef<str>) -> Option<Type<'a>> {
        self.structs.get(name.as_ref()).cloned()
    }

    pub fn get_enum(&self, name: impl std::convert::AsRef<str>) -> Option<Type<'a>> {
        self.enums.get(name.as_ref()).cloned()
    }

    pub fn get_fn(&self, name: impl std::convert::AsRef<str>) -> Option<Type<'a>> {
        self.functions.get(name.as_ref()).cloned()
    }
}

impl<'a> Default for Import<'a> {
    fn default() -> Self {
        Self::new(Vec::new(), "")
    }
}
