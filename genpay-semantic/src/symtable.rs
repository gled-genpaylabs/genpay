use genpay_parser::{statements::Statements, types::Type};
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

/// Symbol Table Structure
#[derive(Debug, Clone, Default)]
pub struct SymbolTable<'s> {
    pub imports: HashMap<&'s str, Import<'s>>,
    pub included: HashMap<&'s str, Include<'s>>,
    pub linked: HashSet<PathBuf>,
}

/// User Import Instance
/// ### Usage
/// ```rust
/// use genpay_parser::{types::Type, statements::Statements};
/// use genpay_semantic::symtable::Import;
///
/// let mut import = Import::new(
///     vec![Statements::None],
///     "source code"
/// );
/// // or with default
/// let mut import = Import::default();
///
/// import.add_fn("func", Type::Undefined);
/// assert!(import.get_fn("func").is_some());
/// ```
#[derive(Debug, Clone)]
pub struct Import<'s> {
    pub functions: HashMap<&'s str, Type<'s>>,
    pub structs: HashMap<&'s str, Type<'s>>,
    pub enums: HashMap<&'s str, Type<'s>>,

    pub embedded_symtable: SymbolTable<'s>,
    pub source: &'s str,
    pub ast: Vec<Statements<'s>>,
}

/// User Include Instance
#[derive(Debug, Clone)]
pub struct Include<'s> {
    pub ast: Vec<Statements<'s>>,
}

impl<'s> Import<'s> {
    pub fn new(ast: Vec<Statements<'s>>, source: &'s str) -> Self {
        Self {
            functions: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),

            embedded_symtable: SymbolTable::default(),
            source,
            ast,
        }
    }

    pub fn add_fn(&mut self, name: &'s str, typ: Type<'s>) {
        self.functions.insert(name, typ);
    }

    pub fn add_struct(&mut self, name: &'s str, typ: Type<'s>) {
        self.structs.insert(name, typ);
    }

    pub fn add_enum(&mut self, name: &'s str, typ: Type<'s>) {
        self.enums.insert(name, typ);
    }

    pub fn get_struct(&self, name: impl std::convert::AsRef<str>) -> Option<Type<'s>> {
        self.structs.get(name.as_ref()).cloned()
    }

    pub fn get_enum(&self, name: impl std::convert::AsRef<str>) -> Option<Type<'s>> {
        self.enums.get(name.as_ref()).cloned()
    }

    pub fn get_fn(&self, name: impl std::convert::AsRef<str>) -> Option<Type<'s>> {
        self.functions.get(name.as_ref()).cloned()
    }
}

impl<'s> Default for Import<'s> {
    fn default() -> Self {
        Self::new(Vec::new(), "")
    }
}
