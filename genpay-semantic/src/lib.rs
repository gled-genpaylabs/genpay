use crate::{
    error::{SemanticError, SemanticWarning},
    macros::{CastMacro, FormatMacro, PanicMacro, PrintMacro, PrintlnMacro, SizeofMacro},
    macros::CompilerMacros,
    scope::Scope,
    symtable::SymbolTable,
};
use genpay_parser::statements::Statements;
use miette::NamedSource;
use std::{collections::HashMap, path::PathBuf};

mod element;
mod error;
mod macros;
mod scope;
pub mod visitor;
/// Semantic Analyzer Symbol Table
pub mod symtable;

pub type SemanticOk = (SymbolTable, Vec<SemanticWarning>);
pub type SemanticErr = (Vec<SemanticError>, Vec<SemanticWarning>);

const STANDARD_LIBRARY_VAR: &str = "GENPAY_LIB";

/// Main Analyzer Struct
#[derive(Debug)]
pub struct Analyzer {
    scope: Scope,
    source: NamedSource<String>,
    source_path: PathBuf,

    errors: Vec<SemanticError>,
    warnings: Vec<SemanticWarning>,

    symtable: SymbolTable,
    compiler_macros: HashMap<String, CompilerMacros>,
}

impl Analyzer {
    pub fn new(src: &str, filename: &str, source_path: PathBuf, is_main: bool) -> Self {
        let compiler_macros = HashMap::from([
            (
                String::from("print"),
                CompilerMacros::PrintMacro(PrintMacro),
            ),
            (
                String::from("println"),
                CompilerMacros::PrintlnMacro(PrintlnMacro),
            ),
            (
                String::from("format"),
                CompilerMacros::FormatMacro(FormatMacro),
            ),
            (
                String::from("panic"),
                CompilerMacros::PanicMacro(PanicMacro),
            ),
            (
                String::from("sizeof"),
                CompilerMacros::SizeofMacro(SizeofMacro),
            ),
            (String::from("cast"), CompilerMacros::CastMacro(CastMacro)),
        ]);

        Analyzer {
            scope: {
                let mut scope = Scope::new();
                scope.is_main = is_main;
                scope
            },
            source: NamedSource::new(filename, src.to_owned()),
            source_path,

            errors: Vec::new(),
            warnings: Vec::new(),

            symtable: SymbolTable::default(),
            compiler_macros,
        }
    }

    pub fn analyze(&mut self, ast: &[Statements]) -> Result<SemanticOk, SemanticErr> {
        ast.iter().for_each(|stmt| self.visit_statement(stmt));

        if self.scope.get_fn("main").is_none() && self.scope.is_main {
            let err = SemanticError::GlobalError {
                message: "Program has no entry `main` function".to_string(),
                help: Some("Consider creating main function: `fn main()) {}`".to_string()),
                src: self.source.clone(),
            };

            self.errors.reverse();
            self.errors.push(err);
            self.errors.reverse();
        }

        if let Some(unused) = self.scope.check_unused_variables() {
            unused.iter().for_each(|var| {
                self.warning(SemanticWarning::UnusedVariable {
                    varname: var.0.clone(),
                    src: self.source.clone(),
                    span: error::position_to_span(var.1),
                });
            });
        }

        if !self.errors.is_empty() {
            return Err((self.errors.clone(), self.warnings.clone()));
        }

        Ok((self.symtable.clone(), self.warnings.clone()))
    }

    fn error(&mut self, error: SemanticError) {
        self.errors.push(error);
    }

    #[allow(unused)]
    fn warning(&mut self, warning: SemanticWarning) {
        self.warnings.push(warning)
    }
}

