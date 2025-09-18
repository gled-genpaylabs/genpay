use crate::{
    error::{SemanticError, SemanticWarning},
    scope::Scope,
    symtable::SymbolTable,
};
use bumpalo::Bump;
use genpay_parser::statements::Statements;
use miette::NamedSource;
use std::{collections::HashMap, path::PathBuf, rc::Rc};

mod element;
mod error;
mod macros;
mod scope;
/// Semantic Analyzer Symbol Table
pub mod symtable;
pub mod visitor;

pub type SemanticOk<'bump> = (SymbolTable<'bump>, Vec<SemanticWarning>);
pub type SemanticErr = (Vec<SemanticError>, Vec<SemanticWarning>);

/// Main Analyzer Struct
#[derive(Debug)]
pub struct Analyzer<'bump> {
    pub scope: Scope<'bump>,
    pub source: Rc<NamedSource<String>>,
    pub source_path: PathBuf,

    pub errors: Vec<SemanticError>,
    pub warnings: Vec<SemanticWarning>,

    pub symtable: SymbolTable<'bump>,
    pub compiler_macros: HashMap<String, Rc<dyn macros::MacroObject<'bump>>>,
    pub bump: &'bump Bump,
}

impl<'bump> Analyzer<'bump> {
    pub fn new(
        src: &str,
        filename: &str,
        source_path: PathBuf,
        is_main: bool,
        bump: &'bump Bump,
    ) -> Self {
        let mut analyzer = Analyzer {
            scope: {
                let mut scope = Scope::new();
                scope.is_main = is_main;
                scope
            },
            source: Rc::new(NamedSource::new(filename, src.to_owned())),
            source_path,

            errors: Vec::new(),
            warnings: Vec::new(),

            symtable: SymbolTable::default(),
            compiler_macros: HashMap::new(),
            bump,
        };

        analyzer.compiler_macros.insert("print".to_string(), Rc::new(macros::PrintMacro));
        analyzer.compiler_macros.insert("println".to_string(), Rc::new(macros::PrintlnMacro));
        analyzer.compiler_macros.insert("format".to_string(), Rc::new(macros::FormatMacro));
        analyzer.compiler_macros.insert("panic".to_string(), Rc::new(macros::PanicMacro));
        analyzer.compiler_macros.insert("sizeof".to_string(), Rc::new(macros::SizeofMacro));
        analyzer.compiler_macros.insert("cast".to_string(), Rc::new(macros::CastMacro));

        analyzer
    }

    pub fn analyze(
        &mut self,
        ast: &[Statements<'bump>],
    ) -> Result<SemanticOk<'bump>, SemanticErr> {
        for stmt in ast {
            self.visit_statement(stmt);
        }

        if self.scope.get_fn("main").is_none() && self.scope.is_main {
            let err = SemanticError::GlobalError {
                message: "Program has no entry `main` function".to_string(),
                help: Some("Consider creating main function: `fn main()) {}`".to_string()),
                src: (*self.source).clone(),
            };

            self.errors.reverse();
            self.errors.push(err);
            self.errors.reverse();
        }

        if let Some(unused) = self.scope.check_unused_variables() {
            for var in unused {
                self.warning(SemanticWarning::UnusedVariable {
                    varname: var.0.clone(),
                    src: (*self.source).clone(),
                    span: error::position_to_span(var.1),
                });
            }
        }

        if !self.errors.is_empty() {
            return Err((self.errors.clone(), self.warnings.clone()));
        }

        Ok((
            self.symtable.clone(),
            self.warnings.clone(),
        ))
    }

    pub fn error(&mut self, error: SemanticError) {
        self.errors.push(error);
    }

    #[allow(unused)]
    pub fn warning(&mut self, warning: SemanticWarning) {
        self.warnings.push(warning)
    }
}
