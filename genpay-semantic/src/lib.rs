use crate::{
    error::{SemanticError, SemanticWarning},
    scope::Scope,
    symtable::SymbolTable,
};
use bumpalo::Bump;
use genpay_parser::statements::Statements;
use miette::NamedSource;
use std::{cell::RefCell, collections::HashMap, path::PathBuf, rc::Rc};

mod element;
mod error;
mod macros;
mod scope;
pub mod visitor;
/// Semantic Analyzer Symbol Table
pub mod symtable;

pub type SemanticOk<'bump> = (SymbolTable<'bump>, Vec<SemanticWarning>);
pub type SemanticErr = (Vec<SemanticError>, Vec<SemanticWarning>);

const STANDARD_LIBRARY_VAR: &str = "GENPAY_LIB";

/// Main Analyzer Struct
#[derive(Debug)]
pub struct Analyzer<'bump> {
    pub scope: RefCell<Scope<'bump>>,
    pub source: Rc<NamedSource<String>>,
    pub source_path: PathBuf,

    pub errors: RefCell<Vec<SemanticError>>,
    pub warnings: RefCell<Vec<SemanticWarning>>,

    pub symtable: RefCell<SymbolTable<'bump>>,
    pub compiler_macros: RefCell<HashMap<String, Rc<dyn macros::MacroObject<'bump>>>>,
    pub bump: &'bump Bump,
}

impl<'bump> Analyzer<'bump> {
    pub fn new(
        src: &str,
        filename: &str,
        source_path: PathBuf,
        is_main: bool,
        bump: &'bump Bump,
    ) -> Rc<Self> {
        let analyzer = Rc::new(Analyzer {
            scope: {
                let mut scope = Scope::new();
                scope.is_main = is_main;
                RefCell::new(scope)
            },
            source: Rc::new(NamedSource::new(filename, src.to_owned())),
            source_path,

            errors: RefCell::new(Vec::new()),
            warnings: RefCell::new(Vec::new()),

            symtable: RefCell::new(SymbolTable::default()),
            compiler_macros: RefCell::new(HashMap::new()),
            bump,
        });

        let mut macros = analyzer.compiler_macros.borrow_mut();
        macros.insert(
            "print".to_string(),
            Rc::new(macros::PrintMacro),
        );
        macros.insert(
            "println".to_string(),
            Rc::new(macros::PrintlnMacro),
        );
        macros.insert(
            "format".to_string(),
            Rc::new(macros::FormatMacro),
        );
        macros.insert(
            "panic".to_string(),
            Rc::new(macros::PanicMacro),
        );
        macros.insert(
            "sizeof".to_string(),
            Rc::new(macros::SizeofMacro),
        );
        macros.insert(
            "cast".to_string(),
            Rc::new(macros::CastMacro),
        );

        analyzer.clone()
    }

    pub fn analyze(
        self: &Rc<Self>,
        ast: &[Statements<'bump>],
    ) -> Result<SemanticOk<'bump>, SemanticErr> {
        for stmt in ast {
            self.visit_statement(stmt);
        }

        if self.scope.borrow().get_fn("main").is_none() && self.scope.borrow().is_main {
            let err = SemanticError::GlobalError {
                message: "Program has no entry `main` function".to_string(),
                help: Some("Consider creating main function: `fn main()) {}`".to_string()),
                src: (*self.source).clone(),
            };

            self.errors.borrow_mut().reverse();
            self.errors.borrow_mut().push(err);
            self.errors.borrow_mut().reverse();
        }

        if let Some(unused) = self.scope.borrow_mut().check_unused_variables() {
            unused.iter().for_each(|var| {
                self.warning(SemanticWarning::UnusedVariable {
                    varname: var.0.clone(),
                    src: (*self.source).clone(),
                    span: error::position_to_span(var.1),
                });
            });
        }

        if !self.errors.borrow().is_empty() {
            return Err((
                self.errors.borrow().clone(),
                self.warnings.borrow().clone(),
            ));
        }

        Ok((
            self.symtable.borrow().clone(),
            self.warnings.borrow().clone(),
        ))
    }

    pub fn error(&self, error: SemanticError) {
        self.errors.borrow_mut().push(error);
    }

    #[allow(unused)]
    pub fn warning(&self, warning: SemanticWarning) {
        self.warnings.borrow_mut().push(warning)
    }
}
