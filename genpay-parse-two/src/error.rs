use miette::{Diagnostic, NamedSource};
use thiserror::Error;

#[derive(Clone, Debug, Error, Diagnostic, PartialEq, Eq)]
#[error("{exception}")]
pub struct ParserError {
    pub exception: String,

    #[help]
    pub help: String,

    #[source_code]
    pub src: NamedSource<String>,

    #[label("here")]
    pub span: (usize, usize),
}

#[derive(Clone, Debug, Error, Diagnostic, PartialEq, Eq)]
#[error("{message}")]
pub struct ParserWarning {
    pub message: String,

    #[source_code]
    pub src: NamedSource<String>,

    #[label("here")]
    pub span: (usize, usize),
}
