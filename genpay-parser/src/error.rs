use miette::{Diagnostic, NamedSource};
use thiserror::Error;

#[derive(Clone, Debug, Error, Diagnostic, PartialEq, Eq)]
#[error("{exception}")]
pub struct ParserError<'a> {
    pub exception: &'a str,

    #[help]
    pub help: &'a str,

    #[source_code]
    pub src: NamedSource<String>,

    #[label("here")]
    pub span: (usize, usize),
}

#[derive(Clone, Debug, Error, Diagnostic, PartialEq, Eq)]
#[error("{message}")]
pub struct ParserWarning<'a> {
    pub message: &'a str,

    #[source_code]
    pub src: NamedSource<String>,

    #[label("here")]
    pub span: (usize, usize),
}
