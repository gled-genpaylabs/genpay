use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

pub fn position_to_span(span: (usize, usize)) -> SourceSpan {
    let span = (span.0, if span.1 <= span.0 { 1 } else { span.1 - span.0 });
    span.into()
}

impl<'a> From<genpay_parser::error::LexerError<'a>> for SemanticError<'a> {
    fn from(error: genpay_parse_two::error::LexerError<'a>) -> Self {
        SemanticError::ModuleLexerError(error)
    }
}

impl<'a> From<genpay_parser::error::ParserError<'a>> for SemanticError<'a> {
    fn from(error: genpay_parse_two::error::ParserError<'a>) -> Self {
        SemanticError::ModuleParserError(error)
    }
}


#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq)]
pub enum SemanticError<'a> {
    #[error("Lexical Analyzer error")]
    ModuleLexerError(genpay_parser::error::LexerError<'a>),

    #[error("Syntax Analyzer error")]
    ModuleParserError(genpay_parser::error::ParserError<'a>),

    #[error("{message}")]
    #[diagnostic(severity(Error), code(genpay::parser::global_error))]
    GlobalError {
        message: String,
        #[help]
        help: Option<String>,
        src: NamedSource<String>,
    },

    #[error("Argument exception found")]
    #[diagnostic(severity(Error), code(genpay::semantics::argument_exception))]
    ArgumentException {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("This feature is currently disabled by @compiler")]
    #[diagnostic(severity(Error), code(genpay::semantics::disabled_feature))]
    DisabledFeature {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("String format error catched")]
    #[diagnostic(severity(Error), code(genpay::semantics::format_error))]
    FormatError {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Illegal method implementation found")]
    #[diagnostic(
        severity(Error),
        code(genpay::semantics::illegal_implementation),
        url("genpay-docs.vercel.app/advanced/structures-implementations.html")
    )]
    IllegalImplementation {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("IO caused exception")]
    #[diagnostic(severity(Error), code(genpay::semantics::io_error))]
    IoError {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("`main` function limitations violation")]
    #[diagnostic(severity(Error), code(genpay::semantics::main_error))]
    MainFunctionError {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Required fields are missing")]
    #[diagnostic(severity(Error), code(genpay::semantics::missing_fields))]
    MissingFields {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Operator usage caused exception")]
    #[diagnostic(severity(Error), code(genpay::semantics::operator_exception))]
    OperatorException {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Value is out of allowed range")]
    #[diagnostic(severity(Error), code(genoay::semantics::range_overflow))]
    RangeOverflow {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Redefinition of reserved identifier found")]
    #[diagnostic(severity(Error), code(genpay::semantics::redefinition_error))]
    RedefinitionError {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Main semantics rules violation")]
    #[diagnostic(severity(Error), code(genpay::semantics::semantic_error))]
    SemanticalError {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Expression's types mismatched")]
    #[diagnostic(severity(Error), code(genpay::semantics::types_mismatch))]
    TypesMismatch {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Unable to resolve provided name")]
    #[diagnostic(severity(Error), code(genpay::semantics::unresolved_name))]
    UnresolvedName {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Expression is not supported in this context")]
    #[diagnostic(severity(Error), code(genpay::semantics::unsupported_expression))]
    UnsupportedExpression {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Type is not supported here")]
    #[diagnostic(severity(Error), code(genpay::semantics::unsupported_type))]
    UnsupportedType {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Unknown object catched")]
    #[diagnostic(severity(Error), code(genpay::semantics::unknown_object))]
    UnknownObject {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Visibility rules are being violated")]
    #[diagnostic(severity(Error), code(genpay::semantics::visibility_error))]
    VisibilityError {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Value compilation caused error")]
    #[diagnostic(severity(Error), code(genpay::semantics::value_error))]
    ValueError {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },
}

#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq)]
pub enum SemanticWarning<'a> {
    #[error("Unused variable `{varname}` found")]
    #[diagnostic(
        severity(Warning),
        code(genpay::semantics::unused_variable),
        help("Consider removing unused variable")
    )]
    UnusedVariable {
        varname: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("variable is defined here")]
        span: SourceSpan,
    },

    #[error("Unused expression result found")]
    #[diagnostic(
        severity(Warning),
        code(genpay::semantics::unused_result),
        help("Consider assigning result to a variable")
    )]
    UnusedResult {
        message: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{message}")]
        span: SourceSpan,
    },
}
