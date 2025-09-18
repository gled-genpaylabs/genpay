use crate::Analyzer;
use genpay_parser::{expressions::Expressions, types::Type};
use std::rc::Rc;

pub use cast::CastMacro;
pub use format::FormatMacro;
pub use panic::PanicMacro;
pub use print::PrintMacro;
pub use println::PrintlnMacro;
pub use sizeof::SizeofMacro;

mod cast;
mod format;
mod panic;
mod print;
mod println;
mod sizeof;

pub trait MacroObject<'bump>: std::fmt::Debug {
    fn verify_call(
        &self,
        analyzer: &Rc<Analyzer<'bump>>,
        arguments: &[Expressions<'bump>],
        span: &(usize, usize),
    ) -> Type<'bump>;
}

/// Enumeration of all existing macros
#[derive(Debug, Clone)]
pub enum CompilerMacros {
    #[allow(dead_code)]
    PrintMacro(PrintMacro),
    #[allow(dead_code)]
    PrintlnMacro(PrintlnMacro),
    #[allow(dead_code)]
    FormatMacro(FormatMacro),
    #[allow(dead_code)]
    PanicMacro(PanicMacro),
    #[allow(dead_code)]
    SizeofMacro(SizeofMacro),
    #[allow(dead_code)]
    CastMacro(CastMacro),
    #[allow(dead_code)]
    None,
}

impl<'bump> MacroObject<'bump> for CompilerMacros {
    fn verify_call(
        &self,
        analyzer: &Rc<Analyzer<'bump>>,
        arguments: &[Expressions<'bump>],
        span: &(usize, usize),
    ) -> Type<'bump> {
        match self {
            CompilerMacros::PrintMacro(instance) => instance.verify_call(analyzer, arguments, span),
            CompilerMacros::PrintlnMacro(instance) => {
                instance.verify_call(analyzer, arguments, span)
            }
            CompilerMacros::FormatMacro(instance) => {
                instance.verify_call(analyzer, arguments, span)
            }
            CompilerMacros::PanicMacro(instance) => instance.verify_call(analyzer, arguments, span),
            CompilerMacros::SizeofMacro(instance) => {
                instance.verify_call(analyzer, arguments, span)
            }
            CompilerMacros::CastMacro(instance) => instance.verify_call(analyzer, arguments, span),
            CompilerMacros::None => Type::Void,
        }
    }
}
