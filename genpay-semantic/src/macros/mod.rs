use crate::Analyzer;
use genpay_parser::{expressions::Expressions, types::Type};

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

use bumpalo::Bump;

pub trait MacroObject<'s>: std::fmt::Debug {
    fn verify_call(
        &self,
        analyzer: &mut Analyzer<'s>,
        arguments: &[Expressions<'s>],
        span: &(usize, usize),
        arena: &'s Bump,
    ) -> Type<'s>;
}

/// Enumeration of all existing macros
#[derive(Debug, Clone)]
pub enum CompilerMacros {
    PrintMacro(PrintMacro),
    PrintlnMacro(PrintlnMacro),
    FormatMacro(FormatMacro),
    PanicMacro(PanicMacro),
    SizeofMacro(SizeofMacro),
    CastMacro(CastMacro),
    None,
}

impl<'s> MacroObject<'s> for CompilerMacros {
    fn verify_call(
        &self,
        analyzer: &mut Analyzer<'s>,
        arguments: &[Expressions<'s>],
        span: &(usize, usize),
        arena: &'s Bump,
    ) -> Type<'s> {
        match self {
            CompilerMacros::PrintMacro(instance) => {
                instance.verify_call(analyzer, arguments, span, arena)
            }
            CompilerMacros::PrintlnMacro(instance) => {
                instance.verify_call(analyzer, arguments, span, arena)
            }
            CompilerMacros::FormatMacro(instance) => {
                instance.verify_call(analyzer, arguments, span, arena)
            }
            CompilerMacros::PanicMacro(instance) => {
                instance.verify_call(analyzer, arguments, span, arena)
            }
            CompilerMacros::SizeofMacro(instance) => {
                instance.verify_call(analyzer, arguments, span, arena)
            }
            CompilerMacros::CastMacro(instance) => {
                instance.verify_call(analyzer, arguments, span, arena)
            }
            CompilerMacros::None => Type::Void,
        }
    }
}
