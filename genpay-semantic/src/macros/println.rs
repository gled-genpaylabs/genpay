use super::MacroObject;
use crate::Analyzer;
use bumpalo::Bump;
use genpay_parser::{expressions::Expressions, types::Type};

/// **Prints formatted string to standard output with new line**
/// `println!(LITERAL, ...)` -> `void`
#[derive(Debug, Clone)]
pub struct PrintlnMacro;
impl<'s> MacroObject<'s> for PrintlnMacro {
    fn verify_call(
        &self,
        analyzer: &mut Analyzer<'s>,
        arguments: &[Expressions<'s>],
        span: &(usize, usize),
        arena: &'s Bump,
    ) -> Type<'s> {
        let _ = super::FormatMacro.verify_call(analyzer, arguments, span, arena);
        Type::Void
    }
}
