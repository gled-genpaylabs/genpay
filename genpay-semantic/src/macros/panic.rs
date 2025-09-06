use super::MacroObject;
use crate::Analyzer;
use bumpalo::Bump;
use genpay_parser::{expressions::Expressions, types::Type};

/// **Calls panic exit with formatted message from program**
/// `panic!(LITERAL, ...)` -> `void`
#[derive(Debug, Clone)]
pub struct PanicMacro;
impl<'s> MacroObject<'s> for PanicMacro {
    fn verify_call(
        &self,
        analyzer: &mut Analyzer<'s>,
        arguments: &[Expressions<'s>],
        span: &(usize, usize),
        arena: &'s Bump,
    ) -> Type<'s> {
        let _ = super::FormatMacro.verify_call(analyzer, arguments, span, arena);
        analyzer.scope.returned = Type::Undefined;

        Type::Void
    }
}
