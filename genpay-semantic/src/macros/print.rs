use super::MacroObject;
use crate::Analyzer;
use bumpalo::Bump;
use genpay_parser::{expressions::Expressions, types::Type};

/// **Prints formatted string to standard output**
/// `print!(LITERAL, ...)` -> `void`
#[derive(Debug, Clone)]
pub struct PrintMacro;
impl<'s> MacroObject<'s> for PrintMacro {
    fn verify_call(
        &self,
        analyzer: &mut Analyzer<'s>,
        arguments: &[Expressions<'s>],
        span: &(usize, usize),
        expr_arena: &'s Bump,
        stmt_arena: &'s Bump,
    ) -> Type<'s> {
        let _ = super::FormatMacro.verify_call(analyzer, arguments, span, expr_arena, stmt_arena);
        Type::Void
    }
}
