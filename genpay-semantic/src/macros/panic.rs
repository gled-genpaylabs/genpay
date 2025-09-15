use super::MacroObject;
use crate::Analyzer;
use genpay_parser::{expressions::Expressions, types::Type};

/// **Calls panic exit with formatted message from program**
/// `panic!(LITERAL, ...)` -> `void`
#[derive(Debug, Clone)]
pub struct PanicMacro;
impl<'bump> MacroObject<'bump> for PanicMacro {
    fn verify_call(
        &self,
        analyzer: &mut Analyzer<'bump>,
        arguments: &[Expressions<'bump>],
        span: &(usize, usize),
    ) -> Type<'bump> {
        let _ = super::FormatMacro.verify_call(analyzer, arguments, span);
        analyzer.scope.returned = Type::Undefined;

        Type::Void
    }
}
