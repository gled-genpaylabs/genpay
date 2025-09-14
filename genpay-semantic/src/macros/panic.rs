use super::MacroObject;
use crate::Analyzer;
use genpay_parse_two::{expressions::Expressions, types::Type};

/// **Calls panic exit with formatted message from program**
/// `panic!(LITERAL, ...)` -> `void`
#[derive(Debug, Clone)]
pub struct PanicMacro;
impl MacroObject for PanicMacro {
    fn verify_call(
        &self,
        analyzer: &mut Analyzer,
        arguments: &[Expressions],
        span: &(usize, usize),
    ) -> Type {
        let _ = super::FormatMacro.verify_call(analyzer, arguments, span);
        analyzer.scope.returned = Type::Undefined;

        Type::Void
    }
}
