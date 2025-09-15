use super::MacroObject;
use crate::Analyzer;
use genpay_parser::{expressions::Expressions, types::Type};

/// **Prints formatted string to standard output with new line**
/// `println!(LITERAL, ...)` -> `void`
#[derive(Debug, Clone)]
pub struct PrintlnMacro;
impl<'bump> MacroObject<'bump> for PrintlnMacro {
    fn verify_call(
        &self,
        analyzer: &mut Analyzer<'bump>,
        arguments: &[Expressions<'bump>],
        span: &(usize, usize),
    ) -> Type<'bump> {
        let _ = super::FormatMacro.verify_call(analyzer, arguments, span);
        Type::Void
    }
}
