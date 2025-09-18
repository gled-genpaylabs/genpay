use super::MacroObject;
use crate::Analyzer;
use genpay_parser::{expressions::Expressions, types::Type};
use std::rc::Rc;

/// **Prints formatted string to standard output**
/// `print!(LITERAL, ...)` -> `void`
#[derive(Debug, Clone)]
pub struct PrintMacro;
impl<'bump> MacroObject<'bump> for PrintMacro {
    fn verify_call(
        &self,
        analyzer: &Rc<Analyzer<'bump>>,
        arguments: &[Expressions<'bump>],
        span: &(usize, usize),
    ) -> Type<'bump> {
        let _ = super::FormatMacro.verify_call(analyzer, arguments, span);
        Type::Void
    }
}
