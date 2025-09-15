use super::MacroObject;
use crate::{
    Analyzer,
    error::{self, SemanticError},
};
use genpay_parser::{expressions::Expressions, types::Type};

/// **Returns size of provided type / expression**
/// `sizeof!(TYPE / EXPRESSION)` -> `usize`
#[derive(Debug, Clone)]
pub struct SizeofMacro;
impl<'bump> MacroObject<'bump> for SizeofMacro {
    fn verify_call(
        &self,
        analyzer: &mut Analyzer<'bump>,
        arguments: &[Expressions<'bump>],
        span: &(usize, usize),
    ) -> Type<'bump> {
        const MINIMUM_ARGUMENTS_LEN: usize = 1;
        const RETURN_TYPE: Type = Type::USIZE;

        if arguments.len() < MINIMUM_ARGUMENTS_LEN {
            analyzer.error(SemanticError::ArgumentException {
                exception: format!(
                    "not enough arguments: expected {}, found {}",
                    MINIMUM_ARGUMENTS_LEN,
                    arguments.len()
                ),
                help: None,
                src: analyzer.source.clone(),
                span: error::position_to_span(*span),
            });
        }

        RETURN_TYPE
    }
}
