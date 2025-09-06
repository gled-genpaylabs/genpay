use super::MacroObject;
use crate::{
    Analyzer,
    error::{self, SemanticError},
};
use bumpalo::Bump;
use genpay_parser::{expressions::Expressions, types::Type};

/// **Converts expression to provided type**
/// `cast!(EXPRESSION, TYPE)` -> `usize`
#[derive(Debug, Clone)]
pub struct CastMacro;
impl<'s> MacroObject<'s> for CastMacro {
    fn verify_call(
        &self,
        analyzer: &mut Analyzer<'s>,
        arguments: &[Expressions<'s>],
        span: &(usize, usize),
        arena: &'s Bump,
    ) -> Type<'s> {
        const MINIMUM_ARGUMENTS_LEN: usize = 2;

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

        if matches!(
            arguments.first(),
            Some(Expressions::Argument {
                name: _,
                r#type: _,
                span: _
            })
        ) || !matches!(
            arguments.get(1),
            Some(Expressions::Argument {
                name: _,
                r#type: _,
                span: _
            })
        ) {
            analyzer.error(SemanticError::ArgumentException {
                exception: "wrong arguments for casting found".to_string(),
                help: Some("Consider using right syntax: cast!(EXPRESSION, TYPE)".to_string()),
                src: analyzer.source.clone(),
                span: error::position_to_span(*span),
            });
        }

        let from_type = analyzer.visit_expression(&arguments[0], None, arena);
        let target_type = analyzer.visit_expression(&arguments[1], None, arena);

        analyzer
            .verify_cast(&from_type, &target_type)
            .unwrap_or_else(|err| {
                analyzer.error(SemanticError::SemanticalError {
                    exception: err,
                    help: None,
                    src: analyzer.source.clone(),
                    span: error::position_to_span(*span),
                });
            });

        target_type
    }
}
