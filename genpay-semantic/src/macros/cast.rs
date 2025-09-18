use super::MacroObject;
use crate::{
    Analyzer,
    error::{self, SemanticError},
};
use genpay_parser::{expressions::Expressions, types::Type};

/// **Converts expression to provided type**
/// `cast!(EXPRESSION, TYPE)` -> `usize`
#[derive(Debug, Clone)]
pub struct CastMacro;
impl<'bump> MacroObject<'bump> for CastMacro {
    fn verify_call(
        &self,
        analyzer: &mut Analyzer<'bump>,
        arguments: &[Expressions<'bump>],
        span: &(usize, usize),
    ) -> Type<'bump> {
        const MINIMUM_ARGUMENTS_LEN: usize = 2;

        if arguments.len() < MINIMUM_ARGUMENTS_LEN {
            let err = SemanticError::ArgumentException {
                exception: format!(
                    "not enough arguments: expected {}, found {}",
                    MINIMUM_ARGUMENTS_LEN,
                    arguments.len()
                ),
                help: None,
                src: (*analyzer.source).clone(),
                span: error::position_to_span(*span),
            };
            analyzer.error(err);
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
            let err = SemanticError::ArgumentException {
                exception: "wrong arguments for casting found".to_string(),
                help: Some("Consider using right syntax: cast!(EXPRESSION, TYPE)".to_string()),
                src: (*analyzer.source).clone(),
                span: error::position_to_span(*span),
            };
            analyzer.error(err);
        }

        let from_type = analyzer.visit_expression(&arguments[0], None);
        let target_type = analyzer.visit_expression(&arguments[1], None);

        let from_type_clone = from_type.clone();
        let target_type_clone = target_type.clone();

        if let Err(err) = analyzer.verify_cast(&from_type_clone, &target_type_clone) {
            let source_clone = (*analyzer.source).clone();
            let span_clone = *span;
            analyzer.error(SemanticError::SemanticalError {
                exception: err,
                help: None,
                src: source_clone,
                span: error::position_to_span(span_clone),
            });
        }

        target_type
    }
}
