use genpay_parser::types::Type;

#[derive(Debug, Clone)]
pub struct ScopeElement<'s> {
    pub datatype: Type<'s>,
}
