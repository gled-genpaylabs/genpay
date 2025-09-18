use genpay_parser::types::Type;

#[derive(Debug, Clone)]
pub struct ScopeElement<'bump> {
    pub datatype: Type<'bump>,
    pub public: bool,
}
