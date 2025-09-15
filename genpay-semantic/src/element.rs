use genpay_parser::types::Type;

#[derive(Debug, Clone)]
pub struct ScopeElement<'a> {
    pub datatype: Type<'a>,
    pub public: bool,
}
