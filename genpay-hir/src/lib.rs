#[derive(Debug, Clone, PartialEq)]
pub enum HirStatement {
    // For now, we'll just have a placeholder
    Placeholder,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirProgram<'a> {
    pub statements: &'a [HirStatement],
}
