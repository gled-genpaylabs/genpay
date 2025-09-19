use bumpalo::Bump;
use genpay_hir::{HirProgram, HirStatement};
use genpay_parser::statements::Statements;

pub fn lower<'a, 'b>(
    ast: &'a [Statements<'b>],
    bump: &'a Bump,
) -> HirProgram<'a> {
    let statements = bump.alloc_slice_fill_iter(
        ast.iter().map(|_| HirStatement::Placeholder)
    );
    HirProgram { statements }
}
