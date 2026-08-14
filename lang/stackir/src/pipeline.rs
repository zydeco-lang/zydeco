use crate::{BranchJoinProgram, SpsLowConverter, SpsLowProgram};
use zydeco_surface::scoped::arena::ScopedArena;

/// Convert branch-join high SPS into first-order SPSLow.
pub struct SpsLowPipeline<'a> {
    scoped: &'a mut ScopedArena,
}

impl<'a> SpsLowPipeline<'a> {
    pub fn new(scoped: &'a mut ScopedArena) -> Self {
        Self { scoped }
    }

    pub fn run(self, stackir: BranchJoinProgram) -> SpsLowProgram {
        crate::sps::check::check(stackir.as_program(), self.scoped);
        SpsLowConverter::new(stackir, self.scoped).convert()
    }
}
