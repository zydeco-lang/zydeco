use crate::{BranchJoinProgram, SpsLowConverter, SpsLowProgram};
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::scoped::arena::ScopedArena;

/// Convert branch-join high SPS into first-order SPSLow.
pub struct SpsLowPipeline<'a> {
    scoped: &'a ScopedArena,
    statics: &'a StaticsArena,
}

impl<'a> SpsLowPipeline<'a> {
    pub fn new(scoped: &'a ScopedArena, statics: &'a StaticsArena) -> Self {
        Self { scoped, statics }
    }

    pub fn run(self, stackir: BranchJoinProgram) -> SpsLowProgram {
        crate::sps::check::check(stackir.as_program(), self.scoped, self.statics);
        SpsLowConverter::new(stackir, self.scoped, self.statics).convert()
    }
}
