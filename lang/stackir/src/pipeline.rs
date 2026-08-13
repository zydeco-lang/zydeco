use crate::{BranchJoinProgram, ClosureConverter, CpsTranslator, StackirProgram};
use zydeco_surface::scoped::arena::ScopedArena;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum CpsMode {
    #[default]
    Enabled,
    Disabled,
}

/// Prepare branch-join Stack IR for assembly lowering.
pub struct StackirPipeline<'a> {
    scoped: &'a mut ScopedArena,
    cps: CpsMode,
}

impl<'a> StackirPipeline<'a> {
    pub fn new(scoped: &'a mut ScopedArena, cps: CpsMode) -> Self {
        Self { scoped, cps }
    }

    pub fn run(self, stackir: BranchJoinProgram) -> StackirProgram {
        let mut stackir = stackir;
        crate::sps::check::check(stackir.as_program(), self.scoped);

        if self.cps == CpsMode::Enabled {
            stackir = CpsTranslator::new(stackir, self.scoped).translate();
            crate::sps::check::check(stackir.as_program(), self.scoped);
        }

        stackir = ClosureConverter::new(stackir, self.scoped).convert();
        crate::sps::check::check(stackir.as_program(), self.scoped);
        stackir.into_program()
    }
}
