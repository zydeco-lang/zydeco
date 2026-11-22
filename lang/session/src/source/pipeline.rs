use super::TextualProgram;
use zydeco_foundation::prelude::FrozenArena;
use zydeco_surface::{
    scoped::{ResolveErrors, ResolveFolder, ResolveSourceOut, arena::ScopedArena, syntax::TermId},
    textual::syntax::SpanArena,
};

pub(crate) struct ScopedProgram {
    pub(crate) spans: FrozenArena<SpanArena>,
    pub(crate) arena: FrozenArena<ScopedArena>,
    pub(crate) root: TermId,
}

#[derive(Debug)]
pub(crate) struct ResolveFailure {
    pub error: Box<ResolveErrors>,
    pub spans: FrozenArena<SpanArena>,
}

impl TextualProgram {
    pub(crate) fn resolve(self) -> Result<ScopedProgram, ResolveFailure> {
        let Self { spans, arena, unit } = self;
        match ResolveFolder::new(&spans, &arena).run_source(unit.root) {
            | Ok(ResolveSourceOut { arena, root }) => Ok(ScopedProgram { spans, arena, root }),
            | Err(error) => Err(ResolveFailure { error, spans }),
        }
    }
}
