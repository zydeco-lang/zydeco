use crate::source::ProgramAssembly;
use zydeco_surface::{
    bitter::{
        DesugarError, SourceDesugarOut, SourceUnitDesugarer,
        arena::BitterArena,
        syntax::{PrimTerms, TermId as BitterTermId},
    },
    scoped::{
        ResolveError, ResolveSourceOut, Resolver,
        arena::ScopedArena,
        syntax::{PrimDefs, TermId as ScopedTermId},
    },
    textual::syntax as t,
};
use zydeco_utils::pass::CompilerPass;

pub(crate) struct BitterProgram {
    pub spans: t::SpanArena,
    pub arena: BitterArena,
    pub prim: PrimTerms,
    pub root: BitterTermId,
}

pub(crate) struct ScopedProgram {
    pub spans: t::SpanArena,
    pub arena: ScopedArena,
    pub prim: PrimDefs,
    pub root: ScopedTermId,
}

impl ProgramAssembly {
    pub(crate) fn desugar(self) -> Result<BitterProgram, DesugarError> {
        let ProgramAssembly { spans, arena: textual, unit } = self;
        let SourceDesugarOut { arena, prim, root } =
            SourceUnitDesugarer::new(&spans, &textual, unit, Default::default()).run()?;
        Ok(BitterProgram { spans, arena, prim, root })
    }
}

impl BitterProgram {
    pub(crate) fn resolve(self) -> Result<ScopedProgram, Box<ResolveError>> {
        let Self { spans, arena, prim, root } = self;
        let ResolveSourceOut { prim, arena, root } =
            Resolver::new(&spans, arena, prim).run_source(root)?;
        Ok(ScopedProgram { spans, arena, prim, root })
    }
}
