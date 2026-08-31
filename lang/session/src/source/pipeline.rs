use crate::source::TextualProgram;
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
use zydeco_utils::arena::FrozenArena;
use zydeco_utils::pass::CompilerPass;

pub(crate) struct BitterProgram {
    pub spans: FrozenArena<t::SpanArena>,
    pub arena: FrozenArena<BitterArena>,
    pub prim: PrimTerms,
    pub root: BitterTermId,
}

/// A desugaring failure together with the merged span arena needed to locate it.
#[derive(Debug)]
pub(crate) struct DesugarFailure {
    pub error: DesugarError,
    pub spans: FrozenArena<t::SpanArena>,
}

pub(crate) struct ScopedProgram {
    pub spans: FrozenArena<t::SpanArena>,
    pub arena: FrozenArena<ScopedArena>,
    pub prim: PrimDefs,
    pub root: ScopedTermId,
}

impl TextualProgram {
    pub(crate) fn desugar(self) -> Result<BitterProgram, DesugarFailure> {
        let TextualProgram { spans, arena: textual, unit } = self;
        match SourceUnitDesugarer::new(&spans, &textual, unit).run() {
            | Ok(SourceDesugarOut { arena, prim, root }) => {
                Ok(BitterProgram { spans, arena, prim, root })
            }
            | Err(error) => Err(DesugarFailure { error, spans }),
        }
    }
}

/// A resolution failure together with the merged program's span arena, so
/// diagnostics can still resolve the error's spans after the failure.
#[derive(Debug)]
pub(crate) struct ResolveFailure {
    pub error: Box<ResolveError>,
    pub spans: FrozenArena<t::SpanArena>,
}

impl BitterProgram {
    pub(crate) fn resolve(self) -> Result<ScopedProgram, ResolveFailure> {
        let Self { spans, arena, prim, root } = self;
        let resolved = Resolver::new(&spans, arena, prim).run_source(root);
        match resolved {
            | Ok(ResolveSourceOut { prim, arena, root }) => {
                Ok(ScopedProgram { spans, arena, prim, root })
            }
            | Err(error) => Err(ResolveFailure { error, spans }),
        }
    }
}
