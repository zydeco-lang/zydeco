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

impl TextualProgram {
    pub(crate) fn desugar(self) -> Result<BitterProgram, DesugarError> {
        let TextualProgram { spans, arena: textual, unit } = self;
        let SourceDesugarOut { arena, prim, root } =
            SourceUnitDesugarer::new(&spans, &textual, unit).run()?;
        Ok(BitterProgram { spans, arena, prim, root })
    }
}

/// A resolution failure together with the merged program's span arena, so
/// diagnostics can still resolve the error's spans after the failure.
#[derive(Debug)]
pub(crate) struct ResolveFailure {
    pub error: Box<ResolveError>,
    pub spans: t::SpanArena,
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
