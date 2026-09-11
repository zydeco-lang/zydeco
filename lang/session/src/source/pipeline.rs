use crate::source::TextualProgram;
use zydeco_surface::{
    bitter::{
        DesugarError, SourceDesugarOut, SourceUnitDesugarer, arena::BitterArena,
        syntax::TermId as BitterTermId,
    },
    scoped::{
        ResolveError, ResolveSourceOut, Resolver, arena::ScopedArena,
        syntax::TermId as ScopedTermId,
    },
    textual::syntax as t,
};
use zydeco_utils::arena::FrozenArena;
use zydeco_utils::pass::CompilerPass;

pub(crate) struct BitterProgram {
    pub spans: FrozenArena<t::SpanArena>,
    pub arena: FrozenArena<BitterArena>,
    pub root: BitterTermId,
}

/// A desugaring failure together with the merged span arena needed to locate it.
#[derive(Debug)]
pub(crate) struct DesugarFailure {
    pub error: Box<DesugarError>,
    pub spans: FrozenArena<t::SpanArena>,
}

pub(crate) struct ScopedProgram {
    pub spans: FrozenArena<t::SpanArena>,
    pub arena: FrozenArena<ScopedArena>,
    pub root: ScopedTermId,
}

impl TextualProgram {
    pub(crate) fn desugar(self) -> Result<BitterProgram, DesugarFailure> {
        let TextualProgram { spans, arena: textual, unit } = self;
        match SourceUnitDesugarer::new(&spans, &textual, unit).run() {
            | Ok(SourceDesugarOut { arena, root }) => Ok(BitterProgram { spans, arena, root }),
            | Err(error) => Err(DesugarFailure { error: Box::new(error), spans }),
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
        let Self { spans, arena, root } = self;
        let resolved = Resolver::new(&spans, arena).run_source(root);
        match resolved {
            | Ok(ResolveSourceOut { arena, root }) => Ok(ScopedProgram { spans, arena, root }),
            | Err(error) => Err(ResolveFailure { error, spans }),
        }
    }
}
