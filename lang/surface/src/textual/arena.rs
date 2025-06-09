use super::syntax::*;

/* ---------------------------------- Arena --------------------------------- */

#[derive(Default, Debug)]
pub struct Arena {
    pub defs: ArenaAssoc<DefId, VarName>,
    pub pats: ArenaAssoc<PatId, Pattern>,
    pub copats: ArenaAssoc<CoPatId, CoPattern>,
    pub terms: ArenaAssoc<TermId, Term>,
    pub decls: ArenaAssoc<DeclId, Modifiers<Declaration>>,
}

#[derive(Clone, Debug, derive_more::AddAssign)]
pub struct SpanArena(ArenaSparse<EntityId, Span>);

mod impl_span_arena {
    use super::*;
    use std::ops::Index;

    impl SpanArena {
        pub fn new(allocator: IndexAlloc<usize>) -> Self {
            Self(ArenaSparse::new(allocator))
        }
        pub fn alloc(&mut self, span: Span) -> EntityId {
            self.0.alloc(span)
        }
    }

    impl Index<&EntityId> for SpanArena {
        type Output = Span;
        fn index(&self, index: &EntityId) -> &Self::Output {
            &self.0[index]
        }
    }
}
