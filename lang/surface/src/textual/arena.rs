use super::syntax::*;
use derive_more::{AddAssign, Index, IntoIterator};

/* ---------------------------------- Arena --------------------------------- */

#[derive(Default, Debug)]
pub struct TextArena {
    pub defs: ArenaAssoc<DefId, VarName>,
    pub pats: ArenaAssoc<PatId, Pattern>,
    pub copats: ArenaAssoc<CoPatId, CoPattern>,
    pub terms: ArenaAssoc<TermId, Term>,
    pub decls: ArenaAssoc<DeclId, Modifiers<Declaration>>,
}

#[derive(Clone, Debug, AddAssign, Index, IntoIterator)]
pub struct SpanArena(#[into_iterator(owned, ref, ref_mut)] ArenaSparse<EntityId, Span>);

mod impl_span_arena {
    use super::*;

    impl SpanArena {
        pub fn new(allocator: IndexAlloc<usize>) -> Self {
            Self(ArenaSparse::new(allocator))
        }
        pub fn alloc(&mut self, span: Span) -> EntityId {
            self.0.alloc(span)
        }
    }

    impl<'a> SpanArena {
        pub fn iter(&'a self) -> <&'a Self as IntoIterator>::IntoIter {
            self.into_iter()
        }
    }
}
