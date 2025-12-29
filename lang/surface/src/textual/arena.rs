use super::syntax::*;
use derive_more::{AddAssign, Index, IntoIterator};

/* ---------------------------------- Arena --------------------------------- */

/// Parsed nodes keyed by textual IDs.
#[derive(Default, Debug)]
pub struct TextArena {
    pub defs: ArenaAssoc<DefId, VarName>,
    pub pats: ArenaAssoc<PatId, Pattern>,
    pub copats: ArenaAssoc<CoPatId, CoPattern>,
    pub terms: ArenaAssoc<TermId, Term>,
    pub decls: ArenaAssoc<DeclId, Modifiers<Declaration>>,
}

/// Span storage keyed by textual entity IDs.
#[derive(Clone, Debug, AddAssign, Index, IntoIterator)]
pub struct SpanArena(#[into_iterator(owned, ref, ref_mut)] ArenaSparse<EntityId, Span>);

mod impl_span_arena {
    use super::*;

    impl SpanArena {
        /// Create a new span arena with a shared allocator.
        pub fn new(allocator: IndexAlloc<usize>) -> Self {
            Self(ArenaSparse::new(allocator))
        }
        /// Allocate a span and return its textual entity ID.
        pub fn alloc(&mut self, span: Span) -> EntityId {
            self.0.alloc(span)
        }
    }

    impl<'a> SpanArena {
        /// Iterate over stored spans with their IDs.
        pub fn iter(&'a self) -> <&'a Self as IntoIterator>::IntoIter {
            self.into_iter()
        }
    }
}
