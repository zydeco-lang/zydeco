use super::syntax::*;

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
#[derive(Debug)]
pub struct SpanArena {
    key_space: KeySpace,
    spans: ArenaAssoc<EntityId, Span>,
}

mod impl_span_arena {
    use super::*;

    impl SpanArena {
        /// Create a span arena that owns the given key space.
        pub fn new(key_space: KeySpace) -> Self {
            Self { key_space, spans: ArenaAssoc::new() }
        }
        /// Allocate a typed textual ID and associate its tagged form with a span.
        pub fn alloc<Id>(&mut self, span: Span) -> Id
        where
            Id: ArenaId + Into<EntityId>,
        {
            let id: Id = self.key_space.alloc();
            self.spans.insert_new(id.into(), span);
            id
        }
        /// Iterate over stored spans with their IDs.
        pub fn iter(&self) -> impl Iterator<Item = (&EntityId, &Span)> {
            self.spans.iter()
        }
    }

    impl std::ops::Index<&EntityId> for SpanArena {
        type Output = Span;
        fn index(&self, id: &EntityId) -> &Self::Output {
            &self.spans[id]
        }
    }

    impl std::ops::AddAssign for SpanArena {
        fn add_assign(&mut self, rhs: Self) {
            self.spans += rhs.spans;
        }
    }
}
