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
#[derive(Default, Debug, derive_more::AddAssign, derive_more::Index)]
pub struct SpanArena {
    spans: ArenaAssoc<EntityId, Span>,
}

mod impl_span_arena {
    use super::*;

    impl SpanArena {
        /// Create empty span storage.
        pub fn new() -> Self {
            Self::default()
        }
        /// Associate an externally issued textual ID with a span.
        pub fn insert_new<Id>(&mut self, id: Id, span: Span)
        where
            Id: Into<EntityId>,
        {
            self.spans.insert_new(id.into(), span);
        }
        /// Iterate over stored spans with their IDs.
        pub fn iter(&self) -> impl Iterator<Item = (&EntityId, &Span)> {
            self.spans.iter()
        }
    }
}
