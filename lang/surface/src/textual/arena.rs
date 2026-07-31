use super::syntax::*;

/* ---------------------------------- Arena --------------------------------- */

/// Allocation and storage scope for parsed textual syntax.
#[derive(Debug)]
pub enum TextualScope {}

impl Allocates<DefId> for TextualScope {}
impl Allocates<PatId> for TextualScope {}
impl Allocates<CoPatId> for TextualScope {}
impl Allocates<TermId> for TextualScope {}

impl ArenaSchema<DefId> for TextualScope {
    type Item = VarName;
}
impl ArenaSchema<PatId> for TextualScope {
    type Item = Pattern;
}
impl ArenaSchema<CoPatId> for TextualScope {
    type Item = CoPattern;
}
impl ArenaSchema<TermId> for TextualScope {
    type Item = Term;
}
/// Parsed nodes keyed by textual IDs.
#[derive(Default, Debug)]
pub struct TextArena {
    pub defs: ArenaSparse<TextualScope, DefId>,
    pub pats: ArenaSparse<TextualScope, PatId>,
    pub copats: ArenaSparse<TextualScope, CoPatId>,
    pub terms: ArenaSparse<TextualScope, TermId>,
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
