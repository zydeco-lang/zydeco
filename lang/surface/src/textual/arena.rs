use super::syntax::*;
use super::{SurfaceIntentions, SurfaceTrivia};
use std::sync::Arc;

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
#[derive(Clone, Default, Debug)]
pub struct TextArena {
    pub defs: ArenaSparse<TextualScope, DefId>,
    pub pats: ArenaSparse<TextualScope, PatId>,
    pub copats: ArenaSparse<TextualScope, CoPatId>,
    pub terms: ArenaSparse<TextualScope, TermId>,
    /// Author-selected layout that does not change canonical syntax.
    pub intentions: SurfaceIntentions,
    /// Source content retained without adding syntax variants.
    pub trivia: SurfaceTrivia,
}

/// Dense span storage for the entities issued by one textual parser.
#[derive(Clone, Default, Debug)]
pub struct SpanArena {
    key_space: Option<KeySpaceId>,
    categories: Vec<EntityCategory>,
    spans: Vec<Span>,
    map: Option<Arc<SourceMap>>,
}

mod impl_span_arena {
    use super::*;
    use std::ops::Index;

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
            let (category, key_space, raw) = id.into().into_parts();
            match self.key_space {
                | Some(existing) => {
                    assert_eq!(existing, key_space, "span ID belongs to another parser")
                }
                | None => self.key_space = Some(key_space),
            }
            assert_eq!(
                raw.into_u32() as usize,
                self.spans.len(),
                "span IDs must follow the parser allocation sequence",
            );
            self.categories.push(category);
            self.spans.push(span);
        }
        /// Replace the span of an existing textual entity.
        pub fn replace<Id>(&mut self, id: Id, span: Span)
        where
            Id: Into<EntityId>,
        {
            let index = self.index_of(id.into()).expect("span ID not found");
            self.spans[index] = span;
        }
        /// Iterate over stored spans with their IDs.
        pub fn iter(&self) -> impl Iterator<Item = (EntityId, &Span)> {
            debug_assert_eq!(self.categories.len(), self.spans.len());
            let key_space = self.key_space;
            self.categories.iter().copied().zip(&self.spans).enumerate().map(
                move |(raw, (category, span))| {
                    let key_space = key_space.expect("a nonempty span arena has a key space");
                    let raw = u32::try_from(raw).expect("span arena exceeded the raw ID range");
                    (category.restore(key_space, RawIdx::from_u32(raw)), span)
                },
            )
        }
        /// Attach the source map that decodes the stored spans' address space.
        ///
        /// Template-local arenas keep `None`: their spans are file-relative and
        /// resolve through the template's own `FileMap`.
        pub fn attach_map(&mut self, map: Arc<SourceMap>) {
            self.map = Some(map);
        }
        /// The source map decoding the stored spans, when one was attached.
        pub fn source_map(&self) -> Option<&SourceMap> {
            self.map.as_deref()
        }
        /// Release geometric vector growth after parsing finishes.
        pub(crate) fn shrink_to_fit(&mut self) {
            self.categories.shrink_to_fit();
            self.spans.shrink_to_fit();
        }
        fn index_of(&self, entity: EntityId) -> Option<usize> {
            let (category, key_space, raw) = entity.into_parts();
            (self.key_space == Some(key_space))
                .then_some(raw.into_u32() as usize)
                .filter(|index| self.categories.get(*index) == Some(&category))
        }
    }

    impl Index<&EntityId> for SpanArena {
        type Output = Span;

        fn index(&self, entity: &EntityId) -> &Self::Output {
            let index = self.index_of(*entity).expect("span ID not found");
            &self.spans[index]
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use std::mem::size_of;

        #[test]
        fn span_arena_round_trips_dense_typed_ids() {
            let mut allocator = IdAllocator::<TextualScope>::new();
            let definition: DefId = allocator.alloc();
            let pattern: PatId = allocator.alloc();
            let copattern: CoPatId = allocator.alloc();
            let term: TermId = allocator.alloc();
            let expected = [definition.into(), pattern.into(), copattern.into(), term.into()];
            let mut spans = SpanArena::new();

            spans.insert_new(definition, Span::new(0, 1));
            spans.insert_new(pattern, Span::new(1, 2));
            spans.insert_new(copattern, Span::new(2, 3));
            spans.insert_new(term, Span::new(3, 4));

            assert_eq!(size_of::<EntityCategory>(), 1);
            assert_eq!(spans.iter().map(|(entity, _)| entity).collect::<Vec<_>>(), expected);
            assert_eq!(spans[&EntityId::Term(term)].range(), 3..4);

            spans.replace(pattern, Span::new(10, 20));
            assert_eq!(spans[&EntityId::Pat(pattern)].range(), 10..20);

            let wrong_category: PatId = restore_id(definition.key_space(), definition.raw());
            assert!(spans.index_of(wrong_category.into()).is_none());
        }

        #[test]
        #[should_panic(expected = "span IDs must follow the parser allocation sequence")]
        fn span_arena_rejects_allocation_gaps() {
            let mut allocator = IdAllocator::<TextualScope>::new();
            let _: DefId = allocator.alloc();
            let term: TermId = allocator.alloc();
            SpanArena::new().insert_new(term, Span::dummy());
        }

        #[test]
        #[should_panic(expected = "span ID belongs to another parser")]
        fn span_arena_rejects_another_parser_key_space() {
            let mut first_allocator = IdAllocator::<TextualScope>::new();
            let mut second_allocator = IdAllocator::<TextualScope>::new();
            let first: DefId = first_allocator.alloc();
            let second: PatId = second_allocator.alloc();
            let mut spans = SpanArena::new();
            spans.insert_new(first, Span::dummy());
            spans.insert_new(second, Span::dummy());
        }
    }
}
