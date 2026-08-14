//! Span lookup for phase-local identifiers.
//!
//! [`SpanView`] decouples an identifier from the context that stores its
//! source location. The canonical lookup path goes through a textual
//! back-map ([`TextualBack`]) into a span store ([`SpanStore`]); the generic
//! pair implementation below covers that shape for every phase, so each phase
//! only implements the two small traits on its arenas.

use zydeco_utils::span::Span;

pub trait SpanView<'a, Arena> {
    fn span(&self, arena: &'a Arena) -> &'a Span;
}

/// Maps a phase-local identifier back to the textual entity it elaborates from.
pub trait TextualBack {
    /// The phase-local identifier kind carried by the arena.
    type Id;
    /// The textual entity owning the source location.
    type Entity;

    fn textual_back(&self, id: Self::Id) -> Option<Self::Entity>;
}

impl<T: ?Sized> TextualBack for &T
where
    T: TextualBack,
{
    type Id = T::Id;
    type Entity = T::Entity;

    fn textual_back(&self, id: Self::Id) -> Option<Self::Entity> {
        (**self).textual_back(id)
    }
}

/// Stores spans keyed by textual entity.
pub trait SpanStore<Entity> {
    fn span(&self, entity: Entity) -> &Span;
}

impl<Entity, T: ?Sized> SpanStore<Entity> for &T
where
    T: SpanStore<Entity>,
{
    fn span(&self, entity: Entity) -> &Span {
        (**self).span(entity)
    }
}

/// Resolve a phase identifier through a textual back-map into a span store.
///
/// The free-function form lets pass drivers resolve spans through the fields
/// they hold without constructing a temporary arena pair.
pub fn span_via_back<'a, S, A, Id>(spans: &'a S, arena: &'a A, id: Id) -> &'a Span
where
    A: TextualBack,
    Id: Copy + Into<A::Id>,
    S: SpanStore<A::Entity>,
{
    let entity = arena.textual_back(id.into()).expect("missing textual back-map entry");
    spans.span(entity)
}

/// The canonical span lookup through a textual back-map and a span store.
///
/// This impl owns the `(span store, phase arena)` pair shape; do not
/// implement [`SpanView`] for tuple arenas elsewhere.
impl<'a, S, A, Id> SpanView<'a, (S, A)> for Id
where
    A: TextualBack,
    Id: Copy + Into<A::Id>,
    S: SpanStore<A::Entity>,
{
    fn span(&self, (spans, arena): &'a (S, A)) -> &'a Span {
        span_via_back(spans, arena, *self)
    }
}
