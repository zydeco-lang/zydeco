//! Span lookup for scoped IDs via textual back-mapping.

use super::{ResolveFolder, syntax::*};
use crate::textual::syntax as t;
use zydeco_syntax::{SpanView, TextualBack, span_via_back};

impl TextualBack for ScopedArena {
    type Id = EntityId;
    type Entity = t::EntityId;

    fn textual_back(&self, id: EntityId) -> Option<t::EntityId> {
        self.origins.source(&id)
    }
}

impl<O> TextualBack for ResolveFolder<'_, O> {
    type Id = EntityId;
    type Entity = t::EntityId;

    fn textual_back(&self, id: EntityId) -> Option<t::EntityId> {
        self.builder.origins.source(&id)
    }
}

macro_rules! impl_span_view_resolver {
    ($($ty:ty)*) => {
        $(
            impl<'a, O> SpanView<'a, ResolveFolder<'a, O>> for $ty {
                fn span(&self, resolver: &'a ResolveFolder<'a, O>) -> &'a Span {
                    span_via_back(&resolver.spans, resolver, *self)
                }
            }
        )*
    };
}

impl_span_view_resolver! {
    DefId
    PatId
    TermId
}
