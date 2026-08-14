//! Span lookup for scoped IDs via textual back-mapping.

use super::{Resolver, syntax::*};
use crate::textual::syntax as t;
use zydeco_syntax::{SpanView, TextualBack, span_via_back};

impl TextualBack for ScopedArena {
    type Id = EntityId;
    type Entity = t::EntityId;

    fn textual_back(&self, id: EntityId) -> Option<t::EntityId> {
        self.textual.back(&id).copied()
    }
}

macro_rules! impl_span_view_resolver {
    ($($ty:ty)*) => {
        $(
            impl<'a> SpanView<'a, Resolver<'a>> for $ty {
                fn span(&self, resolver: &'a Resolver<'a>) -> &'a Span {
                    span_via_back(&resolver.spans, &resolver.bitter, *self)
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
