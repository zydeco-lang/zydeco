//! Span lookup for bitter IDs via textual back-mapping.

use super::{Desugarer, syntax::*};
use crate::textual::syntax as t;
use zydeco_syntax::{SpanView, TextualBack, span_via_back};

impl TextualBack for BitterArena {
    type Id = EntityId;
    type Entity = t::EntityId;

    fn textual_back(&self, id: EntityId) -> Option<t::EntityId> {
        self.textual.back(&id).copied()
    }
}

macro_rules! impl_span_view_desugarer {
    ($($ty:ty)*) => {
        $(
            impl<'a> SpanView<'a, Desugarer<'_>> for $ty {
                fn span(&self, desugarer: &'a Desugarer<'_>) -> &'a Span {
                    span_via_back(&desugarer.spans, &desugarer.bitter, *self)
                }
            }
        )*
    };
}

impl_span_view_desugarer! {
    DefId
    PatId
    TermId
}
