//! Span lookup for bitter IDs via textual back-mapping.

use super::{DesugarFolder, syntax::*};
use crate::textual::syntax as t;
use zydeco_syntax::{SpanView, TextualBack, span_via_back};

impl TextualBack for BitterArena {
    type Id = EntityId;
    type Entity = t::EntityId;

    fn textual_back(&self, id: EntityId) -> Option<t::EntityId> {
        self.origins.source(&id)
    }
}

macro_rules! impl_span_view_folder {
    ($($ty:ty)*) => {
        $(
            impl<'a> SpanView<'a, DesugarFolder<'_>> for $ty {
                fn span(&self, folder: &'a DesugarFolder<'_>) -> &'a Span {
                    span_via_back(&folder.spans, &folder.builder.arena, *self)
                }
            }
        )*
    };
}

impl_span_view_folder! {
    DefId
    PatId
    TermId
}
