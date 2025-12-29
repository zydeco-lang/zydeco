//! Span lookup for scoped IDs via textual back-mapping.

use super::{Resolver, syntax::*};
use crate::textual::syntax::SpanArena;

macro_rules! impl_span_view_pair {
    ($($ty:ty)*) => {
        $(
            impl<'a, 'b> SpanView<'a, (&'b SpanArena, &'b ScopedArena)> for $ty {
                fn span(&self, (spans, arena): &'a (&'b SpanArena, &'b ScopedArena)) -> &'a Span {
                    let entity = arena.textual.back(&(*self).into()).unwrap();
                    &spans[&entity]
                }
            }
        )*
    };
}

impl_span_view_pair! {
    DefId
    PatId
    TermId
    DeclId
}

macro_rules! impl_span_view_resolver {
    ($($ty:ty)*) => {
        $(
            impl<'a> SpanView<'a, Resolver> for $ty {
                fn span(&self, resolver: &'a Resolver) -> &'a Span {
                    let entity = resolver.bitter.textual.back(&(*self).into()).unwrap();
                    &resolver.spans[&entity]
                }
            }
        )*
    };
}

impl_span_view_resolver! {
    DefId
    PatId
    TermId
    DeclId
}
