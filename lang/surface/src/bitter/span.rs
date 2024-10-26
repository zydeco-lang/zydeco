use super::{syntax::*, Desugarer};
use crate::textual::syntax::SpanArena;

macro_rules! impl_span_view_pair {
    ($($ty:ty)*) => {
        $(
            impl<'a> SpanView<'a, (SpanArena, Arena)> for $ty {
                fn span(&self, (spans, arena): &'a (SpanArena, Arena)) -> &'a Span {
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

macro_rules! impl_span_view_desugarer {
    ($($ty:ty)*) => {
        $(
            impl<'a> SpanView<'a, Desugarer> for $ty {
                fn span(&self, desugarer: &'a Desugarer) -> &'a Span {
                    let entity = desugarer.bitter.textual.back(&(*self).into()).unwrap();
                    &desugarer.spans[&entity]
                }
            }
        )*
    };
}

impl_span_view_desugarer! {
    DefId
    PatId
    TermId
    DeclId
}
