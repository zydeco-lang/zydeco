use super::syntax::*;

macro_rules! impl_span_view {
    ($($ty:ty)*) => {
        $(
            impl<'a> SpanView<'a, Parser> for $ty {
                fn span(&self, arena: &'a Parser) -> &'a Span {
                    &arena.spans[&((*self).into())]
                }
            }
        )*
    };
}

impl_span_view! {
    DefId
    PatId
    TermId
    DeclId
}
