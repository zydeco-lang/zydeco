//! The span view trait.

use zydeco_utils::span::Span;

pub trait SpanView<'a, Arena> {
    fn span(&self, arena: &'a Arena) -> &'a Span;
}

impl<'a, T, Arena> SpanView<'a, Arena> for Box<T>
where
    T: SpanView<'a, Arena>,
{
    fn span(&self, arena: &'a Arena) -> &'a Span {
        self.as_ref().span(arena)
    }
}
