//! The span view trait.

use zydeco_utils::span::Span;

#[auto_impl::auto_impl(&, &mut, Box, Rc, Arc)]
pub trait SpanView<'a, Arena> {
    fn span(&self, arena: &'a Arena) -> &'a Span;
}
