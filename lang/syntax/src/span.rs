//! The span view trait.

use zydeco_utils::span::Span;

#[impl_tools::autoimpl(for<T: trait + ?Sized> &T, &mut T, Box<T>, std::rc::Rc<T>, std::sync::Arc<T>)]
pub trait SpanView<'a, Arena> {
    fn span(&self, arena: &'a Arena) -> &'a Span;
}
