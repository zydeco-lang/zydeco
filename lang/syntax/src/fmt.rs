//! The formatter trait.

#[impl_tools::autoimpl(for<T: trait + ?Sized> &T, &mut T, Box<T>, std::rc::Rc<T>, std::sync::Arc<T>)]
pub trait Ugly<'a, Fmter> {
    fn ugly(&self, f: &'a Fmter) -> String;
}
