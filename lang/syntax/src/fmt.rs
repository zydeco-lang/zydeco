//! The formatter traits.

use pretty::RcDoc;

#[impl_tools::autoimpl(for<T: trait + ?Sized> &T, &mut T, Box<T>, std::rc::Rc<T>, std::sync::Arc<T>)]
pub trait Pretty<'a, Fmter> {
    fn pretty(&self, f: &'a Fmter) -> RcDoc<'a>;
}

/// Rendering width for the ugly form; effectively unbounded.
const UGLY_WIDTH: usize = 1 << 20;

/// The canonical single-line rendering of a [`Pretty`] document.
///
/// `Ugly` is derived from [`Pretty`]: rendering the same document at an
/// unbounded width reproduces the byte-exact canonical surface form, so a
/// formatter maintains one document construction instead of two printers.
pub trait Ugly<'a, Fmter>: Pretty<'a, Fmter> {
    fn ugly(&self, f: &'a Fmter) -> String {
        let mut output = String::new();
        self.pretty(f)
            .render_fmt(UGLY_WIDTH, &mut output)
            .expect("rendering a formatter document cannot fail");
        output
    }
}

impl<'a, F, T> Ugly<'a, F> for T where T: Pretty<'a, F> {}
