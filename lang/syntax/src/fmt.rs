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

/// Flattening and rendering of destructor-view application spines.
///
/// A view pattern's head is an iterated application; debug formatters render it in the
/// bracketed form `head[arg, ...]`, flattening the spine first and rendering second.
pub struct ViewSpine;

impl ViewSpine {
    /// Flatten iterated applications into the head and its argument list.
    ///
    /// `decompose` peels one application node into its function and arguments,
    /// or `None` for any other node.
    pub fn parts<Id: Copy>(
        view: Id, decompose: &impl Fn(Id) -> Option<(Id, Vec<Id>)>,
    ) -> (Id, Vec<Id>) {
        let Some((function, arguments)) = decompose(view) else {
            return (view, Vec::new());
        };
        let (head, prefix) = Self::parts(function, decompose);
        (head, prefix.into_iter().chain(arguments).collect())
    }

    /// The bracketed debug form `head[arg, ...]`.
    pub fn bracketed<'a, Id, Fmter>(head: Id, arguments: Vec<Id>, fmter: &'a Fmter) -> RcDoc<'a>
    where
        Id: Pretty<'a, Fmter>,
    {
        if arguments.is_empty() {
            head.pretty(fmter)
        } else {
            RcDoc::concat([
                head.pretty(fmter),
                RcDoc::text("["),
                RcDoc::intersperse(
                    arguments.into_iter().map(|argument| argument.pretty(fmter)),
                    RcDoc::text(", "),
                ),
                RcDoc::text("]"),
            ])
        }
    }
}
