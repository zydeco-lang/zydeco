/// A value of type `A` paired with attached metadata `M`.
///
/// `With<M, A>` is a lightweight, transparent product type meant for *decorating* a value
/// with extra information—without turning it into an opaque "wrapper object".
///
/// The intended convention is:
/// - `A` is the **payload** (the thing you want to transform).
/// - `M` is **metadata / context** that should usually be preserved as you transform `A`
///   (e.g. spans, IDs, annotations, provenance, configuration, etc.).
///
/// In functional terms, `With<M, _>` corresponds to the partially-applied product type
/// `M × _` (Haskell’s `((,) M)`), and is commonly treated as "functor-like" over `A`:
/// mapping over the payload leaves `M` unchanged.
///
/// # Basic usage
///
/// ```
/// # use zydeco_utils::with::With;
/// let x: With<&'static str, i32> = With { info: "source.rs:10", inner: 123 };
///
/// // Destructure to access components:
/// let With { info, inner } = x;
/// assert_eq!(info, "source.rs:10");
/// assert_eq!(inner, 123);
/// ```
///
/// # Domain-specific aliases
///
/// Using aliases often makes call sites clearer:
///
/// ```
/// # use zydeco_utils::with::With;
/// # struct Span;
/// # struct TypeInfo;
/// type Spanned<T>   = With<Span, T>;
/// type Annotated<T> = With<TypeInfo, T>;
/// ```
///
/// # Notes on semantics
///
/// If you implement a `map`/`fmap`-style operation (directly or via a trait), the usual
/// law-abiding behavior is to apply the function to the `A` field while leaving `M`
/// untouched. If you later add sequencing/composition that *combines* metadata, `M`
/// often naturally takes on "log/writer" semantics (e.g. requires some kind of append).
#[derive(Default, Clone, Debug)]
pub struct With<M, A> {
    pub info: M,
    pub inner: A,
}
impl<M, A> AsRef<A> for With<M, A> {
    fn as_ref(&self) -> &A {
        &self.inner
    }
}
impl<M, A> AsMut<A> for With<M, A> {
    fn as_mut(&mut self) -> &mut A {
        &mut self.inner
    }
}

pub struct WithInfo<M>(pub M);

pub trait Apply {
    type On<A>;
}

impl<M> Apply for WithInfo<M> {
    type On<A> = With<M, A>;
}
