use std::{
    borrow::{Borrow, BorrowMut},
    ops::{Deref, DerefMut},
};

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

impl<M, A> With<M, A> {
    pub fn new(info: M, inner: A) -> Self {
        Self { info, inner }
    }
    pub fn mk<B>(&self, inner: B) -> With<M, B>
    where
        M: Clone,
    {
        With { info: self.info.clone(), inner }
    }
    pub fn mk_add<B, T>(&self, other: T, inner: B) -> With<M, B>
    where
        M: Clone + std::ops::Add<T, Output = M>,
    {
        With { info: self.info.clone() + other, inner }
    }
    pub fn mk_ext<B, Iter, Item>(&self, iter: Iter, inner: B) -> With<M, B>
    where
        M: Clone + Extend<Item>,
        Iter: IntoIterator<Item = Item>,
    {
        let mut info = self.info.clone();
        info.extend(iter);
        With { info, inner }
    }
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
impl<M, A> Deref for With<M, A> {
    type Target = A;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl<M, A> DerefMut for With<M, A> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}
impl<M, A> Borrow<A> for With<M, A> {
    fn borrow(&self) -> &A {
        &self.inner
    }
}
impl<M, A> BorrowMut<A> for With<M, A> {
    fn borrow_mut(&mut self) -> &mut A {
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
