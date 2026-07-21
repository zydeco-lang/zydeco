use derive_more::{AsMut, AsRef, Deref, DerefMut};
use std::borrow::{Borrow, BorrowMut};

/// A wrapper around a value that allows trait implementators to keep track of an associated type.
///
/// - S: The source type.
/// - T: The phantom target type.
#[derive(AsRef, AsMut, Deref, DerefMut)]
pub struct Phantom<S, T>(
    #[as_ref]
    #[as_mut]
    #[deref]
    #[deref_mut]
    S,
    /// A phantom type that is used to track the associated type.
    /// The `fn() -> T` is used because `Phantom` doesn't logically store `T`.
    std::marker::PhantomData<fn() -> T>,
);
impl<S, T> Phantom<S, T> {
    pub fn new(s: S) -> Self {
        Self(s, std::marker::PhantomData)
    }
    pub fn clone_inner(&self) -> S
    where
        S: Clone,
    {
        self.0.clone()
    }
}
impl<S, T> From<S> for Phantom<S, T> {
    fn from(s: S) -> Self {
        Self::new(s)
    }
}
impl<S, T> Borrow<S> for Phantom<S, T> {
    fn borrow(&self) -> &S {
        &self.0
    }
}
impl<S, T> BorrowMut<S> for Phantom<S, T> {
    fn borrow_mut(&mut self) -> &mut S {
        &mut self.0
    }
}
