use std::{
    borrow::{Borrow, BorrowMut},
    ops::{Deref, DerefMut},
};

/// A wrapper around a value that allows trait implementators to keep track of an associated type.
///
/// - S: The source type.
/// - T: The phantom target type.
pub struct Phantom<S, T>(S, std::marker::PhantomData<T>);
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
impl<S, T> AsRef<S> for Phantom<S, T> {
    fn as_ref(&self) -> &S {
        &self.0
    }
}
impl<S, T> AsMut<S> for Phantom<S, T> {
    fn as_mut(&mut self) -> &mut S {
        &mut self.0
    }
}
impl<S, T> Deref for Phantom<S, T> {
    type Target = S;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<S, T> DerefMut for Phantom<S, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
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
