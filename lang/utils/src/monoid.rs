use super::span::{Sp, Span};

pub trait Monoid: Sized {
    fn empty() -> Self;
    fn append(self, other: Self) -> Self;
    fn extend(self, others: impl Iterator<Item = Self>) -> Self {
        others.fold(self, Self::append)
    }
    fn concat(others: impl Iterator<Item = Self>) -> Self {
        Self::empty().extend(others)
    }
}

impl<T: Monoid> Monoid for Sp<T> {
    fn empty() -> Self {
        Span::dummy().make(T::empty())
    }
    fn append(self, other: Self) -> Self {
        other.info.make(self.inner.append(other.inner))
    }
}
