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
