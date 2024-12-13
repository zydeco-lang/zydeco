use std::ops::Add;

pub trait Monoid: Default + Add<Output = Self> + Sized {
    fn extend(self, others: impl Iterator<Item = Self>) -> Self {
        others.fold(self, Self::add)
    }
    fn concat(others: impl Iterator<Item = Self>) -> Self {
        Self::default().extend(others)
    }
}
