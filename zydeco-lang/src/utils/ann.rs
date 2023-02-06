use std::{fmt::Debug, hash::Hash};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AnnInfo {}

impl AnnInfo {
    pub fn make<T>(self, inner: T) -> Ann<T> {
        Ann { inner, info: self }
    }
}

pub fn ann() -> AnnInfo {
    AnnInfo {}
}

#[derive(Clone, Debug)]
pub struct Ann<T> {
    inner: T,
    pub info: AnnInfo,
}

impl<T> Ann<T> {
    pub fn inner(&self) -> &T {
        &self.inner
    }
}

impl<T: PartialEq> PartialEq for Ann<T> {
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl<T: Eq> Eq for Ann<T> {}

impl<T: Hash> Hash for Ann<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state);
    }
}
