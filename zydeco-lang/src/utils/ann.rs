use std::fmt::Debug;

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ann<T> {
    inner: T,
    pub info: AnnInfo,
}

impl<T> Ann<T> {
    pub fn inner(&self) -> &T {
        &self.inner
    }
}
