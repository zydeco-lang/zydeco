use std::fmt::Debug;

pub trait AnnT: Clone + Debug {
    type Span;
    fn internal(sort: &'static str) -> Self;
    fn span(&self) -> Self::Span;
}

impl AnnT for () {
    type Span = ();
    fn internal(_: &'static str) -> Self {}
    fn span(&self) -> Self::Span {}
}