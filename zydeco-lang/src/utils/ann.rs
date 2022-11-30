use std::fmt::Debug;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ann {}

pub fn ann() -> Ann {
    Ann {}
}

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

pub trait AnnHolder {
    fn ann(&self) -> &Ann;
    fn set_ann(&mut self, ann: Ann);
}
