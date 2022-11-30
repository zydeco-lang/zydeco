use std::fmt::Debug;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ann {}

pub fn ann() -> Ann {
    Ann {}
}

pub trait AnnHolder {
    fn ann(&self) -> &Ann;
    fn set_ann(&mut self, ann: Ann);
}
