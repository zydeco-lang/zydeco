use std::fmt::Debug;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ann {
    span1: (usize, usize),
}

pub fn ann(l: usize, r: usize) -> Ann {
    Ann { span1: (l, r) }
}

pub trait AnnHolder {
    fn ann(&self) -> &Ann;
    fn set_ann(&mut self, ann: Ann);
}
