pub use super::{arena::*, substitute::*};
pub use crate::syntax::*;

#[derive(Clone, Debug)]
pub enum NonJoin {}

impl<T> From<T> for Computation<NonJoin>
where
    T: Into<NonJoin>,
{
    fn from(_: T) -> Self {
        unreachable!()
    }
}

#[derive(Clone, Debug)]
pub struct SComputation {
    pub compu: Computation<NonJoin>,
    pub assignments: SubstAssignments,
}
