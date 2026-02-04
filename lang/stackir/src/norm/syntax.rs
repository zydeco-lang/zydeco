pub use super::{arena::*, substitute::*};
pub use crate::sps::syntax::*;
pub use zydeco_utils::{
    arena::*,
    context::{CoContext, Context},
};

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
    pub map: SubstPatMap,
}
