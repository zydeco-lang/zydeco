pub use super::arena::*;
pub use crate::syntax::*;

use derive_more::From;

#[derive(From, Clone, Debug)]
pub enum LetJoin {
    Value(Let<VPatId, ValueId, CompuId>),
    Stack(Let<Bullet, StackId, CompuId>),
}

impl<T> From<T> for Computation<LetJoin>
where
    T: Into<LetJoin>,
{
    fn from(j: T) -> Self {
        Computation::Join(j.into())
    }
}
