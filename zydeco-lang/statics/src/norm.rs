use crate::err::*;
use crate::surface_syntax as su;
use crate::syntax::*;
use crate::tyck::*;

pub trait Normalize {
    type Ctx;
    type Out;
    fn normalize(self, tycker: &mut Tycker, ctx: Self::Ctx) -> Result<Self::Out>;
}
