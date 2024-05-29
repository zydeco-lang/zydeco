use crate::err::*;
use crate::syntax::*;
use crate::tyck::*;

/// A type that can be joined with another type, producing their least upper bound.
/// T \/ T ?~~> T'
pub trait Lub<Rhs = Self> {
    type Ctx;
    type Out;
    fn lub(self, other: Rhs, tycker: &mut Tycker, ctx: Self::Ctx) -> Result<Self::Out>;
}

impl Lub for () {
    type Ctx = ();
    type Out = ();
    fn lub(self, (): Self, _tycker: &mut Tycker, (): Self::Ctx) -> Result<Self::Out> {
        Ok(())
    }
}

impl Lub for &KindId {
    type Ctx = ();
    type Out = KindId;

    fn lub(self, other: Self, tycker: &mut Tycker, ctx: Self::Ctx) -> Result<Self::Out> {
        let lhs = tycker.statics.kinds[self].clone();
        let rhs = tycker.statics.kinds[other].clone();
        match (lhs, rhs) {
            | (Kind::VType(VType), Kind::VType(VType)) => {
                let kd = Alloc::alloc(tycker, VType);
                Ok(kd)
            }
            | (Kind::CType(CType), Kind::CType(CType)) => {
                let kd = Alloc::alloc(tycker, CType);
                Ok(kd)
            }
            | (Kind::Arrow(lhs), Kind::Arrow(rhs)) => {
                let Arrow(lin, lout) = lhs;
                let Arrow(rin, rout) = rhs;
                let kd_in = lin.lub(&rin, tycker, ctx)?;
                let kd_out = lout.lub(&rout, tycker, ctx)?;
                let kd = Alloc::alloc(tycker, Arrow(kd_in, kd_out));
                Ok(kd)
            }
            | (Kind::VType(_), _) | (Kind::CType(_), _) | (Kind::Arrow(_), _) => {
                Err(TyckError::KindMismatch)
            }
        }
    }
}

impl Lub for &TypeId {
    /// We need to remember the definitions introduced by both sides.
    // Todo..
    type Ctx = ();
    type Out = TypeId;

    fn lub(self, other: Self, tycker: &mut Tycker, ctx: Self::Ctx) -> Result<Self::Out> {
        let lhs = tycker.statics.types[self].clone();
        let rhs = tycker.statics.types[other].clone();
        // match (lhs, rhs) {
        // }
        todo!()
    }
}
