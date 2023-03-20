pub mod resolve;
mod err;
mod next;
mod legacy;
pub mod eqv;
pub mod syntax;

use self::err::TypeCheckError::*;
use crate::syntax::ann::{ann, Ann};

pub use self::{err::TypeCheckError, legacy::ctx::Ctx};

pub trait TypeCheck {
    type Out: Eqv;
    fn syn(&self, ctx: &Ctx) -> Result<Self::Out, Ann<TypeCheckError>>;
    fn ana(
        &self, typ: &Self::Out, ctx: &Ctx,
    ) -> Result<(), Ann<TypeCheckError>> {
        let typ_syn = self.syn(ctx)?;
        typ.eqv(&typ_syn).ok_or_else(|| {
            ann(0, 0).make(ErrStr(format!("Subsumption failed")))
        })
    }
}

pub trait Eqv {
    fn eqv(&self, other: &Self) -> Option<()>;
}
