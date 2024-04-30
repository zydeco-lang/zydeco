use crate::prelude::*;
use crate::statics::{err::*, syntax::*, tyck::*};

/// monad transformers
mod r#type;
mod term;

pub trait MonadTransType {
    fn lift(&self, ty_m: Type, ctx: Ctx, span: &Span) -> Result<Type, TyckError>;
    fn alg(&self, m: RcValue, ctx: Ctx, span: &Span) -> Result<TermComputation, TyckError>;
}

pub trait MonadTransTerm: Clone {
    fn lift(&self, m: &TermValue) -> Self;
}

pub trait MonadTrans {
    fn lift(self, ctx: Ctx) -> Self;
}

impl MonadTrans for Sp<Program> {
    fn lift(self, _ctx: Ctx) -> Self {
        todo!()
    }
}
