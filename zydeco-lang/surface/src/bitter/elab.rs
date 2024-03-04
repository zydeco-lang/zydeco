use crate::bitter::syntax as b;
use crate::textual::syntax as t;

pub trait Desugar {
    fn desugar(self);
}

pub struct Desugarer<'ctx, T> {
    pub tctx: &'ctx t::Ctx,
    pub bctx: &'ctx mut b::Ctx,
    pub t: T,
}

pub struct RunDesugar {
    pub top: t::TopLevel,
    pub ctx: t::Ctx,
}
