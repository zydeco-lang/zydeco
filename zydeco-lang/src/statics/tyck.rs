use super::{
    err::TypeCheckError,
    syntax::{Kind, RcType, Span, TermV, TypeArity, TypeV, span::SpanHolder},
};
use TypeCheckError::*;

pub trait TypeCheck: SpanHolder {
    type Ctx;
    type Out: Eqv;
    fn syn(&self, ctx: &Self::Ctx) -> Result<Self::Out, Span<TypeCheckError>>;
    fn ana(
        &self, typ: &Self::Out, ctx: &Self::Ctx,
    ) -> Result<(), Span<TypeCheckError>> {
        let typ_syn = self.syn(ctx)?;
        typ.eqv(&typ_syn).ok_or_else(|| {
            self.span().make(ErrStr(format!("Subsumption failed")))
        })
    }
}

pub trait Eqv {
    fn eqv(&self, other: &Self) -> Option<()>;
}
