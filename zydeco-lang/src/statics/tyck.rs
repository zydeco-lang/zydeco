mod r#type;
mod value;
mod computation;
mod module;
mod eqv;
mod ctx;

pub use self::ctx::*;
use super::{err::TyckErrorItem, syntax::*};
use crate::{
    rc,
    statics::resolve::NameResolveError,
    syntax::env::Env,
    utils::{
        fmt::FmtArgs,
        monoid::Monoid,
        span::{Span, SpanInfo, SpanView},
    },
};
use std::collections::HashSet;
use TyckErrorItem::*;

pub trait TypeCheck: SpanView + Sized {
    type Ctx: Default;
    type Out: Eqv;
    fn syn_step(
        &self, ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TyckErrorItem>>;
    fn ana_step(
        &self, typ: Self::Out, ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TyckErrorItem>> {
        let span = self.span().clone();
        let typ_syn = self.syn(ctx)?;
        typ_syn.eqv(&typ, Default::default(), || {
            span.make(Subsumption { sort: "type" })
        })?;
        Ok(Step::Done(typ))
    }
    fn tyck(
        mut step: Step<(Self::Ctx, &Self), Self::Out>,
    ) -> Result<Self::Out, Span<TyckErrorItem>> {
        loop {
            match step {
                Step::SynMode((ctx, term)) => {
                    step = term.syn_step(ctx)?;
                }
                Step::AnaMode((ctx, term), out) => {
                    step = term.ana_step(out, ctx)?;
                }
                Step::Done(out) => {
                    break Ok(out);
                }
            }
        }
    }
    #[must_use]
    fn syn(&self, ctx: Self::Ctx) -> Result<Self::Out, Span<TyckErrorItem>> {
        Self::tyck(self.syn_step(ctx)?)
    }
    #[must_use]
    fn ana(
        &self, typ: Self::Out, ctx: Self::Ctx,
    ) -> Result<Self::Out, Span<TyckErrorItem>> {
        Self::tyck(self.ana_step(typ, ctx)?)
    }
}

pub enum Step<In, Out> {
    SynMode(In),
    AnaMode(In, Out),
    Done(Out),
}

fn bool_test<E>(b: bool, f: impl FnOnce() -> E) -> Result<(), E> {
    b.then_some(()).ok_or_else(f)
}

impl Data<TypeV, Kind, CtorV, RcType> {
    fn type_arity(&self) -> TypeArity<Kind> {
        TypeArity {
            params: (self.params.iter()).map(|(_, kd)| kd.clone()).collect(),
            kd: Kind::VType,
        }
    }
}

impl Codata<TypeV, Kind, DtorV, RcType> {
    fn type_arity(&self) -> TypeArity<Kind> {
        TypeArity {
            params: (self.params.iter()).map(|(_, kd)| kd.clone()).collect(),
            kd: Kind::CType,
        }
    }
}

impl From<Kind> for TypeArity<Kind> {
    fn from(kd: Kind) -> Self {
        TypeArity { params: vec![], kd }
    }
}

pub struct Seal<T>(pub T);
impl<T> Eqv for Seal<T> {
    type Ctx = ();
    fn eqv(
        &self, _other: &Self, _ctx: (),
        _f: impl FnOnce() -> Span<TyckErrorItem> + Clone,
    ) -> Result<(), Span<TyckErrorItem>> {
        Ok(())
    }
}

impl TypeCheck for Span<Program> {
    type Ctx = Ctx;
    type Out = Seal<(Ctx, Type)>;

    fn syn_step(
        &self, ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TyckErrorItem>> {
        let span = self.span();
        let Program { module, entry } = self.inner_ref();
        let Seal(ctx) = module.syn(ctx)?;
        let ty = entry.syn(ctx.to_owned())?;
        let SynType::TypeApp(ty_app) = &ty.synty else {
            Err(span.make(WrongMain { found: ty }))?
        };
        if ty_app.elim_os().is_none() {
            Err(span.make(WrongMain { found: ty_app.clone().into() }))?
        };
        Ok(Step::Done(Seal((ctx, ty_app.clone().into()))))
    }
}

pub trait Eqv {
    type Ctx: Default;
    fn eqv(
        &self, other: &Self, ctx: Self::Ctx,
        f: impl FnOnce() -> Span<TyckErrorItem> + Clone,
    ) -> Result<(), Span<TyckErrorItem>>;
}
