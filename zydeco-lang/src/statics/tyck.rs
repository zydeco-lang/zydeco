mod r#type;
mod value;
mod computation;
mod module;
mod lub;

use super::{
    err::{Frame, Trace, TyckError, TyckErrorItem},
    syntax::*,
};
use crate::{
    rc,
    resolve::err::NameResolveError,
    syntax::env::Env,
    utils::{
        fmt::FmtArgs,
        span::{Span, SpanInfo, SpanView},
    },
};
use std::collections::HashSet;
use TyckErrorItem::*;

pub trait CtxT {
    fn err(&self, span: &SpanInfo, item: TyckErrorItem) -> TyckError;
}
impl CtxT for () {
    fn err(&self, span: &SpanInfo, item: TyckErrorItem) -> TyckError {
        TyckError { item: span.make(item), trace: Default::default() }
    }
}

#[derive(Clone, Default)]
pub struct Ctx {
    pub abst_ctx: im::Vector<Kind>,
    pub type_ctx: im::HashMap<TypeV, TypeArity<Kind>>,
    pub term_ctx: im::HashMap<TermV, Type>,
    pub type_env: Env<TypeV, Type>,
    pub data_env: im::HashMap<TypeV, prelude::Data>,
    pub codata_env: im::HashMap<TypeV, prelude::Codata>,
    pub alias_env: im::HashMap<TypeV, prelude::Alias>,
    pub trace: Trace,
}

mod ctx {
    use super::*;
    impl Ctx {
        pub(super) fn fresh(&mut self, kd: Kind) -> AbstVar {
            self.abst_ctx.push_back(kd);
            AbstVar(self.abst_ctx.len() - 1)
        }
    }
    impl CtxT for Ctx {
        fn err(&self, span: &SpanInfo, item: TyckErrorItem) -> TyckError {
            TyckError { item: span.make(item), trace: self.trace.clone() }
        }
    }
}

pub trait TypeCheck: SpanView + Sized {
    type Ctx: Default + Clone + CtxT;
    type Out: Lub<Out = Self::Out>;
    fn syn_step(&self, ctx: Self::Ctx) -> Result<Step<(Self::Ctx, &Self), Self::Out>, TyckError>;
    fn ana_step(
        &self, typ: Self::Out, ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, TyckError> {
        let span = self.span();
        let typ_syn = self.syn(ctx)?;
        let typ = typ_syn.lub(typ, Default::default(), span)?;
        Ok(Step::Done(typ))
    }
    fn tyck(mut step: Step<(Self::Ctx, &Self), Self::Out>) -> Result<Self::Out, TyckError> {
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
    fn syn(&self, ctx: Self::Ctx) -> Result<Self::Out, TyckError> {
        Self::tyck(self.syn_step(ctx)?)
    }
    #[must_use]
    fn ana(&self, typ: Self::Out, ctx: Self::Ctx) -> Result<Self::Out, TyckError> {
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

pub struct Seal<T>(pub T);

impl TypeCheck for Span<Program> {
    type Ctx = Ctx;
    type Out = Seal<(Ctx, Type)>;

    fn syn_step(&self, ctx: Self::Ctx) -> Result<Step<(Self::Ctx, &Self), Self::Out>, TyckError> {
        let span = self.span();
        let Program { module, entry } = self.inner_ref();
        let Seal(ctx) = module.syn(ctx)?;
        let ty = entry.syn(ctx.to_owned())?;
        if ty.clone().elim_os(ctx.clone(), span).is_none() {
            Err(ctx.err(span, WrongMain { found: ty.clone() }))?
        };
        Ok(Step::Done(Seal((ctx, ty))))
    }
}

/// A type that can be joined with another type, a.k.a the least upper bound.
/// T \/ T? ~~> T'
pub trait Lub<Rhs = Self> {
    type Ctx: Default;
    type Out;
    fn lub(self, other: Rhs, ctx: Self::Ctx, span: &SpanInfo) -> Result<Self::Out, TyckError>;
}
