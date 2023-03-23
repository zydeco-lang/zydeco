#![allow(unused)]

use super::{
    ctx::Ctx,
    err::TypeCheckError,
    syntax::{span::SpanView, *},
};
use TypeCheckError::*;

pub trait TypeCheck: SpanView + Sized {
    type Ctx: Default;
    type Out: Eqv;
    fn syn_step(
        self, ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, Self), Self::Out>, Span<TypeCheckError>>;
    fn ana_step(
        self, typ: Self::Out, ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, Self), Self::Out>, Span<TypeCheckError>> {
        let span = self.span().clone();
        let typ_syn = self.syn(ctx)?;
        typ_syn.eqv(&typ, || span.make(Subsumption))?;
        Ok(Step::Done(typ))
    }
    fn tyck(
        mut step: Step<(Self::Ctx, Self), Self::Out>,
    ) -> Result<Self::Out, Span<TypeCheckError>> {
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
    fn syn(self, ctx: Self::Ctx) -> Result<Self::Out, Span<TypeCheckError>> {
        Self::tyck(self.syn_step(ctx)?)
    }
    fn ana(
        self, typ: Self::Out, ctx: Self::Ctx,
    ) -> Result<(), Span<TypeCheckError>> {
        Self::tyck(self.ana_step(typ, ctx)?).map(|_| ())
    }
}

pub enum Step<In, Out> {
    SynMode(In),
    AnaMode(In, Out),
    Done(Out),
}

impl TypeCheck for Span<Module> {
    type Ctx = Ctx;

    type Out = ();

    fn syn_step(
        self, ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, Self), Self::Out>, Span<TypeCheckError>> {
        todo!()
    }
}

pub trait Eqv {
    fn eqv(
        &self, other: &Self, f: impl FnOnce() -> Span<TypeCheckError> + Clone,
    ) -> Result<(), Span<TypeCheckError>>;
}

fn bool_test(
    b: bool, f: impl FnOnce() -> Span<TypeCheckError>,
) -> Result<(), Span<TypeCheckError>> {
    b.then_some(()).ok_or_else(f)
}

impl Eqv for () {
    fn eqv(
        &self, _other: &Self, _f: impl FnOnce() -> Span<TypeCheckError> + Clone,
    ) -> Result<(), Span<TypeCheckError>> {
        Ok(())
    }
}

impl Eqv for Kind {
    fn eqv(
        &self, other: &Self, f: impl FnOnce() -> Span<TypeCheckError> + Clone,
    ) -> Result<(), Span<TypeCheckError>> {
        bool_test(self == other, f)
    }
}

impl Eqv for TCtor {
    fn eqv(
        &self, other: &Self, f: impl FnOnce() -> Span<TypeCheckError> + Clone,
    ) -> Result<(), Span<TypeCheckError>> {
        match (self, other) {
            (TCtor::Var(x), TCtor::Var(y)) => bool_test(x == y, f.clone()),
            (TCtor::OS, TCtor::OS)
            | (TCtor::Ret, TCtor::Ret)
            | (TCtor::Thunk, TCtor::Thunk)
            | (TCtor::Fun, TCtor::Fun) => Ok(()),
            (TCtor::Var(..), _)
            | (TCtor::OS, _)
            | (TCtor::Ret, _)
            | (TCtor::Thunk, _)
            | (TCtor::Fun, _) => Err(f()),
        }
    }
}

impl Eqv for Type {
    fn eqv(
        &self, other: &Self, f: impl FnOnce() -> Span<TypeCheckError> + Clone,
    ) -> Result<(), Span<TypeCheckError>> {
        self.type_app_form().eqv(other.type_app_form(), f)
    }
}

impl Eqv for TypeApp<TCtor, RcType> {
    fn eqv(
        &self, other: &Self, f: impl FnOnce() -> Span<TypeCheckError> + Clone,
    ) -> Result<(), Span<TypeCheckError>> {
        TCtor::eqv(&self.tctor, &other.tctor, f.clone())?;
        bool_test(self.args.len() == other.args.len(), f.clone())?;
        for (argl, argr) in self.args.iter().zip(&other.args) {
            Type::eqv(argl.inner_ref(), argr.inner_ref(), f.clone())?
        }
        Ok(())
    }
}

impl Kind {
    pub(crate) fn ensure(
        &self, kind: Kind, context: &str, ann: &SpanInfo,
    ) -> Result<(), Span<TypeCheckError>> {
        self.eqv(&kind, || {
            ann.make(TypeCheckError::KindMismatch {
                context: context.to_owned(),
                expected: kind,
                found: *self,
            })
        })
    }
}

impl Type {
    fn type_app_form(&self) -> &TypeApp<TCtor, RcType> {
        match self {
            Type::TypeAnn(TypeAnn { ty, kd: _ }) => {
                ty.inner_ref().type_app_form()
            }
            Type::TypeApp(tapp) => tapp,
        }
    }
}
