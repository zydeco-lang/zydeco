mod type_;
mod value;
mod computation;
mod eqv;

use super::{err::TypeCheckError, syntax::*};
use crate::{
    rc,
    statics::resolve::NameResolveError,
    syntax::env::Env,
    utils::{
        fmt::FmtArgs,
        monoid::Monoid,
        span::{Span, SpanView},
    },
};
use std::collections::HashSet;
use TypeCheckError::*;

#[derive(Clone, Default)]
pub struct Ctx {
    pub abst_ctx: im::Vector<Kind>,
    pub type_env: Env<TypeV, Type>,
    pub type_ctx: im::HashMap<TypeV, TypeArity<Kind>>,
    pub term_ctx: im::HashMap<TermV, Type>,
    pub data_ctx: im::HashMap<TypeV, Data<TypeV, CtorV, RcType>>,
    pub coda_ctx: im::HashMap<TypeV, Codata<TypeV, DtorV, RcType>>,
}
impl Ctx {
    fn fresh(&mut self, kd: Kind) -> Abstract {
        self.abst_ctx.push_back(kd);
        Abstract(self.abst_ctx.len() - 1)
    }
}

pub trait TypeCheck: SpanView + Sized {
    type Ctx: Default;
    type Out: Eqv;
    fn syn_step(
        &self, ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>>;
    fn ana_step(
        &self, typ: Self::Out, ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        let span = self.span().clone();
        let typ_syn = self.syn(ctx)?;
        typ_syn.eqv(&typ, Default::default(), || span.make(Subsumption))?;
        Ok(Step::Done(typ))
    }
    fn tyck(
        mut step: Step<(Self::Ctx, &Self), Self::Out>,
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
    #[must_use]
    fn syn(&self, ctx: Self::Ctx) -> Result<Self::Out, Span<TypeCheckError>> {
        Self::tyck(self.syn_step(ctx)?)
    }
    #[must_use]
    fn ana(
        &self, typ: Self::Out, ctx: Self::Ctx,
    ) -> Result<(), Span<TypeCheckError>> {
        Self::tyck(self.ana_step(typ, ctx)?).map(|_| ())
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

impl Data<TypeV, CtorV, RcType> {
    fn type_arity(&self) -> TypeArity<Kind> {
        TypeArity {
            params: (self.params.iter()).map(|(_, kd)| kd.clone()).collect(),
            kd: Kind::VType,
        }
    }
}

impl Codata<TypeV, DtorV, RcType> {
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

impl TypeCheck for Span<&Data<TypeV, CtorV, RcType>> {
    type Ctx = Ctx;
    type Out = ();

    fn syn_step(
        &self, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        let data = self.inner_ref();
        for (tvar, kd) in data.params.iter() {
            ctx.type_ctx.insert(tvar.clone(), kd.clone().into());
        }
        let mut ctorvs = HashSet::new();
        for DataBr(ctorv, tys) in data.ctors.iter() {
            let span = ctorv.span();
            if ctorvs.contains(ctorv) {
                Err(span.make(
                    NameResolveError::DuplicateCtorDeclaration {
                        name: ctorv.clone(),
                    }
                    .into(),
                ))?;
            }
            ctorvs.insert(ctorv.clone());
            for ty in tys {
                span.make(ty.syn(ctx.clone())?).ensure(&Kind::VType, "data")?;
            }
        }
        Ok(Step::Done(()))
    }
}

impl TypeCheck for Span<&Codata<TypeV, DtorV, RcType>> {
    type Ctx = Ctx;
    type Out = ();

    fn syn_step(
        &self, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        let data = self.inner_ref();
        for (tvar, kd) in data.params.iter() {
            ctx.type_ctx.insert(tvar.clone(), kd.clone().into());
        }
        let mut dtorvs = HashSet::new();
        for CodataBr(dtorv, tys, ty) in data.dtors.iter() {
            let span = dtorv.span();
            if dtorvs.contains(dtorv) {
                Err(span.make(
                    NameResolveError::DuplicateDtorDeclaration {
                        name: dtorv.clone(),
                    }
                    .into(),
                ))?;
            }
            dtorvs.insert(dtorv.clone());
            for ty in tys {
                span.make(ty.syn(ctx.clone())?)
                    .ensure(&Kind::VType, "codata")?;
            }
            span.make(ty.syn(ctx.clone())?).ensure(&Kind::CType, "codata")?;
        }
        Ok(Step::Done(()))
    }
}

pub struct Seal<T>(pub T);
impl<T> Eqv for Seal<T> {
    type Ctx = ();
    fn eqv(
        &self, _other: &Self, _ctx: (),
        _f: impl FnOnce() -> Span<TypeCheckError> + Clone,
    ) -> Result<(), Span<TypeCheckError>> {
        Ok(())
    }
}

impl TypeCheck for Span<Module> {
    type Ctx = Ctx;
    type Out = Seal<Ctx>;
    fn syn_step(
        &self, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        let Module { name: _, data, codata: coda, define, define_ext } =
            self.inner_ref();
        // register data and codata type declarations in the type context
        for DeclSymbol { inner: data, .. } in data {
            let res = ctx.type_ctx.insert(data.name.clone(), data.type_arity());
            if let Some(_) = res {
                Err(data.name.span().make(
                    NameResolveError::DuplicateTypeDeclaration {
                        name: data.name.clone(),
                    }
                    .into(),
                ))?;
            }
        }
        for DeclSymbol { inner: coda, .. } in coda {
            let res = ctx.type_ctx.insert(coda.name.clone(), coda.type_arity());
            if let Some(_) = res {
                Err(coda.name.span().make(
                    NameResolveError::DuplicateTypeDeclaration {
                        name: coda.name.clone(),
                    }
                    .into(),
                ))?;
            }
        }
        // type check data and codata type declarations
        for DeclSymbol { inner: data, .. } in data {
            data.name.span().make(data).syn(ctx.clone())?;
            ctx.data_ctx.insert(data.name.clone(), data.clone());
        }
        for DeclSymbol { inner: coda, .. } in coda {
            coda.name.span().make(coda).syn(ctx.clone())?;
            ctx.coda_ctx.insert(coda.name.clone(), coda.clone());
        }
        for DeclSymbol { inner: Define { name: (var, ty), def: () }, .. } in
            define_ext
        {
            ctx.term_ctx.insert(var.clone(), ty.inner_ref().clone());
        }
        // register term declarations in the term context
        for DeclSymbol { inner: Define { name, def }, external, .. } in define {
            bool_test(!external, || {
                name.span().make(
                    NameResolveError::ExternalDeclaration {
                        name: name.name().to_string(),
                    }
                    .into(),
                )
            })?;
            let ty_def = def.syn(ctx.clone())?;
            let span = name.span();
            let kd = span.make(ty_def.clone()).syn(ctx.clone())?;
            span.make(kd).ensure(&Kind::VType, "define")?;
            ctx.term_ctx.insert(name.clone(), ty_def);
        }
        Ok(Step::Done(Seal(ctx)))
    }
}

impl TypeCheck for Span<Program> {
    type Ctx = Ctx;
    type Out = Seal<(Ctx, Type)>;

    fn syn_step(
        &self, ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        let Program { module, entry } = self.inner_ref();
        let Seal(ctx) = module.syn(ctx)?;
        let ty = entry.syn(ctx.to_owned())?;
        let SynType::TypeApp(ty_app) = &ty.synty else {
            Err(self.span().make(WrongMain { found: ty.clone() }))?
        };
        if !(ty_app.tvar.name() == "OS") {
            Err(self.span().make(WrongMain { found: ty_app.clone().into() }))?
        };
        Ok(Step::Done(Seal((ctx, ty_app.clone().into()))))
    }
}

pub trait Eqv {
    type Ctx: Default;
    fn eqv(
        &self, other: &Self, ctx: Self::Ctx,
        f: impl FnOnce() -> Span<TypeCheckError> + Clone,
    ) -> Result<(), Span<TypeCheckError>>;
}

impl Span<Kind> {
    fn ensure(
        &self, kind: &Kind, context: &str,
    ) -> Result<(), Span<TypeCheckError>> {
        self.inner_ref().eqv(kind, (), || {
            self.span().make(KindMismatch {
                context: context.to_owned(),
                expected: kind.clone(),
                found: self.inner_ref().clone(),
            })
        })
    }
}
