use crate::{
    rc,
    statics::resolve::NameResolveError,
    syntax::env::Env,
    utils::{fmt::FmtArgs, monoid::Monoid},
};

use super::{err::TypeCheckError, syntax::*};
use crate::utils::span::{Span, SpanView};
use TypeCheckError::*;

#[derive(Clone, Default)]
pub struct Ctx {
    pub type_ctx: im::HashMap<TypeV, TypeArity<Kind>>,
    pub term_ctx: im::HashMap<TermV, Type>,
    pub data_ctx: im::HashMap<TypeV, Data<TypeV, CtorV, RcType>>,
    pub coda_ctx: im::HashMap<TypeV, Codata<TypeV, DtorV, RcType>>,
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
        typ_syn.eqv(&typ, || span.make(Subsumption))?;
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

impl TypeCheck for Span<Type> {
    type Ctx = Ctx;
    type Out = Kind;
    fn syn_step(
        &self, ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        let app = self.inner_ref().head_reduction()?;
        Ok(match &app.tctor {
            TCtor::Var(x) => {
                // type constructor
                let Some(TypeArity { params, kd }) = ctx.type_ctx.get(&x) else {
                    Err(self.span().make(
                        NameResolveError::UnboundTypeVariable {
                            tvar: x.to_owned(),
                        }.into()
                    ))?
                };
                bool_test(app.args.len() == params.len(), || {
                    self.span().make(ArityMismatch {
                        context: format!("{}", self.inner_ref().fmt()),
                        expected: params.len(),
                        found: app.args.len(),
                    })
                })?;
                for (arg, kd) in app.args.iter().zip(params.iter()) {
                    self.span()
                        .make(arg.syn(ctx.clone())?)
                        .ensure(kd, "type argument")?;
                }
                if let Some(kd_self) = self.inner_ref().kd {
                    self.span().make(kd_self).ensure(kd, "kind subsumption")?;
                }
                Step::Done(kd.clone())
            }
            TCtor::Thunk => match &app.args.as_slice() {
                &[arg] => {
                    self.span()
                        .make(arg.syn(ctx.clone())?)
                        .ensure(&Kind::VType, "thunk argument")?;
                    Step::Done(Kind::VType)
                }
                _ => Err(self.span().make(ArityMismatch {
                    context: format!("{}", self.inner_ref().fmt()),
                    expected: 1,
                    found: app.args.len(),
                }))?,
            },
            TCtor::Ret => match &app.args.as_slice() {
                &[arg] => {
                    self.span()
                        .make(arg.syn(ctx.clone())?)
                        .ensure(&Kind::VType, "return argument")?;
                    Step::Done(Kind::VType)
                }
                _ => Err(self.span().make(ArityMismatch {
                    context: format!("{}", self.inner_ref().fmt()),
                    expected: 1,
                    found: app.args.len(),
                }))?,
            },
            TCtor::OS => match &app.args.as_slice() {
                &[] => Step::Done(Kind::CType),
                _ => Err(self.span().make(ArityMismatch {
                    context: format!("{}", self.inner_ref().fmt()),
                    expected: 0,
                    found: app.args.len(),
                }))?,
            },
            TCtor::Fun => match &app.args.as_slice() {
                &[arg1, arg2] => {
                    self.span()
                        .make(arg1.syn(ctx.clone())?)
                        .ensure(&Kind::VType, "function argument")?;
                    self.span()
                        .make(arg2.syn(ctx.clone())?)
                        .ensure(&Kind::CType, "function argument")?;
                    Step::Done(Kind::CType)
                }
                _ => Err(self.span().make(ArityMismatch {
                    context: format!("{}", self.inner_ref().fmt()),
                    expected: 2,
                    found: app.args.len(),
                }))?,
            },
        })
    }
}

impl TypeCheck for Span<&Literal> {
    type Ctx = ();
    type Out = Type;
    fn syn_step(
        &self, _ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        Ok(Step::Done(match self.inner_ref() {
            Literal::Int(_) => Type::internal("Int", vec![]),
            Literal::String(_) => Type::internal("String", vec![]),
            Literal::Char(_) => Type::internal("Char", vec![]),
        }))
    }
}

impl TypeCheck for Span<TermValue> {
    type Ctx = Ctx;
    type Out = Type;
    fn syn_step(
        &self, ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        Ok(match self.inner_ref() {
            TermValue::TermAnn(TermAnn { term, ty }) => {
                ty.span()
                    .make(ty.syn(ctx.clone())?)
                    .ensure(&Kind::VType, "value term annotation")?;
                Step::AnaMode((ctx, term), ty.inner_ref().clone())
            }
            TermValue::Var(x) => Step::Done(
                ctx.term_ctx
                    .get(x)
                    .cloned()
                    .ok_or(self.span().make(UnboundVar { var: x.clone() }))?,
            ),
            TermValue::Thunk(Thunk(c)) => {
                let c = c.syn(ctx)?.head_reduction()?;
                Step::Done(
                    TypeApp {
                        tctor: TCtor::Thunk,
                        args: vec![rc!(self.span().make(c.into()))],
                    }
                    .into(),
                )
                // Err(self
                //     .span()
                //     .make(NeedAnnotation { content: format!("thunk") }))?
            }
            TermValue::Ctor(_) => Err(self
                .span()
                .make(NeedAnnotation { content: format!("ctor") }))?,
            TermValue::Literal(l) => Step::Done(self.span().make(l).syn(())?),
        })
    }
    fn ana_step(
        &self, typ: Self::Out, ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        match self.inner_ref() {
            TermValue::Thunk(Thunk(c)) => {
                let app = typ.head_reduction()?;
                bool_test(app.tctor == TCtor::Thunk, || {
                    self.span().make(TypeExpected {
                        context: format!("thunk"),
                        expected: format!("{{a}}"),
                        found: typ.to_owned(),
                    })
                })?;
                bool_test(app.args.len() == 1, || {
                    self.span().make(ArityMismatch {
                        context: format!("thunk"),
                        expected: 1,
                        found: typ.app.args.len(),
                    })
                })?;
                let typ_comp = app.args[0].inner_ref().to_owned();
                c.ana(typ_comp, ctx)?;
                Ok(Step::Done(typ))
            }
            TermValue::Ctor(Ctor { ctor, args }) => {
                let ty_app = typ.head_reduction()?;
                let TCtor::Var(tvar) = &ty_app.tctor else {
                    Err(self.span().make(TypeExpected {
                        context: format!("ctor"),
                        expected: format!("{{a}}"),
                        found: typ.to_owned(),
                    }))?
                };
                let Data { name, params, ctors } =
                    ctx.data_ctx.get(tvar).cloned().ok_or(self.span().make(
                        NameResolve(NameResolveError::UnboundTypeVariable {
                            tvar: tvar.to_owned(),
                        }),
                    ))?;
                bool_test(params.len() == ty_app.args.len(), || {
                    self.span().make(ArityMismatch {
                        context: format!("data type `{}` instiantiation", name),
                        expected: params.len(),
                        found: ty_app.args.len(),
                    })
                })?;
                let diff = Env::from_iter(
                    params.iter().map(|(tvar, _kd)| tvar.to_owned()).zip(
                        ty_app
                            .args
                            .iter()
                            .map(|arg| arg.inner_ref().to_owned()),
                    ),
                );
                let DataBr(_ctorv, tys) = ctors
                    .into_iter()
                    .find(|DataBr(ctorv, _tys)| ctorv == ctor)
                    .ok_or(self.span().make(NameResolve(
                        NameResolveError::UnknownConstructor {
                            ctor: ctor.to_owned(),
                        },
                    )))?;
                bool_test(args.len() == tys.len(), || {
                    self.span().make(ArityMismatch {
                        context: format!("ctor"),
                        expected: tys.len(),
                        found: args.len(),
                    })
                })?;
                for (arg, ty) in args.iter().zip(tys.iter()) {
                    arg.ana(
                        ty.inner_ref().to_owned().subst(diff.clone()),
                        ctx.clone(),
                    )?;
                }
                Ok(Step::Done(typ))
            }
            _ => {
                let typ_syn = self.syn(ctx)?;
                typ.eqv(&typ_syn, || self.span().make(Subsumption))?;
                Ok(Step::Done(typ))
            }
        }
    }
}

impl TypeCheck for Span<TermComputation> {
    type Ctx = Ctx;
    type Out = Type;
    fn syn_step(
        &self, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        Ok(match self.inner_ref() {
            TermComputation::TermAnn(TermAnn { term, ty }) => {
                ty.span()
                    .make(ty.syn(ctx.clone())?)
                    .ensure(&Kind::CType, "computation term annotation")?;
                Step::AnaMode((ctx, term), ty.inner_ref().clone())
            }
            TermComputation::Ret(Ret(v)) => {
                // Err(self
                // .span()
                // .make(NeedAnnotation { content: format!("ret") }))?
                let app_body = v.syn(ctx.clone())?.head_reduction()?;
                let ty_body: Type = app_body.into();
                let kd = self.span().make(ty_body.clone()).syn(ctx)?;
                self.span().make(kd).ensure(&Kind::VType, "force")?;
                Step::Done(
                    TypeApp {
                        tctor: TCtor::Ret,
                        args: vec![rc!(self.span().make(ty_body))],
                    }
                    .into(),
                )
            }
            TermComputation::Force(Force(v)) => {
                let app_body = v.syn(ctx.clone())?.head_reduction()?;
                bool_test(app_body.tctor == TCtor::Thunk, || {
                    self.span().make(TypeExpected {
                        context: format!("force"),
                        expected: format!("{{a}}"),
                        found: app_body.clone().into(),
                    })
                })?;
                let ty_body: Type = app_body.into();
                let kd = self.span().make(ty_body.to_owned()).syn(ctx)?;
                self.span().make(kd).ensure(&Kind::CType, "force")?;
                Step::Done(ty_body)
            }
            TermComputation::Let(Let { var, def, body }) => {
                let ty_def = def.syn(ctx.clone())?;
                let kd = self.span().make(ty_def.clone()).syn(ctx.clone())?;
                self.span().make(kd).ensure(&Kind::VType, "let")?;
                ctx.term_ctx.insert(var.to_owned(), ty_def.clone());
                Step::SynMode((ctx, body))
            }
            TermComputation::Do(Do { var, comp, body }) => {
                let ty_comp = comp.syn(ctx.clone())?;
                let kd = self.span().make(ty_comp.clone()).syn(ctx.clone())?;
                self.span().make(kd).ensure(&Kind::CType, "do")?;
                let ty_app = ty_comp.head_reduction()?;
                bool_test(ty_app.tctor == TCtor::Ret, || {
                    self.span().make(TypeExpected {
                        context: format!("do"),
                        expected: format!("{{a}}"),
                        found: ty_comp.clone(),
                    })
                })?;
                let ty_val = ty_app.args[0].inner_ref().to_owned();
                ctx.term_ctx.insert(var.to_owned(), ty_val);
                Step::SynMode((ctx, body))
            }
            TermComputation::Rec(Rec { var: _, body: _ }) => Err(self
                .span()
                .make(NeedAnnotation { content: format!("rec") }))?,
            TermComputation::Match(_) => todo!(),
            TermComputation::CoMatch(_) => todo!(),
            TermComputation::Dtor(_) => todo!(),
        })
    }
    fn ana_step(
        &self, typ: Self::Out, ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        Ok(match self.inner_ref() {
            TermComputation::Ret(Ret(v)) => {
                let app = typ.head_reduction()?;
                let kd = self.span().make(typ.clone()).syn(ctx.clone())?;
                self.span().make(kd).ensure(&Kind::VType, "ret")?;
                bool_test(app.tctor == TCtor::Ret, || {
                    self.span().make(TypeExpected {
                        context: format!("ret"),
                        expected: format!("{{a}}"),
                        found: typ.clone(),
                    })
                })?;
                bool_test(app.args.len() == 1, || {
                    self.span().make(ArityMismatch {
                        context: format!("ret"),
                        expected: 1,
                        found: app.args.len(),
                    })
                })?;
                v.ana(app.args[1].inner_ref().to_owned(), ctx)?;
                Step::Done(typ)
            }
            TermComputation::Force(_) => todo!(),
            TermComputation::Let(_) => todo!(),
            TermComputation::Do(_) => todo!(),
            TermComputation::Rec(_) => todo!(),
            TermComputation::Match(_) => todo!(),
            TermComputation::CoMatch(_) => todo!(),
            TermComputation::Dtor(_) => todo!(),
            _ => {
                let typ_syn = self.syn(ctx)?;
                typ.eqv(&typ_syn, || self.span().make(Subsumption))?;
                Step::Done(typ)
            }
        })
    }
}

impl TypeCheck for Span<Module> {
    type Ctx = Ctx;
    type Out = ();
    fn syn_step(
        &self, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        let Module { name: _, data, codata, define, entry } = self.inner_ref();
        ctx.data_ctx.extend(
            data.into_iter().map(|data| (data.name.clone(), data.clone())),
        );
        ctx.coda_ctx.extend(
            codata.into_iter().map(|coda| (coda.name.clone(), coda.clone())),
        );
        for Define { name, def } in define {
            let def = def.syn(ctx.clone())?;
            ctx.term_ctx.insert(name.clone(), def);
        }
        let ty = entry.syn(ctx)?;
        match ty.app.tctor {
            TCtor::OS => Ok(()),
            _ => Err(self.span().make(WrongMain { found: ty })),
        }?;
        Ok(Step::Done(()))
    }
}

pub trait Eqv {
    fn eqv(
        &self, other: &Self, f: impl FnOnce() -> Span<TypeCheckError> + Clone,
    ) -> Result<(), Span<TypeCheckError>>;
}

fn bool_test<E>(b: bool, f: impl FnOnce() -> E) -> Result<(), E> {
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
    /// syntactic equality of type constructors
    fn eqv(
        &self, other: &Self, f: impl FnOnce() -> Span<TypeCheckError> + Clone,
    ) -> Result<(), Span<TypeCheckError>> {
        match (self, other) {
            (TCtor::Var(x), TCtor::Var(y)) => bool_test(x == y, f.clone()),
            (TCtor::OS, TCtor::OS)
            | (TCtor::Ret, TCtor::Ret)
            | (TCtor::Thunk, TCtor::Thunk)
            | (TCtor::Fun, TCtor::Fun) => Ok(()),
            (TCtor::Var(_), _)
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
        let lhs = self.head_reduction()?;
        let rhs = other.head_reduction()?;
        // both stuck type variable and type constructor
        lhs.tctor.eqv(&rhs.tctor, f.clone())?;
        // argument length must be equal
        bool_test(lhs.args.len() == rhs.args.len(), f.clone())?;
        // arguments must be equivalent
        for (ty1, ty2) in lhs.args.iter().zip(rhs.args.iter()) {
            ty1.inner_ref().eqv(ty2.inner_ref(), f.clone())?;
        }
        Ok(())
    }
}

impl Type {
    #[must_use]
    fn head_reduction(
        &self,
    ) -> Result<TypeApp<TCtor, RcType>, Span<TypeCheckError>> {
        let Type { app, kd: _, env } = self;
        // Note: the type is either a type constructor applied with types or a type variable
        if app.args.is_empty() {
            // type variable or data type with no parameters
            let mut tctor = app.tctor.clone();
            if let TCtor::Var(tvar) = &mut tctor {
                if let Some(ty) = env.get(tvar) {
                    return ty.head_reduction();
                }
            }
            // base case: stuck
            Ok(app.clone())
        } else {
            // base case: type constructor
            let args: Vec<_> = (app.args.iter())
                .map(|ty| {
                    let ty_subst = ty.inner_ref().clone().subst(env.clone());
                    rc!(ty.span().make(ty_subst))
                })
                .collect();
            let app = TypeApp { tctor: app.tctor.clone(), args };
            Ok(app)
        }
    }
}

impl Monoid for Env<TypeV, Type> {
    fn empty() -> Self {
        Self::new()
    }

    fn append(self, ori: Self) -> Self {
        // append on Env is actually composing lazy substitutions, effectively
        //       M [\gamma] [\delta] = M [\delta . \gamma]
        // where we refer to gamma as "original" and delta as "diff" then
        //      new = append(diff, original)
        let mut new = Self::new();
        for (x, ty) in self.clone() {
            if !ori.contains_key(&x) {
                new.insert(x, ty);
            }
        }
        for (x, ty) in ori {
            new.insert(x, ty.subst(self.clone()));
        }
        new
    }
}

impl Type {
    fn subst(self, diff: Env<TypeV, Type>) -> Self {
        let Type { app, kd, env } = self;
        let env = diff.append(env);
        Type { app, kd, env }
    }
}

impl Span<Kind> {
    fn ensure(
        &self, kind: &Kind, context: &str,
    ) -> Result<(), Span<TypeCheckError>> {
        self.inner_ref().eqv(kind, || {
            self.span().make(KindMismatch {
                context: context.to_owned(),
                expected: kind.clone(),
                found: self.inner_ref().clone(),
            })
        })
    }
}
