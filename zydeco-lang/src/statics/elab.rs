use super::{syntax::*, TypeCheckError};
use crate::{
    parse::syntax as ps,
    rc,
    syntax::{span::span, Span},
};

impl TryFrom<ps::Module> for Module {
    type Error = TypeCheckError;
    fn try_from(
        ps::Module { name, declarations, entry }: ps::Module,
    ) -> Result<Self, TypeCheckError> {
        let mut data = Vec::new();
        let mut codata = Vec::new();
        let mut define = Vec::new();
        for declaration in declarations {
            match declaration.inner {
                ps::Declaration::Data(d) => data.push(d.try_into()?),
                ps::Declaration::Codata(d) => codata.push(d.try_into()?),
                ps::Declaration::Define(d) => define.push(d.try_into()?),
            }
        }
        let entry = entry.try_map(TryInto::try_into)?;
        Ok(Self { name, data, codata, define, entry })
    }
}

impl TryFrom<ps::Data<TypeV, CtorV, Span<ps::Type>>>
    for Data<TypeV, CtorV, RcType>
{
    type Error = TypeCheckError;
    fn try_from(
        Data { name, params, ctors }: ps::Data<TypeV, CtorV, Span<ps::Type>>,
    ) -> Result<Self, TypeCheckError> {
        Ok(Self {
            name,
            params,
            ctors: ctors
                .into_iter()
                .map(TryInto::try_into)
                .collect::<Result<_, _>>()?,
        })
    }
}

impl TryFrom<ps::DataBr<CtorV, Span<ps::Type>>> for DataBr<CtorV, RcType> {
    type Error = TypeCheckError;
    fn try_from(
        DataBr(ctor, params): ps::DataBr<CtorV, Span<ps::Type>>,
    ) -> Result<Self, TypeCheckError> {
        let params = params
            .into_iter()
            .map(|param| param.try_map(TryInto::try_into))
            .map(|param| param.map(|ty| rc!(ty)))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self(ctor, params))
    }
}

impl TryFrom<ps::Codata<TypeV, DtorV, Span<ps::Type>>>
    for Codata<TypeV, DtorV, RcType>
{
    type Error = TypeCheckError;
    fn try_from(
        Codata { name, params, dtors }: ps::Codata<
            TypeV,
            DtorV,
            Span<ps::Type>,
        >,
    ) -> Result<Self, TypeCheckError> {
        Ok(Self {
            name,
            params,
            dtors: dtors
                .into_iter()
                .map(TryInto::try_into)
                .collect::<Result<_, _>>()?,
        })
    }
}

impl TryFrom<ps::CodataBr<DtorV, Span<ps::Type>>> for CodataBr<DtorV, RcType> {
    type Error = TypeCheckError;
    fn try_from(
        CodataBr(dtor, params, ty): ps::CodataBr<DtorV, Span<ps::Type>>,
    ) -> Result<Self, TypeCheckError> {
        let params = params
            .into_iter()
            .map(|param| param.try_map(TryInto::try_into))
            .map(|param| param.map(|ty| rc!(ty)))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self(dtor, params, rc!(ty.try_map(TryInto::try_into)?)))
    }
}

impl TryFrom<ps::Define> for Define<TermV, RcValue> {
    type Error = TypeCheckError;
    fn try_from(
        ps::Define { rec, fun, name, params, def }: ps::Define,
    ) -> Result<Self, TypeCheckError> {
        let (name, def) = desugar_gen_let(rec, fun, name, params, def)?;
        Ok(Self { name, def })
    }
}

fn desugar_gen_let(
    rec: bool, fun: bool, (var, ty): (TermV, Option<Span<ps::Type>>),
    params: Vec<(TermV, Option<Span<ps::Type>>)>, def: Box<Span<ps::Term>>,
) -> Result<(TermV, RcValue), TypeCheckError> {
    let name = var.clone();
    match (rec, fun, def.inner) {
        (false, false, ps::Term::Value(value)) => {
            Ok((name, rc!(def.info.make(value.try_into()?))))
        }
        (_, _, ps::Term::Value(_)) => Err(TypeCheckError::KindMismatch {
            context: format!("desugaring let"),
            expected: Kind::CType,
            found: Kind::VType,
        }),
        (false, false, ps::Term::Computation(_)) => {
            Err(TypeCheckError::KindMismatch {
                context: format!("desugaring let"),
                expected: Kind::VType,
                found: Kind::CType,
            })
        }
        (rec, fun, ps::Term::Computation(body)) => {
            let mut body = Box::new(def.info.make(body));
            if let Some(ty) = ty {
                body = Box::new(def.info.make(ps::TermAnn { body, ty }.into()));
            }
            if fun {
                body = Box::new(
                    def.info.make(ps::Abstraction { params, body }.into()),
                );
            }
            if rec {
                body = Box::new(
                    def.info.make(Rec { var: (var, None), body }.into()),
                );
            }
            Ok((
                name,
                rc!(def.info.make(
                    Thunk(rc!((*body).try_map(TryInto::try_into)?)).into()
                )),
            ))
        }
    }
}

fn desugar_fn(
    ps::Abstraction { params, body }: ps::Abstraction,
) -> Result<TermComputation, TypeCheckError> {
    fn desugar_fn_one(
        (var, _ty): (TermV, Option<Span<ps::Type>>), body: RcComp,
    ) -> Result<TermComputation, TypeCheckError> {
        Ok(CoMatch {
            arms: vec![ps::CoMatcher {
                dtor: DtorV::new(format!("arg"), span(0, 0)),
                vars: vec![var],
                body,
            }],
        }
        .into())
    }
    let mut func = TryInto::try_into(body.inner)?;
    for param in params.into_iter().rev() {
        func = desugar_fn_one(param, rc!(body.info.clone().make(func)))?;
    }
    Ok(func)
}

impl TryFrom<ps::Type> for Type {
    type Error = TypeCheckError;
    fn try_from(ty: ps::Type) -> Result<Self, TypeCheckError> {
        Ok(match ty {
            ps::Type::Basic(tctor) => {
                TypeApp { tctor, args: Vec::new() }.into()
            }
            ps::Type::App(t) => {
                let ps::TypeApp(t1, t2) = t;
                let t1: Type = TryInto::try_into(t1.inner())?;
                let t2 = t2.try_map(TryInto::try_into)?;
                t1.app(rc!(t2))
            }
            ps::Type::Arrow(t) => {
                let ps::Arrow(t1, t2) = t;
                let t1 = t1.try_map(TryInto::try_into)?;
                let t2 = t2.try_map(TryInto::try_into)?;
                TypeApp { tctor: TCtor::Fun, args: vec![rc!(t1), rc!(t2)] }
                    .into()
            }
        })
    }
}

impl Type {
    fn app(self, t2: RcType) -> Type {
        match self {
            Type::TypeApp(t) => {
                let TypeApp { tctor, mut args } = t;
                args.push(t2);
                TypeApp { tctor, args }.into()
            }
            Type::TypeAnn(t) => {
                let TypeAnn { ty, kd } = t;
                let ty = ty.as_ref().clone();
                TypeAnn { ty: rc!(ty.map(|ty| ty.app(t2))), kd }.into()
            }
        }
    }
}

impl TryFrom<ps::TermValue> for TermValue {
    type Error = TypeCheckError;
    fn try_from(value: ps::TermValue) -> Result<Self, TypeCheckError> {
        Ok(match value {
            ps::TermValue::TermAnn(t) => {
                let TermAnn { body, ty } = t;
                TermAnn {
                    body: rc!(body.try_map(TryInto::try_into)?),
                    ty: rc!(ty.try_map(TryInto::try_into)?),
                }
                .into()
            }
            ps::TermValue::Var(x) => x.into(),
            ps::TermValue::Thunk(t) => {
                let Thunk(body) = t;
                Thunk(rc!((body).try_map(TryInto::try_into)?)).into()
            }
            ps::TermValue::Ctor(t) => {
                let Ctor { ctor, args } = t;
                Ctor {
                    ctor,
                    args: args
                        .into_iter()
                        .map(|arg| arg.try_map(TryInto::try_into))
                        .collect::<Result<Vec<_>, _>>()?
                        .into_iter()
                        .map(|arg| rc!(arg))
                        .collect(),
                }
                .into()
            }
            ps::TermValue::Literal(t) => t.into(),
        })
    }
}

impl TryFrom<ps::TermComputation> for TermComputation {
    type Error = TypeCheckError;
    fn try_from(comp: ps::TermComputation) -> Result<Self, TypeCheckError> {
        Ok(match comp {
            ps::TermComputation::TermAnn(t) => {
                let TermAnn { body, ty } = t;
                TermAnn {
                    body: rc!(body.try_map(TryInto::try_into)?),
                    ty: rc!(ty.try_map(TryInto::try_into)?),
                }
                .into()
            }
            ps::TermComputation::Ret(t) => {
                let Ret(body) = t;
                Ret(rc!((body).try_map(TryInto::try_into)?)).into()
            }
            ps::TermComputation::Force(t) => {
                let Force(body) = t;
                Force(rc!((body).try_map(TryInto::try_into)?)).into()
            }
            ps::TermComputation::Let(t) => {
                let ps::Let {
                    gen: ps::GenLet { rec, fun, name, params, def },
                    body,
                } = t;
                let (var, def) = desugar_gen_let(rec, fun, name, params, def)?;
                Let { var, def, body: rc!((body).try_map(TryInto::try_into)?) }
                    .into()
            }
            ps::TermComputation::Do(t) => {
                let ps::Do { var: (var, ty), comp, body } = t;
                let mut comp = rc!((comp).try_map(TryInto::try_into)?);
                if let Some(ty) = ty {
                    comp = rc!(comp.info.clone().make(
                        TermAnn {
                            body: comp,
                            ty: rc!(ty.try_map(TryInto::try_into)?),
                        }
                        .into(),
                    ));
                }
                let body = rc!((body).try_map(TryInto::try_into)?);
                Do { var, comp, body }.into()
            }
            ps::TermComputation::Rec(t) => {
                let Rec { var: (var, ty), body } = t;
                let mut body = rc!((body).try_map(TryInto::try_into)?);
                if let Some(ty) = ty {
                    body = rc!(body.info.clone().make(
                        TermAnn {
                            body,
                            ty: rc!(ty.try_map(TryInto::try_into)?),
                        }
                        .into(),
                    ));
                }
                Rec { var, body }.into()
            }
            ps::TermComputation::Match(t) => {
                let ps::Match { scrut, arms } = t;
                let scrut = rc!((scrut).try_map(TryInto::try_into)?);
                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        let ps::Matcher { ctor, vars, body } = arm;
                        let body = rc!((body).try_map(TryInto::try_into)?);
                        Ok(Matcher { ctor, vars, body })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Match { scrut, arms }.into()
            }
            ps::TermComputation::Abs(t) => desugar_fn(t)?,
            ps::TermComputation::App(t) => {
                let ps::Application { body, arg } = t;
                let fun = rc!((body).try_map(TryInto::try_into)?);
                let arg = arg.try_map(TryInto::try_into)?;
                Dtor {
                    body: fun,
                    dtor: DtorV::new(format!("arg"), span(0, 0)),
                    args: vec![rc!(arg)],
                }
                .into()
            }
            ps::TermComputation::CoMatch(t) => {
                let ps::CoMatch { arms } = t;
                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        let ps::CoMatcher { dtor, vars, body } = arm;
                        let body = rc!((body).try_map(TryInto::try_into)?);
                        Ok(CoMatcher { dtor, vars, body })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                CoMatch { arms }.into()
            }
            ps::TermComputation::Dtor(t) => {
                let ps::Dtor { body, dtor, args } = t;
                let body = rc!((body).try_map(TryInto::try_into)?);
                let args = args
                    .into_iter()
                    .map(|arg| arg.try_map(TryInto::try_into))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .map(|arg| rc!(arg))
                    .collect();
                Dtor { body, dtor, args }.into()
            }
        })
    }
}
