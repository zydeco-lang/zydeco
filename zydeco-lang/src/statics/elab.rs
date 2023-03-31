use super::{err::TypeCheckError, resolve::NameResolveError, syntax::*};
use crate::{
    parse::syntax as ps,
    rc,
    utils::span::{span, Span, SpanView},
};

fn desugar_gen_let(
    rec: bool, fun: bool, (var, ty): (TermV, Option<Span<ps::Type>>),
    params: Vec<(TermV, Option<Span<ps::Type>>)>,
    def: Option<Box<Span<ps::Term>>>,
) -> Result<(TermV, RcType, Option<RcValue>), TypeCheckError> {
    let name = var.clone();
    let ty_rc = {
        if let Some(ty) = ty.clone() {
            rc!(ty.try_map(TryInto::try_into)?)
        } else {
            rc!(var.span().make(Hole.into()))
        }
    };
    let Some(def) = def else {
        return Ok((name, ty_rc, None));
    };
    match (rec, fun, def.inner) {
        (false, false, ps::Term::Value(value)) => {
            Ok((name, ty_rc, Some(rc!(def.info.make(value.try_into()?)))))
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
            if fun {
                let params = params.clone();
                body = Box::new(
                    def.info.make(ps::Abstraction { params, body }.into()),
                );
            }
            if rec {
                body = Box::new(
                    def.info.make(Rec { var: (var, None), body }.into()),
                );
            }
            let mut ty: RcType = if let Some(ty) = ty {
                rc!(ty.try_map(TryInto::try_into)?)
            } else {
                rc!(def.info.make(Hole.into()))
            };
            for (var, ty_param) in params.iter().rev() {
                let ty_dom = if let Some(ty_dom) = ty_param {
                    rc!(ty_dom.to_owned().try_map(TryInto::try_into)?)
                } else {
                    rc!(def.info.make(Hole.into()))
                };
                ty =
                    rc!(var.span().make(Type::internal("Fn", vec![ty_dom, ty])))
            }
            let body = rc!((*body).try_map(TryInto::try_into)?);
            ty = rc!(def.info.make(Type::make_thunk(ty)));
            Ok((name, ty, Some(rc!(def.info.make(Thunk(body).into())))))
        }
    }
}

fn desugar_fn(
    ps::Abstraction { params, body }: ps::Abstraction,
) -> Result<TermComputation, TypeCheckError> {
    fn desugar_fn_one(
        (var, ty): (TermV, Option<Span<ps::Type>>), body: RcComp,
    ) -> Result<TermComputation, TypeCheckError> {
        let mut body = CoMatch {
            arms: vec![ps::CoMatcher {
                dtor: DtorV::new(format!("arg"), span(0, 0)),
                vars: vec![var],
                body,
            }],
        }
        .into();
        if let Some(ty) = ty {
            let mut ty = ty.try_map(TryInto::try_into)?;
            let span = ty.span().clone();
            ty = span.make(Type::internal(
                "Fn",
                vec![rc!(ty), rc!(span.make(Hole.into()))],
            ));
            body = TermAnn { term: rc!(span.make(body)), ty: rc!(ty) }.into();
        }
        Ok(body)
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
            ps::Type::Basic(TCtor::Thunk) => {
                Type::internal("Thunk_U", Vec::new())
            }
            ps::Type::Basic(TCtor::Ret) => Type::internal("Ret_F", Vec::new()),
            ps::Type::Basic(TCtor::Var(v)) => {
                TypeApp { tvar: v, args: vec![] }.into()
            }
            ps::Type::App(t) => {
                let ps::TypeApp(t1, t2) = t;
                let t1: Type = TryInto::try_into(t1.inner())?;
                let t2 = t2.try_map(TryInto::try_into)?;
                let SynType::TypeApp(mut t1) = t1.synty else {
                     Err(TypeCheckError::KindMismatch {
                        context: format!("desugaring type application"),
                        expected: Kind::CType,
                        found: Kind::VType,
                    })?
                };
                t1.args.push(rc!(t2));
                t1.into()
            }
            ps::Type::Arrow(t) => {
                let ps::Arrow(t1, t2) = t;
                let t1 = t1.try_map(TryInto::try_into)?;
                let t2 = t2.try_map(TryInto::try_into)?;
                Type::internal("Fn", vec![rc!(t1), rc!(t2)])
            }
            ps::Type::Forall(ps::Forall(params, t)) => {
                let mut t = t.try_map(TryInto::try_into)?;
                for (param, kd) in params.into_iter().rev() {
                    t = t
                        .span()
                        .clone()
                        .make(Forall { param, kd, ty: rc!(t) }.into())
                }
                t.inner
            }
            ps::Type::Exists(ps::Exists(params, t)) => {
                let mut t = t.try_map(TryInto::try_into)?;
                for (param, kd) in params.into_iter().rev() {
                    t = t
                        .span()
                        .clone()
                        .make(Exists { param, kd, ty: rc!(t) }.into())
                }
                t.inner
            }
            ps::Type::Hole(ps::Hole) => Hole.into(),
        })
    }
}

impl TryFrom<ps::TermValue> for TermValue {
    type Error = TypeCheckError;
    fn try_from(value: ps::TermValue) -> Result<Self, TypeCheckError> {
        Ok(match value {
            ps::TermValue::TermAnn(TermAnn { term: body, ty }) => TermAnn {
                term: rc!(body.try_map(TryInto::try_into)?),
                ty: rc!(ty.try_map(TryInto::try_into)?),
            }
            .into(),
            ps::TermValue::Var(x) => x.into(),
            ps::TermValue::Thunk(Thunk(body)) => {
                Thunk(rc!((body).try_map(TryInto::try_into)?)).into()
            }
            ps::TermValue::Ctor(Ctor { ctor, args }) => Ctor {
                ctor,
                args: args
                    .into_iter()
                    .map(|arg| arg.try_map(TryInto::try_into))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .map(|arg| rc!(arg))
                    .collect(),
            }
            .into(),
            ps::TermValue::Literal(t) => t.into(),
            ps::TermValue::Pack(ps::Pack { ty, body }) => {
                let ty = ty.try_map(TryInto::try_into)?;
                let body = body.try_map(TryInto::try_into)?;
                Pack { ty: rc!(ty), body: rc!(body) }.into()
            }
        })
    }
}

impl TryFrom<ps::TermComputation> for TermComputation {
    type Error = TypeCheckError;
    fn try_from(comp: ps::TermComputation) -> Result<Self, TypeCheckError> {
        Ok(match comp {
            ps::TermComputation::TermAnn(TermAnn { term: body, ty }) => {
                TermAnn {
                    term: rc!(body.try_map(TryInto::try_into)?),
                    ty: rc!(ty.try_map(TryInto::try_into)?),
                }
                .into()
            }
            ps::TermComputation::Ret(Ret(body)) => {
                let body: TermComputation =
                    Ret(rc!((body).try_map(TryInto::try_into)?)).into();
                body
            }
            ps::TermComputation::Force(Force(body)) => {
                Force(rc!((body).try_map(TryInto::try_into)?)).into()
            }
            ps::TermComputation::Let(ps::Let {
                gen: ps::GenLet { rec, fun, name, params, def },
                body,
            }) => {
                let (var, ty, def) =
                    desugar_gen_let(rec, fun, name, params, def)?;
                let Some(def) = def else {
                    Err(NameResolveError::EmptyDeclaration { name: var.name().to_string() })?
                };
                let mut def = def;
                let span = def.span().clone();
                def = rc!(span.make(ps::TermAnn { term: def, ty }.into()));
                let body = rc!((body).try_map(TryInto::try_into)?);
                Let { var, def, body }.into()
            }
            ps::TermComputation::Do(ps::Do { var: (var, ty), comp, body }) => {
                let mut comp = rc!((comp).try_map(TryInto::try_into)?);
                if let Some(ty) = ty {
                    comp = rc!(comp.info.clone().make(
                        TermAnn {
                            term: comp,
                            ty: rc!(ty.try_map(TryInto::try_into)?),
                        }
                        .into(),
                    ));
                }
                let body = rc!((body).try_map(TryInto::try_into)?);
                Do { var, comp, body }.into()
            }
            ps::TermComputation::Rec(Rec { var: (var, ty), body }) => {
                let body = rc!((body).try_map(TryInto::try_into)?);
                let span = body.span().clone();
                let mut body: TermComputation = Rec { var, body }.into();
                if let Some(ty) = ty {
                    let ty: Span<Type> = ty.try_map(TryInto::try_into)?;
                    let ty_ = ty.inner.clone();
                    let SynType::TypeApp(ty_app) = ty.inner.synty else {
                        Err(TypeCheckError::TypeExpected {
                            context: format!("elaborating recursion"),
                            expected: format!("{{a}}"),
                            found: ty_.clone()
                        })?
                    };
                    let Some(ty) = ty_app.elim_thunk() else {
                        Err(TypeCheckError::TypeExpected {
                            context: format!("elaborating recursion"),
                            expected: format!("{{a}}"),
                            found: ty_
                        })?
                    };
                    body = TermAnn {
                        term: rc!(span.make(body)),
                        ty: rc!(span.make(ty)),
                    }
                    .into();
                }
                body
            }
            ps::TermComputation::Match(ps::Match { scrut, arms }) => {
                let scrut = rc!((scrut).try_map(TryInto::try_into)?);
                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        let ps::Matcher { ctor, vars, body } = arm;
                        let body = rc!((body).try_map(TryInto::try_into)?);
                        Ok(Matcher { ctor, vars, body })
                    })
                    .collect::<Result<_, TypeCheckError>>()?;
                Match { scrut, arms }.into()
            }
            ps::TermComputation::Abs(t) => desugar_fn(t)?,
            ps::TermComputation::App(ps::Application { body, arg }) => {
                let fun = rc!((body).try_map(TryInto::try_into)?);
                let arg = arg.try_map(TryInto::try_into)?;
                Dtor {
                    body: fun,
                    dtor: DtorV::new(format!("arg"), span(0, 0)),
                    args: vec![rc!(arg)],
                }
                .into()
            }
            ps::TermComputation::CoMatch(ps::CoMatch { arms }) => {
                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        let ps::CoMatcher { dtor, vars, body } = arm;
                        let body = rc!((body).try_map(TryInto::try_into)?);
                        Ok(CoMatcher { dtor, vars, body })
                    })
                    .collect::<Result<Vec<_>, TypeCheckError>>()?;
                CoMatch { arms }.into()
            }
            ps::TermComputation::Dtor(ps::Dtor { body, dtor, args }) => {
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
            ps::TermComputation::TypAbs(ps::TypAbs { params, body }) => {
                let body = (body).try_map(TryInto::try_into)?;
                let mut body = body;
                for (tvar, kd) in params.into_iter().rev() {
                    body = body
                        .info
                        .clone()
                        .make(TypAbs { tvar, kd, body: rc!(body) }.into());
                }
                body.inner
            }
            ps::TermComputation::TypApp(ps::TypApp { body, arg }) => {
                let body = rc!((body).try_map(TryInto::try_into)?);
                let arg = rc!(arg.try_map(TryInto::try_into)?);
                TypApp { body, arg }.into()
            }
            ps::TermComputation::MatchPack(ps::MatchPack {
                scrut,
                tvar,
                var,
                body,
            }) => {
                let scrut = rc!((scrut).try_map(TryInto::try_into)?);
                let body = rc!((body).try_map(TryInto::try_into)?);
                MatchPack { scrut, tvar, var, body }.into()
            }
        })
    }
}

impl TryFrom<ps::Term> for Term {
    type Error = TypeCheckError;
    fn try_from(term: ps::Term) -> Result<Self, TypeCheckError> {
        Ok(match term {
            ps::Term::Value(t) => Term::Value(t.try_into()?),
            ps::Term::Computation(t) => Term::Computation(t.try_into()?),
        })
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

impl TryFrom<ps::Module> for Module {
    type Error = TypeCheckError;
    fn try_from(
        ps::Module { name, declarations }: ps::Module,
    ) -> Result<Self, TypeCheckError> {
        let mut data = Vec::new();
        let mut codata = Vec::new();
        let mut define = Vec::new();
        let mut define_ext = Vec::new();
        for declaration in declarations {
            let DeclSymbol { public, external, inner } = declaration;
            match inner {
                ps::Declaration::Data(d) => data.push(DeclSymbol {
                    public,
                    external,
                    inner: d.try_into()?,
                }),
                ps::Declaration::Codata(d) => codata.push(DeclSymbol {
                    public,
                    external,
                    inner: d.try_into()?,
                }),
                ps::Declaration::Define(d) => {
                    let ps::GenLet { rec, fun, name, params, def } = d;
                    let (name, ty, te) =
                        desugar_gen_let(rec, fun, name, params, def)?;
                    if external {
                        define_ext.push(DeclSymbol {
                            public,
                            external,
                            inner: Define { name: (name, ty), def: () },
                        })
                    } else {
                        let term = te.ok_or_else(|| {
                            NameResolveError::EmptyDeclaration {
                                name: name.name().to_string(),
                            }
                        })?;
                        let span = term.span().clone();
                        let def = rc!(span.make(TermAnn { term, ty }.into()));
                        define.push(DeclSymbol {
                            public,
                            external,
                            inner: Define { name, def },
                        })
                    }
                }
            }
        }
        Ok(Self { name, data, codata, define, define_ext })
    }
}

impl TryFrom<ps::Program> for Program {
    type Error = TypeCheckError;

    fn try_from(value: ps::Program) -> Result<Self, Self::Error> {
        let ps::Program { module, entry } = value;
        let module = module.try_map(TryInto::try_into)?;
        let entry = entry.try_map(TryInto::try_into)?;
        Ok(Self { module, entry })
    }
}
