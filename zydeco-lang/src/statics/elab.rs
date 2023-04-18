use super::{err::TyckErrorItem, syntax::*};
use crate::{
    parse::syntax as ps,
    rc,
    resolve::err::NameResolveError,
    utils::span::{Span, SpanInfo, SpanView},
};
use im::vector;

pub trait Elaboration<T>: Sized {
    type Error;
    fn elab(value: T) -> Result<Self, Self::Error>;
}

impl<T, S> Elaboration<Span<T>> for Span<S>
where
    S: Elaboration<T>,
{
    type Error = S::Error;
    fn elab(value: Span<T>) -> Result<Self, Self::Error> {
        value.try_map(Elaboration::elab)
    }
}

impl<T, S> Elaboration<Vec<T>> for Vec<S>
where
    S: Elaboration<T>,
{
    type Error = S::Error;
    fn elab(value: Vec<T>) -> Result<Self, Self::Error> {
        value.into_iter().map(Elaboration::elab).collect()
    }
}

impl<T, S> Elaboration<(NameDef, T)> for (TypeV, S)
where
    S: Elaboration<T>,
{
    type Error = S::Error;
    fn elab(value: (NameDef, T)) -> Result<Self, Self::Error> {
        Ok((value.0.into(), Elaboration::elab(value.1)?))
    }
}

impl<T, S> Elaboration<(NameRef, T)> for (TypeV, S)
where
    S: Elaboration<T>,
{
    type Error = S::Error;
    fn elab(value: (NameRef, T)) -> Result<Self, Self::Error> {
        Ok((value.0.into(), Elaboration::elab(value.1)?))
    }
}

impl Elaboration<ps::Kind> for Kind {
    type Error = TyckErrorItem;
    fn elab(kd: ps::Kind) -> Result<Self, Self::Error> {
        match kd {
            ps::Kind::Base(kd) => Ok(Kind::Base(kd)),
            ps::Kind::Arrow(ps::Arrow(k, kd)) => {
                let kd: Span<Kind> = Elaboration::elab(*kd)?;
                match kd.inner {
                    Kind::Base(_) => Ok(TypeArity {
                        params: vec![k.try_map(Elaboration::elab)?],
                        kd: Box::new(kd),
                    }
                    .into()),
                    Kind::TypeArity(TypeArity { mut params, kd }) => {
                        params.insert(0, k.try_map(Elaboration::elab)?);
                        Ok(TypeArity { params, kd }.into())
                    }
                }
            }
        }
    }
}

fn desugar_gen_let(
    rec: bool, fun: bool, (var, ty): (NameDef, Option<Span<ps::Type>>), params: Vec<ps::Pattern>,
    def: Option<Box<Span<ps::Term>>>,
) -> Result<(TermV, RcType, Option<RcValue>), TyckErrorItem> {
    let name = var.clone().into();
    let ty_rc = {
        if let Some(ty) = ty.clone() {
            rc!(ty.try_map(Elaboration::elab)?)
        } else {
            rc!(var.span().make(Hole.into()))
        }
    };
    let Some(def) = def else {
        return Ok((name, ty_rc, None));
    };
    match (rec, fun, def.inner) {
        (false, false, ps::Term::Value(value)) => {
            Ok((name, ty_rc, Some(rc!(def.info.make(Elaboration::elab(value)?)))))
        }
        (_, _, ps::Term::Value(_)) => Err(TyckErrorItem::KindMismatch {
            context: format!("desugaring let"),
            expected: KindBase::CType.into(),
            found: KindBase::VType.into(),
        }),
        (false, false, ps::Term::Computation(_)) => Err(TyckErrorItem::KindMismatch {
            context: format!("desugaring let"),
            expected: KindBase::VType.into(),
            found: KindBase::CType.into(),
        }),
        (rec, fun, ps::Term::Computation(body)) => {
            let mut body = Box::new(def.info.make(body));
            if fun {
                let param = params.clone().into_iter().collect();
                body = Box::new(def.info.make(ps::Abs { param, body }.into()));
            }
            if rec {
                body = Box::new(def.info.make(Rec { var: (var, None), body }.into()));
            }
            let mut ty: RcType = if let Some(ty) = ty {
                rc!(ty.try_map(Elaboration::elab)?)
            } else {
                rc!(def.info.make(Hole.into()))
            };
            for param in params.iter().rev() {
                match param {
                    ps::Pattern::TypePattern((tvar, kd_param)) => {
                        let Some(kd_dom) = kd_param else {
                            Err(TyckErrorItem::NeedAnnotation {
                                content: format!("gen let type variable elabrotion")
                            })?
                        };
                        let kd_dom = kd_dom.clone().try_map(Elaboration::elab)?;
                        ty = rc!(tvar
                            .span()
                            .make(Forall { param: (tvar.into(), kd_dom.clone()), ty }.into()))
                    }
                    ps::Pattern::TermPattern((var, ty_param)) => {
                        let ty_dom = if let Some(ty_dom) = ty_param {
                            rc!(ty_dom.to_owned().try_map(Elaboration::elab)?)
                        } else {
                            rc!(def.info.make(Hole.into()))
                        };
                        ty = rc!(var.span().make(Type::internal("Fn", vec![ty_dom, ty])))
                    }
                }
            }
            let body = rc!((*body).try_map(Elaboration::elab)?);
            ty = rc!(def.info.make(Type::make_thunk(ty)));
            Ok((name, ty, Some(rc!(def.info.make(Thunk(body).into())))))
        }
    }
}

fn desugar_fn(
    ps::Abs { param, body }: ps::Abs<Vec<ps::Pattern>, ps::BoxComp>,
) -> Result<TermComputation, TyckErrorItem> {
    fn desugar_fn_one(
        (var, ty): (NameDef, Option<Span<ps::Type>>), body: RcComp,
    ) -> Result<TermComputation, TyckErrorItem> {
        let mut body = Comatch {
            arms: vec![ps::Comatcher {
                dtorv: DtorV::new(format!("arg"), SpanInfo::dummy()),
                vars: vec![var.into()],
                body,
            }],
        }
        .into();
        if let Some(ty) = ty {
            let mut ty = ty.try_map(Elaboration::elab)?;
            let span = ty.span().clone();
            ty = span.make(Type::internal("Fn", vec![rc!(ty), rc!(span.make(Hole.into()))]));
            body = Annotation { term: rc!(span.make(body)), ty: rc!(ty) }.into();
        }
        Ok(body)
    }
    let mut func = Elaboration::elab(body.inner)?;
    for param in param.into_iter().rev() {
        match param {
            ps::Pattern::TypePattern((tvar, kd)) => {
                let kd = match kd {
                    Some(kd) => Some(kd.try_map(Elaboration::elab)?),
                    None => None,
                };
                func =
                    TyAbsTerm { param: (tvar.into(), kd), body: rc!(body.info.make(func)) }.into()
            }
            ps::Pattern::TermPattern(param) => {
                func = desugar_fn_one(param, rc!(body.info.make(func)))?;
            }
        }
    }
    Ok(func)
}

impl Elaboration<ps::Type> for Type {
    type Error = TyckErrorItem;
    fn elab(ty: ps::Type) -> Result<Self, TyckErrorItem> {
        Ok(match ty {
            ps::Type::Basic(tvar) => TypeApp { tvar: tvar.into(), args: vec![] }.into(),
            ps::Type::App(t) => {
                let ps::TypeApp(t1, t2) = t;
                let t1: Type = Elaboration::elab(t1.inner())?;
                let t2 = t2.try_map(Elaboration::elab)?;
                let SynType::TypeApp(mut t1) = t1.synty else {
                     Err(TyckErrorItem::KindMismatch {
                        context: format!("desugaring type application"),
                        expected: KindBase::CType.into(),
                        found: KindBase::VType.into(),
                    })?
                };
                t1.args.push(rc!(t2));
                t1.into()
            }
            ps::Type::Arrow(t) => {
                let ps::Arrow(t1, t2) = t;
                let t1 = t1.try_map(Elaboration::elab)?;
                let t2 = t2.try_map(Elaboration::elab)?;
                Type::internal("Fn", vec![rc!(t1), rc!(t2)])
            }
            ps::Type::Forall(ps::Forall { param: params, ty: t }) => {
                let mut t = t.try_map(Elaboration::elab)?;
                for (tvar, kd) in params.into_iter().rev() {
                    let kd = kd.ok_or_else(|| TyckErrorItem::NeedAnnotation {
                        content: format!("forall type variable elabrotion"),
                    })?;
                    let param = Elaboration::elab((tvar, kd))?;
                    t = t.span().clone().make(Forall { param, ty: rc!(t) }.into())
                }
                t.inner
            }
            ps::Type::Exists(ps::Exists { param: params, ty: t }) => {
                let mut t = t.try_map(Elaboration::elab)?;
                for (tvar, kd) in params.into_iter().rev() {
                    let kd = kd.ok_or_else(|| TyckErrorItem::NeedAnnotation {
                        content: format!("exists type variable elabrotion"),
                    })?;
                    let param = Elaboration::elab((tvar, kd))?;
                    t = t.span().clone().make(Exists { param, ty: rc!(t) }.into())
                }
                t.inner
            }
            ps::Type::Hole(ps::Hole) => Hole.into(),
        })
    }
}

impl Elaboration<ps::TermValue> for TermValue {
    type Error = TyckErrorItem;
    fn elab(value: ps::TermValue) -> Result<Self, TyckErrorItem> {
        Ok(match value {
            ps::TermValue::TermAnn(Annotation { term: body, ty }) => Annotation {
                term: rc!(body.try_map(Elaboration::elab)?),
                ty: rc!(ty.try_map(Elaboration::elab)?),
            }
            .into(),
            ps::TermValue::Var(x) => TermV::from(x).into(),
            ps::TermValue::Thunk(Thunk(body)) => {
                let span = body.span().clone();
                let body = Thunk(rc!((body).try_map(Elaboration::elab)?)).into();
                Annotation {
                    term: rc!(span.make(body)),
                    ty: rc!(span.make(Type::make_thunk(rc!(span.make(Hole.into()))))),
                }
                .into()
            }
            ps::TermValue::Ctor(Ctor { ctorv: ctor, args }) => Ctor {
                ctorv: ctor,
                args: Vec::<_>::elab(args)?.into_iter().map(|arg| rc!(arg)).collect(),
            }
            .into(),
            ps::TermValue::Literal(t) => t.into(),
            ps::TermValue::Pack(ps::Pack { ty, body }) => {
                let ty = ty.try_map(Elaboration::elab)?;
                let body = body.try_map(Elaboration::elab)?;
                Pack { ty: rc!(ty), body: rc!(body) }.into()
            }
        })
    }
}

impl Elaboration<ps::TermComputation> for TermComputation {
    type Error = TyckErrorItem;
    fn elab(comp: ps::TermComputation) -> Result<Self, TyckErrorItem> {
        Ok(match comp {
            ps::TermComputation::TermAnn(Annotation { term: body, ty }) => Annotation {
                term: rc!(body.try_map(Elaboration::elab)?),
                ty: rc!(ty.try_map(Elaboration::elab)?),
            }
            .into(),
            ps::TermComputation::Ret(Ret(body)) => {
                let span = body.span().clone();
                let body: TermComputation = Ret(rc!((body).try_map(Elaboration::elab)?)).into();
                Annotation {
                    term: rc!(span.make(body)),
                    ty: rc!(span.make(Type::make_ret(rc!(span.make(Hole.into()))))),
                }
                .into()
            }
            ps::TermComputation::Force(Force(body)) => {
                Force(rc!((body).try_map(Elaboration::elab)?)).into()
            }
            ps::TermComputation::Let(ps::Let {
                gen: ps::GenLet { rec, fun, name, params, def },
                body,
            }) => {
                let (var, ty, def) = desugar_gen_let(rec, fun, name, params, def)?;
                let Some(def) = def else {
                    Err(NameResolveError::EmptyDeclaration { name: var.name().to_string() })?
                };
                let mut def = def;
                let span = def.span().clone();
                def = rc!(span.make(ps::Annotation { term: def, ty }.into()));
                let body: Span<TermComputation> = body.try_map(Elaboration::elab)?;
                let item = Let { var, def, body: () }.into();
                if let TermComputation::TailGroup(TailGroup { mut group, body }) = body.inner {
                    group.push_front(item);
                    TailGroup { group, body }.into()
                } else {
                    TailGroup { group: vector![item], body: rc!(body) }.into()
                }
            }
            ps::TermComputation::Do(ps::Do { var: (var, ty), comp, body }) => {
                let var = TermV::from(var);
                let mut comp = rc!((comp).try_map(Elaboration::elab)?);
                if let Some(ty) = ty {
                    comp = rc!(comp.info.clone().make(
                        Annotation { term: comp, ty: rc!(ty.try_map(Elaboration::elab)?) }.into(),
                    ));
                }
                let body = body.try_map(Elaboration::elab)?;
                let item = Do { var, comp, body: () }.into();
                if let TermComputation::TailGroup(TailGroup { mut group, body }) = body.inner {
                    group.push_front(item);
                    TailGroup { group, body }.into()
                } else {
                    TailGroup { group: vector![item], body: rc!(body) }.into()
                }
            }
            ps::TermComputation::Rec(Rec { var: (var, ty), body }) => {
                let var = TermV::from(var);
                let body = rc!((body).try_map(Elaboration::elab)?);
                let span = body.span().clone();
                let mut body: TermComputation = Rec { var, body }.into();
                if let Some(ty) = ty {
                    let ty: Span<Type> = ty.try_map(Elaboration::elab)?;
                    let ty_ = ty.inner.clone();
                    let SynType::TypeApp(ty_app) = ty.inner.synty else {
                        Err(TyckErrorItem::TypeExpected {
                            context: format!("elaborating recursion"),
                            expected: format!("{{a}}"),
                            found: ty_
                        })?
                    };
                    let Some(ty) = ty_app.elim_thunk_syntax() else {
                        Err(TyckErrorItem::TypeExpected {
                            context: format!("elaborating recursion"),
                            expected: format!("{{a}}"),
                            found: ty_
                        })?
                    };
                    body = Annotation { term: rc!(span.make(body)), ty: rc!(span.make(ty)) }.into();
                }
                body
            }
            ps::TermComputation::Match(ps::Match { scrut, arms }) => {
                let scrut = rc!((scrut).try_map(Elaboration::elab)?);
                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        let ps::Matcher { ctorv, vars, body } = arm;
                        let vars = vars.into_iter().map(Into::into).collect();
                        let body = rc!((body).try_map(Elaboration::elab)?);
                        Ok(Matcher { ctorv, vars, body })
                    })
                    .collect::<Result<_, TyckErrorItem>>()?;
                Match { scrut, arms }.into()
            }
            ps::TermComputation::Abs(t) => desugar_fn(t)?,
            ps::TermComputation::App(ps::App { body, arg }) => {
                let fun = rc!((body).try_map(Elaboration::elab)?);
                let arg = arg.try_map(Elaboration::elab)?;
                Dtor {
                    body: fun,
                    dtorv: DtorV::new(format!("arg"), SpanInfo::dummy()),
                    args: vec![rc!(arg)],
                }
                .into()
            }
            ps::TermComputation::Comatch(ps::Comatch { arms }) => {
                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        let ps::Comatcher { dtorv, vars, body } = arm;
                        let vars = vars.into_iter().map(Into::into).collect();
                        let body = rc!((body).try_map(Elaboration::elab)?);
                        Ok(Comatcher { dtorv, vars, body })
                    })
                    .collect::<Result<Vec<_>, TyckErrorItem>>()?;
                Comatch { arms }.into()
            }
            ps::TermComputation::Dtor(ps::Dtor { body, dtorv, args }) => {
                let body = rc!((body).try_map(Elaboration::elab)?);
                let args = Vec::<_>::elab(args)?.into_iter().map(|arg| rc!(arg)).collect();
                Dtor { body, dtorv, args }.into()
            }
            ps::TermComputation::TyAppTerm(ps::App { body, arg }) => {
                let body = rc!((body).try_map(Elaboration::elab)?);
                let arg = rc!(arg.try_map(Elaboration::elab)?);
                TyAppTerm { body, arg }.into()
            }
            ps::TermComputation::MatchPack(ps::MatchPack { scrut, tvar, var, body }) => {
                let tvar = tvar.into();
                let var = var.into();
                let scrut = rc!((scrut).try_map(Elaboration::elab)?);
                let body = rc!((body).try_map(Elaboration::elab)?);
                MatchPack { scrut, tvar, var, body }.into()
            }
        })
    }
}

impl Elaboration<ps::Term> for Term {
    type Error = TyckErrorItem;
    fn elab(term: ps::Term) -> Result<Self, TyckErrorItem> {
        Ok(match term {
            ps::Term::Value(t) => Term::Value(Elaboration::elab(t)?),
            ps::Term::Computation(t) => Term::Computation(Elaboration::elab(t)?),
        })
    }
}

impl Elaboration<ps::Data<NameDef, Option<Span<ps::Kind>>, CtorV, Span<ps::Type>>>
    for prelude::Data
{
    type Error = TyckErrorItem;
    fn elab(
        Data { name, params, ctors }: ps::Data<
            NameDef,
            Option<Span<ps::Kind>>,
            CtorV,
            Span<ps::Type>,
        >,
    ) -> Result<Self, TyckErrorItem> {
        let params = params
            .into_iter()
            .map(|(tvar, kd)| {
                Ok((
                    tvar,
                    kd.ok_or_else(|| TyckErrorItem::NeedAnnotation {
                        content: format!("elaborating data type"),
                    })?,
                ))
            })
            .collect::<Result<_, TyckErrorItem>>()?;
        Ok(Self {
            name: name.into(),
            params: Elaboration::elab(params)?,
            ctors: Elaboration::elab(ctors)?,
        })
    }
}

impl Elaboration<ps::DataBr<CtorV, Span<ps::Type>>> for DataBr<CtorV, RcType> {
    type Error = TyckErrorItem;
    fn elab(
        DataBr(ctor, params): ps::DataBr<CtorV, Span<ps::Type>>,
    ) -> Result<Self, TyckErrorItem> {
        let params = Vec::<_>::elab(params)?.into_iter().map(|ty| rc!(ty)).collect();
        Ok(Self(ctor, params))
    }
}

impl Elaboration<ps::Codata<NameDef, Option<Span<ps::Kind>>, DtorV, Span<ps::Type>>>
    for prelude::Codata
{
    type Error = TyckErrorItem;
    fn elab(
        Codata { name, params, dtors }: ps::Codata<
            NameDef,
            Option<Span<ps::Kind>>,
            DtorV,
            Span<ps::Type>,
        >,
    ) -> Result<Self, TyckErrorItem> {
        let params = params
            .into_iter()
            .map(|(tvar, kd)| {
                Ok((
                    tvar,
                    kd.ok_or_else(|| TyckErrorItem::NeedAnnotation {
                        content: format!("elaborating codata type"),
                    })?,
                ))
            })
            .collect::<Result<_, TyckErrorItem>>()?;
        Ok(Self {
            name: name.into(),
            params: Elaboration::elab(params)?,
            dtors: Elaboration::elab(dtors)?,
        })
    }
}

impl Elaboration<ps::CodataBr<DtorV, Span<ps::Type>>> for CodataBr<DtorV, RcType> {
    type Error = TyckErrorItem;
    fn elab(
        CodataBr(dtor, params, ty): ps::CodataBr<DtorV, Span<ps::Type>>,
    ) -> Result<Self, TyckErrorItem> {
        let params = Vec::<_>::elab(params)?.into_iter().map(|ty| rc!(ty)).collect();
        Ok(Self(dtor, params, rc!(ty.try_map(Elaboration::elab)?)))
    }
}

impl Elaboration<ps::Alias<NameDef, Option<Span<ps::Kind>>, ps::BoxType>> for prelude::Alias {
    type Error = TyckErrorItem;
    fn elab(
        Alias { name, params, ty }: ps::Alias<NameDef, Option<Span<ps::Kind>>, ps::BoxType>,
    ) -> Result<Self, TyckErrorItem> {
        let params = params
            .into_iter()
            .map(|(tvar, kd)| {
                Ok((
                    tvar,
                    kd.ok_or_else(|| TyckErrorItem::NeedAnnotation {
                        content: format!("elaborating alias type"),
                    })?,
                ))
            })
            .collect::<Result<_, TyckErrorItem>>()?;
        Ok(Self {
            name: name.into(),
            params: Elaboration::elab(params)?,
            ty: rc!(ty.try_map(Elaboration::elab)?),
        })
    }
}

impl Elaboration<ps::Module> for Module {
    type Error = TyckErrorItem;
    fn elab(ps::Module { name, declarations }: ps::Module) -> Result<Self, TyckErrorItem> {
        let mut module: Module = Elaboration::elab(ps::TopLevel { declarations })?;
        module.name = name.map(|name| name.ident.inner);
        Ok(module)
    }
}

impl Elaboration<ps::TopLevel> for Module {
    type Error = TyckErrorItem;
    fn elab(ps::TopLevel { declarations }: ps::TopLevel) -> Result<Self, TyckErrorItem> {
        let mut data = Vec::new();
        let mut codata = Vec::new();
        let mut alias = Vec::new();
        let mut define = Vec::new();
        let mut define_ext = Vec::new();
        for declaration in declarations {
            let DeclSymbol { public, external, inner } = declaration;
            match inner {
                ps::Declaration::Module(m) => {
                    let Module {
                        name: _,
                        data: ds,
                        codata: cs,
                        alias: aliases,
                        define: defs,
                        define_ext: defexts,
                    } = Elaboration::elab(m)?;
                    data.extend(ds);
                    codata.extend(cs);
                    alias.extend(aliases);
                    define.extend(defs);
                    define_ext.extend(defexts);
                }
                ps::Declaration::UseDef(_d) => {}
                ps::Declaration::Data(d) => {
                    data.push(DeclSymbol { public, external, inner: Elaboration::elab(d)? })
                }
                ps::Declaration::Codata(d) => {
                    codata.push(DeclSymbol { public, external, inner: Elaboration::elab(d)? })
                }
                ps::Declaration::Alias(d) => {
                    alias.push(DeclSymbol { public, external, inner: Elaboration::elab(d)? })
                }
                ps::Declaration::Define(d) => {
                    let ps::Define(ps::GenLet { rec, fun, name, params, def }) = d;
                    let (name, ty, te) = desugar_gen_let(rec, fun, name, params, def)?;
                    if external {
                        define_ext.push(DeclSymbol {
                            public,
                            external,
                            inner: Define { name: (name, ty), def: () },
                        })
                    } else {
                        let term = te.ok_or_else(|| NameResolveError::EmptyDeclaration {
                            name: name.name().to_string(),
                        })?;
                        let span = term.span().clone();
                        let def = rc!(span.make(Annotation { term, ty }.into()));
                        define.push(DeclSymbol { public, external, inner: Define { name, def } })
                    }
                }
                ps::Declaration::Main(ps::Main { entry: _ }) => {
                    Err(TyckErrorItem::MainEntryInModule)?
                }
            }
        }
        Ok(Self { name: None, data, codata, alias, define, define_ext })
    }
}

impl Elaboration<ps::TopLevel> for Program {
    type Error = TyckErrorItem;

    fn elab(value: ps::TopLevel) -> Result<Self, Self::Error> {
        let ps::TopLevel { declarations } = value;
        let mut non_main = Vec::new();
        let mut main_entry = None;
        for decl in declarations {
            match decl.inner {
                ps::Declaration::Main(ps::Main { entry }) => {
                    if main_entry.is_some() {
                        Err(TyckErrorItem::MultipleMainEntries)?
                    }
                    main_entry = Some(entry)
                }
                _ => non_main.push(decl),
            }
        }
        let Some(entry) = main_entry else { Err(TyckErrorItem::NoMainEntry)? };
        Ok(Self {
            module: Elaboration::elab(
                SpanInfo::dummy().make(ps::TopLevel { declarations: non_main }),
            )?,
            entry: Elaboration::elab(entry)?,
        })
    }
}
