use super::{err::TyckErrorItem, syntax::*};
use crate::{parse::syntax as ps, prelude::*, resolve::err::NameResolveError};
use im::vector;

pub trait Elaboration<T>: Sized {
    type Error;
    fn elab(value: T) -> Result<Self, Self::Error>;
}

impl<T, S> Elaboration<Sp<T>> for Sp<S>
where
    S: Elaboration<T>,
{
    type Error = S::Error;
    fn elab(value: Sp<T>) -> Result<Self, Self::Error> {
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
                let kd: Sp<Kind> = Elaboration::elab(*kd)?;
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
    rec: bool, fun: bool, (var, ty): (NameDef, Option<Sp<ps::Type>>), params: Vec<ps::Pattern>,
    def: Option<Box<Sp<ps::Term>>>,
) -> Result<(TermV, RcType, Option<RcValue>), TyckErrorItem> {
    let name = var.clone().into();
    let ty_rc = {
        if let Some(ty) = ty.clone() {
            ty.try_map_rc(Elaboration::elab)?
        } else {
            var.span().make_rc(Hole.into())
        }
    };
    let Some(def) = def else {
        return Ok((name, ty_rc, None));
    };
    match (rec, fun, def.inner) {
        (false, false, ps::Term::Value(value)) => {
            Ok((name, ty_rc, Some(def.info.make_rc(Elaboration::elab(value)?))))
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
                ty.try_map_rc(Elaboration::elab)?
            } else {
                def.info.make_rc(Hole.into())
            };
            for param in params.iter().rev() {
                match param {
                    ps::Pattern::TypePattern((tvar, kd_param)) => {
                        let Some(kd_dom) = kd_param else {
                            Err(TyckErrorItem::NeedAnnotation {
                                content: format!("gen let type variable elabrotion"),
                            })?
                        };
                        let kd_dom = kd_dom.clone().try_map(Elaboration::elab)?;
                        ty = tvar
                            .span()
                            .make_rc(Forall { param: (tvar.into(), kd_dom.clone()), ty }.into())
                    }
                    ps::Pattern::TermPattern((var, ty_param)) => {
                        let ty_dom = if let Some(ty_dom) = ty_param {
                            ty_dom.to_owned().try_map_rc(Elaboration::elab)?
                        } else {
                            def.info.make_rc(Hole.into())
                        };
                        ty = var.span().make_rc(Arrow(ty_dom, ty).into())
                    }
                }
            }
            let body = (*body).try_map_rc(Elaboration::elab)?;
            ty = def.info.make_rc(Type::make_thunk(ty));
            Ok((name, ty, Some(def.info.make_rc(Thunk(body).into()))))
        }
    }
}

fn desugar_fn(
    ps::Abs { param, body }: ps::Abs<Vec<ps::Pattern>, ps::BoxComp>,
) -> Result<TermComputation, TyckErrorItem> {
    fn desugar_fn_one(
        (param, ty): (TermV, Option<Sp<ps::Type>>), body: RcComp,
    ) -> Result<TermComputation, TyckErrorItem> {
        let mut body = Abs { param, body }.into();
        if let Some(ty) = ty {
            let mut ty = ty.try_map_rc(Elaboration::elab)?;
            let span = ty.span().clone();
            ty = span.make_rc(Arrow(ty, span.make_rc(Hole.into())).into());
            body = Annotation { term: span.make_rc(body), ty }.into();
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
                func = Abs { param: (tvar.into(), kd), body: body.info.make_rc(func) }.into()
            }
            ps::Pattern::TermPattern((var, ty)) => {
                func = desugar_fn_one(
                    (TermV::new(var.ident.inner, var.info), ty),
                    body.info.make_rc(func),
                )?;
            }
        }
    }
    Ok(func)
}

impl Elaboration<ps::Type> for Type {
    type Error = TyckErrorItem;
    fn elab(ty: ps::Type) -> Result<Self, TyckErrorItem> {
        Ok(match ty {
            ps::Type::Basic(tvar) => {
                TypeApp { tvar: TypeV::from(tvar).into(), args: vec![] }.into()
            }
            ps::Type::App(t) => {
                let ps::TypeApp(t1, t2) = t;
                let t1: Type = Elaboration::elab(t1.inner())?;
                let t2 = t2.try_map_rc(Elaboration::elab)?;
                let SynType::TypeApp(mut t1) = t1.synty else {
                    Err(TyckErrorItem::KindMismatch {
                        context: format!("desugaring type application"),
                        expected: KindBase::CType.into(),
                        found: KindBase::VType.into(),
                    })?
                };
                t1.args.push(t2);
                t1.into()
            }
            ps::Type::Arrow(t) => {
                let ps::Arrow(t1, t2) = t;
                let t1 = t1.try_map_rc(Elaboration::elab)?;
                let t2 = t2.try_map_rc(Elaboration::elab)?;
                Arrow(t1, t2).into()
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
            ps::Type::TypeAbs(ps::TypeAbs { params: params_, body: t }) => {
                let mut params = vec![];
                for (tvar, kd) in params_ {
                    let kd = kd.ok_or_else(|| TyckErrorItem::NeedAnnotation {
                        content: format!("type abstraction elabrotion"),
                    })?;
                    params.push(Elaboration::elab((tvar, kd))?);
                }
                let t = t.try_map_rc(Elaboration::elab)?;
                TypeAbs { params, body: t }.into()
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
                term: body.try_map_rc(Elaboration::elab)?,
                ty: ty.try_map_rc(Elaboration::elab)?,
            }
            .into(),
            ps::TermValue::Var(x) => TermV::from(x).into(),
            ps::TermValue::Thunk(Thunk(body)) => {
                let span = body.span().clone();
                let body = Thunk(body.try_map_rc(Elaboration::elab)?).into();
                Annotation {
                    term: span.make_rc(body),
                    ty: span.make_rc(Type::make_thunk(span.make_rc(Hole.into()))),
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
                let ty = ty.try_map_rc(Elaboration::elab)?;
                let body = body.try_map_rc(Elaboration::elab)?;
                Pack { ty, body }.into()
            }
        })
    }
}

impl Elaboration<ps::TermComputation> for TermComputation {
    type Error = TyckErrorItem;
    fn elab(comp: ps::TermComputation) -> Result<Self, TyckErrorItem> {
        Ok(match comp {
            ps::TermComputation::TermAnn(Annotation { term: body, ty }) => Annotation {
                term: body.try_map_rc(Elaboration::elab)?,
                ty: ty.try_map_rc(Elaboration::elab)?,
            }
            .into(),
            ps::TermComputation::Abs(t) => desugar_fn(t)?,
            ps::TermComputation::App(ps::App { body, arg }) => {
                let fun: RcComp = body.try_map_rc(Elaboration::elab)?;
                let arg: RcValue = arg.try_map_rc(Elaboration::elab)?;
                App { body: fun, arg }.into()
            }
            ps::TermComputation::Ret(Ret(body)) => {
                let span = body.span().clone();
                let body: TermComputation = Ret(body.try_map_rc(Elaboration::elab)?).into();
                Annotation {
                    term: span.make_rc(body),
                    ty: span.make_rc(Type::make_ret(span.make_rc(Hole.into()))),
                }
                .into()
            }
            ps::TermComputation::Force(Force(body)) => {
                Force(body.try_map_rc(Elaboration::elab)?).into()
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
                def = span.make_rc(ps::Annotation { term: def, ty }.into());
                let body: Sp<TermComputation> = body.try_map(Elaboration::elab)?;
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
                let span = comp.span().clone();
                let mut comp = comp.try_map_rc(Elaboration::elab)?;
                if let Some(ty) = ty {
                    comp = span.make_rc(
                        Annotation { term: comp, ty: ty.try_map_rc(Elaboration::elab)? }.into(),
                    );
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
                let body = body.try_map_rc(Elaboration::elab)?;
                let span = body.span().clone();
                let mut body: TermComputation = Rec { var, body }.into();
                if let Some(ty) = ty {
                    let ty: Sp<Type> = ty.try_map(Elaboration::elab)?;
                    let ty_ = ty.inner.clone();
                    let SynType::TypeApp(ty_app) = ty.inner.synty else {
                        Err(TyckErrorItem::TypeExpected {
                            context: format!("elaborating recursion"),
                            expected: format!("{{a}}"),
                            found: ty_,
                        })?
                    };
                    let Some(ty) = ty_app.elim_thunk_syntax() else {
                        Err(TyckErrorItem::TypeExpected {
                            context: format!("elaborating recursion"),
                            expected: format!("{{a}}"),
                            found: ty_,
                        })?
                    };
                    body = Annotation { term: span.make_rc(body), ty: span.make_rc(ty) }.into();
                }
                body
            }
            ps::TermComputation::Match(ps::Match { scrut, arms }) => {
                let scrut = (scrut).try_map_rc(Elaboration::elab)?;
                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        let ps::Matcher { ctorv, vars, body } = arm;
                        let vars = vars.into_iter().map(Into::into).collect();
                        let body = body.try_map_rc(Elaboration::elab)?;
                        Ok(Matcher { ctorv, vars, body })
                    })
                    .collect::<Result<_, TyckErrorItem>>()?;
                Match { scrut, arms }.into()
            }
            ps::TermComputation::Comatch(ps::Comatch { arms }) => {
                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        let ps::Comatcher { dtorv, body } = arm;
                        let body = body.try_map_rc(Elaboration::elab)?;
                        Ok(Comatcher { dtorv, body })
                    })
                    .collect::<Result<Vec<_>, TyckErrorItem>>()?;
                Comatch { arms }.into()
            }
            ps::TermComputation::Dtor(ps::Dtor { body, dtorv }) => {
                let body = body.try_map_rc(Elaboration::elab)?;
                Dtor { body, dtorv }.into()
            }
            ps::TermComputation::TyAppTerm(ps::App { body, arg }) => {
                let body = body.try_map_rc(Elaboration::elab)?;
                let arg: RcType = arg.try_map_rc(Elaboration::elab)?;
                App { body, arg }.into()
            }
            ps::TermComputation::MatchPack(ps::MatchPack { scrut, tvar, var, body }) => {
                let tvar = tvar.into();
                let var = var.into();
                let scrut = (scrut).try_map_rc(Elaboration::elab)?;
                let body = body.try_map_rc(Elaboration::elab)?;
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

impl Elaboration<ps::Data<NameDef, Option<Sp<ps::Kind>>, CtorV, Sp<ps::Type>>> for prelude::Data {
    type Error = TyckErrorItem;
    fn elab(
        Data { name, params, ctors }: ps::Data<NameDef, Option<Sp<ps::Kind>>, CtorV, Sp<ps::Type>>,
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

impl Elaboration<ps::DataBr<CtorV, Sp<ps::Type>>> for DataBr<CtorV, RcType> {
    type Error = TyckErrorItem;
    fn elab(DataBr { ctorv, tys }: ps::DataBr<CtorV, Sp<ps::Type>>) -> Result<Self, TyckErrorItem> {
        let tys = Vec::<_>::elab(tys)?.into_iter().map(|ty| rc!(ty)).collect();
        Ok(Self { ctorv, tys })
    }
}

impl Elaboration<ps::Codata<NameDef, Option<Sp<ps::Kind>>, DtorV, Sp<ps::Type>>>
    for prelude::Codata
{
    type Error = TyckErrorItem;
    fn elab(
        Codata { name, params, dtors }: ps::Codata<
            NameDef,
            Option<Sp<ps::Kind>>,
            DtorV,
            Sp<ps::Type>,
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

impl Elaboration<ps::CodataBr<DtorV, Sp<ps::Type>>> for CodataBr<DtorV, RcType> {
    type Error = TyckErrorItem;
    fn elab(
        CodataBr { dtorv, ty }: ps::CodataBr<DtorV, Sp<ps::Type>>,
    ) -> Result<Self, TyckErrorItem> {
        Ok(Self { dtorv, ty: ty.try_map_rc(Elaboration::elab)? })
    }
}

impl Elaboration<ps::Alias<NameDef, Option<Sp<ps::Kind>>, ps::BoxType>> for prelude::Alias {
    type Error = TyckErrorItem;
    fn elab(
        Alias { name, params, ty }: ps::Alias<NameDef, Option<Sp<ps::Kind>>, ps::BoxType>,
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
            ty: ty.try_map_rc(Elaboration::elab)?,
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
            let DeclSymbol { public, external, inner } = declaration.inner;
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
                        let def = span.make_rc(Annotation { term, ty }.into());
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
            match decl.inner.inner {
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
            module: Elaboration::elab(Span::dummy().make(ps::TopLevel { declarations: non_main }))?,
            entry: Elaboration::elab(entry)?,
        })
    }
}
