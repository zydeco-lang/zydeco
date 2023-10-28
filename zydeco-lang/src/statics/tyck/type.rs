use super::*;

impl Type {
    pub fn internal(name: &'static str, args: Vec<RcType>) -> Self {
        TypeApp { tvar: TypeV::new(name.into(), Span::dummy()).into(), args }.into()
    }
    pub fn resolve(&self) -> Result<SynType, TyckError> {
        Ok(self.synty.clone())
    }
    pub fn make_thunk(arg: RcType) -> Self {
        Type::internal("Thunk", vec![arg])
    }
    pub fn elim_thunk(self, ctx: Ctx, span: &Span) -> Option<Type> {
        let ty = self.lub(Type::make_thunk(rc!(span.make(Hole.into()))), ctx, span).ok()?;
        let SynType::TypeApp(ty_app) = ty.synty else { None? };
        ty_app.elim_thunk_syntax()
    }
    pub fn make_ret(arg: RcType) -> Self {
        Type::internal("Ret", vec![arg])
    }
    pub fn elim_ret(self, ctx: Ctx, span: &Span) -> Option<Type> {
        let ty = self.lub(Type::make_ret(rc!(span.make(Hole.into()))), ctx, span).ok()?;
        let SynType::TypeApp(ty_app) = ty.synty else { None? };
        ty_app.elim_ret_syntax()
    }
    pub fn make_os() -> Self {
        Type::internal("OS", vec![])
    }
    pub fn elim_os(self, ctx: Ctx, span: &Span) -> Option<()> {
        self.lub(Type::make_os(), ctx, span).map(|_| ()).ok()
    }
}
impl TypeApp<NeutralVar, RcType> {
    pub fn elim_thunk_syntax(&self) -> Option<Type> {
        match &self.tvar {
            NeutralVar::Var(tvar) if tvar.name() == "Thunk" => {
                Some(self.args.first().unwrap().inner_clone())
            }
            _ => None,
        }
    }
    pub fn elim_ret_syntax(&self) -> Option<Type> {
        match &self.tvar {
            NeutralVar::Var(tvar) if tvar.name() == "Ret" => {
                Some(self.args.first().unwrap().inner_clone())
            }
            _ => None,
        }
    }
    pub fn elim_os_syntax(&self) -> Option<()> {
        match &self.tvar {
            NeutralVar::Var(tvar) if tvar.name() == "OS" => Some(()),
            _ => None,
        }
    }
}

impl Ctx {
    pub(super) fn resolve_data(
        &self, ty: Type, span: &Span,
    ) -> Result<(prelude::Data, Vec<RcType>), TyckError> {
        let ty = self.resolve_alias(ty, span)?;
        let ty_syn = ty.resolve()?;
        let SynType::TypeApp(TypeApp { tvar: NeutralVar::Var(tvar), args }) = ty_syn else {
            Err(self.err(
                span,
                TypeExpected {
                    context: format!("data resolution"),
                    expected: format!("type application"),
                    found: ty,
                },
            ))?
        };
        let data =
            self.data_env.get(&tvar).cloned().ok_or_else(|| {
                self.err(span, NameResolveError::UnboundTypeVariable { tvar }.into())
            })?;
        Ok((data, args))
    }
    pub(super) fn resolve_codata(
        &self, ty: Type, span: &Span,
    ) -> Result<(prelude::Codata, Vec<RcType>), TyckError> {
        let ty = self.resolve_alias(ty, span)?;
        let ty_syn = ty.resolve()?;
        let SynType::TypeApp(TypeApp { tvar: NeutralVar::Var(tvar), args }) = ty_syn else {
            Err(self.err(
                span,
                TypeExpected {
                    context: format!("codata resolution"),
                    expected: format!("type application"),
                    found: ty,
                },
            ))?
        };
        let codata =
            self.codata_env.get(&tvar).cloned().ok_or_else(|| {
                self.err(span, NameResolveError::UnboundTypeVariable { tvar }.into())
            })?;
        Ok((codata, args))
    }
    pub(super) fn resolve_alias(&self, mut typ: Type, span: &Span) -> Result<Type, TyckError> {
        while let SynType::TypeApp(TypeApp { tvar: NeutralVar::Var(ref tvar), ref args }) =
            typ.resolve()?
        {
            if let Some(Alias { name, params, ty }) = self.alias_env.get(tvar) {
                let ty = ty.inner_clone();
                let diff = Env::init(params, args, || {
                    self.err(
                        span,
                        ArityMismatch {
                            context: format!("alias `{}` instiantiation", name),
                            expected: params.len(),
                            found: args.len(),
                        },
                    )
                })?;
                typ = ty.subst(diff, self)?;
            } else {
                break;
            }
        }
        Ok(typ)
    }
}

impl TypeCheck for Sp<Type> {
    type Ctx = Ctx;
    type Out = Kind;
    fn syn_step(
        &self, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, TyckError> {
        ctx.trace.push(Frame {
            blame: format!("{}", std::panic::Location::caller()),
            context: "synthesizing type".to_owned(),
            term: format!("{}", self.inner_ref().fmt()),
            info: self.span().clone(),
        });
        let span = self.span();
        let ty = self.inner_clone().subst(ctx.type_env.clone(), &ctx)?;
        let ty = ctx.resolve_alias(ty, span)?;
        let ty_syn = ty.resolve()?;
        match ty_syn {
            SynType::TypeAbs(TypeAbs { params, body }) => {
                let mut kd_params = Vec::new();
                for (tv, kd) in params.iter() {
                    ctx.type_ctx.insert(tv.clone(), kd.inner_clone());
                    kd_params.push(kd.clone());
                }
                let kd: BoxKind = Box::new(body.span().make(body.syn(ctx)?));
                // Hack: merge arity to support currying
                Ok(Step::Done(TypeArity { params: kd_params, kd }.into()))
            }
            SynType::TypeApp(TypeApp { tvar, args }) => {
                // type constructor
                let kd = {
                    match tvar {
                        NeutralVar::Var(tvar) => {
                            let Some(kd) = ctx.type_ctx.get(&tvar) else {
                                Err(ctx.err(
                                    span,
                                    NameResolveError::UnboundTypeVariable { tvar: tvar.to_owned() }
                                        .into(),
                                ))?
                            };
                            kd
                        }
                        NeutralVar::Abst(AbstVar(abs)) => {
                            let Some(kd) = ctx.abst_ctx.get(abs) else { unreachable!() };
                            kd
                        }
                    }
                };
                match kd {
                    Kind::Base(kd) => return Ok(Step::Done(kd.clone().into())),
                    Kind::TypeArity(TypeArity { params, kd }) => {
                        bool_test(args.len() <= params.len(), || {
                            ctx.err(
                                span,
                                ArityMismatch {
                                    context: format!("`{}`", self.inner_ref().fmt()),
                                    expected: params.len(),
                                    found: args.len(),
                                },
                            )
                        })?;
                        for (arg, kd) in args.iter().zip(params.iter()) {
                            arg.ana(kd.inner_clone(), ctx.clone())?;
                        }
                        let (_, remainder) = params.split_at(args.len());
                        if remainder.is_empty() {
                            Ok(Step::Done(kd.inner_clone()))
                        } else {
                            Ok(Step::Done(
                                TypeArity { params: remainder.to_vec(), kd: kd.clone() }.into(),
                            ))
                        }
                    }
                }
            }
            SynType::Forall(Forall { param: (param, kd), ty }) => {
                ctx.type_ctx.insert(param, kd.inner_clone());
                ty.ana(KindBase::CType.into(), ctx)?;
                Ok(Step::Done(KindBase::CType.into()))
            }
            SynType::Exists(Exists { param: (param, kd), ty }) => {
                ctx.type_ctx.insert(param, kd.inner_clone());
                ty.ana(KindBase::VType.into(), ctx)?;
                Ok(Step::Done(KindBase::VType.into()))
            }
            SynType::AbstVar(AbstVar(abs)) => Ok(Step::Done(ctx.abst_ctx[abs].clone())),
            SynType::Hole(_) => Err(ctx.err(span, NeedAnnotation { content: format!("hole") }))?,
        }
    }
    fn ana_step(
        &self, kd: Self::Out, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, TyckError> {
        ctx.trace.push(Frame {
            blame: format!("{}", std::panic::Location::caller()),
            context: format!("analyzing type with kind {}", kd.fmt()),
            term: format!("{}", self.inner_ref().fmt()),
            info: self.span().clone(),
        });
        let span = self.span();
        let ty = self.inner_clone().subst(ctx.type_env.clone(), &ctx)?;
        let ty = ctx.resolve_alias(ty, span)?;
        let ty_syn = ty.resolve()?;
        match ty_syn {
            SynType::Hole(_) => Ok(Step::Done(kd)),
            SynType::TypeAbs(_)
            | SynType::TypeApp(_)
            | SynType::Forall(_)
            | SynType::Exists(_)
            | SynType::AbstVar(_) => {
                let kd_syn = self.syn(ctx.clone())?;
                let kd = kd_syn.lub(kd, ctx, span)?;
                Ok(Step::Done(kd))
            }
        }
    }
}

impl Type {
    pub(super) fn subst(self, mut diff: Env<TypeV, Type>, ctx: &Ctx) -> Result<Self, TyckError> {
        let typ = ctx.resolve_alias(self, &Span::dummy())?;
        let typ_syn = typ.resolve()?;
        match typ_syn {
            SynType::TypeAbs(TypeAbs { params, body }) => {
                for (tv, _) in params.iter() {
                    diff.remove(tv);
                }
                Ok(Type {
                    synty: TypeAbs {
                        params,
                        body: body.try_map_rc(|ty| ty.clone().subst(diff.clone(), ctx))?,
                    }
                    .into(),
                })
            }
            SynType::TypeApp(TypeApp { tvar, mut args }) => {
                for arg in args.iter_mut() {
                    *arg = arg.try_map_rc(|ty| ty.clone().subst(diff.clone(), ctx))?;
                }
                match tvar {
                    NeutralVar::Var(tvar) => {
                        if let Some(ty) = diff.get(&tvar) {
                            ty.clone().apply(args, ctx)
                        } else {
                            Ok(Type { synty: TypeApp { tvar: NeutralVar::Var(tvar), args }.into() })
                        }
                    }
                    NeutralVar::Abst(abst_var) => Ok(Type {
                        synty: TypeApp { tvar: NeutralVar::Abst(abst_var), args }.into(),
                    }),
                }
            }
            SynType::Forall(Forall { param, ty }) => {
                diff.remove(&param.0);
                Ok(Type {
                    synty: Forall {
                        param,
                        ty: ty.try_map_rc(|ty| ty.clone().subst(diff.clone(), ctx))?,
                    }
                    .into(),
                })
            }
            SynType::Exists(Exists { param, ty }) => {
                diff.remove(&param.0);
                Ok(Type {
                    synty: Exists {
                        param,
                        ty: ty.try_map_rc(|ty| ty.clone().subst(diff.clone(), ctx))?,
                    }
                    .into(),
                })
            }
            SynType::AbstVar(_) | SynType::Hole(_) => Ok(typ),
        }
    }
    pub(super) fn apply(self, args: Vec<RcType>, ctx: &Ctx) -> Result<Self, TyckError> {
        let typ = ctx.resolve_alias(self, &Span::dummy())?;
        let typ_syn = typ.resolve()?;
        match typ_syn {
            SynType::TypeAbs(TypeAbs { params, body }) => {
                // Hack: need to support curried type application
                bool_test(args.len() == params.len(), || {
                    ctx.err(
                        body.span(),
                        ArityMismatch {
                            context: format!("curried type application"),
                            expected: params.len(),
                            found: args.len(),
                        },
                    )
                })?;
                let diff = Env::init(&params, &args, || unreachable!())?;
                body.inner_clone().subst(diff, ctx)
            }
            SynType::TypeApp(TypeApp { tvar, args: mut old_args }) => {
                old_args.extend(args);
                Ok(Type { synty: TypeApp { tvar, args: old_args }.into() })
            }
            SynType::AbstVar(abst_var) => {
                Ok(Type { synty: TypeApp { tvar: NeutralVar::Abst(abst_var), args }.into() })
            }
            SynType::Forall(_) | SynType::Exists(_) | SynType::Hole(_) => {
                if args.is_empty() {
                    Ok(typ)
                } else {
                    Err(ctx.err(&Span::dummy(), ApplyToNonTypeAbs { found: typ }))?
                }
            }
        }
    }
}

impl Env<TypeV, Type> {
    pub(super) fn init(
        params: &[(TypeV, Sp<Kind>)], ty_app_args: &[RcType], arity_err: impl FnOnce() -> TyckError,
    ) -> Result<Self, TyckError> {
        bool_test(params.len() == ty_app_args.len(), arity_err)?;
        Ok(Env::from_iter(
            params
                .iter()
                .map(|(tvar, _)| tvar.to_owned())
                .zip(ty_app_args.iter().map(|arg| arg.inner_clone())),
        ))
    }
}
