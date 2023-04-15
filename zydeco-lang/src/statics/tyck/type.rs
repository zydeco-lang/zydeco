use super::*;

impl Type {
    pub fn internal(name: &'static str, args: Vec<RcType>) -> Self {
        TypeApp { tvar: TypeV::new(name.into(), SpanInfo::dummy()), args }.into()
    }
    pub fn resolve(&self) -> Result<SynType, TyckError> {
        Ok(self.synty.clone())
    }
    pub fn make_thunk(arg: RcType) -> Self {
        Type::internal("Thunk", vec![arg])
    }
    pub fn elim_thunk(self, ctx: Ctx, span: &SpanInfo) -> Option<Type> {
        let ty = self.lub(Type::make_thunk(rc!(span.make(Hole.into()))), ctx, span).ok()?;
        let SynType::TypeApp(ty_app) = ty.synty else {
            None?
        };
        ty_app.elim_thunk_syntax()
    }
    pub fn make_ret(arg: RcType) -> Self {
        Type::internal("Ret", vec![arg])
    }
    pub fn elim_ret(self, ctx: Ctx, span: &SpanInfo) -> Option<Type> {
        let ty = self.lub(Type::make_ret(rc!(span.make(Hole.into()))), ctx, span).ok()?;
        let SynType::TypeApp(ty_app) = ty.synty else {
            None?
        };
        ty_app.elim_ret_syntax()
    }
    pub fn make_os() -> Self {
        Type::internal("OS", vec![])
    }
    pub fn elim_os(self, ctx: Ctx, span: &SpanInfo) -> Option<()> {
        self.lub(Type::make_os(), ctx, span).map(|_| ()).ok()
    }
}
impl TypeApp<TypeV, RcType> {
    pub fn elim_thunk_syntax(&self) -> Option<Type> {
        if self.tvar.name() == "Thunk" {
            Some(self.args.first().unwrap().inner_clone())
        } else {
            None
        }
    }
    pub fn elim_ret_syntax(&self) -> Option<Type> {
        if self.tvar.name() == "Ret" {
            Some(self.args.first().unwrap().inner_clone())
        } else {
            None
        }
    }
    pub fn elim_os_syntax(&self) -> Option<()> {
        if self.tvar.name() == "OS" {
            Some(())
        } else {
            None
        }
    }
}

impl Ctx {
    pub(super) fn resolve_data(
        &self, ty: Type, span: &SpanInfo,
    ) -> Result<(prelude::Data, Vec<RcType>), TyckError> {
        let ty = self.resolve_alias(ty, span)?;
        let ty_syn = ty.resolve()?;
        let SynType::TypeApp(TypeApp { tvar, args }) = ty_syn else {
            Err(self.err(span, TypeExpected {
                context: format!("resolve data"),
                expected: format!("type application"),
                found: ty,
            }))?
        };
        let data =
            self.data_env.get(&tvar).cloned().ok_or_else(|| {
                self.err(span, NameResolveError::UnboundTypeVariable { tvar }.into())
            })?;
        Ok((data, args))
    }
    pub(super) fn resolve_codata(
        &self, ty: Type, span: &SpanInfo,
    ) -> Result<(prelude::Codata, Vec<RcType>), TyckError> {
        let ty = self.resolve_alias(ty, span)?;
        let ty_syn = ty.resolve()?;
        let SynType::TypeApp(TypeApp { tvar, args }) = ty_syn else {
            Err(self.err(span, TypeExpected {
                context: format!("resolve codata"),
                expected: format!("type application"),
                found: ty,
            }))?
        };
        let codata =
            self.codata_env.get(&tvar).cloned().ok_or_else(|| {
                self.err(span, NameResolveError::UnboundTypeVariable { tvar }.into())
            })?;
        Ok((codata, args))
    }
    pub(super) fn resolve_alias(&self, mut typ: Type, span: &SpanInfo) -> Result<Type, TyckError> {
        while let SynType::TypeApp(TypeApp { ref tvar, ref args }) = typ.resolve()? {
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

impl TypeCheck for Span<Type> {
    type Ctx = Ctx;
    type Out = Kind;
    fn syn_step(
        &self, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, TyckError> {
        ctx.trace.push(Frame {
            tycker_src: format!("{}:{}:{}", file!(), line!(), column!()),
            sort: "syn type".to_owned(),
            term: format!("{}", self.inner_ref().fmt()),
            info: self.span().clone(),
        });
        let span = self.span();
        let ty = self.inner_clone().subst(ctx.type_env.clone(), &ctx)?;
        let ty = ctx.resolve_alias(ty, span)?;
        let ty_syn = ty.resolve()?;
        match ty_syn {
            SynType::TypeApp(app) => {
                let tvar = &app.tvar;
                // type constructor
                let Some(kd) = ctx.type_ctx.get(&tvar) else {
                    Err(ctx.err(span,
                        NameResolveError::UnboundTypeVariable {
                            tvar: tvar.to_owned(),
                        }.into()
                    ))?
                };
                let (params, kd) = match kd {
                    Kind::TypeArity(TypeArity { params, kd }) => (params.clone(), kd.inner_clone()),
                    Kind::Base(kd) => (vec![], kd.clone().into()),
                };
                bool_test(app.args.len() == params.len(), || {
                    ctx.err(
                        span,
                        ArityMismatch {
                            context: format!("{}", self.inner_ref().fmt()),
                            expected: params.len(),
                            found: app.args.len(),
                        },
                    )
                })?;
                for (arg, kd) in app.args.iter().zip(params.iter()) {
                    arg.ana(kd.inner_clone(), ctx.clone())?;
                }
                Ok(Step::Done(kd))
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
            tycker_src: format!("{}:{}:{}", file!(), line!(), column!()),
            sort: format!("ana type with kind {}", kd.fmt()),
            term: format!("{}", self.inner_ref().fmt()),
            info: self.span().clone(),
        });
        let span = self.span();
        let ty = self.inner_clone().subst(ctx.type_env.clone(), &ctx)?;
        let ty = ctx.resolve_alias(ty, span)?;
        let ty_syn = ty.resolve()?;
        match ty_syn {
            SynType::Hole(_) => Ok(Step::Done(kd)),
            SynType::TypeApp(_) | SynType::Forall(_) | SynType::Exists(_) | SynType::AbstVar(_) => {
                let kd_syn = self.syn(ctx.clone())?;
                let kd = kd_syn.lub(kd, ctx, span)?;
                Ok(Step::Done(kd))
            }
        }
    }
}

impl Type {
    pub(super) fn subst(self, mut diff: Env<TypeV, Type>, ctx: &Ctx) -> Result<Self, TyckError> {
        let typ = ctx.resolve_alias(self, &SpanInfo::dummy())?;
        let typ_syn = typ.resolve()?;
        match typ_syn {
            SynType::TypeApp(TypeApp { tvar, mut args }) => {
                if let Some(ty) = diff.get(&tvar) {
                    bool_test(args.is_empty(), || {
                        ctx.err(
                            tvar.span(),
                            ArityMismatch {
                                context: format!("type variable `{}`", tvar),
                                expected: 0,
                                found: args.len(),
                            },
                        )
                    })?;
                    Ok(ty.clone())
                } else {
                    for arg in args.iter_mut() {
                        *arg = arg.try_map_rc(|ty| ty.clone().subst(diff.clone(), ctx))?;
                    }
                    Ok(Type { synty: TypeApp { tvar, args }.into() })
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
}

impl Env<TypeV, Type> {
    pub(super) fn init(
        params: &[(TypeV, Span<Kind>)], ty_app_args: &[RcType],
        arity_err: impl FnOnce() -> TyckError,
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
