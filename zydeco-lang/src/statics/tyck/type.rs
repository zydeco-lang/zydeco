use super::*;
use crate::utils::span::span;

impl Type {
    pub fn internal(name: &'static str, args: Vec<RcType>) -> Self {
        TypeApp { tvar: TypeV::new(name.into(), span(0, 0)), args }.into()
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
            Some(self.args.first().unwrap().inner_ref().clone())
        } else {
            None
        }
    }
    pub fn elim_ret_syntax(&self) -> Option<Type> {
        if self.tvar.name() == "Ret" {
            Some(self.args.first().unwrap().inner_ref().clone())
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
    ) -> Result<(Data<TypeV, Kind, CtorV, RcType>, Vec<RcType>), TyckError> {
        let ty = self.resolve_alias(ty, span)?;
        let SynType::TypeApp(TypeApp { tvar, args }) = ty.synty else {
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
    ) -> Result<(Codata<TypeV, Kind, DtorV, RcType>, Vec<RcType>), TyckError> {
        let ty = self.resolve_alias(ty, span)?;
        let SynType::TypeApp(TypeApp { tvar, args }) = ty.synty else {
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
        while let SynType::TypeApp(TypeApp { tvar, args }) = &typ.synty {
            if let Some(Alias { name, params, ty }) = self.alias_env.get(tvar) {
                let ty = ty.inner_ref().clone();
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
                typ = ty.subst(diff, self.clone())?;
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
        let ty = self.inner_ref().clone().subst(ctx.type_env.clone(), ctx.clone())?;
        match &ty.synty {
            SynType::TypeApp(app) => {
                let tvar = &app.tvar;
                // type constructor
                let Some(TypeArity { params, kd }) = ctx.type_ctx.get(&tvar) else {
                    Err(ctx.err(span,
                        NameResolveError::UnboundTypeVariable {
                            tvar: tvar.to_owned(),
                        }.into()
                    ))?
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
                    arg.ana(kd.clone(), ctx.clone())?;
                }
                Ok(Step::Done(kd.clone()))
            }
            SynType::Forall(Forall { param: (param, kd), ty }) => {
                ctx.type_ctx.insert(param.clone(), TypeArity { params: vec![], kd: kd.clone() });
                ty.ana(Kind::CType, ctx)?;
                Ok(Step::Done(Kind::CType))
            }
            SynType::Exists(Exists { param: (param, kd), ty }) => {
                ctx.type_ctx.insert(param.clone(), TypeArity { params: vec![], kd: kd.clone() });
                ty.ana(Kind::VType, ctx)?;
                Ok(Step::Done(Kind::VType))
            }
            SynType::AbstVar(AbstVar(abs)) => Ok(Step::Done(ctx.abst_ctx[*abs])),
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
        let ty = self.inner_ref().clone().subst(ctx.type_env.clone(), ctx.clone())?;
        match ty.synty {
            SynType::Hole(_) => Ok(Step::Done(kd)),
            SynType::TypeApp(_) | SynType::Forall(_) | SynType::Exists(_) | SynType::AbstVar(_) => {
                let kd_syn = self.syn(ctx.clone())?;
                kd_syn.lub(kd, Default::default(), span)?;
                Ok(Step::Done(kd))
            }
        }
    }
}

impl Type {
    pub(super) fn subst(self, mut diff: Env<TypeV, Type>, ctx: Ctx) -> Result<Self, TyckError> {
        match self.synty {
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
                        *arg = rc!((arg.as_ref().clone())
                            .try_map(|ty| ty.subst(diff.clone(), ctx.clone()))?);
                    }
                    Ok(Type { synty: TypeApp { tvar, args }.into() })
                }
            }
            SynType::Forall(Forall { param, ty }) => {
                diff.remove(&param.0);
                Ok(Type {
                    synty: Forall {
                        param,
                        ty: rc!((ty.as_ref().clone())
                            .try_map(|ty| ty.subst(diff.clone(), ctx.clone()))?),
                    }
                    .into(),
                })
            }
            SynType::Exists(Exists { param, ty }) => {
                diff.remove(&param.0);
                Ok(Type {
                    synty: Exists {
                        param,
                        ty: rc!((ty.as_ref().clone())
                            .try_map(|ty| ty.subst(diff.clone(), ctx.clone()))?),
                    }
                    .into(),
                })
            }
            SynType::AbstVar(_) | SynType::Hole(_) => Ok(self),
        }
    }
}

impl Env<TypeV, Type> {
    pub(super) fn init(
        params: &[(TypeV, Kind)], ty_app_args: &[RcType], arity_err: impl FnOnce() -> TyckError,
    ) -> Result<Self, TyckError> {
        bool_test(params.len() == ty_app_args.len(), arity_err)?;
        Ok(Env::from_iter(
            params
                .iter()
                .map(|(tvar, _)| tvar.to_owned())
                .zip(ty_app_args.iter().map(|arg| arg.inner_ref().to_owned())),
        ))
    }
}
