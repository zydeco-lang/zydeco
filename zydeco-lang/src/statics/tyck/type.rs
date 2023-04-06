use super::*;

impl TypeCheck for Span<Type> {
    type Ctx = Ctx;
    type Out = Kind;
    fn syn_step(
        &self, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TyckErrorItem>> {
        ctx.trace.push(Frame {
            tycker_src: format!("{}:{}:{}", file!(), line!(), column!()),
            sort: "syn type".to_owned(),
            term: format!("{}", self.inner_ref().fmt()),
            info: self.span().clone(),
        });
        let span = self.span();
        let ty = self.inner_ref().clone().subst(ctx.type_env.clone())?;
        match &ty.synty {
            SynType::TypeApp(app) => {
                let tvar = &app.tvar;
                // type constructor
                let Some(TypeArity { params, kd }) = ctx.type_ctx.get(&tvar) else {
                        Err(span.make(
                            NameResolveError::UnboundTypeVariable {
                                tvar: tvar.to_owned(),
                            }.into()
                        ))?
                    };
                bool_test(app.args.len() == params.len(), || {
                    span.make(ArityMismatch {
                        context: format!("{}", self.inner_ref().fmt()),
                        expected: params.len(),
                        found: app.args.len(),
                    })
                })?;
                for (arg, kd) in app.args.iter().zip(params.iter()) {
                    arg.ana(kd.clone(), ctx.clone())?;
                }
                Ok(Step::Done(kd.clone()))
            }
            SynType::Forall(Forall { param: (param, kd), ty }) => {
                ctx.type_ctx.insert(
                    param.clone(),
                    TypeArity { params: vec![], kd: kd.clone() },
                );
                ty.ana(Kind::CType, ctx)?;
                Ok(Step::Done(Kind::CType))
            }
            SynType::Exists(Exists { param: (param, kd), ty }) => {
                ctx.type_ctx.insert(
                    param.clone(),
                    TypeArity { params: vec![], kd: kd.clone() },
                );
                ty.ana(Kind::VType, ctx)?;
                Ok(Step::Done(Kind::VType))
            }
            SynType::AbstVar(AbstVar(abs)) => {
                Ok(Step::Done(ctx.abst_ctx[*abs]))
            }
            SynType::Hole(_) => {
                Err(span.make(NeedAnnotation { content: format!("hole") }))?
            }
        }
    }
    fn ana_step(
        &self, kd: Self::Out, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TyckErrorItem>> {
        ctx.trace.push(Frame {
            tycker_src: format!("{}:{}:{}", file!(), line!(), column!()),
            sort: format!("ana type with kind {}", kd.fmt()),
            term: format!("{}", self.inner_ref().fmt()),
            info: self.span().clone(),
        });
        let span = self.span();
        let ty = self.inner_ref().clone().subst(ctx.type_env.clone())?;
        match ty.synty {
            SynType::Hole(_) => Ok(Step::Done(kd)),
            SynType::TypeApp(_)
            | SynType::Forall(_)
            | SynType::Exists(_)
            | SynType::AbstVar(_) => {
                let kd_syn = self.syn(ctx)?;
                kd_syn.eqv(&kd, Default::default(), || {
                    span.make(Subsumption { sort: "type" })
                })?;
                Ok(Step::Done(kd))
            }
        }
    }
}

impl Type {
    pub(super) fn subst(
        self, mut diff: Env<TypeV, Type>,
    ) -> Result<Self, Span<TyckErrorItem>> {
        match self.synty {
            SynType::TypeApp(TypeApp { tvar, mut args }) => {
                if let Some(ty) = diff.get(&tvar) {
                    bool_test(args.is_empty(), || {
                        tvar.span().make(ArityMismatch {
                            context: format!("type variable `{}`", tvar),
                            expected: 0,
                            found: args.len(),
                        })
                    })?;
                    Ok(ty.clone())
                } else {
                    for arg in args.iter_mut() {
                        *arg = rc!(arg
                            .as_ref()
                            .clone()
                            .try_map(|ty| ty.subst(diff.clone()))?);
                    }
                    Ok(Type { synty: TypeApp { tvar, args }.into() })
                }
            }
            SynType::Forall(Forall { param, ty }) => {
                diff.remove(&param.0);
                Ok(Type {
                    synty: Forall {
                        param,
                        ty: rc!(ty
                            .as_ref()
                            .clone()
                            .try_map(|ty| ty.subst(diff.clone()))?),
                    }
                    .into(),
                })
            }
            SynType::Exists(Exists { param, ty }) => {
                diff.remove(&param.0);
                Ok(Type {
                    synty: Exists {
                        param,
                        ty: rc!(ty
                            .as_ref()
                            .clone()
                            .try_map(|ty| ty.subst(diff.clone()))?),
                    }
                    .into(),
                })
            }
            SynType::AbstVar(_) | SynType::Hole(_) => Ok(self),
        }
    }
    pub(super) fn lub(
        lhs: Self, rhs: Self, ctx: Ctx,
        f: impl FnOnce() -> Span<TyckErrorItem> + Clone,
    ) -> Result<Self, Span<TyckErrorItem>> {
        let lhs = lhs.subst(ctx.type_env.clone()).unwrap();
        let rhs = rhs.subst(ctx.type_env.clone()).unwrap();
        match (&lhs.synty, &rhs.synty) {
            (SynType::Hole(_), _) => Ok(rhs),
            (_, SynType::Hole(_)) => Ok(lhs),
            (SynType::TypeApp(lhs), SynType::TypeApp(rhs)) => {
                bool_test(lhs.tvar == rhs.tvar, f.clone())?;
                let mut args = vec![];
                for (lhs, rhs) in (lhs.args.iter()).zip(rhs.args.iter()) {
                    let arg = Self::lub(
                        lhs.inner_ref().clone(),
                        rhs.inner_ref().clone(),
                        ctx.clone(),
                        f.clone(),
                    )?;
                    args.push(rc!(lhs.span().make(arg)));
                }
                Ok(TypeApp { tvar: lhs.tvar.clone(), args }.into())
            }
            (SynType::Forall(lhs), SynType::Forall(rhs)) => {
                bool_test(lhs.param == rhs.param, f.clone())?;
                let ty = Self::lub(
                    lhs.ty.inner_ref().clone(),
                    rhs.ty.inner_ref().clone(),
                    ctx,
                    f,
                )?;
                Ok(Forall {
                    param: lhs.param.clone(),
                    ty: rc!(lhs.ty.span().make(ty)),
                }
                .into())
            }
            (SynType::Exists(lhs), SynType::Exists(rhs)) => {
                bool_test(lhs.param == rhs.param, f.clone())?;
                let ty = Self::lub(
                    lhs.ty.inner_ref().clone(),
                    rhs.ty.inner_ref().clone(),
                    ctx,
                    f,
                )?;
                Ok(Exists {
                    param: lhs.param.clone(),
                    ty: rc!(lhs.ty.span().make(ty)),
                }
                .into())
            }
            (SynType::AbstVar(lhs), SynType::AbstVar(rhs)) => {
                bool_test(lhs == rhs, f)?;
                Ok(lhs.clone().into())
            }
            (SynType::TypeApp(_), _)
            | (SynType::Forall(_), _)
            | (SynType::Exists(_), _)
            | (SynType::AbstVar(_), _) => Err(f()),
        }
    }
}
