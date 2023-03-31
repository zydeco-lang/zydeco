use super::*;

impl TypeCheck for Span<Type> {
    type Ctx = Ctx;
    type Out = Kind;
    fn syn_step(
        &self, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        let ty = self.inner_ref().clone().subst(ctx.type_env.clone())?;
        match &ty.synty {
            SynType::TypeApp(app) => {
                let tvar = &app.tvar;
                // type constructor
                let Some(TypeArity { params, kd }) = ctx.type_ctx.get(&tvar) else {
                        Err(self.span().make(
                            NameResolveError::UnboundTypeVariable {
                                tvar: tvar.to_owned(),
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
                    arg.ana(kd.clone(), ctx.clone())?
                }
                Ok(Step::Done(kd.clone()))
            }
            SynType::Forall(Forall { param, kd, ty }) => {
                ctx.type_ctx.insert(
                    param.clone(),
                    TypeArity { params: vec![], kd: kd.clone() },
                );
                ty.ana(Kind::CType, ctx)?;
                Ok(Step::Done(Kind::CType))
            }
            SynType::Exists(Exists { param, kd, ty }) => {
                ctx.type_ctx.insert(
                    param.clone(),
                    TypeArity { params: vec![], kd: kd.clone() },
                );
                ty.ana(Kind::VType, ctx)?;
                Ok(Step::Done(Kind::VType))
            }
            SynType::Abstract(Abstract(abs)) => {
                Ok(Step::Done(ctx.abst_ctx[*abs]))
            }
            SynType::Hole(_) => Err(self
                .span()
                .make(NeedAnnotation { content: format!("hole") }))?,
        }
    }
    fn ana_step(
        &self, kd: Self::Out, ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        let ty = self.inner_ref().clone().subst(ctx.type_env.clone())?;
        match ty.synty {
            SynType::Hole(_) => Ok(Step::Done(kd)),
            SynType::TypeApp(_)
            | SynType::Forall(_)
            | SynType::Exists(_)
            | SynType::Abstract(_) => {
                let span = self.span().clone();
                let kd_syn = self.syn(ctx)?;
                kd_syn
                    .eqv(&kd, Default::default(), || span.make(Subsumption))?;
                Ok(Step::Done(kd))
            }
        }
    }
}

impl Type {
    pub(super) fn subst(
        self, mut diff: Env<TypeV, Type>,
    ) -> Result<Self, Span<TypeCheckError>> {
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
            SynType::Forall(Forall { param, kd, ty }) => {
                diff.remove(&param);
                Ok(Type {
                    synty: Forall {
                        param,
                        kd,
                        ty: rc!(ty
                            .as_ref()
                            .clone()
                            .try_map(|ty| ty.subst(diff.clone()))?),
                    }
                    .into(),
                })
            }
            SynType::Exists(Exists { param, kd, ty }) => {
                diff.remove(&param);
                Ok(Type {
                    synty: Exists {
                        param,
                        kd,
                        ty: rc!(ty
                            .as_ref()
                            .clone()
                            .try_map(|ty| ty.subst(diff.clone()))?),
                    }
                    .into(),
                })
            }
            SynType::Abstract(_) | SynType::Hole(_) => Ok(self),
        }
    }
}
