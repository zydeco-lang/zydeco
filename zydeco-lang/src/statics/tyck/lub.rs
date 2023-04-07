use super::*;

impl Lub for () {
    type Ctx = Ctx;
    type Out = ();
    fn lub(self, _rhs: (), _ctx: Ctx, _span: &SpanInfo) -> Result<(), TyckError> {
        Ok(())
    }
}

impl<T> Lub for Seal<T> {
    type Ctx = ();
    type Out = Seal<T>;
    fn lub(self, _other: Self, _: Self::Ctx, _: &SpanInfo) -> Result<Self::Out, TyckError> {
        unreachable!()
    }
}

impl Lub for Kind {
    type Ctx = Ctx;
    type Out = Kind;
    fn lub(self, rhs: Kind, ctx: Ctx, span: &SpanInfo) -> Result<Kind, TyckError> {
        bool_test(self == rhs, || {
            ctx.err(span, KindMismatch { context: format!("lub"), expected: self, found: rhs })
        })?;
        Ok(self)
    }
}

impl Lub for Type {
    type Ctx = Ctx;
    type Out = Type;
    fn lub(self, rhs: Type, mut ctx: Ctx, span: &SpanInfo) -> Result<Type, TyckError> {
        let lhs = self;
        let err = {
            let expected = lhs.clone();
            let found = rhs.clone();
            || ctx.err(span, TypeMismatch { context: format!("lub"), expected, found })
        };
        // let lhs = self.subst(ctx.type_env.clone()).map_err(|e| e.traced(ctx.trace.clone()))?;
        // let rhs = rhs.subst(ctx.type_env.clone()).map_err(|e| e.traced(ctx.trace.clone()))?;
        match (&lhs.synty, &rhs.synty) {
            // (SynType::Hole(_), SynType::Hole(_)) => Err(err())?,
            (SynType::Hole(_), _) => Ok(rhs),
            (_, SynType::Hole(_)) => Ok(lhs),
            (SynType::TypeApp(lhs), _) if ctx.type_env.contains_key(&lhs.tvar) => {
                // lhs is a type variable
                let ty = ctx.clone().type_env[&lhs.tvar].clone();
                ty.lub(rhs, ctx, span)
            }
            (_, SynType::TypeApp(rhs)) if ctx.type_env.contains_key(&rhs.tvar) => {
                // rhs is a type variable
                let ty = ctx.clone().type_env[&rhs.tvar].clone();
                lhs.lub(ty, ctx, span)
            }
            (SynType::TypeApp(lhs), _) if ctx.alias_env.contains_key(&lhs.tvar) => {
                // lhs is a type variable
                let Alias { name, params, ty } = ctx.clone().alias_env[&lhs.tvar].clone();
                let diff = Env::init(&params, &lhs.args, || {
                    span.make(ArityMismatch {
                        context: format!("data type `{}` instiantiation", name),
                        expected: params.len(),
                        found: lhs.args.len(),
                    })
                })
                .map_err(|e| e.traced(ctx.trace.clone()))?;
                let ty =
                    ty.inner_ref().clone().subst(diff).map_err(|e| e.traced(ctx.trace.clone()))?;
                ty.lub(rhs, ctx, span)
            }
            (_, SynType::TypeApp(rhs)) if ctx.alias_env.contains_key(&rhs.tvar) => {
                // lhs is a type variable
                let Alias { name, params, ty } = ctx.clone().alias_env[&rhs.tvar].clone();
                let diff = Env::init(&params, &rhs.args, || {
                    span.make(ArityMismatch {
                        context: format!("data type `{}` instiantiation", name),
                        expected: params.len(),
                        found: rhs.args.len(),
                    })
                })
                .map_err(|e| e.traced(ctx.trace.clone()))?;
                let ty =
                    ty.inner_ref().clone().subst(diff).map_err(|e| e.traced(ctx.trace.clone()))?;
                lhs.lub(ty, ctx, span)
            }
            (SynType::TypeApp(lhs), SynType::TypeApp(rhs)) => {
                bool_test(lhs.tvar == rhs.tvar, err)?;
                let mut args = vec![];
                for (lhs, rhs) in (lhs.args.iter()).zip(rhs.args.iter()) {
                    let arg = Self::lub(
                        lhs.inner_ref().clone(),
                        rhs.inner_ref().clone(),
                        ctx.clone(),
                        span,
                    )?;
                    args.push(rc!(lhs.span().make(arg)));
                }
                Ok(TypeApp { tvar: lhs.tvar.clone(), args }.into())
            }
            (SynType::Forall(lhs), SynType::Forall(rhs)) => {
                bool_test(lhs.param.1 == rhs.param.1, err)?;
                let abst_var = ctx.fresh(lhs.param.1.clone());
                let lhs_ty = lhs
                    .ty
                    .inner_ref()
                    .clone()
                    .subst(Env::from_iter([(lhs.param.0.clone(), abst_var.clone().into())]))
                    .map_err(|e| e.traced(ctx.trace.clone()))?;
                let rhs_ty = rhs
                    .ty
                    .inner_ref()
                    .clone()
                    .subst(Env::from_iter([(rhs.param.0.clone(), abst_var.into())]))
                    .map_err(|e| e.traced(ctx.trace.clone()))?;
                let _ty = lhs_ty.lub(rhs_ty, ctx, span)?;
                // HACK: needs revertable type subst
                // Ok(Forall { param: lhs.param.clone(), ty: rc!(lhs.ty.span().make(ty)) }.into())
                Ok(lhs.clone().into())
            }
            (SynType::Exists(lhs), SynType::Exists(rhs)) => {
                bool_test(lhs.param.1 == rhs.param.1, err)?;
                let mut ctx = ctx;
                let abst_var = ctx.fresh(lhs.param.1.clone());
                let lhs_ty = lhs
                    .ty
                    .inner_ref()
                    .clone()
                    .subst(Env::from_iter([(lhs.param.0.clone(), abst_var.clone().into())]))
                    .map_err(|e| e.traced(ctx.trace.clone()))?;
                let rhs_ty = rhs
                    .ty
                    .inner_ref()
                    .clone()
                    .subst(Env::from_iter([(rhs.param.0.clone(), abst_var.into())]))
                    .map_err(|e| e.traced(ctx.trace.clone()))?;
                let _ty = lhs_ty.lub(rhs_ty, ctx, span)?;
                // HACK: needs revertable type subst
                // Ok(Exists { param: lhs.param.clone(), ty: rc!(lhs.ty.span().make(ty)) }.into())
                Ok(lhs.clone().into())
            }
            (SynType::AbstVar(lhs), SynType::AbstVar(rhs)) => {
                bool_test(lhs == rhs, err)?;
                Ok(lhs.clone().into())
            }
            (SynType::TypeApp(_), _)
            | (SynType::Forall(_), _)
            | (SynType::Exists(_), _)
            | (SynType::AbstVar(_), _) => Err(err()),
        }
    }
}
