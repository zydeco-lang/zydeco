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
    fn lub(self, rhs: Type, ctx: Ctx, span: &SpanInfo) -> Result<Type, TyckError> {
        let err = {
            let expected = self.clone().into();
            let found = rhs.clone().into();
            || ctx.err(span, TypeMismatch { context: format!("lub"), expected, found })
        };
        let lhs = self.subst(ctx.type_env.clone()).map_err(|e| e.traced(ctx.trace.clone()))?;
        let rhs = rhs.subst(ctx.type_env.clone()).map_err(|e| e.traced(ctx.trace.clone()))?;
        match (&lhs.synty, &rhs.synty) {
            (SynType::Hole(_), _) => Ok(rhs),
            (_, SynType::Hole(_)) => Ok(lhs),
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
                bool_test(lhs.param == rhs.param, err)?;
                let ty =
                    Self::lub(lhs.ty.inner_ref().clone(), rhs.ty.inner_ref().clone(), ctx, span)?;
                Ok(Forall { param: lhs.param.clone(), ty: rc!(lhs.ty.span().make(ty)) }.into())
            }
            (SynType::Exists(lhs), SynType::Exists(rhs)) => {
                bool_test(lhs.param == rhs.param, err)?;
                let ty =
                    Self::lub(lhs.ty.inner_ref().clone(), rhs.ty.inner_ref().clone(), ctx, span)?;
                Ok(Exists { param: lhs.param.clone(), ty: rc!(lhs.ty.span().make(ty)) }.into())
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
