use super::*;

pub trait MonadTrans {
    fn lift_ty(&self, ty_m: Type, ctx: Ctx, span: &Span) -> Result<Type, TyckError>;
}

impl MonadTrans for Type {
    fn lift_ty(&self, ty_m: Type, ctx: Ctx, span: &Span) -> Result<Type, TyckError> {
        let synty = match &self.synty {
            SynType::TypeAbs(_) => {
                // Type abstraction would not be of kind "CType"
                Err(ctx.err(
                    span,
                    TyckErrorItem::KindMismatch {
                        context: format!("begin-block-lift"),
                        expected: KindBase::CType.into(),
                        found: span.make(self.clone()).syn(ctx.clone())?.into(),
                    },
                ))?
            }
            SynType::TypeApp(TypeApp { tvar, args: old_args }) => {
                let mut args = Vec::new();
                for arg in old_args {
                    args.push(span.make_rc(arg.inner_ref().lift_ty(
                        ty_m.clone(),
                        ctx.clone(),
                        span,
                    )?));
                }
                let tvar = match tvar {
                    NeutralVar::Var(tv) if tv.name() == "Ret" => {
                        // Substitute if it's Ret
                        return ty_m.apply(args, &ctx);
                    }
                    _ => tvar.clone(),
                };
                SynType::TypeApp(TypeApp { tvar, args })
            }
            SynType::Arrow(Arrow(ty_in, ty_out)) => {
                let ty_in =
                    span.make_rc(ty_in.inner_ref().lift_ty(ty_m.clone(), ctx.clone(), span)?);
                let ty_out = span.make_rc(ty_out.inner_ref().lift_ty(ty_m, ctx, span)?);
                SynType::Arrow(Arrow(ty_in, ty_out))
            }
            SynType::Forall(Forall { param, ty }) => {
                // Hack: shadowing not considered
                let param = param.clone();
                let ty = span.make_rc(ty.inner_ref().lift_ty(ty_m, ctx.clone(), span)?);
                SynType::Forall(Forall { param, ty })
            }
            SynType::Exists(Exists { param, ty }) => {
                // Hack: shadowing not considered
                let param = param.clone();
                let ty = span.make_rc(ty.inner_ref().lift_ty(ty_m, ctx.clone(), span)?);
                SynType::Forall(Forall { param, ty })
            }
            SynType::AbstVar(_) | SynType::Hole(_) => self.synty.clone(),
        };
        Ok(Type { synty })
    }
}
