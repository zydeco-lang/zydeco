use super::*;

pub trait MonadTrans {
    fn lift_ty(&self, ty_m: Type, ctx: Ctx, span: &Span) -> Result<Type, TyckError>;
}

impl MonadTrans for Type {
    fn lift_ty(&self, ty_m: Type, ctx: Ctx, span: &Span) -> Result<Type, TyckError> {
        let ty = self.clone();
        // Todo: ...
        Ok(ty)
    }
}
