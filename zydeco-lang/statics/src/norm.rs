use crate::{syntax::*, *};

pub trait Normalize {
    type Ctx;
    type Out;
    fn normalize(self, tycker: &mut Tycker, ctx: Self::Ctx) -> Result<Self::Out>;
}

impl Normalize for &TypeId {
    type Ctx = Context<CtxItem>;
    type Out = TypeId;

    fn normalize(self, tycker: &mut Tycker, ctx: Self::Ctx) -> Result<Self::Out> {
        let ty = tycker.statics.types[self].clone();
        match ty {
            // | Type::Hole(_)
            | Type::Abst(_)
            | Type::Thunk(_)
            | Type::Ret(_)
            | Type::Unit(_)
            | Type::Int(_)
            | Type::Char(_)
            | Type::String(_)
            | Type::OS(_) => Ok(*self),
            | Type::Sealed(_ty) => {
                // let Sealed(ty) = ty;
                // let ty = ty.normalize(tycker, ctx)?;
                // let sealed = Alloc::alloc(tycker, Sealed(ty));
                // Ok(sealed)
                Ok(*self)
            }
            | Type::Var(def) => {
                let ann = ctx[&def].ann;
                let ty = ann.as_type();
                ty.normalize(tycker, ctx)
            }
            | Type::Abs(_) => Ok(*self),
            | Type::App(ty) => {
                let App(ty_ctor, ty_arg) = ty;
                let ty_ctor = ty_ctor.normalize(tycker, ctx.clone())?;
                let ty_arg = ty_arg.normalize(tycker, ctx.clone())?;
                match tycker.statics.types[&ty_ctor].clone() {
                    | Type::Abs(abs) => {
                        let Abs(tpat, body) = abs;
                        todo!()
                        // let mut ctx = ();
                        // ctx = abs.extend(tycker, ctx, ty_arg);
                        // abs.body.normalize(tycker, ctx)
                    }
                    | _ => Err(TyckError::TypeMismatch),
                }
            }
            | Type::Arrow(ty) => {
                let Arrow(ty_dom, ty_cod) = ty;
                let ty_dom = ty_dom.normalize(tycker, ctx.clone())?;
                let ty_cod = ty_cod.normalize(tycker, ctx.clone())?;
                let ty = Alloc::alloc(tycker, Arrow(ty_dom, ty_cod));
                Ok(ty)
            }
            | Type::Forall(_) => todo!(),
            | Type::Prod(ty) => {
                let Prod(ty_l, ty_r) = ty;
                let ty_l = ty_l.normalize(tycker, ctx.clone())?;
                let ty_r = ty_r.normalize(tycker, ctx.clone())?;
                let ty = Alloc::alloc(tycker, Prod(ty_l, ty_r));
                Ok(ty)
            }
            | Type::Exists(_) => todo!(),
            | Type::Data(_) => todo!(),
            | Type::CoData(_) => todo!(),
        }
    }
}
