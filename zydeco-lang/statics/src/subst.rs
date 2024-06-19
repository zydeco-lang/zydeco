use crate::*;
use ss::*;

impl TypeId {
    pub fn subst(&self, tycker: &mut Tycker, var: DefId, with: TypeId) -> Result<TypeId> {
        let ty = tycker.statics.types[self].clone();
        match ty {
            | Type::Var(def) => {
                if def == var {
                    Ok(with)
                } else {
                    Ok(*self)
                }
            }
            | Type::Abst(_) => Ok(*self),
            | Type::Fill(_) => unreachable!(),
            | Type::Abs(abs) => {
                let Abs(tpat, ty) = abs;
                let (def, _) = tycker.extract_tpat(tpat);
                if let Some(def) = def {
                    if def == var {
                        unreachable!()
                    }
                }
                let ty_ = ty.subst(tycker, var, with)?;
                if ty == ty_ {
                    Ok(*self)
                } else {
                    Ok(Alloc::alloc(tycker, Abs(tpat, ty_)))
                }
            }
            | Type::App(app) => {
                let App(ty1, ty2) = app;
                let ty1_ = ty1.subst(tycker, var, with)?;
                let ty2_ = ty2.subst(tycker, var, with)?;
                if ty1 == ty1_ && ty2 == ty2_ {
                    Ok(*self)
                } else {
                    Ok(Alloc::alloc(tycker, App(ty1_, ty2_)))
                }
            }
            | Type::Thunk(_)
            | Type::Ret(_)
            | Type::Unit(_)
            | Type::Int(_)
            | Type::Char(_)
            | Type::String(_)
            | Type::OS(_) => Ok(*self),
            | Type::Arrow(arr) => {
                let Arrow(ty1, ty2) = arr;
                let ty1_ = ty1.subst(tycker, var, with)?;
                let ty2_ = ty2.subst(tycker, var, with)?;
                if ty1 == ty1_ && ty2 == ty2_ {
                    Ok(*self)
                } else {
                    Ok(Alloc::alloc(tycker, Arrow(ty1_, ty2_)))
                }
            }
            | Type::Forall(forall) => {
                let Forall(tpat, ty) = forall;
                let (def, _) = tycker.extract_tpat(tpat);
                if let Some(def) = def {
                    if def == var {
                        unreachable!()
                    }
                }
                let ty_ = ty.subst(tycker, var, with)?;
                if ty == ty_ {
                    Ok(*self)
                } else {
                    Ok(Alloc::alloc(tycker, Forall(tpat, ty_)))
                }
            }
            | Type::Prod(prod) => {
                let Prod(ty1, ty2) = prod;
                let ty1_ = ty1.subst(tycker, var, with)?;
                let ty2_ = ty2.subst(tycker, var, with)?;
                if ty1 == ty1_ && ty2 == ty2_ {
                    Ok(*self)
                } else {
                    Ok(Alloc::alloc(tycker, Prod(ty1_, ty2_)))
                }
            }
            | Type::Exists(exists) => {
                let Exists(tpat, ty) = exists;
                let (def, _) = tycker.extract_tpat(tpat);
                if let Some(def) = def {
                    if def == var {
                        unreachable!()
                    }
                }
                let ty_ = ty.subst(tycker, var, with)?;
                if ty == ty_ {
                    Ok(*self)
                } else {
                    Ok(Alloc::alloc(tycker, Exists(tpat, ty_)))
                }
            }
            | Type::Data(data) => {
                todo!()
            }
            | Type::CoData(coda) => {
                todo!()
            }
        }
    }
}
