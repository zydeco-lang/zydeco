use crate::{syntax::*, *};
use zydeco_utils::arena::ArenaAccess;

impl TypeId {
    pub fn subst(&self, tycker: &mut Tycker, var: DefId, with: TypeId) -> ResultKont<TypeId> {
        let kd = tycker.statics.annotations_type[self].clone();
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
                    Ok(Alloc::alloc(tycker, Abs(tpat, ty_), kd))
                }
            }
            | Type::App(app) => {
                let App(ty1, ty2) = app;
                let ty1_ = ty1.subst(tycker, var, with)?;
                let ty2_ = ty2.subst(tycker, var, with)?;
                if ty1 == ty1_ && ty2 == ty2_ {
                    Ok(*self)
                } else {
                    Ok(Alloc::alloc(tycker, App(ty1_, ty2_), kd))
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
                    Ok(Alloc::alloc(tycker, Arrow(ty1_, ty2_), kd))
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
                    Ok(Alloc::alloc(tycker, Forall(tpat, ty_), kd))
                }
            }
            | Type::Prod(prod) => {
                let Prod(ty1, ty2) = prod;
                let ty1_ = ty1.subst(tycker, var, with)?;
                let ty2_ = ty2.subst(tycker, var, with)?;
                if ty1 == ty1_ && ty2 == ty2_ {
                    Ok(*self)
                } else {
                    Ok(Alloc::alloc(tycker, Prod(ty1_, ty2_), kd))
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
                    Ok(Alloc::alloc(tycker, Exists(tpat, ty_), kd))
                }
            }
            | Type::Data(data) => {
                let arms = tycker.statics.datas.defs[&data].clone();
                let mut unchanged = true;
                let arms_ = arms
                    .into_iter()
                    .map(|(ctor, ty)| {
                        let ty_ = ty.subst(tycker, var, with)?;
                        if ty == ty_ {
                            Ok((ctor, ty))
                        } else {
                            unchanged = false;
                            Ok((ctor, ty_))
                        }
                    })
                    .collect::<ResultKont<im::Vector<_>>>()?;
                if unchanged {
                    Ok(*self)
                } else {
                    let d = Data { arms: arms_.iter().cloned().collect() };
                    let data_ = tycker.statics.datas.defs.alloc(arms_);
                    tycker.statics.datas.tbls.insert(data_, d.to_owned());
                    tycker.statics.datas.eqs.insert(d, data_);
                    Ok(Alloc::alloc(tycker, data_, kd))
                }
            }
            | Type::CoData(coda) => {
                let arms = tycker.statics.codatas.defs[&coda].clone();
                let mut unchanged = true;
                let arms_ = arms
                    .into_iter()
                    .map(|(dtor, ty)| {
                        let ty_ = ty.subst(tycker, var, with)?;
                        if ty == ty_ {
                            Ok((dtor, ty))
                        } else {
                            unchanged = false;
                            Ok((dtor, ty_))
                        }
                    })
                    .collect::<ResultKont<im::Vector<_>>>()?;
                if unchanged {
                    Ok(*self)
                } else {
                    let d = CoData { arms: arms_.iter().cloned().collect() };
                    let coda_ = tycker.statics.codatas.defs.alloc(arms_);
                    tycker.statics.codatas.tbls.insert(coda_, d.to_owned());
                    tycker.statics.codatas.eqs.insert(d, coda_);
                    Ok(Alloc::alloc(tycker, coda_, kd))
                }
            }
        }
    }
}

impl TypeId {
    pub fn unroll(self, tycker: &mut Tycker) -> ResultKont<TypeId> {
        let kd = tycker.statics.annotations_type[&self].clone();
        let res = match tycker.statics.types[&self].to_owned() {
            | Type::Abst(abst) => {
                match tycker.statics.seals.get(&abst) {
                    | Some(ty) => {
                        ty.unroll(tycker)?
                    }
                    | None => self,
                }
            }
            | Type::App(ty) => {
                // congruence rule
                let App(ty1, ty2) = ty;
                let ty1_ = ty1.unroll(tycker)?;
                let ty2_ = ty2.unroll(tycker)?;
                // Todo: normalize
                if ty1 == ty1_ && ty2 == ty2_ {
                    self
                } else {
                    Alloc::alloc(tycker, App(ty1_, ty2_), kd)
                }
            }
            | Type::Fill(_) // unchanged because terms with unfilled types can't be matched against
            | Type::Var(_) // unchanged because type-variable-typed terms are abstract
            | Type::Abs(_) // unchanged because type-abstration-typed terms are ill-formed
            | Type::Thunk(_)
            | Type::Ret(_)
            | Type::Unit(_)
            | Type::Int(_)
            | Type::Char(_)
            | Type::String(_)
            | Type::OS(_) => self,
            | Type::Arrow(_) 
            | Type::Forall(_) 
            | Type::Prod(_) 
            | Type::Exists(_) => self,
            | Type::Data(_) 
            | Type::CoData(_) => self
        };
        Ok(res)
    }
}

// Todo: normalize

