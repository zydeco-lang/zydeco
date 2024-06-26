use crate::{syntax::*, *};
use zydeco_utils::arena::ArenaAccess;

impl TypeId {
    pub fn subst_env(&self, tycker: &mut Tycker, env: &Env<AnnId>) -> ResultKont<TypeId> {
        let kd = tycker.statics.annotations_type[self].clone();
        let ty = tycker.statics.types[self].clone();
        match ty {
            | Type::Var(def) => match env.get(&def) {
                | Some(ann) => match ann {
                    | AnnId::Set | AnnId::Kind(_) => {
                        tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | AnnId::Type(with) => Ok(*with),
                },
                | None => Ok(*self),
            },
            | Type::Abst(_) => Ok(*self),
            // Todo: figure out what to do
            | Type::Fill(_) => Ok(*self),
            | Type::Abs(abs) => {
                let Abs(tpat, ty) = abs;
                let (def, _) = tycker.extract_tpat(tpat);
                if let Some(def) = def {
                    if let Some(_with) = env.get(&def) {
                        unreachable!()
                    }
                }
                let ty_ = ty.subst_env(tycker, env)?;
                if ty == ty_ {
                    Ok(*self)
                } else {
                    Ok(Alloc::alloc(&mut tycker.statics, Abs(tpat, ty_), kd))
                }
            }
            | Type::App(app) => {
                let App(ty1, ty2) = app;
                let ty1_ = ty1.subst_env(tycker, env)?;
                let ty2_ = ty2.subst_env(tycker, env)?;
                if ty1 == ty1_ && ty2 == ty2_ {
                    Ok(*self)
                } else {
                    Ok(Alloc::alloc(&mut tycker.statics, App(ty1_, ty2_), kd))
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
                let ty1_ = ty1.subst_env(tycker, env)?;
                let ty2_ = ty2.subst_env(tycker, env)?;
                if ty1 == ty1_ && ty2 == ty2_ {
                    Ok(*self)
                } else {
                    Ok(Alloc::alloc(&mut tycker.statics, Arrow(ty1_, ty2_), kd))
                }
            }
            | Type::Forall(forall) => {
                let Forall(tpat, ty) = forall;
                let (def, _) = tycker.extract_tpat(tpat);
                if let Some(def) = def {
                    if let Some(_with) = env.get(&def) {
                        unreachable!()
                    }
                }
                let ty_ = ty.subst_env(tycker, env)?;
                if ty == ty_ {
                    Ok(*self)
                } else {
                    Ok(Alloc::alloc(&mut tycker.statics, Forall(tpat, ty_), kd))
                }
            }
            | Type::Prod(prod) => {
                let Prod(ty1, ty2) = prod;
                let ty1_ = ty1.subst_env(tycker, env)?;
                let ty2_ = ty2.subst_env(tycker, env)?;
                if ty1 == ty1_ && ty2 == ty2_ {
                    Ok(*self)
                } else {
                    Ok(Alloc::alloc(&mut tycker.statics, Prod(ty1_, ty2_), kd))
                }
            }
            | Type::Exists(exists) => {
                let Exists(tpat, ty) = exists;
                let (def, _) = tycker.extract_tpat(tpat);
                if let Some(def) = def {
                    if let Some(_with) = env.get(&def) {
                        unreachable!()
                    }
                }
                let ty_ = ty.subst_env(tycker, env)?;
                if ty == ty_ {
                    Ok(*self)
                } else {
                    Ok(Alloc::alloc(&mut tycker.statics, Exists(tpat, ty_), kd))
                }
            }
            | Type::Data(data) => {
                let arms = tycker.statics.datas.defs[&data].clone();
                let mut unchanged = true;
                let arms_ = arms
                    .into_iter()
                    .map(|(ctor, ty)| {
                        let ty_ = ty.subst_env(tycker, env)?;
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
                    tycker.statics.datas.tbls.insert_or_replace(data_, d.to_owned());
                    tycker.statics.datas.eqs.insert_or_replace(d, data_);
                    Ok(Alloc::alloc(&mut tycker.statics, data_, kd))
                }
            }
            | Type::CoData(coda) => {
                let arms = tycker.statics.codatas.defs[&coda].clone();
                let mut unchanged = true;
                let arms_ = arms
                    .into_iter()
                    .map(|(dtor, ty)| {
                        let ty_ = ty.subst_env(tycker, env)?;
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
                    tycker.statics.codatas.tbls.insert_or_replace(coda_, d.to_owned());
                    tycker.statics.codatas.eqs.insert_or_replace(d, coda_);
                    Ok(Alloc::alloc(&mut tycker.statics, coda_, kd))
                }
            }
        }
    }
    pub fn subst(&self, tycker: &mut Tycker, var: DefId, with: TypeId) -> ResultKont<TypeId> {
        self.subst_env(tycker, &Env::singleton(var, with.into()))
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
                if ty1 == ty1_ {
                    self
                } else {
                    let app = Alloc::alloc(&mut tycker.statics, App(ty1_, ty2), kd);
                    app.normalize(tycker, kd)?
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

impl TypeId {
    pub fn normalize(self, tycker: &mut Tycker, kd: KindId) -> ResultKont<TypeId> {
        let res = match tycker.statics.types[&self].to_owned() {
            | Type::App(app) => {
                let App(ty1, ty2) = app;
                ty1.normalize_app(tycker, ty2, kd)?
            }
            | Type::Var(_)
            | Type::Abst(_)
            | Type::Fill(_)
            | Type::Abs(_)
            | Type::Thunk(_)
            | Type::Ret(_)
            | Type::Unit(_)
            | Type::Int(_)
            | Type::Char(_)
            | Type::String(_)
            | Type::OS(_)
            | Type::Arrow(_)
            | Type::Forall(_)
            | Type::Prod(_)
            | Type::Exists(_)
            | Type::Data(_)
            | Type::CoData(_) => self,
        };
        Ok(res)
    }
    pub fn normalize_app(
        self, tycker: &mut Tycker, a_ty: TypeId, kd: KindId,
    ) -> ResultKont<TypeId> {
        let res = match tycker.statics.types[&self].to_owned() {
            | ss::Type::Abs(abs) => {
                // if f_ty is an abstraction, apply it
                let ss::Abs(binder, body_ty) = abs;
                let (def, _) = tycker.extract_tpat(binder);
                let body_ty_subst =
                    if let Some(def) = def { body_ty.subst(tycker, def, a_ty)? } else { body_ty };
                body_ty_subst
            }
            | _ => {
                // else, the app is already normalized
                Alloc::alloc(&mut tycker.statics, ss::App(self, a_ty), kd)
            }
        };
        Ok(res)
    }
}
