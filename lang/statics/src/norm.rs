use crate::{syntax::*, *};
use zydeco_utils::arena::ArenaAccess;

impl TypeId {
    pub fn subst_env_k(&self, tycker: &mut Tycker, env: &Env<AnnId>) -> ResultKont<TypeId> {
        let res = self.subst_env(tycker, env);
        tycker.err_p_to_k(res)
    }
    pub fn subst_env(&self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let kd = tycker.statics.annotations_type[self].clone();
        let ty = tycker.statics.types[self].clone();
        let ty = match ty {
            | Type::Var(def) => match env.get(&def) {
                | Some(ann) => match ann {
                    | AnnId::Set | AnnId::Kind(_) => {
                        tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | AnnId::Type(with) => *with,
                },
                | None => *self,
            },
            | Type::Abst(_) => *self,
            // Todo: figure out what to do
            | Type::Fill(_) => *self,
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
                    *self
                } else {
                    Alloc::alloc(&mut tycker.statics, Abs(tpat, ty_), kd)
                }
            }
            | Type::App(app) => {
                let App(ty1, ty2) = app;
                let ty1_ = ty1.subst_env(tycker, env)?;
                let ty2_ = ty2.subst_env(tycker, env)?;
                if ty1 == ty1_ && ty2 == ty2_ {
                    *self
                } else {
                    Alloc::alloc(&mut tycker.statics, App(ty1_, ty2_), kd)
                }
            }
            | Type::Thunk(_)
            | Type::Ret(_)
            | Type::Unit(_)
            | Type::Int(_)
            | Type::Char(_)
            | Type::String(_)
            | Type::OS(_) => *self,
            | Type::Arrow(arr) => {
                let Arrow(ty1, ty2) = arr;
                let ty1_ = ty1.subst_env(tycker, env)?;
                let ty2_ = ty2.subst_env(tycker, env)?;
                if ty1 == ty1_ && ty2 == ty2_ {
                    *self
                } else {
                    Alloc::alloc(&mut tycker.statics, Arrow(ty1_, ty2_), kd)
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
                    *self
                } else {
                    Alloc::alloc(&mut tycker.statics, Forall(tpat, ty_), kd)
                }
            }
            | Type::Prod(prod) => {
                let Prod(ty1, ty2) = prod;
                let ty1_ = ty1.subst_env(tycker, env)?;
                let ty2_ = ty2.subst_env(tycker, env)?;
                if ty1 == ty1_ && ty2 == ty2_ {
                    *self
                } else {
                    Alloc::alloc(&mut tycker.statics, Prod(ty1_, ty2_), kd)
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
                    *self
                } else {
                    Alloc::alloc(&mut tycker.statics, Exists(tpat, ty_), kd)
                }
            }
            | Type::Data(id) => {
                let arms = tycker.statics.datas.defs[&id].clone();
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
                    .collect::<Result<im::Vector<_>>>()?;
                if unchanged {
                    *self
                } else {
                    let data_ = Data { arms: arms_.iter().cloned().collect() };
                    let id_ = tycker.statics.datas.lookup_or_alloc(arms_, data_);
                    Alloc::alloc(&mut tycker.statics, id_, kd)
                }
            }
            | Type::CoData(id) => {
                let arms = tycker.statics.codatas.defs[&id].clone();
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
                    .collect::<Result<im::Vector<_>>>()?;
                if unchanged {
                    *self
                } else {
                    let codata_ = CoData { arms: arms_.iter().cloned().collect() };
                    let id_ = tycker.statics.codatas.lookup_or_alloc(arms_, codata_);
                    Alloc::alloc(&mut tycker.statics, id_, kd)
                }
            }
        };
        let kd = tycker.statics.annotations_type[&ty].clone();
        let ty = ty.normalize(tycker, kd)?;
        Ok(ty)
    }
    pub fn subst_k(&self, tycker: &mut Tycker, var: DefId, with: TypeId) -> ResultKont<TypeId> {
        let res = self.subst(tycker, var, with);
        tycker.err_p_to_k(res)
    }
    pub fn subst(&self, tycker: &mut Tycker, var: DefId, with: TypeId) -> Result<TypeId> {
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
                    app.normalize_k(tycker, kd)?
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
    pub fn normalize_k(self, tycker: &mut Tycker, kd: KindId) -> ResultKont<TypeId> {
        let res = self.normalize(tycker, kd);
        tycker.err_p_to_k(res)
    }
    pub fn normalize(self, tycker: &mut Tycker, kd: KindId) -> Result<TypeId> {
        let res = match tycker.statics.types[&self].to_owned() {
            | Type::App(app) => {
                let App(ty1, ty2) = app;
                let kd2 = tycker.statics.annotations_type[&ty2].clone();
                let ty2 = ty2.normalize(tycker, kd2)?;
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
    // pub fn weak_head_normalize_k(self, tycker: &mut Tycker, kd: KindId) -> ResultKont<TypeId> {
    //     let res = match tycker.statics.types[&self].to_owned() {
    //         | Type::App(app) => {
    //             let App(ty1, ty2) = app;
    //             ty1.normalize_app_k(tycker, ty2, kd)?
    //         }
    //         | Type::Var(_)
    //         | Type::Abst(_)
    //         | Type::Fill(_)
    //         | Type::Abs(_)
    //         | Type::Thunk(_)
    //         | Type::Ret(_)
    //         | Type::Unit(_)
    //         | Type::Int(_)
    //         | Type::Char(_)
    //         | Type::String(_)
    //         | Type::OS(_)
    //         | Type::Arrow(_)
    //         | Type::Forall(_)
    //         | Type::Prod(_)
    //         | Type::Exists(_)
    //         | Type::Data(_)
    //         | Type::CoData(_) => self,
    //     };
    //     Ok(res)
    // }
    pub fn normalize_app_k(
        self, tycker: &mut Tycker, a_ty: TypeId, kd: KindId,
    ) -> ResultKont<TypeId> {
        let res = self.normalize_app(tycker, a_ty, kd);
        tycker.err_p_to_k(res)
    }
    pub fn normalize_app(self, tycker: &mut Tycker, a_ty: TypeId, kd: KindId) -> Result<TypeId> {
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

impl Tycker {
    pub fn fill_k(&mut self, fill: FillId, ann: AnnId) -> ResultKont<AnnId> {
        let res = self.fill(fill, ann);
        self.err_p_to_k(res)
    }
    pub fn fill(&mut self, fill: FillId, mut ann: AnnId) -> Result<AnnId> {
        if let Some(ann_) = self.statics.solus.insert_or_get(fill, ann) {
            ann = Lub::lub(ann, ann_, self)?;
            self.statics.solus.replace(fill, ann);
        }
        Ok(ann)
    }
}

impl TypeId {
    pub fn application_normal_form_k(&self, tycker: &mut Tycker) -> ResultKont<(TypeId, Vec<TypeId>)> {
        let res = self.application_normal_form(tycker);
        tycker.err_p_to_k(res)
    }
    pub fn application_normal_form(&self, tycker: &mut Tycker) -> Result<(TypeId, Vec<TypeId>)> {
        let ty = self.normalize(tycker, tycker.statics.annotations_type[self].to_owned())?;
        let res = match tycker.statics.types[&ty].to_owned() {
            | Type::App(app_ty) => {
                let App(f_ty, a_ty) = app_ty;
                let (f_ty, mut a_tys) = f_ty.application_normal_form(tycker)?;
                let a_ty = a_ty.normalize(tycker, tycker.statics.annotations_type[&a_ty].clone())?;
                a_tys.push(a_ty);
                (f_ty, a_tys)
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
            | Type::CoData(_) => (ty, Vec::new()),
        };
        Ok(res)
    }
}
