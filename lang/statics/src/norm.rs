use crate::{syntax::*, *};
use zydeco_utils::{arena::ArenaAccess, imc::ImmutableMonoidMap};

/* ------------------------------ Substitution ------------------------------ */

impl TypeId {
    pub fn subst_env_k(&self, tycker: &mut Tycker, env: &Env<AnnId>) -> ResultKont<TypeId> {
        let res = self.subst_env(tycker, env);
        tycker.err_p_to_k(res)
    }
    pub fn subst_env(&self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
        let kd = tycker.statics.annotations_type[self];
        let ty = tycker.statics.types[&self].to_owned();
        let ty = match ty {
            | Fillable::Fill(_) => *self,
            | Fillable::Done(ty) => match ty {
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
                | Type::Abs(abs) => {
                    let Abs(tpat, ty) = abs;
                    let (def, _) = tpat.try_destruct_def(tycker);
                    if let Some(def) = def {
                        if let Some(_with) = env.get(&def) {
                            unreachable!()
                        }
                    }
                    let ty_ = ty.subst_env(tycker, env)?;
                    if ty == ty_ { *self } else { Alloc::alloc(tycker, Abs(tpat, ty_), kd) }
                }
                | Type::App(app) => {
                    let App(ty1, ty2) = app;
                    let ty1_ = ty1.subst_env(tycker, env)?;
                    let ty2_ = ty2.subst_env(tycker, env)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, App(ty1_, ty2_), kd)
                    }
                }
                | Type::Thk(_)
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
                        Alloc::alloc(tycker, Arrow(ty1_, ty2_), kd)
                    }
                }
                | Type::Forall(forall) => {
                    let Forall(tpat, ty) = forall;
                    let ty_ = ty.subst_env(tycker, env)?;
                    if ty == ty_ { *self } else { Alloc::alloc(tycker, Forall(tpat, ty_), kd) }
                }
                | Type::Prod(prod) => {
                    let Prod(ty1, ty2) = prod;
                    let ty1_ = ty1.subst_env(tycker, env)?;
                    let ty2_ = ty2.subst_env(tycker, env)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Prod(ty1_, ty2_), kd)
                    }
                }
                | Type::Exists(exists) => {
                    let Exists(tpat, ty) = exists;
                    let ty_ = ty.subst_env(tycker, env)?;
                    if ty == ty_ { *self } else { Alloc::alloc(tycker, Exists(tpat, ty_), kd) }
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
                        let data_ = Data::new(arms_.iter().cloned());
                        let id_ = tycker.statics.datas.lookup_or_alloc(arms_, data_);
                        Alloc::alloc(tycker, id_, kd)
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
                        let codata_ = CoData::new(arms_.iter().cloned());
                        let id_ = tycker.statics.codatas.lookup_or_alloc(arms_, codata_);
                        Alloc::alloc(tycker, id_, kd)
                    }
                }
            },
        };
        let kd = tycker.statics.annotations_type[&ty];
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
    pub fn subst_abst_k(
        &self, tycker: &mut Tycker, assign: (AbstId, TypeId),
    ) -> ResultKont<TypeId> {
        let res = self.subst_abst(tycker, assign);
        tycker.err_p_to_k(res)
    }
    pub fn subst_abst(&self, tycker: &mut Tycker, assign: (AbstId, TypeId)) -> Result<TypeId> {
        let kd = tycker.statics.annotations_type[self];
        let ty = match tycker.statics.types[self].to_owned() {
            // Todo: add subst obligation to fills
            | Fillable::Fill(_) => *self,
            | Fillable::Done(ty) => match ty {
                | Type::Var(_) => *self,
                | Type::Abst(abst) => {
                    let (abst_, with) = assign;
                    if abst == abst_ { with } else { *self }
                }
                | Type::Abs(abs) => {
                    let Abs(tpat, ty) = abs;
                    let ty_ = ty.subst_abst(tycker, assign)?;
                    if ty == ty_ { *self } else { Alloc::alloc(tycker, Abs(tpat, ty_), kd) }
                }
                | Type::App(app) => {
                    let App(ty1, ty2) = app;
                    let ty1_ = ty1.subst_abst(tycker, assign)?;
                    let ty2_ = ty2.subst_abst(tycker, assign)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, App(ty1_, ty2_), kd)
                    }
                }
                | Type::Thk(_)
                | Type::Ret(_)
                | Type::Unit(_)
                | Type::Int(_)
                | Type::Char(_)
                | Type::String(_)
                | Type::OS(_) => *self,
                | Type::Arrow(arr) => {
                    let Arrow(ty1, ty2) = arr;
                    let ty1_ = ty1.subst_abst(tycker, assign)?;
                    let ty2_ = ty2.subst_abst(tycker, assign)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Arrow(ty1_, ty2_), kd)
                    }
                }
                | Type::Forall(forall) => {
                    let Forall(tpat, ty) = forall;
                    let ty_ = ty.subst_abst(tycker, assign)?;
                    if ty == ty_ { *self } else { Alloc::alloc(tycker, Forall(tpat, ty_), kd) }
                }
                | Type::Prod(prod) => {
                    let Prod(ty1, ty2) = prod;
                    let ty1_ = ty1.subst_abst(tycker, assign)?;
                    let ty2_ = ty2.subst_abst(tycker, assign)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Prod(ty1_, ty2_), kd)
                    }
                }
                | Type::Exists(exists) => {
                    let Exists(tpat, ty) = exists;
                    let ty_ = ty.subst_abst(tycker, assign)?;
                    if ty == ty_ { *self } else { Alloc::alloc(tycker, Exists(tpat, ty_), kd) }
                }
                | Type::Data(id) => {
                    let arms = tycker.statics.datas.defs[&id].clone();
                    let mut unchanged = true;
                    let arms_ = arms
                        .into_iter()
                        .map(|(ctor, ty)| {
                            let ty_ = ty.subst_abst(tycker, assign)?;
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
                        let data_ = Data::new(arms_.iter().cloned());
                        let id_ = tycker.statics.datas.lookup_or_alloc(arms_, data_);
                        Alloc::alloc(tycker, id_, kd)
                    }
                }
                | Type::CoData(id) => {
                    let arms = tycker.statics.codatas.defs[&id].clone();
                    let mut unchanged = true;
                    let arms_ = arms
                        .into_iter()
                        .map(|(dtor, ty)| {
                            let ty_ = ty.subst_abst(tycker, assign)?;
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
                        let codata_ = CoData::new(arms_.iter().cloned());
                        let id_ = tycker.statics.codatas.lookup_or_alloc(arms_, codata_);
                        Alloc::alloc(tycker, id_, kd)
                    }
                }
            },
        };
        let kd = tycker.statics.annotations_type[&ty];
        let ty = ty.normalize(tycker, kd)?;
        Ok(ty)
    }
}

/* --------------------------- Unroll Sealed Types -------------------------- */

impl TypeId {
    pub fn unroll(self, tycker: &mut Tycker) -> ResultKont<TypeId> {
        let kd = tycker.statics.annotations_type[&self];
        let res = match tycker.type_filled_k(&self)?.to_owned() {
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
                    let app = Alloc::alloc(tycker, App(ty1_, ty2), kd);
                    app.normalize_k(tycker, kd)?
                }
            }
            // Todo: figure out if this is correct
            // | Type::Fill(_) // unchanged because terms with unfilled types can't be matched against
            | Type::Var(_) // unchanged because type-variable-typed terms are abstract
            | Type::Abs(_) // unchanged because type-abstration-typed terms are ill-formed
            | Type::Thk(_)
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

/* ------------------------------ Normalization ----------------------------- */

impl TypeId {
    pub fn normalize_k(self, tycker: &mut Tycker, kd: KindId) -> ResultKont<TypeId> {
        let res = self.normalize(tycker, kd);
        tycker.err_p_to_k(res)
    }
    pub fn normalize(self, tycker: &mut Tycker, kd: KindId) -> Result<TypeId> {
        let res = match tycker.statics.types[&self].to_owned() {
            | Fillable::Fill(_) => self,
            | Fillable::Done(ty) => match ty {
                | Type::App(app) => {
                    let App(ty1, ty2) = app;
                    let kd2 = tycker.statics.annotations_type[&ty2];
                    let ty2 = ty2.normalize(tycker, kd2)?;
                    ty1.normalize_app(tycker, ty2, kd)?
                }
                // weak head normalization (?)
                // | Type::App(app) => {
                //     let App(ty1, ty2) = app;
                //     ty1.normalize_app_k(tycker, ty2, kd)?
                // }
                | Type::Var(_)
                | Type::Abst(_)
                | Type::Abs(_)
                | Type::Thk(_)
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
            },
        };
        Ok(res)
    }
    pub fn normalize_app_k(
        self, tycker: &mut Tycker, a_ty: TypeId, kd: KindId,
    ) -> ResultKont<TypeId> {
        let res = self.normalize_app(tycker, a_ty, kd);
        tycker.err_p_to_k(res)
    }
    pub fn normalize_app(self, tycker: &mut Tycker, a_ty: TypeId, kd: KindId) -> Result<TypeId> {
        let res = match tycker.statics.types[&self].to_owned() {
            | Fillable::Fill(_) => self,
            | Fillable::Done(ty) => match ty {
                | Type::Abs(abs) => {
                    // if f_ty is an abstraction, apply it
                    let Abs(binder, body_ty) = abs;
                    let (def, _) = binder.try_destruct_def(tycker);
                    let body_ty_subst = if let Some(def) = def {
                        body_ty.subst(tycker, def, a_ty)?
                    } else {
                        body_ty
                    };
                    body_ty_subst
                }
                | _ => {
                    // else, the app is already normalized
                    Alloc::alloc(tycker, App(self, a_ty), kd)
                }
            },
        };
        Ok(res)
    }
    pub fn normalize_apps_k(self, tycker: &mut Tycker, a_tys: Vec<TypeId>) -> ResultKont<TypeId> {
        let res = self.normalize_apps(tycker, a_tys);
        tycker.err_p_to_k(res)
    }
    pub fn normalize_apps(self, tycker: &mut Tycker, a_tys: Vec<TypeId>) -> Result<TypeId> {
        let res = a_tys.into_iter().try_fold(self, |f_ty, a_ty| {
            let abs_kd = tycker.statics.annotations_type[&f_ty];
            let kd = match tycker.kind_filled(&abs_kd)?.to_owned() {
                | Kind::Arrow(Arrow(arg_kd, body_kd)) => {
                    let arg_kd_ = tycker.statics.annotations_type[&a_ty];
                    Lub::lub(arg_kd_, arg_kd, tycker)?;
                    body_kd
                }
                | _ => tycker.err(TyckError::KindMismatch, std::panic::Location::caller())?,
            };
            f_ty.normalize_app(tycker, a_ty, kd)
        })?;
        Ok(res)
    }
}

/* ------------------------- Hole Filling & Solution ------------------------ */

impl FillId {
    pub fn fill_k(&self, tycker: &mut Tycker, ann: AnnId) -> ResultKont<AnnId> {
        let res = self.fill(tycker, ann);
        tycker.err_p_to_k(res)
    }
    pub fn fill(&self, tycker: &mut Tycker, mut ann: AnnId) -> Result<AnnId> {
        if let Some(ann_) = tycker.statics.solus.insert_or_get(*self, ann) {
            ann = Lub::lub(ann, ann_, tycker)?;
            tycker.statics.solus.replace(*self, ann);
        }
        Ok(ann)
    }
}

impl TypeId {
    pub fn solution_k(&self, tycker: &mut Tycker) -> ResultKont<(TypeId, Vec<FillId>)> {
        let res = self.solution(tycker);
        tycker.err_p_to_k(res)
    }
    /// Solve unfilled types as much as possible; returns the final type and the unfilled holes
    pub fn solution(&self, tycker: &mut Tycker) -> Result<(TypeId, Vec<FillId>)> {
        let mut res = *self;
        let mut fills = Vec::new();
        while let Fillable::Fill(fill) = tycker.statics.types[&res].to_owned() {
            let solu = match tycker.statics.solus.get(&fill).cloned() {
                | None => break,
                | Some(AnnId::Type(ty)) => ty,
                | Some(AnnId::Set | AnnId::Kind(_)) => {
                    tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                }
            };
            res = solu;
        }
        let res = match tycker.statics.types[&res].to_owned() {
            | Fillable::Fill(fill) => {
                fills.push(fill);
                res
            }
            | Fillable::Done(ty) => match ty {
                | Type::Var(_) | Type::Abst(_) => res,
                | Type::Abs(ty) => {
                    let Abs(tpat, ty) = ty;
                    let tpat_ = tpat;
                    let (ty_, fills_) = ty.solution(tycker)?;
                    fills.extend(fills_);
                    if ty == ty_ {
                        res
                    } else {
                        Alloc::alloc(tycker, Abs(tpat_, ty_), tycker.statics.annotations_type[&res])
                    }
                }
                | Type::App(ty) => {
                    let App(f_ty, a_ty) = ty;
                    let (f_ty_, fills_) = f_ty.solution(tycker)?;
                    fills.extend(fills_);
                    let (a_ty_, fills_) = a_ty.solution(tycker)?;
                    fills.extend(fills_);
                    if f_ty == f_ty_ && a_ty == a_ty_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            App(f_ty_, a_ty_),
                            tycker.statics.annotations_type[&res],
                        )
                    }
                }
                | Type::Thk(_)
                | Type::Ret(_)
                | Type::Unit(_)
                | Type::Int(_)
                | Type::Char(_)
                | Type::String(_)
                | Type::OS(_) => res,
                | Type::Arrow(ty) => {
                    let Arrow(ty1, ty2) = ty;
                    let (ty1_, fills_) = ty1.solution(tycker)?;
                    fills.extend(fills_);
                    let (ty2_, fills_) = ty2.solution(tycker)?;
                    fills.extend(fills_);
                    if ty1 == ty1_ && ty2 == ty2_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            Arrow(ty1_, ty2_),
                            tycker.statics.annotations_type[&res],
                        )
                    }
                }
                | Type::Forall(ty) => {
                    let Forall(tpat, ty) = ty;
                    let tpat_ = tpat;
                    let (ty_, fills_) = ty.solution(tycker)?;
                    fills.extend(fills_);
                    if ty == ty_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            Forall(tpat_, ty_),
                            tycker.statics.annotations_type[&res],
                        )
                    }
                }
                | Type::Prod(ty) => {
                    let Prod(ty1, ty2) = ty;
                    let (ty1_, fills_) = ty1.solution(tycker)?;
                    fills.extend(fills_);
                    let (ty2_, fills_) = ty2.solution(tycker)?;
                    fills.extend(fills_);
                    if ty1 == ty1_ && ty2 == ty2_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            Prod(ty1_, ty2_),
                            tycker.statics.annotations_type[&res],
                        )
                    }
                }
                | Type::Exists(ty) => {
                    let Exists(tpat, ty) = ty;
                    let tpat_ = tpat;
                    let (ty_, fills_) = ty.solution(tycker)?;
                    fills.extend(fills_);
                    if ty == ty_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            Exists(tpat_, ty_),
                            tycker.statics.annotations_type[&res],
                        )
                    }
                }
                | Type::Data(data) => {
                    let arms = tycker.statics.datas.defs[&data].clone();
                    let mut unchanged = true;
                    let arms_ = arms
                        .into_iter()
                        .map(|(ctor, ty)| {
                            let (ty_, fills_) = ty.solution(tycker)?;
                            fills.extend(fills_);
                            if ty == ty_ {
                                Ok((ctor, ty))
                            } else {
                                unchanged = false;
                                Ok((ctor, ty_))
                            }
                        })
                        .collect::<Result<im::Vector<_>>>()?;
                    if unchanged {
                        res
                    } else {
                        let data_ = Data::new(arms_.iter().cloned());
                        let data = tycker.statics.datas.lookup_or_alloc(arms_, data_);
                        Alloc::alloc(tycker, data, tycker.statics.annotations_type[&res])
                    }
                }
                | Type::CoData(codata) => {
                    let arms = tycker.statics.codatas.defs[&codata].clone();
                    let mut unchanged = true;
                    let arms_ = arms
                        .into_iter()
                        .map(|(dtor, ty)| {
                            let (ty_, fills_) = ty.solution(tycker)?;
                            fills.extend(fills_);
                            if ty == ty_ {
                                Ok((dtor, ty))
                            } else {
                                unchanged = false;
                                Ok((dtor, ty_))
                            }
                        })
                        .collect::<Result<im::Vector<_>>>()?;
                    if unchanged {
                        res
                    } else {
                        let codata_ = CoData::new(arms_.iter().cloned());
                        let codata = tycker.statics.codatas.lookup_or_alloc(arms_, codata_);
                        Alloc::alloc(tycker, codata, tycker.statics.annotations_type[&res])
                    }
                }
            },
        };
        Ok((res, fills))
    }
}

impl Tycker {
    pub fn kind_filled_k(&mut self, id: &KindId) -> ResultKont<Kind> {
        let res = self.kind_filled(id);
        self.err_p_to_k(res)
    }
    /// internally resolves unfilled kinds; fails if the kind has no solution
    pub fn kind_filled(&self, id: &KindId) -> Result<Kind> {
        match self.statics.kinds[id].to_owned() {
            | Fillable::Fill(fill) => match self.statics.solus.get(&fill) {
                | Some(AnnId::Kind(kind)) => self.kind_filled(kind),
                | Some(_) => self.err(TyckError::SortMismatch, std::panic::Location::caller())?,
                | None => self
                    .err(TyckError::MissingSolution(vec![fill]), std::panic::Location::caller())?,
            },
            | Fillable::Done(kind) => Ok(kind),
        }
    }

    pub fn type_filled_k(&mut self, id: &TypeId) -> ResultKont<Type> {
        let res = self.type_filled(id);
        self.err_p_to_k(res)
    }
    pub fn type_filled(&self, id: &TypeId) -> Result<Type> {
        match self.statics.types[id].to_owned() {
            | Fillable::Fill(fill) => match self.statics.solus.get(&fill) {
                | Some(AnnId::Type(ty)) => self.type_filled(ty),
                | Some(_) => self.err(TyckError::SortMismatch, std::panic::Location::caller())?,
                | None => self
                    .err(TyckError::MissingSolution(vec![fill]), std::panic::Location::caller())?,
            },
            | Fillable::Done(ty) => Ok(ty),
        }
    }
}
