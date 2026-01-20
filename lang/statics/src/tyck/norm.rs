use super::{syntax::*, *};
use zydeco_utils::arena::ArenaAccess;

/* ------------------------------ Substitution ------------------------------ */

impl TypeId {
    pub fn subst_env_k(&self, tycker: &mut Tycker<'_>, env: &TyEnv) -> ResultKont<TypeId> {
        let res = self.subst_env(tycker, env);
        tycker.err_p_to_k(res)
    }
    pub fn subst_env(&self, tycker: &mut Tycker<'_>, env: &TyEnv) -> Result<TypeId> {
        let kd = tycker.statics.annotations_type[self];
        let ty = tycker.statics.types_pre[&self].to_owned();
        let ty = match ty {
            // Fixme: should invoke substitution once the type is filled
            | Fillable::Fill(_) => *self,
            | Fillable::Done(ty) => match ty {
                | Type::Var(def) => match env.get(&def) {
                    | Some(ann) => match ann {
                        | AnnId::Set | AnnId::Kind(_) => {
                            tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                        | AnnId::Type(with) => {
                            // // Debug: print
                            // {
                            //     println!("[var] {} -> {}", tycker.dump_statics(def), tycker.dump_statics(with));
                            // }
                            *with
                        }
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
                    if ty == ty_ { *self } else { Alloc::alloc(tycker, Abs(tpat, ty_), kd, env) }
                }
                | Type::App(app) => {
                    let App(ty1, ty2) = app;
                    let ty1_ = ty1.subst_env(tycker, env)?;
                    let ty2_ = ty2.subst_env(tycker, env)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, App(ty1_, ty2_), kd, env)
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
                        Alloc::alloc(tycker, Arrow(ty1_, ty2_), kd, env)
                    }
                }
                | Type::Forall(forall) => {
                    let Forall(tpat, ty) = forall;
                    let ty_ = ty.subst_env(tycker, env)?;
                    if ty == ty_ { *self } else { Alloc::alloc(tycker, Forall(tpat, ty_), kd, env) }
                }
                | Type::Prod(prod) => {
                    let Prod(ty1, ty2) = prod;
                    let ty1_ = ty1.subst_env(tycker, env)?;
                    let ty2_ = ty2.subst_env(tycker, env)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Prod(ty1_, ty2_), kd, env)
                    }
                }
                | Type::Exists(exists) => {
                    let Exists(tpat, ty) = exists;
                    let ty_ = ty.subst_env(tycker, env)?;
                    if ty == ty_ { *self } else { Alloc::alloc(tycker, Exists(tpat, ty_), kd, env) }
                }
                | Type::Data(id) => {
                    // // Debug: print
                    // {
                    //     println!("{}", ">".repeat(20));
                    //     println!("target: {}", tycker.dump_statics(id));
                    //     println!("{}", "=".repeat(20));
                    //     for (def, ty) in env.iter() {
                    //         println!("{} := {}", tycker.dump_statics(def), tycker.dump_statics(ty));
                    //     }
                    //     println!("{}", "-".repeat(20));
                    // }
                    let arms = tycker.statics.datas[&id].clone();
                    // // Debug: print
                    // {
                    //     for (ctor, ty) in arms.iter() {
                    //         println!("{} : {}", tycker.dump_statics(ctor), tycker.dump_statics(ty));
                    //     }
                    // }
                    // let mut unchanged = true;
                    let arms_ = arms
                        .into_iter()
                        .map(|(ctor, ty)| {
                            let ty_ = ty.subst_env(tycker, env)?;
                            // if ty == ty_ {
                            //     Ok((ctor, ty))
                            // } else {
                            //     unchanged = false;
                            //     Ok((ctor, ty_))
                            // }
                            Ok((ctor, ty_))
                        })
                        .collect::<Result<im::Vector<_>>>()?;
                    // if unchanged {
                    //     *self
                    // } else
                    {
                        // // Debug: print
                        // {
                        //     for (ctor, ty_) in arms_.iter() {
                        //         println!("{}", "-".repeat(20));
                        //         println!(
                        //             "{} : {}",
                        //             tycker.dump_statics(ctor),
                        //             tycker.dump_statics(ty_)
                        //         );
                        //     }
                        // }
                        let id_ = tycker.statics.datas.alloc(Data::new(arms_));
                        // // Debug: print
                        // {
                        //     println!("{}", "-".repeat(20));
                        //     println!("{}", tycker.dump_statics(id_));
                        //     println!("{}", "<".repeat(20));
                        // }
                        Alloc::alloc(tycker, id_, kd, env)
                    }
                }
                | Type::CoData(id) => {
                    let arms = tycker.statics.codatas[&id].clone();
                    // let mut unchanged = true;
                    let arms_ = arms
                        .into_iter()
                        .map(|(dtor, ty)| {
                            let ty_ = ty.subst_env(tycker, env)?;
                            // if ty == ty_ {
                            //     Ok((dtor, ty))
                            // } else {
                            //     unchanged = false;
                            //     Ok((dtor, ty_))
                            // }
                            Ok((dtor, ty_))
                        })
                        .collect::<Result<im::Vector<_>>>()?;
                    // if unchanged {
                    //     *self
                    // } else
                    {
                        let id_ = tycker.statics.codatas.alloc(CoData::new(arms_));
                        Alloc::alloc(tycker, id_, kd, env)
                    }
                }
            },
        };
        let kd = tycker.statics.annotations_type[&ty];
        let ty = ty.normalize(tycker, kd)?;
        Ok(ty)
    }
    pub fn subst_k(&self, tycker: &mut Tycker<'_>, var: DefId, with: TypeId) -> ResultKont<TypeId> {
        let res = self.subst(tycker, var, with);
        tycker.err_p_to_k(res)
    }
    pub fn subst(&self, tycker: &mut Tycker<'_>, var: DefId, with: TypeId) -> Result<TypeId> {
        self.subst_env(tycker, &TyEnv::from_iter([(var, with.into())]))
    }
}

impl TypeId {
    pub fn subst_abst_k(
        &self, tycker: &mut Tycker<'_>, assign: (AbstId, TypeId),
    ) -> ResultKont<TypeId> {
        let res = self.subst_abst(tycker, assign);
        tycker.err_p_to_k(res)
    }
    pub fn subst_abst(&self, tycker: &mut Tycker<'_>, assign: (AbstId, TypeId)) -> Result<TypeId> {
        let kd = tycker.statics.annotations_type[self];
        let env = tycker.statics.env_type[self].clone();
        let ty = match tycker.statics.types_pre[self].to_owned() {
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
                    if ty == ty_ { *self } else { Alloc::alloc(tycker, Abs(tpat, ty_), kd, &env) }
                }
                | Type::App(app) => {
                    let App(ty1, ty2) = app;
                    let ty1_ = ty1.subst_abst(tycker, assign)?;
                    let ty2_ = ty2.subst_abst(tycker, assign)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, App(ty1_, ty2_), kd, &env)
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
                        Alloc::alloc(tycker, Arrow(ty1_, ty2_), kd, &env)
                    }
                }
                | Type::Forall(forall) => {
                    let Forall(tpat, ty) = forall;
                    let ty_ = ty.subst_abst(tycker, assign)?;
                    if ty == ty_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Forall(tpat, ty_), kd, &env)
                    }
                }
                | Type::Prod(prod) => {
                    let Prod(ty1, ty2) = prod;
                    let ty1_ = ty1.subst_abst(tycker, assign)?;
                    let ty2_ = ty2.subst_abst(tycker, assign)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Prod(ty1_, ty2_), kd, &env)
                    }
                }
                | Type::Exists(exists) => {
                    let Exists(tpat, ty) = exists;
                    let ty_ = ty.subst_abst(tycker, assign)?;
                    if ty == ty_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Exists(tpat, ty_), kd, &env)
                    }
                }
                | Type::Data(id) => {
                    let arms = tycker.statics.datas[&id].clone();
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
                        let id_ = tycker.statics.datas.alloc(Data::new(arms_));
                        Alloc::alloc(tycker, id_, kd, &env)
                    }
                }
                | Type::CoData(id) => {
                    let arms = tycker.statics.codatas[&id].clone();
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
                        let id_ = tycker.statics.codatas.alloc(CoData::new(arms_));
                        Alloc::alloc(tycker, id_, kd, &env)
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
    pub fn unroll_k(self, tycker: &mut Tycker<'_>) -> ResultKont<TypeId> {
        let res = self.unroll(tycker);
        tycker.err_p_to_k(res)
    }
    pub fn unroll(self, tycker: &mut Tycker<'_>) -> Result<TypeId> {
        let kd = tycker.statics.annotations_type[&self];
        let env = tycker.statics.env_type[&self].clone();
        let res = match tycker.type_filled(&self)?.to_owned() {
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
                    let app = Alloc::alloc(tycker, App(ty1_, ty2), kd, &env);
                    app.normalize(tycker, kd)?
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
    pub fn normalize_k(self, tycker: &mut Tycker<'_>, kd: KindId) -> ResultKont<TypeId> {
        let res = self.normalize(tycker, kd);
        tycker.err_p_to_k(res)
    }
    pub fn normalize(self, tycker: &mut Tycker<'_>, kd: KindId) -> Result<TypeId> {
        let res = match tycker.statics.types_pre[&self].to_owned() {
            | Fillable::Fill(_) => self,
            | Fillable::Done(ty) => match ty {
                | Type::App(app) => {
                    let App(ty1, ty2) = app;
                    let kd2 = tycker.statics.annotations_type[&ty2];
                    let ty2 = ty2.normalize(tycker, kd2)?;
                    ty1.normalize_app(tycker, ty2, kd)?
                }
                // | Type::App(app) => {
                //     let App(ty1, ty2) = app;
                //     let kd2 = tycker.statics.annotations_type[&ty2];
                //     let ty2 = ty2.normalize(tycker, kd2)?;
                //     ty1.normalize_app(tycker, ty2, kd)?
                // }
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
        self, tycker: &mut Tycker<'_>, a_ty: TypeId, kd: KindId,
    ) -> ResultKont<TypeId> {
        let res = self.normalize_app(tycker, a_ty, kd);
        tycker.err_p_to_k(res)
    }
    pub fn normalize_app(
        self, tycker: &mut Tycker<'_>, a_ty: TypeId, kd: KindId,
    ) -> Result<TypeId> {
        let env = tycker.statics.env_type[&self].clone();
        let res = match tycker.statics.types_pre[&self].to_owned() {
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
                    Alloc::alloc(tycker, App(self, a_ty), kd, &env)
                }
            },
        };
        Ok(res)
    }
    pub fn normalize_apps_k(
        self, tycker: &mut Tycker<'_>, a_tys: Vec<TypeId>,
    ) -> ResultKont<TypeId> {
        let res = self.normalize_apps(tycker, a_tys);
        tycker.err_p_to_k(res)
    }
    pub fn normalize_apps(self, tycker: &mut Tycker<'_>, a_tys: Vec<TypeId>) -> Result<TypeId> {
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
    pub fn fill_k(&self, tycker: &mut Tycker<'_>, ann: AnnId) -> ResultKont<AnnId> {
        let res = self.fill(tycker, ann);
        tycker.err_p_to_k(res)
    }
    pub fn fill(&self, tycker: &mut Tycker<'_>, mut ann: AnnId) -> Result<AnnId> {
        if let Some(ann_) = tycker.statics.solus.insert_or_get(*self, ann) {
            ann = Lub::lub(ann, ann_, tycker)?;
            tycker.statics.solus.replace(*self, ann);
        }
        Ok(ann)
    }
}

impl TypeId {
    pub fn solution_k(&self, tycker: &mut Tycker<'_>) -> ResultKont<(TypeId, Vec<FillId>)> {
        let res = self.solution(tycker);
        tycker.err_p_to_k(res)
    }
    /// Solve unfilled types as much as possible; returns the final type and the unfilled holes
    pub fn solution(&self, tycker: &mut Tycker<'_>) -> Result<(TypeId, Vec<FillId>)> {
        let mut res = *self;
        let mut fills = Vec::new();
        // recursively lookup unfilled types as much as possible
        while let Fillable::Fill(fill) = tycker.statics.types_pre[&res].to_owned() {
            let solu = match tycker.statics.solus.get(&fill).cloned() {
                | None => break,
                | Some(AnnId::Type(ty)) => ty,
                | Some(AnnId::Set | AnnId::Kind(_)) => {
                    tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                }
            };
            res = solu;
        }
        let env = tycker.statics.env_type[&res].clone();
        let res = match tycker.statics.types_pre[&res].to_owned() {
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
                        Alloc::alloc(
                            tycker,
                            Abs(tpat_, ty_),
                            tycker.statics.annotations_type[&res],
                            &env,
                        )
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
                            &env,
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
                            &env,
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
                            &env,
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
                            &env,
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
                            &env,
                        )
                    }
                }
                | Type::Data(data) => {
                    let arms = tycker.statics.datas[&data].clone();
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
                        let data = tycker.statics.datas.alloc(Data::new(arms_));
                        Alloc::alloc(tycker, data, tycker.statics.annotations_type[&res], &env)
                    }
                }
                | Type::CoData(codata) => {
                    let arms = tycker.statics.codatas[&codata].clone();
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
                        let codata = tycker.statics.codatas.alloc(CoData::new(arms_));
                        Alloc::alloc(tycker, codata, tycker.statics.annotations_type[&res], &env)
                    }
                }
            },
        };
        Ok((res, fills))
    }
}

impl<'a> Tycker<'a> {
    pub fn filling_k<R>(
        &mut self, id: &AnnId, f_set: impl FnOnce(&mut Tycker<'a>) -> Result<R>,
        f_kind: impl FnOnce(&mut Tycker<'a>, Kind) -> Result<R>,
        f_type: impl FnOnce(&mut Tycker<'a>, Type) -> Result<R>,
        f_fill: impl FnOnce(&mut Tycker<'a>, FillId) -> Result<R>,
    ) -> ResultKont<R> {
        let res = self.filling(id, f_set, f_kind, f_type, f_fill);
        self.err_p_to_k(res)
    }
    /// internally resolves unfilled annotations; fails if the annotation has no solution.
    /// only fills the uppermost (or head?) annotation
    pub fn filling<R>(
        &mut self, id: &AnnId, f_set: impl FnOnce(&mut Tycker<'a>) -> Result<R>,
        f_kind: impl FnOnce(&mut Tycker<'a>, Kind) -> Result<R>,
        f_type: impl FnOnce(&mut Tycker<'a>, Type) -> Result<R>,
        f_fill: impl FnOnce(&mut Tycker<'a>, FillId) -> Result<R>,
    ) -> Result<R> {
        match id {
            | AnnId::Set => f_set(self),
            | AnnId::Kind(id) => match self.statics.kinds_pre[id].to_owned() {
                | Fillable::Fill(fill) => match self.statics.solus.get(&fill).cloned() {
                    | Some(AnnId::Kind(kind)) => {
                        self.filling(&kind.into(), f_set, f_kind, f_type, f_fill)
                    }
                    | Some(_) => {
                        self.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | None => f_fill(self, fill),
                },
                | Fillable::Done(kind) => f_kind(self, kind),
            },
            | AnnId::Type(id) => match self.statics.types_pre[id].to_owned() {
                | Fillable::Fill(fill) => match self.statics.solus.get(&fill).cloned() {
                    | Some(AnnId::Type(ty)) => {
                        self.filling(&ty.into(), f_set, f_kind, f_type, f_fill)
                    }
                    | Some(_) => {
                        self.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | None => f_fill(self, fill),
                },
                | Fillable::Done(ty) => f_type(self, ty),
            },
        }
    }

    pub fn kind_filled_k(&mut self, id: &KindId) -> ResultKont<Kind> {
        let res = self.kind_filled(id);
        self.err_p_to_k(res)
    }
    /// internally resolves unfilled kinds; fails if the kind has no solution.
    /// only fills the uppermost (or head?) kind
    pub fn kind_filled(&mut self, id: &KindId) -> Result<Kind> {
        self.filling(
            &id.to_owned().into(),
            |_tycker| unreachable!(),
            |_tycker, kd| Ok(kd),
            |_tycker, _ty| unreachable!(),
            |tycker, fill| {
                tycker.err(TyckError::MissingSolution(vec![fill]), std::panic::Location::caller())
            },
        )
    }

    pub fn type_filled_k(&mut self, id: &TypeId) -> ResultKont<Type> {
        let res = self.type_filled(id);
        self.err_p_to_k(res)
    }
    /// internally resolves unfilled types; fails if the type has no solution.
    /// only fills the uppermost (or head?) type
    pub fn type_filled(&mut self, id: &TypeId) -> Result<Type> {
        self.filling(
            &id.to_owned().into(),
            |_tycker| unreachable!(),
            |_tycker, _kd| unreachable!(),
            |_tycker, ty| Ok(ty),
            |tycker, fill| {
                tycker.err(TyckError::MissingSolution(vec![fill]), std::panic::Location::caller())
            },
        )
    }
}

/* ------------------------------ Normalization ----------------------------- */

impl KindId {
    pub fn do_normalize_filled_k(self, tycker: &mut Tycker<'_>) -> ResultKont<()> {
        let res = self.do_normalize_filled(tycker);
        tycker.err_p_to_k(res)
    }
    pub fn do_normalize_filled(self, tycker: &mut Tycker<'_>) -> Result<()> {
        let mut memo = std::collections::HashMap::new();
        let _ = self.filled_norm_id(tycker, &mut memo)?;
        Ok(())
    }
    fn filled_norm_id(
        self, tycker: &mut Tycker<'_>, memo: &mut std::collections::HashMap<KindId, KindId>,
    ) -> Result<KindId> {
        if let Some(norm) = memo.get(&self).cloned() {
            return Ok(norm);
        }
        let res = match tycker.statics.kinds_pre[&self].to_owned() {
            | Fillable::Fill(fill) => match tycker.statics.solus.get(&fill).cloned() {
                | Some(AnnId::Kind(kd)) => kd.filled_norm_id(tycker, memo)?,
                | Some(AnnId::Set | AnnId::Type(_)) => {
                    let _: ResultKont<()> =
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller());
                    self
                }
                | None => {
                    let _: ResultKont<()> = tycker.err_k(
                        TyckError::MissingSolution(vec![fill]),
                        std::panic::Location::caller(),
                    );
                    self
                }
            },
            | Fillable::Done(kind) => match kind {
                | Kind::VType(VType) | Kind::CType(CType) => self,
                | Kind::Arrow(Arrow(from, to)) => {
                    let from_norm = from.filled_norm_id(tycker, memo)?;
                    let to_norm = to.filled_norm_id(tycker, memo)?;
                    if from_norm == from && to_norm == to {
                        self
                    } else {
                        Alloc::alloc(tycker, Arrow(from_norm, to_norm), (), &())
                    }
                }
            },
        };
        memo.insert(self, res);
        memo.insert(res, res);
        if let Fillable::Done(kind) = tycker.statics.kinds_pre[&res].to_owned() {
            tycker.statics.kinds_normalized.insert(self, kind.clone());
            tycker.statics.kinds_normalized.insert(res, kind);
        }
        Ok(res)
    }
}

impl TypeId {
    pub fn do_normalize_filled_k(self, tycker: &mut Tycker<'_>) -> ResultKont<()> {
        let res = self.do_normalize_filled(tycker);
        tycker.err_p_to_k(res)
    }
    pub fn do_normalize_filled(self, tycker: &mut Tycker<'_>) -> Result<()> {
        let mut memo = std::collections::HashMap::new();
        let mut memo_kd = std::collections::HashMap::new();
        let _ = self.filled_norm_id(tycker, &mut memo, &mut memo_kd)?;
        Ok(())
    }
    fn filled_norm_id(
        self, tycker: &mut Tycker<'_>, memo: &mut std::collections::HashMap<TypeId, TypeId>,
        memo_kd: &mut std::collections::HashMap<KindId, KindId>,
    ) -> Result<TypeId> {
        if let Some(norm) = memo.get(&self).cloned() {
            return Ok(norm);
        }
        let kd = tycker.statics.annotations_type[&self];
        let kd_norm = kd.filled_norm_id(tycker, memo_kd)?;
        let env = tycker.statics.env_type[&self].clone();
        let res = match tycker.statics.types_pre[&self].to_owned() {
            | Fillable::Fill(fill) => match tycker.statics.solus.get(&fill).cloned() {
                | Some(AnnId::Type(ty)) => ty.filled_norm_id(tycker, memo, memo_kd)?,
                | Some(AnnId::Set | AnnId::Kind(_)) => {
                    let _: ResultKont<()> =
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller());
                    self
                }
                | None => {
                    let _: ResultKont<()> = tycker.err_k(
                        TyckError::MissingSolution(vec![fill]),
                        std::panic::Location::caller(),
                    );
                    self
                }
            },
            | Fillable::Done(ty) => match ty {
                | Type::Var(def) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, def, kd_norm, &env)
                    }
                }
                | Type::Abst(abst) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, abst, kd_norm, &env)
                    }
                }
                | Type::Abs(abs) => {
                    let Abs(tpat, body) = abs;
                    let body_norm = body.filled_norm_id(tycker, memo, memo_kd)?;
                    if body_norm == body && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, Abs(tpat, body_norm), kd_norm, &env)
                    }
                }
                | Type::App(app) => {
                    let App(f_ty, a_ty) = app;
                    let f_norm = f_ty.filled_norm_id(tycker, memo, memo_kd)?;
                    let a_norm = a_ty.filled_norm_id(tycker, memo, memo_kd)?;
                    match tycker.statics.types_pre[&f_norm].to_owned() {
                        | Fillable::Done(Type::Abs(abs)) => {
                            let Abs(tpat, body) = abs;
                            let (def, _) = tpat.try_destruct_def(tycker);
                            let body_subst = if let Some(def) = def {
                                body.subst(tycker, def, a_norm)?
                            } else {
                                body
                            };
                            if body_subst == self {
                                self
                            } else {
                                body_subst.filled_norm_id(tycker, memo, memo_kd)?
                            }
                        }
                        | _ => {
                            if f_norm == f_ty && a_norm == a_ty && kd_norm == kd {
                                self
                            } else {
                                Alloc::alloc(tycker, App(f_norm, a_norm), kd_norm, &env)
                            }
                        }
                    }
                }
                | Type::Thk(ThkTy) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, ThkTy, kd_norm, &env)
                    }
                }
                | Type::Ret(RetTy) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, RetTy, kd_norm, &env)
                    }
                }
                | Type::Unit(UnitTy) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, UnitTy, kd_norm, &env)
                    }
                }
                | Type::Int(IntTy) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, IntTy, kd_norm, &env)
                    }
                }
                | Type::Char(CharTy) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, CharTy, kd_norm, &env)
                    }
                }
                | Type::String(StringTy) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, StringTy, kd_norm, &env)
                    }
                }
                | Type::OS(OSTy) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, OSTy, kd_norm, &env)
                    }
                }
                | Type::Arrow(arr) => {
                    let Arrow(ty1, ty2) = arr;
                    let ty1_norm = ty1.filled_norm_id(tycker, memo, memo_kd)?;
                    let ty2_norm = ty2.filled_norm_id(tycker, memo, memo_kd)?;
                    if ty1_norm == ty1 && ty2_norm == ty2 && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, Arrow(ty1_norm, ty2_norm), kd_norm, &env)
                    }
                }
                | Type::Forall(forall) => {
                    let Forall(abst, body) = forall;
                    let body_norm = body.filled_norm_id(tycker, memo, memo_kd)?;
                    if body_norm == body && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, Forall(abst, body_norm), kd_norm, &env)
                    }
                }
                | Type::Prod(prod) => {
                    let Prod(ty1, ty2) = prod;
                    let ty1_norm = ty1.filled_norm_id(tycker, memo, memo_kd)?;
                    let ty2_norm = ty2.filled_norm_id(tycker, memo, memo_kd)?;
                    if ty1_norm == ty1 && ty2_norm == ty2 && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, Prod(ty1_norm, ty2_norm), kd_norm, &env)
                    }
                }
                | Type::Exists(exists) => {
                    let Exists(abst, body) = exists;
                    let body_norm = body.filled_norm_id(tycker, memo, memo_kd)?;
                    if body_norm == body && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, Exists(abst, body_norm), kd_norm, &env)
                    }
                }
                | Type::Data(data) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, data, kd_norm, &env)
                    }
                }
                | Type::CoData(codata) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, codata, kd_norm, &env)
                    }
                }
            },
        };
        memo.insert(self, res);
        memo.insert(res, res);
        if let Fillable::Done(ty) = tycker.statics.types_pre[&res].to_owned() {
            tycker.statics.types_normalized.insert(self, ty.clone());
            tycker.statics.types_normalized.insert(res, ty);
        }
        Ok(res)
    }
}
