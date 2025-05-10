use crate::{syntax::*, *};

impl KindId {
    pub fn destruct_arrow(&self, tycker: &mut Tycker) -> Option<(KindId, KindId)> {
        match tycker.statics.kinds[self].to_owned() {
            | Fillable::Fill(_) => todo!(),
            | Fillable::Done(kind) => match kind {
                | Kind::Arrow(Arrow(from, to)) => Some((from, to)),
                | _ => None,
            },
        }
    }
}

impl TPatId {
    pub fn try_destruct_def(&self, tycker: &mut Tycker) -> (Option<DefId>, KindId) {
        use TypePattern as TPat;
        let kd = tycker.statics.annotations_tpat[self].to_owned();
        match tycker.statics.tpats[self].to_owned() {
            | TPat::Hole(Hole) => (None, kd),
            | TPat::Var(def) => (Some(def), kd),
        }
    }
    pub fn destruct_def(&self, tycker: &mut Tycker) -> (DefId, KindId) {
        let (def, kd) = self.try_destruct_def(tycker);
        match def {
            | Some(def) => (def, kd),
            | None => {
                let def = Alloc::alloc(tycker, VarName("_".to_owned()), kd.into());
                (def, kd)
            }
        }
    }
}

impl TypeId {
    // pub fn destruct_type_abs_nf(&self, tycker: &mut Tycker) -> Option<(TPatId, TypeId)> {
    //     let res = match tycker.statics.types.get(self)?.to_owned() {
    //         | Type::Abs(Abs(tpat, ty)) => (tpat, ty),
    //         | Type::Abst(abst) => {
    //             let kd = tycker.statics.annotations_abst[&abst].to_owned();
    //             let abst_ty = Alloc::alloc(tycker, abst, kd.into());

    //             let (kd_z, kd_body) = match tycker.kind_filled(&kd).to_owned() {
    //                 | Kind::Arrow(Arrow(kd_z, kd_body)) => (kd_z, kd_body),
    //                 | Kind::Fill(_) | Kind::VType(_) | Kind::CType(_) => unreachable!(),
    //             };
    //             // Todo: use construct API
    //             let tvar_z = Alloc::alloc(tycker, VarName("Z".to_owned()), kd_z.into());
    //             let tpat_z: TPatId = Alloc::alloc(tycker, tvar_z, kd_z);
    //             let ty_z: TypeId = Alloc::alloc(tycker, tvar_z, kd_z);
    //             let z_app_abst = Alloc::alloc(tycker, App(abst_ty, ty_z), kd_body);
    //             (tpat_z, z_app_abst)
    //         }
    //         | _ => None?,
    //     };
    //     Some(res)
    // }
    pub fn destruct_type_app_nf_k(&self, tycker: &mut Tycker) -> ResultKont<(TypeId, Vec<TypeId>)> {
        let res = self.destruct_type_app_nf(tycker);
        tycker.err_p_to_k(res)
    }
    pub fn destruct_type_app_nf(&self, tycker: &mut Tycker) -> Result<(TypeId, Vec<TypeId>)> {
        let ty = self.normalize(tycker, tycker.statics.annotations_type[self].to_owned())?;
        let res = match tycker.type_filled(&ty)?.to_owned() {
            | Type::App(app_ty) => {
                let App(f_ty, a_ty) = app_ty;
                let (f_ty, mut a_tys) = f_ty.destruct_type_app_nf(tycker)?;
                let a_ty =
                    a_ty.normalize(tycker, tycker.statics.annotations_type[&a_ty].clone())?;
                a_tys.push(a_ty);
                (f_ty, a_tys)
            }
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
            | Type::CoData(_) => (ty, Vec::new()),
        };
        Ok(res)
    }
    pub fn destruct_thk_app(&self, tycker: &mut Tycker) -> Option<TypeId> {
        let (f_ty, a_tys) = self.destruct_type_app_nf(tycker).ok()?;
        let res = match tycker.statics.types[&f_ty].to_owned() {
            | Fillable::Fill(_) => todo!(),
            | Fillable::Done(ty) => match ty {
                | Type::Thk(ThkTy) => {
                    if a_tys.len() == 1 {
                        let mut iter = a_tys.into_iter();
                        iter.next()?
                    } else {
                        None?
                    }
                }
                | _ => None?,
            },
        };
        Some(res)
    }
    pub fn destruct_ret_app(&self, tycker: &mut Tycker) -> Option<TypeId> {
        let (f_ty, a_tys) = self.destruct_type_app_nf(tycker).ok()?;
        let res = match tycker.statics.types[&f_ty].to_owned() {
            | Fillable::Fill(_) => todo!(),
            | Fillable::Done(ty) => match ty {
                | Type::Ret(RetTy) => {
                    if a_tys.len() == 1 {
                        let mut iter = a_tys.into_iter();
                        iter.next()?
                    } else {
                        None?
                    }
                }
                | _ => None?,
            },
        };
        Some(res)
    }
    pub fn destruct_top(&self, _env: &Env<AnnId>, tycker: &mut Tycker) -> Option<()> {
        let res = match tycker.statics.types[&self].to_owned() {
            | Fillable::Fill(_) => todo!(),
            | Fillable::Done(ty) => match ty {
                | Type::CoData(coda) => {
                    let coda = tycker.statics.codatas.defs[&coda].to_owned();
                    (coda.len() == 0).then(|| ())?
                }
                | _ => None?,
            },
        };
        Some(res)
    }
    pub fn destruct_arrow(&self, tycker: &mut Tycker) -> Option<(TypeId, TypeId)> {
        let res = match tycker.statics.types[&self].to_owned() {
            | Fillable::Fill(_) => todo!(),
            | Fillable::Done(ty) => match ty {
                | Type::Arrow(ty) => {
                    let Arrow(from, to) = ty;
                    (from, to)
                }
                | _ => None?,
            },
        };
        Some(res)
    }
    pub fn destruct_forall(&self, tycker: &mut Tycker) -> Option<(AbstId, TypeId)> {
        match tycker.statics.types[&self].to_owned() {
            | Fillable::Fill(_) => todo!(),
            | Fillable::Done(ty) => match ty {
                | Type::Forall(Forall(abst, ty)) => Some((abst, ty)),
                | _ => None,
            },
        }
    }
    pub fn destruct_monad(&self, env: &Env<AnnId>, tycker: &mut Tycker) -> Option<TypeId> {
        let (f_ty, a_tys) = self.destruct_type_app_nf(tycker).ok()?;
        if a_tys.len() != 1 {
            None?;
        }
        let res = match tycker.statics.types[&f_ty].to_owned() {
            | Fillable::Done(Type::Abst(abst)) => {
                let AnnId::Type(id) = env[tycker.prim.monad.get()] else { unreachable!() };
                let Type::Abst(monad_real) = tycker.type_filled(&id).ok()?.to_owned() else {
                    unreachable!()
                };
                if abst != monad_real {
                    None?;
                }
                a_tys.into_iter().next()?
            }
            | _ => None?,
        };
        Some(res)
    }
    pub fn destruct_algebra(
        &self, env: &Env<AnnId>, tycker: &mut Tycker,
    ) -> Option<(TypeId, TypeId)> {
        let (f_ty, a_tys) = self.destruct_type_app_nf(tycker).ok()?;
        if a_tys.len() != 2 {
            None?;
        }
        let res = match tycker.statics.types[&f_ty].to_owned() {
            | Fillable::Done(Type::Abst(abst)) => {
                let AnnId::Type(id) = env[tycker.prim.algebra.get()] else { unreachable!() };
                let Type::Abst(algebra_real) = tycker.type_filled(&id).ok()?.to_owned() else {
                    unreachable!()
                };
                if abst != algebra_real {
                    None?;
                }
                let mut iter = a_tys.into_iter();
                let mo_ty = iter.next()?;
                let carrier_ty = iter.next()?;
                (mo_ty, carrier_ty)
            }
            | _ => None?,
        };
        Some(res)
    }
    pub fn destruct_codata<'t>(
        &self, _env: &Env<AnnId>, tycker: &'t mut Tycker,
    ) -> Option<&'t CoData> {
        use zydeco_utils::arena::ArenaAccess;
        match tycker.statics.types[&self].to_owned() {
            | Fillable::Fill(_) => todo!(),
            | Fillable::Done(ty) => match ty {
                | Type::CoData(coda) => tycker.statics.codatas.tbls.get(&coda),
                | _ => None,
            },
        }
    }
}

impl VPatId {
    /// If the pattern is a variable definition, return the definition and its type;
    /// otherwise, return the type of the pattern.
    pub fn try_destruct_def(&self, tycker: &mut Tycker) -> (Option<DefId>, TypeId) {
        use ValuePattern as VPat;
        let ty = tycker.statics.annotations_vpat[self].to_owned();
        match tycker.statics.vpats[self].to_owned() {
            | VPat::Hole(Hole) => (None, ty),
            | VPat::Var(def) => (Some(def), ty),
            | VPat::Ctor(_) => (None, ty),
            | VPat::Triv(_) => (None, ty),
            | VPat::VCons(_) => (None, ty),
            | VPat::TCons(_) => (None, ty),
        }
    }
}
