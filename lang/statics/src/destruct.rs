use crate::{syntax::*, *};
use zydeco_utils::arena::ArenaAccess;

impl TPatId {
    pub fn destruct(&self, tycker: &mut Tycker) -> (Option<DefId>, KindId) {
        match tycker.statics.tpats[self].clone() {
            | TypePattern::Hole(Ann { tm: Hole, ty: kd }) => (None, kd),
            | TypePattern::Var(def) => {
                let AnnId::Kind(kd) = tycker.statics.annotations_var[&def] else { unreachable!() };
                (Some(def), kd)
            }
        }
    }
    pub fn destruct_def(&self, tycker: &mut Tycker) -> (DefId, KindId) {
        let (def, kd) = self.destruct(tycker);
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
    pub fn destruct_fill(&self, tycker: &mut Tycker) -> Option<FillId> {
        match tycker.statics.types.get(self)?.to_owned() {
            | Type::Fill(fill) => Some(fill),
            | _ => None,
        }
    }
    pub fn destruct_type_abs(&self, tycker: &mut Tycker) -> Option<(TPatId, TypeId)> {
        match tycker.statics.types.get(self)?.to_owned() {
            | Type::Abs(Abs(tpat, ty)) => Some((tpat, ty)),
            | _ => None,
        }
    }
    pub fn destruct_type_app_nf_k(&self, tycker: &mut Tycker) -> ResultKont<(TypeId, Vec<TypeId>)> {
        let res = self.destruct_type_app_nf(tycker);
        tycker.err_p_to_k(res)
    }
    pub fn destruct_type_app_nf(&self, tycker: &mut Tycker) -> Result<(TypeId, Vec<TypeId>)> {
        let ty = self.normalize(tycker, tycker.statics.annotations_type[self].to_owned())?;
        let res = match tycker.statics.types[&ty].to_owned() {
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
    pub fn destruct_arrow(&self, tycker: &mut Tycker) -> Option<(TypeId, TypeId)> {
        let res = match tycker.statics.types[&self].to_owned() {
            | Type::Arrow(ty) => {
                let Arrow(from, to) = ty;
                (from, to)
            }
            | _ => None?,
        };
        Some(res)
    }
    pub fn destruct_thunk_app(&self, tycker: &mut Tycker) -> Option<TypeId> {
        let (f_ty, a_tys) = self.destruct_type_app_nf(tycker).ok()?;
        let res = match tycker.statics.types[&f_ty].to_owned() {
            | Type::Thunk(ThunkTy) => {
                if a_tys.len() == 1 {
                    let mut iter = a_tys.into_iter();
                    iter.next()?
                } else {
                    None?
                }
            }
            | _ => None?,
        };
        Some(res)
    }
    pub fn destruct_algebra(
        &self, env: &Env<AnnId>, tycker: &mut Tycker,
    ) -> Option<(TypeId, TypeId)> {
        let (f_ty, a_tys) = self.destruct_type_app_nf(tycker).ok()?;
        let res = match tycker.statics.types[&f_ty].to_owned() {
            | Type::Abst(abst) => {
                let AnnId::Type(id) = env[tycker.prim.algebra.get()] else { unreachable!() };
                let Type::Abst(alg_real) = tycker.statics.types.get(&id).cloned()? else {
                    unreachable!()
                };
                if abst != alg_real {
                    None?;
                }
                let mut iter = a_tys.into_iter();
                (iter.next()?, iter.next()?)
            }
            | _ => None?,
        };
        Some(res)
    }
    pub fn destruct_structure(&self, env: &Env<AnnId>, tycker: &mut Tycker) -> Option<Structure> {
        let (f_ty, a_tys) = self.destruct_type_app_nf(tycker).ok()?;
        let res = 'out: {
            match tycker.statics.types[&f_ty].to_owned() {
                | Type::Abst(abst) => {
                    let AnnId::Type(id) = env[tycker.prim.top.get()] else { unreachable!() };
                    let Type::Abst(top_real) = tycker.statics.types.get(&id).cloned()? else {
                        unreachable!()
                    };
                    let AnnId::Type(id) = env[tycker.prim.algebra.get()] else { unreachable!() };
                    let Type::Abst(alg_real) = tycker.statics.types.get(&id).cloned()? else {
                        unreachable!()
                    };

                    if abst == top_real {
                        assert!(a_tys.is_empty());
                        break 'out Structure::Top;
                    }

                    if abst == alg_real {
                        let mut iter = a_tys.into_iter();
                        let mo_ty = iter.next()?;
                        let carrier_ty = iter.next()?;
                        break 'out Structure::Algebra(mo_ty, carrier_ty);
                    }

                    None?
                }
                | _ => None?,
            }
        };
        Some(res)
    }
    pub fn destruct_forall(&self, tycker: &mut Tycker) -> Option<(AbstId, TypeId)> {
        match tycker.statics.types.get(self)?.to_owned() {
            | Type::Forall(Forall(abst, ty)) => Some((abst, ty)),
            | _ => None,
        }
    }
}
