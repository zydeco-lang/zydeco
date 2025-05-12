//! Constructors for patterns, types, and terms upon StaticArena.

use crate::{syntax::*, *};

pub trait Construct<T> {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> T;
}

/* ---------------------------------- Kind ---------------------------------- */
impl Construct<KindId> for KindId {
    fn build(self, _tycker: &mut Tycker, _env: &Env<AnnId>) -> KindId {
        self
    }
}
impl Construct<KindId> for VType {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> KindId {
        let AnnId::Kind(kd) = env[tycker.prim.vtype.get()] else { unreachable!() };
        kd
    }
}
impl Construct<KindId> for CType {
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> KindId {
        let AnnId::Kind(kd) = env[tycker.prim.ctype.get()] else { unreachable!() };
        kd
    }
}
impl<K1, K2> Construct<KindId> for SArrow<K1, K2>
where
    K1: Construct<KindId>,
    K2: Construct<KindId>,
{
    fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> KindId {
        let SArrow(k1, k2) = self;
        let k1 = k1.build(tycker, env);
        let k2 = k2.build(tycker, env);
        Alloc::alloc(tycker, Arrow(k1, k2), ())
    }
}

impl Tycker {
    pub fn vtype(&mut self, env: &Env<AnnId>) -> KindId {
        VType.build(self, env)
    }
    pub fn ctype(&mut self, env: &Env<AnnId>) -> KindId {
        CType.build(self, env)
    }
}

#[cfg(test)]
#[test]
fn static_test() {
    fn _f(tycker: &mut Tycker, env: &Env<AnnId>) -> KindId {
        SArrow(VType, SArrow(CType, CType)).build(tycker, env)
    }
}

/* ---------------------------------- Type ---------------------------------- */
impl Tycker {
    pub fn type_thk(&mut self, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[self.prim.thk.get()] else { unreachable!() };
        ty
    }
    /// generates `Thunk B`
    pub fn thk_arg(&mut self, env: &Env<AnnId>, arg: TypeId) -> TypeId {
        let thk = self.type_thk(env);
        let vtype = self.vtype(env);
        Alloc::alloc(self, App(thk, arg), vtype)
    }
    /// generates `Thunk _`
    pub fn thk_hole(&mut self, env: &Env<AnnId>, site: su::TermId) -> TypeId {
        let fill = self.statics.fills.alloc(site);
        let ctype = self.ctype(env);
        let hole = Alloc::alloc(self, fill, ctype);
        self.thk_arg(env, hole)
    }
    pub fn type_ret(&mut self, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[self.prim.ret.get()] else { unreachable!() };
        ty
    }
    /// generates `Ret A`
    pub fn ret_arg(&mut self, env: &Env<AnnId>, arg: TypeId) -> TypeId {
        let ret = self.type_ret(env);
        let ctype = self.ctype(env);
        Alloc::alloc(self, App(ret, arg), ctype)
    }
    pub fn ret_hole(&mut self, env: &Env<AnnId>, site: su::TermId) -> TypeId {
        let fill = self.statics.fills.alloc(site);
        let vtype = self.vtype(env);
        let hole = Alloc::alloc(self, fill, vtype);
        self.ret_arg(env, hole)
    }
    pub fn type_unit(&mut self, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[self.prim.unit.get()] else { unreachable!() };
        ty
    }
    pub fn type_int(&mut self, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[self.prim.int.get()] else { unreachable!() };
        ty
    }
    pub fn type_char(&mut self, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[self.prim.char.get()] else { unreachable!() };
        ty
    }
    pub fn type_string(&mut self, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[self.prim.string.get()] else { unreachable!() };
        ty
    }
    pub fn type_top(&mut self, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[self.prim.top.get()] else { unreachable!() };
        ty
    }
    pub fn type_os(&mut self, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[self.prim.os.get()] else { unreachable!() };
        ty
    }
    pub fn type_prod(&mut self, env: &Env<AnnId>, a1: TypeId, a2: TypeId) -> TypeId {
        let vtype = self.vtype(env);
        Alloc::alloc(self, Prod(a1, a2), vtype)
    }
    pub fn type_exists(&mut self, env: &Env<AnnId>, x: AbstId, b: TypeId) -> TypeId {
        let ctype = self.ctype(env);
        Alloc::alloc(self, Exists(x, b), ctype)
    }
    pub fn type_arrow(&mut self, env: &Env<AnnId>, a: TypeId, b: TypeId) -> TypeId {
        let ctype = self.ctype(env);
        Alloc::alloc(self, Arrow(a, b), ctype)
    }
    pub fn type_forall(&mut self, env: &Env<AnnId>, x: AbstId, b: TypeId) -> TypeId {
        let ctype = self.ctype(env);
        Alloc::alloc(self, Forall(x, b), ctype)
    }
    // pub fn type_abs
    pub fn type_app(&mut self, ty_1: TypeId, ty_2: TypeId) -> TypeId {
        let kd_1 = self.statics.annotations_type[&ty_1];
        let Some((kd_a, kd_b)) = kd_1.destruct_arrow(self) else { unreachable!() };
        let kd_2 = self.statics.annotations_type[&ty_2];
        let Ok(_) = Lub::lub(kd_a, kd_2, self) else { unreachable!() };
        Alloc::alloc(self, App(ty_1, ty_2), kd_b)
    }
    /// generates `Monad M` where:
    /// 1. M is `monad_ty` of kind `VType -> CType`
    pub fn monad_mo(&mut self, env: &Env<AnnId>, monad_ty: TypeId) -> TypeId {
        let AnnId::Type(monad) = env[self.prim.monad.get()] else { unreachable!() };
        let ctype = self.ctype(env);
        Alloc::alloc(self, App(monad, monad_ty), ctype)
    }
    /// generates `Algebra M R` where:
    /// 1. M is `monad_ty` of kind `VType -> CType`
    /// 2. R is `carrier` of kind `CType`
    pub fn algebra_mo_car(
        &mut self, env: &Env<AnnId>, monad_ty: TypeId, carrier: TypeId,
    ) -> TypeId {
        let AnnId::Type(algebra) = env[self.prim.algebra.get()] else { unreachable!() };
        let ctype = self.ctype(env);
        let algebra_mo_kd = Alloc::alloc(self, Arrow(ctype, ctype), ());
        let algebra_mo = Alloc::alloc(self, App(algebra, monad_ty), algebra_mo_kd);
        Alloc::alloc(self, App(algebra_mo, carrier), ctype)
    }
}

/* ---------------------------------- Value --------------------------------- */

impl Tycker {
    pub fn value_var(&mut self, _env: &Env<AnnId>, def: DefId, ty: TypeId) -> ValueId {
        Alloc::alloc(self, def, ty.into())
    }
    pub fn value_thunk(&mut self, env: &Env<AnnId>, body: CompuId) -> ValueId {
        let body_ty = self.statics.annotations_compu[&body];
        let ty = self.thk_arg(env, body_ty);
        Alloc::alloc(self, Thunk(body), ty)
    }
    pub fn value_triv(&mut self, env: &Env<AnnId>) -> ValueId {
        let ty = self.type_unit(env);
        Alloc::alloc(self, Triv, ty)
    }
    pub fn value_vcons(&mut self, env: &Env<AnnId>, a: ValueId, b: ValueId) -> ValueId {
        let a_ty = self.statics.annotations_value[&a];
        let b_ty = self.statics.annotations_value[&b];
        let ty = self.type_prod(env, a_ty, b_ty);
        Alloc::alloc(self, Cons(a, b), ty)
    }
}

/* ---------------------------------- Compu --------------------------------- */

impl Tycker {
    pub fn compu_vabs(
        &mut self, env: &Env<AnnId>, name: VarName, def_ty: TypeId,
        body: impl Fn(&mut Self, DefId) -> CompuId,
    ) -> CompuId {
        let ctype = self.ctype(env);
        let def = Alloc::alloc(self, name, def_ty.into());
        let vpat: VPatId = Alloc::alloc(self, def, def_ty);
        let body = body(self, def);
        let body_ty = self.statics.annotations_compu[&body];
        let ty = Alloc::alloc(self, Arrow(def_ty, body_ty), ctype);
        Alloc::alloc(self, Abs(vpat, body), ty)
    }
    pub fn try_compu_vabs(
        &mut self, env: &Env<AnnId>, name: VarName, def_ty: TypeId,
        body: impl Fn(&mut Self, DefId) -> Result<CompuId>,
    ) -> Result<CompuId> {
        let ctype = self.ctype(env);
        let def = Alloc::alloc(self, name, def_ty.into());
        let vpat: VPatId = Alloc::alloc(self, def, def_ty);
        let body = body(self, def)?;
        let body_ty = self.statics.annotations_compu[&body];
        let ty = Alloc::alloc(self, Arrow(def_ty, body_ty), ctype);
        Ok(Alloc::alloc(self, Abs(vpat, body), ty))
    }
    pub fn compu_vapp(&mut self, _env: &Env<AnnId>, abs: CompuId, arg: ValueId) -> CompuId {
        let abs_ty = self.statics.annotations_compu[&abs];
        let Some((param_ty, body_ty)) = abs_ty.destruct_arrow(self) else { unreachable!() };
        let arg_ty = self.statics.annotations_value[&arg];
        let Ok(_) = Lub::lub(param_ty, arg_ty, self) else { unreachable!() };
        Alloc::alloc(self, App(abs, arg), body_ty)
    }
    pub fn compu_tabs(
        &mut self, env: &Env<AnnId>, name: VarName, def_kd: impl Construct<KindId>,
        body: impl Fn(&mut Self, DefId, AbstId) -> CompuId,
    ) -> CompuId {
        let def_kd = def_kd.build(self, env);
        let def = Alloc::alloc(self, name, def_kd.into());
        let abst = Alloc::alloc(self, def, def_kd);
        let tpat: TPatId = Alloc::alloc(self, def, def_kd);
        let body = body(self, def, abst);
        let body_ty = self.statics.annotations_compu[&body];
        let ctype = self.ctype(env);
        let ty = Alloc::alloc(self, Forall(abst, body_ty), ctype);
        Alloc::alloc(self, Abs(tpat, body), ty)
    }
    pub fn try_compu_tabs(
        &mut self, env: &Env<AnnId>, name: VarName, def_kd: impl Construct<KindId>,
        body: impl Fn(&mut Self, DefId, AbstId) -> Result<CompuId>,
    ) -> Result<CompuId> {
        let def_kd = def_kd.build(self, env);
        let def = Alloc::alloc(self, name, def_kd.into());
        let abst = Alloc::alloc(self, def, def_kd);
        let tpat: TPatId = Alloc::alloc(self, def, def_kd);
        let body = body(self, def, abst)?;
        let body_ty = self.statics.annotations_compu[&body];
        let ctype = self.ctype(env);
        let ty = Alloc::alloc(self, Forall(abst, body_ty), ctype);
        Ok(Alloc::alloc(self, Abs(tpat, body), ty))
    }
    pub fn compu_tapp(&mut self, _env: &Env<AnnId>, abs: CompuId, arg: TypeId) -> CompuId {
        let abs_ty = self.statics.annotations_compu[&abs];
        let Some((abst, body_ty)) = abs_ty.destruct_forall(self) else { unreachable!() };
        let param_kd = self.statics.annotations_abst[&abst];
        let arg_kd = self.statics.annotations_type[&arg];
        let Ok(_) = Lub::lub(param_kd, arg_kd, self) else { unreachable!() };
        let Ok(ty) = body_ty.subst_abst(self, (abst, arg)) else { unreachable!() };
        Alloc::alloc(self, App(abs, arg), ty)
    }
    pub fn compu_fix(
        &mut self, env: &Env<AnnId>, name: VarName, ty: TypeId,
        body: impl Fn(&mut Self, DefId) -> CompuId,
    ) -> CompuId {
        let ctype = self.ctype(env);
        let def = Alloc::alloc(self, name, ty.into());
        let vpat: VPatId = Alloc::alloc(self, def, ty);
        let body = body(self, def);
        let body_ty = self.statics.annotations_compu[&body];
        let ty = Alloc::alloc(self, Arrow(ty, body_ty), ctype);
        Alloc::alloc(self, Fix(vpat, body), ty)
    }
    pub fn compu_force(&mut self, _env: &Env<AnnId>, thk: ValueId) -> CompuId {
        let thk_ty = self.statics.annotations_value[&thk];
        let Some(body_ty) = thk_ty.destruct_thk_app(self) else { unreachable!() };
        Alloc::alloc(self, Force(thk), body_ty)
    }
    pub fn compu_ret(&mut self, env: &Env<AnnId>, val: ValueId) -> CompuId {
        let val_ty = self.statics.annotations_value[&val];
        let ret_ty = self.ret_arg(env, val_ty);
        Alloc::alloc(self, Ret(val), ret_ty)
    }
    pub fn compu_bind(
        &mut self, _env: &Env<AnnId>, bindee: CompuId, name: VarName,
        tail: impl Fn(&mut Self, DefId) -> CompuId,
    ) -> CompuId {
        let bindee_ty = self.statics.annotations_compu[&bindee];
        let Some(def_ty) = bindee_ty.destruct_ret_app(self) else { unreachable!() };
        let def = Alloc::alloc(self, name, def_ty.into());
        let binder = Alloc::alloc(self, def, def_ty);
        let tail = tail(self, def);
        let tail_ty = self.statics.annotations_compu[&tail];
        Alloc::alloc(self, Bind { binder, bindee, tail }, tail_ty)
    }
    pub fn compu_let(
        &mut self, _env: &Env<AnnId>, bindee: ValueId, name: VarName,
        tail: impl Fn(&mut Self, DefId) -> CompuId,
    ) -> CompuId {
        let def_ty = self.statics.annotations_value[&bindee];
        let def = Alloc::alloc(self, name, def_ty.into());
        let binder = Alloc::alloc(self, def, def_ty);
        let tail = tail(self, def);
        let tail_ty = self.statics.annotations_compu[&tail];
        Alloc::alloc(self, PureBind { binder, bindee, tail }, tail_ty)
    }
    pub fn compu_top(&mut self, env: &Env<AnnId>) -> CompuId {
        let top = self.type_top(env);
        Alloc::alloc(self, CoMatch { arms: Vec::new() }, top)
    }
    pub fn compu_dtor(&mut self, env: &Env<AnnId>, head: CompuId, dtor: DtorName) -> CompuId {
        let head_ty = self.statics.annotations_compu[&head];
        let Some(coda) = head_ty.destruct_codata(env, self) else { unreachable!() };
        let Some(ty) = coda.get(&dtor).cloned() else { unreachable!() };
        Alloc::alloc(self, Dtor(head, dtor), ty)
    }
}
