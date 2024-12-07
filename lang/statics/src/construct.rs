//! Constructors for patterns, types, and terms upon StaticArena.

use crate::{syntax::*, *};

/* ---------------------------------- Kind ---------------------------------- */
impl Tycker {
    pub fn vtype(&mut self, env: &Env<AnnId>) -> KindId {
        let AnnId::Kind(kd) = env[self.prim.vtype.get()] else { unreachable!() };
        kd
    }
    pub fn ctype(&mut self, env: &Env<AnnId>) -> KindId {
        let AnnId::Kind(kd) = env[self.prim.ctype.get()] else { unreachable!() };
        kd
    }
}

/* ---------------------------------- Type ---------------------------------- */
impl Tycker {
    pub fn thk(&mut self, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[self.prim.thk.get()] else { unreachable!() };
        ty
    }
    /// generates `Thunk B`
    pub fn thk_arg(&mut self, env: &Env<AnnId>, arg: TypeId) -> TypeId {
        let thk = self.thk(env);
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
    pub fn ret(&mut self, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[self.prim.ret.get()] else { unreachable!() };
        ty
    }
    /// generates `Ret A`
    pub fn ret_arg(&mut self, env: &Env<AnnId>, arg: TypeId) -> TypeId {
        let ret = self.ret(env);
        let ctype = self.ctype(env);
        Alloc::alloc(self, App(ret, arg), ctype)
    }
    pub fn ret_hole(&mut self, env: &Env<AnnId>, site: su::TermId) -> TypeId {
        let fill = self.statics.fills.alloc(site);
        let vtype = self.vtype(env);
        let hole = Alloc::alloc(self, fill, vtype);
        self.ret_arg(env, hole)
    }
    pub fn unit(&mut self, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[self.prim.unit.get()] else { unreachable!() };
        ty
    }
    pub fn int(&mut self, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[self.prim.int.get()] else { unreachable!() };
        ty
    }
    pub fn char(&mut self, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[self.prim.char.get()] else { unreachable!() };
        ty
    }
    pub fn string(&mut self, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[self.prim.string.get()] else { unreachable!() };
        ty
    }
    pub fn top(&mut self, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[self.prim.top.get()] else { unreachable!() };
        ty
    }
    pub fn os(&mut self, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[self.prim.os.get()] else { unreachable!() };
        ty
    }
    /// generates `Monad M` where:
    /// 1. M is `mo` of kind `VType -> CType`
    pub fn monad_mo(&mut self, env: &Env<AnnId>, mo: TypeId) -> TypeId {
        let AnnId::Type(monad) = env[self.prim.monad.get()] else { unreachable!() };
        let ctype = self.ctype(env);
        Alloc::alloc(self, App(monad, mo), ctype)
    }
    /// generates `Algebra M R` where:
    /// 1. M is `mo` of kind `VType -> CType`
    /// 2. R is `carrier` of kind `CType`
    pub fn algebra_mo_car(&mut self, env: &Env<AnnId>, mo: TypeId, carrier: TypeId) -> TypeId {
        let AnnId::Type(algebra) = env[self.prim.algebra.get()] else { unreachable!() };
        let ctype = self.ctype(env);
        let algebra_mo_kd = Alloc::alloc(self, Arrow(ctype, ctype), ());
        let algebra_mo = Alloc::alloc(self, App(algebra, mo), algebra_mo_kd);
        Alloc::alloc(self, App(algebra_mo, carrier), ctype)
    }
    pub fn prod(&mut self, env: &Env<AnnId>, a: TypeId, b: TypeId) -> TypeId {
        let vtype = self.vtype(env);
        Alloc::alloc(self, Prod(a, b), vtype)
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
        let ty = self.unit(env);
        Alloc::alloc(self, Triv, ty)
    }
    pub fn value_vcons(&mut self, env: &Env<AnnId>, a: ValueId, b: ValueId) -> ValueId {
        let a_ty = self.statics.annotations_value[&a];
        let b_ty = self.statics.annotations_value[&b];
        let ty = self.prod(env, a_ty, b_ty);
        Alloc::alloc(self, Cons(a, b), ty)
    }
}

/* ---------------------------------- Compu --------------------------------- */

impl Tycker {
    pub fn compu_uni_vabs(
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
    pub fn compu_vapp(&mut self, _env: &Env<AnnId>, abs: CompuId, arg: ValueId) -> CompuId {
        let abs_ty = self.statics.annotations_compu[&abs];
        let Some((param_ty, body_ty)) = abs_ty.destruct_arrow(self) else { unreachable!() };
        let arg_ty = self.statics.annotations_value[&arg];
        let Ok(_) = Lub::lub(param_ty, arg_ty, self) else { unreachable!() };
        Alloc::alloc(self, App(abs, arg), body_ty)
    }
    pub fn compu_tabs(
        &mut self, env: &Env<AnnId>, name: VarName, def_kd: KindId,
        body: impl Fn(&mut Self, DefId, AbstId) -> CompuId,
    ) -> CompuId {
        let ctype = self.ctype(env);
        let def = Alloc::alloc(self, name, def_kd.into());
        let abst = Alloc::alloc(self, def, def_kd);
        let tpat: TPatId = Alloc::alloc(self, def, def_kd);
        let body = body(self, def, abst);
        let body_ty = self.statics.annotations_compu[&body];
        let ty = Alloc::alloc(self, Forall(abst, body_ty), ctype);
        Alloc::alloc(self, Abs(tpat, body), ty)
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
        let top = self.top(env);
        Alloc::alloc(self, CoMatch { arms: Vec::new() }, top)
    }
}
