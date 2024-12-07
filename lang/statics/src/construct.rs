//! Constructors for patterns, types, and terms upon StaticArena.

use crate::{syntax::*, *};

/* -------------------------------------------------------------------------- */
/*                                   Legacy                                   */
/* -------------------------------------------------------------------------- */

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
}

/* ---------------------------------- Term ---------------------------------- */
impl Tycker {
    pub fn top_compu(&mut self, env: &Env<AnnId>) -> CompuId {
        let top = self.top(env);
        let top_compu = Alloc::alloc(self, CoMatch { arms: Vec::new() }, top);
        top_compu
    }
}
