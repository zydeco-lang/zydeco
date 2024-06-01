use crate::*;

impl Tycker {
    pub fn vtype(&mut self, ctx: ss::Context<ss::CtxItem>) -> ss::KindId {
        let ss::TermId::Kind(kd) = ctx[self.prim.vtype.get()].out else { unreachable!() };
        kd
    }
    pub fn ctype(&mut self, ctx: ss::Context<ss::CtxItem>) -> ss::KindId {
        let ss::TermId::Kind(kd) = ctx[self.prim.ctype.get()].out else { unreachable!() };
        kd
    }
    pub fn register_prim_ty(
        &mut self, mut ctx: ss::Context<ss::CtxItem>, def: ss::DefId, prim: ss::Type,
        syn_kd: su::TermId,
    ) -> Result<ss::Context<ss::CtxItem>> {
        let kd = syn_kd.tyck_out(self, ByAction::syn(ctx.clone()))?.as_kind();
        let ty = Alloc::alloc(self, prim);
        ctx += ss::CtxExtend(def, ty.into(), kd.into());
        Ok(ctx)
    }
    pub fn thunk(&mut self, ctx: ss::Context<ss::CtxItem>) -> ss::TypeId {
        let ss::TermId::Type(ty) = ctx[self.prim.thunk.get()].out else { unreachable!() };
        ty
    }
    pub fn ret(&mut self, ctx: ss::Context<ss::CtxItem>) -> ss::TypeId {
        let ss::TermId::Type(ty) = ctx[self.prim.ret.get()].out else { unreachable!() };
        ty
    }
    pub fn unit(&mut self, ctx: ss::Context<ss::CtxItem>) -> ss::TypeId {
        let ss::TermId::Type(ty) = ctx[self.prim.unit.get()].out else { unreachable!() };
        ty
    }
    pub fn int(&mut self, ctx: ss::Context<ss::CtxItem>) -> ss::TypeId {
        let ss::TermId::Type(ty) = ctx[self.prim.int.get()].out else { unreachable!() };
        ty
    }
    pub fn char(&mut self, ctx: ss::Context<ss::CtxItem>) -> ss::TypeId {
        let ss::TermId::Type(ty) = ctx[self.prim.char.get()].out else { unreachable!() };
        ty
    }
    pub fn string(&mut self, ctx: ss::Context<ss::CtxItem>) -> ss::TypeId {
        let ss::TermId::Type(ty) = ctx[self.prim.string.get()].out else { unreachable!() };
        ty
    }
    pub fn os(&mut self, ctx: ss::Context<ss::CtxItem>) -> ss::TypeId {
        let ss::TermId::Type(ty) = ctx[self.prim.os.get()].out else { unreachable!() };
        ty
    }
}
