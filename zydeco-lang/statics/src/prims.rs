use super::{ss::Context, *};

impl Tycker {
    pub fn vtype(&mut self) -> ss::KindId {
        let ss::TermId::Kind(kd) = self.statics.defs[self.prim.vtype.get()] else { unreachable!() };
        kd
    }
    pub fn ctype(&mut self) -> ss::KindId {
        let ss::TermId::Kind(kd) = self.statics.defs[self.prim.ctype.get()] else { unreachable!() };
        kd
    }
    pub fn register_prim_ty(
        &mut self, ctx: Context<ss::AnnId>, def: ss::DefId, prim: ss::Type, syn_kd: su::TermId,
    ) -> Result<()> {
        let kd = syn_kd.tyck_out(self, ByAction::syn(ctx))?.as_kind();
        let ty = Alloc::alloc(self, prim);
        self.statics.type_of_defs.insert(def, kd.into());
        self.statics.defs.insert(def, ty.into());
        Ok(())
    }
    pub fn thunk(&mut self) -> ss::TypeId {
        let ss::TermId::Type(ty) = self.statics.defs[self.prim.thunk.get()] else { unreachable!() };
        ty
    }
    pub fn ret(&mut self) -> ss::TypeId {
        let ss::TermId::Type(ty) = self.statics.defs[self.prim.ret.get()] else { unreachable!() };
        ty
    }
    pub fn unit(&mut self) -> ss::TypeId {
        let ss::TermId::Type(ty) = self.statics.defs[self.prim.unit.get()] else { unreachable!() };
        ty
    }
    pub fn int(&mut self) -> ss::TypeId {
        let ss::TermId::Type(ty) = self.statics.defs[self.prim.int.get()] else { unreachable!() };
        ty
    }
    pub fn char(&mut self) -> ss::TypeId {
        let ss::TermId::Type(ty) = self.statics.defs[self.prim.char.get()] else { unreachable!() };
        ty
    }
    pub fn string(&mut self) -> ss::TypeId {
        let ss::TermId::Type(ty) = self.statics.defs[self.prim.string.get()] else {
            unreachable!()
        };
        ty
    }
    pub fn os(&mut self) -> ss::TypeId {
        let ss::TermId::Type(ty) = self.statics.defs[self.prim.os.get()] else { unreachable!() };
        ty
    }
}
