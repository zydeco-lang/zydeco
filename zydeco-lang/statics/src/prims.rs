use crate::*;
use zydeco_utils::arena::ArenaAccess;

impl Tycker {
    pub fn vtype(&mut self, ctx: ss::Context<ss::CtxItem>) -> ss::KindId {
        let ss::TermId::Kind(kd) = ctx[self.prim.vtype.get()].out else { unreachable!() };
        kd
    }
    pub fn ctype(&mut self, ctx: ss::Context<ss::CtxItem>) -> ss::KindId {
        let ss::TermId::Kind(kd) = ctx[self.prim.ctype.get()].out else { unreachable!() };
        kd
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

impl Tycker {
    pub fn register_prim_ty(
        &mut self, mut ctx: ss::Context<ss::CtxItem>, def: ss::DefId, prim: ss::Type,
        syn_kd: su::TermId,
    ) -> Result<ss::Context<ss::CtxItem>> {
        let kd = syn_kd.tyck_out(self, ByAction::syn(ctx.clone()))?.as_kind();
        let ty = Alloc::alloc(self, prim);
        ctx += ss::CtxExtend(def, ty.into(), kd.into());
        Ok(ctx)
    }
    pub fn register_prim_decl(
        &mut self, decl: su::AliasHead, id: &su::DeclId, mut ctx: ss::Context<ss::CtxItem>,
    ) -> Result<ss::Context<ss::CtxItem>> {
        let su::AliasHead { binder, ty } = decl;
        let internal_or = self.scoped.exts.get(id).cloned();
        match internal_or {
            | Some((internal, def)) => {
                // the alias head is a internal type; unless it's VType or CType
                match internal {
                    | su::Internal::VType => {
                        let kd = Alloc::alloc(self, ss::VType);
                        // no type_of_defs for VType since it's the largest universe level we can get
                        // Note: the annotation itself is wrong but we should never use it
                        ctx += ss::CtxExtend(def, kd.into(), kd.into());
                    }
                    | su::Internal::CType => {
                        let kd = Alloc::alloc(self, ss::CType);
                        // no type_of_defs for CType since it's the largest universe level we can get
                        // Note: the annotation itself is wrong but we should never use it
                        ctx += ss::CtxExtend(def, kd.into(), kd.into());
                    }
                    | su::Internal::Thunk => {
                        let kd = ty.unwrap();
                        ctx = self.register_prim_ty(ctx.clone(), def, ss::ThunkTy.into(), kd)?
                    }
                    | su::Internal::Ret => {
                        let kd = ty.unwrap();
                        ctx = self.register_prim_ty(ctx.clone(), def, ss::RetTy.into(), kd)?
                    }
                    | su::Internal::Unit => {
                        let kd = ty.unwrap();
                        ctx = self.register_prim_ty(ctx.clone(), def, ss::UnitTy.into(), kd)?
                    }
                    | su::Internal::Int => {
                        let kd = ty.unwrap();
                        ctx = self.register_prim_ty(ctx.clone(), def, ss::IntTy.into(), kd)?
                    }
                    | su::Internal::Char => {
                        let kd = ty.unwrap();
                        ctx = self.register_prim_ty(ctx.clone(), def, ss::CharTy.into(), kd)?
                    }
                    | su::Internal::String => {
                        let kd = ty.unwrap();
                        ctx = self.register_prim_ty(ctx.clone(), def, ss::StringTy.into(), kd)?
                    }
                    | su::Internal::OS => {
                        let kd = ty.unwrap();
                        ctx = self.register_prim_ty(ctx.clone(), def, ss::OSTy.into(), kd)?
                    }
                    | su::Internal::Monad | su::Internal::Algebra => {
                        unreachable!()
                    }
                }
            }
            | None => {
                // the alias head is a primitive value that needs to be linked later
                let Some(ty) = ty else { Err(TyckError::MissingAnnotation)? };
                let ty = ty.tyck_ann(self, ByAction::syn(ctx.clone()))?.as_type();
                let _ = binder.tyck(self, ByAction::ana(ctx.clone(), ty.into()))?;
            }
        }
        Ok(ctx)
    }
}
