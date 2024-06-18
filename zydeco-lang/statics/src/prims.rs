use crate::*;
use zydeco_utils::arena::ArenaAccess;

impl Tycker {
    pub fn vtype(&mut self, env: &ss::Context<ss::AnnId>) -> ss::KindId {
        let ss::AnnId::Kind(kd) = env[self.prim.vtype.get()] else { unreachable!() };
        kd
    }
    pub fn ctype(&mut self, env: &ss::Context<ss::AnnId>) -> ss::KindId {
        let ss::AnnId::Kind(kd) = env[self.prim.ctype.get()] else { unreachable!() };
        kd
    }
    pub fn thunk(&mut self, env: &ss::Context<ss::AnnId>) -> ss::TypeId {
        let ss::AnnId::Type(ty) = env[self.prim.thunk.get()] else { unreachable!() };
        ty
    }
    pub fn thunk_app_hole(&mut self, env: &ss::Context<ss::AnnId>) -> ss::TypeId {
        let ss::AnnId::Type(ty) = env[self.prim.thunk.get()] else { unreachable!() };
        let hole = self.statics.absts.alloc(());
        let hole = Alloc::alloc(self, hole);
        let app = Alloc::alloc(self, ss::App(ty, hole));
        app
    }
    pub fn ret(&mut self, env: &ss::Context<ss::AnnId>) -> ss::TypeId {
        let ss::AnnId::Type(ty) = env[self.prim.ret.get()] else { unreachable!() };
        ty
    }
    pub fn ret_app_hole(&mut self, env: &ss::Context<ss::AnnId>) -> ss::TypeId {
        let ss::AnnId::Type(ty) = env[self.prim.ret.get()] else { unreachable!() };
        let hole = self.statics.absts.alloc(());
        let hole = Alloc::alloc(self, hole);
        let app = Alloc::alloc(self, ss::App(ty, hole));
        app
    }
    pub fn unit(&mut self, env: &ss::Context<ss::AnnId>) -> ss::TypeId {
        let ss::AnnId::Type(ty) = env[self.prim.unit.get()] else { unreachable!() };
        ty
    }
    pub fn int(&mut self, env: &ss::Context<ss::AnnId>) -> ss::TypeId {
        let ss::AnnId::Type(ty) = env[self.prim.int.get()] else { unreachable!() };
        ty
    }
    pub fn char(&mut self, env: &ss::Context<ss::AnnId>) -> ss::TypeId {
        let ss::AnnId::Type(ty) = env[self.prim.char.get()] else { unreachable!() };
        ty
    }
    pub fn string(&mut self, env: &ss::Context<ss::AnnId>) -> ss::TypeId {
        let ss::AnnId::Type(ty) = env[self.prim.string.get()] else { unreachable!() };
        ty
    }
    pub fn os(&mut self, env: &ss::Context<ss::AnnId>) -> ss::TypeId {
        let ss::AnnId::Type(ty) = env[self.prim.os.get()] else { unreachable!() };
        ty
    }
}

impl Tycker {
    pub fn register_prim_ty(
        &mut self, mut ctx: TopCtx, def: ss::DefId, prim: ss::Type, syn_kd: su::TermId,
    ) -> Result<TopCtx> {
        let kd = ctx.as_env(syn_kd).tyck(self, ByAction::syn())?.as_ann().as_kind();
        let ty = Alloc::alloc(self, prim);
        ctx.term_ctx += (def, kd.into());
        ctx.type_env += (def, ty.into());
        Ok(ctx)
    }
    pub fn register_prim_decl(
        &mut self, decl: su::AliasHead, id: &su::DeclId, mut ctx: TopCtx,
    ) -> Result<TopCtx> {
        let su::AliasHead { binder, ty } = decl;
        let internal_or = self.scoped.exts.get(id).cloned();
        match internal_or {
            | Some((internal, def)) => {
                // the alias head is a internal type; unless it's VType or CType
                match internal {
                    | su::Internal::VType => {
                        let kd = Alloc::alloc(self, ss::VType);
                        ctx.term_ctx += (def, ss::AnnId::Set);
                        ctx.type_env += (def, kd.into());
                    }
                    | su::Internal::CType => {
                        let kd = Alloc::alloc(self, ss::CType);
                        ctx.term_ctx += (def, ss::AnnId::Set);
                        ctx.type_env += (def, kd.into());
                    }
                    | su::Internal::Thunk => {
                        let kd = ty.unwrap();
                        ctx = self.register_prim_ty(ctx, def, ss::ThunkTy.into(), kd)?
                    }
                    | su::Internal::Ret => {
                        let kd = ty.unwrap();
                        ctx = self.register_prim_ty(ctx, def, ss::RetTy.into(), kd)?
                    }
                    | su::Internal::Unit => {
                        let kd = ty.unwrap();
                        ctx = self.register_prim_ty(ctx, def, ss::UnitTy.into(), kd)?
                    }
                    | su::Internal::Int => {
                        let kd = ty.unwrap();
                        ctx = self.register_prim_ty(ctx, def, ss::IntTy.into(), kd)?
                    }
                    | su::Internal::Char => {
                        let kd = ty.unwrap();
                        ctx = self.register_prim_ty(ctx, def, ss::CharTy.into(), kd)?
                    }
                    | su::Internal::String => {
                        let kd = ty.unwrap();
                        ctx = self.register_prim_ty(ctx, def, ss::StringTy.into(), kd)?
                    }
                    | su::Internal::OS => {
                        let kd = ty.unwrap();
                        ctx = self.register_prim_ty(ctx, def, ss::OSTy.into(), kd)?
                    }
                    | su::Internal::Monad | su::Internal::Algebra => {
                        unreachable!()
                    }
                }
            }
            | None => {
                // the alias head is a primitive value that needs to be linked later
                let Some(ty) = ty else { Err(TyckError::MissingAnnotation)? };
                let ty = ctx.as_env(ty).tyck(self, ByAction::syn())?.as_ann().as_type();
                let _ = ctx.as_env(binder).tyck(self, ByAction::ana(ty.into()))?;
            }
        }
        Ok(ctx)
    }
}

pub enum MonadOrAlgebra {
    Monad(ss::TypeId),
    Algebra(ss::TypeId, ss::TypeId),
}

impl Tycker {
    pub fn monad_or_algebra(
        &self, env: &ss::Context<ss::AnnId>, ty: ss::TypeId,
    ) -> Option<MonadOrAlgebra> {
        match self.statics.types.get(&ty).clone()? {
            | ss::Type::App(ss::App(head, ty_1)) => match self.statics.types.get(&head).clone()? {
                | ss::Type::Abst(mo) => {
                    // check if mo is monad
                    let ss::AnnId::Type(id) = env[self.prim.algebra.get()] else { unreachable!() };
                    let ss::Type::Abst(mo_real) = self.statics.types.get(&id).clone()? else {
                        unreachable!()
                    };
                    if mo == mo_real {
                        Some(MonadOrAlgebra::Monad(*ty_1))
                    } else {
                        None
                    }
                }
                | ss::Type::App(ss::App(head, ty_0)) => {
                    match self.statics.types.get(&head).clone()? {
                        | ss::Type::Abst(alg) => {
                            // check if alg is algebra
                            let ss::AnnId::Type(id) = env[self.prim.algebra.get()] else {
                                unreachable!()
                            };
                            let ss::Type::Abst(alg_real) = self.statics.types.get(&id).clone()?
                            else {
                                unreachable!()
                            };
                            if alg == alg_real {
                                Some(MonadOrAlgebra::Algebra(*ty_0, *ty_1))
                            } else {
                                None
                            }
                        }
                        | _ => None,
                    }
                }
                | _ => None,
            },
            | _ => None,
        }
    }
}
