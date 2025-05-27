use crate::{syntax::*, *};
use zydeco_utils::arena::ArenaAccess;

/* ---------------------------------- Term ---------------------------------- */

impl Tycker {
    pub fn register_prim_ty(
        &mut self, mut env: TyEnvT<()>, def: DefId, prim: Type, syn_kd: su::TermId,
    ) -> ResultKont<TyEnvT<()>> {
        let kd = match env.mk(syn_kd).tyck_k(self, Action::syn())?.as_term_static() {
            | AnnId::Kind(kd) => kd,
            | AnnId::Set | AnnId::Type(_) => unreachable!(),
        };
        let ty = Alloc::alloc(self, prim, kd);
        self.statics.annotations_var.insert(def, kd.into());
        env.env += [(def, ty.into())];
        Ok(env)
    }
    pub fn register_prim_decl(
        &mut self, decl: su::AliasHead, id: &su::DeclId, mut env: TyEnvT<()>,
    ) -> ResultKont<TyEnvT<()>> {
        let su::AliasHead { binder, ty } = decl;
        let internal_or = self.scoped.exts.get(id).cloned();
        match internal_or {
            | Some((internal, def)) => {
                // the alias head is a internal type / kind
                match internal {
                    | su::Internal::VType => {
                        let kd = Alloc::alloc(self, VType, ());
                        self.statics.annotations_var.insert(def, AnnId::Set);
                        env.env += [(def, kd.into())];
                        // should also be added to global
                        self.statics.global_defs.insert(def, ());
                    }
                    | su::Internal::CType => {
                        let kd = Alloc::alloc(self, CType, ());
                        self.statics.annotations_var.insert(def, AnnId::Set);
                        env.env += [(def, kd.into())];
                        // should also be added to global
                        self.statics.global_defs.insert(def, ());
                    }
                    | su::Internal::Thk => {
                        let kd = ty.unwrap();
                        env = self.register_prim_ty(env, def, ThkTy.into(), kd)?;
                        // should also be added to global
                        self.statics.global_defs.insert(def, ());
                    }
                    | su::Internal::Ret => {
                        let kd = ty.unwrap();
                        env = self.register_prim_ty(env, def, RetTy.into(), kd)?;
                        // should also be added to global
                        self.statics.global_defs.insert(def, ());
                    }
                    | su::Internal::Unit => {
                        let kd = ty.unwrap();
                        env = self.register_prim_ty(env, def, UnitTy.into(), kd)?;
                        // should also be added to global
                        self.statics.global_defs.insert(def, ());
                    }
                    | su::Internal::Int => {
                        let kd = ty.unwrap();
                        env = self.register_prim_ty(env, def, IntTy.into(), kd)?;
                        // should NOT be added to global
                    }
                    | su::Internal::Char => {
                        let kd = ty.unwrap();
                        env = self.register_prim_ty(env, def, CharTy.into(), kd)?;
                        // should NOT be added to global
                    }
                    | su::Internal::String => {
                        let kd = ty.unwrap();
                        env = self.register_prim_ty(env, def, StringTy.into(), kd)?;
                        // should NOT be added to global
                    }
                    | su::Internal::OS => {
                        let kd = ty.unwrap();
                        env = self.register_prim_ty(env, def, OSTy.into(), kd)?;
                        // should NOT be added to global
                    }
                    | su::Internal::Top | su::Internal::Monad | su::Internal::Algebra => {
                        unreachable!()
                    }
                    // | su::Internal::Top => {
                    //     let kd = ty.unwrap();
                    //     env = self.register_prim_ty(env, def, TopTy.into(), kd)?;
                    //     // should NOT be added to global
                    // }
                    // | su::Internal::Monad => {
                    //     let kd = ty.unwrap();
                    //     env = self.register_prim_ty(env, def, MonadTy.into(), kd)?;
                    //     // should NOT be added to global
                    // }
                    // | su::Internal::Algebra => {
                    //     let kd = ty.unwrap();
                    //     env = self.register_prim_ty(env, def, AlgebraTy.into(), kd)?;
                    //     // should NOT be added to global
                    // }
                }
            }
            | None => {
                // the alias head is a primitive value that needs to be linked later
                let Some(ty) = ty else {
                    self.err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?
                };
                let ty = match env.mk(ty).tyck_k(self, Action::syn())?.as_term_static() {
                    | AnnId::Type(ty) => ty,
                    | AnnId::Set | AnnId::Kind(_) => unreachable!(),
                };
                let pat_out_ann = env.mk(binder).tyck_k(self, Action::ana(ty.into()))?;
                let binder = match pat_out_ann {
                    | PatAnnId::Type(_, _) => unreachable!(),
                    | PatAnnId::Value(vpat, _) => vpat,
                };
                self.statics.decls.insert(*id, VAliasHead { binder, ty }.into());

                // should NOT be added to global
                // match binder.try_destruct_def(self) {
                //     | (Some(def), _) => {
                //         self.statics.global_defs.insert(def, ());
                //     }
                //     | (None, _) => {}
                // }
            }
        }
        Ok(env)
    }
}
