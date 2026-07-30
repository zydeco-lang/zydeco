use super::{syntax::*, *};
use zydeco_utils::arena::ArenaAccess;

/* ---------------------------------- Term ---------------------------------- */

impl Tycker<'_> {
    pub fn register_prim_ty(
        &mut self, mut env: TyEnvT<()>, def: DefId, prim: Type, syn_kd: su::TermId,
    ) -> ResultKont<TyEnvT<()>> {
        let kd = match env.mk(syn_kd).tyck_k(self, Action::syn())?.as_term_static() {
            | AnnId::Kind(kd) => kd,
            | AnnId::Set | AnnId::Type(_) => unreachable!(),
        };
        let ty = Alloc::alloc(self, prim, kd, &env.info);
        self.statics.annotations_var.insert_new(def, kd.into());
        env.info += [(def, ty.into())];
        Ok(env)
    }
    pub fn register_prim_decl(
        &mut self, external: su::External, id: &su::BindingId, mut env: TyEnvT<()>,
    ) -> ResultKont<TyEnvT<()>> {
        let su::External { binder, classifier: ty } = external;
        let internal_or = self.scoped.exts.get(id).cloned();
        match internal_or {
            | Some((internal, def)) => {
                // the alias head is a internal type / kind
                match internal {
                    | su::Internal::VType => {
                        let kd = Alloc::alloc(self, VType, (), &());
                        self.statics.annotations_var.insert_new(def, AnnId::Set);
                        env.info += [(def, kd.into())];
                        // should also be added to global
                        self.statics.global_defs.ensure(def);
                    }
                    | su::Internal::CType => {
                        let kd = Alloc::alloc(self, CType, (), &());
                        self.statics.annotations_var.insert_new(def, AnnId::Set);
                        env.info += [(def, kd.into())];
                        // should also be added to global
                        self.statics.global_defs.ensure(def);
                    }
                    | su::Internal::Thk => {
                        let kd = ty.unwrap();
                        env = self.register_prim_ty(env, def, ThkTy.into(), kd)?;
                        // should also be added to global
                        self.statics.global_defs.ensure(def);
                    }
                    | su::Internal::Ret => {
                        let kd = ty.unwrap();
                        env = self.register_prim_ty(env, def, RetTy.into(), kd)?;
                        // should also be added to global
                        self.statics.global_defs.ensure(def);
                    }
                    | su::Internal::Unit => {
                        let kd = ty.unwrap();
                        env = self.register_prim_ty(env, def, UnitTy.into(), kd)?;
                        // should also be added to global
                        self.statics.global_defs.ensure(def);
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
                    | su::Internal::Monad | su::Internal::Algebra => {
                        // they are AliasBodys, which are not registered here
                        unreachable!()
                    }
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
                let pat_out_ann = env.mk(binder).tyck_k(self, PatternAction::ana(ty.into()))?;
                let (binder, _) = pat_out_ann.as_value();
                self.statics.decls.insert_new(*id, VAliasHead { binder, ty }.into());

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
