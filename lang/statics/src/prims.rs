use crate::{syntax::*, *};
use zydeco_utils::arena::ArenaAccess;

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
    pub fn thunk(&mut self, env: &Env<AnnId>) -> TypeId {
        let AnnId::Type(ty) = env[self.prim.thunk.get()] else { unreachable!() };
        ty
    }
    /// generates `Thunk B`
    pub fn thunk_arg(&mut self, env: &Env<AnnId>, arg: TypeId) -> TypeId {
        let thunk = self.thunk(env);
        let vtype = self.vtype(env);
        Alloc::alloc(self, App(thunk, arg), vtype)
    }
    /// generates `Thunk _`
    pub fn thunk_hole(&mut self, env: &Env<AnnId>, site: su::TermId) -> TypeId {
        let fill = self.statics.fills.alloc(site);
        let ctype = self.ctype(env);
        let hole = Alloc::alloc(self, fill, ctype);
        self.thunk_arg(env, hole)
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
    pub fn algebra_mo_carrier(&mut self, env: &Env<AnnId>, mo: TypeId, carrier: TypeId) -> TypeId {
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

impl Tycker {
    pub fn register_prim_ty(
        &mut self, mut env: SEnv<()>, def: DefId, prim: Type, syn_kd: su::TermId,
    ) -> ResultKont<SEnv<()>> {
        let kd = match env.mk(syn_kd).tyck(self, Action::syn())?.as_term_static() {
            | AnnId::Kind(kd) => kd,
            | AnnId::Set | AnnId::Type(_) => unreachable!(),
        };
        let ty = Alloc::alloc(self, prim, kd);
        self.statics.annotations_var.insert(def, kd.into());
        env.env += (def, ty.into());
        Ok(env)
    }
    pub fn register_prim_decl(
        &mut self, decl: su::AliasHead, id: &su::DeclId, mut env: SEnv<()>,
    ) -> ResultKont<SEnv<()>> {
        let su::AliasHead { binder, ty } = decl;
        let internal_or = self.scoped.exts.get(id).cloned();
        match internal_or {
            | Some((internal, def)) => {
                // the alias head is a internal type; unless it's VType or CType
                match internal {
                    | su::Internal::VType => {
                        let kd = Alloc::alloc(self, VType, ());
                        self.statics.annotations_var.insert(def, AnnId::Set);
                        env.env += (def, kd.into());
                    }
                    | su::Internal::CType => {
                        let kd = Alloc::alloc(self, CType, ());
                        self.statics.annotations_var.insert(def, AnnId::Set);
                        env.env += (def, kd.into());
                    }
                    | su::Internal::Thunk => {
                        let kd = ty.unwrap();
                        env = self.register_prim_ty(env, def, ThunkTy.into(), kd)?
                    }
                    | su::Internal::Ret => {
                        let kd = ty.unwrap();
                        env = self.register_prim_ty(env, def, RetTy.into(), kd)?
                    }
                    | su::Internal::Unit => {
                        let kd = ty.unwrap();
                        env = self.register_prim_ty(env, def, UnitTy.into(), kd)?
                    }
                    | su::Internal::Int => {
                        let kd = ty.unwrap();
                        env = self.register_prim_ty(env, def, IntTy.into(), kd)?
                    }
                    | su::Internal::Char => {
                        let kd = ty.unwrap();
                        env = self.register_prim_ty(env, def, CharTy.into(), kd)?
                    }
                    | su::Internal::String => {
                        let kd = ty.unwrap();
                        env = self.register_prim_ty(env, def, StringTy.into(), kd)?
                    }
                    | su::Internal::OS => {
                        let kd = ty.unwrap();
                        env = self.register_prim_ty(env, def, OSTy.into(), kd)?
                    }
                    | su::Internal::Top | su::Internal::Monad | su::Internal::Algebra => {
                        unreachable!()
                    }
                }
            }
            | None => {
                // the alias head is a primitive value that needs to be linked later
                let Some(ty) = ty else {
                    self.err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?
                };
                let ty = match env.mk(ty).tyck(self, Action::syn())?.as_term_static() {
                    | AnnId::Type(ty) => ty,
                    | AnnId::Set | AnnId::Kind(_) => unreachable!(),
                };
                let pat_out_ann = env.mk(binder).tyck(self, Action::ana(ty.into()))?;
                let binder = match pat_out_ann {
                    | PatAnnId::Type(_, _) => unreachable!(),
                    | PatAnnId::Value(vpat, _) => vpat,
                };
                self.statics.decls.insert(id.clone(), VAliasHead { binder, ty }.into());
            }
        }
        Ok(env)
    }
}

pub enum MonadOrAlgebra {
    Monad(TypeId),
    Algebra(TypeId, TypeId),
}

pub enum Structure {
    Top,
    Algebra(TypeId, TypeId),
    // Arrow(Box<Structure>, Box<Structure>),
}

impl Tycker {
    /// If ty is `Thunk (Monad M)` or `Thunk (Algebra M R)`, returns `Some(Monad(M))` or `Some(Algebra(M, R))` respectively.
    pub fn monad_or_algebra(&self, env: &Env<AnnId>, ty: TypeId) -> Option<MonadOrAlgebra> {
        let ty = match self.statics.types.get(&ty)? {
            | Type::App(ty) => {
                let App(head, body) = ty;
                match self.statics.types.get(&head).clone()? {
                    | Type::Thunk(_) => {}
                    | _ => None?,
                }
                body
            }
            | _ => None?,
        };
        match self.statics.types.get(&ty)? {
            | Type::App(App(head, ty_1)) => match self.statics.types.get(&head).clone()? {
                | Type::Abst(mo) => {
                    // check if mo is monad
                    let AnnId::Type(id) = env[self.prim.monad.get()] else { unreachable!() };
                    let Type::Abst(mo_real) = self.statics.types.get(&id).clone()? else {
                        unreachable!()
                    };
                    if mo == mo_real {
                        Some(MonadOrAlgebra::Monad(*ty_1))
                    } else {
                        None
                    }
                }
                | Type::App(App(head, ty_0)) => {
                    match self.statics.types.get(&head).clone()? {
                        | Type::Abst(alg) => {
                            // check if alg is algebra
                            let AnnId::Type(id) = env[self.prim.algebra.get()] else {
                                unreachable!()
                            };
                            let Type::Abst(alg_real) = self.statics.types.get(&id).clone()? else {
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
