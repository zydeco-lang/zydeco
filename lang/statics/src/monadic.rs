//! Implementation of the monadic block via algebra translation.
//! See the following for details:
//!
//! + Core Idea: [https://dl.acm.org/doi/10.1145/3720434](section 5.3 of oopsla25)
//! + Detailed Implementation: [https://arxiv.org/abs/2502.15031](appendix D of the extended version)

use crate::{syntax::*, *};

pub mod syntax {
    /// signature translation
    #[derive(Clone, Copy)]
    pub struct Signature<T> {
        pub ty: T,
    }
    /// structure translation
    #[derive(Clone)]
    pub struct Structure<T> {
        pub ty: T,
    }
    /// type translation (lift)
    #[derive(Clone, Copy)]
    pub struct TypeLift<T> {
        pub ty: T,
    }
    /// term translation (lift)
    #[derive(Clone)]
    pub struct TermLift<T> {
        pub tm: T,
    }
    /// monadic elaboration
    #[derive(Clone)]
    pub struct Elaboration<T> {
        pub tm: T,
    }

    // /// invoke substitution on defs
    // pub struct Subst<T>(pub T);

    /// structure pattern introduction
    ///
    /// need to register the fresh var `S` as a structure of a given (abstract) type `A`
    pub struct StrPat<S, A, T>(pub S, pub A, pub T);
}

mod syntax_impl {
    use super::*;

    // SignatureTrans
    impl<T> MonConstruct<TypeId> for cs::Signature<T>
    where
        T: MonConstruct<TypeId>,
    {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
            let cs::Signature { ty } = self;
            let (env, ty) = ty.mbuild(tycker, env)?;
            signature_translation(tycker, env, ty)
        }
    }

    // StructureTrans
    impl<T> MonConstruct<CompuId> for cs::Structure<T>
    where
        T: MonConstruct<TypeId>,
    {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
            let cs::Structure { ty } = self;
            let (env, ty) = ty.mbuild(tycker, env)?;
            structure_translation(tycker, env, ty)
        }
    }

    // TypeLift (type pattern translation)
    impl<T> MonConstruct<TPatId> for cs::TypeLift<T>
    where
        T: MonConstruct<TPatId>,
    {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TPatId)> {
            let cs::TypeLift { ty } = self;
            let (env, ty) = ty.mbuild(tycker, env)?;
            type_pattern_translation(tycker, env, ty)
        }
    }

    // TypeLift
    impl<T> MonConstruct<TypeId> for cs::TypeLift<T>
    where
        T: MonConstruct<TypeId>,
    {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
            let cs::TypeLift { ty } = self;
            let (env, ty) = ty.mbuild(tycker, env)?;
            type_translation(tycker, env, ty)
        }
    }

    // TermLift (value pattern translation)
    impl<T> MonConstruct<VPatId> for cs::TermLift<T>
    where
        T: MonConstruct<VPatId>,
    {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, VPatId)> {
            let cs::TermLift { tm } = self;
            let (env, tm) = tm.mbuild(tycker, env)?;
            value_pattern_translation(tycker, env, tm)
        }
    }

    // TermLift (value translation)
    impl<T> MonConstruct<ValueId> for cs::TermLift<T>
    where
        T: MonConstruct<ValueId>,
    {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, ValueId)> {
            let cs::TermLift { tm } = self;
            let (env, tm) = tm.mbuild(tycker, env)?;
            value_translation(tycker, env, tm)
        }
    }

    // TermLift (computation translation)
    impl<T> MonConstruct<CompuId> for cs::TermLift<T>
    where
        T: MonConstruct<CompuId>,
    {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
            let cs::TermLift { tm } = self;
            let (env, tm) = tm.mbuild(tycker, env)?;
            computation_translation(tycker, env, tm)
        }
    }

    // Elaboration (value)
    impl<T> MonConstruct<ValueId> for cs::Elaboration<T>
    where
        T: MonConstruct<ValueId>,
    {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, ValueId)> {
            let cs::Elaboration { tm } = self;
            let (env, tm) = tm.mbuild(tycker, env)?;
            value_monadic_elaboration(tycker, env, tm)
        }
    }

    // Elaboration (computation)
    impl<T> MonConstruct<CompuId> for cs::Elaboration<T>
    where
        T: MonConstruct<CompuId>,
    {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
            let cs::Elaboration { tm } = self;
            let (env, tm) = tm.mbuild(tycker, env)?;
            computation_monadic_elaboration(tycker, env, tm)
        }
    }

    // StrPat
    impl<S, A, T> MonConstruct<VPatId> for cs::StrPat<S, A, T>
    where
        S: MonConstruct<VarName>,
        A: MonConstruct<AbstId>,
        T: MonConstruct<Option<DefId>>,
    {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, VPatId)> {
            let cs::StrPat(var, abst, tvar) = self;
            let (env, abst) = abst.mbuild(tycker, env)?;
            let (env, ty) = cs::Thk(cs::Signature { ty: cs::Type(abst) }).mbuild(tycker, env)?;
            let (env, var) = var.mbuild(tycker, env)?;
            let def = Alloc::alloc(tycker, var, ty.into());
            let (env, str) = cs::Value(def).mbuild(tycker, env)?;
            let (env, tvar) = tvar.mbuild(tycker, env)?;
            let mut env = env;
            env.structure.absts.insert(abst, str);
            if let Some(tvar) = tvar {
                env.structure.def_map.insert(tvar, abst);
            }
            let (env, ty) = ty.mbuild(tycker, env)?;
            cs::Pat(def, ty).mbuild(tycker, env)
        }
    }
}

/// Signature Translation `Sig_K(T)`
///
/// Given the monad type `M` and the type environment, implement the signature
/// translation from input type `T` to an output computation type `CompuId`
/// which is the type of algebra of `T`. Specifically,
///
/// + `T: VType` -> `Top`
/// + `T: CType` -> `Algebra M T`
/// + `T: K_1 -> K_2` -> `forall X: K_1 . Thk (Sig_K_1(X)) -> Sig_K_2(T X)`
fn signature_translation(tycker: &mut Tycker, env: MonEnv, ty: TypeId) -> Result<(MonEnv, TypeId)> {
    let (env, kd) = cs::TypeOf(ty).mbuild(tycker, env)?;
    let res = match tycker.kind_filled(&kd)?.to_owned() {
        | Kind::VType(VType) => cs::TopTy.mbuild(tycker, env)?,
        | Kind::CType(CType) => cs::Algebra(env.monad_ty, ty).mbuild(tycker, env)?,
        | Kind::Arrow(Arrow(kd_1, _)) => cs::Forall(cs::Ann(None, kd_1), |abst| {
            let ty_1 = cs::Ann(abst, kd_1);
            Arrow(cs::Thk(cs::Signature { ty: ty_1 }), cs::Signature { ty: App(ty, ty_1) })
        })
        .mbuild(tycker, env)?,
    };
    Ok(res)
}

/// Structure Translation `Str(T)`
fn structure_translation(
    tycker: &mut Tycker, env: MonEnv, ty: TypeId,
) -> Result<(MonEnv, CompuId)> {
    let monad_ty = env.monad_ty;
    let monad_impl = env.monad_impl;
    let res = match tycker.type_filled(&ty)? {
        | Type::Var(def) => {
            match env.structure.def_map.get(&def).map(|abst| env.structure.absts[abst]) {
                | Some(str) => Force(str).mbuild(tycker, env)?,
                | None => {
                    let (env, sig) = cs::Signature { ty }.mbuild(tycker, env)?;
                    (env, Alloc::alloc(tycker, Hole, sig))
                    // tycker.err(TyckError::MissingStructure(ty), std::panic::Location::caller())?
                }
            }
        }
        | Type::Abst(abst) => match env.structure.absts.get(&abst).cloned() {
            | Some(str) => Force(str).mbuild(tycker, env)?,
            | None => {
                logg::warn!(
                    "backtracking structure for abstract type: {}",
                    tycker.dump_statics(abst)
                );
                use zydeco_utils::arena::ArenaAccess;
                match tycker.statics.abst_hints.get(&abst).cloned() {
                    | Some(def) => {
                        logg::warn!("backtracked to definition: {}", tycker.dump_statics(def));
                        let kd = tycker.statics.annotations_type[&ty];
                        let ty: TypeId = Alloc::alloc(tycker, def, kd);
                        cs::Structure { ty }.mbuild(tycker, env)?
                    }
                    | None => {
                        // tycker.err(TyckError::MissingStructure(ty), std::panic::Location::caller())?
                        let (env, sig) = cs::Signature { ty }.mbuild(tycker, env)?;
                        (env, Alloc::alloc(tycker, Hole, sig))
                    }
                }
            }
        },
        | Type::Abs(ty) => {
            // input: fn (X : K) -> S
            let Abs(tpat, ty) = ty;
            let svar = {
                let (tvar, _) = tpat.try_destruct_def(tycker);
                match tvar {
                    | Some(tvar) => format!("str_{}", tycker.scoped.defs[&tvar].as_str()),
                    | None => "str".to_string(),
                }
            };
            // output: fn (X : K) (str_X : Thk (Sig_K(X))) -> Str(S)
            Abs(cs::Ty(cs::TypeLift { ty: tpat }), move |_tvar, abst| {
                Abs(cs::StrPat(svar, abst, None), move |_str: VPatId| cs::Structure { ty })
            })
            .mbuild(tycker, env)?
        }
        | Type::App(ty) => {
            // input: S_f S_a
            let App(ty_f, ty_a) = ty;
            // output: Str(S_f) Lift(S_a) { Str(S_a) }
            let str_f = { cs::Structure { ty: ty_f } };
            let ty_a_lift = cs::TypeLift { ty: ty_a };
            let str_a = cs::Structure { ty: ty_a };
            App(App(str_f, cs::Ty(ty_a_lift)), Thunk(str_a)).mbuild(tycker, env)?
        }
        // primitive types are not allowed in monadic blocks
        | Type::Int(_) | Type::Char(_) | Type::String(_) => unreachable!(),
        // unit, product, and existential types have the trivial structure `top`
        // (so should data types)
        | Type::Unit(UnitTy) | Type::Prod(_) | Type::Exists(_) | Type::Data(_) => {
            cs::Top.mbuild(tycker, env)?
        }
        // the thunk type is itself a type constructor,
        // so its structure takes a type and the type's structure as arguments
        | Type::Thk(ThkTy) => {
            // output: fn (X : CType) (_ : Thk (Sig_CType(X))) -> <top>
            Abs(cs::Ty(cs::Pat("_", CType)), |_tvar, abst| {
                let thk_sig = cs::Thk(cs::Signature { ty: cs::Ty(abst) });
                Abs(cs::Pat("_", thk_sig), |_var| {
                    // <top> = comatch end
                    cs::Top
                })
            })
            .mbuild(tycker, env)?
        }
        // the os type is a primitive computation type and thus not allowed in monadic blocks
        | Type::OS(_) => unreachable!(),
        | Type::Ret(RetTy) => {
            // output: fn (X : VType) (_ : Thk (Sig_VType(X))) -> <monadic_bind>
            Abs(cs::Ty(cs::Pat("_", VType)), |_tvar, abst_x| {
                let abst_x_ty = cs::Ty(abst_x);
                let thk_sig = cs::Thk(cs::Signature { ty: abst_x_ty });
                Abs(cs::Pat("_", thk_sig), move |_var| {
                    // <monadic_bind> = fn (Z : VType) -> ! monad_impl .bind Z X
                    Abs(cs::Ty(cs::Pat("Z", VType)), move |_tvar, abst_z| {
                        let abst_z_ty = cs::Ty(abst_z);
                        let body = cs::Dtor(Force(monad_impl), ".bind");
                        App(App(body, cs::Ty(abst_z_ty)), cs::Ty(abst_x_ty))
                    })
                })
            })
            .mbuild(tycker, env)?
        }
        | Type::CoData(coda) => {
            // output: fn (Z : VType) (mz : Thk (M Z)) (f: Thk (Z -> &_n[B_n])) -> <body>
            Abs(cs::Ty(cs::Pat("Z", VType)), |_tvar, abst_z| {
                let abst_z_ty = cs::Ty(abst_z);
                let mz_ty = cs::Thk(App(monad_ty, abst_z_ty));
                Abs(cs::Pat("mz", mz_ty), move |mz: VPatId| {
                    let ty_ = cs::TypeLift { ty };
                    let f_ty = cs::Thk(Arrow(abst_z_ty, ty_));
                    Abs(cs::Pat("f", f_ty), move |f: VPatId| {
                        // <body> =
                        // comatch
                        // | .dtor_n -> Str(B_n) Z mz { fn (z : Z) -> ! f z .dtor_n }
                        // | ...
                        // end
                        cs::CoMatch(coda, move |dtor, ty| {
                            let kont = Abs(cs::Pat("z", abst_z_ty), move |z: VPatId| {
                                Dtor(App(Force(cs::Value(f)), cs::Value(z)), dtor)
                            });
                            let str = cs::Structure { ty };
                            App(App(App(str, abst_z_ty), cs::Value(mz)), Thunk(kont))
                        })
                    })
                })
            })
            .mbuild(tycker, env)?
        }
        | Type::Arrow(ty) => {
            // input: A -> B
            let Arrow(ty_p, ty_b) = ty;
            // output: fn (Z : VType) (mz : Thk (M Z)) (f : Thk (Z -> [A] -> [B])) -> <body>
            Abs(cs::Ty(cs::Pat("Z", VType)), move |_tvar, abst_z| {
                let abst_z_ty = cs::Ty(abst_z);
                let mz_ty = cs::Thk(App(env.monad_ty, abst_z_ty));
                Abs(cs::Pat("mz", mz_ty), move |mz: VPatId| {
                    let ty_p_ = cs::TypeLift { ty: ty_p };
                    let ty_b_ = cs::TypeLift { ty: ty_b };
                    let f_ty = cs::Thk(Arrow(abst_z_ty, Arrow(ty_p_, ty_b_)));
                    Abs(cs::Pat("f", f_ty), move |f: VPatId| {
                        // <body> = fn (x : [A]) -> Str(B) Z mz { fn (z : Z) -> ! f z x }
                        Abs(cs::Pat("x", ty_p_), move |x: VPatId| {
                            let alg_b = cs::Structure { ty: ty_b };
                            let kont = Abs(cs::Pat("z", abst_z_ty), move |z: VPatId| {
                                App(App(Force(cs::Value(f)), cs::Value(z)), cs::Value(x))
                            });
                            App(App(App(alg_b, cs::Ty(abst_z_ty)), cs::Value(mz)), Thunk(kont))
                        })
                    })
                })
            })
            .mbuild(tycker, env)?
        }
        | Type::Forall(ty) => {
            // input: forall (X : K) . B
            let Forall(abst, ty) = ty;
            let kd = cs::TypeOf(abst);
            // output: fn (Z : VType) (mz : Thk (M Z)) (f : <f_ty>) -> <body>
            Abs(cs::Ty(cs::Pat("Z", VType)), move |_tvar, abst_z| {
                let abst_z_ty = cs::Ty(abst_z);
                Abs(cs::Pat("mz", cs::Thk(App(env.monad_ty, abst_z_ty))), move |mz: VPatId| {
                    // construct abstract type X first
                    cs::CBind::new(cs::Ann("X", kd), move |abst_x| {
                        // <f_ty> = Thk (Z -> forall (X : K) . Thk (Sig_K(X)) -> [B])
                        let f_ty = cs::Thk(Arrow(
                            abst_z_ty,
                            cs::Forall(abst_x, move |abst_x: AbstId| {
                                let abst_x_ty = cs::Ty(abst_x);
                                Arrow(cs::Thk(cs::Signature { ty: abst_x_ty }), cs::TypeLift { ty })
                            }),
                        ));
                        Abs(cs::Pat("f", f_ty), move |f: VPatId| {
                            // <body> = fn (X : K) (str_X : Thk (Sig_K(X))) -> Str(B) Z mz <kont>
                            Abs(cs::Ty(abst_x), move |_, abst_x: AbstId| {
                                let abst_x_ty = cs::Ty(abst_x);
                                let sig = cs::Signature { ty: abst_x_ty };
                                Abs(cs::Pat("str_X", cs::Thk(sig)), move |str_x: VPatId| {
                                    // <kont> = { fn (z : Z) -> ! f z X str_X }
                                    let kont = Abs(cs::Pat("z", abst_z_ty), move |z: VPatId| {
                                        let f = cs::Value(f);
                                        let z = cs::Value(z);
                                        let str_x = cs::Value(str_x);
                                        App(App(App(Force(f), z), abst_x_ty), str_x)
                                    });
                                    let str_ = cs::Structure { ty };
                                    App(App(App(str_, abst_z_ty), cs::Value(mz)), Thunk(kont))
                                })
                            })
                        })
                    })
                })
            })
            .mbuild(tycker, env)?
        }
    };
    Ok(res)
}

/// Type Pattern Translation `[TPat]`
fn type_pattern_translation(
    tycker: &mut Tycker, env: MonEnv, tpat: TPatId,
) -> Result<(MonEnv, TPatId)> {
    use TypePattern as TPat;
    let (env, kd) = cs::TypeOf(tpat).mbuild(tycker, env)?;
    let (env, tpat_) = match tycker.tpat(&tpat) {
        | TPat::Hole(hole) => cs::Pat(hole, kd).mbuild(tycker, env)?,
        | TPat::Var(def) => cs::Pat(def, kd).mbuild(tycker, env)?,
    };
    Ok((env, tpat_))
}

/// Carrier (Type) Translation `[T]`
fn type_translation(tycker: &mut Tycker, env: MonEnv, ty: TypeId) -> Result<(MonEnv, TypeId)> {
    let monad_ty = env.monad_ty;
    let (env, kd) = cs::TypeOf(ty).mbuild(tycker, env)?;
    let (env, res) = match tycker.type_filled(&ty)?.to_owned() {
        | Type::Var(def) => {
            // substitute according to the environment
            let Some(def_) = env.subst.get(&def).cloned() else {
                tycker.err(TyckError::NotInlinable(def), std::panic::Location::caller())?
            };
            (env, Alloc::alloc(tycker, def_, kd))
        }
        | Type::Abst(abst) => {
            // // Debug: log the abst type
            // {
            //     use zydeco_utils::arena::ArenaAccess;
            //     let hint = match tycker.statics.abst_hints.get(&abst) {
            //         | Some(hint) => tycker.dump_scoped(hint),
            //         | None => "<unknown>".to_string(),
            //     };
            //     logg::warn!(
            //         "carrier translation of {}({}) may leak",
            //         tycker.dump_statics(abst),
            //         hint
            //     );
            // }
            // only types that are not sealed are allowed here
            use zydeco_utils::arena::ArenaAccess;
            match tycker.statics.seals.get(&abst) {
                | Some(_) => {
                    tycker.err(TyckError::NotInlinableSeal(abst), std::panic::Location::caller())?
                }
                | None => (env, Alloc::alloc(tycker, abst, kd)),
            }
        }
        // | Type::Abst(_abst) => unreachable!(),
        | Type::Abs(ty) => {
            let Abs(tpat, ty) = ty;
            // Fixme: bind environment
            Abs(cs::TypeLift { ty: tpat }, |_, _, _| cs::TypeLift { ty }).mbuild(tycker, env)?
        }
        | Type::App(ty) => {
            let App(ty_f, ty_a) = ty;
            let ty_f_ = cs::TypeLift { ty: ty_f };
            let ty_a_ = cs::TypeLift { ty: ty_a };
            App(ty_f_, ty_a_).mbuild(tycker, env)?
        }
        | Type::Thk(ThkTy) => (env, Alloc::alloc(tycker, ThkTy, kd)),
        // primitive types are not allowed in monadic blocks
        | ty @ (Type::Int(_) | Type::Char(_) | Type::String(_)) => {
            let def = {
                if let Type::Int(_) = ty {
                    tycker.prim.int.get().to_owned()
                } else if let Type::Char(_) = ty {
                    tycker.prim.char.get().to_owned()
                } else if let Type::String(_) = ty {
                    tycker.prim.string.get().to_owned()
                } else {
                    unreachable!()
                }
            };
            tycker.err(TyckError::NotInlinable(def), std::panic::Location::caller())?
        }
        | Type::Data(data) => {
            cs::Data(data, |_ctor, ty| cs::TypeLift { ty }).mbuild(tycker, env)?
        }
        | Type::Unit(UnitTy) => UnitTy.mbuild(tycker, env)?,
        | Type::Prod(ty) => {
            let Prod(ty_1, ty_2) = ty;
            let ty_1_ = cs::TypeLift { ty: ty_1 };
            let ty_2_ = cs::TypeLift { ty: ty_2 };
            Prod(ty_1_, ty_2_).mbuild(tycker, env)?
        }
        | Type::Exists(ty) => {
            let Exists(abst, ty) = ty;
            let abst_ty = cs::Ty(abst);
            let thk_sig = cs::Thk(cs::Signature { ty: abst_ty });
            let ty_ = cs::TypeLift { ty };
            let prod = Prod(thk_sig, ty_);
            cs::Exists(abst, |_| prod).mbuild(tycker, env)?
        }
        // os type is also not allowed in monadic blocks
        | Type::OS(_) => unreachable!(),
        // the return type is translated to the provided monad type
        | Type::Ret(RetTy) => (env, monad_ty),
        | Type::CoData(coda) => {
            cs::CoData(coda, |_dtor, ty| cs::TypeLift { ty }).mbuild(tycker, env)?
        }
        | Type::Arrow(ty) => {
            let Arrow(ty_1, ty_2) = ty;
            let ty_1_ = cs::TypeLift { ty: ty_1 };
            let ty_2_ = cs::TypeLift { ty: ty_2 };
            Arrow(ty_1_, ty_2_).mbuild(tycker, env)?
        }
        | Type::Forall(ty) => {
            let Forall(abst, ty) = ty;
            cs::Forall(abst, move |abst| {
                Arrow(cs::Thk(cs::Signature { ty: cs::Ty(abst) }), cs::TypeLift { ty })
            })
            .mbuild(tycker, env)?
        }
    };
    Ok((env, res))
}

/// Value Pattern Translation `[VPat]`
fn value_pattern_translation(
    tycker: &mut Tycker, env: MonEnv, vpat: VPatId,
) -> Result<(MonEnv, VPatId)> {
    use ValuePattern as VPat;
    let (env, ty) = cs::TypeOf(vpat).mbuild(tycker, env)?;
    let (env, ty_) = cs::TypeLift { ty }.mbuild(tycker, env)?;
    let (env, vpat_) = match tycker.vpat(&vpat) {
        | VPat::Hole(hole) => cs::Pat(hole, ty_).mbuild(tycker, env)?,
        | VPat::Var(def) => {
            // create a fresh variable, and track the substitution
            cs::Pat(def, ty_).mbuild(tycker, env)?
        }
        | VPat::Ctor(vpat) => {
            let Ctor(ctor, body) = vpat;
            let body_ = cs::TermLift { tm: body };
            cs::Pat(cs::Ctor(ctor, body_), ty_).mbuild(tycker, env)?
        }
        | VPat::Triv(triv) => triv.mbuild(tycker, env)?,
        | VPat::VCons(vpat) => {
            let Cons(a, b) = vpat;
            let a_ = cs::TermLift { tm: a };
            let b_ = cs::TermLift { tm: b };
            Cons(a_, b_).mbuild(tycker, env)?
        }
        | VPat::TCons(vpat) => {
            let Cons(tpat, body) = vpat;
            cs::Pat(
                cs::TCons(cs::TypeLift { ty: tpat }, move |tvar, abst| {
                    Cons(cs::StrPat("str", abst, tvar), cs::TermLift { tm: body })
                }),
                ty_,
            )
            .mbuild(tycker, env)?
        }
    };
    Ok((env, vpat_))
}

/// Term Translation (Value) `[V]`
fn value_translation(
    tycker: &mut Tycker, env: MonEnv, value: ValueId,
) -> Result<(MonEnv, ValueId)> {
    let (env, ty) = cs::TypeOf(value).mbuild(tycker, env)?;
    let (env, ty_) = cs::TypeLift { ty }.mbuild(tycker, env)?;
    let (env, res) = match tycker.value(&value).to_owned() {
        | Value::Hole(Hole) => cs::Ann(Hole, ty_).mbuild(tycker, env)?,
        // figure out how to handle literals
        | Value::Lit(_) => unreachable!(),
        | Value::Var(def) => {
            // substitute according to the environment
            match env.subst.get(&def).cloned() {
                | Some(def_) => {
                    // the definition is closed in this monadic block
                    (env, Alloc::alloc(tycker, def_, ty_))
                }
                | None => {
                    use zydeco_utils::arena::ArenaAccess;
                    // it should then be global and should be in the inlinables
                    let Some(value) = tycker.statics.inlinables.get(&def).cloned() else {
                        tycker.err(TyckError::NotInlinable(def), std::panic::Location::caller())?
                    };
                    cs::TermLift { tm: value }.mbuild(tycker, env)?
                }
            }
        }
        | Value::Thunk(value) => {
            let Thunk(body) = value;
            Thunk(cs::TermLift { tm: body }).mbuild(tycker, env)?
        }
        | Value::Ctor(value) => {
            let Ctor(ctor, body) = value;
            let body_ = cs::TermLift { tm: body };
            let ty_ = cs::TypeLift { ty };
            cs::Ann(cs::Ctor(ctor, body_), ty_).mbuild(tycker, env)?
        }
        | Value::Triv(Triv) => Triv.mbuild(tycker, env)?,
        | Value::VCons(value) => {
            let Cons(value_1, value_2) = value;
            let value_1_ = { cs::TermLift { tm: value_1 } };
            let value_2_ = cs::TermLift { tm: value_2 };
            Cons(value_1_, value_2_).mbuild(tycker, env)?
        }
        | Value::TCons(value) => {
            let Cons(a_ty, body) = value;
            let a_str = { cs::Structure { ty: a_ty } };
            let body_ = cs::TermLift { tm: body };
            let a_ty_ = cs::TypeLift { ty: a_ty };
            // existential type construct should be type-guided
            cs::Ann(Cons(cs::Ty(a_ty_), Cons(Thunk(a_str), body_)), ty_).mbuild(tycker, env)?
        }
    };
    Ok((env, res))
}

/// Term Translation (Computation) `[C]`
fn computation_translation(
    tycker: &mut Tycker, env: MonEnv, compu: CompuId,
) -> Result<(MonEnv, CompuId)> {
    use Computation as Compu;
    let (env, ty) = cs::TypeOf(compu).mbuild(tycker, env)?;

    // Debug: print
    {
        logg::trace!("{}", ">".repeat(20));
        logg::trace!("[begin] {} : {}", tycker.dump_statics(compu), tycker.dump_statics(ty));
        logg::trace!("@ {}", compu.span(tycker));
        logg::trace!("{}", "=".repeat(20));
        for (abst, str) in env.structure.absts.iter() {
            let defs = env
                .structure
                .def_map
                .iter()
                .filter_map(|(def, a)| (a == abst).then_some(def))
                .map(|d| tycker.dump_statics(*d))
                .collect::<Vec<_>>();
            logg::trace!(
                "{} ({}) := {}",
                tycker.dump_statics(abst),
                defs.join(", "),
                tycker.dump_statics(str)
            );
        }
        logg::trace!("{}", "<".repeat(20));
    }

    let (env, res) = match tycker.compu(&compu) {
        | Compu::Hole(Hole) => {
            let (env, ty_) = cs::TypeLift { ty }.mbuild(tycker, env)?;
            cs::Ann(Hole, ty_).mbuild(tycker, env)?
        }
        | Compu::VAbs(compu) => {
            let Abs(vpat, compu) = compu;
            Abs(cs::TermLift { tm: vpat }, move |_def| cs::TermLift { tm: compu })
                .mbuild(tycker, env)?
        }
        | Compu::VApp(compu) => {
            let App(fun, arg) = compu;
            let fun_ = { cs::TermLift { tm: fun } };
            let arg_ = cs::TermLift { tm: arg };
            App(fun_, arg_).mbuild(tycker, env)?
        }
        | Compu::TAbs(compu) => {
            let Abs(tpat, compu) = compu;
            let Some((abst, _)) = ty.destruct_forall(tycker) else { unreachable!() };
            Abs(cs::Ty((cs::TypeLift { ty: tpat }, abst)), move |_tpat, abst| {
                Abs(cs::StrPat("str", abst, None), move |_str| cs::TermLift { tm: compu })
            })
            .mbuild(tycker, env)?
        }
        | Compu::TApp(compu) => {
            let App(fun, arg) = compu;
            let fun_ = { cs::TermLift { tm: fun } };
            let arg_ = cs::TypeLift { ty: arg };
            let str_ = cs::Structure { ty: arg };
            App(App(fun_, cs::Ty(arg_)), Thunk(str_)).mbuild(tycker, env)?
        }
        | Compu::Fix(compu) => {
            let Fix(vpat, compu) = compu;
            Fix(cs::TermLift { tm: vpat }, move |_vpat| cs::TermLift { tm: compu })
                .mbuild(tycker, env)?
        }
        | Compu::Force(compu) => {
            let Force(value) = compu;
            Force(cs::TermLift { tm: value }).mbuild(tycker, env)?
        }
        | Compu::Ret(compu) => {
            let Ret(value) = compu;
            App(
                App(
                    cs::Dtor(Force(env.monad_impl), ".return"),
                    cs::Ty(cs::TypeLift { ty: cs::TypeOf(value) }),
                ),
                cs::TermLift { tm: value },
            )
            .mbuild(tycker, env)?
        }
        | Compu::Do(compu) => {
            let Bind { binder, bindee, tail } = compu;
            let str_ = cs::Structure { ty };
            let (env, ret_ty) = cs::TypeOf(bindee).mbuild(tycker, env)?;
            let Some(a_ty) = ret_ty.destruct_ret_app(tycker) else { unreachable!() };
            let a_ty_ = cs::TypeLift { ty: a_ty };
            let bindee_ = { cs::TermLift { tm: bindee } };
            let kont = Abs(cs::TermLift { tm: binder }, move |_var| cs::TermLift { tm: tail });
            App(App(App(str_, cs::Ty(a_ty_)), Thunk(bindee_)), Thunk(kont)).mbuild(tycker, env)?
        }
        | Compu::Let(compu) => {
            let PureBind { binder, bindee, tail } = compu;
            let bindee_ = cs::TermLift { tm: bindee };
            let binder_ = cs::TermLift { tm: binder };
            PureBind { binder: binder_, bindee: bindee_, tail: move |_| cs::TermLift { tm: tail } }
                .mbuild(tycker, env)?
        }
        | Compu::Match(_) => {
            // let Match { scrut, arms } = compu;
            todo!()
        }
        | Compu::CoMatch(compu) => {
            let (env, ty_) = cs::TypeLift { ty }.mbuild(tycker, env)?;
            let ty_ = ty_.unroll(tycker)?;
            let Type::CoData(coda) = tycker.type_filled(&ty_)? else { unreachable!() };
            let CoMatch { arms } = compu;
            let arms: std::collections::HashMap<_, _> =
                arms.into_iter().map(|CoMatcher { dtor, tail }| (dtor, tail)).collect();
            cs::CoMatch(coda, |dtor, _ty| {
                let tail = arms.get(&dtor).cloned().unwrap();
                cs::TermLift { tm: tail }
            })
            .mbuild(tycker, env)?
        }
        | Compu::Dtor(compu) => {
            let Dtor(compu, dtor) = compu;
            let compu_ = cs::TermLift { tm: compu };
            cs::Dtor(compu_, dtor).mbuild(tycker, env)?
        }
    };

    // Debug: print
    {
        logg::trace!("{}", ">".repeat(20));
        logg::trace!("[end] {} : {}", tycker.dump_statics(compu), tycker.dump_statics(ty));
        logg::trace!("@ {}", compu.span(tycker));
        logg::trace!("{}", "=".repeat(20));
        // for (abst, str) in env.structure.absts.iter() {
        //     logg::trace!("{}", tycker.dump_statics(cs::Ann(abst, str)));
        // }
        logg::trace!("{}", tycker.dump_statics(res));
        logg::trace!("{}", "<".repeat(20));
    }

    Ok((env, res))
}

/// Monadic Block Elaboration (Value)
fn value_monadic_elaboration(
    tycker: &mut Tycker, env: MonEnv, value: ValueId,
) -> Result<(MonEnv, ValueId)> {
    let _ = tycker;
    let _ = env;
    let _ = value;
    todo!()
}

/// Monadic Block Elaboration (Computation)
fn computation_monadic_elaboration(
    tycker: &mut Tycker, env: MonEnv, compu: CompuId,
) -> Result<(MonEnv, CompuId)> {
    let _ = tycker;
    let _ = env;
    let _ = compu;
    todo!()
}
