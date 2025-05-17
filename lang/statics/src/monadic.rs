//! Implementation of the monadic block via algebra translation.
//! See the following for details:
//!
//! + Core Idea: [https://dl.acm.org/doi/10.1145/3720434](section 5.3 of oopsla25)
//! + Detailed Implementation: [https://arxiv.org/abs/2502.15031](appendix D of the extended version)

use crate::{syntax::*, *};

pub mod syntax {
    use super::*;
    /// signature translation
    #[derive(Clone, Copy)]
    pub struct Signature<T> {
        pub monad_ty: TypeId,
        pub ty: T,
    }
    /// structure translation
    #[derive(Clone, Copy)]
    pub struct Structure<'a, T> {
        pub monad_ty: TypeId,
        pub monad_impl: ValueId,
        pub str_env: &'a StructureEnv,
        pub ty: T,
    }
    /// type translation (lift)
    #[derive(Clone, Copy)]
    pub struct TypeLift<T> {
        pub monad_ty: TypeId,
        pub ty: T,
    }
    /// term translation (lift)
    #[derive(Clone, Copy)]
    pub struct TermLift<'a, T> {
        pub monad_ty: TypeId,
        pub monad_impl: ValueId,
        pub str_env: &'a StructureEnv,
        pub tm: T,
    }
    /// monadic elaboration
    #[derive(Clone, Copy)]
    pub struct Elaboration<'a, T> {
        pub monad_ty: TypeId,
        pub monad_impl: ValueId,
        pub str_env: &'a StructureEnv,
        pub tm: T,
    }
}

mod syntax_impl {
    use super::*;

    // SignatureTrans
    impl<T> Construct<TypeId> for cs::Signature<T>
    where
        T: Construct<TypeId>,
    {
        fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
            let cs::Signature { monad_ty, ty } = self;
            let ty = ty.build(tycker, env)?;
            signature_translation(tycker, monad_ty, env, ty)
        }
    }

    // StructureTrans
    impl<T> Construct<CompuId> for cs::Structure<'_, T>
    where
        T: Construct<TypeId>,
    {
        fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
            let cs::Structure { monad_ty, monad_impl, str_env, ty } = self;
            let ty = ty.build(tycker, env)?;
            structure_translation(tycker, monad_ty, monad_impl, str_env, env, ty)
        }
    }

    // TypeLift
    impl<T> Construct<TypeId> for cs::TypeLift<T>
    where
        T: Construct<TypeId>,
    {
        fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
            let cs::TypeLift { monad_ty, ty } = self;
            let ty = ty.build(tycker, env)?;
            type_translation(tycker, monad_ty, env, ty)
        }
    }

    // TermLift (value translation)
    impl<T> Construct<ValueId> for cs::TermLift<'_, T>
    where
        T: Construct<ValueId>,
    {
        fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<ValueId> {
            let cs::TermLift { monad_ty, monad_impl, str_env, tm } = self;
            let tm = tm.build(tycker, env)?;
            value_translation(tycker, monad_ty, monad_impl, str_env, env, tm)
        }
    }

    // TermLift (computation translation)
    impl<T> Construct<CompuId> for cs::TermLift<'_, T>
    where
        T: Construct<CompuId>,
    {
        fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
            let cs::TermLift { monad_ty, monad_impl, str_env, tm } = self;
            let tm = tm.build(tycker, env)?;
            computation_translation(tycker, monad_ty, monad_impl, str_env, env, tm)
        }
    }

    // Elaboration (value)
    impl<T> Construct<ValueId> for cs::Elaboration<'_, T>
    where
        T: Construct<ValueId>,
    {
        fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<ValueId> {
            let cs::Elaboration { monad_ty, monad_impl, str_env, tm } = self;
            let tm = tm.build(tycker, env)?;
            value_monadic_elaboration(tycker, monad_ty, monad_impl, str_env, env, tm)
        }
    }

    // Elaboration (computation)
    impl<T> Construct<CompuId> for cs::Elaboration<'_, T>
    where
        T: Construct<CompuId>,
    {
        fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
            let cs::Elaboration { monad_ty, monad_impl, str_env, tm } = self;
            let tm = tm.build(tycker, env)?;
            computation_monadic_elaboration(tycker, monad_ty, monad_impl, str_env, env, tm)
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
fn signature_translation(
    tycker: &mut Tycker, monad_ty: TypeId, env: &Env<AnnId>, ty: TypeId,
) -> Result<TypeId> {
    let kd = cs::TypeOf(ty).build(tycker, env)?;
    let res = match tycker.kind_filled(&kd)?.to_owned() {
        | Kind::VType(VType) => cs::TopTy.build(tycker, env)?,
        | Kind::CType(CType) => cs::Algebra(monad_ty, ty).build(tycker, env)?,
        | Kind::Arrow(Arrow(kd_1, _)) => cs::Forall(cs::Ann(None, kd_1), |abst| {
            let ty_1 = cs::Ann(abst, kd_1);
            Arrow(
                cs::Thk(cs::Signature { monad_ty, ty: ty_1 }),
                cs::Signature { monad_ty, ty: App(ty, ty_1) },
            )
        })
        .build(tycker, env)?,
    };
    Ok(res)
}

#[derive(Clone)]
pub struct StructureEnv {
    pub def_map: im::HashMap<DefId, AbstId>,
    pub absts: im::HashMap<AbstId, ValueId>,
}

impl StructureEnv {
    pub fn new() -> Self {
        Self { def_map: im::HashMap::new(), absts: im::HashMap::new() }
    }
    fn extended(
        &self, abst: AbstId, def: Option<DefId>, str: impl Construct<ValueId>, tycker: &mut Tycker,
        env: &Env<AnnId>,
    ) -> Self {
        let mut new = self.clone();
        if let Some(def) = def {
            new.def_map.insert(def, abst);
        }
        let Ok(str) = str.build(tycker, env) else { unreachable!() };
        new.absts.insert(abst, str);
        new
    }
}

/// Structure Translation `Str(T)`
fn structure_translation(
    tycker: &mut Tycker, monad_ty: TypeId, monad_impl: ValueId, str_env: &StructureEnv,
    env: &Env<AnnId>, ty: TypeId,
) -> Result<CompuId> {
    let res = match tycker.type_filled(&ty)?.to_owned() {
        | Type::Var(def) => {
            let str = match str_env.def_map.get(&def).map(|abst| str_env.absts[&abst]) {
                | Some(str) => str.to_owned(),
                | None => tycker.err(
                    TyckError::MissingStructure(ty.to_owned()),
                    std::panic::Location::caller(),
                )?,
            };
            Force(str).build(tycker, env)?
        }
        | Type::Abst(abst) => {
            let str = match str_env.absts.get(&abst) {
                | Some(str) => str.to_owned(),
                | None => tycker.err(
                    TyckError::MissingStructure(ty.to_owned()),
                    std::panic::Location::caller(),
                )?,
            };
            Force(str).build(tycker, env)?
        }
        | Type::Abs(ty) => {
            // input: fn (X : K) -> S
            let Abs(tpat, ty) = ty;
            // output: fn (X : K) (str_X : Thk (Sig_K(X))) -> Str(S)
            let (tvar, kd) = {
                let (def, kd) = tpat.destruct_def(tycker);
                (tycker.scoped.defs[&def].to_owned(), kd)
            };
            let tvar = tvar.as_str();
            Abs(cs::Ty(cs::Pat(tvar, kd)), |tdef, abst| {
                let svar = VarName(format!("str_{}", tvar));
                let svar_ty = cs::Thk(cs::Signature { monad_ty, ty: cs::Ty(abst) });
                Abs(cs::Pat(svar, svar_ty), |str_def: Option<DefId>| {
                    // let str = str_def.unwrap();
                    // let str_env = &str_env.extended(abst, tdef, str, tycker, env);
                    // cs::Structure { monad_ty, monad_impl, str_env, ty }
                    // Fixme: figure out how to register structure
                    let _ = ty;
                    let _ = tdef;
                    let _ = str_def;
                    cs::Top
                })
            })
            .build(tycker, env)?
        }
        | Type::App(ty) => {
            // input: S_f S_a
            let App(ty_f, ty_a) = ty;
            // output: Str(S_f) Lift(S_a) { Str(S_a) }
            let str_f = cs::Structure { monad_ty, monad_impl, str_env, ty: ty_f };
            let ty_a_lift = cs::TypeLift { monad_ty, ty: ty_a };
            let str_a = cs::Structure { monad_ty, monad_impl, str_env, ty: ty_a };
            App(App(str_f, cs::Ty(ty_a_lift)), Thunk(str_a)).build(tycker, env)?
        }
        // primitive types are not allowed in monadic blocks
        | Type::Int(_) | Type::Char(_) | Type::String(_) => unreachable!(),
        // unit, product, and existential types have the trivial structure `top`
        // (so should data types)
        | Type::Unit(UnitTy) | Type::Prod(_) | Type::Exists(_) | Type::Data(_) => {
            cs::Top.build(tycker, env)?
        }
        // the thunk type is itself a type constructor,
        // so its structure takes a type and the type's structure as arguments
        | Type::Thk(ThkTy) => {
            // output: fn (X : CType) (_ : Thk (Sig_CType(X))) -> <top>
            Abs(cs::Ty(cs::Pat("_", CType)), |_tvar, abst| {
                let thk_sig = cs::Thk(cs::Signature { monad_ty, ty: cs::Ty(abst) });
                Abs(cs::Pat("_", thk_sig), |_var| {
                    // <top> = comatch end
                    cs::Top
                })
            })
            .build(tycker, env)?
        }
        // the os type is a primitive computation type and thus not allowed in monadic blocks
        | Type::OS(_) => unreachable!(),
        | Type::Ret(RetTy) => {
            // output: fn (X : VType) (_ : Thk (Sig_VType(X))) -> <monadic_bind>
            Abs(cs::Ty(cs::Pat("_", VType)), |_tvar, abst_x| {
                let abst_x_ty = cs::Ty(abst_x);
                let thk_sig = cs::Thk(cs::Signature { monad_ty, ty: abst_x_ty });
                Abs(cs::Pat("_", thk_sig), move |_var| {
                    // <monadic_bind> = fn (Z : VType) -> ! monad_impl .bind Z X
                    Abs(cs::Ty(cs::Pat("Z", VType)), move |_tvar, abst_z| {
                        let abst_z_ty = cs::Ty(abst_z);
                        let body = cs::Dtor(Force(monad_impl), ".bind");
                        App(App(body, cs::Ty(abst_z_ty)), cs::Ty(abst_x_ty))
                    })
                })
            })
            .build(tycker, env)?
        }
        | Type::CoData(coda) => {
            // output: fn (Z : VType) (mz : Thk (M Z)) (f: Thk (Z -> &_n[B_n])) -> <body>
            Abs(cs::Ty(cs::Pat("Z", VType)), |_tvar, abst_z| {
                let abst_z_ty = cs::Ty(abst_z);
                let mz_ty = cs::Thk(App(monad_ty, abst_z_ty));
                Abs(cs::Pat("mz", mz_ty), move |var_mz| {
                    let ty_ = cs::TypeLift { monad_ty, ty };
                    let f_ty = cs::Thk(Arrow(abst_z_ty, ty_));
                    Abs(cs::Pat("f", f_ty), move |var_f| {
                        // <body> =
                        // comatch
                        // | .dtor_n -> Str(B_n) Z mz { fn (z : Z) -> ! f z .dtor_n }
                        // | ...
                        // end
                        cs::CoMatch(coda, move |dtor, ty| {
                            let kont = Abs(cs::Pat("z", abst_z_ty), move |var_z| {
                                Dtor(App(Force(var_f), var_z), dtor)
                            });
                            let str = cs::Structure { monad_ty, monad_impl, str_env, ty };
                            App(App(App(str, abst_z_ty), var_mz), Thunk(kont))
                        })
                    })
                })
            })
            .build(tycker, env)?
        }
        | Type::Arrow(ty) => {
            // input: A -> B
            let Arrow(ty_p, ty_b) = ty;
            // output: fn (Z : VType) (mz : Thk (M Z)) (f : Thk (Z -> [A] -> [B])) -> <body>
            Abs(cs::Ty(cs::Pat("Z", VType)), |_tvar, abst_z| {
                let abst_z_ty = cs::Ty(abst_z);
                let mz_ty = cs::Thk(App(monad_ty, abst_z_ty));
                Abs(cs::Pat("mz", mz_ty), move |var_mz| {
                    let ty_p_ = cs::TypeLift { monad_ty, ty: ty_p };
                    let ty_b_ = cs::TypeLift { monad_ty, ty: ty_b };
                    let f_ty = cs::Thk(Arrow(abst_z_ty, Arrow(ty_p_, ty_b_)));
                    Abs(cs::Pat("f", f_ty), move |var_f| {
                        // <body> = fn (x : [A]) -> Str(B) Z mz { fn (z : Z) -> ! f z x }
                        Abs(cs::Pat("x", ty_p_), move |var_x| {
                            let alg_b = cs::Structure { monad_ty, monad_impl, str_env, ty: ty_b };
                            let kont = Abs(cs::Pat("z", abst_z_ty), move |var_z| {
                                App(App(Force(var_f), var_z), var_x)
                            });
                            App(App(App(alg_b, cs::Ty(abst_z_ty)), var_mz), Thunk(kont))
                        })
                    })
                })
            })
            .build(tycker, env)?
        }
        | Type::Forall(ty) => {
            // input: forall (X : K) . B
            let Forall(abst, ty) = ty;
            let kd = cs::TypeOf(abst);
            // output: fn (Z : VType) (mz : Thk (M Z)) (f : <f_ty>) -> <body>
            Abs(cs::Ty(cs::Pat("Z", VType)), move |_tvar, abst_z| {
                let abst_z_ty = cs::Ty(abst_z);
                Abs(cs::Pat("mz", cs::Thk(App(monad_ty, abst_z_ty))), move |var_mz| {
                    // construct abstract type X first
                    cs::CBind::new(cs::Ann("X", kd), move |abst_x| {
                        // <f_ty> = Thk (Z -> forall (X : K) . Thk (Sig_K(X)) -> [B])
                        let f_ty = cs::Thk(Arrow(
                            abst_z_ty,
                            cs::Forall(abst_x, move |abst_x: AbstId| {
                                let abst_x_ty = cs::Ty(abst_x);
                                Arrow(
                                    cs::Thk(cs::Signature { monad_ty, ty: abst_x_ty }),
                                    cs::TypeLift { monad_ty, ty },
                                )
                            }),
                        ));
                        Abs(cs::Pat("f", f_ty), move |var_f| {
                            // <body> = fn (X : K) (str_X : Thk (Sig_K(X))) -> Str(B) Z mz <kont>
                            Abs(cs::Ty(abst_x), move |_, abst_x: AbstId| {
                                let abst_x_ty = cs::Ty(abst_x);
                                let sig = cs::Signature { monad_ty, ty: abst_x_ty };
                                Abs(cs::Pat("str_X", cs::Thk(sig)), move |var_str_x| {
                                    // <kont> = { fn (z : Z) -> ! f z X str_X }
                                    let kont = Abs(cs::Pat("z", abst_z_ty), move |var_z| {
                                        App(App(App(Force(var_f), var_z), abst_x_ty), var_str_x)
                                    });
                                    let str_ = cs::Structure { monad_ty, monad_impl, str_env, ty };
                                    App(App(App(str_, abst_z_ty), var_mz), Thunk(kont))
                                })
                            })
                        })
                    })
                })
            })
            .build(tycker, env)?
        }
    };
    Ok(res)
}

/// Carrier (Type) Translation `[T]`
fn type_translation(
    tycker: &mut Tycker, monad_ty: TypeId, env: &Env<AnnId>, ty: TypeId,
) -> Result<TypeId> {
    let kd = cs::TypeOf(ty).build(tycker, env)?;
    let res = match tycker.type_filled(&ty)?.to_owned() {
        | Type::Var(def) => Alloc::alloc(tycker, def, kd),
        | Type::Abst(abst) => Alloc::alloc(tycker, abst, kd),
        | Type::Abs(ty) => {
            let Abs(tpat, ty) = ty;
            // Fixme: bind environment
            Abs(cs::Fresh(tpat), |_, _, _| cs::TypeLift { monad_ty, ty }).build(tycker, env)?
        }
        | Type::App(ty) => {
            let App(ty_f, ty_a) = ty;
            let ty_f_ = cs::TypeLift { monad_ty, ty: ty_f };
            let ty_a_ = cs::TypeLift { monad_ty, ty: ty_a };
            App(ty_f_, ty_a_).build(tycker, env)?
        }
        | Type::Thk(ThkTy) => Alloc::alloc(tycker, ThkTy, kd),
        // primitive types are not allowed in monadic blocks
        | Type::Int(_) | Type::Char(_) | Type::String(_) => unreachable!(),
        | Type::Data(data) => {
            cs::Data(data, |_ctor, ty| cs::TypeLift { monad_ty, ty }).build(tycker, env)?
        }
        | Type::Unit(UnitTy) => UnitTy.build(tycker, env)?,
        | Type::Prod(ty) => {
            let Prod(ty_1, ty_2) = ty;
            let ty_1_ = cs::TypeLift { monad_ty, ty: ty_1 };
            let ty_2_ = cs::TypeLift { monad_ty, ty: ty_2 };
            Prod(ty_1_, ty_2_).build(tycker, env)?
        }
        | Type::Exists(ty) => {
            let Exists(abst, ty) = ty;
            let abst_ty = cs::Ty(abst);
            let thk_sig = cs::Thk(cs::Signature { monad_ty, ty: abst_ty });
            let ty_ = cs::TypeLift { monad_ty, ty };
            let prod = Prod(thk_sig, ty_);
            cs::Exists(abst, |_| prod).build(tycker, env)?
        }
        // os type is also not allowed in monadic blocks
        | Type::OS(_) => unreachable!(),
        // the return type is translated to the provided monad type
        | Type::Ret(RetTy) => monad_ty,
        | Type::CoData(coda) => {
            cs::CoData(coda, |_dtor, ty| cs::TypeLift { monad_ty, ty }).build(tycker, env)?
        }
        | Type::Arrow(ty) => {
            let Arrow(ty_1, ty_2) = ty;
            let ty_1_ = cs::TypeLift { monad_ty, ty: ty_1 };
            let ty_2_ = cs::TypeLift { monad_ty, ty: ty_2 };
            Arrow(ty_1_, ty_2_).build(tycker, env)?
        }
        | Type::Forall(ty) => {
            let Forall(abst, ty) = ty;
            cs::Forall(abst, move |abst| {
                Arrow(
                    cs::Thk(cs::Signature { monad_ty, ty: cs::Ty(abst) }),
                    cs::TypeLift { monad_ty, ty },
                )
            })
            .build(tycker, env)?
        }
    };
    Ok(res)
}

/// Term Translation (Value) `[V]`
fn value_translation(
    tycker: &mut Tycker, monad_ty: TypeId, monad_impl: ValueId, str_env: &StructureEnv,
    env: &Env<AnnId>, value: ValueId,
) -> Result<ValueId> {
    let ty = cs::TypeOf(value).build(tycker, env)?;
    let ty_ = cs::TypeLift { monad_ty, ty }.build(tycker, env)?;
    let res = match tycker.value(&value).to_owned() {
        | Value::Hole(Hole) => cs::Ann(Hole, ty_).build(tycker, env)?,
        // figure out how to handle literals
        | Value::Lit(_) => unreachable!(),
        // Fixme: variables should be freshed and substituted
        | Value::Var(_def) => todo!(),
        | Value::Thunk(value) => {
            let Thunk(body) = value;
            Thunk(cs::TermLift { monad_ty, monad_impl, str_env, tm: body }).build(tycker, env)?
        }
        | Value::Ctor(value) => {
            let Ctor(ctor, body) = value;
            let body_ = cs::TermLift { monad_ty, monad_impl, str_env, tm: body };
            let ty_ = cs::TypeLift { monad_ty, ty };
            cs::Ann(cs::Ctor(ctor, body_), ty_).build(tycker, env)?
        }
        | Value::Triv(Triv) => Triv.build(tycker, env)?,
        | Value::VCons(value) => {
            let Cons(value_1, value_2) = value;
            let value_1_ = cs::TermLift { monad_ty, monad_impl, str_env, tm: value_1 };
            let value_2_ = cs::TermLift { monad_ty, monad_impl, str_env, tm: value_2 };
            Cons(value_1_, value_2_).build(tycker, env)?
        }
        | Value::TCons(value) => {
            let Cons(a_ty, body) = value;
            let a_str = cs::Structure { monad_ty, monad_impl, str_env, ty: a_ty };
            let body_ = cs::TermLift { monad_ty, monad_impl, str_env, tm: body };
            let a_ty_ = cs::TypeLift { monad_ty, ty: a_ty };
            // existential type construct should be type-guided
            cs::Ann(Cons(cs::Ty(a_ty_), Cons(Thunk(a_str), body_)), ty_).build(tycker, env)?
        }
    };
    Ok(res)
}

/// Term Translation (Computation) `[C]`
fn computation_translation(
    tycker: &mut Tycker, monad_ty: TypeId, monad_impl: ValueId, str_env: &StructureEnv,
    env: &Env<AnnId>, compu: CompuId,
) -> Result<CompuId> {
    use Computation as Compu;
    let ty = cs::TypeOf(compu).build(tycker, env)?;
    let ty_ = cs::TypeLift { monad_ty, ty }.build(tycker, env)?;
    let res = match tycker.compu(&compu) {
        | Compu::Hole(Hole) => cs::Ann(Hole, ty_).build(tycker, env)?,
        | Compu::VAbs(compu) => {
            let Abs(vpat, compu) = compu;
            let (def, param_ty) = vpat.try_destruct_def(tycker);
            let param_ty_ = cs::TypeLift { monad_ty, ty: param_ty };
            Abs(cs::Pat(def, param_ty_), move |_def| cs::TermLift {
                monad_ty,
                monad_impl,
                str_env,
                tm: compu,
            })
            .build(tycker, env)?
        }
        | Compu::VApp(compu) => {
            let App(fun, arg) = compu;
            let fun_ = cs::TermLift { monad_ty, monad_impl, str_env, tm: fun };
            let arg_ = cs::TermLift { monad_ty, monad_impl, str_env, tm: arg };
            App(fun_, arg_).build(tycker, env)?
        }
        | Compu::TAbs(compu) => {
            let Abs(tpat, compu) = compu;
            Abs(cs::Ty(cs::Fresh(tpat)), move |_def, abst| {
                let thk_sig = cs::Thk(cs::Signature { monad_ty, ty: cs::Ty(abst) });
                Abs(cs::Pat("str", thk_sig), move |_str_var| {
                    // Fixme: bind environment
                    cs::TermLift { monad_ty, monad_impl, str_env, tm: compu }
                })
            })
            .build(tycker, env)?
        }
        | Compu::TApp(compu) => {
            let App(fun, arg) = compu;
            let fun_ = cs::TermLift { monad_ty, monad_impl, str_env, tm: fun };
            let arg_ = cs::TypeLift { monad_ty, ty: arg };
            let str_ = cs::Structure { monad_ty, monad_impl, str_env, ty: arg };
            App(App(fun_, cs::Ty(arg_)), Thunk(str_)).build(tycker, env)?
        }
        | Compu::Fix(compu) => {
            let Fix(vpat, compu) = compu;
            let (def, param_ty) = vpat.try_destruct_def(tycker);
            let param_ty_ = cs::TypeLift { monad_ty, ty: param_ty }.build(tycker, env)?;
            Abs(cs::Ann(def, param_ty_), move |_def| cs::TermLift {
                monad_ty,
                monad_impl,
                str_env,
                tm: compu,
            })
            .build(tycker, env)?
        }
        | Compu::Force(compu) => {
            let Force(value) = compu;
            Force(cs::TermLift { monad_ty, monad_impl, str_env, tm: value }).build(tycker, env)?
        }
        | Compu::Ret(compu) => {
            let Ret(value) = compu;
            App(
                App(
                    cs::Dtor(Force(monad_impl), ".return"),
                    cs::Ty(cs::TypeLift { monad_ty, ty: cs::TypeOf(value) }),
                ),
                cs::TermLift { monad_ty, monad_impl, str_env, tm: value },
            )
            .build(tycker, env)?
        }
        | Compu::Do(compu) => {
            let Bind { binder, bindee, tail } = compu;
            let str_ = cs::Structure { monad_ty, monad_impl, str_env, ty: ty_ };
            let ret_ty = cs::TypeOf(bindee).build(tycker, env)?;
            let Some(a_ty) = ret_ty.destruct_ret_app(tycker) else { unreachable!() };
            let a_ty_ = cs::TypeLift { monad_ty, ty: a_ty };
            let bindee_ = cs::TermLift { monad_ty, monad_impl, str_env, tm: bindee };
            let kont = Abs(cs::Fresh(binder), move |_var| cs::TermLift {
                monad_ty,
                monad_impl,
                str_env,
                tm: tail,
            });
            App(App(App(str_, cs::Ty(a_ty_)), Thunk(bindee_)), Thunk(kont)).build(tycker, env)?
        }
        | Compu::Let(_) => todo!(),
        | Compu::Match(_) => todo!(),
        | Compu::CoMatch(_) => todo!(),
        | Compu::Dtor(_) => todo!(),
    };
    Ok(res)
}

/// Monadic Block Elaboration (Value)
fn value_monadic_elaboration(
    tycker: &mut Tycker, monad_ty: TypeId, monad_impl: ValueId, str_env: &StructureEnv,
    env: &Env<AnnId>, value: ValueId,
) -> Result<ValueId> {
    let _ = tycker;
    let _ = monad_ty;
    let _ = monad_impl;
    let _ = str_env;
    let _ = env;
    let _ = value;
    todo!()
}

/// Monadic Block Elaboration (Computation)
fn computation_monadic_elaboration(
    tycker: &mut Tycker, monad_ty: TypeId, monad_impl: ValueId, str_env: &StructureEnv,
    env: &Env<AnnId>, compu: CompuId,
) -> Result<CompuId> {
    let _ = tycker;
    let _ = monad_ty;
    let _ = monad_impl;
    let _ = str_env;
    let _ = env;
    let _ = compu;
    todo!()
}
