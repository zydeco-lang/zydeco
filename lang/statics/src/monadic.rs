//! Implementation of the monadic block via algebra translation.
//! See the following for details:
//!
//! + Core Idea: [https://dl.acm.org/doi/10.1145/3720434](section 5.3 of oopsla25)
//! + Detailed Implementation: [https://arxiv.org/abs/2502.15031](appendix D of the extended version)

use crate::{syntax::*, *};
use std::collections::HashMap;

pub mod syntax {
    use super::*;
    /// signature translation
    pub struct Signature<T> {
        pub monad_ty: TypeId,
        pub ty: T,
    }
    /// structure translation
    pub struct Structure<'a, T> {
        pub monad_ty: TypeId,
        pub monad_impl: ValueId,
        pub str_env: &'a StructureEnv,
        pub ty: T,
    }
    /// type translation (lift)
    pub struct TypeLift<T> {
        pub monad_ty: TypeId,
        pub ty: T,
    }
    /// term translation (lift)
    pub struct TermLift<'a, T> {
        pub monad_ty: TypeId,
        pub monad_impl: ValueId,
        pub str_env: &'a StructureEnv,
        pub tm: T,
    }
    /// monadic elaboration
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
    impl<T> Construct<Result<TypeId>> for cs::Signature<T>
    where
        T: Construct<TypeId>,
    {
        fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
            let cs::Signature { monad_ty, ty } = self;
            let ty = ty.build(tycker, env);
            signature_translation(tycker, monad_ty, env, ty)
        }
    }

    // StructureTrans
    impl<T> Construct<Result<CompuId>> for cs::Structure<'_, T>
    where
        T: Construct<TypeId>,
    {
        fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
            let cs::Structure { monad_ty, monad_impl, str_env, ty } = self;
            let ty = ty.build(tycker, env);
            structure_translation(tycker, monad_ty, monad_impl, str_env, env, ty)
        }
    }

    // TypeLift
    impl<T> Construct<Result<TypeId>> for cs::TypeLift<T>
    where
        T: Construct<TypeId>,
    {
        fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<TypeId> {
            let cs::TypeLift { monad_ty, ty } = self;
            let ty = ty.build(tycker, env);
            type_translation(tycker, monad_ty, env, ty)
        }
    }

    // TermLift (value translation)
    impl<T> Construct<Result<ValueId>> for cs::TermLift<'_, T>
    where
        T: Construct<ValueId>,
    {
        fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<ValueId> {
            let cs::TermLift { monad_ty, monad_impl, str_env, tm } = self;
            let tm = tm.build(tycker, env);
            value_translation(tycker, monad_ty, monad_impl, str_env, env, tm)
        }
    }

    // TermLift (computation translation)
    impl<T> Construct<Result<CompuId>> for cs::TermLift<'_, T>
    where
        T: Construct<CompuId>,
    {
        fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
            let cs::TermLift { monad_ty, monad_impl, str_env, tm } = self;
            let tm = tm.build(tycker, env);
            computation_translation(tycker, monad_ty, monad_impl, str_env, env, tm)
        }
    }

    // Elaboration (value)
    impl<T> Construct<Result<ValueId>> for cs::Elaboration<'_, T>
    where
        T: Construct<ValueId>,
    {
        fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<ValueId> {
            let cs::Elaboration { monad_ty, monad_impl, str_env, tm } = self;
            let tm = tm.build(tycker, env);
            value_monadic_elaboration(tycker, monad_ty, monad_impl, str_env, env, tm)
        }
    }

    // Elaboration (computation)
    impl<T> Construct<Result<CompuId>> for cs::Elaboration<'_, T>
    where
        T: Construct<CompuId>,
    {
        fn build(self, tycker: &mut Tycker, env: &Env<AnnId>) -> Result<CompuId> {
            let cs::Elaboration { monad_ty, monad_impl, str_env, tm } = self;
            let tm = tm.build(tycker, env);
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
    let kd = cs::TypeOf(ty).build(tycker, env);
    let res = match tycker.kind_filled(&kd)?.to_owned() {
        | Kind::VType(VType) => cs::TopTy.build(tycker, env),
        | Kind::CType(CType) => cs::Algebra(monad_ty, ty).build(tycker, env),
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
    pub defs: HashMap<DefId, ValueId>,
    pub absts: HashMap<AbstId, ValueId>,
}

impl StructureEnv {
    pub fn new() -> Self {
        Self { defs: HashMap::new(), absts: HashMap::new() }
    }
    fn extend_with_abst(&self, abst: AbstId, str: ValueId) -> Self {
        let mut new = self.clone();
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
            let str = match str_env.defs.get(&def) {
                | Some(str) => str.to_owned(),
                | None => tycker.err(
                    TyckError::MissingStructure(ty.to_owned()),
                    std::panic::Location::caller(),
                )?,
            };
            Force(str).build(tycker, env)
        }
        | Type::Abst(abst) => {
            let str = match str_env.absts.get(&abst) {
                | Some(str) => str.to_owned(),
                | None => tycker.err(
                    TyckError::MissingStructure(ty.to_owned()),
                    std::panic::Location::caller(),
                )?,
            };
            Force(str).build(tycker, env)
        }
        | Type::Abs(ty) => {
            // input: fn (X : K) -> S
            let Abs(tpat, ty) = ty;
            // output: fn (X : K) (str_X : Thk (Sig_K(X))) -> Str(S)
            let (tvar, kd) = {
                let (def, kd) = tpat.destruct_def(tycker);
                (tycker.scoped.defs[&def].to_owned(), kd)
            };
            tycker.try_compu_tabs(env, tvar.to_owned(), kd, |tycker, _env, _tvar_def, abst| {
                let svar = VarName(format!("str_{}", tvar.as_str()));
                let svar_ty =
                cs::Thk(cs::Signature { monad_ty, ty: cs::AbstTy(abst) }).build(tycker, env)?;
                tycker.try_compu_vabs(env, svar, svar_ty, |tycker, _env, str_def| {
                    let str = Alloc::alloc(tycker, str_def, svar_ty);
                    let str_env = &str_env.extend_with_abst(abst, str);
                    cs::Structure { monad_ty, monad_impl, str_env, ty }.build(tycker, env)
                })
            })?
        }
        | Type::App(ty) => {
            // input: S_f S_a
            let App(ty_f, ty_a) = ty;
            // output: Str(S_f) Lift(S_a) Str(S_a)
            let str_f =
                cs::Structure { monad_ty, monad_impl, str_env, ty: ty_f }.build(tycker, env)?;
            let ty_lift_a = cs::TypeLift { monad_ty, ty: ty_a }.build(tycker, env)?;
            let str_f_app_sig: CompuId = App(str_f, cs::Ty(ty_lift_a)).build(tycker, env);
            let str_a =
                cs::Structure { monad_ty, monad_impl, str_env, ty: ty_a }.build(tycker, env)?;
            let thk_str_a = Thunk(str_a).build(tycker, env);
            App(str_f_app_sig, thk_str_a).build(tycker, env)
        }
        // primitive types are not allowed in monadic blocks
        | Type::Int(_) | Type::Char(_) | Type::String(_) => unreachable!(),
        // unit, product, and existential types have the trivial structure `top`
        // (so should data types)
        | Type::Unit(UnitTy) | Type::Prod(_) | Type::Exists(_) | Type::Data(_) => {
            cs::Top.build(tycker, env)
        }
        // the thunk type is itself a type constructor,
        // so its structure takes a type and the type's structure as arguments
        | Type::Thk(ThkTy) => {
            // output: fn (X : CType) (_ : Thk (Sig_CType(X))) -> <top>
            tycker.try_compu_tabs(env, "_", CType, |tycker, _env, _tvar, abst| {
                let thk_sig =
                cs::Thk(cs::Signature { monad_ty, ty: cs::AbstTy(abst) }).build(tycker, env)?;
                tycker.try_compu_vabs(env, "_", thk_sig, |tycker, _env, _var| {
                    // <top> = comatch end
                    Ok(cs::Top.build(tycker, env))
                })
            })?
        }
        // the os type is a primitive computation type and thus not allowed in monadic blocks
        | Type::OS(_) => unreachable!(),
        | Type::Ret(RetTy) => {
            // output: fn (X : VType) (_ : Thk (Sig_VType(X))) -> <monadic_bind>
            tycker.try_compu_tabs(env, "_", VType, |tycker, _env, _tvar, abst_x| {
                let abst_x_ty = cs::AbstTy(abst_x).build(tycker, env);
                let thk_sig =
                cs::Thk(cs::Signature { monad_ty, ty: abst_x_ty }).build(tycker, env)?;
                tycker.try_compu_vabs(env, "_", thk_sig, |tycker, _env, _var| {
                    // <monadic_bind> = fn (Z : VType) -> ! monad_impl .bind Z X
                    tycker.try_compu_tabs(env, "Z", VType, |tycker, _env, _tvar_z, abst_z| {
                        let abst_z_ty = cs::AbstTy(abst_z).build(tycker, env);
                        let body = cs::Dtor(Force(monad_impl), ".bind");
                        let res = App(App(body, cs::Ty(abst_z_ty)), cs::Ty(abst_x_ty));
                        Ok(res)
                    })
                })
            })?
        }
        | Type::CoData(coda) => todo!(),
        | Type::Arrow(ty) => {
            let Arrow(ty_p, ty_b) = ty;
            tycker.try_compu_tabs(env, "Z", VType, |tycker, env, _tvar_z, abst_z| {
                let abst_z_ty = cs::AbstTy(abst_z).build(tycker, env);
                let mz_ty = App(monad_ty, abst_z_ty);
                tycker.try_compu_vabs(env, "mz", mz_ty, |tycker, env, var_mz| -> Result<_> {
                    let ty_p_ = cs::TypeLift { monad_ty, ty: ty_p }.build(tycker, env)?;
                    let ty_b_ = cs::TypeLift { monad_ty, ty: ty_b }.build(tycker, env)?;
                    let f_ty = cs::Thk(Arrow(abst_z_ty, Arrow(ty_p_, ty_b_)));
                    tycker.try_compu_vabs(env, "f", f_ty, |tycker, env, var_f| {
                        tycker.try_compu_vabs(env, "x", ty_p_, |tycker, env, var_x| {
                            let alg_b = cs::Structure { monad_ty, monad_impl, str_env, ty: ty_b }
                                .build(tycker, env)?;
                            let mz: ValueId = var_mz.build(tycker, env);
                            let kont =
                                tycker.compu_vabs(env, "z", abst_z_ty, |tycker, env, var_z| {
                                    let f: ValueId = var_f.build(tycker, env);
                                    let z: ValueId = var_z.build(tycker, env);
                                    let x: ValueId = var_x.build(tycker, env);
                                    App(App(Force(f), z), x)
                                });
                            let res = App(App(App(alg_b, cs::Ty(abst_z_ty)), mz), Thunk(kont));
                            Ok(res)
                        })
                    })
                })
            })?
        }
        | Type::Forall(forall) => todo!(),
    };
    Ok(res)
}

/// Carrier (Type) Translation `[T]`
fn type_translation(
    tycker: &mut Tycker, monad_ty: TypeId, env: &Env<AnnId>, ty: TypeId,
) -> Result<TypeId> {
    let kd = cs::TypeOf(ty).build(tycker, env);
    let res = match tycker.type_filled(&ty)?.to_owned() {
        | Type::Var(def) => Alloc::alloc(tycker, def, kd),
        | Type::Abst(abst) => Alloc::alloc(tycker, abst, kd),
        | Type::Abs(ty) => {
            let Abs(tpat, ty) = ty;
            let (def, param_kd) = tpat.destruct_def(tycker);
            let tpat_ = Alloc::alloc(tycker, def, param_kd);
            let ty_ = cs::TypeLift { monad_ty, ty }.build(tycker, env)?;
            Alloc::alloc(tycker, Abs(tpat_, ty_), kd)
        }
        | Type::App(ty) => {
            let App(ty_f, ty_a) = ty;
            let ty_f_ = cs::TypeLift { monad_ty, ty: ty_f }.build(tycker, env)?;
            let ty_a_ = cs::TypeLift { monad_ty, ty: ty_a }.build(tycker, env)?;
            App(ty_f_, ty_a_).build(tycker, env)
        }
        | Type::Thk(ThkTy) => Alloc::alloc(tycker, ThkTy, kd),
        // primitive types are not allowed in monadic blocks
        | Type::Int(_) | Type::Char(_) | Type::String(_) => unreachable!(),
        // | Type::Int(IntTy) => Alloc::alloc(tycker, IntTy, kd),
        // | Type::Char(CharTy) => Alloc::alloc(tycker, CharTy, kd),
        // | Type::String(StringTy) => Alloc::alloc(tycker, StringTy, kd),
        | Type::Data(data) => {
            let arms = tycker.statics.datas.defs[&data].clone();
            let arms_ = arms
                .into_iter()
                .map(|(ctor, ty)| {
                    let ty_ = cs::TypeLift { monad_ty, ty }.build(tycker, env)?;
                    Ok((ctor, ty_))
                })
                .collect::<Result<im::Vector<_>>>()?;
            let data_ = Data::new(arms_.iter().cloned());
            let data = tycker.statics.datas.lookup_or_alloc(arms_, data_);
            let kd = cs::TypeOf(ty).build(tycker, env);
            Alloc::alloc(tycker, data, kd)
        }
        | Type::Unit(UnitTy) => ty,
        | Type::Prod(ty) => {
            let Prod(ty_1, ty_2) = ty;
            let ty_1_ = cs::TypeLift { monad_ty, ty: ty_1 }.build(tycker, env)?;
            let ty_2_ = cs::TypeLift { monad_ty, ty: ty_2 }.build(tycker, env)?;
            Prod(ty_1_, ty_2_).build(tycker, env)
        }
        | Type::Exists(ty) => {
            let Exists(abst, ty) = ty;
            let abst_kd = cs::TypeOf(abst).build(tycker, env);
            let ty_abst = Alloc::alloc(tycker, abst, abst_kd);
            let thk_sig = cs::Thk(cs::Signature { monad_ty, ty: ty_abst }).build(tycker, env)?;
            let ty_ = cs::TypeLift { monad_ty, ty }.build(tycker, env)?;
            let prod = Prod(thk_sig, ty_).build(tycker, env);
            cs::Exists(abst, |_| prod).build(tycker, env)
        }
        // os type is also not allowed in monadic blocks
        | Type::OS(_) => unreachable!(),
        // the return type is translated to the provided monad type
        | Type::Ret(RetTy) => monad_ty,
        | Type::CoData(coda) => {
            let arms = tycker.statics.codatas.defs[&coda].clone();
            let arms_ = arms
                .into_iter()
                .map(|(ctor, ty)| {
                    let ty_ = cs::TypeLift { monad_ty, ty }.build(tycker, env)?;
                    Ok((ctor, ty_))
                })
                .collect::<Result<im::Vector<_>>>()?;
            let coda_ = CoData::new(arms_.iter().cloned());
            let coda = tycker.statics.codatas.lookup_or_alloc(arms_, coda_);
            let kd = cs::TypeOf(ty).build(tycker, env);
            Alloc::alloc(tycker, coda, kd)
        }
        | Type::Arrow(ty) => {
            let Arrow(ty_1, ty_2) = ty;
            let ty_1_ = cs::TypeLift { monad_ty, ty: ty_1 }.build(tycker, env)?;
            let ty_2_ = cs::TypeLift { monad_ty, ty: ty_2 }.build(tycker, env)?;
            Arrow(ty_1_, ty_2_).build(tycker, env)
        }
        | Type::Forall(ty) => {
            let Forall(abst, ty) = ty;
            let abst_kd = cs::TypeOf(abst).build(tycker, env);
            let ty_abst = Alloc::alloc(tycker, abst, abst_kd);
            let thk_sig = cs::Thk(cs::Signature { monad_ty, ty: ty_abst }).build(tycker, env)?;
            let ty_ = cs::TypeLift { monad_ty, ty }.build(tycker, env)?;
            cs::Forall(abst, |_| Arrow(thk_sig, ty_)).build(tycker, env)
        }
    };
    Ok(res)
}

/// Term Translation (Value) `[V]`
fn value_translation(
    tycker: &mut Tycker, monad_ty: TypeId, monad_impl: ValueId, str_env: &StructureEnv,
    env: &Env<AnnId>, value: ValueId,
) -> Result<ValueId> {
    let ty = cs::TypeOf(value).build(tycker, env);
    let ty_ = cs::TypeLift { monad_ty, ty }.build(tycker, env)?;
    let res = match tycker.value(&value).to_owned() {
        | Value::Hole(Hole) => Alloc::alloc(tycker, Hole, ty_),
        // figure out how to handle literals
        | Value::Lit(_) => unreachable!(),
        // Fixme: variables should be freshed and substituted
        | Value::Var(def) => todo!(),
        | Value::Thunk(value) => {
            let Thunk(body) = value;
            let body_ =
                cs::TermLift { monad_ty, monad_impl, str_env, tm: body }.build(tycker, env)?;
            Thunk(body_).build(tycker, env)
        }
        | Value::Ctor(value) => {
            let Ctor(ctor, body) = value;
            let body_ =
                cs::TermLift { monad_ty, monad_impl, str_env, tm: body }.build(tycker, env)?;
            let ty_ = cs::TypeLift { monad_ty, ty }.build(tycker, env)?;
            cs::Ann(Ctor(ctor, body_), ty_).build(tycker, env)
        }
        | Value::Triv(Triv) => value,
        | Value::VCons(value) => {
            let Cons(value_1, value_2) = value;
            let value_1_ =
                cs::TermLift { monad_ty, monad_impl, str_env, tm: value_1 }.build(tycker, env)?;
            let value_2_ =
                cs::TermLift { monad_ty, monad_impl, str_env, tm: value_2 }.build(tycker, env)?;
            Cons(value_1_, value_2_).build(tycker, env)
        }
        | Value::TCons(value) => {
            let Cons(a_ty, body) = value;
            let str =
                cs::Structure { monad_ty, monad_impl, str_env, ty: a_ty }.build(tycker, env)?;
            let thk_str = Thunk(str).build(tycker, env);
            let body_ =
                cs::TermLift { monad_ty, monad_impl, str_env, tm: body }.build(tycker, env)?;
            let vcons = Cons(thk_str, body_).build(tycker, env);
            let a_ty_ = cs::TypeLift { monad_ty, ty: a_ty }.build(tycker, env)?;
            // existential type construct should be type-guided
            Alloc::alloc(tycker, Cons(a_ty_, vcons), ty_)
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
    let res = match tycker.compu(&compu) {
        | Compu::Hole(_) => todo!(),
        | Compu::VAbs(compu) => {
            let Abs(vpat, compu) = compu;
            let (def, param_ty) = vpat.try_destruct_def(tycker);
            let param_ty_ = cs::TypeLift { monad_ty, ty: param_ty }.build(tycker, env)?;
            cs::HAbs(cs::Ann(def, param_ty_)).try_vbody((tycker, env), |tycker, env, _def| {
                cs::TermLift { monad_ty, monad_impl, str_env, tm: compu }.build(tycker, env)
            })?
        }
        | Compu::VApp(compu) => {
            let App(fun, arg) = compu;
            let fun_ =
                cs::TermLift { monad_ty, monad_impl, str_env, tm: fun }.build(tycker, env)?;
            let arg_ =
                cs::TermLift { monad_ty, monad_impl, str_env, tm: arg }.build(tycker, env)?;
            App(fun_, arg_).build(tycker, env)
        }
        | Compu::TAbs(compu) => {
            let Abs(tpat, compu) = compu;
            let (def, param_kd) = tpat.try_destruct_def(tycker);
            let param_kd_ = param_kd;
            cs::HAbs(cs::Ann(def, param_kd_)).try_tbody(
                (tycker, env),
                |tycker, env, _def, abst| {
                    let thk_sig = cs::Thk(cs::Signature { monad_ty, ty: cs::AbstTy(abst) })
                        .build(tycker, env)?;
                    let str_name = {
                        use crate::fmt::*;
                        format!(
                            "str_{}",
                            abst.ugly(&Formatter::new(&tycker.scoped, &tycker.statics))
                        )
                    };
                    tycker.try_compu_vabs(env, str_name, thk_sig, |tycker, env, _str_var| {
                        cs::TermLift { monad_ty, monad_impl, str_env, tm: compu }.build(tycker, env)
                    })
                },
            )?
        }
        | Compu::TApp(compu) => {
            let App(fun, arg) = compu;
            let fun_ =
                cs::TermLift { monad_ty, monad_impl, str_env, tm: fun }.build(tycker, env)?;
            let arg_ = cs::TypeLift { monad_ty, ty: arg }.build(tycker, env)?;
            App(fun_, cs::Ty(arg_)).build(tycker, env)
        }
        | Compu::Fix(compu) => {
            let Fix(vpat, compu) = compu;
            let (def, param_ty) = vpat.try_destruct_def(tycker);
            let param_ty_ = cs::TypeLift { monad_ty, ty: param_ty }.build(tycker, env)?;
            cs::HAbs(cs::Ann(def, param_ty_)).try_fix((tycker, env), |tycker, env, _def| {
                cs::TermLift { monad_ty, monad_impl, str_env, tm: compu }.build(tycker, env)
            })?
        }
        | Compu::Force(compu) => {
            let Force(value) = compu;
            let value_ =
                cs::TermLift { monad_ty, monad_impl, str_env, tm: value }.build(tycker, env)?;
            Force(value_).build(tycker, env)
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
        | Compu::Do(_) => todo!(),
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
