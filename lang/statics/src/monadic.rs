use crate::{syntax::*, *};
use std::collections::HashMap;

/// Signature Translation `Sig_K(T)`
///
/// Given the monad type `M` and the type environment, implement the signature
/// translation from input type `T` to an output computation type `CompuId`
/// which is the type of algebra of `T`. Specifically,
///
/// + `T: VType` -> `Top`
/// + `T: CType` -> `Algebra M T`
/// + `T: K_1 -> K_2` -> `forall X: K_1 . Thk (Sig_K_1(X)) -> Sig_K_2(T X)`
pub fn signature_translation(
    tycker: &mut Tycker, monad_ty: TypeId, env: &Env<AnnId>, ty: TypeId,
) -> Result<TypeId> {
    let kd = tycker.statics.annotations_type[&ty].to_owned();
    let res = match tycker.kind_filled(&kd)?.to_owned() {
        | Kind::VType(VType) => tycker.type_top(env),
        | Kind::CType(CType) => tycker.algebra_mo_car(env, monad_ty, ty),
        | Kind::Arrow(Arrow(kd_1, _)) => {
            let abst = Alloc::alloc(tycker, None, kd_1);
            let ty_1 = Alloc::alloc(tycker, abst, kd_1);
            let sig_1 = signature_translation(tycker, monad_ty, env, ty_1)?;
            let thk_sig_1 = tycker.thk_arg(env, sig_1);
            let ty_2 = App(ty, ty_1).build(tycker, env);
            let sig_2 = signature_translation(tycker, monad_ty, env, ty_2)?;
            let arr = Arrow(thk_sig_1, sig_2).build(tycker, env);
            tycker.type_forall(env, abst, arr)
        }
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
    pub fn extend_with_abst(&self, abst: AbstId, str: ValueId) -> Self {
        let mut new = self.clone();
        new.absts.insert(abst, str);
        new
    }
}

/// Structure Translation `Str(T)`
pub fn structure_translation(
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
            tycker.compu_force(env, str)
        }
        | Type::Abst(abst) => {
            let str = match str_env.absts.get(&abst) {
                | Some(str) => str.to_owned(),
                | None => tycker.err(
                    TyckError::MissingStructure(ty.to_owned()),
                    std::panic::Location::caller(),
                )?,
            };
            tycker.compu_force(env, str)
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
                let abst_ty = Alloc::alloc(tycker, abst, kd);
                let sig_ty = signature_translation(tycker, monad_ty, env, abst_ty)?;
                let svar_ty = tycker.thk_arg(env, sig_ty);
                tycker.try_compu_vabs(env, svar, svar_ty, |tycker, _env, str_def| {
                    let str = Alloc::alloc(tycker, str_def, svar_ty);
                    let str_env = str_env.extend_with_abst(abst, str);
                    structure_translation(tycker, monad_ty, monad_impl, &str_env, env, ty)
                })
            })?
        }
        | Type::App(ty) => {
            // input: S_f S_a
            let App(ty_f, ty_a) = ty;
            // output: Str(S_f) Lift(S_a) Str(S_a)
            let str_f = structure_translation(tycker, monad_ty, monad_impl, str_env, env, ty_f)?;
            let ty_lift_a = type_translation(tycker, monad_ty, env, ty_a)?;
            let str_f_app_sig = App(str_f, ty_lift_a).build(tycker, env);
            let str_a = structure_translation(tycker, monad_ty, monad_impl, str_env, env, ty_a)?;
            let thk_str_a = tycker.value_thunk(env, str_a);
            tycker.compu_vapp(env, str_f_app_sig, thk_str_a)
        }
        // primitive types are not allowed in monadic blocks
        | Type::Int(_) | Type::Char(_) | Type::String(_) => unreachable!(),
        // unit, product, and existential types have the trivial structure `top`
        // (so should data types)
        | Type::Unit(UnitTy) | Type::Prod(_) | Type::Exists(_) | Type::Data(_) => {
            tycker.compu_top(env)
        }
        // the thunk type is itself a type constructor,
        // so its structure takes a type and the type's structure as arguments
        | Type::Thk(ThkTy) => {
            // output: fn (X : CType) (_ : Thk (Sig_CType(X))) -> <top>
            tycker.try_compu_tabs(env, "_", CType, |tycker, _env, _tvar, abst| {
                let abst_ty = abst.build(tycker, env);
                let sig = signature_translation(tycker, monad_ty, env, abst_ty)?;
                let thk_sig = tycker.thk_arg(env, sig);
                tycker.try_compu_vabs(env, "_", thk_sig, |tycker, _env, _var| {
                    // <top> = comatch end
                    Ok(tycker.compu_top(env))
                })
            })?
        }
        // the os type is a primitive computation type and thus not allowed in monadic blocks
        | Type::OS(_) => unreachable!(),
        | Type::Ret(RetTy) => {
            // output: fn (X : VType) (_ : Thk (Sig_VType(X))) -> <monadic_bind>
            tycker.try_compu_tabs(env, "_", VType, |tycker, _env, _tvar, abst_x| {
                let abst_x_ty: TypeId = abst_x.build(tycker, env);
                let sig = signature_translation(tycker, monad_ty, env, abst_x_ty)?;
                tycker.try_compu_vabs(env, "_", Thunk(sig), |tycker, _env, _var| {
                    // <monadic_bind> = fn (Z : VType) -> ! monad_impl .bind Z X
                    tycker.try_compu_tabs(env, "Z", VType, |tycker, _env, _tvar_z, abst_z| {
                        let abst_z_ty: TypeId = abst_z.build(tycker, env);
                        let res =
                            App(App(cs::Dtor(Force(monad_impl), ".bind"), abst_z_ty), abst_x_ty);
                        Ok(res)
                    })
                })
            })?
        }
        | Type::CoData(coda) => todo!(),
        | Type::Arrow(ty) => {
            let Arrow(ty_p, ty_b) = ty;
            tycker.try_compu_tabs(env, "Z", VType, |tycker, env, _tvar_z, abst_z| {
                let abst_z_ty: TypeId = abst_z.build(tycker, env);
                let mz_ty = App(monad_ty, abst_z_ty);
                tycker.try_compu_vabs(env, "mz", mz_ty, |tycker, env, var_mz| -> Result<_> {
                    let ty_p_ = type_translation(tycker, monad_ty, env, ty_p)?;
                    let ty_b_ = type_translation(tycker, monad_ty, env, ty_b)?;
                    let f_ty = Thunk(Arrow(abst_z_ty, Arrow(ty_p_, ty_b_)));
                    tycker.try_compu_vabs(env, "f", f_ty, |tycker, env, var_f| {
                        tycker.try_compu_vabs(env, "x", ty_p_, |tycker, env, var_x| {
                            let alg_b = structure_translation(
                                tycker, monad_ty, monad_impl, str_env, env, ty_b,
                            )?;
                            let mz: ValueId = var_mz.build(tycker, env);
                            let kont =
                                tycker.compu_vabs(env, "z", abst_z_ty, |tycker, env, var_z| {
                                    let f: ValueId = var_f.build(tycker, env);
                                    let z: ValueId = var_z.build(tycker, env);
                                    let x: ValueId = var_x.build(tycker, env);
                                    App(App(Force(f), z), x)
                                });
                            let res = App(App(App(alg_b, abst_z_ty), mz), Thunk(kont));
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
pub fn type_translation(
    tycker: &mut Tycker, monad_ty: TypeId, env: &Env<AnnId>, ty: TypeId,
) -> Result<TypeId> {
    let kd = tycker.statics.annotations_type[&ty].to_owned();
    let res = match tycker.type_filled(&ty)?.to_owned() {
        | Type::Var(def) => Alloc::alloc(tycker, def, kd),
        | Type::Abst(abst) => Alloc::alloc(tycker, abst, kd),
        | Type::Abs(ty) => {
            let Abs(tpat, ty) = ty;
            let (def, param_kd) = tpat.destruct_def(tycker);
            let tpat_ = Alloc::alloc(tycker, def, param_kd);
            let ty_ = type_translation(tycker, monad_ty, env, ty)?;
            Alloc::alloc(tycker, Abs(tpat_, ty_), kd)
        }
        | Type::App(ty) => {
            let App(ty_f, ty_a) = ty;
            let ty_f_ = type_translation(tycker, monad_ty, env, ty_f)?;
            let ty_a_ = type_translation(tycker, monad_ty, env, ty_a)?;
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
                    let ty_ = type_translation(tycker, monad_ty, env, ty)?;
                    Ok((ctor, ty_))
                })
                .collect::<Result<im::Vector<_>>>()?;
            let data_ = Data::new(arms_.iter().cloned());
            let data = tycker.statics.datas.lookup_or_alloc(arms_, data_);
            Alloc::alloc(tycker, data, tycker.statics.annotations_type[&ty].clone())
        }
        | Type::Unit(UnitTy) => ty,
        | Type::Prod(ty) => {
            let Prod(ty_1, ty_2) = ty;
            let ty_1_ = type_translation(tycker, monad_ty, env, ty_1)?;
            let ty_2_ = type_translation(tycker, monad_ty, env, ty_2)?;
            Prod(ty_1_, ty_2_).build(tycker, env)
        }
        | Type::Exists(ty) => {
            let Exists(abst, ty) = ty;
            let abst_kd = tycker.statics.annotations_abst[&abst];
            let ty_abst = Alloc::alloc(tycker, abst, abst_kd);
            let sig = signature_translation(tycker, monad_ty, env, ty_abst)?;
            let thk_sig = tycker.thk_arg(env, sig);
            let ty_ = type_translation(tycker, monad_ty, env, ty)?;
            let prod = Prod(thk_sig, ty_).build(tycker, env);
            tycker.type_exists(env, abst, prod)
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
                    let ty_ = type_translation(tycker, monad_ty, env, ty)?;
                    Ok((ctor, ty_))
                })
                .collect::<Result<im::Vector<_>>>()?;
            let coda_ = CoData::new(arms_.iter().cloned());
            let coda = tycker.statics.codatas.lookup_or_alloc(arms_, coda_);
            Alloc::alloc(tycker, coda, tycker.statics.annotations_type[&ty].clone())
        }
        | Type::Arrow(ty) => {
            let Arrow(ty_1, ty_2) = ty;
            let ty_1_ = type_translation(tycker, monad_ty, env, ty_1)?;
            let ty_2_ = type_translation(tycker, monad_ty, env, ty_2)?;
            Arrow(ty_1_, ty_2_).build(tycker, env)
        }
        | Type::Forall(ty) => {
            let Forall(abst, ty) = ty;
            let abst_kd = tycker.statics.annotations_abst[&abst];
            let ty_abst = Alloc::alloc(tycker, abst, abst_kd);
            let sig = signature_translation(tycker, monad_ty, env, ty_abst)?;
            let thk_sig = tycker.thk_arg(env, sig);
            let ty_ = type_translation(tycker, monad_ty, env, ty)?;
            let arr = Arrow(thk_sig, ty_).build(tycker, env);
            tycker.type_forall(env, abst, arr)
        }
    };
    Ok(res)
}

/// Term Translation (Value) `[V]`
pub fn value_translation(
    tycker: &mut Tycker, monad_ty: TypeId, monad_impl: ValueId, str_env: &StructureEnv,
    env: &Env<AnnId>, value: ValueId,
) -> Result<ValueId> {
    let ty = tycker.statics.annotations_value[&value];
    let ty_ = type_translation(tycker, monad_ty, env, ty)?;
    let res = match tycker.value(&value).to_owned() {
        | Value::Hole(Hole) => Alloc::alloc(tycker, Hole, ty_),
        // figure out how to handle literals
        | Value::Lit(_) => unreachable!(),
        // variables should be freshed and substituted
        | Value::Var(def) => todo!(),
        | Value::Thunk(value) => {
            let Thunk(body) = value;
            let body_ = computation_translation(tycker, monad_ty, monad_impl, env, body)?;
            Thunk(body_).build(tycker, env)
        }
        | Value::Ctor(value) => {
            let Ctor(ctor, body) = value;
            let body_ = value_translation(tycker, monad_ty, monad_impl, str_env, env, body)?;
            let ty_ = type_translation(tycker, monad_ty, env, ty)?;
            Ann { tm: Ctor(ctor, body_), ty: ty_ }.build(tycker, env)
        }
        | Value::Triv(Triv) => value,
        | Value::VCons(value) => {
            let Cons(value_1, value_2) = value;
            let value_1_ = value_translation(tycker, monad_ty, monad_impl, str_env, env, value_1)?;
            let value_2_ = value_translation(tycker, monad_ty, monad_impl, str_env, env, value_2)?;
            Cons(value_1_, value_2_).build(tycker, env)
        }
        | Value::TCons(value) => {
            let Cons(a_ty, body) = value;
            let str = structure_translation(tycker, monad_ty, monad_impl, str_env, env, a_ty)?;
            let thk_str = tycker.value_thunk(env, str);
            let body_ = value_translation(tycker, monad_ty, monad_impl, str_env, env, body)?;
            let vcons = tycker.value_vcons(env, thk_str, body_);
            let a_ty_ = type_translation(tycker, monad_ty, env, a_ty)?;
            // existential type construct should be type-guided
            Alloc::alloc(tycker, Cons(a_ty_, vcons), ty_)
        }
    };
    Ok(res)
}

/// Term Translation (Computation) `[C]`
pub fn computation_translation(
    tycker: &mut Tycker, monad_ty: TypeId, monad_impl: ValueId, env: &Env<AnnId>, compu: CompuId,
) -> Result<CompuId> {
    let res = match tycker.compu(&compu) {
        | Computation::Hole(_) => todo!(),
        | Computation::VAbs(_) => todo!(),
        | Computation::VApp(_) => todo!(),
        | Computation::TAbs(_) => todo!(),
        | Computation::TApp(_) => todo!(),
        | Computation::Fix(_) => todo!(),
        | Computation::Force(_) => todo!(),
        | Computation::Ret(_) => todo!(),
        | Computation::Do(_) => todo!(),
        | Computation::Let(_) => todo!(),
        | Computation::Match(_) => todo!(),
        | Computation::CoMatch(_) => todo!(),
        | Computation::Dtor(_) => todo!(),
    };
    Ok(res)
}

/// Monadic Block Elaboration (Value)
pub fn value_monadic_elaboration(
    tycker: &mut Tycker, monad_ty: TypeId, monad_impl: ValueId, env: &Env<AnnId>, value: ValueId,
) -> ValueId {
    let _ = tycker;
    let _ = monad_ty;
    let _ = monad_impl;
    let _ = env;
    let _ = value;
    todo!()
}

/// Monadic Block Elaboration (Computation)
pub fn computation_monadic_elaboration(
    tycker: &mut Tycker, monad_ty: TypeId, monad_impl: ValueId, env: &Env<AnnId>, compu: CompuId,
) -> CompuId {
    let _ = tycker;
    let _ = monad_ty;
    let _ = monad_impl;
    let _ = env;
    let _ = compu;
    todo!()
}
