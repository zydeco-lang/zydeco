//! Algebra structure translation `Str(T)` by type former.

use super::*;

/// Structure translation for a codata type.
///
/// The source codata directs the recursive structure translations of its
/// destructor signatures, while the translated codata is the interface
/// inhabited by the resulting `comatch`.
pub(super) struct CoDataStructureTranslation {
    pub(super) source: TypeId,
    pub(super) source_codata: CoDataId,
}

impl Construct<MonEnv, CompuId> for CoDataStructureTranslation {
    fn construct(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Self { source, source_codata } = self;
        let source_arms = tycker.statics.codatas[&source_codata].clone();
        let (env, target) = cs::TypeLift { ty: source }.construct(tycker, env)?;
        let monad_ty = env.monad_ty;

        Abs(cs::Ty(cs::Pat("Z", VType)), move |_tvar, carrier| {
            let monadic_value_ty = cs::Thk(App(monad_ty, carrier));
            Abs(cs::Pat("mz", monadic_value_ty), move |monadic_value: VPatId| {
                let function_ty = cs::Thk(Arrow(carrier, target));
                let source_arms = source_arms.clone();
                Abs(cs::Pat("f", function_ty), move |function: VPatId| {
                    cs::CoMatch(target, move |dtor, _target_signature| {
                        let source_signature = source_arms
                            .get(&dtor)
                            .expect("translated codata must preserve its destructors");
                        let continuation = Abs(cs::Pat("z", carrier), move |value: VPatId| {
                            Dtor(App(Force(cs::Value(function)), cs::Value(value)), dtor)
                        });
                        let structure = cs::Structure { ty: source_signature };
                        App(
                            App(App(structure, cs::Ty(carrier)), cs::Value(monadic_value)),
                            Thunk(continuation),
                        )
                    })
                })
            })
        })
        .construct(tycker, env)
    }
}

/// Resolve a recorded structure for a type reference, synthesizing the unique
/// trivial structure when the reference has value kind.
struct ReferencedStructure {
    ty: TypeId,
    structure: Option<ValueId>,
}

impl Construct<MonEnv, CompuId> for ReferencedStructure {
    fn construct(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        if let Some(structure) = self.structure {
            return Force(structure).construct(tycker, env);
        }

        let (env, kind) = cs::TypeOf(self.ty).construct(tycker, env)?;
        match tycker.kind_filled(&kind)?.to_owned() {
            | Kind::VType(VType) => cs::Top.construct(tycker, env),
            | _ => tycker.err(TyckError::MissingStructure(self.ty)),
        }
    }
}

/// Structure Translation `Str(T)`
pub(super) fn structure_translation(
    tycker: &mut Tycker, env: MonEnv, ty: TypeId,
) -> Result<(MonEnv, CompuId)> {
    let shareable = tycker.monadic_memo.as_ref().is_some_and(|memo| memo.share_structures)
        && env.subst.is_empty()
        && env.subst_abst.is_empty()
        && env.structure.def_map.is_empty()
        && env.structure.absts.is_empty()
        && !matches!(
            tycker.type_filled(&ty)?,
            Type::Abst(_)
                | Type::Label(_)
                | Type::Unit(_)
                | Type::Primitive(_)
                | Type::ValPi(_)
                | Type::Prod(_)
                | Type::Exists(_)
                | Type::ManifestKind(_)
                | Type::Data(_)
        );
    if !shareable {
        return structure_translation_inner(tycker, env, ty);
    }
    if let Some(&(var_val, compu_ty)) =
        tycker.monadic_memo.as_ref().and_then(|memo| memo.structures.get(&ty))
    {
        return Ok((env, Alloc::alloc(tycker, Force(var_val), compu_ty, &())));
    }
    let (env, compu) = structure_translation_inner(tycker, env, ty)?;
    let compu_ty = tycker.statics.annotations_compu[&compu];
    let thunk_ty = cs::Thk(compu_ty).build(tycker, &env.ty);
    let thunk_val: ValueId = Alloc::alloc(tycker, Thunk(compu), thunk_ty, &());
    let def: DefId = Alloc::alloc(tycker, VarName::new("__monadic_str"), thunk_ty.into(), &());
    let vpat: VPatId = Alloc::alloc(tycker, def, thunk_ty, &());
    let var_val: ValueId = Alloc::alloc(tycker, Value::Var(def), thunk_ty, &());
    if let Some(memo) = &mut tycker.monadic_memo {
        memo.structure_bindings.push((vpat, thunk_val));
        memo.structures.insert(ty, (var_val, compu_ty));
    }
    Ok((env, Alloc::alloc(tycker, Force(var_val), compu_ty, &())))
}

fn structure_translation_inner(
    tycker: &mut Tycker, env: MonEnv, ty: TypeId,
) -> Result<(MonEnv, CompuId)> {
    let res = match tycker.type_filled(&ty)? {
        | Type::Var(def) => ReferencedStructure {
            ty,
            structure: env
                .structure
                .def_map
                .get(&def)
                .and_then(|abst| env.structure.absts.get(abst))
                .copied(),
        }
        .construct(tycker, env)?,
        | Type::Abst(abst) if tycker.statics.seals.get(abst).is_some() => {
            nominal::NominalTranslation::structure(tycker, env, abst)?
        }
        | Type::Abst(abst) => {
            let abst = env.subst_abst.get(&abst).copied().unwrap_or(abst);
            ReferencedStructure { ty, structure: env.structure.absts.get(&abst).copied() }
                .construct(tycker, env)?
        }
        | Type::Abs(abs) => abs_structure_translation(tycker, env, abs)?,
        | Type::App(App(ty_f, ty_a)) => {
            // input: S_f S_a -> output: Str(S_f) Lift(S_a) { Str(S_a) }
            let str_f = cs::Structure { ty: ty_f };
            let ty_a_lift = cs::TypeLift { ty: ty_a };
            let str_a = cs::Structure { ty: ty_a };
            App(App(str_f, cs::Ty(ty_a_lift)), Thunk(str_a)).construct(tycker, env)?
        }
        | Type::Opaque(_) => unreachable!(),
        | Type::Named(Named(_, inner)) | Type::Proj(Proj(inner, _)) => {
            cs::Structure { ty: inner }.construct(tycker, env)?
        }
        // Unit, product, and existential types have the trivial structure `top`.
        | Type::Label(_)
        | Type::Unit(UnitTy)
        | Type::Primitive(_)
        | Type::ValPi(_)
        | Type::Prod(_)
        | Type::Exists(_)
        | Type::ManifestKind(_)
        | Type::Data(_) => cs::Top.construct(tycker, env)?,
        | Type::Thk(ThkTy) => thk_structure_translation(tycker, env)?,
        // The `OS` type is a primitive computation type and thus not allowed in monadic blocks.
        | Type::OS(_) => unreachable!(),
        | Type::Ret(RetTy) => ret_structure_translation(tycker, env)?,
        | Type::CoData(source_codata) => {
            CoDataStructureTranslation { source: ty, source_codata }.construct(tycker, env)?
        }
        | Type::Arrow(arrow) => arrow_structure_translation(tycker, env, arrow)?,
        | Type::Forall(ty_forall) => forall_structure_translation(tycker, env, ty_forall)?,
        | Type::PackPi(signature) => {
            PackPiStructureTranslation { source: ty, signature: *signature }
                .translate(tycker, env)?
        }
    };
    Ok(res)
}

fn abs_structure_translation(
    tycker: &mut Tycker, env: MonEnv, TypeAbstraction { binder, body }: TypeAbstraction,
) -> Result<(MonEnv, CompuId)> {
    // input: fn (X : K) => S -> output: fn (X : K) (str_X : Thk (Sig_K(X))) => Str(S)
    let (tvar, _) = binder.pattern.try_destruct_def(tycker);
    let svar = match tvar {
        | Some(tvar) => format!("str_{}", tycker.def_name(&tvar).plain()),
        | None => "str".to_string(),
    };
    Abs(cs::Ty((cs::TypeLift { ty: binder.pattern }, binder.witness)), move |_tvar, abst| {
        Abs(cs::StrPat(svar, abst, None), move |_str: VPatId| cs::Structure { ty: body })
    })
    .construct(tycker, env)
}

fn thk_structure_translation(tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
    // `Thk` is a type constructor, so its structure receives the argument type and its structure.
    // output: fn (X : CType) (_ : Thk (Sig_CType(X))) => <top>
    Abs(cs::Ty(cs::Pat("_", CType)), move |_tvar, abst| {
        let thk_sig = cs::Thk(cs::Signature { ty: cs::Type(abst) });
        Abs(cs::Pat("_", thk_sig), move |_var| cs::Top)
    })
    .construct(tycker, env)
}

fn ret_structure_translation(tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
    // output: fn (X : VType) (_ : Thk (Sig_VType(X))) => fn (Z : VType) => ! monad_impl .bind Z X
    let monad_impl = env.monad_impl;
    Abs(cs::Ty(cs::Pat("_", VType)), |_tvar, abst_x| {
        let thk_sig = cs::Thk(cs::Signature { ty: abst_x });
        Abs(cs::Pat("_", thk_sig), move |_var| {
            Abs(cs::Ty(cs::Pat("Z", VType)), move |_tvar, abst_z| {
                let body = cs::Dtor(Force(monad_impl), ".bind");
                App(App(body, cs::Ty(abst_z)), cs::Ty(abst_x))
            })
        })
    })
    .construct(tycker, env)
}

fn arrow_structure_translation(
    tycker: &mut Tycker, env: MonEnv, Arrow(ty_a, ty_b): Arrow<TypeId, TypeId>,
) -> Result<(MonEnv, CompuId)> {
    // input: A -> B
    // output: fn (Z : VType) (mz : Thk (M Z)) (f : Thk (Z -> [A] -> [B])) =>
    //           fn (x : [A]) => Str(B) Z mz { fn (z : Z) => ! f z x }
    Abs(cs::Ty(cs::Pat("Z", VType)), move |_tvar, abst_z| {
        let mz_ty = cs::Thk(App(env.monad_ty, abst_z));
        Abs(cs::Pat("mz", mz_ty), move |mz: VPatId| {
            let ty_a_ = cs::TypeLift { ty: ty_a };
            let ty_b_ = cs::TypeLift { ty: ty_b };
            let f_ty = cs::Thk(Arrow(abst_z, Arrow(ty_a_, ty_b_)));
            Abs(cs::Pat("f", f_ty), move |f: VPatId| {
                Abs(cs::Pat("x", ty_a_), move |x: VPatId| {
                    let alg_b = cs::Structure { ty: ty_b };
                    let kont = Abs(cs::Pat("z", abst_z), move |z: VPatId| {
                        App(App(Force(cs::Value(f)), cs::Value(z)), cs::Value(x))
                    });
                    App(App(App(alg_b, cs::Ty(abst_z)), cs::Value(mz)), Thunk(kont))
                })
            })
        })
    })
    .construct(tycker, env)
}

fn forall_structure_translation(
    tycker: &mut Tycker, env: MonEnv, ty_forall: Forall,
) -> Result<(MonEnv, CompuId)> {
    // input: forall (X : K) . B
    let Forall(binder, ty) = ty_forall;
    let abst = binder.witness;
    let source_pattern = binder.pattern;
    let kd = cs::TypeOf(abst);
    // output: fn (Z : VType) (mz : Thk (M Z)) (f : <f_ty>) => <body>
    Abs(cs::Ty(cs::Pat("Z", VType)), move |_tvar, abst_z| {
        Abs(cs::Pat("mz", cs::Thk(App(env.monad_ty, abst_z))), move |mz: VPatId| {
            // construct abstract type X first
            // substitute the abstract type `abst` with `abst_x`
            cs::CBind::new(cs::AbstPat(abst, cs::Ann("X", kd)), move |abst_x: AbstId| {
                // <f_ty> = Thk (Z -> forall (X : K) . Thk (Sig_K(X)) -> [B])
                let f_ty = cs::Thk(Arrow(
                    abst_z,
                    cs::PatternForall {
                        pattern: cs::ReboundTypePattern { source: source_pattern, witness: abst_x },
                        witness: abst_x,
                        // substitute the abstract type `abst` in the `ty` with `abst_x`
                        body: Arrow(cs::Thk(cs::Signature { ty: abst_x }), cs::TypeLift { ty }),
                    },
                ));
                Abs(cs::Pat("f", f_ty), move |f: VPatId| {
                    // <body> = fn (X : K) (str_X : Thk (Sig_K(X))) => Str(B) Z mz <kont>
                    Abs(
                        cs::Ty((
                            cs::ReboundTypePattern { source: source_pattern, witness: abst_x },
                            abst_x,
                        )),
                        move |_, abst_x: AbstId| {
                            Abs(cs::StrPat("str_X", abst_x, None), move |str_x: VPatId| {
                                // <kont> = { fn (z : Z) => ! f z X str_X }
                                let kont = Abs(cs::Pat("z", abst_z), move |z: VPatId| {
                                    let f = cs::Value(f);
                                    let z = cs::Value(z);
                                    let str_x = cs::Value(str_x);
                                    let argument = cs::PatternArgument {
                                        pattern: source_pattern,
                                        payload: abst_x,
                                    };
                                    App(App(App(Force(f), z), cs::Ty(argument)), str_x)
                                });
                                // substitute `abst` in `ty` with `abst_x`
                                let str_ = cs::Structure { ty };
                                App(App(App(str_, cs::Ty(abst_z)), cs::Value(mz)), Thunk(kont))
                            })
                        },
                    )
                })
            })
        })
    })
    .construct(tycker, env)
}
