//! Value-pattern, value, intrinsic, and computation monadic translation.

use super::*;

/// Value Pattern Translation `[VPat]`
// Remaining worklist: split package pattern translation into its phases.
#[allow(clippy::too_many_lines)]
pub(super) fn package_pattern_translation(
    tycker: &mut Tycker, env: MonEnv, witnesses: &[StaticPatId],
    source_layout: Option<&[PackPiWitnessLayoutEntry]>, body: VPatId, translated_ty: TypeId,
) -> Result<(MonEnv, VPatId)> {
    let Some((&witness, remaining)) = witnesses.split_first() else {
        if let Some(source_layout) = source_layout
            && !source_layout.is_empty()
        {
            // The remaining witnesses belong to openings nested in this
            // package's body; continue the layout-driven walk there.
            let layout = PackPiWitnessLayout(source_layout.to_vec());
            return PackPiPatternTranslation { pattern: body, layout }.translate(tycker, env);
        }
        return cs::TermLift { tm: body }.construct(tycker, env);
    };
    let (source_entry, remaining_source_layout) = match source_layout {
        | Some(source_layout) => {
            let Some((&source_entry, remaining)) = source_layout.split_first() else {
                return tycker.err(TyckError::PackageWitnessArityMismatch {
                    expected: witnesses.len(),
                    found: 0,
                });
            };
            (Some(source_entry), Some(remaining))
        }
        | None => (None, None),
    };

    match witness {
        | StaticPatId::Kind(witness) => {
            let Type::ManifestKind(ManifestKind { definition, body: translated_tail_ty, .. }) =
                tycker.type_filled(&translated_ty)?.to_owned()
            else {
                return tycker.err(TyckError::PackageWitnessArityMismatch {
                    expected: witnesses.len(),
                    found: 0,
                });
            };
            if !matches!(source_entry, None | Some(PackPiWitnessLayoutEntry::KindManifest)) {
                return tycker.err(TyckError::PackageWitnessArityMismatch {
                    expected: witnesses.len(),
                    found: 0,
                });
            }
            let (env, witness) = kind_pattern_translation(tycker, env, witness, definition)?;
            let (env, body) = package_pattern_translation(
                tycker,
                env,
                remaining,
                remaining_source_layout,
                body,
                translated_tail_ty,
            )?;
            let package = Alloc::alloc(
                tycker,
                ConsN(vec![StaticPatId::Kind(witness)], body),
                translated_ty,
                &(),
            );
            Ok((env, package))
        }
        | StaticPatId::Type(witness) => {
            let Type::Exists(exists) = tycker.type_filled(&translated_ty)?.to_owned() else {
                return tycker.err(TyckError::PackageWitnessArityMismatch {
                    expected: witnesses.len(),
                    found: 0,
                });
            };
            let Exists {
                binder: translated_binder,
                mode: translated_mode,
                body: translated_body_ty,
            } = *exists;
            let domain_abst = translated_binder.witness;
            let (env, witness) = cs::TypeLift { ty: witness }.construct(tycker, env)?;
            let (witness_var, _) = witness.try_destruct_def(tycker);
            let mut env = env;
            let (pattern_abst, bound_ty, structure) = match (source_entry, translated_mode) {
                | (
                    Some(PackPiWitnessLayoutEntry::Abstract(source_skolem)),
                    ExistsMode::Abstract,
                ) => {
                    let pattern_abst = Alloc::alloc(tycker, witness, (), &());
                    tycker.transfer_builtin_role(source_skolem, pattern_abst)?;
                    tycker.statics.existential_skolems.ensure(pattern_abst);
                    env.ty = env.ty.with_skolem(pattern_abst);
                    env.subst_abst.insert_mut(source_skolem, pattern_abst);
                    let (env_, abst_ty) = cs::Type(pattern_abst).construct(tycker, env)?;
                    env = env_;
                    (pattern_abst, abst_ty, PackagePatternStructure::Abstract)
                }
                | (None, ExistsMode::Abstract) => {
                    let (env_, abst_ty) = cs::Type(domain_abst).construct(tycker, env)?;
                    env = env_;
                    (domain_abst, abst_ty, PackagePatternStructure::Abstract)
                }
                | (
                    Some(PackPiWitnessLayoutEntry::TypeManifest),
                    ExistsMode::Manifest(definition),
                )
                | (None, ExistsMode::Manifest(definition)) => {
                    (domain_abst, definition, PackagePatternStructure::Manifest)
                }
                | (
                    Some(
                        PackPiWitnessLayoutEntry::KindManifest
                        | PackPiWitnessLayoutEntry::Abstract(_),
                    ),
                    ExistsMode::Manifest(_),
                )
                | (
                    Some(
                        PackPiWitnessLayoutEntry::KindManifest
                        | PackPiWitnessLayoutEntry::TypeManifest,
                    ),
                    ExistsMode::Abstract,
                ) => {
                    return tycker.err(TyckError::PackageWitnessArityMismatch {
                        expected: witnesses.len(),
                        found: 0,
                    });
                }
            };
            let translated_body_ty = if pattern_abst == domain_abst {
                translated_body_ty
            } else {
                translated_body_ty.subst_abst(tycker, (domain_abst, bound_ty))?
            };
            let Type::Prod(Prod(translated_components)) =
                tycker.type_filled(&translated_body_ty)?.to_owned()
            else {
                unreachable!("translated existential body must contain its structure")
            };
            let [structure_ty, translated_tail_ty, ..] = translated_components.as_slice() else {
                unreachable!("translated existential body starts with its structure")
            };
            let structure_ty = *structure_ty;
            let translated_tail_ty = *translated_tail_ty;
            if let Some(witness_var) = witness_var {
                env.ty += [(witness_var, bound_ty.into())];
            }
            let (env, structure) = match structure {
                | PackagePatternStructure::Abstract => {
                    cs::StrPat("str", pattern_abst, witness_var).construct(tycker, env)?
                }
                | PackagePatternStructure::Manifest => {
                    cs::Pat("str", structure_ty).construct(tycker, env)?
                }
            };
            let (env, body) = package_pattern_translation(
                tycker,
                env,
                remaining,
                remaining_source_layout,
                body,
                translated_tail_ty,
            )?;
            let product = Alloc::alloc(
                tycker,
                ss::ValuePattern::VCons(vec![structure, body]),
                translated_body_ty,
                &(),
            );
            let package = Alloc::alloc(
                tycker,
                ConsN(vec![StaticPatId::Type(witness)], product),
                translated_ty,
                &(),
            );
            Ok((env, package))
        }
    }
}

fn package_value_translation(
    tycker: &mut Tycker, env: MonEnv, witnesses: &[StaticTermId], body: ValueId,
    translated_ty: TypeId,
) -> Result<(MonEnv, ValueId)> {
    let Some((&witness, remaining)) = witnesses.split_first() else {
        return cs::TermLift { tm: body }.construct(tycker, env);
    };
    match witness {
        | StaticTermId::Kind(witness) => {
            let Type::ManifestKind(ManifestKind { body: translated_tail_ty, .. }) =
                tycker.type_filled(&translated_ty)?.to_owned()
            else {
                unreachable!("translated manifest-kind package field must remain manifest")
            };
            let (env, body) =
                package_value_translation(tycker, env, remaining, body, translated_tail_ty)?;
            let package = Alloc::alloc(
                tycker,
                ConsN(vec![StaticTermId::Kind(witness)], body),
                translated_ty,
                &(),
            );
            Ok((env, package))
        }
        | StaticTermId::Type(witness) => {
            let Some((_abst, translated_body_ty)) = translated_ty.destruct_exists(tycker) else {
                unreachable!("translated type package field must remain existential")
            };
            let Type::Prod(Prod(translated_components)) =
                tycker.type_filled(&translated_body_ty)?.to_owned()
            else {
                unreachable!("translated existential body must contain its structure")
            };
            let translated_tail_ty = *translated_components.last().expect(
                "translated existential body starts with its structure and ends with its body",
            );

            let (env, translated_witness) = cs::TypeLift { ty: witness }.construct(tycker, env)?;
            let (env, structure) = cs::Structure { ty: witness }.construct(tycker, env)?;
            let (env, structure) = Thunk(structure).construct(tycker, env)?;
            let (env, body) =
                package_value_translation(tycker, env, remaining, body, translated_tail_ty)?;
            let product = Alloc::alloc(
                tycker,
                ss::Value::VCons(vec![structure, body]),
                translated_body_ty,
                &(),
            );
            let package = Alloc::alloc(
                tycker,
                ConsN(vec![StaticTermId::Type(translated_witness)], product),
                translated_ty,
                &(),
            );
            Ok((env, package))
        }
    }
}

pub(super) fn value_pattern_translation(
    tycker: &mut Tycker, env: MonEnv, vpat: VPatId,
) -> Result<(MonEnv, VPatId)> {
    use ValuePattern as VPat;
    let (env, ty) = cs::TypeOf(vpat).construct(tycker, env)?;
    let (env, ty_) = cs::TypeLift { ty }.construct(tycker, env)?;
    let (env, vpat_) = match tycker.statics.vpats[&vpat].to_owned() {
        | VPat::Hole(hole) => cs::Pat(hole, ty_).construct(tycker, env)?,
        | VPat::Var(def) => {
            // create a fresh variable, and track the substitution
            cs::Pat(def, ty_).construct(tycker, env)?
        }
        | VPat::Named(vpat) => {
            let Named(name, inner) = vpat;
            let (env, inner) = value_pattern_translation(tycker, env, inner)?;
            let named = Alloc::alloc(tycker, Named(name, inner), ty_, &());
            (env, named)
        }
        | VPat::Ctor(vpat) => {
            let Ctor(ctor, body) = vpat;
            let body_ = cs::TermLift { tm: body };
            cs::Pat(cs::Ctor(ctor, body_), ty_).construct(tycker, env)?
        }
        | VPat::Alias(Alias(patterns)) => {
            let (env, patterns) = patterns.into_iter().try_fold(
                (env, Vec::new()),
                |(env, mut output), pattern| -> Result<_> {
                    let (env, pattern) = value_pattern_translation(tycker, env, pattern)?;
                    output.push(pattern);
                    Ok((env, output))
                },
            )?;
            let alias = Alias(ConsN::from_vec(patterns).unwrap());
            let pattern = Alloc::alloc(tycker, alias, ty_, &());
            (env, pattern)
        }
        | VPat::Triv(Triv) => Triv.construct(tycker, env)?,
        | VPat::Lit(literal) => {
            let pattern = Alloc::alloc(tycker, ValuePattern::Lit(literal), ty_, &());
            (env, pattern)
        }
        | VPat::VCons(components) => {
            let mut items =
                components.into_iter().map(|tm| cs::TermLift { tm }).collect::<Vec<_>>();
            let tail =
                items.pop().expect("a translated product pattern has at least two components");
            ConsN(items, tail).construct(tycker, env)?
        }
        | VPat::SCons(vpat) => {
            let ConsN(witnesses, body) = vpat;
            package_pattern_translation(tycker, env, &witnesses, None, body, ty_)?
        }
        | VPat::View(view) => {
            let ss::ViewPattern { function, pattern } = *view;
            let (env, function) = value_translation(tycker, env, function)?;
            let (env, pattern) = value_pattern_translation(tycker, env, pattern)?;
            let view = ss::ViewPattern { function, pattern };
            let pattern = Alloc::alloc(tycker, Box::new(view), ty_, &());
            (env, pattern)
        }
    };
    Ok((env, vpat_))
}

/// Term Translation (Value) `[V]`
pub(super) fn value_translation(
    tycker: &mut Tycker, env: MonEnv, value: ValueId,
) -> Result<(MonEnv, ValueId)> {
    let (env, ty) = cs::TypeOf(value).construct(tycker, env)?;
    if let Value::ValAbs(abstraction) = tycker.statics.values[&value].to_owned() {
        let signature = ValueFunctionTranslation::signature(tycker, ty)?;
        let (env, translated) =
            ValueFunctionTranslation { signature }.abstraction(tycker, env, abstraction)?;
        if let Some(origin) = tycker.statics.terms.source(&value.into()) {
            tycker.statics.terms.record(origin, translated.into());
        }
        return Ok((env, translated));
    }
    let (env, ty_) = cs::TypeLift { ty }.construct(tycker, env)?;
    let (env, res) = match tycker.statics.values[&value].to_owned() {
        | Value::Hole(Hole) => cs::Ann(Hole, ty_).construct(tycker, env)?,
        | Value::Lit(literal) => {
            let literal = Alloc::alloc(tycker, literal, ty_, &());
            (env, literal)
        }
        | Value::Var(def) => {
            // substitute according to the environment
            match env.subst.get(&def).cloned() {
                | Some(def_) => {
                    // the definition is closed in this monadic block
                    let alloc = Alloc::alloc(tycker, def_, ty_, &());
                    (env, alloc)
                }
                | None => {
                    use zydeco_foundation::arena::ArenaAccess;
                    // it should then be global and should be in the inlinables
                    let Some(value) = tycker.statics.inlinables.get(&def).cloned() else {
                        tycker.err(TyckError::NotInlinable(def))?
                    };
                    cs::TermLift { tm: value }.construct(tycker, env)?
                }
            }
        }
        | Value::Named(value) => {
            let Named(name, inner) = value;
            let (env, inner) = value_translation(tycker, env, inner)?;
            let named = Alloc::alloc(tycker, Named(name, inner), ty_, &());
            (env, named)
        }
        | Value::Let(value) => {
            let Let { binder, bindee, tail } = value;
            Let {
                binder: cs::TermLift { tm: binder },
                bindee: cs::TermLift { tm: bindee },
                tail: move |_| cs::TermLift { tm: tail },
            }
            .construct(tycker, env)?
        }
        | Value::Match(Match { scrut, arms }) => {
            let (env, scrut) = value_translation(tycker, env, scrut)?;
            let arms = arms
                .into_iter()
                .map(|Matcher { binder, tail }| {
                    let (local, binder) = value_pattern_translation(tycker, env.clone(), binder)?;
                    let (_, tail) = value_translation(tycker, local, tail)?;
                    Ok(Matcher { binder, tail })
                })
                .collect::<Result<Vec<_>>>()?;
            let value = Alloc::alloc(tycker, Value::Match(Match { scrut, arms }), ty_, &());
            (env, value)
        }
        | Value::IntOp(IntValueOp { operation, operands: [left, right] }) => {
            let (env, left) = value_translation(tycker, env, left)?;
            let (env, right) = value_translation(tycker, env, right)?;
            let value = Alloc::alloc(
                tycker,
                Value::IntOp(IntValueOp { operation, operands: [left, right] }),
                ty_,
                &(),
            );
            (env, value)
        }
        | Value::IntrinsicOp(operation) => {
            intrinsic_op_translation(tycker, env, ty, ty_, operation)?
        }
        | Value::ValAbs(_) => unreachable!("value abstractions translate their binder scope first"),
        | Value::ValApp(App(function, argument)) => {
            ValueFunctionTranslation::application(tycker, env, function, argument, ty_)?
        }
        | Value::Thunk(value) => {
            let Thunk(body) = value;
            Thunk(cs::TermLift { tm: body }).construct(tycker, env)?
        }
        | Value::Ctor(value) => {
            let Ctor(ctor, body) = value;
            let body_ = cs::TermLift { tm: body };
            let ty_ = cs::TypeLift { ty };
            cs::Ann(cs::Ctor(ctor, body_), ty_).construct(tycker, env)?
        }
        | Value::Triv(Triv) => Triv.construct(tycker, env)?,
        | Value::VCons(components) => {
            let mut items =
                components.into_iter().map(|tm| cs::TermLift { tm }).collect::<Vec<_>>();
            let tail = items.pop().expect("a translated product value has at least two components");
            ConsN(items, tail).construct(tycker, env)?
        }
        | Value::SCons(value) => {
            let ConsN(witnesses, body) = value;
            package_value_translation(tycker, env, &witnesses, body, ty_)?
        }
        | Value::Seal(inner) => {
            let (env, lifted) = value_translation(tycker, env, inner)?;
            let sealed = Alloc::alloc(tycker, Value::Seal(lifted), ty_, &());
            (env, sealed)
        }
        | Value::Proj(value) => {
            let Proj(head, field) = value;
            ValueProjectionTranslation { head, field, result: ty_ }.translate(tycker, env)?
        }
    };
    if let Some(origin) = tycker.statics.terms.source(&value.into()) {
        tycker.statics.terms.record(origin, res.into());
    }
    Ok((env, res))
}

fn peel_arrows(tycker: &mut Tycker, mut cursor: TypeId) -> (Vec<TypeId>, TypeId) {
    let mut param_tys = Vec::new();
    while let Some((param_ty, next_ty)) = cursor.destruct_arrow(tycker) {
        param_tys.push(param_ty);
        cursor = next_ty;
    }
    (param_tys, cursor)
}

fn alloc_intrinsic_params(
    tycker: &mut Tycker, mut env: MonEnv, param_tys: Vec<TypeId>,
) -> Result<(MonEnv, Vec<(VPatId, ValueId)>)> {
    let mut params = Vec::with_capacity(param_tys.len());
    for (index, param_ty) in param_tys.into_iter().enumerate() {
        let lifted_param;
        (env, lifted_param) = cs::TypeLift { ty: param_ty }.construct(tycker, env)?;
        let def = Alloc::alloc(
            tycker,
            VarName::new(format!("__intrinsic_arg_{index}")),
            AnnId::Type(lifted_param),
            &(),
        );
        let vpat = Alloc::alloc(tycker, ValuePattern::Var(def), lifted_param, &());
        let val = Alloc::alloc(tycker, Value::Var(def), lifted_param, &());
        params.push((vpat, val));
    }
    Ok((env, params))
}

fn intrinsic_op_translation(
    tycker: &mut Tycker, env: MonEnv, ty: TypeId, ty_: TypeId,
    operation: zydeco_syntax::IntrinsicOp,
) -> Result<(MonEnv, ValueId)> {
    let op_val = Alloc::alloc(tycker, Value::IntrinsicOp(operation), ty, &());
    let body_ty = ty.destruct_thk_app(tycker).expect("intrinsic op has Thk classifier");
    let (env, translated) = if let Some((binder, inner_ty)) = body_ty.destruct_forall_binder(tycker)
    {
        let (param_tys, _) = peel_arrows(tycker, inner_ty);
        let ctype = CType.build(tycker, &env.ty);
        let (mut body_env, tpat) =
            cs::TypeLift { ty: binder.pattern }.construct(tycker, env.clone())?;
        let abst = Alloc::alloc(tycker, tpat, (), &());
        body_env.subst_abst.insert_mut(binder.witness, abst);
        let (body_env, str_pat) = cs::StrPat("str", abst, None).construct(tycker, body_env)?;
        let r_ty = Alloc::alloc(tycker, abst, ctype, &());
        let (mut body_env, params) = alloc_intrinsic_params(tycker, body_env, param_tys)?;
        let force_op = Force(op_val).construct(tycker, body_env.clone())?.1;
        let tapp = App(force_op, cs::Ty(r_ty)).construct(tycker, body_env.clone())?.1;
        let mut body = tapp;
        for (_, val) in &params {
            (body_env, body) = App(body, *val).construct(tycker, body_env)?;
        }
        for (vpat, _) in params.into_iter().rev() {
            (body_env, body) = Abs(vpat, move |_| body).construct(tycker, body_env)?;
        }
        let (_, body) = Abs(str_pat, move |_| body).construct(tycker, body_env)?;
        let forall_ty = Alloc::alloc(
            tycker,
            Forall(
                TypeBinder { pattern: tpat, witness: abst },
                tycker.statics.annotations_compu[&body],
            ),
            ctype,
            &(),
        );
        let tabs = Alloc::alloc(tycker, Abs(tpat, body), forall_ty, &());
        Thunk(tabs).construct(tycker, env)?
    } else {
        let (param_tys, tail_ty) = peel_arrows(tycker, body_ty);
        let result_ty =
            tail_ty.destruct_ret_app(tycker).expect("pure returning intrinsic op ends in Ret");
        let (body_env, params) = alloc_intrinsic_params(tycker, env.clone(), param_tys)?;
        let (mut body_env, mut call) = Force(op_val).construct(tycker, body_env)?;
        for (_, val) in &params {
            (body_env, call) = App(call, *val).construct(tycker, body_env)?;
        }
        let (body_env, lifted_result) =
            cs::TypeLift { ty: result_ty }.construct(tycker, body_env)?;
        let res_def =
            Alloc::alloc(tycker, VarName::new("__intrinsic_result"), AnnId::Type(result_ty), &());
        let res_pat = Alloc::alloc(tycker, ValuePattern::Var(res_def), result_ty, &());
        let res_val = Alloc::alloc(tycker, Value::Var(res_def), result_ty, &());
        let (mut body_env, ret_m) = App(
            App(cs::Dtor(Force(body_env.monad_impl), ".return"), cs::Ty(lifted_result)),
            res_val,
        )
        .construct(tycker, body_env)?;
        let monad_res_ty = tycker.statics.annotations_compu[&ret_m];
        let mut body = Alloc::alloc(
            tycker,
            Computation::Do(Bind { binder: res_pat, bindee: call, tail: ret_m }),
            monad_res_ty,
            &(),
        );
        for (vpat, _) in params.into_iter().rev() {
            (body_env, body) = Abs(vpat, move |_| body).construct(tycker, body_env)?;
        }
        Thunk(body).construct(tycker, env)?
    };
    Lub::lub(ty_, tycker.statics.annotations_value[&translated], tycker)?;
    Ok((env, translated))
}

/// Term Translation (Computation) `[C]`
pub(super) fn computation_translation(
    tycker: &mut Tycker, env: MonEnv, compu: CompuId,
) -> Result<(MonEnv, CompuId)> {
    use Computation as Compu;
    let copattern_match = tycker.statics.copattern_matches.contains(compu);
    let (env, ty) = cs::TypeOf(compu).construct(tycker, env)?;

    let (env, res) = match tycker.statics.compus[&compu].to_owned() {
        | Compu::Hole(Hole) => {
            let (env, ty_) = cs::TypeLift { ty }.construct(tycker, env)?;
            cs::Ann(Hole, ty_).construct(tycker, env)?
        }
        | Compu::VAbs(compu) => {
            let Abs(vpat, compu) = compu;
            match ty.destruct_pack_pi(tycker) {
                | Some(signature) => {
                    let translation =
                        PackPiPatternTranslation::new(vpat, &signature, tycker, &env.ty)?;
                    let (env, vpat) = translation.translate(tycker, env)?;
                    let (env, compu) = cs::TermLift { tm: compu }.construct(tycker, env)?;
                    let (env, signature) = cs::TypeLift { ty }.construct(tycker, env)?;
                    let abstraction = PackPiAbstraction { binder: vpat, body: compu, signature }
                        .build(tycker, &env.ty);
                    (env, abstraction)
                }
                | None => Abs(cs::TermLift { tm: vpat }, move |_def| cs::TermLift { tm: compu })
                    .construct(tycker, env)?,
            }
        }
        | Compu::VApp(compu) => {
            let App(fun, arg) = compu;
            let fun_ty = tycker.statics.annotations_compu[&fun];
            match fun_ty.destruct_pack_pi(tycker) {
                | Some(_) => {
                    let (env, fun) = cs::TermLift { tm: fun }.construct(tycker, env)?;
                    let (env, arg) = cs::TermLift { tm: arg }.construct(tycker, env)?;
                    let (env, result) = cs::TypeLift { ty }.construct(tycker, env)?;
                    let application = PackPiApplication { function: fun, argument: arg, result }
                        .build(tycker, &env.ty);
                    (env, application)
                }
                | None => {
                    let fun_ = cs::TermLift { tm: fun };
                    let arg_ = cs::TermLift { tm: arg };
                    App(fun_, arg_).construct(tycker, env)?
                }
            }
        }
        | Compu::TAbs(compu) => {
            let Abs(tpat, compu) = compu;
            let Some((abst, _)) = ty.destruct_forall(tycker) else { unreachable!() };
            Abs(cs::Ty((cs::TypeLift { ty: tpat }, abst)), move |_tpat, abst| {
                Abs(cs::StrPat("str", abst, None), move |_str| cs::TermLift { tm: compu })
            })
            .construct(tycker, env)?
        }
        | Compu::TApp(compu) => {
            let App(fun, arg) = compu;
            let fun_ = cs::TermLift { tm: fun };
            let arg_ = cs::TypeLift { ty: arg };
            let str_ = cs::Structure { ty: arg };
            App(App(fun_, cs::Ty(arg_)), Thunk(str_)).construct(tycker, env)?
        }
        | Compu::Fix(compu) => {
            let Fix(vpat, compu) = compu;
            Fix(cs::TermLift { tm: vpat }, move |_vpat| cs::TermLift { tm: compu })
                .construct(tycker, env)?
        }
        | Compu::Force(compu) => {
            let Force(value) = compu;
            Force(cs::TermLift { tm: value }).construct(tycker, env)?
        }
        | Compu::Ret(compu) => {
            let Return(value) = compu;
            App(
                App(
                    cs::Dtor(Force(env.monad_impl), ".return"),
                    cs::Ty(cs::TypeLift { ty: cs::TypeOf(value) }),
                ),
                cs::TermLift { tm: value },
            )
            .construct(tycker, env)?
        }
        | Compu::Do(compu) => {
            let Bind { binder, bindee, tail } = compu;
            let str_ = cs::Structure { ty };
            let (env, ret_ty) = cs::TypeOf(bindee).construct(tycker, env)?;
            let Some(a_ty) = ret_ty.destruct_ret_app(tycker) else { unreachable!() };
            let a_ty_ = cs::TypeLift { ty: a_ty };
            let bindee_ = cs::TermLift { tm: bindee };
            let kont = Abs(cs::TermLift { tm: binder }, move |_var| cs::TermLift { tm: tail });
            App(App(App(str_, cs::Ty(a_ty_)), Thunk(bindee_)), Thunk(kont))
                .construct(tycker, env)?
        }
        | Compu::Let(compu) => {
            let Let { binder, bindee, tail } = compu;
            let bindee_ = cs::TermLift { tm: bindee };
            let binder_ = cs::TermLift { tm: binder };
            Let { binder: binder_, bindee: bindee_, tail: move |_| cs::TermLift { tm: tail } }
                .construct(tycker, env)?
        }
        | Compu::Match(compu) => {
            let Match { scrut, arms } = compu;
            let (env, scrut_) = cs::TermLift { tm: scrut }.construct(tycker, env)?;
            let arms_ = arms
                .into_iter()
                .map(|Matcher { binder, tail }| {
                    let (env, binder_) =
                        cs::TermLift { tm: binder }.construct(tycker, env.clone())?;
                    let (_env, tail_) = cs::TermLift { tm: tail }.construct(tycker, env)?;
                    Ok(Matcher { binder: binder_, tail: tail_ })
                })
                .collect::<Result<Vec<_>>>()?;
            let (env, ty_) = cs::TypeLift { ty }.construct(tycker, env)?;
            let alloc = Alloc::alloc(tycker, Match { scrut: scrut_, arms: arms_ }, ty_, &());
            (env, alloc)
        }
        | Compu::CoMatch(compu) => {
            let (env, ty_) = cs::TypeLift { ty }.construct(tycker, env)?;
            let CoMatch { arms } = compu;
            let arms: std::collections::HashMap<_, _> =
                arms.into_iter().map(|CoMatcher { dtor, tail }| (dtor, tail)).collect();
            cs::CoMatch(ty_, |dtor, _ty| {
                let tail = arms.get(&dtor).cloned().unwrap();
                cs::TermLift { tm: tail }
            })
            .construct(tycker, env)?
        }
        | Compu::Dtor(compu) => {
            let Dtor(compu, dtor) = compu;
            let compu_ = cs::TermLift { tm: compu };
            cs::Dtor(compu_, dtor).construct(tycker, env)?
        }
    };

    if copattern_match {
        let _ = tycker.statics.copattern_matches.ensure(res);
    }
    if let Some(origin) = tycker.statics.terms.source(&compu.into()) {
        tycker.statics.terms.record(origin, res.into());
    }
    Ok((env, res))
}
