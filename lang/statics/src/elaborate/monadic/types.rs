//! Kind-pattern, type-pattern, and carrier-type translation.

use super::*;

/// Type Pattern Translation `[TPat]`
pub(super) fn type_pattern_translation(
    tycker: &mut Tycker, env: MonEnv, tpat: TPatId,
) -> Result<(MonEnv, TPatId)> {
    use TypePattern as TPat;
    let (env, kd) = cs::TypeOf(tpat).construct(tycker, env)?;
    let (env, tpat_) = match tycker.statics.tpats[&tpat].to_owned() {
        | TPat::Hole(hole) => cs::Pat(hole, kd).construct(tycker, env)?,
        | TPat::Var(def) => cs::Pat(def, kd).construct(tycker, env)?,
        | TPat::Named(Named(name, inner)) => {
            let (env, inner) = type_pattern_translation(tycker, env, inner)?;
            let pattern = Alloc::alloc(tycker, Named(name, inner), kd, &());
            (env, pattern)
        }
    };
    Ok((env, tpat_))
}

pub(super) fn kind_pattern_translation(
    tycker: &mut Tycker, env: MonEnv, pattern: KPatId, definition: KindId,
) -> Result<(MonEnv, KPatId)> {
    match tycker.statics.kpats[pattern].to_owned() {
        | KindPattern::Hole(Hole) => {
            let pattern = Alloc::alloc(tycker, Hole, (), &());
            Ok((env, pattern))
        }
        | KindPattern::Var(source) => {
            let variable = *tycker.def_name(&source);
            let target = Alloc::alloc(tycker, variable, AnnId::Set, &());
            let mut env = env;
            env.subst += [(source, target)];
            env.ty += [(target, AnnId::Kind(definition))];
            tycker.kind_bindings.insert(target, definition);
            let pattern = Alloc::alloc(tycker, target, (), &());
            Ok((env, pattern))
        }
    }
}

/// Carrier (Type) Translation `[T]`
pub(super) fn type_translation(
    tycker: &mut Tycker, env: MonEnv, ty: TypeId,
) -> Result<(MonEnv, TypeId)> {
    let summary = tycker.statics.types_pre.summary(ty);
    let closed = (env.subst.is_empty() || !summary.contains(TypeSummary::HAS_FREE_VAR))
        && (env.subst_abst.is_empty() || !summary.contains(TypeSummary::HAS_FREE_ABST));
    if !closed || tycker.monadic_memo.is_none() {
        return type_translation_inner(tycker, env, ty);
    }
    let key = (env.monad_ty, env.basis.algebra, ty);
    if let Some(cached) =
        tycker.monadic_memo.as_ref().and_then(|memo| memo.type_lifts.get(&key).copied())
    {
        return Ok((env, cached));
    }
    let caller_env = env.clone();
    let (_, res) = type_translation_inner(tycker, env, ty)?;
    if let Some(memo) = &mut tycker.monadic_memo {
        memo.type_lifts.insert(key, res);
    }
    Ok((caller_env, res))
}

fn type_translation_inner(
    tycker: &mut Tycker, env: MonEnv, ty: TypeId,
) -> Result<(MonEnv, TypeId)> {
    let monad_ty = env.monad_ty;
    let (env, kd) = cs::TypeOf(ty).construct(tycker, env)?;
    let (env, res) = match tycker.type_filled(&ty)?.to_owned() {
        | Type::Var(def) => {
            // substitute according to the environment
            let Some(def_) = env.subst.get(&def).cloned() else {
                tycker.err(TyckError::NotInlinable(def))?
            };
            let alloc = Alloc::alloc(tycker, def_, kd, &());
            (env, alloc)
        }
        | Type::Abst(abst) => match tycker.statics.seals.get(abst) {
            | Some(_) => nominal::NominalTranslation::apply(tycker, env, abst)?,
            | None => match env.subst_abst.get(&abst).cloned() {
                | Some(new) => {
                    let alloc = Alloc::alloc(tycker, new, kd, &());
                    (env, alloc)
                }
                | None => {
                    let alloc = Alloc::alloc(tycker, abst, kd, &());
                    (env, alloc)
                }
            },
        },
        // | Type::Abst(_abst) => unreachable!(),
        | Type::Abs(ty) => {
            let TypeAbstraction { binder, body } = ty;
            let (env, pattern) = type_pattern_translation(tycker, env, binder.pattern)?;
            let witness = env.subst_abst.get(&binder.witness).copied().unwrap_or(binder.witness);
            let (env, body) = cs::TypeLift { ty: body }.construct(tycker, env)?;
            let abstraction = TypeAbstraction { binder: TypeBinder { pattern, witness }, body };
            let abstraction = Alloc::alloc(tycker, abstraction, kd, &());
            (env, abstraction)
        }
        | Type::App(ty) => {
            let App(ty_f, ty_a) = ty;
            let ty_f_ = cs::TypeLift { ty: ty_f };
            let ty_a_ = cs::TypeLift { ty: ty_a };
            App(ty_f_, ty_a_).construct(tycker, env)?
        }
        | Type::Named(ty) => {
            let Named(name, inner) = ty;
            let (env, inner) = cs::TypeLift { ty: inner }.construct(tycker, env)?;
            let named = Alloc::alloc(tycker, Named(name, inner), kd, &());
            (env, named)
        }
        | Type::Label(ty) => {
            let Label(name, inner) = ty;
            let (env, inner) = cs::TypeLift { ty: inner }.construct(tycker, env)?;
            let label = Alloc::alloc(tycker, Label(name, inner), kd, &());
            (env, label)
        }
        | Type::Proj(ty) => {
            let Proj(head, name) = ty;
            let (env, head) = cs::TypeLift { ty: head }.construct(tycker, env)?;
            let projected = head.project_named(tycker, &name, kd)?;
            (env, projected)
        }
        | Type::Thk(ThkTy) => {
            let alloc = Alloc::alloc(tycker, ThkTy, kd, &());
            (env, alloc)
        }
        // primitive types are not allowed in monadic blocks
        | Type::Opaque(_) => tycker.err(TyckError::Expressivity(
            "opaque checker-test types are unavailable in monadic blocks",
        ))?,
        | Type::Primitive(primitive) => PrimitiveTy(primitive.0).construct(tycker, env)?,
        | Type::Data(data) => {
            cs::Data(data, |_ctor, ty| cs::TypeLift { ty }).construct(tycker, env)?
        }
        | Type::Unit(UnitTy) => UnitTy.construct(tycker, env)?,
        | Type::Prod(ty) => {
            let Prod(components) = ty;
            let components =
                components.into_iter().map(|ty| cs::TypeLift { ty }).collect::<Vec<_>>();
            Prod(components).construct(tycker, env)?
        }
        | Type::Exists(ty) => {
            let Exists { binder, mode, body } = *ty;
            let (env, pattern) = type_pattern_translation(tycker, env, binder.pattern)?;
            let witness = env.subst_abst.get(&binder.witness).copied().unwrap_or(binder.witness);
            let witness_ty = Alloc::alloc(tycker, witness, binder.payload_kind(tycker), &());
            let (env, mode, structure_ty) = match mode {
                | ExistsMode::Abstract => (env, ExistsMode::Abstract, witness_ty),
                | ExistsMode::Manifest(definition) => {
                    let (env, definition) =
                        cs::TypeLift { ty: definition }.construct(tycker, env)?;
                    (env, ExistsMode::Manifest(definition), definition)
                }
            };
            let (env, structure) =
                cs::Thk(cs::Signature { ty: structure_ty }).construct(tycker, env)?;
            let (env, body) = cs::TypeLift { ty: body }.construct(tycker, env)?;
            let (env, body) = Prod(vec![structure, body]).construct(tycker, env)?;
            let (env, vtype) = VType.construct(tycker, env)?;
            let binder = TypeBinder { pattern, witness };
            let exists = Alloc::alloc(tycker, Exists { binder, mode, body }, vtype, &());
            (env, exists)
        }
        | Type::ManifestKind(manifest) => {
            let ManifestKind { binder, definition, body } = manifest;
            let (env, body) = cs::TypeLift { ty: body }.construct(tycker, env)?;
            let manifest = Alloc::alloc(tycker, ManifestKind { binder, definition, body }, kd, &());
            (env, manifest)
        }
        // os type is also not allowed in monadic blocks
        | Type::OS(_) => unreachable!(),
        // the return type is translated to the provided monad type
        | Type::Ret(RetTy) => (env, monad_ty),
        | Type::CoData(coda) => {
            cs::CoData(coda, |_dtor, ty| cs::TypeLift { ty }).construct(tycker, env)?
        }
        | Type::ValPi(pi) => ValueFunctionTranslation { signature: *pi }.classifier(tycker, env)?,
        | Type::Arrow(ty) => {
            let Arrow(ty_1, ty_2) = ty;
            let ty_1_ = cs::TypeLift { ty: ty_1 };
            let ty_2_ = cs::TypeLift { ty: ty_2 };
            Arrow(ty_1_, ty_2_).construct(tycker, env)?
        }
        | Type::Forall(ty) => {
            let Forall(binder, ty) = ty;
            let (env, pattern) = type_pattern_translation(tycker, env, binder.pattern)?;
            let witness = env.subst_abst.get(&binder.witness).copied().unwrap_or(binder.witness);
            let (env, structure) = cs::Thk(cs::Signature { ty: witness }).construct(tycker, env)?;
            let (env, body) = cs::TypeLift { ty }.construct(tycker, env)?;
            let (env, body) = Arrow(structure, body).construct(tycker, env)?;
            let (env, ctype) = CType.construct(tycker, env)?;
            let binder = TypeBinder { pattern, witness };
            let forall = Alloc::alloc(tycker, Forall(binder, body), ctype, &());
            (env, forall)
        }
        | Type::PackPi(pack_pi) => {
            let PackPi { domain, witnesses, codomain } = *pack_pi;
            let (env, domain) = cs::TypeLift { ty: domain }.construct(tycker, env)?;
            let witnesses =
                witnesses.map(|witness| env.subst_abst.get(&witness).copied().unwrap_or(witness));
            let (env, codomain) = cs::TypeLift { ty: codomain }.construct(tycker, env)?;
            let alloc = Alloc::alloc(tycker, PackPi { domain, witnesses, codomain }, kd, &());
            (env, alloc)
        }
    };
    Ok((env, res))
}
