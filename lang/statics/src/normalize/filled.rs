//! Normalize filled kinds and types with one cache shared across arena roots.

use super::reduction::TypeApplicationSpine;
use super::*;

/// Pass-wide memoization for filled kind and type normalization.
///
/// A single context is shared by every arena root after inference closes, so
/// overlapping type subgraphs are normalized only once.
#[derive(Default)]
pub(crate) struct FilledNormalizer {
    kinds: HashMap<KindId, KindId>,
    types: HashMap<TypeId, TypeId>,
}

impl FilledNormalizer {
    pub(crate) fn normalize_kind_k(
        &mut self, root: KindId, tycker: &mut Tycker<'_>,
    ) -> ResultKont<()> {
        let result = self.normalize_kind(root, tycker);
        tycker.err_p_to_k(result)
    }

    fn normalize_kind(&mut self, root: KindId, tycker: &mut Tycker<'_>) -> Result<()> {
        let _ = root.filled_norm_id(tycker, self)?;
        Ok(())
    }

    pub(crate) fn normalize_type_k(
        &mut self, root: TypeId, tycker: &mut Tycker<'_>,
    ) -> ResultKont<()> {
        let result = self.normalize_type(root, tycker);
        tycker.err_p_to_k(result)
    }

    fn normalize_type(&mut self, root: TypeId, tycker: &mut Tycker<'_>) -> Result<()> {
        let _ = root.filled_norm_id(tycker, self)?;
        Ok(())
    }
}

impl KindId {
    fn filled_norm_id(
        self, tycker: &mut Tycker<'_>, norm: &mut FilledNormalizer,
    ) -> Result<KindId> {
        if let Some(norm) = norm.kinds.get(&self).cloned() {
            return Ok(norm);
        }
        let res = match tycker.statics.kinds_pre[&self].to_owned() {
            | Fillable::Fill(fill) => match tycker.statics.solus.get(&fill).cloned() {
                | Some(AnnId::Kind(kd)) => kd.filled_norm_id(tycker, norm)?,
                | Some(AnnId::Set | AnnId::Type(_)) => {
                    let _: ResultKont<()> =
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller());
                    self
                }
                | None => {
                    let _: ResultKont<()> = tycker.err_k(
                        TyckError::MissingSolution(vec![fill]),
                        std::panic::Location::caller(),
                    );
                    self
                }
            },
            | Fillable::Done(kind) => match kind {
                | Kind::VType(VType) | Kind::CType(CType) => self,
                | Kind::Arrow(Arrow(from, to)) => {
                    let from_norm = from.filled_norm_id(tycker, norm)?;
                    let to_norm = to.filled_norm_id(tycker, norm)?;
                    if from_norm == from && to_norm == to {
                        self
                    } else {
                        Alloc::alloc(tycker, Arrow(from_norm, to_norm), (), &())
                    }
                }
                | Kind::Label(Label(name, inner)) => {
                    let inner_norm = inner.filled_norm_id(tycker, norm)?;
                    if inner_norm == inner {
                        self
                    } else {
                        let target: KindId = Alloc::alloc(tycker, Label(name, inner_norm), (), &());
                        tycker.statics.member_provenance.transfer(self.into(), target.into());
                        target
                    }
                }
            },
        };
        norm.kinds.insert(self, res);
        if self != res {
            norm.kinds.insert(res, res);
        }
        // Unchanged nodes already carry their normal form in `kinds_pre`.
        // Keep only the old-ID-to-new-form delta for fills and rebuilt paths.
        if self != res
            && let Fillable::Done(kind) = tycker.statics.kinds_pre[&res].to_owned()
        {
            let _ = tycker.statics.kinds_normalized.upsert(self, kind);
        }
        Ok(res)
    }
}

impl TypeId {
    fn filled_norm_id(
        self, tycker: &mut Tycker<'_>, norm: &mut FilledNormalizer,
    ) -> Result<TypeId> {
        if let Some(normalized) = norm.types.get(&self).cloned() {
            return Ok(normalized);
        }
        let kd = tycker.statics.type_kind(self);
        let kd_norm = kd.filled_norm_id(tycker, norm)?;
        let env = tycker.statics.env_at(self);
        let res = match tycker.statics.types_pre[&self].to_owned() {
            | Fillable::Fill(fill) => match tycker.statics.solus.get(&fill).cloned() {
                | Some(AnnId::Type(ty)) => ty.filled_norm_id(tycker, norm)?,
                | Some(AnnId::Set | AnnId::Kind(_)) => {
                    let _: ResultKont<()> =
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller());
                    self
                }
                | None => {
                    let _: ResultKont<()> = tycker.err_k(
                        TyckError::MissingSolution(vec![fill]),
                        std::panic::Location::caller(),
                    );
                    self
                }
            },
            | Fillable::Done(ty) => match ty {
                | Type::Var(def) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, def, kd_norm, &env)
                    }
                }
                | Type::Abst(abst) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, abst, kd_norm, &env)
                    }
                }
                | Type::Abs(abs) => {
                    let TypeAbstraction { binder, body } = abs;
                    let body_norm = body.filled_norm_id(tycker, norm)?;
                    if body_norm == body && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(
                            tycker,
                            TypeAbstraction { binder, body: body_norm },
                            kd_norm,
                            &env,
                        )
                    }
                }
                | Type::App(app) => {
                    let App(f_ty, a_ty) = app;
                    let f_norm = f_ty.filled_norm_id(tycker, norm)?;
                    let a_norm = a_ty.filled_norm_id(tycker, norm)?;
                    // Arrow-kinded applications are the compact normal form used between
                    // successive arguments. Structural consumers force them through `normalize`.
                    if matches!(tycker.kind_filled(&kd_norm)?, Kind::Arrow(_)) {
                        if f_norm == f_ty && a_norm == a_ty && kd_norm == kd {
                            self
                        } else {
                            Alloc::alloc(tycker, App(f_norm, a_norm), kd_norm, &env)
                        }
                    } else {
                        let spine =
                            TypeApplicationSpine::with_application(tycker, f_norm, a_norm, kd_norm);
                        let spine = if f_norm == f_ty && a_norm == a_ty && kd_norm == kd {
                            spine.with_original(self)
                        } else {
                            spine
                        };
                        let materialized = spine.materialize(tycker)?;
                        if materialized == self {
                            self
                        } else {
                            materialized.filled_norm_id(tycker, norm)?
                        }
                    }
                }
                | Type::Named(named) => {
                    let Named(name, inner) = named;
                    let inner_norm = inner.filled_norm_id(tycker, norm)?;
                    if inner_norm == inner && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, Named(name, inner_norm), kd_norm, &env)
                    }
                }
                | Type::Label(label) => {
                    let Label(name, inner) = label;
                    let inner_norm = inner.filled_norm_id(tycker, norm)?;
                    if inner_norm == inner && kd_norm == kd {
                        self
                    } else {
                        let target = Alloc::alloc(tycker, Label(name, inner_norm), kd_norm, &env);
                        tycker
                            .statics
                            .builtin_roles
                            .transfer_value(self, target)
                            .expect("a fresh normalized label cannot have a conflicting role");
                        tycker.statics.member_provenance.transfer(self.into(), target.into());
                        target
                    }
                }
                | Type::Proj(proj) => {
                    let Proj(head, name) = proj;
                    let head_norm = head.filled_norm_id(tycker, norm)?;
                    match tycker.statics.types_pre[&head_norm].to_owned() {
                        | Fillable::Done(Type::Named(Named(found, inner))) if found == name => {
                            inner.filled_norm_id(tycker, norm)?
                        }
                        | _ if head_norm == head && kd_norm == kd => self,
                        | _ => Alloc::alloc(tycker, Proj(head_norm, name), kd_norm, &env),
                    }
                }
                | Type::Thk(ThkTy) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, ThkTy, kd_norm, &env)
                    }
                }
                | Type::Ret(RetTy) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, RetTy, kd_norm, &env)
                    }
                }
                | Type::Unit(UnitTy) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, UnitTy, kd_norm, &env)
                    }
                }
                | Type::Opaque(OpaqueTy) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, OpaqueTy, kd_norm, &env)
                    }
                }
                | Type::Primitive(primitive) => primitive.build(tycker, &env),
                | Type::OS(OSTy) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, OSTy, kd_norm, &env)
                    }
                }
                | Type::ValPi(pi) => {
                    let ValPi { binder, codomain } = *pi;
                    let (binder, domain_changed) = match binder {
                        | ValPiBinder::Type(binder) => (ValPiBinder::Type(binder), false),
                        | ValPiBinder::Value(parameter) => {
                            let domain = parameter.domain.filled_norm_id(tycker, norm)?;
                            let changed = domain != parameter.domain;
                            (
                                ValPiBinder::Value(ValueParameter {
                                    domain,
                                    witnesses: parameter.witnesses,
                                    witness_projection: parameter.witness_projection,
                                }),
                                changed,
                            )
                        }
                    };
                    let codomain_norm = codomain.filled_norm_id(tycker, norm)?;
                    if !domain_changed && codomain_norm == codomain && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(
                            tycker,
                            ValPi { binder, codomain: codomain_norm },
                            kd_norm,
                            &env,
                        )
                    }
                }
                | Type::Arrow(arr) => {
                    let Arrow(ty1, ty2) = arr;
                    let ty1_norm = ty1.filled_norm_id(tycker, norm)?;
                    let ty2_norm = ty2.filled_norm_id(tycker, norm)?;
                    if ty1_norm == ty1 && ty2_norm == ty2 && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, Arrow(ty1_norm, ty2_norm), kd_norm, &env)
                    }
                }
                | Type::Forall(forall) => {
                    let Forall(abst, body) = forall;
                    let body_norm = body.filled_norm_id(tycker, norm)?;
                    if body_norm == body && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, Forall(abst, body_norm), kd_norm, &env)
                    }
                }
                | Type::PackPi(pack_pi) => {
                    let PackPi { domain, witnesses, codomain } = *pack_pi;
                    let domain_norm = domain.filled_norm_id(tycker, norm)?;
                    let codomain_norm = codomain.filled_norm_id(tycker, norm)?;
                    if domain_norm == domain && codomain_norm == codomain && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(
                            tycker,
                            PackPi { domain: domain_norm, witnesses, codomain: codomain_norm },
                            kd_norm,
                            &env,
                        )
                    }
                }
                | Type::Prod(prod) => {
                    let Prod(components) = prod;
                    let components_norm = components
                        .iter()
                        .map(|ty| ty.filled_norm_id(tycker, norm))
                        .collect::<Result<Vec<_>>>()?;
                    if components_norm == *components && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, Prod(components_norm), kd_norm, &env)
                    }
                }
                | Type::Exists(exists) => {
                    let Exists { binder, mode, body } = *exists;
                    let (mode, definition_changed) = match mode {
                        | ExistsMode::Abstract => (ExistsMode::Abstract, false),
                        | ExistsMode::Manifest(definition) => {
                            let definition_norm = definition.filled_norm_id(tycker, norm)?;
                            (ExistsMode::Manifest(definition_norm), definition_norm != definition)
                        }
                    };
                    let body_norm = body.filled_norm_id(tycker, norm)?;
                    if !definition_changed && body_norm == body && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(
                            tycker,
                            Exists { binder, mode, body: body_norm },
                            kd_norm,
                            &env,
                        )
                    }
                }
                | Type::ManifestKind(manifest) => {
                    let ManifestKind { binder, definition, body } = manifest;
                    let definition_norm = definition.filled_norm_id(tycker, norm)?;
                    let body_norm = body.filled_norm_id(tycker, norm)?;
                    if definition_norm == definition && body_norm == body && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(
                            tycker,
                            ManifestKind { binder, definition: definition_norm, body: body_norm },
                            kd_norm,
                            &env,
                        )
                    }
                }
                | Type::Data(data) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, data, kd_norm, &env)
                    }
                }
                | Type::CoData(codata) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, codata, kd_norm, &env)
                    }
                }
            },
        };
        norm.types.insert(self, res);
        if self != res {
            norm.types.insert(res, res);
        }
        // Store the normalized form only when it differs from the pre form:
        // readers fall back to `types_pre` for unchanged nodes, so the arena
        // keeps a delta instead of duplicating every type. Solved fill nodes
        // still store their solution's type, as the pre form is a fill marker.
        if self != res
            && let Fillable::Done(ty) = tycker.statics.types_pre[&res].to_owned()
        {
            let _ = tycker.statics.types_normalized.upsert(self, ty);
        }
        Ok(res)
    }
}
