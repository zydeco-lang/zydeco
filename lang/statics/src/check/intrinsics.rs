//! Intrinsic materialization and registration of Builtin and foreign metadata.

use super::*;
use crate::check::judgment::{Switch, TyckTask};

/// Validate and record the static attachment point of one typed Builtin role.
pub(super) struct BuiltinAttachment {
    pub(super) role: ss::BuiltinRole,
    pub(super) term: TermAnnId,
}

/// One foreign target awaiting interpretation of its normalized classifier.
pub(super) struct PendingForeignImport {
    pub(super) value: ss::ValueId,
    pub(super) classifier: ss::TypeId,
    pub(super) target: ss::ForeignTarget,
    pub(super) blame: &'static std::panic::Location<'static>,
    pub(super) stack: rpds::VectorSync<TyckTask>,
}

/// Validate and record the static attachment point of one foreign implementation.
pub(super) struct ForeignAttachment {
    pub(super) target: ss::ForeignTarget,
    pub(super) term: TermAnnId,
    pub(super) implementation: su::TermId,
}

/// Resolve one compiler-generated host type against the abstract Builtin
/// identities visible at its lexical use site.
struct BuiltinTypeResolution(ss::BuiltinTypeRole);

impl BuiltinTypeResolution {
    #[track_caller]
    fn resolve_k(self, tycker: &mut Tycker<'_>, env: &ss::TyEnv) -> ResultKont<ss::TypeId> {
        let mut witnesses = tycker
            .statics
            .builtin_roles
            .type_witnesses(self.0)
            .filter(|witness| env.skolem_scope().contains(witness))
            .collect::<Vec<_>>();
        witnesses.sort_unstable();

        match witnesses.as_slice() {
            | [] => tycker.err_k(
                TyckError::MissingBuiltinTypeRole { role: self.0 },
                std::panic::Location::caller(),
            ),
            | [witness] => {
                let kind = tycker.statics.annotations_abst[witness];
                Ok(Alloc::alloc(tycker, *witness, kind, env))
            }
            | _ => tycker.err_k(
                TyckError::AmbiguousBuiltinTypeRole { role: self.0, witnesses },
                std::panic::Location::caller(),
            ),
        }
    }
}

/// Give compiler-generated primitive syntax its intrinsic or lexical static
/// meaning without routing it through a source-level definition name.
pub(crate) struct InternalTerm(pub(super) su::Internal, pub(super) su::TermId);

/// Intrinsic identities installed before a checker can construct typed nodes.
pub(crate) struct IntrinsicStatics {
    pub(crate) vtype: KindId,
    pub(crate) ctype: KindId,
    pub(crate) thk: TypeId,
    pub(crate) ret: TypeId,
    pub(crate) unit: TypeId,
    pub(crate) primitives: std::collections::BTreeMap<ss::PrimitiveType, TypeId>,
}

impl IntrinsicStatics {
    pub(super) fn new<'db>(
        db: &'db dyn crate::query::TyckDb, data: crate::query::ScopedData<'db>,
        statics: &mut StaticsArena,
    ) -> Self {
        use crate::query::IntrinsicKey;
        let mut materializer = IntrinsicMaterializer { db, data, statics };
        Self {
            vtype: materializer.kind(IntrinsicKey::VType),
            ctype: materializer.kind(IntrinsicKey::CType),
            thk: materializer.ty(IntrinsicKey::Thk),
            ret: materializer.ty(IntrinsicKey::Ret),
            unit: materializer.ty(IntrinsicKey::Unit),
            primitives: ss::PrimitiveType::all()
                .map(|primitive| (primitive, materializer.ty(IntrinsicKey::Primitive(primitive))))
                .collect(),
        }
    }
}

struct IntrinsicMaterializer<'arena, 'db> {
    db: &'db dyn crate::query::TyckDb,
    data: crate::query::ScopedData<'db>,
    statics: &'arena mut StaticsArena,
}

impl IntrinsicMaterializer<'_, '_> {
    fn singleton(&self, key: crate::query::IntrinsicKey) -> crate::query::IntrinsicSingleton {
        let key = crate::query::InternedIntrinsic::new(self.db, key);
        crate::query::intrinsic_singleton(self.db, self.data, key)
    }

    fn kind(&mut self, key: crate::query::IntrinsicKey) -> KindId {
        let crate::query::IntrinsicSingleton::Kind { id, kind } = self.singleton(key) else {
            unreachable!("an intrinsic kind key produces a kind singleton")
        };
        self.statics.kinds_pre.insert_new(id, ss::Fillable::Done(kind));
        id
    }

    fn ty(&mut self, key: crate::query::IntrinsicKey) -> TypeId {
        let crate::query::IntrinsicSingleton::Type { kinds, ty: (ty, node), ann } =
            self.singleton(key)
        else {
            unreachable!("an intrinsic type key produces a type singleton")
        };
        for (id, kind) in kinds {
            self.statics.kinds_pre.insert_new(id, ss::Fillable::Done(kind));
        }
        self.statics.types_pre.insert_new(ty, ss::Fillable::Done(node), ann);
        let env = self.statics.intern_env(&TyEnv::default());
        self.statics.env_type.insert_new(ty, env);
        ty
    }
}

impl InternalTerm {
    #[track_caller]
    pub(super) fn tyck_k(
        self, tycker: &mut Tycker<'_>, env: &ss::TyEnv, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        let synthesized = match self.0 {
            | su::Internal::VType => TermAnnId::Kind(ss::VType.build(tycker, env)),
            | su::Internal::CType => TermAnnId::Kind(ss::CType.build(tycker, env)),
            | su::Internal::Thk => {
                let ty = ss::ThkTy.build(tycker, env);
                TermAnnId::Type(ty, tycker.statics.type_kind(ty))
            }
            | su::Internal::Ret => {
                let ty = ss::RetTy.build(tycker, env);
                TermAnnId::Type(ty, tycker.statics.type_kind(ty))
            }
            | su::Internal::Unit => {
                let ty = ss::UnitTy.build(tycker, env);
                TermAnnId::Type(ty, tycker.statics.type_kind(ty))
            }
            | su::Internal::Primitive(primitive) => {
                let ty = ss::PrimitiveTy(primitive).build(tycker, env);
                TermAnnId::Type(ty, tycker.statics.type_kind(ty))
            }
            | su::Internal::ValueInt(operation) => {
                let domain = ss::PrimitiveTy(ss::PrimitiveType::Integer(ss::IntegerType::Int))
                    .build(tycker, env);
                let kind = ss::VType.build(tycker, env);
                let variables = ["left", "right"].map(|name| {
                    let definition =
                        Alloc::alloc(tycker, ss::VarName(name.into()), AnnId::Type(domain), &());
                    let binder =
                        Alloc::alloc(tycker, ss::ValuePattern::Var(definition), domain, env);
                    let value = Alloc::alloc(tycker, ss::Value::Var(definition), domain, env);
                    (binder, value)
                });
                let mut body = Alloc::alloc(
                    tycker,
                    ss::Value::IntOp(ss::IntValueOp {
                        operation,
                        operands: variables.map(|(_, value)| value),
                    }),
                    domain,
                    env,
                );
                tycker.statics.terms.record(self.1, ss::TermId::Value(body));
                let mut codomain = domain;
                for (binder, _) in variables.into_iter().rev() {
                    let classifier = ss::ValPi {
                        binder: ss::ValPiBinder::Value(ss::ValueParameter {
                            domain,
                            witnesses: None,
                            witness_projection: ss::PackageWitnessProjection::Ignore,
                        }),
                        codomain,
                    };
                    codomain = Alloc::alloc(tycker, classifier, kind, env);
                    body = Alloc::alloc(
                        tycker,
                        ss::Abs(ss::ValBinder::Value(binder), body),
                        codomain,
                        env,
                    );
                }
                TermAnnId::Value(body, codomain)
            }
            | su::Internal::OS => self.builtin_type_k(tycker, env, ss::BuiltinTypeRole::OS)?,
            | su::Internal::Monad | su::Internal::Algebra => {
                let term = crate::query::InternedTerm::new(tycker.db, self.1);
                let env_data = crate::query::EnvData::new(tycker.db, env.clone());
                if let Some(error) =
                    crate::query::internal_judgment(tycker.db, tycker.data, term, env_data)
                {
                    tycker.err_k(error, std::panic::Location::caller())?
                } else {
                    unreachable!("intrinsic rejections are query-produced")
                }
            }
        };
        self.reconcile_k(tycker, synthesized, switch)
    }

    #[track_caller]
    fn builtin_type_k(
        &self, tycker: &mut Tycker<'_>, env: &ss::TyEnv, role: ss::BuiltinTypeRole,
    ) -> ResultKont<TermAnnId> {
        let ty = BuiltinTypeResolution(role).resolve_k(tycker, env)?;
        Ok(TermAnnId::Type(ty, tycker.statics.type_kind(ty)))
    }

    #[track_caller]
    pub(super) fn reconcile_k(
        &self, tycker: &mut Tycker<'_>, synthesized: TermAnnId, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        let annotation = match synthesized {
            | TermAnnId::Kind(_) => AnnId::Set,
            | TermAnnId::Type(_, kind) => AnnId::Kind(kind),
            | TermAnnId::Value(_, ty) => AnnId::Type(ty),
            | TermAnnId::Hole(_) | TermAnnId::Compu(_, _) => {
                unreachable!("internal terms synthesize kinds, types, and total value functions")
            }
        };
        let annotation = match switch {
            | Switch::Syn => annotation,
            | Switch::Ana(expected) => Lub::lub_k(expected, annotation, tycker)?,
        };

        match (synthesized, annotation) {
            | (TermAnnId::Kind(kind), AnnId::Set) => Ok(TermAnnId::Kind(kind)),
            | (TermAnnId::Type(ty, _), AnnId::Kind(kind)) => Ok(TermAnnId::Type(ty, kind)),
            | (TermAnnId::Value(value, _), AnnId::Type(ty)) => Ok(TermAnnId::Value(value, ty)),
            | _ => unreachable!("annotation reconciliation preserves the internal term sort"),
        }
    }
}

impl BuiltinAttachment {
    pub(super) fn new(role: ss::BuiltinRole, term: TermAnnId) -> Self {
        Self { role, term }
    }

    #[track_caller]
    pub(super) fn register_k(self, tycker: &mut Tycker<'_>, env: &ss::TyEnv) -> ResultKont<()> {
        match self.role {
            | ss::BuiltinRole::Type(role) => {
                let expected = match role.universe() {
                    | ss::BuiltinTypeUniverse::Value => "an abstract existential value-type entry",
                    | ss::BuiltinTypeUniverse::Computation => {
                        "an abstract existential computation-type entry"
                    }
                };
                let (witness, kind) = self.existential_witness_k(tycker, expected)?;
                let expected_kind = match role.universe() {
                    | ss::BuiltinTypeUniverse::Value => ss::VType.build(tycker, env),
                    | ss::BuiltinTypeUniverse::Computation => ss::CType.build(tycker, env),
                };
                Lub::lub_k(expected_kind, kind, tycker)?;
                tycker.statics.builtin_roles.attach_type(witness, role).map_err(|existing| {
                    tycker.errors.push(TyckErrorEntry {
                        error: TyckError::ConflictingBuiltinRole { existing, found: self.role },
                        blame: std::panic::Location::caller(),
                        stack: tycker.tasks.clone(),
                    });
                    KontFailure
                })
            }
            | ss::BuiltinRole::Value(role) => {
                let TermAnnId::Type(entry, _) = self.term else {
                    return tycker.err_k(
                        TyckError::InvalidBuiltinAttachment {
                            role: self.role,
                            expected: "a named value classifier",
                        },
                        std::panic::Location::caller(),
                    );
                };
                let ss::Type::Label(_) = tycker.type_filled_k(&entry)?.to_owned() else {
                    return tycker.err_k(
                        TyckError::InvalidBuiltinAttachment {
                            role: self.role,
                            expected: "a named value classifier",
                        },
                        std::panic::Location::caller(),
                    );
                };
                let result =
                    tycker.statics.builtin_roles.attach_value(entry, role).map_err(|existing| {
                        tycker.errors.push(TyckErrorEntry {
                            error: TyckError::ConflictingBuiltinRole {
                                existing: ss::BuiltinRole::Value(existing),
                                found: self.role,
                            },
                            blame: std::panic::Location::caller(),
                            stack: tycker.tasks.clone(),
                        });
                        KontFailure
                    });
                if result.is_ok() {
                    tycker.invalidate_field_materializations();
                }
                result
            }
        }
    }

    #[track_caller]
    fn existential_witness_k(
        &self, tycker: &mut Tycker<'_>, expected: &'static str,
    ) -> ResultKont<(ss::AbstId, ss::KindId)> {
        let TermAnnId::Type(entry, _) = self.term else {
            return tycker.err_k(
                TyckError::InvalidBuiltinAttachment { role: self.role, expected },
                std::panic::Location::caller(),
            );
        };
        let ss::Type::Exists(exists) = tycker.type_filled_k(&entry)?.to_owned() else {
            return tycker.err_k(
                TyckError::InvalidBuiltinAttachment { role: self.role, expected },
                std::panic::Location::caller(),
            );
        };
        if !matches!(exists.mode, ss::ExistsMode::Abstract) {
            return tycker.err_k(
                TyckError::InvalidBuiltinAttachment { role: self.role, expected },
                std::panic::Location::caller(),
            );
        }
        Ok((exists.binder.witness, exists.binder.payload_kind(tycker)))
    }
}

impl ForeignAttachment {
    pub(super) fn new(
        target: ss::ForeignTarget, term: TermAnnId, implementation: su::TermId,
    ) -> Self {
        Self { target, term, implementation }
    }

    #[track_caller]
    pub(super) fn register_k(self, tycker: &mut Tycker<'_>) -> ResultKont<()> {
        let TermAnnId::Value(value, classifier) = self.term else {
            return tycker
                .err_k(TyckError::InvalidForeignAttachment, std::panic::Location::caller());
        };
        tycker.pending_foreign_imports.push(PendingForeignImport {
            value,
            classifier,
            target: self.target,
            blame: std::panic::Location::caller(),
            stack: tycker.tasks.clone(),
        });
        tycker
            .statics
            .fills
            .iter()
            .filter_map(|(fill, site)| {
                (*site == ss::InferenceSite::Term(self.implementation)).then_some(*fill)
            })
            .collect::<Vec<_>>()
            .into_iter()
            .for_each(|fill| {
                tycker.statics.fill_hints.remove(&fill);
            });
        Ok(())
    }
}

impl Tycker<'_> {
    /// Peel named wrappers and inference fills to a primitive type, if the
    /// type resolves to one.
    pub(super) fn primitive_type_of(&self, ty: ss::TypeId) -> Option<ss::PrimitiveType> {
        match self.statics.types_pre.get(&ty)?.to_owned() {
            | ss::Fillable::Fill(fill) => match self.statics.solus.get(&fill) {
                | Some(ss::AnnId::Type(solution)) => self.primitive_type_of(*solution),
                | _ => None,
            },
            | ss::Fillable::Done(ss::Type::Primitive(ss::PrimitiveTy(primitive))) => {
                Some(primitive)
            }
            | ss::Fillable::Done(ss::Type::Named(ss::Named(_, inner))) => {
                self.primitive_type_of(inner)
            }
            | ss::Fillable::Done(_) => None,
        }
    }

    #[track_caller]
    pub(crate) fn transfer_builtin_role(
        &mut self, source: ss::AbstId, target: ss::AbstId,
    ) -> Result<()> {
        let Some(found) = self.statics.builtin_roles.witness(source) else {
            return Ok(());
        };
        match self.statics.builtin_roles.transfer_witness(source, target) {
            | Ok(()) => Ok(()),
            | Err(existing) => self.err(
                TyckError::ConflictingBuiltinRole { existing, found },
                std::panic::Location::caller(),
            ),
        }
    }

    #[track_caller]
    pub(super) fn transfer_builtin_role_k(
        &mut self, source: ss::AbstId, target: ss::AbstId,
    ) -> ResultKont<()> {
        let result = self.transfer_builtin_role(source, target);
        self.err_p_to_k(result)
    }

    #[track_caller]
    pub(super) fn validate_builtin_signature_k(
        &mut self, signature: &ss::PackPi,
    ) -> ResultKont<()> {
        match BuiltinSignatureValidator::new(&self.statics).validate(signature) {
            | Ok(()) => Ok(()),
            | Err(error) => self
                .err_k(TyckError::InvalidBuiltinSignature(error), std::panic::Location::caller()),
        }
    }

    pub(super) fn validate_foreign_imports(&mut self) {
        let pending = std::mem::take(&mut self.pending_foreign_imports);
        for PendingForeignImport { value, classifier, target, blame, stack } in pending {
            let import = match target.abi {
                | ss::ForeignAbi::C => ForeignClassifier::new(&self.statics)
                    .validate(target, classifier)
                    .map(ss::CheckedImport::C),
                | ss::ForeignAbi::Zydeco => crate::UnitClassifier { statics: &self.statics }
                    .import(target, classifier)
                    .map(ss::CheckedImport::Zydeco)
                    .map_err(ForeignClassifierError::from),
            };
            match import {
                | Ok(import) => {
                    if let Some(existing) =
                        self.statics.foreign_imports.insert_or_get(value, import.clone())
                        && existing != import
                    {
                        self.errors.push(TyckErrorEntry {
                            error: TyckError::ConflictingForeignImport {
                                existing: existing.target().clone(),
                                found: import.target().clone(),
                            },
                            blame,
                            stack,
                        });
                    }
                }
                | Err(error) => self.errors.push(TyckErrorEntry {
                    error: TyckError::InvalidForeignClassifier(error),
                    blame,
                    stack,
                }),
            }
        }
    }
}
