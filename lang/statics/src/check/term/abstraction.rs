//! Value and computation abstractions with type-directed binder checking.

use super::*;

impl TermChecker<'_> {
    pub(super) fn check_val_abs_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Abs<su::PatId, su::TermId>,
        switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::Abs(source_binder, source_body) = term;
            match switch {
                | Switch::Syn => {
                    let binder = self.mk(source_binder).tyck_k(tycker, PatternAction::syn())?;
                    let vtype = ss::VType.build(tycker, &self.info);
                    let (binder_node, body, classifier) = match binder.inner.annotation {
                        | PatAnnId::Kind(_) => tycker.err_k(
                            TyckError::Expressivity(
                                "kind parameters are not supported by value functions",
                            ),
                            std::panic::Location::caller(),
                        )?,
                        | PatAnnId::Type(pattern, _) => {
                            let witness = Alloc::alloc(tycker, pattern, (), &());
                            let payload_kind = tycker.statics.annotations_abst[&witness];
                            let payload = Alloc::alloc(tycker, witness, payload_kind, &self.info);
                            let argument = pattern.introduce_payload(tycker, payload);
                            let argument = tycker.err_p_to_k(argument)?;
                            let body_env =
                                TyEnvT::new(binder.info.clone(), Assign(pattern, argument))
                                    .tyck_k(tycker, ())?
                                    .info;
                            let body =
                                TyEnvT::new(body_env, source_body).tyck_k(tycker, Action::syn())?;
                            let (body, codomain) = body.try_as_value(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            let classifier = ss::ValPi {
                                binder: ss::ValPiBinder::Type(ss::TypeBinder { pattern, witness }),
                                codomain,
                            };
                            (ss::ValBinder::Type(pattern), body, classifier)
                        }
                        | PatAnnId::Value(pattern, domain) => {
                            if !ValuePatternShape::is_irrefutable(tycker, pattern) {
                                tycker.err_k(
                                    TyckError::Expressivity(
                                        "value-function parameters must be irrefutable patterns",
                                    ),
                                    std::panic::Location::caller(),
                                )?
                            }
                            let domain_kind = tycker.statics.type_kind(domain);
                            Lub::lub_k(vtype, domain_kind, tycker)?;
                            let body = TyEnvT::new(binder.info.clone(), source_body)
                                .tyck_k(tycker, Action::syn())?;
                            let (body, codomain) = body.try_as_value(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            let witnesses = binder.package_telescope_k(tycker)?;
                            let witness_projection =
                                PackageWitnessProjectionBuilder::build(pattern, tycker);
                            let projected_arity = PackageWitnessProjectionBuilder::abstract_arity(
                                &witness_projection,
                            );
                            let witness_arity =
                                witnesses.as_ref().map_or(0, ss::PackTelescope::len);
                            if projected_arity != witness_arity {
                                tycker.err_k(
                                    TyckError::PackageWitnessArityMismatch {
                                        expected: witness_arity,
                                        found: projected_arity,
                                    },
                                    std::panic::Location::caller(),
                                )?
                            }
                            if witnesses.is_none() {
                                binder.close_scope_k(tycker, codomain)?;
                            }
                            if let Some(witnesses) = &witnesses {
                                tycker.validate_builtin_signature_k(&ss::PackPi {
                                    domain,
                                    witnesses: witnesses.clone(),
                                    codomain,
                                })?;
                            }
                            let classifier = ss::ValPi {
                                binder: ss::ValPiBinder::Value(ss::ValueParameter {
                                    domain,
                                    witnesses,
                                    witness_projection,
                                }),
                                codomain,
                            };
                            (ss::ValBinder::Value(pattern), body, classifier)
                        }
                    };
                    let classifier = Alloc::alloc(tycker, classifier, vtype, &self.info);
                    let value =
                        Alloc::alloc(tycker, ss::Abs(binder_node, body), classifier, &self.info);
                    TermAnnId::Value(value, classifier)
                }
                | Switch::Ana(AnnId::Type(expected)) => {
                    let vtype = ss::VType.build(tycker, &self.info);
                    let expected_kind = tycker.statics.type_kind(expected);
                    Lub::lub_k(vtype, expected_kind, tycker)?;
                    let view = expected.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                    let ss::Type::ValPi(pi) = tycker.type_filled_k(&view)?.to_owned() else {
                        tycker.err_k(
                            TyckError::TypeExpected {
                                expected: "a value-function classifier".to_string(),
                                found: view,
                            },
                            std::panic::Location::caller(),
                        )?
                    };
                    let ss::ValPi { binder: expected_binder, codomain } = *pi;
                    let (binder, body) = match expected_binder {
                        | ss::ValPiBinder::Type(expected_binder) => {
                            let domain_kind = expected_binder.domain_kind(tycker);
                            let binder = self
                                .mk(source_binder)
                                .tyck_k(tycker, PatternAction::ana(domain_kind.into()))?;
                            let (pattern, _) = binder.try_as_type(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            let payload_kind = expected_binder.payload_kind(tycker);
                            let payload = Alloc::alloc(
                                tycker,
                                expected_binder.witness,
                                payload_kind,
                                &self.info,
                            );
                            let argument =
                                expected_binder.pattern.introduce_payload(tycker, payload);
                            let argument = tycker.err_p_to_k(argument)?;
                            let body_env = TyEnvT::new(binder.info, Assign(pattern, argument))
                                .tyck_k(tycker, ())?
                                .info;
                            let body = TyEnvT::new(body_env, source_body).tyck_k(
                                tycker,
                                Action::ana_prepared(codomain.into(), &self.info),
                            )?;
                            let (body, _) = body.try_as_value(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            (ss::ValBinder::Type(pattern), body)
                        }
                        | ss::ValPiBinder::Value(parameter) => {
                            let skolems = match &parameter.witnesses {
                                | Some(witnesses) => self
                                    .mk(ValuePiPatternSkolems {
                                        pattern: source_binder,
                                        signature: PackageSignature {
                                            domain: parameter.domain,
                                            witnesses: witnesses.clone(),
                                            codomain,
                                        },
                                        projection: parameter.witness_projection.clone(),
                                    })
                                    .tyck_k(tycker, ())?,
                                | None => PatternSkolems::default(),
                            };
                            let binder = self.mk(source_binder).tyck_k(
                                tycker,
                                PatternAction::ana(parameter.domain.into()).with_skolems(skolems),
                            )?;
                            let (pattern, domain) = binder.try_as_value(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            Lub::lub_k(parameter.domain, domain, tycker)?;
                            if !ValuePatternShape::is_irrefutable(tycker, pattern) {
                                tycker.err_k(
                                    TyckError::Expressivity(
                                        "value-function parameters must be irrefutable patterns",
                                    ),
                                    std::panic::Location::caller(),
                                )?
                            }
                            let actual_witnesses = binder.package_telescope_k(tycker)?;
                            let actual_projection =
                                PackageWitnessProjectionBuilder::build(pattern, tycker);
                            match (&parameter.witnesses, &actual_witnesses) {
                                | (None, None) => binder.close_scope_k(tycker, codomain)?,
                                | (Some(expected), Some(actual))
                                    if expected.len() == actual.len() => {}
                                | (expected, actual) => tycker.err_k(
                                    TyckError::PackageWitnessArityMismatch {
                                        expected: expected.as_ref().map_or(0, |w| w.len()),
                                        found: actual.as_ref().map_or(0, |w| w.len()),
                                    },
                                    std::panic::Location::caller(),
                                )?,
                            }
                            if parameter.witness_projection != actual_projection {
                                tycker.err_k(
                                TyckError::Expressivity(
                                    "value-function package witness projection does not match its classifier",
                                ),
                                std::panic::Location::caller(),
                            )?
                            }
                            let body = TyEnvT::new(binder.info, source_body).tyck_k(
                                tycker,
                                Action::ana_prepared(codomain.into(), &self.info),
                            )?;
                            let (body, _) = body.try_as_value(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            (ss::ValBinder::Value(pattern), body)
                        }
                    };
                    let value = Alloc::alloc(tycker, ss::Abs(binder, body), expected, &self.info);
                    TermAnnId::Value(value, expected)
                }
                | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                }
            }
        })
    }

    pub(super) fn check_abs_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Abs<su::PatId, su::TermId>,
        switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::Abs(pat, body) = term;
            match switch {
                | Switch::Syn => {
                    let pat_out_ann = self.mk(pat).tyck_k(tycker, PatternAction::syn())?;
                    match pat_out_ann.annotation {
                        | PatAnnId::Kind(_) => {
                            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                            let input = crate::query::InternedAbsSyn::new(
                                tycker.db,
                                crate::query::AbsSynArm::Expressivity,
                            );
                            let Some(crate::query::AbsSynOutcome::Error(error)) =
                                crate::query::abs_syn_judgment(
                                    tycker.db,
                                    tycker.data,
                                    term,
                                    input,
                                    tycker.site_occurrence(),
                                )
                            else {
                                unreachable!(
                                    "the kind arm of abstraction judgments is query-produced"
                                )
                            };
                            tycker.err_k(error, std::panic::Location::caller())?
                        }
                        | PatAnnId::Type(tpat, kd) => {
                            // could be either type-polymorphic function or type function
                            let abst = Alloc::alloc(tycker, tpat, (), &());
                            let subst_vec = {
                                let mut subst_vec = Vec::new();
                                if let (Some(def), kd) = tpat.try_destruct_def(tycker) {
                                    let ty_abst = Alloc::alloc(tycker, abst, kd, &self.info);
                                    subst_vec.push((def, ty_abst.into()));
                                }
                                subst_vec
                            };
                            let body_out_ann =
                                self.mk_add(subst_vec, body).tyck_k(tycker, Action::syn())?;
                            match body_out_ann {
                            | TermAnnId::Type(ty, body_kd) => {
                                // a type function
                                let term =
                                    crate::query::InternedTerm::new(tycker.db, self.inner);
                                let input = crate::query::InternedAbsSyn::new(
                                    tycker.db,
                                    crate::query::AbsSynArm::TypeFunction {
                                        tpat,
                                        witness: abst,
                                        kd,
                                        body_kd,
                                        body: ty,
                                    },
                                );
                                let Some(crate::query::AbsSynOutcome::TypeFunction {
                                    arrow_id,
                                    arrow,
                                    abs_id,
                                    abs,
                                }) = crate::query::abs_syn_judgment(
                                    tycker.db,
                                    tycker.data,
                                    term,
                                    input,
                                    tycker.site_occurrence(),
                                )
                                else {
                                    unreachable!(
                                        "the type function arm of abstraction judgments is query-produced"
                                    )
                                };
                                tycker
                                    .statics
                                    .kinds_pre
                                    .insert_new(arrow_id, ss::Fillable::Done(arrow));
                                tycker.statics.types_pre.insert_new(
                                    abs_id,
                                    ss::Fillable::Done(abs),
                                    arrow_id,
                                );
                                tycker.store_env(abs_id, &self.info);
                                TermAnnId::Type(abs_id, arrow_id)
                            }
                            | TermAnnId::Compu(compu, body_ty) => {
                                // a type-polymorphic function
                                let term =
                                    crate::query::InternedTerm::new(tycker.db, self.inner);
                                let input = crate::query::InternedAbsSyn::new(
                                    tycker.db,
                                    crate::query::AbsSynArm::PolymorphicCompu {
                                        tpat,
                                        abst,
                                        compu,
                                        body_ty,
                                    },
                                );
                                let Some(crate::query::AbsSynOutcome::TAbsCompu {
                                    ann_id,
                                    ann,
                                    kd,
                                    abs_id,
                                    abs,
                                }) = crate::query::abs_syn_judgment(
                                    tycker.db,
                                    tycker.data,
                                    term,
                                    input,
                                    tycker.site_occurrence(),
                                )
                                else {
                                    unreachable!(
                                        "the polymorphic computation arm of abstraction judgments is query-produced"
                                    )
                                };
                                tycker.statics.types_pre.insert_new(
                                    ann_id,
                                    ss::Fillable::Done(ann),
                                    kd,
                                );
                                tycker.store_env(ann_id, &self.info);
                                tycker.statics.compus.insert_new(abs_id, abs);
                                tycker.statics.annotations_compu.insert_new(abs_id, ann_id);
                                tycker
                                    .statics
                                    .env_compu
                                    .insert_new(abs_id, self.info.clone());
                                TermAnnId::Compu(abs_id, ann_id)
                            }
                            | TermAnnId::Value(_, _) => tycker.err_k(
                                TyckError::Expressivity(
                                    "use `val` or block-form `param val` to introduce a pure value function",
                                ),
                                std::panic::Location::caller(),
                            )?,
                            | TermAnnId::Hole(_) | TermAnnId::Kind(_) => {
                                let term =
                                    crate::query::InternedTerm::new(tycker.db, self.inner);
                                let input = crate::query::InternedAbsSyn::new(
                                    tycker.db,
                                    crate::query::AbsSynArm::SortMismatch,
                                );
                                let Some(crate::query::AbsSynOutcome::Error(error)) =
                                    crate::query::abs_syn_judgment(
                                        tycker.db,
                                        tycker.data,
                                        term,
                                        input,
                                        tycker.site_occurrence(),
                                    )
                                else {
                                    unreachable!(
                                        "the sort arm of abstraction judgments is query-produced"
                                    )
                                };
                                tycker.err_k(error, std::panic::Location::caller())?
                            }
                        }
                        }
                        | PatAnnId::Value(vpat, ty) => {
                            // A value-pattern abstraction is pure when its body is a value
                            // and computational when its body is a computation.
                            let body_out_ann = TyEnvT::new(pat_out_ann.info.clone(), body)
                                .tyck_k(tycker, Action::syn())?;
                            match body_out_ann {
                            | TermAnnId::Value(_, _) => tycker.err_k(
                                TyckError::Expressivity(
                                    "use `val` or block-form `param val` to introduce a pure value function",
                                ),
                                std::panic::Location::caller(),
                            )?,
                            | TermAnnId::Compu(compu, body_ty) => {
                                let arm = match pat_out_ann.package_telescope_k(tycker)? {
                                    | None => {
                                        pat_out_ann.close_scope_k(tycker, body_ty)?;
                                        crate::query::AbsSynArm::CompuArrow {
                                            vpat,
                                            ty,
                                            compu,
                                            body_ty,
                                        }
                                    }
                                    | Some(witnesses) => {
                                        let pack_pi = ss::PackPi {
                                            domain: ty,
                                            witnesses,
                                            codomain: body_ty,
                                        };
                                        tycker.validate_builtin_signature_k(&pack_pi)?;
                                        let mut iter = pack_pi.witnesses.iter();
                                        let first = *iter.next().expect("a package telescope opens at least one witness");
                                        let rest = iter.copied().collect::<Vec<_>>();
                                        crate::query::AbsSynArm::CompuPackPi {
                                            vpat,
                                            domain: ty,
                                            first,
                                            rest,
                                            codomain: body_ty,
                                            compu,
                                        }
                                    }
                                };
                                let term =
                                    crate::query::InternedTerm::new(tycker.db, self.inner);
                                let input =
                                    crate::query::InternedAbsSyn::new(tycker.db, arm);
                                let Some(crate::query::AbsSynOutcome::VAbsCompu {
                                    ann_id,
                                    ann,
                                    kd,
                                    abs_id,
                                    abs,
                                }) = crate::query::abs_syn_judgment(
                                    tycker.db,
                                    tycker.data,
                                    term,
                                    input,
                                    tycker.site_occurrence(),
                                )
                                else {
                                    unreachable!(
                                        "the computation arrow arm of abstraction judgments is query-produced"
                                    )
                                };
                                let is_pack_pi = matches!(&ann, ss::Type::PackPi(_));
                                tycker.statics.types_pre.insert_new(
                                    ann_id,
                                    ss::Fillable::Done(ann),
                                    kd,
                                );
                                tycker.store_env(ann_id, &self.info);
                                if is_pack_pi {
                                    ann_id.constrain_to_scope_k(
                                        tycker,
                                        self.info.skolem_scope(),
                                    )?;
                                }
                                tycker.statics.compus.insert_new(abs_id, abs);
                                tycker.statics.annotations_compu.insert_new(abs_id, ann_id);
                                tycker
                                    .statics
                                    .env_compu
                                    .insert_new(abs_id, self.info.clone());
                                TermAnnId::Compu(abs_id, ann_id)
                            }
                            | TermAnnId::Hole(_)
                            | TermAnnId::Kind(_)
                            | TermAnnId::Type(_, _) => {
                                let term =
                                    crate::query::InternedTerm::new(tycker.db, self.inner);
                                let input = crate::query::InternedAbsSyn::new(
                                    tycker.db,
                                    crate::query::AbsSynArm::SortMismatch,
                                );
                                let Some(crate::query::AbsSynOutcome::Error(error)) =
                                    crate::query::abs_syn_judgment(
                                        tycker.db,
                                        tycker.data,
                                        term,
                                        input,
                                        tycker.site_occurrence(),
                                    )
                                else {
                                    unreachable!(
                                        "the sort arm of abstraction judgments is query-produced"
                                    )
                                };
                                tycker.err_k(error, std::panic::Location::caller())?
                            }
                        }
                        }
                    }
                }
                | Switch::Ana(ana) => {
                    match ana {
                        | AnnId::Set => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                        | AnnId::Kind(kd) => {
                            // type function in f omega
                            // expecting a kind arrow
                            let ss::Kind::Arrow(kd_arr) = tycker.kind_filled_k(&kd)?.to_owned()
                            else {
                                tycker.err_k(
                                    TyckError::KindMismatch,
                                    std::panic::Location::caller(),
                                )?
                            };
                            let ss::Arrow(kd_1, kd_2) = kd_arr;
                            let binder =
                                self.mk(pat).tyck_k(tycker, PatternAction::ana(kd_1.into()))?;
                            let (binder, binder_kd) = binder.try_as_type(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            let witness: ss::AbstId = Alloc::alloc(tycker, binder, (), &());
                            let body_env = match binder.try_destruct_def(tycker).0 {
                                | Some(def) => {
                                    let payload_kind = tycker.statics.annotations_abst[&witness];
                                    let witness_ty =
                                        Alloc::alloc(tycker, witness, payload_kind, &self.info);
                                    self.info.clone() + [(def, witness_ty.into())]
                                }
                                | None => self.info.clone(),
                            };
                            let body_out_ann = TyEnvT::new(body_env, body)
                                .tyck_k(tycker, Action::ana(kd_2.into()))?;
                            let (body_out, body_kd) = body_out_ann.try_as_type(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            let ann = Alloc::alloc(tycker, ss::Arrow(binder_kd, body_kd), (), &());
                            let abs = Alloc::alloc(
                                tycker,
                                ss::TypeAbstraction {
                                    binder: ss::TypeBinder { pattern: binder, witness },
                                    body: body_out,
                                },
                                ann,
                                &self.info,
                            );
                            TermAnnId::Type(abs, ann)
                        }
                        | AnnId::Type(ty) => {
                            // could be either a term function or a type-polymorphic term function
                            let kind = tycker.statics.type_kind(ty);
                            let expected = match tycker.kind_filled_k(&kind)?.to_owned() {
                            | ss::Kind::VType(_) => tycker.err_k(
                                TyckError::Expressivity(
                                    "use `val` or block-form `param val` to introduce a pure value function",
                                ),
                                std::panic::Location::caller(),
                            )?,
                            | ss::Kind::CType(_) => tycker.type_filled_k(&ty)?.to_owned(),
                            | ss::Kind::Arrow(_) | ss::Kind::Label(_) => tycker.err_k(
                                TyckError::KindMismatch,
                                std::panic::Location::caller(),
                            )?,
                        };
                            match expected {
                                | ss::Type::Arrow(ty) => {
                                    // a term-term function
                                    let ss::Arrow(ty_1, ty_2) = ty;
                                    let binder_elaboration = self
                                        .mk(pat)
                                        .tyck_k(tycker, PatternAction::ana(ty_1.into()))?;
                                    let (binder, binder_ty) = binder_elaboration.try_as_value(
                                        tycker,
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?;
                                    let body_out_ann =
                                        TyEnvT::new(binder_elaboration.info.clone(), body).tyck_k(
                                            tycker,
                                            Action::ana_prepared(ty_2.into(), &self.info),
                                        )?;
                                    let (body_out, body_ty) = body_out_ann.try_as_compu(
                                        tycker,
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?;
                                    binder_elaboration.close_scope_k(tycker, body_ty)?;
                                    let ctype = ss::CType.build(tycker, &self.info);
                                    let ann = Alloc::alloc(
                                        tycker,
                                        ss::Arrow(binder_ty, body_ty),
                                        ctype,
                                        &self.info,
                                    );
                                    let abs = Alloc::alloc(
                                        tycker,
                                        ss::Abs(binder, body_out),
                                        ann,
                                        &self.info,
                                    );
                                    TermAnnId::Compu(abs, ann)
                                }
                                | ss::Type::PackPi(signature) => self
                                    .mk(PackPiIntroduction {
                                        binder: pat,
                                        body,
                                        signature: *signature,
                                    })
                                    .tyck_k(tycker, ())?,
                                | ss::Type::Forall(ty) => {
                                    let ss::Forall(source_binder, ty_body) = ty;
                                    let domain_kind = source_binder.domain_kind(tycker);
                                    let binder = self
                                        .mk(pat)
                                        .tyck_k(tycker, PatternAction::ana(domain_kind.into()))?;
                                    let (binder, _binder_kd) = binder.try_as_type(
                                        tycker,
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?;
                                    let payload_kind = source_binder.payload_kind(tycker);
                                    let abst_ty = Alloc::alloc(
                                        tycker,
                                        source_binder.witness,
                                        payload_kind,
                                        &self.info,
                                    );
                                    let full_argument =
                                        source_binder.pattern.introduce_payload(tycker, abst_ty);
                                    let full_argument = tycker.err_p_to_k(full_argument)?;
                                    let env = self
                                        .mk(Assign(binder, full_argument))
                                        .tyck_k(tycker, ())?
                                        .info;
                                    let body_out_ann = TyEnvT { info: env, inner: body }.tyck_k(
                                        tycker,
                                        Action::ana_prepared(ty_body.into(), &self.info),
                                    )?;
                                    // throwing _body_ty away because it has been substituted
                                    // Todo: reuse _body_ty by substituting abst back
                                    let (body_out, body_ty) = body_out_ann.try_as_compu(
                                        tycker,
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?;
                                    let ctype = ss::CType.build(tycker, &self.info);
                                    let ann = Alloc::alloc(
                                        tycker,
                                        ss::Forall(source_binder, body_ty),
                                        ctype,
                                        &self.info,
                                    );
                                    let abs = Alloc::alloc(
                                        tycker,
                                        ss::Abs(binder, body_out),
                                        ann,
                                        &self.info,
                                    );
                                    TermAnnId::Compu(abs, ann)
                                }
                                | _ => tycker.err_k(
                                    TyckError::TypeExpected {
                                        expected: "one of `_ -> _`, a package-dependent \
                                               arrow, or `forall _ . _`"
                                            .to_string(),
                                        found: ty,
                                    },
                                    std::panic::Location::caller(),
                                )?,
                            }
                        }
                    }
                }
            }
        })
    }
}
