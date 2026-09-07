//! Instantiate visible package witnesses and check dependent introductions and applications.

use super::*;

impl<'a> PackPiInstantiationState<'a> {
    fn new(
        signature: &PackageSignature, canonical: &'a [ss::AbstId], actual: &'a [ss::StaticTermId],
    ) -> Self {
        Self {
            domain: signature.domain,
            codomain: signature.codomain,
            canonical,
            actual,
            expected: canonical.len(),
            found: actual.len(),
        }
    }

    fn instantiate_k(self, tycker: &mut Tycker<'_>, env: &ss::TyEnv) -> ResultKont<ss::TypeId> {
        if self.canonical.is_empty() {
            return Ok(self.codomain);
        }
        let view = self.domain.unroll_k(tycker)?.subst_env_k(tycker, env)?;
        match tycker.type_filled_k(&view)?.to_owned() {
            | ss::Type::ManifestKind(ss::ManifestKind { definition, body, .. }) => {
                let Some((&ss::StaticTermId::Kind(actual_kind), actual)) =
                    self.actual.split_first()
                else {
                    return self.mismatch_k(tycker);
                };
                Lub::lub_k(definition, actual_kind, tycker)?;
                Self {
                    domain: body,
                    codomain: self.codomain,
                    canonical: self.canonical,
                    actual,
                    expected: self.expected,
                    found: self.found,
                }
                .instantiate_k(tycker, env)
            }
            | ss::Type::Exists(exists) => {
                let ss::Exists { binder, mode, body } = *exists;
                let Some((&ss::StaticTermId::Type(witness), actual)) = self.actual.split_first()
                else {
                    return self.mismatch_k(tycker);
                };
                let payload = binder.pattern.bind_argument_k(tycker, witness)?;
                match mode {
                    | ss::ExistsMode::Abstract => {
                        let Some((&canonical, remaining)) = self.canonical.split_first() else {
                            unreachable!()
                        };
                        let canonical_kind = tycker.statics.annotations_abst[&canonical];
                        let payload_kind = tycker.statics.type_kind(payload);
                        Lub::lub_k(canonical_kind, payload_kind, tycker)?;
                        let codomain = self.codomain.subst_abst_k(tycker, (canonical, payload))?;
                        let domain = body.subst_abst_k(tycker, (binder.witness, payload))?;
                        Self {
                            domain,
                            codomain,
                            canonical: remaining,
                            actual,
                            expected: self.expected,
                            found: self.found,
                        }
                        .instantiate_k(tycker, env)
                    }
                    | ss::ExistsMode::Manifest(definition) => {
                        let payload = Lub::lub_k(definition, payload, tycker)?;
                        let domain = body.subst_abst_k(tycker, (binder.witness, payload))?;
                        Self {
                            domain,
                            codomain: self.codomain,
                            canonical: self.canonical,
                            actual,
                            expected: self.expected,
                            found: self.found,
                        }
                        .instantiate_k(tycker, env)
                    }
                }
            }
            | _ => self.mismatch_k(tycker),
        }
    }

    #[track_caller]
    fn mismatch_k<T>(&self, tycker: &mut Tycker<'_>) -> ResultKont<T> {
        tycker.err_k(
            TyckError::PackageWitnessArityMismatch { expected: self.expected, found: self.found },
            std::panic::Location::caller(),
        )
    }
}

impl<'a> Tyck<'a> for TyEnvT<PackPiInstantiation> {
    type Out = ss::TypeId;
    type Action = ();

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, (): Self::Action) -> ResultKont<Self::Out> {
        let PackPiInstantiation { signature, witnesses } = &self.inner;
        let canonical = signature.witnesses.iter().copied().collect::<Vec<_>>();
        PackPiInstantiationState::new(signature, &canonical, witnesses)
            .instantiate_k(tycker, &self.info)
    }
}

impl ValuePiInstantiation {
    fn projected_arguments_k(
        env: &ss::TyEnv, tycker: &mut Tycker<'_>, projection: &ss::PackageWitnessProjection,
        domain: ss::TypeId, argument: ss::ValueId,
    ) -> ResultKont<Vec<ProjectedPackageArgument>> {
        if matches!(projection, ss::PackageWitnessProjection::Ignore) {
            return Ok(Vec::new());
        }
        let shape = argument.static_shape(tycker);
        Self::projected_shape_k(env, tycker, projection, domain, argument, &shape)
    }

    fn projected_shape_k(
        env: &ss::TyEnv, tycker: &mut Tycker<'_>, projection: &ss::PackageWitnessProjection,
        domain: ss::TypeId, argument: ss::ValueId,
        shape: &crate::elaborate::static_values::StaticShape,
    ) -> ResultKont<Vec<ProjectedPackageArgument>> {
        match projection {
            | ss::PackageWitnessProjection::Ignore => Ok(Vec::new()),
            | ss::PackageWitnessProjection::Package { abstracts } => {
                let Some(witnesses) = shape.witnesses() else {
                    tycker.err_k(
                        TyckError::PackageWitnessesUnavailable { package: argument },
                        std::panic::Location::caller(),
                    )?
                };
                Ok(vec![ProjectedPackageArgument { domain, witnesses, abstracts: *abstracts }])
            }
            | ss::PackageWitnessProjection::Product(projections) => {
                let domain_view = domain.unroll_k(tycker)?.subst_env_k(tycker, env)?;
                let ss::Type::Prod(ss::Prod(domains)) =
                    tycker.type_filled_k(&domain_view)?.to_owned()
                else {
                    tycker.err_k(
                        TyckError::TypeExpected {
                            expected: "a product carrying package witnesses".to_string(),
                            found: domain_view,
                        },
                        std::panic::Location::caller(),
                    )?
                };
                let crate::elaborate::static_values::StaticShape::Product(arguments) = shape else {
                    tycker.err_k(
                        TyckError::PackageWitnessesUnavailable { package: argument },
                        std::panic::Location::caller(),
                    )?
                };
                if projections.len() != domains.len() || domains.len() != arguments.len() {
                    tycker.err_k(
                        TyckError::PackageWitnessArityMismatch {
                            expected: projections.len(),
                            found: arguments.len(),
                        },
                        std::panic::Location::caller(),
                    )?
                }
                projections.iter().zip(domains).zip(arguments).try_fold(
                    Vec::new(),
                    |mut projected, ((projection, domain), shape)| {
                        projected.extend(Self::projected_shape_k(
                            env, tycker, projection, domain, argument, shape,
                        )?);
                        Ok(projected)
                    },
                )
            }
        }
    }
}

impl<'a> Tyck<'a> for TyEnvT<ValuePiInstantiation> {
    type Out = ss::TypeId;
    type Action = ();

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, (): Self::Action) -> ResultKont<Self::Out> {
        let ValuePiInstantiation { signature, projection, argument } = &self.inner;
        let canonical = signature.witnesses.iter().copied().collect::<Vec<_>>();
        let projected = ValuePiInstantiation::projected_arguments_k(
            &self.info,
            tycker,
            projection,
            signature.domain,
            *argument,
        )?;
        let (codomain, consumed) = projected.into_iter().try_fold(
            (signature.codomain, 0usize),
            |(codomain, consumed), projected| -> ResultKont<_> {
                let remaining = canonical.get(consumed..).unwrap_or_default();
                let Some((first, tail)) = remaining.split_first() else {
                    return tycker.err_k(
                        TyckError::PackageWitnessArityMismatch {
                            expected: canonical.len(),
                            found: consumed + projected.abstracts,
                        },
                        std::panic::Location::caller(),
                    );
                };
                if projected.abstracts > remaining.len() {
                    return tycker.err_k(
                        TyckError::PackageWitnessArityMismatch {
                            expected: canonical.len(),
                            found: consumed + projected.abstracts,
                        },
                        std::panic::Location::caller(),
                    );
                }
                let selected_tail = tail
                    .iter()
                    .take(projected.abstracts.saturating_sub(1))
                    .copied()
                    .collect::<Vec<_>>();
                let witnesses = ss::PackTelescope::new(*first, selected_tail);
                let segment = PackageSignature { domain: projected.domain, witnesses, codomain };
                let codomain = PackPiInstantiationState::new(
                    &segment,
                    &remaining[..projected.abstracts],
                    &projected.witnesses,
                )
                .instantiate_k(tycker, &self.info)?;
                Ok((codomain, consumed + projected.abstracts))
            },
        )?;
        if consumed != canonical.len() {
            tycker.err_k(
                TyckError::PackageWitnessArityMismatch {
                    expected: canonical.len(),
                    found: consumed,
                },
                std::panic::Location::caller(),
            )?
        }
        Ok(codomain)
    }
}

impl<'a> Tyck<'a> for TyEnvT<PackPiIntroduction> {
    type Out = TermAnnId;
    type Action = ();

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, (): Self::Action) -> ResultKont<Self::Out> {
        let PackPiIntroduction { binder, body, signature } = &self.inner;
        let skolems = self
            .mk(PackPiPatternSkolems { pattern: *binder, signature: signature.clone().into() })
            .tyck_k(tycker, ())?;
        let binder = self
            .mk(*binder)
            .tyck_k(tycker, PatternAction::ana(signature.domain.into()).with_skolems(skolems))?;
        let (pattern, domain) =
            binder.try_as_value(tycker, TyckError::SortMismatch, std::panic::Location::caller())?;
        ValuePatternShape::require_binding_k(tycker, self.inner.binder, pattern)?;
        Lub::lub_k(signature.domain, domain, tycker)?;

        let Some(witnesses) = binder.package_telescope_k(tycker)? else {
            tycker.err_k(
                TyckError::PackageWitnessArityMismatch {
                    expected: signature.witnesses.len(),
                    found: 0,
                },
                std::panic::Location::caller(),
            )?
        };
        let body = TyEnvT::new(binder.info.clone(), *body)
            .tyck_k(tycker, Action::ana(signature.codomain.into()))?;
        let (body, codomain) =
            body.try_as_compu(tycker, TyckError::SortMismatch, std::panic::Location::caller())?;

        let mut iter = witnesses.iter();
        let first = *iter.next().expect("a package telescope opens at least one witness");
        let rest = iter.copied().collect::<Vec<_>>();
        let input = crate::query::InternedPackPiIntro::new(
            tycker.db, pattern, body, domain, first, rest, codomain,
        );
        let Some(outcome) = crate::query::pack_pi_intro_judgment(
            tycker.db,
            tycker.data,
            tycker.query_site(),
            input,
        ) else {
            unreachable!("pack-pi introduction judgments are query-produced")
        };
        tycker.statics.types_pre.insert_new(
            outcome.sig_id,
            ss::Fillable::Done(outcome.sig),
            outcome.kd,
        );
        tycker.store_env(outcome.sig_id, &self.info);
        outcome.sig_id.constrain_to_scope_k(tycker, self.info.skolem_scope())?;
        tycker.statics.compus.insert_new(outcome.abs_id, outcome.abs);
        tycker.statics.annotations_compu.insert_new(outcome.abs_id, outcome.sig_id);
        tycker.statics.env_compu.insert_new(outcome.abs_id, self.info.clone());
        Ok(TermAnnId::Compu(outcome.abs_id, outcome.sig_id))
    }
}

impl<'a> Tyck<'a> for TyEnvT<PackPiElimination> {
    type Out = TermAnnId;
    type Action = Action<AnnId>;

    fn tyck_inner_k(
        &self, tycker: &mut Tycker<'a>, Action { switch, .. }: Self::Action,
    ) -> ResultKont<Self::Out> {
        let PackPiElimination { function, argument, signature } = &self.inner;
        let argument = self.mk(*argument).tyck_k(tycker, Action::ana(signature.domain.into()))?;
        let (argument, _) = argument.try_as_value(
            tycker,
            TyckError::SortMismatch,
            std::panic::Location::caller(),
        )?;
        let Some(witnesses) = argument.package_witnesses(tycker) else {
            tycker.err_k(
                TyckError::PackageWitnessesUnavailable { package: argument },
                std::panic::Location::caller(),
            )?
        };
        let codomain = self
            .mk(PackPiInstantiation { signature: signature.clone().into(), witnesses })
            .tyck_k(tycker, ())?;
        codomain.constrain_to_scope_k(tycker, self.info.skolem_scope())?;
        let codomain = match switch {
            | Switch::Syn => codomain,
            | Switch::Ana(AnnId::Type(expected)) => Lub::lub_k(codomain, expected, tycker)?,
            | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
            }
        };
        let input = crate::query::InternedAppInput::new(
            tycker.db,
            crate::query::AppKind::CompuValue { function: *function, argument },
            codomain,
            codomain,
        );
        let Some(crate::query::AppSynOutcome::Compu { id, compu, ann, reported }) =
            crate::query::app_judgment_at(tycker.db, tycker.data, tycker.query_site(), input)
        else {
            unreachable!("pack-pi elimination judgments are query-produced")
        };
        tycker.statics.compus.insert_new(id, compu);
        tycker.statics.annotations_compu.insert_new(id, ann);
        tycker.statics.env_compu.insert_new(id, self.info.clone());
        Ok(TermAnnId::Compu(id, reported))
    }
}
