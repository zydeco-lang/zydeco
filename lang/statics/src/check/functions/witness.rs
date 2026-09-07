//! Relate package-opening patterns to the canonical witness telescope of a function.

use super::*;

impl PackageWitnessProjectionBuilder {
    pub(in crate::check) fn build(
        pattern: ss::VPatId, tycker: &Tycker<'_>,
    ) -> ss::PackageWitnessProjection {
        match tycker.statics.vpats[&pattern].to_owned() {
            | ss::ValuePattern::Named(ss::Named(_, inner)) => Self::build(inner, tycker),
            | ss::ValuePattern::Alias(ss::Alias(patterns)) => patterns
                .iter()
                .map(|pattern| Self::build(*pattern, tycker))
                .find(|projection| !matches!(projection, ss::PackageWitnessProjection::Ignore))
                .unwrap_or(ss::PackageWitnessProjection::Ignore),
            | ss::ValuePattern::VCons(patterns) => {
                let projections = patterns
                    .iter()
                    .map(|pattern| Self::build(*pattern, tycker))
                    .collect::<Vec<_>>();
                if projections
                    .iter()
                    .all(|projection| matches!(projection, ss::PackageWitnessProjection::Ignore))
                {
                    ss::PackageWitnessProjection::Ignore
                } else {
                    ss::PackageWitnessProjection::Product(projections)
                }
            }
            | ss::ValuePattern::SCons(_) => tycker
                .statics
                .package_pattern_opened_arity
                .get(&pattern)
                .copied()
                .map(|abstracts| ss::PackageWitnessProjection::Package { abstracts })
                .unwrap_or(ss::PackageWitnessProjection::Ignore),
            | ss::ValuePattern::Hole(_)
            | ss::ValuePattern::Var(_)
            | ss::ValuePattern::Ctor(_)
            | ss::ValuePattern::Lit(_)
            | ss::ValuePattern::Triv(_)
            | ss::ValuePattern::View(_) => ss::PackageWitnessProjection::Ignore,
        }
    }

    pub(in crate::check) fn abstract_arity(projection: &ss::PackageWitnessProjection) -> usize {
        match projection {
            | ss::PackageWitnessProjection::Ignore => 0,
            | ss::PackageWitnessProjection::Package { abstracts } => *abstracts,
            | ss::PackageWitnessProjection::Product(projections) => {
                projections.iter().map(Self::abstract_arity).sum()
            }
        }
    }
}

impl From<ss::PackPi> for PackageSignature {
    fn from(signature: ss::PackPi) -> Self {
        let ss::PackPi { domain, witnesses, codomain } = signature;
        Self { domain, witnesses, codomain }
    }
}

impl PackageSignature {
    /// Open a value function's package witnesses as fresh skolems while
    /// checking the result pattern of a view pattern.
    pub(in crate::check) fn open_codomain_k(
        &self, tycker: &mut Tycker<'_>, env: &ss::TyEnv,
    ) -> ResultKont<(ss::TyEnv, ss::TypeId, Vec<ss::AbstId>)> {
        self.witnesses.iter().try_fold(
            (env.clone(), self.codomain, Vec::new()),
            |(env, codomain, mut opened), canonical| -> ResultKont<_> {
                let kind = tycker.statics.annotations_abst[canonical];
                let name = tycker.statics.abst_hints.get(canonical).copied();
                let fresh = Alloc::alloc(tycker, name, kind, &());
                tycker.transfer_builtin_role_k(*canonical, fresh)?;
                tycker.statics.existential_skolems.ensure(fresh);
                let env = env.with_skolem(fresh);
                let payload = Alloc::alloc(tycker, fresh, kind, &env);
                let codomain = codomain.subst_abst_k(tycker, (*canonical, payload))?;
                opened.push(fresh);
                Ok((env, codomain, opened))
            },
        )
    }
}

impl<'a> PackPiWitnessSkolems<'a> {
    fn new(witnesses: &'a [ss::AbstId], domain: ss::TypeId) -> Self {
        Self { witnesses, domain, expected: witnesses.len() }
    }

    fn collect_k(
        self, tycker: &mut Tycker<'_>, env: &ss::TyEnv,
    ) -> ResultKont<Vec<(ss::AbstId, ss::AbstId)>> {
        if self.witnesses.is_empty() {
            return Ok(Vec::new());
        }
        let view = self.domain.unroll_k(tycker)?.subst_env_k(tycker, env)?;
        match tycker.type_filled_k(&view)?.to_owned() {
            | ss::Type::ManifestKind(ss::ManifestKind { body, .. }) => {
                Self { witnesses: self.witnesses, domain: body, expected: self.expected }
                    .collect_k(tycker, env)
            }
            | ss::Type::Label(ss::Label(_, payload)) => {
                Self { witnesses: self.witnesses, domain: payload, expected: self.expected }
                    .collect_k(tycker, env)
            }
            | ss::Type::Prod(ss::Prod(components)) => {
                let mut assignments = Vec::new();
                let mut witnesses = self.witnesses;
                for component in components {
                    let collected = Self { witnesses, domain: component, expected: self.expected }
                        .collect_k(tycker, env)?;
                    witnesses = &witnesses[collected.len()..];
                    assignments.extend(collected);
                }
                Ok(assignments)
            }
            | ss::Type::Exists(exists) => {
                let ss::Exists { binder, mode, body } = *exists;
                match mode {
                    | ss::ExistsMode::Abstract => {
                        let Some((&canonical, witnesses)) = self.witnesses.split_first() else {
                            unreachable!()
                        };
                        let kind = tycker.statics.annotations_abst[&canonical];
                        let payload = Alloc::alloc(tycker, canonical, kind, env);
                        let domain = body.subst_abst_k(tycker, (binder.witness, payload))?;
                        let tail = Self { witnesses, domain, expected: self.expected }
                            .collect_k(tycker, env)?;
                        Ok(std::iter::once((binder.witness, canonical)).chain(tail).collect())
                    }
                    | ss::ExistsMode::Manifest(definition) => {
                        let domain = body.subst_abst_k(tycker, (binder.witness, definition))?;
                        Self { witnesses: self.witnesses, domain, expected: self.expected }
                            .collect_k(tycker, env)
                    }
                }
            }
            | _ => tycker.err_k(
                TyckError::PackageWitnessArityMismatch {
                    expected: self.expected,
                    found: self.expected - self.witnesses.len(),
                },
                std::panic::Location::caller(),
            ),
        }
    }
}

impl<'a> PackPiPatternAssignments<'a> {
    fn new(items: &'a [su::PatId], witnesses: &'a [ss::AbstId], domain: ss::TypeId) -> Self {
        Self { items, witnesses, domain, expected: witnesses.len(), found: items.len() }
    }

    fn collect_k(
        self, tycker: &mut Tycker<'_>, env: &ss::TyEnv,
    ) -> ResultKont<Vec<(su::PatId, ss::AbstId)>> {
        if self.witnesses.is_empty() {
            return Ok(Vec::new());
        }
        let Some((&item, items)) = self.items.split_first() else {
            return self.mismatch_k(tycker);
        };
        let view = self.domain.unroll_k(tycker)?.subst_env_k(tycker, env)?;
        match tycker.type_filled_k(&view)?.to_owned() {
            | ss::Type::ManifestKind(ss::ManifestKind { body, .. }) => Self {
                items,
                witnesses: self.witnesses,
                domain: body,
                expected: self.expected,
                found: self.found,
            }
            .collect_k(tycker, env),
            | ss::Type::Exists(exists) => {
                let ss::Exists { binder, mode, body } = *exists;
                match mode {
                    | ss::ExistsMode::Abstract => {
                        let Some((&witness, witnesses)) = self.witnesses.split_first() else {
                            unreachable!()
                        };
                        let kind = tycker.statics.annotations_abst[&witness];
                        let payload = Alloc::alloc(tycker, witness, kind, env);
                        let domain = body.subst_abst_k(tycker, (binder.witness, payload))?;
                        let tail = Self {
                            items,
                            witnesses,
                            domain,
                            expected: self.expected,
                            found: self.found,
                        }
                        .collect_k(tycker, env)?;
                        Ok(std::iter::once((item, witness)).chain(tail).collect())
                    }
                    | ss::ExistsMode::Manifest(definition) => {
                        let domain = body.subst_abst_k(tycker, (binder.witness, definition))?;
                        Self {
                            items,
                            witnesses: self.witnesses,
                            domain,
                            expected: self.expected,
                            found: self.found,
                        }
                        .collect_k(tycker, env)
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

impl PackPiPatternSkolems {
    fn assignments_k(
        &self, tycker: &mut Tycker<'_>, env: &ss::TyEnv, pattern: su::PatId,
    ) -> ResultKont<Vec<(su::PatId, ss::AbstId)>> {
        match tycker.scoped.pats[&pattern].to_owned() {
            | su::Pattern::Ann(su::Ann { tm, .. }) => self.assignments_k(tycker, env, tm),
            | su::Pattern::Named(su::Named(_, inner)) => self.assignments_k(tycker, env, inner),
            | su::Pattern::Cons(items) => {
                let witnesses = self.signature.witnesses.iter().copied().collect::<Vec<_>>();
                PackPiPatternAssignments::new(&items, &witnesses, self.signature.domain)
                    .collect_k(tycker, env)
            }
            | su::Pattern::Project(_) => Ok(Vec::new()),
            | su::Pattern::View(_) => Ok(Vec::new()),
            | su::Pattern::Alias(su::Alias(patterns))
                if ExistentialProjectionPattern::members(tycker, patterns.iter().copied())
                    .is_some() =>
            {
                Ok(Vec::new())
            }
            | su::Pattern::Hole(_)
            | su::Pattern::Var(_)
            | su::Pattern::Ctor(_)
            | su::Pattern::Lit(_)
            | su::Pattern::Alias(_)
            | su::Pattern::Triv(_) => tycker.err_k(
                TyckError::PackageWitnessArityMismatch {
                    expected: self.signature.witnesses.len(),
                    found: 0,
                },
                std::panic::Location::caller(),
            ),
        }
    }
}

impl<'a> Tyck<'a> for TyEnvT<PackPiPatternSkolems> {
    type Out = PatternSkolems;
    type Action = ();

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, (): Self::Action) -> ResultKont<Self::Out> {
        let patterns = self.inner.assignments_k(tycker, &self.info, self.inner.pattern)?;
        let canonical = self.inner.signature.witnesses.iter().copied().collect::<Vec<_>>();
        let witnesses = PackPiWitnessSkolems::new(&canonical, self.inner.signature.domain)
            .collect_k(tycker, &self.info)?;
        Ok(PatternSkolems::new(patterns, witnesses))
    }
}

impl ValuePiPatternSkolems {
    fn collect_projection_k(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, pattern: su::PatId, domain: ss::TypeId,
        projection: &ss::PackageWitnessProjection, canonical: &[ss::AbstId], codomain: ss::TypeId,
    ) -> ResultKont<(PatternSkolems, usize)> {
        match tycker.scoped.pats[&pattern].to_owned() {
            | su::Pattern::Ann(su::Ann { tm, .. }) | su::Pattern::Named(su::Named(_, tm)) => {
                return Self::collect_projection_k(
                    tycker, env, tm, domain, projection, canonical, codomain,
                );
            }
            | _ => {}
        }

        match projection {
            | ss::PackageWitnessProjection::Ignore => Ok((PatternSkolems::default(), 0)),
            | ss::PackageWitnessProjection::Package { abstracts } => {
                if *abstracts == 0 || *abstracts > canonical.len() {
                    return tycker.err_k(
                        TyckError::PackageWitnessArityMismatch {
                            expected: canonical.len(),
                            found: *abstracts,
                        },
                        std::panic::Location::caller(),
                    );
                }
                let selected = &canonical[..*abstracts];
                let (&first, rest) = selected.split_first().expect("a package route is nonempty");
                let witnesses = ss::PackTelescope::new(first, rest.iter().copied());
                let signature = PackageSignature { domain, witnesses, codomain };
                let helper = PackPiPatternSkolems { pattern, signature: signature.clone() };
                let patterns = helper.assignments_k(tycker, env, pattern)?;
                let witnesses =
                    PackPiWitnessSkolems::new(selected, domain).collect_k(tycker, env)?;
                Ok((PatternSkolems::new(patterns, witnesses), *abstracts))
            }
            | ss::PackageWitnessProjection::Product(projections) => {
                let su::Pattern::Cons(patterns) = tycker.scoped.pats[&pattern].to_owned() else {
                    return tycker.err_k(
                        TyckError::PackageWitnessArityMismatch {
                            expected: projections.len(),
                            found: 0,
                        },
                        std::panic::Location::caller(),
                    );
                };
                let domain_view = domain.unroll_k(tycker)?.subst_env_k(tycker, env)?;
                let ss::Type::Prod(ss::Prod(domains)) =
                    tycker.type_filled_k(&domain_view)?.to_owned()
                else {
                    return tycker.err_k(
                        TyckError::TypeExpected {
                            expected: "a product carrying package witnesses".to_string(),
                            found: domain_view,
                        },
                        std::panic::Location::caller(),
                    );
                };
                if projections.len() != domains.len() || domains.len() != patterns.len() {
                    return tycker.err_k(
                        TyckError::PackageWitnessArityMismatch {
                            expected: projections.len(),
                            found: patterns.len(),
                        },
                        std::panic::Location::caller(),
                    );
                }
                projections.iter().zip(domains).zip(patterns).try_fold(
                    (PatternSkolems::default(), 0usize),
                    |(skolems, consumed), ((projection, domain), pattern)| {
                        let remaining = canonical.get(consumed..).unwrap_or_default();
                        let (component, component_consumed) = Self::collect_projection_k(
                            tycker, env, pattern, domain, projection, remaining, codomain,
                        )?;
                        Ok((skolems.merge(component), consumed + component_consumed))
                    },
                )
            }
        }
    }
}

impl<'a> Tyck<'a> for TyEnvT<ValuePiPatternSkolems> {
    type Out = PatternSkolems;
    type Action = ();

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, (): Self::Action) -> ResultKont<Self::Out> {
        let canonical = self.inner.signature.witnesses.iter().copied().collect::<Vec<_>>();
        let (skolems, consumed) = ValuePiPatternSkolems::collect_projection_k(
            tycker,
            &self.info,
            self.inner.pattern,
            self.inner.signature.domain,
            &self.inner.projection,
            &canonical,
            self.inner.signature.codomain,
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
        Ok(skolems)
    }
}
