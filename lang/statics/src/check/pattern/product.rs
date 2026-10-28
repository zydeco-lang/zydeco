//! Product and package patterns, including ordered witness opening.

use super::*;

/// The environment and opened witnesses threaded left to right through the
/// components of a cons pattern.
struct ConsScope {
    env: TyEnv,
    opened: Vec<AbstId>,
}

impl ConsScope {
    /// Check one value component under the scope, then adopt the environment
    /// and witnesses it produces.
    fn check_value_k(
        &mut self, tycker: &mut Tycker<'_>, item: su::PatId, action: PatternAction,
    ) -> ResultKont<(ss::VPatId, ss::TypeId)> {
        let checked = TyEnvT::new(self.env.clone(), item).tyck_k(tycker, action)?;
        let component = checked.try_as_value(tycker, TyckError::SortMismatch)?;
        self.env = checked.info;
        self.opened.extend(checked.inner.opened);
        Ok(component)
    }

    /// Check the leading components against their component types, then the
    /// tail against the product of the remaining component types.
    fn check_value_components_k(
        &mut self, tycker: &mut Tycker<'_>, items: &[su::PatId], tail: su::PatId,
        component_tys: &[ss::TypeId], skolems: &PatternSkolems,
    ) -> ResultKont<(Vec<(ss::VPatId, ss::TypeId)>, (ss::VPatId, ss::TypeId))> {
        let components = items
            .iter()
            .copied()
            .zip(component_tys.iter().copied())
            .map(|(item, item_ty)| {
                let action = PatternAction::ana(item_ty.into()).with_skolems(skolems.clone());
                self.check_value_k(tycker, item, action)
            })
            .collect::<ResultKont<Vec<_>>>()?;
        let remaining = tycker.rest_product_k(&component_tys[items.len()..], &self.env)?;
        let action = PatternAction::ana(remaining.into()).with_skolems(skolems.clone());
        let tail = self.check_value_k(tycker, tail, action)?;
        Ok((components, tail))
    }

    /// Check the value components of a package pattern against its body type.
    ///
    /// A lone tail component takes the whole body; several components split
    /// the body as a product.
    fn check_package_body_k(
        &mut self, tycker: &mut Tycker<'_>, body_items: &[su::PatId], tail: su::PatId,
        body_ty: ss::TypeId, skolems: &PatternSkolems,
    ) -> ResultKont<ss::VPatId> {
        if body_items.is_empty() {
            let action = PatternAction::ana(body_ty.into()).with_skolems(skolems.clone());
            let (body, _) = self.check_value_k(tycker, tail, action)?;
            return Ok(body);
        }
        let ss::Prod(body_component_tys) = body_ty.view_product_k(tycker, &self.env)?;
        let (components, (tail, ann)) =
            self.check_value_components_k(tycker, body_items, tail, &body_component_tys, skolems)?;
        let vtype = ss::VType.build(tycker, &self.env);
        let (mut components, mut component_tys): (Vec<_>, Vec<_>) = components.into_iter().unzip();
        component_tys.push(ann);
        let ann = Alloc::alloc(tycker, ss::Prod(component_tys), vtype, &self.env);
        components.push(tail);
        Ok(Alloc::alloc(tycker, ss::ValuePattern::VCons(components), ann, &self.env))
    }
}

/// The static components of a package pattern opened so far.
struct PackageOpening {
    scope: ConsScope,
    static_patterns: Vec<ss::StaticPatId>,
    boundary_opened: usize,
}

impl PackageOpening {
    /// Bind a manifest kind component to its definition.
    fn open_manifest_kind_k(
        &mut self, tycker: &mut Tycker<'_>, item: su::PatId, definition: ss::KindId,
        skolems: &PatternSkolems,
    ) -> ResultKont<()> {
        let checked = TyEnvT::new(self.scope.env.clone(), item)
            .tyck_k(tycker, PatternAction::ana(AnnId::Set).with_skolems(skolems.clone()))?;
        let pattern = checked.annotation.try_as_kind(tycker, TyckError::SortMismatch)?;
        self.scope.env = TyEnvT::new(self.scope.env.clone(), Assign(pattern, definition))
            .tyck_k(tycker, ())?
            .info;
        self.static_patterns.push(pattern.into());
        Ok(())
    }

    /// Open an existential component, returning the telescope that remains.
    fn open_exists_k(
        &mut self, tycker: &mut Tycker<'_>, item: su::PatId, source_binder: ss::TypeBinder,
        mode: DeferredTelescopeExistsMode, next_ty: DeferredTelescopeType,
        skolems: &PatternSkolems,
    ) -> ResultKont<DeferredTelescopeType> {
        let domain_kind = source_binder.domain_kind(tycker);
        let payload_kind = source_binder.payload_kind(tycker);
        let checked = TyEnvT::new(self.scope.env.clone(), item)
            .tyck_k(tycker, PatternAction::ana(domain_kind.into()).with_skolems(skolems.clone()))?;
        let (witness, _) = checked.try_as_type(tycker, TyckError::SortMismatch)?;
        self.scope.env = checked.info;
        self.scope.opened.extend(checked.inner.opened);
        let next_ty = match mode {
            | DeferredTelescopeExistsMode::Abstract => {
                let canonical = skolems.get(&item);
                let witness = OpenedWitness { pattern: witness, source_binder, payload_kind };
                self.open_abstract_witness_k(tycker, witness, canonical, next_ty)?
            }
            | DeferredTelescopeExistsMode::Manifest(definition) => {
                let witness = OpenedWitness { pattern: witness, source_binder, payload_kind };
                self.open_manifest_witness_k(tycker, witness, definition, next_ty)?
            }
        };
        self.static_patterns.push(witness.into());
        Ok(next_ty)
    }

    /// Open an abstract witness under a skolem.
    ///
    /// Ordinary elimination is fresh. Checking a PackPi introduction instead
    /// reuses the signature's canonical identity for this pattern component.
    fn open_abstract_witness_k(
        &mut self, tycker: &mut Tycker<'_>, witness: OpenedWitness, canonical: Option<AbstId>,
        next_ty: DeferredTelescopeType,
    ) -> ResultKont<DeferredTelescopeType> {
        let OpenedWitness { pattern, source_binder, payload_kind } = witness;
        let skolem = match canonical {
            | Some(skolem) => {
                let expected = tycker.statics.annotations_abst[&skolem];
                Lub::lub_k(expected, payload_kind, tycker)?;
                skolem
            }
            | None => {
                let (def, _) = pattern.try_destruct_def(tycker);
                Alloc::alloc(tycker, def, payload_kind, &())
            }
        };
        tycker.transfer_builtin_role_k(source_binder.witness, skolem)?;
        tycker.statics.existential_skolems.ensure(skolem);
        self.scope.env = self.scope.env.clone().with_skolem(skolem);
        let abstract_ty = Alloc::alloc(tycker, skolem, payload_kind, &self.scope.env);
        let full_witness = source_binder.pattern.introduce_payload(tycker, abstract_ty);
        let full_witness = tycker.err_p_to_k(full_witness)?;
        self.scope.env = TyEnvT::new(self.scope.env.clone(), Assign(pattern, full_witness))
            .tyck_k(tycker, ())?
            .info;
        self.scope.opened.push(skolem);
        self.boundary_opened += 1;
        Ok(next_ty.with_abstract(source_binder.witness, abstract_ty))
    }

    /// Bind a manifest witness to its definition.
    fn open_manifest_witness_k(
        &mut self, tycker: &mut Tycker<'_>, witness: OpenedWitness,
        definition: DeferredTelescopeType, next_ty: DeferredTelescopeType,
    ) -> ResultKont<DeferredTelescopeType> {
        let OpenedWitness { pattern, source_binder, payload_kind } = witness;
        let definition = definition.materialize_k(tycker)?;
        let definition_kind = tycker.statics.type_kind(definition);
        Lub::lub_k(payload_kind, definition_kind, tycker)?;
        let full_definition = source_binder.pattern.introduce_payload(tycker, definition);
        let full_definition = tycker.err_p_to_k(full_definition)?;
        self.scope.env = TyEnvT::new(self.scope.env.clone(), Assign(pattern, full_definition))
            .tyck_k(tycker, ())?
            .info;
        Ok(next_ty.with_abstract(source_binder.witness, definition))
    }
}

/// A checked existential component together with the telescope binder it opens.
struct OpenedWitness {
    pattern: ss::TPatId,
    source_binder: ss::TypeBinder,
    payload_kind: KindId,
}

impl PatternChecker<'_> {
    pub(super) fn check_cons_k<'db>(
        &self, tycker: &mut Tycker<'db>, pat: Vec<su::PatId>, switch: Switch<AnnId>,
        skolems: PatternSkolems,
    ) -> ResultKont<CheckedPattern> {
        let mut items = pat;
        let tail = items.pop().expect("a cons pattern carries at least two components");
        match switch {
            | Switch::Syn => self.check_cons_syn_k(tycker, items, tail, skolems),
            | Switch::Ana(AnnId::Type(expected)) => {
                match expected.reveal_or_refine_product_k(tycker, &self.info)? {
                    | ss::Type::Prod(_) => {
                        self.check_cons_ana_product_k(tycker, items, tail, expected, skolems)
                    }
                    | ss::Type::Exists(_) | ss::Type::ManifestKind(_) => {
                        self.check_cons_ana_package_k(tycker, items, tail, expected, skolems)
                    }
                    | _ => tycker.err_k(TyckError::TypeExpected {
                        expected: "one of `_ * _` or `exists _ . _`".to_string(),
                        found: expected,
                    }),
                }
            }
            | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => tycker.err_k(TyckError::SortMismatch),
        }
    }

    /// Synthesize a product pattern from its components, left to right.
    fn check_cons_syn_k(
        &self, tycker: &mut Tycker<'_>, items: Vec<su::PatId>, tail: su::PatId,
        skolems: PatternSkolems,
    ) -> ResultKont<CheckedPattern> {
        let mut scope = ConsScope { env: self.info.clone(), opened: Vec::new() };
        let components = items
            .into_iter()
            .map(|item| {
                let action = PatternAction::syn().with_skolems(skolems.clone());
                scope.check_value_k(tycker, item, action)
            })
            .collect::<ResultKont<Vec<_>>>()?;
        let tail = scope.check_value_k(
            tycker,
            tail,
            PatternAction::syn().with_skolems(skolems.clone()),
        )?;
        Ok(self.finish_product_pattern(tycker, scope, components, tail))
    }

    /// Check a product pattern against a product type with matching components.
    fn check_cons_ana_product_k(
        &self, tycker: &mut Tycker<'_>, items: Vec<su::PatId>, tail: su::PatId,
        expected: ss::TypeId, skolems: PatternSkolems,
    ) -> ResultKont<CheckedPattern> {
        let ss::Prod(component_tys) = expected.view_product_k(tycker, &self.info)?;
        if component_tys.len() != items.len() + 1 {
            tycker.err_k(TyckError::TypeExpected {
                expected: "a product with matching components".to_string(),
                found: expected,
            })?
        }
        let mut scope = ConsScope { env: self.info.clone(), opened: Vec::new() };
        let (components, tail) =
            scope.check_value_components_k(tycker, &items, tail, &component_tys, &skolems)?;
        Ok(self.finish_product_pattern(tycker, scope, components, tail))
    }

    /// Record a product pattern through the cons judgment.
    fn finish_product_pattern(
        &self, tycker: &mut Tycker<'_>, scope: ConsScope,
        components: Vec<(ss::VPatId, ss::TypeId)>, tail: (ss::VPatId, ss::TypeId),
    ) -> CheckedPattern {
        let ConsScope { env: pattern_env, opened } = scope;
        let item_outcomes =
            components.into_iter().map(|(vpat, ty)| PatAnnId::Value(vpat, ty)).collect::<Vec<_>>();
        let items_interned = crate::query::InternedPatItems::new(tycker.db, item_outcomes);
        let (tail, tail_annotation) = tail;
        let tail_interned =
            crate::query::InternedPatAnn::new(tycker.db, PatAnnId::Value(tail, tail_annotation));
        let outcome = tycker.judge(
            self.inner,
            (items_interned, tail_interned),
            |db, data, pat, (items, tail), occurrence| {
                crate::query::pat_cons_syn_judgment(db, data, pat, items, tail, occurrence)
            },
        );
        for (id, prod) in outcome.prods {
            tycker.statics.types_pre.insert_new(id, ss::Fillable::Done(prod), outcome.vtype);
            tycker.store_env(id, &pattern_env);
        }
        tycker.statics.vpats.insert_new(outcome.pat_id, outcome.pat);
        tycker.statics.annotations_vpat.insert_new(outcome.pat_id, outcome.ann);
        tycker.statics.env_vpat.insert_new(outcome.pat_id, self.info.clone());
        TyEnvT::new(
            pattern_env,
            PatternCheck::with_opened(PatAnnId::Value(outcome.pat_id, outcome.ann), opened),
        )
    }

    /// Check a package pattern against an existential or manifest-kind
    /// telescope: open its static components in order, then check the
    /// remaining components against the package body.
    fn check_cons_ana_package_k(
        &self, tycker: &mut Tycker<'_>, items: Vec<su::PatId>, tail: su::PatId,
        expected: ss::TypeId, skolems: PatternSkolems,
    ) -> ResultKont<CheckedPattern> {
        let mut opening = PackageOpening {
            scope: ConsScope { env: self.info.clone(), opened: Vec::new() },
            static_patterns: Vec::new(),
            boundary_opened: 0,
        };
        let mut body_ty = DeferredTelescopeType::new(expected);
        let mut body_index = items.len();
        for (index, item) in items.iter().copied().enumerate() {
            body_ty = match body_ty.with_environment(&opening.scope.env).reveal_k(tycker)? {
                | DeferredTelescopeView::ManifestKind { binder: _, definition, body } => {
                    opening.open_manifest_kind_k(tycker, item, definition, &skolems)?;
                    body
                }
                | DeferredTelescopeView::Exists { binder, mode, body } => {
                    opening.open_exists_k(tycker, item, binder, mode, body, &skolems)?
                }
                | DeferredTelescopeView::Other(body) => {
                    body_ty = body;
                    body_index = index;
                    break;
                }
            };
        }

        if opening.static_patterns.is_empty() {
            tycker.err_k(TyckError::TypeExpected {
                expected: "an existential package".to_string(),
                found: expected,
            })?
        }

        let body_ty = body_ty.with_environment(&opening.scope.env).materialize_k(tycker)?;
        let body = opening.scope.check_package_body_k(
            tycker,
            &items[body_index..],
            tail,
            body_ty,
            &skolems,
        )?;
        let PackageOpening { scope, static_patterns, boundary_opened } = opening;
        let cons = Alloc::alloc(tycker, ss::ConsN(static_patterns, body), expected, &self.info);
        if boundary_opened > 0 {
            tycker.statics.package_pattern_opened_arity.insert_new(cons, boundary_opened);
        }
        Ok(TyEnvT::new(
            scope.env,
            PatternCheck::with_opened(PatAnnId::Value(cons, expected), scope.opened),
        ))
    }
}
