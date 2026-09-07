//! Product and package patterns, including ordered witness opening.

use super::*;

impl PatternChecker<'_> {
    pub(super) fn check_cons_k<'db>(
        &self, tycker: &mut Tycker<'db>, pat: Vec<su::PatId>, switch: Switch<AnnId>,
        skolems: PatternSkolems,
    ) -> ResultKont<CheckedPattern> {
        Ok({
            let mut items = pat;
            let tail = items.pop().expect("a cons pattern carries at least two components");
            match switch {
                | Switch::Syn => {
                    let initial = (self.info.clone(), Vec::new(), Vec::new(), Vec::new());
                    let (pattern_env, output, annotations, mut opened) =
                        items.into_iter().try_fold(initial, |state, item| -> ResultKont<_> {
                            let (pattern_env, mut output, mut annotations, mut opened) = state;
                            let checked = TyEnvT::new(pattern_env, item).tyck_k(
                                tycker,
                                PatternAction::syn().with_skolems(skolems.clone()),
                            )?;
                            let TyEnvT { info, inner } = checked;
                            let (item, annotation) = inner.annotation.try_as_value(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            output.push(item);
                            annotations.push(annotation);
                            opened.extend(inner.opened);
                            Ok((info, output, annotations, opened))
                        })?;

                    let checked = TyEnvT::new(pattern_env, tail)
                        .tyck_k(tycker, PatternAction::syn().with_skolems(skolems.clone()))?;
                    let TyEnvT { info: pattern_env, inner } = checked;
                    let (tail, tail_annotation) = inner.annotation.try_as_value(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?;
                    opened.extend(inner.opened);
                    let item_outcomes = output
                        .iter()
                        .zip(&annotations)
                        .map(|(vpat, ty)| PatAnnId::Value(*vpat, *ty))
                        .collect::<Vec<_>>();
                    let pat_interned = crate::query::InternedPat::new(tycker.db, self.inner);
                    let items_interned =
                        crate::query::InternedPatItems::new(tycker.db, item_outcomes);
                    let tail_interned = crate::query::InternedPatAnn::new(
                        tycker.db,
                        PatAnnId::Value(tail, tail_annotation),
                    );
                    let Some(outcome) = crate::query::pat_cons_syn_judgment(
                        tycker.db,
                        tycker.data,
                        pat_interned,
                        items_interned,
                        tail_interned,
                        tycker.site_occurrence(),
                    ) else {
                        unreachable!("consumed pattern judgments are query-produced")
                    };
                    for (id, prod) in outcome.prods {
                        tycker.statics.types_pre.insert_new(
                            id,
                            ss::Fillable::Done(prod),
                            outcome.vtype,
                        );
                        tycker.store_env(id, &pattern_env);
                    }
                    tycker.statics.vpats.insert_new(outcome.pat_id, outcome.pat);
                    tycker.statics.annotations_vpat.insert_new(outcome.pat_id, outcome.ann);
                    tycker.statics.env_vpat.insert_new(outcome.pat_id, self.info.clone());
                    TyEnvT::new(
                        pattern_env,
                        PatternCheck::with_opened(
                            PatAnnId::Value(outcome.pat_id, outcome.ann),
                            opened,
                        ),
                    )
                }
                | Switch::Ana(AnnId::Type(expected)) => {
                    let expected_view = expected;
                    match expected_view.reveal_or_refine_product_k(tycker, &self.info)? {
                        | ss::Type::Prod(_) => {
                            let ss::Prod(component_tys) =
                                expected_view.view_product_k(tycker, &self.info)?;
                            if component_tys.len() != items.len() + 1 {
                                tycker.err_k(
                                    TyckError::TypeExpected {
                                        expected: "a product with matching components".to_string(),
                                        found: expected_view,
                                    },
                                    std::panic::Location::caller(),
                                )?
                            }
                            let item_count = items.len();
                            let mut pattern_env = self.info.clone();
                            let mut opened = Vec::new();
                            let mut output = Vec::with_capacity(item_count);
                            let mut annotations = Vec::with_capacity(item_count);
                            for (item, item_ty) in
                                items.into_iter().zip(component_tys.iter().copied())
                            {
                                let checked = TyEnvT::new(pattern_env.clone(), item).tyck_k(
                                    tycker,
                                    PatternAction::ana(item_ty.into())
                                        .with_skolems(skolems.clone()),
                                )?;
                                let (item, annotation) = checked.try_as_value(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                pattern_env = checked.info;
                                opened.extend(checked.inner.opened);
                                output.push(item);
                                annotations.push(annotation);
                            }

                            let remaining = tycker
                                .rest_product_k(&component_tys[item_count..], &pattern_env)?;
                            let checked = TyEnvT::new(pattern_env.clone(), tail).tyck_k(
                                tycker,
                                PatternAction::ana(remaining.into()).with_skolems(skolems.clone()),
                            )?;
                            let (tail, ann) = checked.try_as_value(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            pattern_env = checked.info;
                            opened.extend(checked.inner.opened);
                            let item_outcomes = output
                                .iter()
                                .zip(&annotations)
                                .map(|(vpat, ty)| PatAnnId::Value(*vpat, *ty))
                                .collect::<Vec<_>>();
                            let pat_interned =
                                crate::query::InternedPat::new(tycker.db, self.inner);
                            let items_interned =
                                crate::query::InternedPatItems::new(tycker.db, item_outcomes);
                            let tail_interned = crate::query::InternedPatAnn::new(
                                tycker.db,
                                PatAnnId::Value(tail, ann),
                            );
                            let Some(outcome) = crate::query::pat_cons_syn_judgment(
                                tycker.db,
                                tycker.data,
                                pat_interned,
                                items_interned,
                                tail_interned,
                                tycker.site_occurrence(),
                            ) else {
                                unreachable!(
                                    "the product arm of consumed pattern judgments is query-produced"
                                )
                            };
                            for (id, prod) in outcome.prods {
                                tycker.statics.types_pre.insert_new(
                                    id,
                                    ss::Fillable::Done(prod),
                                    outcome.vtype,
                                );
                                tycker.store_env(id, &pattern_env);
                            }
                            tycker.statics.vpats.insert_new(outcome.pat_id, outcome.pat);
                            tycker.statics.annotations_vpat.insert_new(outcome.pat_id, outcome.ann);
                            tycker.statics.env_vpat.insert_new(outcome.pat_id, self.info.clone());
                            TyEnvT::new(
                                pattern_env,
                                PatternCheck::with_opened(
                                    PatAnnId::Value(outcome.pat_id, outcome.ann),
                                    opened,
                                ),
                            )
                        }
                        | ss::Type::Exists(_) | ss::Type::ManifestKind(_) => {
                            let mut body_env = self.info.clone();
                            let mut body_ty = DeferredTelescopeType::new(expected);
                            let mut body_index = items.len();
                            let mut static_patterns: Vec<ss::StaticPatId> = Vec::new();
                            let mut opened = Vec::new();
                            let mut boundary_opened = 0usize;
                            for (index, item) in items.iter().copied().enumerate() {
                                match body_ty.with_environment(&body_env).reveal_k(tycker)? {
                                    | DeferredTelescopeView::ManifestKind {
                                        binder,
                                        definition,
                                        body,
                                    } => {
                                        let checked = TyEnvT::new(body_env.clone(), item).tyck_k(
                                            tycker,
                                            PatternAction::ana(AnnId::Set)
                                                .with_skolems(skolems.clone()),
                                        )?;
                                        let pattern = checked.annotation.try_as_kind(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?;
                                        body_env =
                                            TyEnvT::new(body_env, Assign(pattern, definition))
                                                .tyck_k(tycker, ())?
                                                .info;
                                        body_ty = body;
                                        static_patterns.push(pattern.into());
                                        let _ = binder;
                                    }
                                    | DeferredTelescopeView::Exists {
                                        binder: source_binder,
                                        mode,
                                        body: next_ty,
                                    } => {
                                        let domain_kind = source_binder.domain_kind(tycker);
                                        let payload_kind = source_binder.payload_kind(tycker);
                                        let checked = TyEnvT::new(body_env.clone(), item).tyck_k(
                                            tycker,
                                            PatternAction::ana(domain_kind.into())
                                                .with_skolems(skolems.clone()),
                                        )?;
                                        let (witness, _) = checked.try_as_type(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?;
                                        body_env = checked.info;
                                        opened.extend(checked.inner.opened);

                                        match mode {
                                            | DeferredTelescopeExistsMode::Abstract => {
                                                // Ordinary elimination is fresh. Checking a PackPi
                                                // introduction instead reuses the signature's
                                                // canonical identity for this pattern component.
                                                let skolem = match skolems.get(&item) {
                                                    | Some(skolem) => {
                                                        let expected = tycker
                                                            .statics
                                                            .annotations_abst[&skolem];
                                                        Lub::lub_k(expected, payload_kind, tycker)?;
                                                        skolem
                                                    }
                                                    | None => {
                                                        let (def, _) =
                                                            witness.try_destruct_def(tycker);
                                                        Alloc::alloc(tycker, def, payload_kind, &())
                                                    }
                                                };
                                                tycker.transfer_builtin_role_k(
                                                    source_binder.witness,
                                                    skolem,
                                                )?;
                                                tycker.statics.existential_skolems.ensure(skolem);
                                                body_env = body_env.with_skolem(skolem);
                                                let abstract_ty = Alloc::alloc(
                                                    tycker,
                                                    skolem,
                                                    payload_kind,
                                                    &body_env,
                                                );
                                                let full_witness = source_binder
                                                    .pattern
                                                    .introduce_payload(tycker, abstract_ty);
                                                let full_witness =
                                                    tycker.err_p_to_k(full_witness)?;
                                                body_env = TyEnvT::new(
                                                    body_env,
                                                    Assign(witness, full_witness),
                                                )
                                                .tyck_k(tycker, ())?
                                                .info;
                                                body_ty = next_ty.with_abstract(
                                                    source_binder.witness,
                                                    abstract_ty,
                                                );
                                                opened.push(skolem);
                                                boundary_opened += 1;
                                            }
                                            | DeferredTelescopeExistsMode::Manifest(definition) => {
                                                let definition =
                                                    definition.materialize_k(tycker)?;
                                                let definition_kind =
                                                    tycker.statics.type_kind(definition);
                                                Lub::lub_k(payload_kind, definition_kind, tycker)?;
                                                let full_definition = source_binder
                                                    .pattern
                                                    .introduce_payload(tycker, definition);
                                                let full_definition =
                                                    tycker.err_p_to_k(full_definition)?;
                                                body_env = TyEnvT::new(
                                                    body_env,
                                                    Assign(witness, full_definition),
                                                )
                                                .tyck_k(tycker, ())?
                                                .info;
                                                body_ty = next_ty.with_abstract(
                                                    source_binder.witness,
                                                    definition,
                                                );
                                            }
                                        }
                                        static_patterns.push(witness.into());
                                    }
                                    | DeferredTelescopeView::Other(body) => {
                                        body_ty = body;
                                        body_index = index;
                                        break;
                                    }
                                }
                            }

                            if static_patterns.is_empty() {
                                tycker.err_k(
                                    TyckError::TypeExpected {
                                        expected: "an existential package".to_string(),
                                        found: expected,
                                    },
                                    std::panic::Location::caller(),
                                )?
                            }

                            let body_ty =
                                body_ty.with_environment(&body_env).materialize_k(tycker)?;
                            let body_items = &items[body_index..];
                            let body = if body_items.is_empty() {
                                let checked = TyEnvT::new(body_env.clone(), tail).tyck_k(
                                    tycker,
                                    PatternAction::ana(body_ty.into())
                                        .with_skolems(skolems.clone()),
                                )?;
                                let (body, _) = checked.try_as_value(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                body_env = checked.info;
                                opened.extend(checked.inner.opened);
                                body
                            } else {
                                let body_view = body_ty;
                                let ss::Prod(body_component_tys) =
                                    body_view.view_product_k(tycker, &body_env)?;

                                let mut output = Vec::with_capacity(body_items.len());
                                let mut annotations = Vec::with_capacity(body_items.len());
                                for (item, item_ty) in body_items
                                    .iter()
                                    .copied()
                                    .zip(body_component_tys.iter().copied())
                                {
                                    let checked = TyEnvT::new(body_env.clone(), item).tyck_k(
                                        tycker,
                                        PatternAction::ana(item_ty.into())
                                            .with_skolems(skolems.clone()),
                                    )?;
                                    let (item, annotation) = checked.try_as_value(
                                        tycker,
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?;
                                    body_env = checked.info;
                                    opened.extend(checked.inner.opened);
                                    output.push(item);
                                    annotations.push(annotation);
                                }

                                let remaining = tycker.rest_product_k(
                                    &body_component_tys[body_items.len()..],
                                    &body_env,
                                )?;
                                let checked = TyEnvT::new(body_env.clone(), tail).tyck_k(
                                    tycker,
                                    PatternAction::ana(remaining.into())
                                        .with_skolems(skolems.clone()),
                                )?;
                                let (tail, ann) = checked.try_as_value(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                body_env = checked.info;
                                opened.extend(checked.inner.opened);
                                let vtype = ss::VType.build(tycker, &body_env);
                                let mut component_tys = annotations;
                                component_tys.push(ann);
                                let ann =
                                    Alloc::alloc(tycker, ss::Prod(component_tys), vtype, &body_env);
                                let mut components = output;
                                components.push(tail);
                                Alloc::alloc(
                                    tycker,
                                    ss::ValuePattern::VCons(components),
                                    ann,
                                    &body_env,
                                )
                            };
                            let cons = Alloc::alloc(
                                tycker,
                                ss::ConsN(static_patterns, body),
                                expected,
                                &self.info,
                            );
                            if boundary_opened > 0 {
                                tycker
                                    .statics
                                    .package_pattern_opened_arity
                                    .insert_new(cons, boundary_opened);
                            }
                            TyEnvT::new(
                                body_env,
                                PatternCheck::with_opened(PatAnnId::Value(cons, expected), opened),
                            )
                        }
                        | _ => tycker.err_k(
                            TyckError::TypeExpected {
                                expected: "one of `_ * _` or `exists _ . _`".to_string(),
                                found: expected,
                            },
                            std::panic::Location::caller(),
                        )?,
                    }
                }
                | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                }
            }
        })
    }
}
