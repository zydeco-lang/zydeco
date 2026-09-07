//! Open package telescopes once and bind selective fields and whole-package aliases.

use super::*;

impl ExistentialProjectionSlot {
    fn field(&self) -> Option<&FieldName> {
        match self {
            | Self::Kind { field, .. } | Self::Type { field, .. } => field.as_ref(),
        }
    }

    fn pattern(&self) -> ss::StaticPatId {
        match self {
            | Self::Kind { pattern, .. } => (*pattern).into(),
            | Self::Type { pattern, .. } => (*pattern).into(),
        }
    }

    fn term(&self) -> ss::StaticTermId {
        match self {
            | Self::Kind { definition, .. } => (*definition).into(),
            | Self::Type { full_payload, .. } => (*full_payload).into(),
        }
    }

    fn opens_abstract(&self) -> bool {
        matches!(self, Self::Type { skolem: Some(_), .. })
    }

    fn set_pattern(&mut self, pattern: ss::StaticPatId) {
        match (self, pattern) {
            | (Self::Kind { pattern, .. }, ss::StaticPatId::Kind(selected)) => *pattern = selected,
            | (Self::Type { pattern, .. }, ss::StaticPatId::Type(selected)) => *pattern = selected,
            | _ => unreachable!("a selected package field retains its static sort"),
        }
    }
}

impl ExistentialProjectionPattern {
    pub(in crate::check) fn members(
        tycker: &Tycker<'_>, patterns: impl IntoIterator<Item = su::PatId>,
    ) -> Option<Vec<ExistentialProjectionMember>> {
        let members = patterns
            .into_iter()
            .map(|source| match tycker.scoped.pats[&source].to_owned() {
                | su::Pattern::Project(su::ProjectionPattern(field, payload)) => {
                    ExistentialProjectionMember::Project { source, field, payload }
                }
                | _ => ExistentialProjectionMember::Whole(source),
            })
            .collect::<Vec<_>>();
        members
            .iter()
            .any(|member| matches!(member, ExistentialProjectionMember::Project { .. }))
            .then_some(members)
    }

    pub(in crate::check) fn applies_k(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, expected: ss::TypeId,
    ) -> ResultKont<bool> {
        let view = expected.unroll_k(tycker)?.subst_env_k(tycker, env)?;
        Ok(matches!(tycker.type_filled_k(&view)?, ss::Type::ManifestKind(_) | ss::Type::Exists(_)))
    }

    pub(super) fn kind_field_name(tycker: &Tycker<'_>, pattern: ss::KPatId) -> Option<FieldName> {
        match tycker.statics.kpats[&pattern] {
            | ss::KindPattern::Var(definition) => Some(tycker.def_name(&definition).plain().into()),
            | ss::KindPattern::Hole(_) => None,
        }
    }

    pub(super) fn type_field_name(
        tycker: &Tycker<'_>, binder: &ss::TypeBinder,
    ) -> Option<FieldName> {
        match tycker.statics.tpats[&binder.pattern].to_owned() {
            | ss::TypePattern::Named(ss::Named(field, _)) => Some(field),
            | ss::TypePattern::Var(definition) => Some(tycker.def_name(&definition).plain().into()),
            | ss::TypePattern::Hole(_) => tycker
                .statics
                .abst_hints
                .get(&binder.witness)
                .map(|definition| tycker.def_name(definition).plain().into()),
        }
    }

    fn open_k(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, expected: ss::TypeId, skolems: &PatternSkolems,
    ) -> ResultKont<ExistentialProjectionOpening> {
        let mut body = DeferredTelescopeType::new(expected);
        let mut body_env = env.clone();
        let mut slots = Vec::new();
        let mut opened = Vec::new();

        let body = loop {
            match body.with_environment(&body_env).reveal_k(tycker)? {
                | DeferredTelescopeView::ManifestKind { binder, definition, body: next } => {
                    let field = Self::kind_field_name(tycker, binder);
                    body_env =
                        TyEnvT::new(body_env, Assign(binder, definition)).tyck_k(tycker, ())?.info;
                    body = next;
                    slots.push(ExistentialProjectionSlot::Kind {
                        field,
                        definition,
                        pattern: binder,
                    });
                }
                | DeferredTelescopeView::Exists { binder, mode, body: next } => {
                    let payload_kind = binder.payload_kind(tycker);
                    let field = Self::type_field_name(tycker, &binder);
                    let (payload, skolem) = match mode {
                        | DeferredTelescopeExistsMode::Abstract => {
                            let skolem = match skolems.get_witness(&binder.witness) {
                                | Some(skolem) => {
                                    let canonical_kind = tycker.statics.annotations_abst[&skolem];
                                    Lub::lub_k(canonical_kind, payload_kind, tycker)?;
                                    skolem
                                }
                                | None => {
                                    let (definition, _) = binder.pattern.try_destruct_def(tycker);
                                    Alloc::alloc(tycker, definition, payload_kind, &())
                                }
                            };
                            tycker.transfer_builtin_role_k(binder.witness, skolem)?;
                            tycker.statics.existential_skolems.ensure(skolem);
                            body_env = body_env.with_skolem(skolem);
                            let payload = Alloc::alloc(tycker, skolem, payload_kind, &body_env);
                            opened.push(skolem);
                            (payload, Some(skolem))
                        }
                        | DeferredTelescopeExistsMode::Manifest(definition) => {
                            let definition = definition.materialize_k(tycker)?;
                            let definition_kind = tycker.statics.type_kind(definition);
                            Lub::lub_k(payload_kind, definition_kind, tycker)?;
                            (definition, None)
                        }
                    };
                    let full_payload = binder.pattern.introduce_payload(tycker, payload);
                    let full_payload = tycker.err_p_to_k(full_payload)?;
                    body_env = TyEnvT::new(body_env, Assign(binder.pattern, full_payload))
                        .tyck_k(tycker, ())?
                        .info;
                    body = next.with_abstract(binder.witness, payload);
                    slots.push(ExistentialProjectionSlot::Type {
                        field,
                        source_pattern: binder.pattern,
                        payload_kind,
                        payload,
                        full_payload,
                        pattern: binder.pattern,
                        skolem,
                    });
                }
                | DeferredTelescopeView::Other(body) => break body.materialize_k(tycker)?,
            }
        };

        Ok(ExistentialProjectionOpening { expected, body, slots, env: body_env, opened })
    }

    fn wrap_type_pattern(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, source: ss::TPatId, payload: ss::TPatId,
    ) -> ss::TPatId {
        match tycker.statics.tpats[&source].to_owned() {
            | ss::TypePattern::Named(ss::Named(field, inner)) => {
                let inner = Self::wrap_type_pattern(tycker, env, inner, payload);
                let annotation = tycker.statics.annotations_tpat[&source];
                Alloc::alloc(tycker, ss::Named(field, inner), annotation, env)
            }
            | ss::TypePattern::Hole(_) | ss::TypePattern::Var(_) => payload,
        }
    }

    /// Check the payload pattern of a selected telescope entry and adopt its binding.
    fn select_slot_k(
        tycker: &mut Tycker<'_>, opening: &mut ExistentialProjectionOpening, slot_index: usize,
        payload: su::PatId, skolems: &PatternSkolems,
    ) -> ResultKont<ss::PatId> {
        let selected = match &opening.slots[slot_index] {
            | ExistentialProjectionSlot::Kind { definition, .. } => {
                let checked = TyEnvT::new(opening.env.clone(), payload)
                    .tyck_k(tycker, PatternAction::ana(AnnId::Set).with_skolems(skolems.clone()))?;
                let payload_pattern = checked.annotation.try_as_kind(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                opening.env = TyEnvT::new(checked.info, Assign(payload_pattern, *definition))
                    .tyck_k(tycker, ())?
                    .info;
                ss::StaticPatId::Kind(payload_pattern)
            }
            | ExistentialProjectionSlot::Type {
                source_pattern,
                payload_kind,
                payload: slot_payload,
                skolem,
                ..
            } => {
                let checked = TyEnvT::new(opening.env.clone(), payload).tyck_k(
                    tycker,
                    PatternAction::ana((*payload_kind).into()).with_skolems(skolems.clone()),
                )?;
                let (payload_pattern, _) = checked.try_as_type(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                opening.env = TyEnvT::new(checked.info, Assign(payload_pattern, *slot_payload))
                    .tyck_k(tycker, ())?
                    .info;
                if let (Some(skolem), (Some(definition), _)) =
                    (*skolem, payload_pattern.try_destruct_def(tycker))
                    && tycker.statics.abst_hints.get(&skolem).is_none()
                {
                    tycker.statics.abst_hints.insert_new(skolem, definition);
                }
                let full_pattern =
                    Self::wrap_type_pattern(tycker, &opening.env, *source_pattern, payload_pattern);
                ss::StaticPatId::Type(full_pattern)
            }
        };
        opening.slots[slot_index].set_pattern(selected);
        Ok(match selected {
            | ss::StaticPatId::Kind(pattern) => ss::PatId::Kind(pattern),
            | ss::StaticPatId::Type(pattern) => ss::PatId::Type(pattern),
        })
    }

    pub(in crate::check) fn check_k(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, expected: ss::TypeId,
        members: Vec<ExistentialProjectionMember>, skolems: PatternSkolems,
    ) -> ResultKont<CheckedPattern> {
        let mut opening = Self::open_k(tycker, env, expected, &skolems)?;
        let mut body_patterns = Vec::new();
        let mut whole_patterns = Vec::new();
        let mut package_aliases = Vec::new();
        let mut package_selections = Vec::new();

        for member in members {
            let ExistentialProjectionMember::Project { source, field, payload } = member else {
                let ExistentialProjectionMember::Whole(source) = member else { unreachable!() };
                let checked = TyEnvT::new(opening.env.clone(), source).tyck_k(
                    tycker,
                    PatternAction::ana(expected.into()).with_skolems(skolems.clone()),
                )?;
                let (pattern, _) = checked.try_as_value(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                if !ValuePatternShape::is_irrefutable(tycker, pattern) {
                    tycker
                        .err_k(TyckError::RefutablePatternAlias, std::panic::Location::caller())?
                }
                opening.env = checked.info;
                opening.opened.extend(checked.inner.opened);
                if let Some(definition) = Self::whole_definition(tycker, pattern) {
                    package_aliases.push(definition);
                }
                whole_patterns.push(pattern);
                continue;
            };
            let static_candidates = opening
                .slots
                .iter()
                .enumerate()
                .filter_map(|(index, slot)| (slot.field() == Some(&field)).then_some(index))
                .collect::<Vec<_>>();
            let value_candidates = FieldProjectionResolver::value_candidates_k(
                tycker,
                DeferredEnvType::with_environment(opening.body, &opening.env),
                &field,
                &[],
                &mut std::collections::HashSet::new(),
            )?;
            let package_field_candidates = FieldProjectionResolver::package_candidates_k(
                tycker,
                DeferredEnvType::with_environment(opening.body, &opening.env),
                &field,
                &[],
                false,
                false,
                &mut std::collections::HashSet::new(),
            )?;

            match (
                static_candidates.as_slice(),
                value_candidates.as_slice(),
                package_field_candidates.as_slice(),
            ) {
                | ([], [], []) => tycker.err_k(
                    TyckError::MissingNamedField { field, found: expected },
                    std::panic::Location::caller(),
                )?,
                | ([slot_index], [], []) => {
                    let selected =
                        Self::select_slot_k(tycker, &mut opening, *slot_index, payload, &skolems)?;
                    tycker.statics.pats.record(source, selected);
                }
                | ([], [candidate], []) => {
                    let candidate = candidate.clone().materialize_k(tycker)?;
                    FieldProjectionResolver::record_value_origin(tycker, source.into(), &candidate);
                    let checked = TyEnvT::new(opening.env.clone(), payload).tyck_k(
                        tycker,
                        PatternAction::ana(candidate.projected.into())
                            .with_skolems(skolems.clone()),
                    )?;
                    let (payload_pattern, _) = checked.try_as_value(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?;
                    if !ValuePatternShape::is_irrefutable(tycker, payload_pattern) {
                        tycker.err_k(
                            TyckError::RefutableFieldProjectionPattern,
                            std::panic::Location::caller(),
                        )?
                    }
                    opening.env = checked.info;
                    opening.opened.extend(checked.inner.opened);
                    let pattern = FieldProjectionResolver::value_pattern(
                        tycker,
                        &opening.env,
                        opening.body,
                        candidate.clone(),
                        payload_pattern,
                    );
                    tycker.statics.pats.record(source, pattern.into());
                    body_patterns.push(pattern);
                }
                | ([], [], [candidate]) => {
                    package_selections.push(PackageBodySelection {
                        source,
                        payload,
                        candidate: candidate.clone(),
                    });
                }
                | _ => tycker.err_k(
                    TyckError::DuplicateNamedField { field, found: expected },
                    std::panic::Location::caller(),
                )?,
            }
        }

        if !package_selections.is_empty() {
            let deep =
                Self::package_body_pattern_k(tycker, &mut opening, &package_selections, &skolems)?;
            for selection in &package_selections {
                tycker.statics.pats.record(selection.source, deep.into());
            }
            body_patterns.push(deep);
        }
        let body_pattern = match body_patterns.len() {
            | 0 => Alloc::alloc(tycker, ss::Hole, opening.body, &opening.env),
            | 1 => body_patterns[0],
            | _ => {
                let patterns = ss::ConsN::from_vec(body_patterns).unwrap();
                Alloc::alloc(tycker, ss::Alias(patterns), opening.body, &opening.env)
            }
        };
        let boundary_opened = opening.slots.iter().filter(|slot| slot.opens_abstract()).count();
        let deep_opened = Self::subtree_opened_abstracts(tycker, &body_pattern);
        let package_terms: Vec<ss::StaticTermId> =
            opening.slots.iter().map(ExistentialProjectionSlot::term).collect();
        let static_patterns: Vec<ss::StaticPatId> =
            opening.slots.iter().map(ExistentialProjectionSlot::pattern).collect();
        let package_pattern = if opening.slots.is_empty() {
            body_pattern
        } else {
            Alloc::alloc(tycker, ss::ConsN(static_patterns, body_pattern), opening.expected, env)
        };
        if !opening.slots.is_empty() && boundary_opened + deep_opened > 0 {
            tycker
                .statics
                .package_pattern_opened_arity
                .insert_new(package_pattern, boundary_opened + deep_opened);
        }
        for definition in package_aliases {
            let _ = tycker.statics.package_aliases.upsert(definition, package_terms.clone());
        }
        let pattern = if whole_patterns.is_empty() {
            package_pattern
        } else {
            let patterns = ss::ConsN::from_vec(
                std::iter::once(package_pattern).chain(whole_patterns).collect(),
            )
            .unwrap();
            Alloc::alloc(tycker, ss::Alias(patterns), opening.expected, env)
        };
        Ok(TyEnvT::new(
            opening.env,
            PatternCheck::with_opened(PatAnnId::Value(pattern, opening.expected), opening.opened),
        ))
    }

    /// Elaborate deep selections into one pattern over the package body.
    ///
    /// Selections through the same nested package occurrence share its opening,
    /// so identities selected at different depths through one package agree.
    fn package_body_pattern_k(
        tycker: &mut Tycker<'_>, opening: &mut ExistentialProjectionOpening,
        selections: &[PackageBodySelection], skolems: &PatternSkolems,
    ) -> ResultKont<ss::VPatId> {
        let group = selections.iter().collect::<Vec<_>>();
        let node = opening.body.unroll_k(tycker)?.subst_env_k(tycker, &opening.env)?;
        Self::deep_node_pattern_k(
            tycker,
            &mut opening.env,
            &mut opening.opened,
            node,
            &group,
            0,
            skolems,
        )
    }

    /// Build the pattern for one node of the merged deep-selection trie.
    ///
    /// Every selection in `group` reached this node through the same prefix of
    /// its route, so their next steps agree on the node's structure.
    fn deep_node_pattern_k(
        tycker: &mut Tycker<'_>, env: &mut ss::TyEnv, opened: &mut Vec<ss::AbstId>,
        node: ss::TypeId, group: &[&PackageBodySelection], consumed: usize,
        skolems: &PatternSkolems,
    ) -> ResultKont<ss::VPatId> {
        let view = tycker.type_filled_k(&node)?.to_owned();
        match view {
            | ss::Type::Prod(ss::Prod(components)) => {
                let mut patterns = Vec::new();
                for (position, component) in components.into_iter().enumerate() {
                    let members = group
                        .iter()
                        .copied()
                        .filter(|selection| {
                            matches!(
                                selection.candidate.steps.get(consumed),
                                Some(PackageFieldStep::Product { position: found })
                                if *found == position
                            )
                        })
                        .collect::<Vec<_>>();
                    if members.is_empty() {
                        patterns.push(Alloc::alloc(tycker, ss::Hole, component, env));
                        continue;
                    }
                    let component = component.unroll_k(tycker)?.subst_env_k(tycker, env)?;
                    patterns.push(Self::deep_node_pattern_k(
                        tycker,
                        env,
                        opened,
                        component,
                        &members,
                        consumed + 1,
                        skolems,
                    )?);
                }
                Ok(Alloc::alloc(tycker, ss::ValuePattern::VCons(patterns), node, env))
            }
            | ss::Type::Label(ss::Label(name, payload_ty)) => {
                let (terminals, passing) =
                    group.iter().copied().partition::<Vec<_>, _>(|selection| {
                        selection.candidate.steps.len() == consumed + 1
                    });
                let payload_node = payload_ty.unroll_k(tycker)?.subst_env_k(tycker, env)?;
                let mut members = Vec::new();
                for selection in terminals {
                    let checked = TyEnvT::new(env.clone(), selection.payload).tyck_k(
                        tycker,
                        PatternAction::ana(payload_node.into()).with_skolems(skolems.clone()),
                    )?;
                    let (payload_pattern, _) = checked.try_as_value(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?;
                    if !ValuePatternShape::is_irrefutable(tycker, payload_pattern) {
                        tycker.err_k(
                            TyckError::RefutableFieldProjectionPattern,
                            std::panic::Location::caller(),
                        )?
                    }
                    *env = checked.info;
                    opened.extend(checked.inner.opened);
                    tycker
                        .statics
                        .member_provenance
                        .record_projection(selection.source.into(), node.into());
                    members.push(payload_pattern);
                }
                if !passing.is_empty() {
                    members.push(Self::deep_node_pattern_k(
                        tycker,
                        env,
                        opened,
                        payload_node,
                        &passing,
                        consumed + 1,
                        skolems,
                    )?);
                }
                let inner = match ss::ConsN::from_vec(members) {
                    | Some(alias) => Alloc::alloc(tycker, ss::Alias(alias), payload_node, env),
                    | None => unreachable!("a label node receives at least one member"),
                };
                Ok(Alloc::alloc(tycker, ss::Named(name, inner), node, env))
            }
            | ss::Type::Exists(_) | ss::Type::ManifestKind(_) => {
                let mut nested = Self::open_k(tycker, env, node, skolems)?;
                let mut passing = Vec::new();
                for selection in group {
                    match &selection.candidate.terminal {
                        | PackageFieldTerminal::Entry { index } => {
                            Self::select_slot_k(
                                tycker,
                                &mut nested,
                                *index,
                                selection.payload,
                                skolems,
                            )?;
                        }
                        | PackageFieldTerminal::Label { .. } => passing.push(*selection),
                    }
                }
                let body_pattern = if passing.is_empty() {
                    Alloc::alloc(tycker, ss::Hole, nested.body, &nested.env)
                } else {
                    let body_node =
                        nested.body.unroll_k(tycker)?.subst_env_k(tycker, &nested.env)?;
                    Self::deep_node_pattern_k(
                        tycker,
                        &mut nested.env,
                        &mut nested.opened,
                        body_node,
                        &passing,
                        consumed + 1,
                        skolems,
                    )?
                };
                let static_patterns: Vec<ss::StaticPatId> =
                    nested.slots.iter().map(ExistentialProjectionSlot::pattern).collect();
                let pattern =
                    Alloc::alloc(tycker, ss::ConsN(static_patterns, body_pattern), node, env);
                let boundary_opened =
                    nested.slots.iter().filter(|slot| slot.opens_abstract()).count();
                if boundary_opened > 0 {
                    tycker
                        .statics
                        .package_pattern_opened_arity
                        .insert_new(pattern, boundary_opened);
                }
                *env = nested.env;
                opened.extend(nested.opened);
                Ok(pattern)
            }
            | _ => Ok(Alloc::alloc(tycker, ss::Hole, node, env)),
        }
    }

    /// Total abstract witnesses opened by the package patterns of a subtree.
    ///
    /// Witness recovery reads one arity per package-pattern node, so an outer
    /// group records its own opening plus the openings nested in its body.
    fn subtree_opened_abstracts(tycker: &Tycker<'_>, pattern: &ss::VPatId) -> usize {
        match tycker.statics.vpats[pattern].to_owned() {
            | ss::ValuePattern::SCons(ss::ConsN(_, body)) => {
                tycker
                    .statics
                    .package_pattern_opened_arity
                    .get(pattern)
                    .copied()
                    .unwrap_or_default()
                    + Self::subtree_opened_abstracts(tycker, &body)
            }
            | ss::ValuePattern::Named(ss::Named(_, inner)) => {
                Self::subtree_opened_abstracts(tycker, &inner)
            }
            | ss::ValuePattern::VCons(patterns) => {
                patterns.iter().map(|pattern| Self::subtree_opened_abstracts(tycker, pattern)).sum()
            }
            | ss::ValuePattern::Alias(ss::Alias(patterns)) => {
                patterns.iter().map(|pattern| Self::subtree_opened_abstracts(tycker, pattern)).sum()
            }
            | ss::ValuePattern::Hole(_)
            | ss::ValuePattern::Var(_)
            | ss::ValuePattern::Ctor(_)
            | ss::ValuePattern::Lit(_)
            | ss::ValuePattern::Triv(_)
            | ss::ValuePattern::View(_) => 0,
        }
    }

    fn whole_definition(tycker: &Tycker<'_>, pattern: ss::VPatId) -> Option<ss::DefId> {
        match tycker.statics.vpats[&pattern].to_owned() {
            | ss::ValuePattern::Var(definition) => Some(definition),
            | ss::ValuePattern::Named(ss::Named(_, inner)) => Self::whole_definition(tycker, inner),
            | ss::ValuePattern::Hole(_)
            | ss::ValuePattern::Ctor(_)
            | ss::ValuePattern::Lit(_)
            | ss::ValuePattern::Alias(_)
            | ss::ValuePattern::Triv(_)
            | ss::ValuePattern::VCons(_)
            | ss::ValuePattern::SCons(_)
            | ss::ValuePattern::View(_) => None,
        }
    }
}
