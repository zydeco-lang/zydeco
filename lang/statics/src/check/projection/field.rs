//! Resolve named fields and materialize their structural projection paths.

use super::*;

impl FieldProjectionResolver {
    pub(in crate::check) fn record_value_origin(
        tycker: &mut Tycker<'_>, source: su::EntityId, candidate: &ValueFieldCandidate,
    ) {
        if let Some(ValueFieldStep::Named { whole, .. }) = candidate.route.last() {
            tycker.statics.member_provenance.record_projection(source, (*whole).into());
        }
    }

    pub(in crate::check) fn record_type_origin(
        tycker: &mut Tycker<'_>, source: su::EntityId, candidate: &TypeFieldCandidate,
    ) {
        if let Some(step) = candidate.path.last() {
            tycker.statics.member_provenance.record_projection(source, step.whole.into());
        }
    }

    pub(super) fn value_candidates_k(
        tycker: &mut Tycker<'_>, current: DeferredEnvType, field: &FieldName,
        route: &[DeferredValueFieldStep],
        active: &mut std::collections::HashSet<DeferredEnvIdentity>,
    ) -> ResultKont<Vec<DeferredValueFieldCandidate>> {
        let view = current.reveal_k(tycker)?;
        let identity = view.identity();
        if !active.insert(identity) {
            return Ok(Vec::new());
        }
        let candidates = match view {
            | DeferredEnvTypeView::Label { name: found, whole, projected } => {
                let route = route
                    .iter()
                    .cloned()
                    .chain([DeferredValueFieldStep::Named { name: found.clone(), whole }])
                    .collect::<Vec<_>>();
                let direct = (found == *field).then(|| DeferredValueFieldCandidate {
                    route: route.clone(),
                    projected: projected.clone(),
                });
                let nested = Self::value_candidates_k(tycker, projected, field, &route, active)?;
                direct.into_iter().chain(nested).collect()
            }
            | DeferredEnvTypeView::Product { whole, .. } => {
                let components = Self::product_components_k(tycker, whole.clone())?;
                let branches = components
                    .iter()
                    .cloned()
                    .enumerate()
                    .map(|(position, component)| {
                        let route = route
                            .iter()
                            .cloned()
                            .chain([DeferredValueFieldStep::Product {
                                product: whole.clone(),
                                position,
                            }])
                            .collect::<Vec<_>>();
                        Self::value_candidates_k(tycker, component, field, &route, active)
                    })
                    .collect::<ResultKont<Vec<_>>>()?;
                branches.into_iter().flatten().collect()
            }
            | _ => Vec::new(),
        };
        active.remove(&identity);
        Ok(candidates)
    }

    fn product_components_k(
        tycker: &mut Tycker<'_>, product: DeferredEnvType,
    ) -> ResultKont<Vec<DeferredEnvType>> {
        match product.reveal_k(tycker)? {
            | DeferredEnvTypeView::Product { components, .. } => Ok(components),
            | view => Ok(vec![view.into_whole()]),
        }
    }

    /// Search the complete receiver structure for field occurrences that at least one
    /// package telescope separates from the root. Direct named and product matches
    /// remain the ordinary value resolver's responsibility, so the two searches never
    /// report the same occurrence.
    pub(super) fn package_candidates_k(
        tycker: &mut Tycker<'_>, current: DeferredEnvType, field: &FieldName,
        steps: &[PackageFieldStep], crossed: bool, sealed: bool,
        active: &mut std::collections::HashSet<DeferredEnvIdentity>,
    ) -> ResultKont<Vec<PackageFieldCandidate>> {
        let view = current.reveal_k(tycker)?;
        let identity = view.identity();
        if !active.insert(identity) {
            return Ok(Vec::new());
        }
        let candidates = match view {
            | DeferredEnvTypeView::Label { name: found, whole: _, projected } => {
                let steps = steps
                    .iter()
                    .cloned()
                    .chain([PackageFieldStep::Named { name: found.clone() }])
                    .collect::<Vec<_>>();
                let direct = (found == *field && crossed).then(|| PackageFieldCandidate {
                    steps: steps.clone(),
                    terminal: PackageFieldTerminal::Label { name: found.clone() },
                    sealed,
                });
                let nested = Self::package_candidates_k(
                    tycker, projected, field, &steps, crossed, sealed, active,
                )?;
                direct.into_iter().chain(nested).collect()
            }
            | DeferredEnvTypeView::Product { whole, .. } => {
                let components = Self::product_components_k(tycker, whole.clone())?;
                let branches = components
                    .iter()
                    .cloned()
                    .enumerate()
                    .map(|(position, component)| {
                        let steps = steps
                            .iter()
                            .cloned()
                            .chain([PackageFieldStep::Product { position }])
                            .collect::<Vec<_>>();
                        Self::package_candidates_k(
                            tycker, component, field, &steps, crossed, sealed, active,
                        )
                    })
                    .collect::<ResultKont<Vec<_>>>()?;
                branches.into_iter().flatten().collect()
            }
            | DeferredEnvTypeView::Package { whole, .. } => {
                Self::package_entry_candidates_k(tycker, whole, field, steps, sealed, active)?
            }
            | DeferredEnvTypeView::Other(_) => Vec::new(),
        };
        active.remove(&identity);
        Ok(candidates)
    }

    /// Walk one package telescope, contributing a candidate per named binder and
    /// continuing the search through the telescope body.
    ///
    /// The walk stays read-only: binder field names do not depend on witness
    /// substitution, and the elaboration re-opens the telescope with fresh or
    /// canonical witnesses once a selection is unique.
    fn package_entry_candidates_k(
        tycker: &mut Tycker<'_>, package: DeferredEnvType, field: &FieldName,
        steps: &[PackageFieldStep], sealed: bool,
        active: &mut std::collections::HashSet<DeferredEnvIdentity>,
    ) -> ResultKont<Vec<PackageFieldCandidate>> {
        let steps = steps.iter().cloned().chain([PackageFieldStep::Package]).collect::<Vec<_>>();
        let mut candidates = Vec::new();
        let mut sealed = sealed;
        let mut current = package;
        let mut visited = Vec::new();
        let mut index = 0usize;
        loop {
            let view = current.reveal_k(tycker)?;
            let head = match view {
                | DeferredEnvTypeView::Package { whole, head } => {
                    if index > 0 {
                        let identity =
                            DeferredEnvIdentity { root: whole.root, pending: whole.pending };
                        if !active.insert(identity) {
                            return Ok(candidates);
                        }
                        visited.push(identity);
                    }
                    current = whole;
                    head
                }
                | view => {
                    let body = Self::package_candidates_k(
                        tycker,
                        view.into_whole(),
                        field,
                        &steps,
                        true,
                        sealed,
                        active,
                    )?;
                    candidates.extend(body);
                    visited.drain(..).for_each(|identity| {
                        active.remove(&identity);
                    });
                    return Ok(candidates);
                }
            };
            match head {
                | DeferredPackageHead::ManifestKind(kind) => {
                    if ExistentialProjectionPattern::kind_field_name(tycker, kind.binder).as_ref()
                        == Some(field)
                    {
                        candidates.push(PackageFieldCandidate {
                            steps: steps.clone(),
                            terminal: PackageFieldTerminal::Entry { index },
                            sealed,
                        });
                    }
                    current = current.descend(kind.body);
                }
                | DeferredPackageHead::Exists(exists) => {
                    let ss::Exists { binder, mode, body } = exists;
                    let found = ExistentialProjectionPattern::type_field_name(tycker, &binder);
                    match mode {
                        | ss::ExistsMode::Abstract => {
                            if found.as_ref() == Some(field) {
                                candidates.push(PackageFieldCandidate {
                                    steps: steps.clone(),
                                    terminal: PackageFieldTerminal::Entry { index },
                                    sealed: true,
                                });
                            }
                            sealed = true;
                        }
                        | ss::ExistsMode::Manifest(_) => {
                            if found.as_ref() == Some(field) {
                                candidates.push(PackageFieldCandidate {
                                    steps: steps.clone(),
                                    terminal: PackageFieldTerminal::Entry { index },
                                    sealed,
                                });
                            }
                        }
                    }
                    current = current.descend(body);
                }
            }
            index += 1;
        }
    }

    /// Materialize a package-crossing route for term projection.
    ///
    /// The route crosses only manifest telescopes, whose disclosed definitions
    /// substitute while descending, so the physical projection stays a chain of
    /// product positions while named wrappers record provenance.
    fn materialize_package_candidate_k(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, root: ss::TypeId,
        candidate: &PackageFieldCandidate,
    ) -> ResultKont<ValueFieldCandidate> {
        let mut walk_env = env.clone();
        let mut current = root.unroll_k(tycker)?.subst_env_k(tycker, &walk_env)?;
        let mut route = Vec::new();
        for step in &candidate.steps {
            match step {
                | PackageFieldStep::Product { position } => {
                    let ss::Type::Prod(ss::Prod(components)) =
                        tycker.type_filled_k(&current)?.to_owned()
                    else {
                        unreachable!("a package route traverses declared products");
                    };
                    route.push(ValueFieldStep::Product {
                        product: current,
                        components: components.clone(),
                        position: *position,
                    });
                    current =
                        components[*position].unroll_k(tycker)?.subst_env_k(tycker, &walk_env)?;
                }
                | PackageFieldStep::Named { .. } => {
                    let ss::Type::Label(ss::Label(name, payload)) =
                        tycker.type_filled_k(&current)?.to_owned()
                    else {
                        unreachable!("a package route traverses declared labels");
                    };
                    route.push(ValueFieldStep::Named { name, whole: current });
                    current = payload.unroll_k(tycker)?.subst_env_k(tycker, &walk_env)?;
                }
                | PackageFieldStep::Package => {
                    current = Self::manifest_telescope_body_k(tycker, &mut walk_env, current)?;
                }
            }
        }
        Ok(ValueFieldCandidate { route, projected: current })
    }

    /// Descend through a manifest-only telescope, substituting disclosed definitions.
    fn manifest_telescope_body_k(
        tycker: &mut Tycker<'_>, walk_env: &mut ss::TyEnv, package: ss::TypeId,
    ) -> ResultKont<ss::TypeId> {
        let mut body = DeferredTelescopeType::new(package);
        loop {
            match body.clone().with_environment(walk_env).reveal_k(tycker)? {
                | DeferredTelescopeView::ManifestKind { binder, definition, body: next } => {
                    *walk_env = TyEnvT::new(walk_env.clone(), Assign(binder, definition))
                        .tyck_k(tycker, ())?
                        .info;
                    body = next;
                }
                | DeferredTelescopeView::Exists {
                    binder,
                    mode: DeferredTelescopeExistsMode::Manifest(definition),
                    body: next,
                } => {
                    let definition = definition.materialize_k(tycker)?;
                    let payload_kind = binder.payload_kind(tycker);
                    let definition_kind = tycker.statics.type_kind(definition);
                    Lub::lub_k(payload_kind, definition_kind, tycker)?;
                    let full_payload = binder.pattern.introduce_payload(tycker, definition);
                    let full_payload = tycker.err_p_to_k(full_payload)?;
                    *walk_env = TyEnvT::new(walk_env.clone(), Assign(binder.pattern, full_payload))
                        .tyck_k(tycker, ())?
                        .info;
                    body = next.with_abstract(binder.witness, definition);
                }
                | DeferredTelescopeView::Exists {
                    mode: DeferredTelescopeExistsMode::Abstract,
                    ..
                } => unreachable!("a term projection crosses only manifest telescopes"),
                | DeferredTelescopeView::Other(body) => return body.materialize_k(tycker),
            }
        }
    }

    /// Resolve one term projection, searching direct structure and manifest packages.
    ///
    /// Field names behind abstract witnesses count for uniqueness but cannot be
    /// selected by a term, and package telescope entries name witnesses rather
    /// than runtime components; both report why a projection pattern is required.
    pub(in crate::check) fn value_term_k(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, root: ss::TypeId, field: &FieldName,
    ) -> ResultKont<ValueFieldCandidate> {
        let values = Self::value_candidates_k(
            tycker,
            DeferredEnvType::with_environment(root, env),
            field,
            &[],
            &mut std::collections::HashSet::new(),
        )?;
        let packages = Self::package_candidates_k(
            tycker,
            DeferredEnvType::with_environment(root, env),
            field,
            &[],
            false,
            false,
            &mut std::collections::HashSet::new(),
        )?;
        match (values.as_slice(), packages.as_slice()) {
            | ([], []) => tycker.err_k(
                TyckError::MissingNamedField { field: field.clone(), found: root },
                std::panic::Location::caller(),
            ),
            | ([candidate], []) => Ok(candidate.clone().materialize_k(tycker)?),
            | ([], [candidate]) => {
                if candidate.sealed {
                    return tycker.err_k(
                        TyckError::SealedNamedField { field: field.clone(), found: root },
                        std::panic::Location::caller(),
                    );
                }
                match &candidate.terminal {
                    | PackageFieldTerminal::Entry { .. } => tycker.err_k(
                        TyckError::StaticNamedField { field: field.clone(), found: root },
                        std::panic::Location::caller(),
                    ),
                    | PackageFieldTerminal::Label { .. } => {
                        Self::materialize_package_candidate_k(tycker, env, root, candidate)
                    }
                }
            }
            | _ => tycker.err_k(
                TyckError::DuplicateNamedField { field: field.clone(), found: root },
                std::panic::Location::caller(),
            ),
        }
    }

    pub(in crate::check) fn r#type(
        tycker: &mut Tycker<'_>, root: ss::KindId, field: &FieldName,
    ) -> ResultKont<TypeFieldCandidate> {
        let candidates = Self::type_candidates_k(
            tycker,
            root,
            field,
            &[],
            &mut std::collections::HashSet::new(),
        )?;
        match candidates.as_slice() {
            | [] => tycker.err_k(
                TyckError::MissingNamedTypeField { field: field.clone(), found: root },
                std::panic::Location::caller(),
            ),
            | [candidate] => Ok(candidate.clone()),
            | _ => tycker.err_k(
                TyckError::AmbiguousNamedTypeField { field: field.clone(), found: root },
                std::panic::Location::caller(),
            ),
        }
    }

    fn type_candidates_k(
        tycker: &mut Tycker<'_>, current: ss::KindId, field: &FieldName, path: &[TypeFieldStep],
        active: &mut std::collections::HashSet<ss::KindId>,
    ) -> ResultKont<Vec<TypeFieldCandidate>> {
        if !active.insert(current) {
            return Ok(Vec::new());
        }
        let candidates = match tycker.kind_filled_k(&current)?.to_owned() {
            | ss::Kind::Label(ss::Label(found, projected)) => {
                let path = path
                    .iter()
                    .cloned()
                    .chain([TypeFieldStep { name: found.clone(), whole: current, projected }])
                    .collect::<Vec<_>>();
                let direct =
                    (found == *field).then(|| TypeFieldCandidate { path: path.clone(), projected });
                let nested = Self::type_candidates_k(tycker, projected, field, &path, active)?;
                direct.into_iter().chain(nested).collect()
            }
            | _ => Vec::new(),
        };
        active.remove(&current);
        Ok(candidates)
    }

    pub(in crate::check) fn project_type_k(
        tycker: &mut Tycker<'_>, head: ss::TypeId, candidate: TypeFieldCandidate,
        projected: ss::KindId,
    ) -> ResultKont<ss::TypeId> {
        let final_step = candidate.path.len() - 1;
        candidate.path.into_iter().enumerate().try_fold(
            head,
            |head, (index, TypeFieldStep { name, projected: step_kind, .. })| {
                let step_kind = if index == final_step { projected } else { step_kind };
                let result = head.project_named(tycker, &name, step_kind);
                tycker.err_p_to_k(result)
            },
        )
    }

    pub(in crate::check) fn value_target(candidate: &ValueFieldCandidate) -> ss::ProjTarget {
        let products = candidate
            .route
            .iter()
            .filter_map(|step| match step {
                | ValueFieldStep::Named { .. } => None,
                | ValueFieldStep::Product { product, position, .. } => {
                    Some(ss::ProductProjection { product: *product, position: *position })
                }
            })
            .collect();
        ss::ProjTarget { products }
    }

    pub(super) fn value_pattern(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, root: ss::TypeId, candidate: ValueFieldCandidate,
        payload: ss::VPatId,
    ) -> ss::VPatId {
        candidate.route.into_iter().enumerate().rev().fold(payload, |payload, (index, step)| {
            match step {
                | ValueFieldStep::Named { name, whole } => {
                    let annotation = if index == 0 { root } else { whole };
                    Alloc::alloc(tycker, ss::Named(name, payload), annotation, env)
                }
                | ValueFieldStep::Product { product, components, position } => {
                    let patterns = components
                        .into_iter()
                        .enumerate()
                        .map(|(component_index, component)| {
                            if component_index == position {
                                payload
                            } else {
                                Alloc::alloc(tycker, ss::Hole, component, env)
                            }
                        })
                        .collect::<Vec<_>>();
                    let annotation = if index == 0 { root } else { product };
                    Alloc::alloc(tycker, ss::ValuePattern::VCons(patterns), annotation, env)
                }
            }
        })
    }

    pub(in crate::check) fn type_pattern(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, root: ss::KindId, candidate: TypeFieldCandidate,
        payload: ss::TPatId,
    ) -> ss::TPatId {
        candidate.path.into_iter().enumerate().rev().fold(
            payload,
            |payload, (index, TypeFieldStep { name, whole, .. })| {
                let annotation = if index == 0 { root } else { whole };
                Alloc::alloc(tycker, ss::Named(name, payload), annotation, env)
            },
        )
    }
}
