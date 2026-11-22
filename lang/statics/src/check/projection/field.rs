//! Resolve named fields and materialize their structural projection paths.

use super::*;

impl FieldProjectionResolver {
    pub(super) fn field_index(
        tycker: &mut Tycker<'_>, root: ss::TypeId, env: &ss::TyEnv,
    ) -> Result<std::sync::Arc<FieldIndex>> {
        let shared_env = std::sync::Arc::new(env.clone());
        let key = (root, shared_env.clone());
        if let Some(index) = tycker.field_materializations.field_index(&key) {
            return Ok(index);
        }
        let mut index = FieldIndex::default();
        Self::collect_value_fields(
            tycker,
            DeferredEnvType::with_shared_environment(root, shared_env.clone()),
            &[],
            &mut rustc_hash::FxHashSet::default(),
            &mut index.value_fields,
        )?;
        Self::collect_package_fields(
            tycker,
            DeferredEnvType::with_shared_environment(root, shared_env),
            &[],
            false,
            false,
            &mut rustc_hash::FxHashSet::default(),
            &mut index.package_fields,
        )?;
        let index = std::sync::Arc::new(index);
        tycker.field_materializations.insert_field_index(key, index.clone());
        Ok(index)
    }

    fn collect_value_fields(
        tycker: &mut Tycker<'_>, current: DeferredEnvType, route: &[DeferredValueFieldStep],
        active: &mut rustc_hash::FxHashSet<DeferredEnvIdentity>,
        value_fields: &mut rustc_hash::FxHashMap<FieldName, Vec<DeferredValueFieldCandidate>>,
    ) -> Result<()> {
        zydeco_foundation::stack::maybe_grow(|| {
            let view = current.reveal(tycker)?;
            let identity = view.identity();
            if !active.insert(identity) {
                return Ok(());
            }
            match view {
                | DeferredEnvTypeView::Label { name: found, whole, projected } => {
                    let route = route
                        .iter()
                        .cloned()
                        .chain([DeferredValueFieldStep::Named { name: found, whole }])
                        .collect::<Vec<_>>();
                    value_fields.entry(found).or_default().push(DeferredValueFieldCandidate {
                        route: route.clone(),
                        projected: projected.clone(),
                    });
                    Self::collect_value_fields(tycker, projected, &route, active, value_fields)?;
                }
                | DeferredEnvTypeView::Product { whole, .. } => {
                    let components = Self::product_components(tycker, whole.clone())?;
                    for (position, component) in components.into_iter().enumerate() {
                        let route = route
                            .iter()
                            .cloned()
                            .chain([DeferredValueFieldStep::Product {
                                product: whole.clone(),
                                position,
                            }])
                            .collect::<Vec<_>>();
                        Self::collect_value_fields(
                            tycker,
                            component,
                            &route,
                            active,
                            value_fields,
                        )?;
                    }
                }
                | _ => {}
            }
            active.remove(&identity);
            Ok(())
        })
    }

    fn product_components(
        tycker: &mut Tycker<'_>, product: DeferredEnvType,
    ) -> Result<Vec<DeferredEnvType>> {
        match product.reveal(tycker)? {
            | DeferredEnvTypeView::Product { components, .. } => Ok(components),
            | view => Ok(vec![view.into_whole()]),
        }
    }

    /// Collect every field occurrence that at least one package telescope separates
    /// from the root. Direct named and product matches remain the ordinary value
    /// index's responsibility, so the two searches never report the same occurrence.
    fn collect_package_fields(
        tycker: &mut Tycker<'_>, current: DeferredEnvType, steps: &[PackageFieldStep],
        crossed: bool, sealed: bool, active: &mut rustc_hash::FxHashSet<DeferredEnvIdentity>,
        package_fields: &mut rustc_hash::FxHashMap<FieldName, Vec<PackageFieldCandidate>>,
    ) -> Result<()> {
        zydeco_foundation::stack::maybe_grow(|| {
            let view = current.reveal(tycker)?;
            let identity = view.identity();
            if !active.insert(identity) {
                return Ok(());
            }
            match view {
                | DeferredEnvTypeView::Label { name: found, whole: _, projected } => {
                    let steps = steps
                        .iter()
                        .cloned()
                        .chain([PackageFieldStep::Named { name: found }])
                        .collect::<Vec<_>>();
                    if crossed {
                        package_fields.entry(found).or_default().push(PackageFieldCandidate {
                            steps: steps.clone(),
                            terminal: PackageFieldTerminal::Label { name: found },
                            sealed,
                        });
                    }
                    Self::collect_package_fields(
                        tycker,
                        projected,
                        &steps,
                        crossed,
                        sealed,
                        active,
                        package_fields,
                    )?;
                }
                | DeferredEnvTypeView::Product { whole, .. } => {
                    let components = Self::product_components(tycker, whole)?;
                    for (position, component) in components.into_iter().enumerate() {
                        let steps = steps
                            .iter()
                            .cloned()
                            .chain([PackageFieldStep::Product { position }])
                            .collect::<Vec<_>>();
                        Self::collect_package_fields(
                            tycker,
                            component,
                            &steps,
                            crossed,
                            sealed,
                            active,
                            package_fields,
                        )?;
                    }
                }
                | DeferredEnvTypeView::Package { whole, .. } => {
                    Self::collect_package_entry_fields(
                        tycker,
                        whole,
                        steps,
                        sealed,
                        active,
                        package_fields,
                    )?;
                }
                | DeferredEnvTypeView::Other(_) => {}
            }
            active.remove(&identity);
            Ok(())
        })
    }

    /// Walk one package telescope, indexing every named binder and continuing the search
    /// through the telescope body.
    ///
    /// The walk stays read-only: binder field names do not depend on witness
    /// substitution, and the elaboration re-opens the telescope with fresh or
    /// canonical witnesses once a selection is unique.
    fn collect_package_entry_fields(
        tycker: &mut Tycker<'_>, package: DeferredEnvType, steps: &[PackageFieldStep],
        sealed: bool, active: &mut rustc_hash::FxHashSet<DeferredEnvIdentity>,
        package_fields: &mut rustc_hash::FxHashMap<FieldName, Vec<PackageFieldCandidate>>,
    ) -> Result<()> {
        let steps = steps.iter().cloned().chain([PackageFieldStep::Package]).collect::<Vec<_>>();
        let mut sealed = sealed;
        let mut current = package;
        let mut visited = Vec::new();
        let mut index = 0usize;
        loop {
            let view = current.reveal(tycker)?;
            let head = match view {
                | DeferredEnvTypeView::Package { whole, head } => {
                    if index > 0 {
                        let identity =
                            DeferredEnvIdentity { root: whole.root, pending: whole.pending };
                        if !active.insert(identity) {
                            for identity in visited.drain(..) {
                                active.remove(&identity);
                            }
                            return Ok(());
                        }
                        visited.push(identity);
                    }
                    current = whole;
                    head
                }
                | view => {
                    Self::collect_package_fields(
                        tycker,
                        view.into_whole(),
                        &steps,
                        true,
                        sealed,
                        active,
                        package_fields,
                    )?;
                    for identity in visited.drain(..) {
                        active.remove(&identity);
                    }
                    return Ok(());
                }
            };
            match head {
                | DeferredPackageHead::ManifestKind(kind) => {
                    if let Some(found) =
                        ExistentialProjectionPattern::kind_field_name(tycker, kind.binder)
                    {
                        package_fields.entry(found).or_default().push(PackageFieldCandidate {
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
                            if let Some(found) = found {
                                package_fields.entry(found).or_default().push(
                                    PackageFieldCandidate {
                                        steps: steps.clone(),
                                        terminal: PackageFieldTerminal::Entry { index },
                                        sealed: true,
                                    },
                                );
                            }
                            sealed = true;
                        }
                        | ss::ExistsMode::Manifest(_) => {
                            if let Some(found) = found {
                                package_fields.entry(found).or_default().push(
                                    PackageFieldCandidate {
                                        steps: steps.clone(),
                                        terminal: PackageFieldTerminal::Entry { index },
                                        sealed,
                                    },
                                );
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
    fn materialize_package_candidate(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, root: ss::TypeId,
        candidate: &PackageFieldCandidate,
    ) -> Result<ValueFieldCandidate> {
        let mut walk_env = env.clone();
        let mut current = root.unroll(tycker)?.subst_env(tycker, &walk_env)?;
        let mut route = Vec::new();
        for step in &candidate.steps {
            match step {
                | PackageFieldStep::Product { position } => {
                    let ss::Type::Prod(ss::Prod(components)) =
                        tycker.type_filled(&current)?.to_owned()
                    else {
                        unreachable!("a package route traverses declared products");
                    };
                    route.push(ValueFieldStep::Product {
                        product: current,
                        components: components.clone(),
                        position: *position,
                    });
                    current = components[*position].unroll(tycker)?.subst_env(tycker, &walk_env)?;
                }
                | PackageFieldStep::Named { .. } => {
                    let ss::Type::Label(ss::Label(name, payload)) =
                        tycker.type_filled(&current)?.to_owned()
                    else {
                        unreachable!("a package route traverses declared labels");
                    };
                    route.push(ValueFieldStep::Named { name, whole: current });
                    current = payload.unroll(tycker)?.subst_env(tycker, &walk_env)?;
                }
                | PackageFieldStep::Package => {
                    current = Self::manifest_telescope_body(tycker, &mut walk_env, current)?;
                }
            }
        }
        Ok(ValueFieldCandidate { route, projected: current })
    }

    /// Descend through a manifest-only telescope, substituting disclosed definitions.
    fn manifest_telescope_body(
        tycker: &mut Tycker<'_>, walk_env: &mut ss::TyEnv, package: ss::TypeId,
    ) -> Result<ss::TypeId> {
        let mut body = DeferredTelescopeType::new(package);
        loop {
            match body.clone().with_environment(walk_env).reveal(tycker)? {
                | DeferredTelescopeView::ManifestKind { binder, definition, body: next } => {
                    *walk_env = TyEnvT::new(walk_env.clone(), Assign(binder, definition))
                        .tyck(tycker, ())?
                        .info;
                    body = next;
                }
                | DeferredTelescopeView::Exists {
                    binder,
                    mode: DeferredTelescopeExistsMode::Manifest(definition),
                    body: next,
                } => {
                    let definition = definition.materialize(tycker)?;
                    let payload_kind = binder.payload_kind(tycker);
                    let definition_kind = tycker.statics.type_kind(definition);
                    Lub::lub(payload_kind, definition_kind, tycker)?;
                    let full_payload = binder.pattern.introduce_payload(tycker, definition);
                    let full_payload = full_payload?;
                    *walk_env = TyEnvT::new(walk_env.clone(), Assign(binder.pattern, full_payload))
                        .tyck(tycker, ())?
                        .info;
                    body = next.with_abstract(binder.witness, definition);
                }
                | DeferredTelescopeView::Exists {
                    mode: DeferredTelescopeExistsMode::Abstract,
                    ..
                } => unreachable!("a term projection crosses only manifest telescopes"),
                | DeferredTelescopeView::Other(body) => return body.materialize(tycker),
            }
        }
    }

    /// Resolve one term projection, searching direct structure and manifest packages.
    ///
    /// Field names behind abstract witnesses count for uniqueness but cannot be
    /// selected by a term, and package telescope entries name witnesses rather
    /// than runtime components; both report why a projection pattern is required.
    pub(in crate::check) fn value_term(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, root: ss::TypeId, field: &FieldName,
    ) -> Result<ValueFieldCandidate> {
        let index = Self::field_index(tycker, root, env)?;
        let values = index.value_fields.get(field).map_or(&[][..], Vec::as_slice);
        let packages = index.package_fields.get(field).map_or(&[][..], Vec::as_slice);
        match (values, packages) {
            | ([], []) => tycker.err(TyckError::MissingNamedField { field: *field, found: root }),
            | ([candidate], []) => Ok(candidate.clone().materialize(tycker)?),
            | ([], [candidate]) => {
                if candidate.sealed {
                    return tycker.err(TyckError::SealedNamedField { field: *field, found: root });
                }
                match &candidate.terminal {
                    | PackageFieldTerminal::Entry { .. } => {
                        tycker.err(TyckError::StaticNamedField { field: *field, found: root })
                    }
                    | PackageFieldTerminal::Label { .. } => {
                        Self::materialize_package_candidate(tycker, env, root, candidate)
                    }
                }
            }
            | _ => tycker.err(TyckError::DuplicateNamedField { field: *field, found: root }),
        }
    }

    pub(in crate::check) fn r#type(
        tycker: &mut Tycker<'_>, root: ss::KindId, field: &FieldName,
    ) -> Result<TypeFieldCandidate> {
        let candidates =
            Self::type_candidates(tycker, root, field, &[], &mut std::collections::HashSet::new())?;
        match candidates.as_slice() {
            | [] => tycker.err(TyckError::MissingNamedTypeField { field: *field, found: root }),
            | [candidate] => Ok(candidate.clone()),
            | _ => tycker.err(TyckError::AmbiguousNamedTypeField { field: *field, found: root }),
        }
    }

    fn type_candidates(
        tycker: &mut Tycker<'_>, current: ss::KindId, field: &FieldName, path: &[TypeFieldStep],
        active: &mut std::collections::HashSet<ss::KindId>,
    ) -> Result<Vec<TypeFieldCandidate>> {
        if !active.insert(current) {
            return Ok(Vec::new());
        }
        let candidates = match tycker.kind_filled(&current)?.to_owned() {
            | ss::Kind::Label(ss::Label(found, projected)) => {
                let path = path
                    .iter()
                    .cloned()
                    .chain([TypeFieldStep { name: found, whole: current, projected }])
                    .collect::<Vec<_>>();
                let direct =
                    (found == *field).then(|| TypeFieldCandidate { path: path.clone(), projected });
                let nested = Self::type_candidates(tycker, projected, field, &path, active)?;
                direct.into_iter().chain(nested).collect()
            }
            | _ => Vec::new(),
        };
        active.remove(&current);
        Ok(candidates)
    }

    pub(in crate::check) fn project_type(
        tycker: &mut Tycker<'_>, head: ss::TypeId, candidate: TypeFieldCandidate,
        projected: ss::KindId,
    ) -> Result<ss::TypeId> {
        let final_step = candidate.path.len() - 1;
        candidate.path.into_iter().enumerate().try_fold(
            head,
            |head, (index, TypeFieldStep { name, projected: step_kind, .. })| {
                let step_kind = if index == final_step { projected } else { step_kind };
                head.project_named(tycker, &name, step_kind)
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
        tycker: &mut Tycker<'_>, _env: &ss::TyEnv, root: ss::TypeId,
        candidate: ValueFieldCandidate, payload: ss::VPatId,
    ) -> ss::VPatId {
        candidate.route.into_iter().enumerate().rev().fold(payload, |payload, (index, step)| {
            match step {
                | ValueFieldStep::Named { name, whole } => {
                    let annotation = if index == 0 { root } else { whole };
                    Alloc::alloc(tycker, ss::Named(name, payload), annotation, &())
                }
                | ValueFieldStep::Product { product, components, position } => {
                    let patterns = components
                        .into_iter()
                        .enumerate()
                        .map(|(component_index, component)| {
                            if component_index == position {
                                payload
                            } else {
                                Alloc::alloc(tycker, ss::Hole, component, &())
                            }
                        })
                        .collect::<Vec<_>>();
                    let annotation = if index == 0 { root } else { product };
                    Alloc::alloc(tycker, ss::ValuePattern::VCons(patterns), annotation, &())
                }
            }
        })
    }

    pub(in crate::check) fn type_pattern(
        tycker: &mut Tycker<'_>, _env: &ss::TyEnv, root: ss::KindId, candidate: TypeFieldCandidate,
        payload: ss::TPatId,
    ) -> ss::TPatId {
        candidate.path.into_iter().enumerate().rev().fold(
            payload,
            |payload, (index, TypeFieldStep { name, whole, .. })| {
                let annotation = if index == 0 { root } else { whole };
                Alloc::alloc(tycker, ss::Named(name, payload), annotation, &())
            },
        )
    }
}
