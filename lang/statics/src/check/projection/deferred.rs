//! Defer environment substitution and cache materialization during field search.

use super::*;

impl DeferredEnvMaterializationKey {
    fn new(root: ss::TypeId, environment: &ss::TyEnv, operation: DeferredEnvOperation) -> Self {
        Self { root, environment: environment.clone(), operation }
    }
}

impl DeferredEnvMaterializationCache {
    fn get(&self, key: &DeferredEnvMaterializationKey) -> Option<ss::TypeId> {
        self.entries.get(key).copied()
    }

    fn insert(&mut self, key: DeferredEnvMaterializationKey, materialized: ss::TypeId) {
        self.entries.insert(key, materialized);
    }

    pub(in crate::check) fn clear(&mut self) {
        self.entries.clear();
    }
}

impl DeferredEnvType {
    pub(in crate::check) fn with_environment(root: ss::TypeId, environment: &ss::TyEnv) -> Self {
        Self {
            root,
            environment: environment.clone(),
            pending: Some(DeferredEnvOperation::UnrollThenSubstitute),
        }
    }

    fn materialized(root: ss::TypeId, environment: ss::TyEnv) -> Self {
        Self { root, environment, pending: None }
    }

    pub(in crate::check) fn descend(&self, root: ss::TypeId) -> Self {
        let pending = self.pending.map(|_| DeferredEnvOperation::Substitute);
        Self { root, environment: self.environment.clone(), pending }
    }

    pub(in crate::check) fn materialize_k(
        &self, tycker: &mut Tycker<'_>,
    ) -> ResultKont<ss::TypeId> {
        let Some(operation) = self.pending else { return Ok(self.root) };
        let key = DeferredEnvMaterializationKey::new(self.root, &self.environment, operation);
        if let Some(materialized) = tycker.field_materializations.get(&key) {
            return Ok(materialized);
        }
        let root = match operation {
            | DeferredEnvOperation::UnrollThenSubstitute => self.root.unroll_k(tycker)?,
            | DeferredEnvOperation::Substitute => self.root,
        };
        let materialized = root.subst_env_k(tycker, &self.environment)?;
        tycker.field_materializations.insert(key, materialized);
        Ok(materialized)
    }

    pub(super) fn reveal_k(self, tycker: &mut Tycker<'_>) -> ResultKont<DeferredEnvTypeView> {
        match tycker.statics.types_pre[&self.root].to_owned() {
            | ss::Fillable::Fill(_) => {
                let ty = tycker.type_filled_k(&self.root)?.to_owned();
                Ok(Self::materialized(self.root, self.environment).view(ty))
            }
            | ss::Fillable::Done(
                ss::Type::Var(_) | ss::Type::Abst(_) | ss::Type::App(_) | ss::Type::Proj(_),
            ) => {
                let needs_search_step =
                    self.pending != Some(DeferredEnvOperation::UnrollThenSubstitute);
                let mut root = self.materialize_k(tycker)?;
                if needs_search_step {
                    root = Self {
                        root,
                        environment: self.environment.clone(),
                        pending: Some(DeferredEnvOperation::UnrollThenSubstitute),
                    }
                    .materialize_k(tycker)?;
                }
                let ty = tycker.type_filled_k(&root)?.to_owned();
                Ok(Self::materialized(root, self.environment).view(ty))
            }
            | ss::Fillable::Done(ty) => Ok(self.view(ty)),
        }
    }

    fn view(self, ty: ss::Type) -> DeferredEnvTypeView {
        match ty {
            | ss::Type::Label(ss::Label(name, projected)) => {
                let projected = self.descend(projected);
                DeferredEnvTypeView::Label { name, whole: self, projected }
            }
            | ss::Type::Prod(ss::Prod(components)) => {
                let components =
                    components.into_iter().map(|component| self.descend(component)).collect();
                DeferredEnvTypeView::Product { whole: self, components }
            }
            | ss::Type::Exists(exists) => DeferredEnvTypeView::Package {
                whole: self,
                head: DeferredPackageHead::Exists(*exists),
            },
            | ss::Type::ManifestKind(kind) => DeferredEnvTypeView::Package {
                whole: self,
                head: DeferredPackageHead::ManifestKind(kind),
            },
            | _ => DeferredEnvTypeView::Other(self),
        }
    }
}

impl DeferredEnvTypeView {
    pub(super) fn identity(&self) -> DeferredEnvIdentity {
        match self {
            | Self::Label { whole, .. }
            | Self::Product { whole, .. }
            | Self::Package { whole, .. }
            | Self::Other(whole) => {
                DeferredEnvIdentity { root: whole.root, pending: whole.pending }
            }
        }
    }

    pub(super) fn into_whole(self) -> DeferredEnvType {
        match self {
            | Self::Label { whole, .. }
            | Self::Product { whole, .. }
            | Self::Package { whole, .. }
            | Self::Other(whole) => whole,
        }
    }
}

impl DeferredValueFieldCandidate {
    pub(super) fn materialize_k(self, tycker: &mut Tycker<'_>) -> ResultKont<ValueFieldCandidate> {
        let mut current = None;
        let route = self
            .route
            .into_iter()
            .map(|step| match step {
                | DeferredValueFieldStep::Named { name, whole } => {
                    let materialized = match current {
                        | Some(candidate)
                            if matches!(
                                tycker.type_filled_k(&candidate)?,
                                ss::Type::Label(ss::Label(found, _)) if found == name
                            ) =>
                        {
                            candidate
                        }
                        | _ => whole.materialize_k(tycker)?,
                    };
                    let ss::Type::Label(ss::Label(_, projected)) =
                        tycker.type_filled_k(&materialized)?.to_owned()
                    else {
                        unreachable!("a named field route retains its label shape")
                    };
                    current = Some(projected);
                    Ok(ValueFieldStep::Named { name, whole: materialized })
                }
                | DeferredValueFieldStep::Product { product, position } => {
                    let materialized = match current {
                        | Some(candidate)
                            if matches!(tycker.type_filled_k(&candidate)?, ss::Type::Prod(_)) =>
                        {
                            candidate
                        }
                        | _ => product.materialize_k(tycker)?,
                    };
                    let components = Self::materialized_product_components_k(tycker, materialized)?;
                    current = Some(components[position]);
                    Ok(ValueFieldStep::Product { product: materialized, components, position })
                }
            })
            .collect::<ResultKont<Vec<_>>>()?;
        let projected = match current {
            | Some(projected) => projected,
            | None => self.projected.materialize_k(tycker)?,
        };
        Ok(ValueFieldCandidate { route, projected })
    }

    fn materialized_product_components_k(
        tycker: &mut Tycker<'_>, product: ss::TypeId,
    ) -> ResultKont<Vec<ss::TypeId>> {
        match tycker.type_filled_k(&product)? {
            | ss::Type::Prod(ss::Prod(components)) => Ok(components),
            | _ => Ok(vec![product]),
        }
    }
}
