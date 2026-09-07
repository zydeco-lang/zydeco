//! Compose delayed environments and abstract assignments while traversing a telescope.

use super::*;

impl DeferredTelescopeType {
    pub(in crate::check) fn new(root: ss::TypeId) -> Self {
        Self { root, environment: None, abstracts: rpds::VectorSync::new_sync() }
    }

    pub(in crate::check) fn with_environment(mut self, environment: &ss::TyEnv) -> Self {
        self.environment =
            Some(DeferredTelescopeEnvironment { value: environment.clone(), unroll: true });
        self
    }

    pub(in crate::check) fn with_abstract(
        mut self, witness: ss::AbstId, payload: ss::TypeId,
    ) -> Self {
        self.abstracts.push_back_mut(DeferredAbstractAssignment { witness, payload });
        self
    }

    pub(in crate::check) fn descend(&self, root: ss::TypeId) -> Self {
        Self {
            root,
            environment: self.environment.as_ref().map(|environment| {
                DeferredTelescopeEnvironment { value: environment.value.clone(), unroll: false }
            }),
            abstracts: self.abstracts.clone(),
        }
    }

    pub(in crate::check) fn materialize_k(
        &self, tycker: &mut Tycker<'_>,
    ) -> ResultKont<ss::TypeId> {
        let assignments = self
            .abstracts
            .iter()
            .map(|assignment| (assignment.witness, assignment.payload))
            .collect::<Vec<_>>();
        let root = self.root.subst_absts_k(tycker, &assignments)?;
        let Some(environment) = &self.environment else { return Ok(root) };
        let root = if environment.unroll { root.unroll_k(tycker)? } else { root };
        root.subst_env_k(tycker, &environment.value)
    }

    pub(in crate::check) fn reveal_k(
        self, tycker: &mut Tycker<'_>,
    ) -> ResultKont<DeferredTelescopeView> {
        match tycker.statics.types_pre[&self.root].to_owned() {
            | ss::Fillable::Fill(_) => {
                let structure = tycker.type_filled_k(&self.root)?.to_owned();
                Ok(Self::new(self.root).view(structure))
            }
            | ss::Fillable::Done(
                ss::Type::Var(_) | ss::Type::Abst(_) | ss::Type::App(_) | ss::Type::Proj(_),
            ) => {
                let root = self.materialize_k(tycker)?;
                let structure = tycker.type_filled_k(&root)?.to_owned();
                Ok(Self::new(root).view(structure))
            }
            | ss::Fillable::Done(structure) => Ok(self.view(structure)),
        }
    }

    fn view(self, structure: ss::Type) -> DeferredTelescopeView {
        match structure {
            | ss::Type::ManifestKind(ss::ManifestKind { binder, definition, body }) => {
                let body = self.descend(body);
                DeferredTelescopeView::ManifestKind { binder, definition, body }
            }
            | ss::Type::Exists(exists) => {
                let ss::Exists { binder, mode, body } = *exists;
                let mode = match mode {
                    | ss::ExistsMode::Abstract => DeferredTelescopeExistsMode::Abstract,
                    | ss::ExistsMode::Manifest(definition) => {
                        DeferredTelescopeExistsMode::Manifest(self.descend(definition))
                    }
                };
                let body = self.descend(body);
                DeferredTelescopeView::Exists { binder, mode, body }
            }
            | _ => DeferredTelescopeView::Other(self),
        }
    }
}
