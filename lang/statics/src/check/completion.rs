use super::{AnnotationCompatibility, Switch, Tycker};
use crate::{arena::ArenaAssoc, surface_syntax as su, syntax::*};
use zydeco_utils::arena::ArenaAccess;

/// Incoming analytic constraints and per-definition evidence from one completion check.
#[derive(Clone, Debug)]
pub struct CompletionTyping {
    expectations: Vec<AnnId>,
    compatibility: ArenaAssoc<DefId, AnnotationCompatibility>,
}

impl CompletionTyping {
    pub fn expectations(&self) -> &[AnnId] {
        &self.expectations
    }

    pub fn compatibility(&self, definition: DefId) -> AnnotationCompatibility {
        self.compatibility.get(&definition).copied().unwrap_or_default()
    }
}

pub(super) struct CompletionCapture {
    target: su::TermId,
    expectations: Vec<AnnId>,
}

impl Tycker<'_> {
    pub(crate) fn set_completion_target(&mut self, target: su::TermId) -> &mut Self {
        self.completion = Some(CompletionCapture { target, expectations: Vec::new() });
        self
    }

    pub(super) fn observe_completion(&mut self, term: su::TermId, switch: Switch<AnnId>) {
        if let Some(capture) = &mut self.completion
            && capture.target == term
            && let Switch::Ana(expected) = switch
            && !capture.expectations.contains(&expected)
        {
            capture.expectations.push(expected);
        }
    }

    pub(crate) fn completion_typing(&mut self, definitions: &[DefId]) -> Option<CompletionTyping> {
        let expectations = self.completion.as_ref()?.expectations.clone();
        let compatibility = definitions
            .iter()
            .copied()
            .map(|definition| {
                let evidence = self
                    .statics
                    .annotations_var
                    .get(&definition)
                    .copied()
                    .and_then(|annotation| {
                        expectations
                            .iter()
                            .map(|expected| self.annotation_compatibility(annotation, *expected))
                            .max()
                    })
                    .unwrap_or_default();
                (definition, evidence)
            })
            .collect();
        Some(CompletionTyping { expectations, compatibility })
    }
}

#[cfg(test)]
mod tests {
    use crate::check::tests::with_empty_tycker;
    use crate::*;

    #[test]
    fn completion_capture_keeps_distinct_analytic_constraints() {
        with_empty_tycker(|tycker| {
            let root = tycker.data.root(tycker.db);
            tycker.set_completion_target(root);
            let environment = TyEnv::new();
            let vtype = ss::VType.build(tycker, &environment);

            assert!(
                TyEnvT::new(environment.clone(), root)
                    .tyck_k(tycker, Action::ana(AnnId::Set))
                    .is_err()
            );
            TyEnvT::new(environment.clone(), root)
                .tyck_k(tycker, Action::ana(AnnId::Kind(vtype)))
                .unwrap();
            TyEnvT::new(environment, root).tyck_k(tycker, Action::ana(AnnId::Kind(vtype))).unwrap();

            let candidate =
                Alloc::alloc(tycker, ss::VarName("candidate".to_owned()), AnnId::Kind(vtype), &());
            let typing = tycker.completion_typing(&[candidate]).unwrap();
            assert_eq!(typing.expectations(), [AnnId::Set, AnnId::Kind(vtype)]);
            assert_eq!(typing.compatibility(candidate), AnnotationCompatibility::Mismatch);
        });
    }

    #[test]
    fn completion_capture_uses_the_prepared_lexical_annotation() {
        with_empty_tycker(|tycker| {
            let root = tycker.data.root(tycker.db);
            tycker.set_completion_target(root);
            let empty = TyEnv::new();
            let vtype = ss::VType.build(tycker, &empty);
            let unit = ss::UnitTy.build(tycker, &empty);
            let definition =
                Alloc::alloc(tycker, ss::VarName("Alias".to_owned()), AnnId::Kind(vtype), &());
            let alias: ss::TypeId = Alloc::alloc(tycker, definition, vtype, &empty);
            let environment = empty + [(definition, AnnId::Type(unit))];
            TyEnvT::new(environment, root).tyck_k(tycker, Action::ana(alias.into())).unwrap();

            assert_eq!(tycker.completion_typing(&[]).unwrap().expectations(), [AnnId::Type(unit)]);
        });
    }

    #[test]
    fn completion_capture_preserves_an_unresolved_expected_type_category() {
        with_empty_tycker(|tycker| {
            let root = tycker.data.root(tycker.db);
            tycker.set_completion_target(root);
            let environment = TyEnv::new();
            let vtype = ss::VType.build(tycker, &environment);
            let unit = ss::UnitTy.build(tycker, &environment);
            let fill = Alloc::alloc(tycker, root, (), &());
            let pending: ss::TypeId = Alloc::alloc(tycker, fill, vtype, &environment);

            assert!(
                TyEnvT::new(environment, root).tyck_k(tycker, Action::ana(pending.into())).is_err()
            );
            let value =
                Alloc::alloc(tycker, ss::VarName("value".to_owned()), AnnId::Type(unit), &());
            let type_name =
                Alloc::alloc(tycker, ss::VarName("TypeName".to_owned()), AnnId::Kind(vtype), &());
            let errors = tycker.errors.len();
            let typing = tycker.completion_typing(&[value, type_name]).unwrap();

            assert_eq!(typing.expectations(), [AnnId::Type(pending)]);
            assert_eq!(typing.compatibility(value), AnnotationCompatibility::Unknown);
            assert_eq!(typing.compatibility(type_name), AnnotationCompatibility::Mismatch);
            assert!(tycker.statics.solus.get(&fill).is_none());
            assert_eq!(tycker.errors.len(), errors);
        });
    }
}
