//! Source boundaries, classifier queries, metadata, and elaborated blocks.

use super::*;
use crate::check::source::CheckedTerm;

impl TermChecker<'_> {
    pub(super) fn check_type_of<'db>(
        &self, tycker: &mut Tycker<'db>, operand: su::TermId, switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        Ok({
            let environment = self.info.clone();
            let checked = tycker.synthesize_once(self.inner, &environment, |tycker| {
                let operand = self.mk(operand).tyck(tycker, Action::syn())?;
                let classifier = operand.classifier(tycker)?;
                if let TermAnnId::Type(ty, _) = classifier {
                    ty.constrain_to_scope(tycker, environment.skolem_scope())?;
                }
                Ok(classifier)
            })?;
            checked.reconcile(tycker, switch)?
        })
    }

    pub(super) fn check_meta<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::MetaTerm, switch: Switch<AnnId>,
        prepared_environment: Option<ss::TyEnv>,
    ) -> Result<TermAnnId> {
        Ok({
            let su::MetaTerm { meta, body: term } = term;
            let res = self
                .mk(term)
                .tyck(tycker, Action::forward(switch, prepared_environment.as_ref()))?;
            match meta {
                | su::ResolvedMeta::Builtin(role) => {
                    BuiltinAttachment::new(role, res).register(tycker, &self.info)?;
                }
                | su::ResolvedMeta::Ffi(target) => {
                    ForeignAttachment::new(target, res, term).register(tycker)?;
                }
                | su::ResolvedMeta::Debug(metadata) => {
                    tycker.observations.push(TyckObservation::Debug { metadata, result: res });
                }
                | su::ResolvedMeta::Opaque(_) => {}
            }
            res
        })
    }

    pub(super) fn check_source_boundary<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::TermId, switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        if let Some(prechecked) = tycker.prechecked_boundaries.get(&self.inner).copied() {
            let checked = tycker.source_guarded(|tycker| {
                tycker.tasks.push(TyckTask::Term(self.inner, Switch::Syn));
                match prechecked {
                    | Ok(root) => {
                        CheckedTerm(root).require_complete(tycker, TyckError::MissingAnnotation)
                    }
                    | Err(()) => Err(ReportedError),
                }
            })?;
            return checked.reconcile(tycker, switch);
        }
        Ok({
            let environment = TyEnv::new();
            let checked = tycker.source_guarded(|tycker| {
                let checked = tycker.synthesize_once(term, &environment, |tycker| {
                    let inference = InferenceRegion::enter(tycker);
                    let checked =
                        TyEnvT::new(environment.clone(), term).tyck(tycker, Action::syn())?;
                    inference.close(tycker)?;
                    Ok(checked)
                })?;
                tycker.tasks.push(TyckTask::Term(term, Switch::Syn));
                checked.require_complete(tycker, TyckError::MissingAnnotation)
            })?;
            checked.reconcile(tycker, switch)?
        })
    }

    pub(super) fn check_signature_boundary<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::TermId, switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        Ok({
            let environment = TyEnv::new();
            let checked = tycker.source_guarded(|tycker| {
                let checked = tycker.synthesize_once(term, &environment, |tycker| {
                    let inference = InferenceRegion::enter(tycker);
                    let checked =
                        TyEnvT::new(environment.clone(), term).tyck(tycker, Action::syn())?;
                    inference.close(tycker)?;
                    Ok(checked)
                })?;
                tycker.tasks.push(TyckTask::Term(term, Switch::Syn));
                match checked.root() {
                    | TermAnnId::Type(_, _) => Ok(checked),
                    | TermAnnId::Hole(_)
                    | TermAnnId::Kind(_)
                    | TermAnnId::Value(_, _)
                    | TermAnnId::Compu(_, _) => tycker.err(TyckError::SignatureNotType)?,
                }
            })?;
            checked.reconcile(tycker, switch)?
        })
    }

    pub(super) fn check_residual<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Residual, switch: Switch<AnnId>,
        prepared_environment: Option<ss::TyEnv>,
    ) -> Result<TermAnnId> {
        Ok({
            let su::Residual(body) = term;
            self.mk(body).tyck(tycker, Action::forward(switch, prepared_environment.as_ref()))?
        })
    }

    pub(super) fn check_block<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Block, switch: Switch<AnnId>,
        prepared_environment: Option<ss::TyEnv>,
    ) -> Result<TermAnnId> {
        Ok({
            let su::Block(body) = term;
            let inference = InferenceRegion::enter(tycker);
            let checked = self
                .mk(body)
                .tyck(tycker, Action::forward(switch, prepared_environment.as_ref()))?;
            inference.close(tycker)?;
            checked
        })
    }

    pub(super) fn check_rec_group<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::RecGroup, switch: Switch<AnnId>,
        prepared_environment: Option<ss::TyEnv>,
    ) -> Result<TermAnnId> {
        Ok({
            let su::RecGroup { definitions, tail } = term;
            let bindings = definitions
                .into_iter()
                .enumerate()
                .map(|(source_order, su::RecursiveDefinition { binder, bindee })| {
                    su::Binding::from_term(
                        bindee,
                        su::BindingForm::Definition(su::Definition { binder, bindee }),
                        source_order,
                    )
                })
                .collect::<Vec<_>>();
            let env = FixPoint(self.mk(bindings)).tyck(tycker, ())?;
            env.mk(tail).tyck(tycker, Action::forward(switch, prepared_environment.as_ref()))?
        })
    }

    pub(super) fn check_mo_block<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::MoBlock, switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        Ok({
            let environment = self.info.clone();
            let checked = tycker.synthesize_once(self.inner, &environment, |tycker| {
                MonadicBlockElaboration::new(&term, &environment).check(tycker)
            })?;
            checked.reconcile(tycker, switch)?
        })
    }
}
