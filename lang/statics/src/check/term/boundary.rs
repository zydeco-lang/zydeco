//! Source boundaries, classifier queries, metadata, and elaborated blocks.

use super::*;

impl TermChecker<'_> {
    pub(super) fn check_type_of_k<'db>(
        &self, tycker: &mut Tycker<'db>, operand: su::TermId, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let environment = self.info.clone();
            let checked = tycker.synthesize_once_k(self.inner, &environment, |tycker| {
                let operand = self.mk(operand).tyck_k(tycker, Action::syn())?;
                let classifier = operand.classifier_k(tycker)?;
                if let TermAnnId::Type(ty, _) = classifier {
                    ty.constrain_to_scope_k(tycker, environment.skolem_scope())?;
                }
                Ok(classifier)
            })?;
            checked.reconcile_k(tycker, switch)?
        })
    }

    pub(super) fn check_meta_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::MetaT<su::TermId>, switch: Switch<AnnId>,
        prepared_environment: Option<ss::TyEnv>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::MetaT(meta, term) = term;
            let res = self
                .mk(term)
                .tyck_k(tycker, Action::forward(switch, prepared_environment.as_ref()))?;
            if let Some(meta) = meta
                .specialize::<BuiltinMeta>()
                .expect("builtin metadata is validated during desugaring")
            {
                BuiltinAttachment::new(meta.role, res).register_k(tycker, &self.info)?;
            }
            if let Some(meta) =
                meta.specialize::<FfiMeta>().expect("ffi metadata is validated during desugaring")
            {
                ForeignAttachment::new(meta.target, res, term).register_k(tycker)?;
            }
            if meta.is(MetadataKind::Debug.name()) {
                tycker.observations.push(TyckObservation::Debug { metadata: meta, result: res });
            }
            res
        })
    }

    pub(super) fn check_source_boundary_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::TermId, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let environment = TyEnv::new();
            let checked = tycker.source_guarded(|tycker| {
                let checked = tycker.synthesize_once_k(term, &environment, |tycker| {
                    let inference = InferenceRegion::enter(tycker);
                    let checked =
                        TyEnvT::new(environment.clone(), term).tyck_k(tycker, Action::syn())?;
                    inference.close_k(tycker)?;
                    Ok(checked)
                })?;
                tycker.tasks.push_back_mut(TyckTask::Term(term, Switch::Syn));
                checked.require_complete_k(tycker, TyckError::MissingAnnotation)
            })?;
            checked.reconcile_k(tycker, switch)?
        })
    }

    pub(super) fn check_signature_boundary_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::TermId, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let environment = TyEnv::new();
            let checked = tycker.source_guarded(|tycker| {
                let checked = tycker.synthesize_once_k(term, &environment, |tycker| {
                    let inference = InferenceRegion::enter(tycker);
                    let checked =
                        TyEnvT::new(environment.clone(), term).tyck_k(tycker, Action::syn())?;
                    inference.close_k(tycker)?;
                    Ok(checked)
                })?;
                tycker.tasks.push_back_mut(TyckTask::Term(term, Switch::Syn));
                match checked.root() {
                    | TermAnnId::Type(_, _) => Ok(checked),
                    | TermAnnId::Hole(_)
                    | TermAnnId::Kind(_)
                    | TermAnnId::Value(_, _)
                    | TermAnnId::Compu(_, _) => {
                        tycker.err_k(TyckError::SignatureNotType, std::panic::Location::caller())?
                    }
                }
            })?;
            checked.reconcile_k(tycker, switch)?
        })
    }

    pub(super) fn check_residual_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Residual, switch: Switch<AnnId>,
        prepared_environment: Option<ss::TyEnv>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::Residual(body) = term;
            self.mk(body).tyck_k(tycker, Action::forward(switch, prepared_environment.as_ref()))?
        })
    }

    pub(super) fn check_block_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Block, switch: Switch<AnnId>,
        prepared_environment: Option<ss::TyEnv>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::Block(body) = term;
            let inference = InferenceRegion::enter(tycker);
            let checked = self
                .mk(body)
                .tyck_k(tycker, Action::forward(switch, prepared_environment.as_ref()))?;
            inference.close_k(tycker)?;
            checked
        })
    }

    pub(super) fn check_rec_group_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::RecGroup, switch: Switch<AnnId>,
        prepared_environment: Option<ss::TyEnv>,
    ) -> ResultKont<TermAnnId> {
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
            let env = FixPoint(self.mk(bindings)).tyck_k(tycker, ())?;
            env.mk(tail).tyck_k(tycker, Action::forward(switch, prepared_environment.as_ref()))?
        })
    }

    pub(super) fn check_mo_block_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::MoBlock, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let environment = self.info.clone();
            let checked = tycker.synthesize_once_k(self.inner, &environment, |tycker| {
                MonadicBlockElaboration::new(&term, &environment).check_k(tycker)
            })?;
            checked.reconcile_k(tycker, switch)?
        })
    }
}
