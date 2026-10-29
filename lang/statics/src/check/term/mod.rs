//! Term checking dispatch, expected-type preparation, and source fact recording.

use super::*;
use crate::check::binding::{Assign, FixPoint};
use crate::check::functions::{
    ComputationPiFormation, PackPiElimination, PackPiIntroduction, PackageSignature,
    PackageWitnessProjectionBuilder, ValuePiFormation, ValuePiInstantiation, ValuePiPatternSkolems,
};
use crate::check::intrinsics::{BuiltinAttachment, ForeignAttachment, InternalTerm};
use crate::check::judgment::{Action, Switch, Tyck, TyckTask};
use crate::check::monadic::MonadicBlockElaboration;
use crate::check::pattern::CheckedPatternExt;
use crate::check::pattern::{PatternAction, PatternSkolems, ValuePatternShape};
use crate::check::projection::{
    DeferredTelescopeExistsMode, DeferredTelescopeType, DeferredTelescopeView,
    FieldProjectionResolver,
};
use crate::check::source::{InferenceRegion, TyckObservation};
use std::ops::ControlFlow;

mod abstraction;
mod application;
mod atomic;
mod boundary;
mod classifiers;
mod computation;
mod data;
mod package;
mod structure;

#[derive(Deref)]
struct TermChecker<'site> {
    site: &'site TyEnvT<su::TermId>,
}

impl<'a> Tyck<'a> for TyEnvT<su::TermId> {
    type Out = TermAnnId;
    type Action = Action<AnnId>;

    fn tyck_k(&self, tycker: &mut Tycker<'a>, action: Self::Action) -> Result<Self::Out> {
        let switch = action.switch;
        tycker.guarded(|tycker| {
            // administrative
            tycker.tasks.push_back_mut(TyckTask::Term(self.inner, switch));
            tycker.enter_site(su::EntityId::Term(self.inner));
            let result = self.tyck_inner_k(tycker, action);
            tycker.allocator.exit();
            result
        })
    }

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, action: Self::Action) -> Result<Self::Out> {
        let checker = TermChecker { site: self };
        let (switch, prepared_environment) = match checker.prepare_expected_k(tycker, action)? {
            | ControlFlow::Break(out_ann) => return Ok(out_ann),
            | ControlFlow::Continue(prepared) => prepared,
        };
        // the switch should contain no unfilled from here on

        tycker.observe_completion(self.inner, switch);

        let term = tycker.scoped.terms[&self.inner].to_owned();
        // if an annotation is a hole, we should stay in current switch; the
        // annotated term records its own facts
        if let su::Term::Ann(su::Ann { tm, ty }) = &term
            && let su::Term::Hole(su::Hole) = tycker.scoped.terms[ty]
        {
            return self
                .mk(*tm)
                .tyck_k(tycker, Action::forward(switch, prepared_environment.as_ref()));
        }
        let out_ann = checker.check_term_k(tycker, term, switch, prepared_environment)?;
        checker.record_term_facts(tycker, out_ann);
        Ok(out_ann)
    }
}

impl TermChecker<'_> {
    /// Prepare the expected classifier for checking.
    ///
    /// Analysis against an unfilled classifier synthesizes the term and fills
    /// the classifier with the result, which completes the check. Analysis
    /// against a type substitutes and normalizes it under the current
    /// environment unless an enclosing check already prepared it.
    fn prepare_expected_k(
        &self, tycker: &mut Tycker<'_>, action: Action<AnnId>,
    ) -> Result<ControlFlow<TermAnnId, (Switch<AnnId>, Option<ss::TyEnv>)>> {
        let Action { switch, prepared_environment } = action;
        match switch {
            | Switch::Syn | Switch::Ana(AnnId::Set) => {}
            | Switch::Ana(AnnId::Kind(kd)) => {
                if let Fillable::Fill(fill) = tycker.statics.kinds_pre[&kd].to_owned() {
                    return self.fill_expected_kind_k(tycker, switch, fill).map(ControlFlow::Break);
                }
            }
            | Switch::Ana(AnnId::Type(ty)) => match tycker.statics.types_pre[&ty].to_owned() {
                | Fillable::Fill(fill) => {
                    return self.fill_expected_type_k(tycker, switch, fill).map(ControlFlow::Break);
                }
                | _ => {
                    let preparation_is_valid = prepared_environment
                        .as_ref()
                        .is_some_and(|prepared| self.info.is_extension_of(prepared));
                    if !preparation_is_valid {
                        let kd = tycker.statics.type_kind(ty);
                        let switch = Switch::Ana(
                            ty.subst_env(tycker, &self.info)?.normalize(tycker, kd)?.into(),
                        );
                        return Ok(ControlFlow::Continue((switch, Some(self.info.clone()))));
                    }
                }
            },
        }
        Ok(ControlFlow::Continue((switch, prepared_environment)))
    }

    /// Synthesize a type and fill the expected kind with its kind.
    fn fill_expected_kind_k(
        &self, tycker: &mut Tycker<'_>, switch: Switch<AnnId>, fill: FillId,
    ) -> Result<TermAnnId> {
        tycker.observe_completion(self.inner, switch);
        match self.tyck_k(tycker, Action::syn())? {
            | TermAnnId::Type(ty, kd) => {
                let kd = fill.fill(tycker, kd.into())?.as_kind();
                Ok(TermAnnId::Type(ty, kd))
            }
            | TermAnnId::Hole(_)
            | TermAnnId::Kind(_)
            | TermAnnId::Value(_, _)
            | TermAnnId::Compu(_, _) => tycker.err(TyckError::SortMismatch),
        }
    }

    /// Synthesize a value or computation and fill the expected type with its
    /// type.
    fn fill_expected_type_k(
        &self, tycker: &mut Tycker<'_>, switch: Switch<AnnId>, fill: FillId,
    ) -> Result<TermAnnId> {
        tycker.observe_completion(self.inner, switch);
        match self.tyck_k(tycker, Action::syn())? {
            | TermAnnId::Value(v, ty) => {
                let ty = fill.fill(tycker, ty.into())?.as_type();
                Ok(TermAnnId::Value(v, ty))
            }
            | TermAnnId::Compu(c, ty) => {
                let ty = fill.fill(tycker, ty.into())?.as_type();
                Ok(TermAnnId::Compu(c, ty))
            }
            | TermAnnId::Hole(_) | TermAnnId::Kind(_) | TermAnnId::Type(_, _) => {
                tycker.err(TyckError::SortMismatch)
            }
        }
    }

    /// Dispatch a term to the rule for its syntactic form.
    fn check_term_k(
        &self, tycker: &mut Tycker<'_>, term: su::Term<su::DefId>, switch: Switch<AnnId>,
        prepared_environment: Option<ss::TyEnv>,
    ) -> Result<TermAnnId> {
        use su::Term as Tm;
        Ok(match term {
            | Tm::TypeOf(su::TypeOf(operand)) => self.check_type_of_k(tycker, operand, switch)?,
            | Tm::Meta(term) => self.check_meta_k(tycker, *term, switch, prepared_environment)?,
            | Tm::SourceBoundary(su::SourceBoundary(term)) => {
                self.check_source_boundary_k(tycker, term, switch)?
            }
            | Tm::SignatureBoundary(su::SignatureBoundary(term)) => {
                self.check_signature_boundary_k(tycker, term, switch)?
            }
            | Tm::Internal(internal) => {
                InternalTerm(internal, self.inner).tyck_k(tycker, &self.info, switch)?
            }
            | Tm::Sealed(_) => unreachable!(),
            | Tm::Ann(term) => self.check_ann_k(tycker, term, switch)?,
            | Tm::Hole(term) => self.check_hole_k(tycker, term, switch)?,
            | Tm::Var(def) => self.check_var_k(tycker, def, switch)?,
            | Tm::Named(term) => self.check_named_k(tycker, term, switch)?,
            | Tm::Label(term) => self.check_label_k(tycker, term, switch)?,
            | Tm::Triv(su::Triv) => self.check_triv_k(tycker, switch)?,
            | Tm::Cons(term) => self.check_cons_k(tycker, term, switch)?,
            | Tm::ValAbs(term) => self.check_val_abs_k(tycker, term, switch)?,
            | Tm::Abs(term) => self.check_abs_k(tycker, term, switch)?,
            | Tm::App(term) => self.check_app_k(tycker, term, switch, prepared_environment)?,
            | Tm::Fix(term) => self.check_fix_k(tycker, term, switch)?,
            | Tm::ValPi(term) => self.check_val_pi_k(tycker, term, switch, prepared_environment)?,
            | Tm::Pi(term) => self.check_pi_k(tycker, term, switch)?,
            | Tm::Sigma(term) => self.check_sigma_k(tycker, term, switch)?,
            | Tm::ManifestExists(term) => self.check_manifest_exists_k(tycker, *term, switch)?,
            | Tm::Pack(term) => self.check_pack_k(tycker, *term, switch)?,
            | Tm::Thunk(term) => self.check_thunk_k(tycker, term, switch)?,
            | Tm::Force(term) => self.check_force_k(tycker, term, switch)?,
            | Tm::Ret(term) => self.check_ret_k(tycker, term, switch)?,
            | Tm::Do(term) => self.check_do_k(tycker, *term, switch, prepared_environment)?,
            | Tm::Let(term) => self.check_let_k(tycker, *term, switch, prepared_environment)?,
            | Tm::MobileParam(_) | Tm::MobileBind(_) => {
                unreachable!("mobile syntax must be eliminated during name resolution")
            }
            | Tm::Residual(term) => {
                self.check_residual_k(tycker, term, switch, prepared_environment)?
            }
            | Tm::Block(term) => self.check_block_k(tycker, term, switch, prepared_environment)?,
            | Tm::RecGroup(term) => {
                self.check_rec_group_k(tycker, term, switch, prepared_environment)?
            }
            | Tm::MoBlock(term) => self.check_mo_block_k(tycker, *term, switch)?,
            | Tm::Data(term) => self.check_data_k(tycker, term, switch)?,
            | Tm::CoData(term) => self.check_co_data_k(tycker, term, switch)?,
            | Tm::Ctor(term) => self.check_ctor_k(tycker, term, switch)?,
            | Tm::Match(term) => self.check_match_k(tycker, term, switch)?,
            | Tm::CoMatchClauses(term) => self.check_co_match_clauses_k(tycker, term, switch)?,
            | Tm::CoMatch(term) => self.check_co_match_k(tycker, term, switch)?,
            | Tm::Dtor(term) => self.check_dtor_k(tycker, term, switch)?,
            | Tm::Proj(term) => self.check_proj_k(tycker, term, switch)?,
            | Tm::Lit(lit) => self.check_lit_k(tycker, lit, switch)?,
        })
    }

    /// Check a term against its non-hole annotation, meeting the annotation
    /// with the expected classifier.
    fn check_ann_k(
        &self, tycker: &mut Tycker<'_>, term: su::Ann<su::TermId, su::TermId>,
        switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        let su::Ann { tm, ty } = term;
        let ty_out_ann = self.mk(ty).tyck_k(tycker, Action::syn())?;
        let ty_ann = match ty_out_ann {
            | TermAnnId::Kind(kd) => kd.into(),
            | TermAnnId::Type(ty, _kd) => ty.into(),
            | TermAnnId::Hole(_) | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                tycker.err(TyckError::SortMismatch)?
            }
        };
        let ann = match switch {
            | Switch::Syn => ty_ann,
            | Switch::Ana(ty_ana) => Lub::lub(ty_ana, ty_ann, tycker)?,
        };

        self.mk(tm).tyck_k(tycker, Action::ana_prepared(ann, &self.info))
    }

    /// Record the source facts of a checked term: its back mapping, its final
    /// annotation, and whether it is global.
    fn record_term_facts(&self, tycker: &mut Tycker<'_>, out_ann: TermAnnId) {
        let Some(out) = out_ann.as_term() else { return };
        // Maintain one canonical back mapping for the materialized term.
        // Import boundaries are reference sites, so they retain their own
        // facts below without replacing the provider's source location.
        if !matches!(tycker.scoped.terms[&self.inner], su::Term::SourceBoundary(_)) {
            tycker.statics.terms.record(self.inner, out);
        }
        // record the final annotation for editor facts; fixpoint re-checks
        // overwrite the earlier occurrence's entry
        tycker.statics.record_term_annotation(self.inner, out_ann);

        // check if the term is global
        let global = tycker
            .source_free_variables(&self.inner)
            .iter()
            .all(|def| tycker.statics.global_defs.get(def).is_some());

        if global {
            tycker.statics.global_terms.ensure(out);
        }
    }
}

#[cfg(test)]
mod tests;
