//! Pattern checking, lexical bindings, and the witnesses introduced by opening patterns.

use super::*;
use crate::check::binding::Assign;
use crate::check::functions::PackageSignature;
use crate::check::judgment::{Action, Switch, Tyck, TyckTask};
use crate::check::projection::{
    DeferredTelescopeExistsMode, DeferredTelescopeType, DeferredTelescopeView,
    ExistentialProjectionPattern, FieldProjectionResolver,
};

/// Non-contextual output of the pattern-checking judgment.
///
/// The surrounding [`TyEnvT`] carries the environment after the pattern. This
/// payload records only the checked pattern and the existential identities it
/// opened, in source order.
#[derive(Clone, Debug, Deref)]
pub struct PatternCheck {
    #[deref]
    pub(super) annotation: PatAnnId,
    pub(super) opened: Vec<AbstId>,
}

/// A checked pattern paired with the environment it makes available to its
/// body.
pub type CheckedPattern = TyEnvT<PatternCheck>;

impl PatternCheck {
    pub(super) fn new(annotation: PatAnnId) -> Self {
        Self { annotation, opened: Vec::new() }
    }

    pub(super) fn with_opened(annotation: PatAnnId, opened: Vec<AbstId>) -> Self {
        Self { annotation, opened }
    }
}

/// Deterministic materializer for the high-frequency leaf-pattern cases.
///
/// These nodes depend only on the current allocation site and the annotation
/// the checker has already computed. Keeping them inside the active check
/// avoids retaining a second copy as a salsa producer-query memo.
enum PatternLeaf {
    Hole,
    Variable(su::DefId),
}

impl PatternLeaf {
    fn materialize(self, tycker: &mut Tycker<'_>, env: &TyEnv, ann: AnnId) -> PatAnnId {
        match (self, ann) {
            | (Self::Hole, AnnId::Set) => {
                let id = tycker.query_derived_id(2);
                tycker.statics.kpats.insert_new(id, ss::KindPattern::Hole(ss::Hole));
                PatAnnId::Kind(id)
            }
            | (Self::Variable(def), AnnId::Set) => {
                let id = tycker.query_derived_id(2);
                tycker.statics.kpats.insert_new(id, ss::KindPattern::Var(def));
                PatAnnId::Kind(id)
            }
            | (Self::Hole, AnnId::Kind(kd)) => {
                let id = tycker.query_derived_id(2);
                tycker.statics.tpats.insert_new(id, ss::TypePattern::Hole(ss::Hole));
                tycker.statics.annotations_tpat.insert_new(id, kd);
                tycker.statics.env_tpat.insert_new(id, env.clone());
                PatAnnId::Type(id, kd)
            }
            | (Self::Variable(def), AnnId::Kind(kd)) => {
                let id = tycker.query_derived_id(2);
                tycker.statics.tpats.insert_new(id, ss::TypePattern::Var(def));
                tycker.statics.annotations_tpat.insert_new(id, kd);
                tycker.statics.env_tpat.insert_new(id, env.clone());
                PatAnnId::Type(id, kd)
            }
            | (Self::Hole, AnnId::Type(ty)) => {
                let id = tycker.query_derived_id(2);
                tycker.statics.vpats.insert_new(id, ss::ValuePattern::Hole(ss::Hole));
                tycker.statics.annotations_vpat.insert_new(id, ty);
                tycker.statics.env_vpat.insert_new(id, env.clone());
                PatAnnId::Value(id, ty)
            }
            | (Self::Variable(def), AnnId::Type(ty)) => {
                let id = tycker.query_derived_id(2);
                tycker.statics.vpats.insert_new(id, ss::ValuePattern::Var(def));
                tycker.statics.annotations_vpat.insert_new(id, ty);
                tycker.statics.env_vpat.insert_new(id, env.clone());
                PatAnnId::Value(id, ty)
            }
        }
    }
}

/// Materializer for the inference stand-in of an unannotated variable pattern.
struct PatternVariableStandIn;

impl PatternVariableStandIn {
    fn materialize(tycker: &mut Tycker<'_>, env: &TyEnv, source: su::PatId) -> AnnId {
        let fill = tycker.query_derived_id(0);
        let ty = tycker.query_derived_id(1);
        let vtype = ss::VType.build(tycker, env);

        tycker.statics.fills.insert_new(fill, ss::InferenceSite::Pattern(source));
        tycker.statics.types_pre.insert_new(ty, ss::Fillable::Fill(fill), vtype);
        tycker.store_env(ty, env);

        let scope = env.skolem_scope().clone();
        if let Some(existing) = tycker.statics.fill_scopes.insert_or_get(fill, scope.clone()) {
            tycker.statics.fill_scopes.replace_existing(fill, existing.intersection(&scope));
        }
        ty.into()
    }
}

/// Structural patterns that bind without testing a constructor or literal.
pub(super) struct ValuePatternShape;

impl ValuePatternShape {
    pub(super) fn is_irrefutable(tycker: &Tycker<'_>, pattern: ss::VPatId) -> bool {
        match &tycker.statics.vpats[&pattern] {
            | ss::ValuePattern::Hole(_) | ss::ValuePattern::Var(_) | ss::ValuePattern::Triv(_) => {
                true
            }
            | ss::ValuePattern::Named(ss::Named(_, inner)) => Self::is_irrefutable(tycker, *inner),
            | ss::ValuePattern::Ctor(_) => false,
            | ss::ValuePattern::Lit(_) => false,
            | ss::ValuePattern::Alias(ss::Alias(patterns)) => {
                patterns.iter().all(|pattern| Self::is_irrefutable(tycker, *pattern))
            }
            | ss::ValuePattern::VCons(patterns) => {
                patterns.iter().all(|pattern| Self::is_irrefutable(tycker, *pattern))
            }
            | ss::ValuePattern::SCons(ss::ConsN(_, body)) => Self::is_irrefutable(tycker, *body),
            | ss::ValuePattern::View(view) => Self::is_irrefutable(tycker, view.pattern),
        }
    }
}

pub(super) trait CheckedPatternExt {
    fn with_annotation(self, annotation: PatAnnId) -> Self;
    fn close_scope_k(&self, tycker: &mut Tycker<'_>, result: ss::TypeId) -> ResultKont<()>;
    fn package_telescope_k(&self, tycker: &mut Tycker<'_>)
    -> ResultKont<Option<ss::PackTelescope>>;
}

impl CheckedPatternExt for CheckedPattern {
    fn with_annotation(self, annotation: PatAnnId) -> Self {
        let TyEnvT { info, inner } = self;
        TyEnvT::new(info, PatternCheck::with_opened(annotation, inner.opened))
    }

    #[track_caller]
    fn close_scope_k(&self, tycker: &mut Tycker<'_>, result: ss::TypeId) -> ResultKont<()> {
        if self.inner.opened.is_empty() {
            return Ok(());
        }
        let outer = self.info.skolem_scope().without(&self.inner.opened);
        result.constrain_to_scope_k(tycker, &outer)
    }

    #[track_caller]
    fn package_telescope_k(
        &self, tycker: &mut Tycker<'_>,
    ) -> ResultKont<Option<ss::PackTelescope>> {
        let Some((first, rest)) = self.inner.opened.split_first() else {
            return Ok(None);
        };
        let (pattern, _) = self.inner.annotation.try_as_value(
            tycker,
            TyckError::SortMismatch,
            std::panic::Location::caller(),
        )?;
        let boundary_arity = pattern.package_witness_arity(tycker).unwrap_or_default();
        if boundary_arity < self.inner.opened.len() {
            tycker.err_k(
                TyckError::PackageWitnessArityMismatch {
                    expected: self.inner.opened.len(),
                    found: boundary_arity,
                },
                std::panic::Location::caller(),
            )?
        }
        Ok(Some(ss::PackTelescope::new(*first, rest.iter().copied())))
    }
}

/// Canonical existential identities assigned to package-pattern components.
#[derive(Clone, Debug, Default)]
pub(super) struct PatternSkolems {
    pub(super) patterns: rpds::HashTrieMapSync<su::PatId, ss::AbstId>,
    pub(super) witnesses: rpds::HashTrieMapSync<ss::AbstId, ss::AbstId>,
}

impl PatternSkolems {
    pub(super) fn new(
        patterns: impl IntoIterator<Item = (su::PatId, ss::AbstId)>,
        witnesses: impl IntoIterator<Item = (ss::AbstId, ss::AbstId)>,
    ) -> Self {
        Self {
            patterns: patterns.into_iter().collect(),
            witnesses: witnesses.into_iter().collect(),
        }
    }

    pub(super) fn get(&self, pattern: &su::PatId) -> Option<ss::AbstId> {
        self.patterns.get(pattern).copied()
    }

    pub(super) fn get_witness(&self, witness: &ss::AbstId) -> Option<ss::AbstId> {
        self.witnesses.get(witness).copied()
    }

    pub(super) fn merge(mut self, other: Self) -> Self {
        other.patterns.iter().for_each(|(pattern, witness)| {
            self.patterns.insert_mut(*pattern, *witness);
        });
        other.witnesses.iter().for_each(|(source, canonical)| {
            self.witnesses.insert_mut(*source, *canonical);
        });
        self
    }
}

/// Pattern-checking mode together with any canonical package witnesses.
#[derive(Clone, Debug)]
pub struct PatternAction {
    pub(super) switch: Switch<AnnId>,
    pub(super) skolems: PatternSkolems,
}

impl PatternAction {
    pub fn syn() -> Self {
        Self { switch: Switch::Syn, skolems: PatternSkolems::default() }
    }

    pub fn ana(ann: AnnId) -> Self {
        Self { switch: Switch::Ana(ann), skolems: PatternSkolems::default() }
    }

    pub fn switch(switch: Switch<AnnId>) -> Self {
        Self { switch, skolems: PatternSkolems::default() }
    }

    pub(super) fn with_skolems(mut self, skolems: PatternSkolems) -> Self {
        self.skolems = skolems;
        self
    }
}

mod atomic;
mod data;
mod named;
mod product;

#[derive(Deref)]
struct PatternChecker<'site> {
    site: &'site TyEnvT<su::PatId>,
}

impl<'a> Tyck<'a> for TyEnvT<su::PatId> {
    type Out = CheckedPattern;
    type Action = PatternAction;

    fn tyck_k(&self, tycker: &mut Tycker<'a>, action: Self::Action) -> ResultKont<Self::Out> {
        tycker.guarded(|tycker| {
            // administrative
            tycker.tasks.push_back_mut(TyckTask::Pat(self.inner, action.switch));
            let entity = su::EntityId::Pat(self.inner);
            let occurrence = tycker.check_counts.get(&entity).copied().unwrap_or(0);
            let _ = tycker.check_counts.upsert(entity, occurrence + 1);
            tycker.allocator.enter(
                self.inner.key_space().as_u64(),
                self.inner.raw().into_u32(),
                occurrence,
            );
            let result = self.tyck_inner_k(tycker, action);
            tycker.allocator.exit();
            result
        })
    }

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, action: Self::Action) -> ResultKont<Self::Out> {
        let PatternAction { switch, skolems } = action;
        use su::Pattern as Pat;
        let checker = PatternChecker { site: self };
        let elaboration = match tycker.scoped.pats[&self.inner].clone() {
            | Pat::Ann(pat) => checker.check_ann_k(tycker, pat, switch, skolems)?,
            | Pat::Hole(pat) => checker.check_hole_k(tycker, pat, switch)?,
            | Pat::Var(def) => checker.check_var_k(tycker, def, switch)?,
            | Pat::Named(pat) => checker.check_named_k(tycker, pat, switch, skolems)?,
            | Pat::Project(su::ProjectionPattern(field, inner)) => {
                checker.check_project_k(tycker, field, inner, switch, skolems)?
            }
            | Pat::View(su::ViewPattern { function, pattern }) => {
                checker.check_view_k(tycker, function, pattern, switch, skolems)?
            }
            | Pat::Ctor(pat) => checker.check_ctor_k(tycker, pat, switch, skolems)?,
            | Pat::Lit(literal) => checker.check_lit_k(tycker, literal, switch)?,
            | Pat::Alias(su::Alias(patterns)) => {
                checker.check_alias_k(tycker, patterns, switch, skolems)?
            }
            | Pat::Triv(su::Triv) => checker.check_triv_k(tycker, switch)?,
            | Pat::Cons(pat) => checker.check_cons_k(tycker, pat, switch, skolems)?,
        };

        // maintain back mapping
        tycker.statics.pats.record(self.inner, elaboration.annotation.as_pat());

        Ok(elaboration)
    }
}

impl Tycker<'_> {
    pub(in crate::check) fn pattern_has_payload_annotation(&self, pattern: su::PatId) -> bool {
        match self.scoped.pats[&pattern].clone() {
            | su::Pattern::Ann(_) => true,
            | su::Pattern::Named(su::Named(_, inner)) => self.pattern_has_payload_annotation(inner),
            | su::Pattern::Project(su::ProjectionPattern(_, inner)) => {
                self.pattern_has_payload_annotation(inner)
            }
            | su::Pattern::Alias(su::Alias(patterns)) => {
                patterns.iter().any(|pattern| self.pattern_has_payload_annotation(*pattern))
            }
            | su::Pattern::View(su::ViewPattern { pattern, .. }) => {
                self.pattern_has_payload_annotation(pattern)
            }
            | su::Pattern::Hole(_)
            | su::Pattern::Var(_)
            | su::Pattern::Ctor(_)
            | su::Pattern::Lit(_)
            | su::Pattern::Triv(_)
            | su::Pattern::Cons(_) => false,
        }
    }
}
