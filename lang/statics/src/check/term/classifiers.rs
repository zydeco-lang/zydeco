//! Formation of value and computation function classifiers.

use super::*;

impl TermChecker<'_> {
    pub(super) fn check_val_pi_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::ValPi, switch: Switch<AnnId>,
        prepared_environment: Option<ss::TyEnv>,
    ) -> Result<TermAnnId> {
        Ok({
            let su::ValPi(binder, codomain) = term;
            let binder = self.mk(binder).tyck_k(tycker, PatternAction::syn())?;
            self.mk(ValuePiFormation { binder, codomain })
                .tyck_k(tycker, Action::forward(switch, prepared_environment.as_ref()))?
        })
    }

    pub(super) fn check_pi_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Pi, switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        let su::Pi(binder, body) = term;
        match switch {
            | Switch::Syn => self.check_pi_syn_k(tycker, binder, body),
            | Switch::Ana(AnnId::Set) => tycker.err(TyckError::SortMismatch),
            | Switch::Ana(AnnId::Kind(kd)) => match tycker.kind_filled(&kd)?.to_owned() {
                | ss::Kind::VType(_) => {
                    tycker.err(TyckError::Expressivity("use `val pi` to classify a value function"))
                }
                | ss::Kind::CType(ss::CType) => self.check_pi_ana_ctype_k(tycker, binder, body),
                | ss::Kind::Arrow(kd_arr) => {
                    self.check_pi_ana_kind_arrow_k(tycker, binder, body, kd_arr)
                }
                | ss::Kind::Label(_) => tycker.err(TyckError::KindMismatch),
            },
            | Switch::Ana(AnnId::Type(_)) => tycker.err(TyckError::SortMismatch),
        }
    }

    /// Synthesize a pi from its binder: a type binder forms a kind arrow or a
    /// universal type, a value binder a computation arrow.
    fn check_pi_syn_k(
        &self, tycker: &mut Tycker<'_>, binder: su::PatId, body: su::TermId,
    ) -> Result<TermAnnId> {
        let binder_out_ann = self.mk(binder).tyck_k(tycker, PatternAction::syn())?;
        match binder_out_ann.annotation {
            | PatAnnId::Kind(_) => {
                tycker.err(TyckError::Expressivity("kind quantification is not supported"))
            }
            | PatAnnId::Type(tpat, kd_1) => {
                self.check_pi_syn_type_binder_k(tycker, tpat, kd_1, body)
            }
            | PatAnnId::Value(_, _) => self
                .mk(ComputationPiFormation { binder: binder_out_ann, codomain: body })
                .tyck_k(tycker, Action::syn()),
        }
    }

    /// Synthesize the body of a pi over a type binder: a kind body forms a
    /// kind arrow, a computation-type body a universal type.
    fn check_pi_syn_type_binder_k(
        &self, tycker: &mut Tycker<'_>, tpat: ss::TPatId, kd_1: ss::KindId, body: su::TermId,
    ) -> Result<TermAnnId> {
        let abst = Alloc::alloc(tycker, tpat, (), &());
        let subst_vec = {
            let mut subst_vec = Vec::new();
            if let (Some(def), kd) = tpat.try_destruct_def(tycker) {
                let ty_abst = Alloc::alloc(tycker, abst, kd, &self.info);
                subst_vec.push((def, ty_abst.into()));
            }
            subst_vec
        };
        let body = self.mk_add(subst_vec, body).tyck_k(tycker, Action::syn())?;
        match body {
            | TermAnnId::Kind(kd_2) => {
                let input = crate::query::InternedPiSyn::new(
                    tycker.db,
                    crate::query::PiSynArm::KindArrow { kd_1, kd_2 },
                    tpat,
                    abst,
                );
                let outcome = tycker.judge(self.inner, input, crate::query::pi_syn_judgment);
                let crate::query::PiSynOutcome::Kind { id, kind } = outcome else {
                    unreachable!("the kind arrow of pi judgments is query-produced")
                };
                tycker.statics.kinds_pre.insert_new(id, ss::Fillable::Done(kind));
                Ok(TermAnnId::Kind(id))
            }
            | TermAnnId::Type(ty_2, kd_2) => {
                self.check_pi_syn_forall_k(tycker, tpat, abst, ty_2, kd_2)
            }
            | TermAnnId::Hole(_) => {
                self.reject_pi_syn_k(tycker, crate::query::PiSynArm::MissingAnnotation, tpat, abst)
            }
            | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                self.reject_pi_syn_k(tycker, crate::query::PiSynArm::SortMismatch, tpat, abst)
            }
        }
    }

    /// Form a universal type over a synthesized computation-type body.
    fn check_pi_syn_forall_k(
        &self, tycker: &mut Tycker<'_>, tpat: ss::TPatId, abst: ss::AbstId, ty_2: ss::TypeId,
        kd_2: ss::KindId,
    ) -> Result<TermAnnId> {
        let arm = match tycker.kind_filled(&kd_2)?.to_owned() {
            | ss::Kind::VType(_) => {
                tycker.err(TyckError::Expressivity("value-level universal types use `val pi`"))?
            }
            | ss::Kind::CType(_) => crate::query::PiSynArm::Forall { ty_2, kd_2 },
            | ss::Kind::Arrow(_) | ss::Kind::Label(_) => crate::query::PiSynArm::KindMismatch,
        };
        let input = crate::query::InternedPiSyn::new(tycker.db, arm, tpat, abst);
        match tycker.judge(self.inner, input, crate::query::pi_syn_judgment) {
            | crate::query::PiSynOutcome::Type { id, ty, kd } => {
                tycker.statics.types_pre.insert_new(id, ss::Fillable::Done(ty), kd);
                tycker.store_env(id, &self.info);
                Ok(TermAnnId::Type(id, kd))
            }
            | crate::query::PiSynOutcome::Error(error) => tycker.err(error),
            | crate::query::PiSynOutcome::Kind { .. } => {
                unreachable!("the type arm of pi judgments is query-produced")
            }
        }
    }

    /// Report the rejection that the pi judgment produces for `arm`.
    fn reject_pi_syn_k(
        &self, tycker: &mut Tycker<'_>, arm: crate::query::PiSynArm, tpat: ss::TPatId,
        abst: ss::AbstId,
    ) -> Result<TermAnnId> {
        let input = crate::query::InternedPiSyn::new(tycker.db, arm, tpat, abst);
        let outcome = tycker.judge(self.inner, input, crate::query::pi_syn_judgment);
        let crate::query::PiSynOutcome::Error(error) = outcome else {
            unreachable!("the rejection arms of pi judgments are query-produced")
        };
        tycker.err(error)
    }

    /// Check a pi against `CType`: a type binder forms a universal type, a
    /// value binder a computation arrow.
    fn check_pi_ana_ctype_k(
        &self, tycker: &mut Tycker<'_>, binder: su::PatId, body: su::TermId,
    ) -> Result<TermAnnId> {
        // synthesize the binder
        let binder_out_ann = self.mk(binder).tyck_k(tycker, PatternAction::syn())?;
        match binder_out_ann.annotation {
            | PatAnnId::Kind(_) => {
                tycker.err(TyckError::Expressivity("kind quantification is not supported"))
            }
            | PatAnnId::Type(tpat, _kd_1) => {
                let ctype = ss::CType.build(tycker, &self.info);
                let abst = Alloc::alloc(tycker, tpat, (), &());
                let subst_vec = {
                    let mut subst_vec = Vec::new();
                    if let (Some(def), kd) = tpat.try_destruct_def(tycker) {
                        let ty_abst = Alloc::alloc(tycker, abst, kd, &self.info);
                        subst_vec.push((def, ty_abst.into()));
                    }
                    subst_vec
                };
                let ty_2 =
                    self.mk_add(subst_vec, body).tyck_k(tycker, Action::ana(ctype.into()))?;
                let (ty_2, _ctype) = ty_2.try_as_type(tycker, TyckError::SortMismatch)?;
                let binder = ss::TypeBinder { pattern: tpat, witness: abst };
                let forall = Alloc::alloc(tycker, ss::Forall(binder, ty_2), ctype, &self.info);
                Ok(TermAnnId::Type(forall, ctype))
            }
            | PatAnnId::Value(_, _) => {
                let ctype = ss::CType.build(tycker, &self.info);
                self.mk(ComputationPiFormation { binder: binder_out_ann, codomain: body })
                    .tyck_k(tycker, Action::ana(ctype.into()))
            }
        }
    }

    /// Check a pi against a kind arrow, forming a kind arrow.
    fn check_pi_ana_kind_arrow_k(
        &self, tycker: &mut Tycker<'_>, binder: su::PatId, body: su::TermId,
        kd_arr: ss::Arrow<ss::KindId, ss::KindId>,
    ) -> Result<TermAnnId> {
        let ss::Arrow(kd_1, kd_2) = kd_arr;
        // ana binder with kd_1
        let binder_out_ann = self.mk(binder).tyck_k(tycker, PatternAction::ana(kd_1.into()))?;
        let (_, kd_1) = binder_out_ann.try_as_type(tycker, TyckError::SortMismatch)?;
        // ana body with kd_2
        let body = self.mk(body).tyck_k(tycker, Action::ana(kd_2.into()))?;
        let kd_2 = body.try_as_kind(tycker, TyckError::SortMismatch)?;
        let arr = Alloc::alloc(tycker, ss::Arrow(kd_1, kd_2), (), &());
        Ok(TermAnnId::Kind(arr))
    }
}
