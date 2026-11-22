//! Formation of value and computation function classifiers.

use super::*;

impl TermChecker<'_> {
    pub(super) fn check_val_pi<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::ValPi, switch: Switch<AnnId>,
        prepared_environment: Option<ss::TyEnv>,
    ) -> Result<TermAnnId> {
        Ok({
            let su::ValPi(binder, codomain) = term;
            let binder = self.mk(binder).tyck(tycker, PatternAction::syn())?;
            self.mk(ValuePiFormation { binder, codomain })
                .tyck(tycker, Action::forward(switch, prepared_environment.as_ref()))?
        })
    }

    pub(super) fn check_pi<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Pi, switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        let su::Pi(binders, body) = term;
        let (binder, tail) = TelescopeTail::split(self.inner, TelescopeForm::Pi, binders, body);
        self.check_pi_binder(tycker, binder, tail, switch)
    }

    /// Check one binder of a pi over what follows it.
    pub(super) fn check_pi_binder<'db>(
        &self, tycker: &mut Tycker<'db>, binder: su::PatId, body: TelescopeTail,
        switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        match switch {
            | Switch::Syn => self.check_pi_syn(tycker, binder, body),
            | Switch::Ana(AnnId::Set) => tycker.err(TyckError::SortMismatch),
            | Switch::Ana(AnnId::Kind(kd)) => match tycker.kind_filled(&kd)?.to_owned() {
                | ss::Kind::VType(_) => {
                    tycker.err(TyckError::Expressivity("use `val pi` to classify a value function"))
                }
                | ss::Kind::CType(ss::CType) => self.check_pi_ana_ctype(tycker, binder, body),
                | ss::Kind::Arrow(kd_arr) => {
                    self.check_pi_ana_kind_arrow(tycker, binder, body, kd_arr)
                }
                | ss::Kind::Label(_) => tycker.err(TyckError::KindMismatch),
            },
            | Switch::Ana(AnnId::Type(_)) => tycker.err(TyckError::SortMismatch),
        }
    }

    /// Synthesize a pi from its binder: a type binder forms a kind arrow or a
    /// universal type, a value binder a computation arrow.
    fn check_pi_syn(
        &self, tycker: &mut Tycker<'_>, binder: su::PatId, body: TelescopeTail,
    ) -> Result<TermAnnId> {
        let binder_out_ann = self.mk(binder).tyck(tycker, PatternAction::syn())?;
        match binder_out_ann.annotation {
            | PatAnnId::Kind(_) => {
                tycker.err(TyckError::Expressivity("kind quantification is not supported"))
            }
            | PatAnnId::Type(tpat, kd_1) => self.check_pi_syn_type_binder(tycker, tpat, kd_1, body),
            | PatAnnId::Value(_, _) => self
                .mk(ComputationPiFormation { binder: binder_out_ann, codomain: body })
                .tyck(tycker, Action::syn()),
        }
    }

    /// Synthesize the body of a pi over a type binder: a kind body forms a
    /// kind arrow, a computation-type body a universal type.
    fn check_pi_syn_type_binder(
        &self, tycker: &mut Tycker<'_>, tpat: ss::TPatId, kd_1: ss::KindId, body: TelescopeTail,
    ) -> Result<TermAnnId> {
        let abst = Alloc::alloc(tycker, tpat, (), &());
        let subst_vec = {
            let mut subst_vec = Vec::new();
            if let (Some(def), kd) = tpat.try_destruct_def(tycker) {
                let ty_abst = Alloc::alloc(tycker, abst, kd, &());
                subst_vec.push((def, ty_abst.into()));
            }
            subst_vec
        };
        let body = self.mk_add(subst_vec, body).tyck(tycker, Action::syn())?;
        match body {
            | TermAnnId::Kind(kd_2) => {
                let id = Alloc::alloc(tycker, ss::Arrow(kd_1, kd_2), (), &());
                Ok(TermAnnId::Kind(id))
            }
            | TermAnnId::Type(ty_2, kd_2) => {
                self.check_pi_syn_forall(tycker, tpat, abst, ty_2, kd_2)
            }
            | TermAnnId::Hole(_) => tycker.err(TyckError::MissingAnnotation),
            | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                tycker.err(TyckError::SortMismatch)
            }
        }
    }

    /// Form a universal type over a synthesized computation-type body.
    fn check_pi_syn_forall(
        &self, tycker: &mut Tycker<'_>, tpat: ss::TPatId, abst: ss::AbstId, ty_2: ss::TypeId,
        kd_2: ss::KindId,
    ) -> Result<TermAnnId> {
        match tycker.kind_filled(&kd_2)?.to_owned() {
            | ss::Kind::VType(_) => {
                tycker.err(TyckError::Expressivity("value-level universal types use `val pi`"))?
            }
            | ss::Kind::CType(_) => {
                let id = Alloc::alloc(
                    tycker,
                    ss::Forall(ss::TypeBinder { pattern: tpat, witness: abst }, ty_2),
                    kd_2,
                    &(),
                );
                Ok(TermAnnId::Type(id, kd_2))
            }
            | ss::Kind::Arrow(_) | ss::Kind::Label(_) => tycker.err(TyckError::KindMismatch),
        }
    }

    /// Check a pi against `CType`: a type binder forms a universal type, a
    /// value binder a computation arrow.
    fn check_pi_ana_ctype(
        &self, tycker: &mut Tycker<'_>, binder: su::PatId, body: TelescopeTail,
    ) -> Result<TermAnnId> {
        // synthesize the binder
        let binder_out_ann = self.mk(binder).tyck(tycker, PatternAction::syn())?;
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
                        let ty_abst = Alloc::alloc(tycker, abst, kd, &());
                        subst_vec.push((def, ty_abst.into()));
                    }
                    subst_vec
                };
                let ty_2 = self.mk_add(subst_vec, body).tyck(tycker, Action::ana(ctype.into()))?;
                let (ty_2, _ctype) = ty_2.try_as_type(tycker, TyckError::SortMismatch)?;
                let binder = ss::TypeBinder { pattern: tpat, witness: abst };
                let forall = Alloc::alloc(tycker, ss::Forall(binder, ty_2), ctype, &());
                Ok(TermAnnId::Type(forall, ctype))
            }
            | PatAnnId::Value(_, _) => {
                let ctype = ss::CType.build(tycker, &self.info);
                self.mk(ComputationPiFormation { binder: binder_out_ann, codomain: body })
                    .tyck(tycker, Action::ana(ctype.into()))
            }
        }
    }

    /// Check a pi against a kind arrow, forming a kind arrow.
    fn check_pi_ana_kind_arrow(
        &self, tycker: &mut Tycker<'_>, binder: su::PatId, body: TelescopeTail,
        kd_arr: ss::Arrow<ss::KindId, ss::KindId>,
    ) -> Result<TermAnnId> {
        let ss::Arrow(kd_1, kd_2) = kd_arr;
        // ana binder with kd_1
        let binder_out_ann = self.mk(binder).tyck(tycker, PatternAction::ana(kd_1.into()))?;
        let (_, kd_1) = binder_out_ann.try_as_type(tycker, TyckError::SortMismatch)?;
        // ana body with kd_2
        let body = self.mk(body).tyck(tycker, Action::ana(kd_2.into()))?;
        let kd_2 = body.try_as_kind(tycker, TyckError::SortMismatch)?;
        let arr = Alloc::alloc(tycker, ss::Arrow(kd_1, kd_2), (), &());
        Ok(TermAnnId::Kind(arr))
    }
}
