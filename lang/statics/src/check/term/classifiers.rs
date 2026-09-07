//! Formation of value and computation function classifiers.

use super::*;

impl TermChecker<'_> {
    pub(super) fn check_val_pi_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::ValPi, switch: Switch<AnnId>,
        prepared_environment: Option<ss::TyEnv>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::ValPi(binder, codomain) = term;
            let binder = self.mk(binder).tyck_k(tycker, PatternAction::syn())?;
            self.mk(ValuePiFormation { binder, codomain })
                .tyck_k(tycker, Action::forward(switch, prepared_environment.as_ref()))?
        })
    }

    pub(super) fn check_pi_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Pi, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::Pi(binder, body) = term;
            match switch {
                | Switch::Syn => {
                    let binder_out_ann = self.mk(binder).tyck_k(tycker, PatternAction::syn())?;
                    match binder_out_ann.annotation {
                        | PatAnnId::Kind(_) => tycker.err_k(
                            TyckError::Expressivity("kind quantification is not supported"),
                            std::panic::Location::caller(),
                        )?,
                        | PatAnnId::Type(tpat, kd_1) => {
                            let abst = Alloc::alloc(tycker, tpat, (), &());
                            let subst_vec = {
                                let mut subst_vec = Vec::new();
                                if let (Some(def), kd) = tpat.try_destruct_def(tycker) {
                                    let ty_abst = Alloc::alloc(tycker, abst, kd, &self.info);
                                    subst_vec.push((def, ty_abst.into()));
                                }
                                subst_vec
                            };
                            let body =
                                self.mk_add(subst_vec, body).tyck_k(tycker, Action::syn())?;
                            match body {
                                | TermAnnId::Kind(kd_2) => {
                                    let term =
                                        crate::query::InternedTerm::new(tycker.db, self.inner);
                                    let input = crate::query::InternedPiSyn::new(
                                        tycker.db,
                                        crate::query::PiSynArm::KindArrow { kd_1, kd_2 },
                                        tpat,
                                        abst,
                                    );
                                    let Some(crate::query::PiSynOutcome::Kind { id, kind }) =
                                        crate::query::pi_syn_judgment(
                                            tycker.db,
                                            tycker.data,
                                            term,
                                            input,
                                            tycker.site_occurrence(),
                                        )
                                    else {
                                        unreachable!(
                                            "the kind arrow of pi judgments is query-produced"
                                        )
                                    };
                                    tycker
                                        .statics
                                        .kinds_pre
                                        .insert_new(id, ss::Fillable::Done(kind));
                                    TermAnnId::Kind(id)
                                }
                                | TermAnnId::Type(ty_2, kd_2) => {
                                    let arm = match tycker.kind_filled_k(&kd_2)?.to_owned() {
                                        | ss::Kind::VType(_) => tycker.err_k(
                                            TyckError::Expressivity(
                                                "value-level universal types use `val pi`",
                                            ),
                                            std::panic::Location::caller(),
                                        )?,
                                        | ss::Kind::CType(_) => {
                                            crate::query::PiSynArm::Forall { ty_2, kd_2 }
                                        }
                                        | ss::Kind::Arrow(_) | ss::Kind::Label(_) => {
                                            crate::query::PiSynArm::KindMismatch
                                        }
                                    };
                                    let term =
                                        crate::query::InternedTerm::new(tycker.db, self.inner);
                                    let input = crate::query::InternedPiSyn::new(
                                        tycker.db, arm, tpat, abst,
                                    );
                                    match crate::query::pi_syn_judgment(
                                        tycker.db,
                                        tycker.data,
                                        term,
                                        input,
                                        tycker.site_occurrence(),
                                    ) {
                                        | Some(crate::query::PiSynOutcome::Type { id, ty, kd }) => {
                                            tycker.statics.types_pre.insert_new(
                                                id,
                                                ss::Fillable::Done(ty),
                                                kd,
                                            );
                                            tycker.store_env(id, &self.info);
                                            TermAnnId::Type(id, kd)
                                        }
                                        | Some(crate::query::PiSynOutcome::Error(error)) => {
                                            tycker.err_k(error, std::panic::Location::caller())?
                                        }
                                        | _ => unreachable!(
                                            "the type arm of pi judgments is query-produced"
                                        ),
                                    }
                                }
                                | TermAnnId::Hole(_) => {
                                    let term =
                                        crate::query::InternedTerm::new(tycker.db, self.inner);
                                    let input = crate::query::InternedPiSyn::new(
                                        tycker.db,
                                        crate::query::PiSynArm::MissingAnnotation,
                                        tpat,
                                        abst,
                                    );
                                    let Some(crate::query::PiSynOutcome::Error(error)) =
                                        crate::query::pi_syn_judgment(
                                            tycker.db,
                                            tycker.data,
                                            term,
                                            input,
                                            tycker.site_occurrence(),
                                        )
                                    else {
                                        unreachable!(
                                            "the hole arm of pi judgments is query-produced"
                                        )
                                    };
                                    tycker.err_k(error, std::panic::Location::caller())?
                                }
                                | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                                    let term =
                                        crate::query::InternedTerm::new(tycker.db, self.inner);
                                    let input = crate::query::InternedPiSyn::new(
                                        tycker.db,
                                        crate::query::PiSynArm::SortMismatch,
                                        tpat,
                                        abst,
                                    );
                                    let Some(crate::query::PiSynOutcome::Error(error)) =
                                        crate::query::pi_syn_judgment(
                                            tycker.db,
                                            tycker.data,
                                            term,
                                            input,
                                            tycker.site_occurrence(),
                                        )
                                    else {
                                        unreachable!(
                                            "the sort arm of pi judgments is query-produced"
                                        )
                                    };
                                    tycker.err_k(error, std::panic::Location::caller())?
                                }
                            }
                        }
                        | PatAnnId::Value(_, _) => self
                            .mk(ComputationPiFormation { binder: binder_out_ann, codomain: body })
                            .tyck_k(tycker, Action::syn())?,
                    }
                }
                | Switch::Ana(ana) => {
                    match ana {
                        | AnnId::Set => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                        | AnnId::Kind(kd) => {
                            match tycker.kind_filled_k(&kd)?.to_owned() {
                                | ss::Kind::VType(_) => tycker.err_k(
                                    TyckError::Expressivity(
                                        "use `val pi` to classify a value function",
                                    ),
                                    std::panic::Location::caller(),
                                )?,
                                | ss::Kind::CType(ss::CType) => {
                                    // could be forall or type arrow
                                    // synthesize the binder
                                    let binder_out_ann =
                                        self.mk(binder).tyck_k(tycker, PatternAction::syn())?;
                                    match binder_out_ann.annotation {
                                        | PatAnnId::Kind(_) => tycker.err_k(
                                            TyckError::Expressivity(
                                                "kind quantification is not supported",
                                            ),
                                            std::panic::Location::caller(),
                                        )?,
                                        | PatAnnId::Type(tpat, _kd_1) => {
                                            // forall
                                            let ctype = ss::CType.build(tycker, &self.info);
                                            let abst = Alloc::alloc(tycker, tpat, (), &());
                                            let subst_vec = {
                                                let mut subst_vec = Vec::new();
                                                if let (Some(def), kd) =
                                                    tpat.try_destruct_def(tycker)
                                                {
                                                    let ty_abst =
                                                        Alloc::alloc(tycker, abst, kd, &self.info);
                                                    subst_vec.push((def, ty_abst.into()));
                                                }
                                                subst_vec
                                            };
                                            let ty_2 = self
                                                .mk_add(subst_vec, body)
                                                .tyck_k(tycker, Action::ana(ctype.into()))?;
                                            let (ty_2, _ctype) = ty_2.try_as_type(
                                                tycker,
                                                TyckError::SortMismatch,
                                                std::panic::Location::caller(),
                                            )?;
                                            let binder =
                                                ss::TypeBinder { pattern: tpat, witness: abst };
                                            let forall = Alloc::alloc(
                                                tycker,
                                                ss::Forall(binder, ty_2),
                                                ctype,
                                                &self.info,
                                            );
                                            TermAnnId::Type(forall, ctype)
                                        }
                                        | PatAnnId::Value(_, _) => {
                                            let ctype = ss::CType.build(tycker, &self.info);
                                            self.mk(ComputationPiFormation {
                                                binder: binder_out_ann,
                                                codomain: body,
                                            })
                                            .tyck_k(tycker, Action::ana(ctype.into()))?
                                        }
                                    }
                                }
                                | ss::Kind::Arrow(kd_arr) => {
                                    // kind arrow
                                    let ss::Arrow(kd_1, kd_2) = kd_arr;
                                    // ana binder with kd_1
                                    let binder_out_ann = self
                                        .mk(binder)
                                        .tyck_k(tycker, PatternAction::ana(kd_1.into()))?;
                                    let (_, kd_1) = binder_out_ann.try_as_type(
                                        tycker,
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?;
                                    // ana body with kd_2
                                    let body =
                                        self.mk(body).tyck_k(tycker, Action::ana(kd_2.into()))?;
                                    let kd_2 = body.try_as_kind(
                                        tycker,
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?;
                                    let arr = Alloc::alloc(tycker, ss::Arrow(kd_1, kd_2), (), &());
                                    TermAnnId::Kind(arr)
                                }
                                | ss::Kind::Label(_) => tycker.err_k(
                                    TyckError::KindMismatch,
                                    std::panic::Location::caller(),
                                )?,
                            }
                        }
                        | AnnId::Type(_) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                    }
                }
            }
        })
    }
}
