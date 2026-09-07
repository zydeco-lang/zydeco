//! Data and codata declarations, construction, matching, and observation.

use super::*;

impl TermChecker<'_> {
    pub(super) fn check_data_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Data, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::Data { arms } = term;
            let vtype = ss::VType.build(tycker, &self.info);
            let vtype = match switch {
                | Switch::Syn => vtype,
                | Switch::Ana(ann) => {
                    let AnnId::Kind(ann_kd) = ann else {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    };
                    Lub::lub_k(vtype, ann_kd, tycker)?
                }
            };
            let mut arms_vec = rpds::VectorSync::new_sync();
            for su::DataArm { name, param } in arms {
                let param = self.mk(param).tyck_k(tycker, Action::ana(vtype.into()))?;
                let TermAnnId::Type(ty, _kd) = param else {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                };
                arms_vec.push_back_mut((name, ty));
            }
            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
            let arms_interned = crate::query::InternedDataArms::new(
                tycker.db,
                arms_vec.iter().cloned().collect::<Vec<_>>(),
            );
            let kd_interned = crate::query::InternedKind::new(tycker.db, vtype);
            let Some(outcome) = crate::query::data_syn_judgment(
                tycker.db,
                tycker.data,
                term,
                arms_interned,
                kd_interned,
                tycker.site_occurrence(),
            ) else {
                unreachable!("data declaration judgments are query-produced")
            };
            tycker.statics.datas.insert_new(outcome.data_id, outcome.data);
            tycker.statics.types_pre.insert_new(
                outcome.ty_id,
                ss::Fillable::Done(outcome.ty),
                outcome.kd,
            );
            tycker.store_env(outcome.ty_id, &self.info);
            TermAnnId::Type(outcome.ty_id, outcome.kd)
        })
    }

    pub(super) fn check_co_data_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::CoData, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::CoData { arms } = term;
            let ctype = ss::CType.build(tycker, &self.info);
            let ctype = match switch {
                | Switch::Syn => ctype,
                | Switch::Ana(ann) => {
                    let AnnId::Kind(ann_kd) = ann else {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    };
                    Lub::lub_k(ctype, ann_kd, tycker)?
                }
            };
            let mut arms_vec = rpds::VectorSync::new_sync();
            for su::CoDataArm { name, out } in arms {
                let out = self.mk(out).tyck_k(tycker, Action::ana(ctype.into()))?;
                let TermAnnId::Type(ty, _kd) = out else {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                };
                arms_vec.push_back_mut((name, ty));
            }
            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
            let arms_interned = crate::query::InternedCoDataArms::new(
                tycker.db,
                arms_vec.iter().cloned().collect::<Vec<_>>(),
            );
            let kd_interned = crate::query::InternedKind::new(tycker.db, ctype);
            let Some(outcome) = crate::query::codata_syn_judgment(
                tycker.db,
                tycker.data,
                term,
                arms_interned,
                kd_interned,
                tycker.site_occurrence(),
            ) else {
                unreachable!("codata declaration judgments are query-produced")
            };
            tycker.statics.codatas.insert_new(outcome.codata_id, outcome.codata);
            tycker.statics.types_pre.insert_new(
                outcome.ty_id,
                ss::Fillable::Done(outcome.ty),
                outcome.kd,
            );
            tycker.store_env(outcome.ty_id, &self.info);
            TermAnnId::Type(outcome.ty_id, outcome.kd)
        })
    }

    pub(super) fn check_ctor_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Ctor<CtorName, su::TermId>,
        switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::Ctor(ctor, arg) = term;
            let ana_ty = match switch {
                | Switch::Syn => {
                    tycker.err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?
                }
                | Switch::Ana(ann) => ann,
            };
            let AnnId::Type(ana_ty) = ana_ty else {
                tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
            };
            let ana_ty_unroll = ana_ty.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
            let ss::Type::Data(data_id) = tycker.type_filled_k(&ana_ty_unroll)? else {
                tycker.err_k(
                    TyckError::TypeExpected {
                        expected: "data type definition".to_string(),
                        found: ana_ty_unroll,
                    },
                    std::panic::Location::caller(),
                )?
            };
            let arg_ty = match tycker.statics.datas[&data_id].get(&ctor) {
                | Some(ty) => ty.to_owned(),
                | None => tycker.err_k(
                    TyckError::UnknownDataConstructor(ctor.clone()),
                    std::panic::Location::caller(),
                )?,
            };
            let arg_out_ann = self.mk(arg).tyck_k(tycker, Action::ana(arg_ty.into()))?;
            let TermAnnId::Value(arg, _arg_ty) = arg_out_ann else { unreachable!() };
            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
            let input = crate::query::InternedCtorInput::new(
                tycker.db,
                ctor.to_owned(),
                arg,
                ana_ty,
                data_id,
            );
            let Some(outcome) = crate::query::ctor_syn_judgment(
                tycker.db,
                tycker.data,
                term,
                input,
                tycker.site_occurrence(),
            ) else {
                unreachable!("constructor judgments are query-produced")
            };
            tycker.statics.values.insert_new(outcome.id, outcome.value);
            tycker.statics.annotations_value.insert_new(outcome.id, outcome.ann);
            tycker.statics.env_value.insert_new(outcome.id, self.info.clone());
            // hint the ctor to be associated with the definition name
            tycker.statics.data_hints.insert_new(outcome.id, data_id);
            TermAnnId::Value(outcome.id, outcome.ann)
        })
    }

    pub(super) fn check_match_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Match<su::TermId, su::PatId, su::TermId>,
        switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::Match { scrut, arms } = term;
            let scrut_out_ann = self.mk(scrut).tyck_k(tycker, Action::syn())?;
            let (scrut, scrut_ty) = scrut_out_ann.try_as_value(
                tycker,
                TyckError::SortMismatch,
                std::panic::Location::caller(),
            )?;
            let scrut_ty_unroll = scrut_ty.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
            // hint the scrut to be associated with the data type
            match tycker.type_filled_k(&scrut_ty_unroll)? {
                | ss::Type::Data(data_id) => {
                    let _ = tycker.statics.data_hints.upsert(scrut, data_id);
                }
                | _ => {}
            }
            let mut matchers = Vec::new();
            let mut arms_ty = Vec::new();
            for su::Matcher { binder, tail } in arms {
                let binder_elaboration =
                    self.mk(binder).tyck_k(tycker, PatternAction::ana(scrut_ty_unroll.into()))?;
                let (binder, _ty) = binder_elaboration.try_as_value(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                match switch {
                    | Switch::Syn => {
                        let tail_out_ann = TyEnvT::new(binder_elaboration.info.clone(), tail)
                            .tyck_k(tycker, Action::syn())?;
                        let TermAnnId::Compu(tail, ty) = tail_out_ann else {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        };
                        binder_elaboration.close_scope_k(tycker, ty)?;
                        matchers.push(ss::Matcher { binder, tail });
                        arms_ty.push(ty);
                    }
                    | Switch::Ana(ana_ty) => {
                        let tail_out_ann = TyEnvT::new(binder_elaboration.info.clone(), tail)
                            .tyck_k(tycker, Action::ana(ana_ty))?;
                        let TermAnnId::Compu(tail, ty) = tail_out_ann else {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        };
                        binder_elaboration.close_scope_k(tycker, ty)?;
                        matchers.push(ss::Matcher { binder, tail });
                        arms_ty.push(ty);
                    }
                }
            }
            // Note: use hole
            if arms_ty.is_empty() {
                match switch {
                    | Switch::Syn => tycker
                        .err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?,
                    | Switch::Ana(ana_ty) => match ana_ty {
                        | AnnId::Set | AnnId::Kind(_) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                        | AnnId::Type(ana_ty) => {
                            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                            let input = crate::query::InternedMatchInput::new(
                                tycker.db,
                                scrut,
                                matchers
                                    .iter()
                                    .map(|matcher| (matcher.binder, matcher.tail))
                                    .collect::<Vec<_>>(),
                                ana_ty,
                            );
                            let Some(outcome) = crate::query::match_syn_judgment(
                                tycker.db,
                                tycker.data,
                                term,
                                input,
                                tycker.site_occurrence(),
                            ) else {
                                unreachable!("the empty-arms match judgment is query-produced")
                            };
                            tycker.statics.compus.insert_new(outcome.id, outcome.compu);
                            tycker.statics.annotations_compu.insert_new(outcome.id, outcome.ann);
                            tycker.statics.env_compu.insert_new(outcome.id, self.info.clone());
                            TermAnnId::Compu(outcome.id, outcome.ann)
                        }
                    },
                }
            } else {
                // make sure that each arm has the same type
                let mut iter = arms_ty.into_iter();
                let mut res = iter.next().unwrap();
                for ty in iter {
                    res = Lub::lub_k(res, ty, tycker)?;
                }
                let whole_ty = res;
                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                let input = crate::query::InternedMatchInput::new(
                    tycker.db,
                    scrut,
                    matchers
                        .iter()
                        .map(|matcher| (matcher.binder, matcher.tail))
                        .collect::<Vec<_>>(),
                    whole_ty,
                );
                let Some(outcome) = crate::query::match_syn_judgment(
                    tycker.db,
                    tycker.data,
                    term,
                    input,
                    tycker.site_occurrence(),
                ) else {
                    unreachable!("match judgments are query-produced")
                };
                tycker.statics.compus.insert_new(outcome.id, outcome.compu);
                tycker.statics.annotations_compu.insert_new(outcome.id, outcome.ann);
                tycker.statics.env_compu.insert_new(outcome.id, self.info.clone());
                TermAnnId::Compu(outcome.id, outcome.ann)
            }
        })
    }

    pub(super) fn check_co_match_clauses_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::CoMatchClauses, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let expected = match switch {
                | Switch::Syn => {
                    tycker.err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?
                }
                | Switch::Ana(AnnId::Type(expected)) => expected,
                | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                }
            };
            let computation = CopatternElaborator::new(self.inner, term, expected, &self.info)
                .elaborate_k(tycker)?;
            TermAnnId::Compu(computation, expected)
        })
    }

    pub(super) fn check_co_match_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::CoMatch<DtorName, su::TermId>,
        switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::CoMatch { arms: comatchers } = term;
            let ana_ty = match switch {
                | Switch::Syn => {
                    tycker.err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?
                }
                | Switch::Ana(ana) => match ana {
                    | AnnId::Set | AnnId::Kind(_) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | AnnId::Type(ana_ty) => ana_ty,
                },
            };
            let ana_ty_unroll = ana_ty.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
            let ss::Type::CoData(codata_id) = tycker.type_filled_k(&ana_ty_unroll)? else {
                tycker.err_k(
                    TyckError::TypeExpected {
                        expected: "codata type definition".to_string(),
                        found: ana_ty_unroll,
                    },
                    std::panic::Location::caller(),
                )?
            };
            let arms = tycker.statics.codatas[&codata_id].clone();
            let mut comatchers_new = Vec::new();
            for su::CoMatcher { dtor, tail } in comatchers {
                let arm_ty = match arms.get(&dtor) {
                    | Some(arm_ty) => arm_ty,
                    | None => tycker.err_k(
                        TyckError::UnknownCoDataDestructor(dtor.clone()),
                        std::panic::Location::caller(),
                    )?,
                };
                let tail_out_ann = self.mk(tail).tyck_k(tycker, Action::ana(arm_ty.into()))?;
                let TermAnnId::Compu(tail, _ty) = tail_out_ann else {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                };
                comatchers_new.push(ss::CoMatcher { dtor, tail });
            }
            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
            let input = crate::query::InternedCoMatchInput::new(
                tycker.db,
                comatchers_new
                    .iter()
                    .map(|comatcher| (comatcher.dtor.clone(), comatcher.tail))
                    .collect::<Vec<_>>(),
                ana_ty,
            );
            let Some(outcome) = crate::query::comatch_syn_judgment(
                tycker.db,
                tycker.data,
                term,
                input,
                tycker.site_occurrence(),
            ) else {
                unreachable!("comatch judgments are query-produced")
            };
            tycker.statics.compus.insert_new(outcome.id, outcome.compu);
            tycker.statics.annotations_compu.insert_new(outcome.id, outcome.ann);
            tycker.statics.env_compu.insert_new(outcome.id, self.info.clone());
            // hint the whole computation to be associated with the codata type
            tycker.statics.codata_hints.insert_new(outcome.id, codata_id);
            TermAnnId::Compu(outcome.id, outcome.ann)
        })
    }

    pub(super) fn check_dtor_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Dtor<su::TermId, DtorName>,
        switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::Dtor(body, dtor) = term;
            let body_out_ann = self.mk(body).tyck_k(tycker, Action::syn())?;
            let TermAnnId::Compu(body, ty_body) = body_out_ann else {
                tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
            };
            let ty_body_unroll = ty_body.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
            let ss::Type::CoData(codata_id) = tycker.type_filled_k(&ty_body_unroll)? else {
                tycker.err_k(
                    TyckError::TypeExpected {
                        expected: "codata type definition".to_string(),
                        found: ty_body_unroll,
                    },
                    std::panic::Location::caller(),
                )?
            };
            // hint the body to be associated with the codata type
            let _ = tycker.statics.codata_hints.upsert(body, codata_id);
            let whole_ty = match tycker.statics.codatas[&codata_id].get(&dtor) {
                | Some(ty) => ty.to_owned(),
                | None => tycker.err_k(
                    TyckError::UnknownCoDataDestructor(dtor.clone()),
                    std::panic::Location::caller(),
                )?,
            };
            match switch {
                | Switch::Syn => {
                    let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                    let input =
                        crate::query::InternedDtorInput::new(tycker.db, body, dtor, whole_ty);
                    let Some(outcome) = crate::query::dtor_syn_judgment(
                        tycker.db,
                        tycker.data,
                        term,
                        input,
                        tycker.site_occurrence(),
                    ) else {
                        unreachable!("destructor judgments are query-produced")
                    };
                    tycker.statics.compus.insert_new(outcome.id, outcome.compu);
                    tycker.statics.annotations_compu.insert_new(outcome.id, outcome.ann);
                    tycker.statics.env_compu.insert_new(outcome.id, self.info.clone());
                    TermAnnId::Compu(outcome.id, outcome.ann)
                }
                | Switch::Ana(ana) => {
                    let AnnId::Type(ana_ty) = ana else {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    };
                    let whole_ty = Lub::lub_k(whole_ty, ana_ty, tycker)?;
                    let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                    let input =
                        crate::query::InternedDtorInput::new(tycker.db, body, dtor, whole_ty);
                    let Some(outcome) = crate::query::dtor_syn_judgment(
                        tycker.db,
                        tycker.data,
                        term,
                        input,
                        tycker.site_occurrence(),
                    ) else {
                        unreachable!("destructor judgments are query-produced")
                    };
                    tycker.statics.compus.insert_new(outcome.id, outcome.compu);
                    tycker.statics.annotations_compu.insert_new(outcome.id, outcome.ann);
                    tycker.statics.env_compu.insert_new(outcome.id, self.info.clone());
                    TermAnnId::Compu(outcome.id, outcome.ann)
                }
            }
        })
    }
}
