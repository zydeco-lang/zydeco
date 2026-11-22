//! Data and codata declarations, construction, matching, and observation.

use super::*;

impl TermChecker<'_> {
    pub(super) fn check_data<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Data, switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        Ok({
            let su::Data { arms } = term;
            let vtype = ss::VType.build(tycker, &self.info);
            let vtype = match switch {
                | Switch::Syn => vtype,
                | Switch::Ana(ann) => {
                    let AnnId::Kind(ann_kd) = ann else { tycker.err(TyckError::SortMismatch)? };
                    Lub::lub(vtype, ann_kd, tycker)?
                }
            };
            let arms = arms
                .into_iter()
                .map(|su::DataArm { name, param }| {
                    let checked = self.mk(param).tyck(tycker, Action::ana(vtype.into()))?;
                    let TermAnnId::Type(ty, _) = checked else {
                        tycker.err(TyckError::SortMismatch)?
                    };
                    Ok((name, ty))
                })
                .collect_reported::<Vec<_>>()?;
            let data_id = tycker.statics.datas.alloc(ss::Data::new(arms));
            let ty_id = Alloc::alloc(tycker, data_id, vtype, &());
            TermAnnId::Type(ty_id, vtype)
        })
    }

    pub(super) fn check_co_data<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::CoData, switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        Ok({
            let su::CoData { arms } = term;
            let ctype = ss::CType.build(tycker, &self.info);
            let ctype = match switch {
                | Switch::Syn => ctype,
                | Switch::Ana(ann) => {
                    let AnnId::Kind(ann_kd) = ann else { tycker.err(TyckError::SortMismatch)? };
                    Lub::lub(ctype, ann_kd, tycker)?
                }
            };
            let arms = arms
                .into_iter()
                .map(|su::CoDataArm { name, out }| {
                    let checked = self.mk(out).tyck(tycker, Action::ana(ctype.into()))?;
                    let TermAnnId::Type(ty, _) = checked else {
                        tycker.err(TyckError::SortMismatch)?
                    };
                    Ok((name, ty))
                })
                .collect_reported::<Vec<_>>()?;
            let codata_id = tycker.statics.codatas.alloc(ss::CoData::new(arms));
            let ty_id = Alloc::alloc(tycker, codata_id, ctype, &());
            TermAnnId::Type(ty_id, ctype)
        })
    }

    pub(super) fn check_ctor<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Ctor<CtorName, su::TermId>,
        switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        Ok({
            let su::Ctor(ctor, arg) = term;
            let ana_ty = match switch {
                | Switch::Syn => tycker.err(TyckError::MissingAnnotation)?,
                | Switch::Ana(ann) => ann,
            };
            let AnnId::Type(ana_ty) = ana_ty else { tycker.err(TyckError::SortMismatch)? };
            let ana_ty_unroll = ana_ty.unroll(tycker)?.subst_env(tycker, &self.info)?;
            let ss::Type::Data(data_id) = tycker.type_filled(&ana_ty_unroll)? else {
                tycker.err(TyckError::TypeExpected {
                    expected: "data type definition".to_string(),
                    found: ana_ty_unroll,
                })?
            };
            let arg_ty = match tycker.statics.datas[&data_id].get(&ctor) {
                | Some(ty) => ty.to_owned(),
                | None => tycker.err(TyckError::UnknownDataConstructor(ctor))?,
            };
            let arg_out_ann = self.mk(arg).tyck(tycker, Action::ana(arg_ty.into()))?;
            let TermAnnId::Value(arg, _arg_ty) = arg_out_ann else { unreachable!() };
            let id = Alloc::alloc(tycker, ss::Ctor(ctor, arg), ana_ty, &());
            // hint the ctor to be associated with the definition name
            tycker.statics.data_hints.insert_new(id, data_id);
            TermAnnId::Value(id, ana_ty)
        })
    }

    pub(super) fn check_match<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Match<su::TermId, su::PatId, su::TermId>,
        switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        Ok({
            let su::Match { scrut, arms } = term;
            let scrut_out_ann = self.mk(scrut).tyck(tycker, Action::syn())?;
            let (scrut, scrut_ty) = scrut_out_ann.try_as_value(tycker, TyckError::SortMismatch)?;
            let scrut_ty_unroll = scrut_ty.unroll(tycker)?.subst_env(tycker, &self.info)?;
            // hint the scrut to be associated with the data type
            match tycker.type_filled(&scrut_ty_unroll)? {
                | ss::Type::Data(data_id) => {
                    let _ = tycker.statics.data_hints.upsert(scrut, data_id);
                }
                | _ => {}
            }
            let mut result_type = None;
            let matchers = arms
                .into_iter()
                .map(|su::Matcher { binder, tail }| {
                    let binding =
                        self.mk(binder).tyck(tycker, PatternAction::ana(scrut_ty_unroll.into()))?;
                    let (binder, _) = binding.try_as_value(tycker, TyckError::SortMismatch)?;
                    let action = match switch {
                        | Switch::Syn => Action::syn(),
                        | Switch::Ana(annotation) => Action::ana(annotation),
                    };
                    let source_tail = tail;
                    let tail = TyEnvT::new(binding.info.clone(), tail).tyck(tycker, action)?;
                    let ty = match tail {
                        | TermAnnId::Value(_, ty) | TermAnnId::Compu(_, ty) => ty,
                        | _ => tycker.err(TyckError::SortMismatch)?,
                    };
                    let joined = tycker.guarded(|tycker| {
                        tycker.tasks.push(TyckTask::Term(source_tail, switch));
                        binding.close_scope(tycker, ty)?;
                        match result_type {
                            | Some(previous) => Lub::lub(previous, ty, tycker),
                            | None => Ok(ty),
                        }
                    })?;
                    result_type = Some(joined);
                    Ok((binder, tail))
                })
                .collect_reported::<Vec<_>>()?;
            let ty = match (result_type, switch) {
                | (Some(ty), _) | (None, Switch::Ana(AnnId::Type(ty))) => ty,
                | (None, Switch::Syn) => tycker.err(TyckError::MissingAnnotation)?,
                | _ => tycker.err(TyckError::SortMismatch)?,
            };
            let kind = tycker.statics.type_kind(ty);
            if matches!(tycker.kind_filled(&kind)?, ss::Kind::VType(_)) {
                let arms: Vec<_> = matchers
                    .into_iter()
                    .map(|(binder, tail)| {
                        let TermAnnId::Value(tail, _) = tail else {
                            unreachable!("arm classifiers have the same kind")
                        };
                        ss::Matcher { binder, tail }
                    })
                    .collect();
                let value =
                    Alloc::alloc(tycker, ss::Value::Match(ss::Match { scrut, arms }), ty, &());
                TermAnnId::Value(value, ty)
            } else {
                let arms: Vec<_> = matchers
                    .into_iter()
                    .map(|(binder, tail)| {
                        let TermAnnId::Compu(tail, _) = tail else {
                            unreachable!("arm classifiers have the same kind")
                        };
                        ss::Matcher { binder, tail }
                    })
                    .collect();
                let id = Alloc::alloc(tycker, ss::Match { scrut, arms }, ty, &());
                TermAnnId::Compu(id, ty)
            }
        })
    }

    pub(super) fn check_co_match_clauses<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::CoMatchClauses, switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        Ok({
            let expected = match switch {
                | Switch::Syn => tycker.err(TyckError::MissingAnnotation)?,
                | Switch::Ana(AnnId::Type(expected)) => expected,
                | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                    tycker.err(TyckError::SortMismatch)?
                }
            };
            let computation = CopatternElaborator::new(self.inner, term, expected, &self.info)
                .elaborate(tycker)?;
            TermAnnId::Compu(computation, expected)
        })
    }

    pub(super) fn check_co_match<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::CoMatch<DtorName, su::TermId>,
        switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        Ok({
            let su::CoMatch { arms: comatchers } = term;
            let ana_ty = match switch {
                | Switch::Syn => tycker.err(TyckError::MissingAnnotation)?,
                | Switch::Ana(ana) => match ana {
                    | AnnId::Set | AnnId::Kind(_) => tycker.err(TyckError::SortMismatch)?,
                    | AnnId::Type(ana_ty) => ana_ty,
                },
            };
            let ana_ty_unroll = ana_ty.unroll(tycker)?.subst_env(tycker, &self.info)?;
            let ss::Type::CoData(codata_id) = tycker.type_filled(&ana_ty_unroll)? else {
                tycker.err(TyckError::TypeExpected {
                    expected: "codata type definition".to_string(),
                    found: ana_ty_unroll,
                })?
            };
            let arms = tycker.statics.codatas[&codata_id].clone();
            let comatchers_new = comatchers
                .into_iter()
                .map(|su::CoMatcher { dtor, tail }| {
                    let arm_ty = match arms.get(&dtor) {
                        | Some(arm_ty) => arm_ty,
                        | None => tycker.err(TyckError::UnknownCoDataDestructor(dtor))?,
                    };
                    let tail_out_ann = self.mk(tail).tyck(tycker, Action::ana(arm_ty.into()))?;
                    let TermAnnId::Compu(tail, _ty) = tail_out_ann else {
                        tycker.err(TyckError::SortMismatch)?
                    };
                    Ok(ss::CoMatcher { dtor, tail })
                })
                .collect_reported::<Vec<_>>()?;
            let id = Alloc::alloc(tycker, ss::CoMatch { arms: comatchers_new }, ana_ty, &());
            // hint the whole computation to be associated with the codata type
            tycker.statics.codata_hints.insert_new(id, codata_id);
            TermAnnId::Compu(id, ana_ty)
        })
    }

    pub(super) fn check_dtor<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Dtor<su::TermId, DtorName>,
        switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        Ok({
            let su::Dtor(body, dtor) = term;
            let body_out_ann = self.mk(body).tyck(tycker, Action::syn())?;
            let TermAnnId::Compu(body, ty_body) = body_out_ann else {
                tycker.err(TyckError::SortMismatch)?
            };
            let ty_body_unroll = ty_body.unroll(tycker)?.subst_env(tycker, &self.info)?;
            let ss::Type::CoData(codata_id) = tycker.type_filled(&ty_body_unroll)? else {
                tycker.err(TyckError::TypeExpected {
                    expected: "codata type definition".to_string(),
                    found: ty_body_unroll,
                })?
            };
            // hint the body to be associated with the codata type
            let _ = tycker.statics.codata_hints.upsert(body, codata_id);
            let whole_ty = match tycker.statics.codatas[&codata_id].get(&dtor) {
                | Some(ty) => ty.to_owned(),
                | None => tycker.err(TyckError::UnknownCoDataDestructor(dtor))?,
            };
            match switch {
                | Switch::Syn => {
                    let id = Alloc::alloc(tycker, ss::Dtor(body, dtor), whole_ty, &());
                    TermAnnId::Compu(id, whole_ty)
                }
                | Switch::Ana(ana) => {
                    let AnnId::Type(ana_ty) = ana else { tycker.err(TyckError::SortMismatch)? };
                    let whole_ty = Lub::lub(ana_ty, whole_ty, tycker)?;
                    let id = Alloc::alloc(tycker, ss::Dtor(body, dtor), whole_ty, &());
                    TermAnnId::Compu(id, whole_ty)
                }
            }
        })
    }
}
