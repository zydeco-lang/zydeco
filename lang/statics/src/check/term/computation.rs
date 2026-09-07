//! CBPV suspension, forcing, return, sequencing, and local value bindings.

use super::*;

impl TermChecker<'_> {
    pub(super) fn check_thunk_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Thunk<su::TermId>, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::Thunk(body) = term;
            let ana = match switch {
                | Switch::Syn => tycker.thk_hole(&self.info, self.inner).into(),
                | Switch::Ana(ana) => ana,
            };
            let AnnId::Type(ana_ty) = ana else {
                tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
            };
            let thunk_app_hole = tycker.thk_hole(&self.info, body);
            let ty = Lub::lub_k(ana_ty, thunk_app_hole, tycker)?;
            let ss::Type::App(thunk_app_body_ty) = tycker.type_filled_k(&ty)?.to_owned() else {
                unreachable!()
            };
            let ss::App(_thunk_ty, body_ty) = thunk_app_body_ty;
            let body_out_ann =
                self.mk(body).tyck_k(tycker, Action::ana_prepared(body_ty.into(), &self.info))?;
            let (body_out, body_ty) = body_out_ann.try_as_compu(
                tycker,
                TyckError::SortMismatch,
                std::panic::Location::caller(),
            )?;
            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
            let body_interned =
                crate::query::InternedTermAnn::new(tycker.db, TermAnnId::Compu(body_out, body_ty));
            let Some(outcome) = crate::query::thunk_judgment(
                tycker.db,
                tycker.data,
                term,
                body_interned,
                tycker.site_occurrence(),
            ) else {
                unreachable!("thunk judgments are query-produced")
            };
            tycker.statics.types_pre.insert_new(
                outcome.thk_ty_id,
                ss::Fillable::Done(outcome.thk_ty),
                outcome.vtype,
            );
            tycker.store_env(outcome.thk_ty_id, &self.info);
            tycker.statics.values.insert_new(outcome.thunk_id, outcome.thunk);
            tycker.statics.annotations_value.insert_new(outcome.thunk_id, outcome.thk_ty_id);
            tycker.statics.env_value.insert_new(outcome.thunk_id, self.info.clone());
            TermAnnId::Value(outcome.thunk_id, outcome.thk_ty_id)
        })
    }

    pub(super) fn check_force_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Force<su::TermId>, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::Force(body) = term;
            let body_ty = {
                match switch {
                    | Switch::Syn => {
                        // if syn, then ana the body with thunk_app_hole

                        tycker.thk_hole(&self.info, body)
                    }
                    | Switch::Ana(ana) => {
                        let ana_ty = match ana {
                            | AnnId::Set | AnnId::Kind(_) => tycker
                                .err_k(TyckError::SortMismatch, std::panic::Location::caller())?,
                            | AnnId::Type(ty) => ty,
                        };
                        // check ana_ty is computation type
                        let ctype = ss::CType.build(tycker, &self.info);
                        let ana_ty_kd = tycker.statics.type_kind(ana_ty);
                        Lub::lub_k(ctype, ana_ty_kd, tycker)?;
                        // if ana, then ana the body with thunked body_ty
                        cs::Thk(ana_ty).build(tycker, &self.info)
                    }
                }
            };
            let (body, body_ty) = {
                let body_out_ann = self
                    .mk(body)
                    .tyck_k(tycker, Action::ana_prepared(body_ty.into(), &self.info))?;
                let (body_out, body_ty) = body_out_ann.try_as_value(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                (body_out, body_ty)
            };
            let force_ty = {
                let ss::Type::App(thunk_app_body_ty) = tycker.type_filled_k(&body_ty)?.to_owned()
                else {
                    unreachable!()
                };
                let ss::App(_thunk_ty, force_ty) = thunk_app_body_ty;
                force_ty
            };
            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
            let input = crate::query::InternedForceInput::new(tycker.db, body, force_ty);
            let Some(outcome) = crate::query::force_judgment(
                tycker.db,
                tycker.data,
                term,
                input,
                tycker.site_occurrence(),
            ) else {
                unreachable!("force judgments are query-produced")
            };
            tycker.statics.compus.insert_new(outcome.id, outcome.compu);
            tycker.statics.annotations_compu.insert_new(outcome.id, outcome.ann);
            tycker.statics.env_compu.insert_new(outcome.id, self.info.clone());
            TermAnnId::Compu(outcome.id, outcome.ann)
        })
    }

    pub(super) fn check_ret_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Return<su::TermId>, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::Return(body) = term;
            let ana = match switch {
                | Switch::Syn => tycker.ret_hole(&self.info, self.inner).into(),
                | Switch::Ana(ana) => ana,
            };
            let AnnId::Type(ana_ty) = ana else {
                tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
            };
            let ret_app_hole = tycker.ret_hole(&self.info, self.inner);
            let ty = Lub::lub_k(ana_ty, ret_app_hole, tycker)?;
            let ss::Type::App(ret_app_body_ty) = tycker.type_filled_k(&ty)?.to_owned() else {
                unreachable!()
            };
            let ss::App(_ret_ty, body_ty) = ret_app_body_ty;
            let body_out_ann =
                self.mk(body).tyck_k(tycker, Action::ana_prepared(body_ty.into(), &self.info))?;
            let (body_out, body_ty) = body_out_ann.try_as_value(
                tycker,
                TyckError::SortMismatch,
                std::panic::Location::caller(),
            )?;
            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
            let body_interned =
                crate::query::InternedTermAnn::new(tycker.db, TermAnnId::Value(body_out, body_ty));
            let Some(outcome) = crate::query::ret_judgment(
                tycker.db,
                tycker.data,
                term,
                body_interned,
                tycker.site_occurrence(),
            ) else {
                unreachable!("return judgments are query-produced")
            };
            tycker.statics.types_pre.insert_new(
                outcome.ret_ty_id,
                ss::Fillable::Done(outcome.ret_ty),
                outcome.ctype,
            );
            tycker.store_env(outcome.ret_ty_id, &self.info);
            tycker.statics.compus.insert_new(outcome.ret_id, outcome.ret);
            tycker.statics.annotations_compu.insert_new(outcome.ret_id, outcome.ret_ty_id);
            tycker.statics.env_compu.insert_new(outcome.ret_id, self.info.clone());
            TermAnnId::Compu(outcome.ret_id, outcome.ret_ty_id)
        })
    }

    pub(super) fn check_do_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Bind<su::PatId, su::TermId, su::TermId>,
        switch: Switch<AnnId>, prepared_environment: Option<ss::TyEnv>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::Bind { binder, bindee, tail } = term;
            // first, ana bindee with ret_app_hole, and we get a compu that should be ret_app_body_ty
            let (bindee_out, bindee_ty) = {
                let ret_app_hole = tycker.ret_hole(&self.info, bindee);
                let bindee_out_ann =
                    self.mk(bindee).tyck_k(tycker, Action::ana(ret_app_hole.into()))?;
                bindee_out_ann.try_as_compu(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?
            };
            // then we get the binder_ty from bindee_ty and ana binder with it
            let ss::Type::App(ret_app_binder_ty) = tycker.type_filled_k(&bindee_ty)?.to_owned()
            else {
                unreachable!()
            };
            let ss::App(_ret_ty, binder_ty) = ret_app_binder_ty;
            let binder_elaboration =
                self.mk(binder).tyck_k(tycker, PatternAction::ana(binder_ty.into()))?;
            let (binder_out, _binder_ty) = binder_elaboration.as_value();
            ValuePatternShape::require_binding_k(tycker, binder, binder_out)?;
            // finally, we tyck the tail
            let (tail_out, tail_ty) = {
                let tail_out_ann = TyEnvT::new(binder_elaboration.info.clone(), tail)
                    .tyck_k(tycker, Action::forward(switch, prepared_environment.as_ref()))?;
                tail_out_ann.try_as_compu(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?
            };
            binder_elaboration.close_scope_k(tycker, tail_ty)?;
            let bind_ty = tail_ty;
            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
            let input = crate::query::InternedDoInput::new(
                tycker.db, binder_out, bindee_out, tail_out, bind_ty,
            );
            let Some(outcome) = crate::query::do_judgment(
                tycker.db,
                tycker.data,
                term,
                input,
                tycker.site_occurrence(),
            ) else {
                unreachable!("bind judgments are query-produced")
            };
            tycker.statics.compus.insert_new(outcome.id, outcome.compu);
            tycker.statics.annotations_compu.insert_new(outcome.id, outcome.ann);
            tycker.statics.env_compu.insert_new(outcome.id, self.info.clone());
            TermAnnId::Compu(outcome.id, outcome.ann)
        })
    }

    pub(super) fn check_let_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Let<su::PatId, su::TermId, su::TermId>,
        switch: Switch<AnnId>, prepared_environment: Option<ss::TyEnv>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::Let { binder, bindee, tail } = term;
            let (bindee, is_sealed) = match bindee.syntactically_sealed(tycker) {
                | Some(bindee) => (bindee, true),
                | None => (bindee, false),
            };
            // first, synthesize bindee
            let bindee_out_ann = self.mk(bindee).tyck_k(tycker, Action::syn())?;
            match bindee_out_ann {
                | TermAnnId::Type(bindee_out, bindee_kd) => {
                    // a type alias
                    // then, ana binder with bindee_kd
                    let binder_out_ann =
                        self.mk(binder).tyck_k(tycker, PatternAction::ana(bindee_kd.into()))?;
                    let (binder_out, _binder_kd) = binder_out_ann.as_type();
                    if let (Some(def), _) = binder_out.try_destruct_def(tycker) {
                        let _ = tycker.statics.type_definitions.upsert(def, bindee_out);
                    }
                    let bindee_out = if is_sealed {
                        let abst: AbstId = tycker.fresh();
                        tycker.statics.absts.insert_new(abst, ());
                        if let (Some(def), _) = binder_out.try_destruct_def(tycker) {
                            tycker.statics.abst_hints.insert_new(abst, def);
                        }
                        tycker.record_seal(abst, bindee_out);
                        Alloc::alloc(tycker, abst, bindee_kd, &self.info)
                    } else {
                        bindee_out
                    };
                    // and then assign bindee_out to binder_out;
                    // the type is effectively inlined
                    let env = self.mk(Assign(binder_out, bindee_out)).tyck_k(tycker, ())?;
                    match binder_out.try_destruct_def(tycker) {
                        | (Some(def), _) => {
                            // consider adding it to the globals if bindee is global
                            if tycker.statics.global_terms.get(&bindee_out.into()).is_some() {
                                tycker.statics.global_defs.ensure(def);
                            }
                        }
                        | (None, _) => {}
                    }
                    // finally, we tyck the tail
                    let tail_out_ann = env
                        .mk(tail)
                        .tyck_k(tycker, Action::forward(switch, prepared_environment.as_ref()))?;
                    match tail_out_ann {
                        | TermAnnId::Type(tail_out, tail_kd) => {
                            // the resulting type will be the tail
                            TermAnnId::Type(tail_out, tail_kd)
                        }
                        | TermAnnId::Value(tail_out, tail_ty) => {
                            // Type aliases and definitions are static, so the
                            // resulting value is the checked tail itself.
                            TermAnnId::Value(tail_out, tail_ty)
                        }
                        | TermAnnId::Compu(tail_out, tail_ty) => {
                            // the resulting computation will only be the tail
                            TermAnnId::Compu(tail_out, tail_ty)
                        }
                        | TermAnnId::Hole(_) | TermAnnId::Kind(_) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                    }
                }
                | TermAnnId::Kind(bindee_out) => {
                    // a kind alias
                    // Kind binders have no classifier, so check the
                    // binder against the set of kinds.
                    let binder_out_ann =
                        self.mk(binder).tyck_k(tycker, PatternAction::ana(AnnId::Set))?;
                    let ss::PatId::Kind(binder_out) = binder_out_ann.annotation.as_pat() else {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    };
                    let env = self.mk(Assign(binder_out, bindee_out)).tyck_k(tycker, ())?;
                    // finally, we tyck the tail
                    let tail_out_ann = env
                        .mk(tail)
                        .tyck_k(tycker, Action::forward(switch, prepared_environment.as_ref()))?;
                    match tail_out_ann {
                        | TermAnnId::Type(tail_out, tail_kd) => TermAnnId::Type(tail_out, tail_kd),
                        | TermAnnId::Value(tail_out, tail_ty) => {
                            TermAnnId::Value(tail_out, tail_ty)
                        }
                        | TermAnnId::Compu(tail_out, tail_ty) => {
                            TermAnnId::Compu(tail_out, tail_ty)
                        }
                        | TermAnnId::Hole(_) | TermAnnId::Kind(_) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                    }
                }
                | TermAnnId::Value(bindee_out, bindee_ty) => {
                    // a value alias
                    // then, ana binder with bindee_ty
                    let binder_elaboration =
                        self.mk(binder).tyck_k(tycker, PatternAction::ana(bindee_ty.into()))?;
                    let (binder_out, _binder_ty) = binder_elaboration.as_value();
                    match binder_out.try_destruct_def(tycker) {
                        | (Some(def), _) => {
                            let _ = tycker.statics.value_aliases.upsert(def, bindee_out);
                            // consider adding it to the globals if bindee is global
                            if tycker.statics.global_terms.get(&bindee_out.into()).is_some() {
                                tycker.statics.global_defs.ensure(def);
                                // consider adding it to the inlinables as well
                                let _ = tycker.statics.inlinables.upsert(def, bindee_out);
                            }
                        }
                        | (None, _) => {}
                    }
                    // finally, we tyck the tail
                    let tail_out_ann = TyEnvT::new(binder_elaboration.info.clone(), tail)
                        .tyck_k(tycker, Action::forward(switch, prepared_environment.as_ref()))?;
                    match tail_out_ann {
                        | TermAnnId::Value(tail_out, tail_ty) => {
                            if !ValuePatternShape::is_irrefutable(tycker, binder_out) {
                                tycker.err_k(
                                    TyckError::Expressivity(
                                        "value bindings must be irrefutable patterns",
                                    ),
                                    std::panic::Location::caller(),
                                )?
                            }
                            binder_elaboration.close_scope_k(tycker, tail_ty)?;
                            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                            let binder = crate::query::InternedVPat::new(tycker.db, binder_out);
                            let bindee = crate::query::InternedValue::new(tycker.db, bindee_out);
                            let tail = crate::query::InternedTermAnn::new(
                                tycker.db,
                                TermAnnId::Value(tail_out, tail_ty),
                            );
                            let Some(crate::query::LetSynOutcome::Value { id, value, ann }) =
                                crate::query::let_judgment(
                                    tycker.db,
                                    tycker.data,
                                    term,
                                    binder,
                                    bindee,
                                    tail,
                                    tycker.site_occurrence(),
                                )
                            else {
                                unreachable!("the value tail of let judgments is query-produced")
                            };
                            tycker.statics.values.insert_new(id, value);
                            tycker.statics.annotations_value.insert_new(id, ann);
                            tycker.statics.env_value.insert_new(id, self.info.clone());
                            TermAnnId::Value(id, ann)
                        }
                        | TermAnnId::Compu(tail_out, tail_ty) => {
                            binder_elaboration.close_scope_k(tycker, tail_ty)?;
                            ValuePatternShape::require_binding_k(tycker, binder, binder_out)?;
                            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                            let binder = crate::query::InternedVPat::new(tycker.db, binder_out);
                            let bindee = crate::query::InternedValue::new(tycker.db, bindee_out);
                            let tail = crate::query::InternedTermAnn::new(
                                tycker.db,
                                TermAnnId::Compu(tail_out, tail_ty),
                            );
                            let Some(crate::query::LetSynOutcome::Compu { id, compu, ann }) =
                                crate::query::let_judgment(
                                    tycker.db,
                                    tycker.data,
                                    term,
                                    binder,
                                    bindee,
                                    tail,
                                    tycker.site_occurrence(),
                                )
                            else {
                                unreachable!(
                                    "the computation tail of let judgments is query-produced"
                                )
                            };
                            tycker.statics.compus.insert_new(id, compu);
                            tycker.statics.annotations_compu.insert_new(id, ann);
                            tycker.statics.env_compu.insert_new(id, self.info.clone());
                            TermAnnId::Compu(id, ann)
                        }
                        | TermAnnId::Hole(_) | TermAnnId::Kind(_) | TermAnnId::Type(_, _) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                    }
                }
                | TermAnnId::Hole(_) | TermAnnId::Compu(_, _) => {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                }
            }
        })
    }
}
