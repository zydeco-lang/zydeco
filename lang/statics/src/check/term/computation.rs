//! CBPV suspension, forcing, return, sequencing, and local value bindings.

use super::*;

impl TermChecker<'_> {
    pub(super) fn check_thunk<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Thunk<su::TermId>, switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        Ok({
            let su::Thunk(body) = term;
            let ana = match switch {
                | Switch::Syn => tycker.thk_hole(&self.info, self.inner).into(),
                | Switch::Ana(ana) => ana,
            };
            let AnnId::Type(ana_ty) = ana else { tycker.err(TyckError::SortMismatch)? };
            let thunk_app_hole = tycker.thk_hole(&self.info, body);
            let ty = Lub::lub(ana_ty, thunk_app_hole, tycker)?;
            let ss::Type::App(thunk_app_body_ty) = tycker.type_filled(&ty)?.to_owned() else {
                unreachable!()
            };
            let ss::App(_thunk_ty, body_ty) = thunk_app_body_ty;
            let body_out_ann =
                self.mk(body).tyck(tycker, Action::ana_prepared(body_ty.into(), &self.info))?;
            let (body_out, body_ty) = body_out_ann.try_as_compu(tycker, TyckError::SortMismatch)?;
            let thk_ty_id = Alloc::alloc(
                tycker,
                ss::App(tycker.intrinsics.thk, body_ty),
                tycker.intrinsics.vtype,
                &(),
            );
            let thunk_id = Alloc::alloc(tycker, ss::Thunk(body_out), thk_ty_id, &());
            TermAnnId::Value(thunk_id, thk_ty_id)
        })
    }

    pub(super) fn check_force<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Force<su::TermId>, switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
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
                            | AnnId::Set | AnnId::Kind(_) => tycker.err(TyckError::SortMismatch)?,
                            | AnnId::Type(ty) => ty,
                        };
                        // check ana_ty is computation type
                        let ctype = ss::CType.build(tycker, &self.info);
                        let ana_ty_kd = tycker.statics.type_kind(ana_ty);
                        Lub::lub(ctype, ana_ty_kd, tycker)?;
                        // if ana, then ana the body with thunked body_ty
                        cs::Thk(ana_ty).build(tycker, &self.info)
                    }
                }
            };
            let (body, body_ty) = {
                let body_out_ann =
                    self.mk(body).tyck(tycker, Action::ana_prepared(body_ty.into(), &self.info))?;
                let (body_out, body_ty) =
                    body_out_ann.try_as_value(tycker, TyckError::SortMismatch)?;
                (body_out, body_ty)
            };
            let force_ty = {
                let ss::Type::App(thunk_app_body_ty) = tycker.type_filled(&body_ty)?.to_owned()
                else {
                    unreachable!()
                };
                let ss::App(_thunk_ty, force_ty) = thunk_app_body_ty;
                force_ty
            };
            let id = Alloc::alloc(tycker, ss::Force(body), force_ty, &());
            TermAnnId::Compu(id, force_ty)
        })
    }

    pub(super) fn check_ret<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Return<su::TermId>, switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        Ok({
            let su::Return(body) = term;
            let ana = match switch {
                | Switch::Syn => tycker.ret_hole(&self.info, self.inner).into(),
                | Switch::Ana(ana) => ana,
            };
            let AnnId::Type(ana_ty) = ana else { tycker.err(TyckError::SortMismatch)? };
            let ret_app_hole = tycker.ret_hole(&self.info, self.inner);
            let ty = Lub::lub(ana_ty, ret_app_hole, tycker)?;
            let ss::Type::App(ret_app_body_ty) = tycker.type_filled(&ty)?.to_owned() else {
                unreachable!()
            };
            let ss::App(_ret_ty, body_ty) = ret_app_body_ty;
            let body_out_ann =
                self.mk(body).tyck(tycker, Action::ana_prepared(body_ty.into(), &self.info))?;
            let (body_out, body_ty) = body_out_ann.try_as_value(tycker, TyckError::SortMismatch)?;
            let ret_ty_id = Alloc::alloc(
                tycker,
                ss::App(tycker.intrinsics.ret, body_ty),
                tycker.intrinsics.ctype,
                &(),
            );
            let ret_id = Alloc::alloc(tycker, ss::Return(body_out), ret_ty_id, &());
            TermAnnId::Compu(ret_id, ret_ty_id)
        })
    }

    pub(super) fn check_do<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Bind<su::PatId, su::TermId, su::TermId>,
        switch: Switch<AnnId>, prepared_environment: Option<ss::TyEnv>,
    ) -> Result<TermAnnId> {
        Ok({
            let su::Bind { binder, bindee, tail } = term;
            // first, ana bindee with ret_app_hole, and we get a compu that should be ret_app_body_ty
            let (bindee_out, bindee_ty) = {
                let ret_app_hole = tycker.ret_hole(&self.info, bindee);
                let bindee_out_ann =
                    self.mk(bindee).tyck(tycker, Action::ana(ret_app_hole.into()))?;
                bindee_out_ann.try_as_compu(tycker, TyckError::SortMismatch)?
            };
            // then we get the binder_ty from bindee_ty and ana binder with it
            let ss::Type::App(ret_app_binder_ty) = tycker.type_filled(&bindee_ty)?.to_owned()
            else {
                unreachable!()
            };
            let ss::App(_ret_ty, binder_ty) = ret_app_binder_ty;
            let binder_elaboration =
                self.mk(binder).tyck(tycker, PatternAction::ana(binder_ty.into()))?;
            let (binder_out, _binder_ty) = binder_elaboration.as_value();
            ValuePatternShape::require_binding(tycker, binder, binder_out)?;
            // finally, we tyck the tail
            let (tail_out, tail_ty) = {
                let tail_out_ann = TyEnvT::new(binder_elaboration.info.clone(), tail)
                    .tyck(tycker, Action::forward(switch, prepared_environment.as_ref()))?;
                tail_out_ann.try_as_compu(tycker, TyckError::SortMismatch)?
            };
            binder_elaboration.close_scope(tycker, tail_ty)?;
            let id = Alloc::alloc(
                tycker,
                ss::Bind { binder: binder_out, bindee: bindee_out, tail: tail_out },
                tail_ty,
                &(),
            );
            TermAnnId::Compu(id, tail_ty)
        })
    }

    pub(super) fn check_let<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Let<su::PatId, su::TermId, su::TermId>,
        switch: Switch<AnnId>, prepared_environment: Option<ss::TyEnv>,
    ) -> Result<TermAnnId> {
        let su::Let { binder, bindee, tail } = term;
        let (bindee, is_sealed) = match bindee.syntactically_sealed(tycker) {
            | Some(bindee) => (bindee, true),
            | None => (bindee, false),
        };
        let tail_action = Action::forward(switch, prepared_environment.as_ref());
        // first, synthesize bindee
        let bindee_out_ann = self.mk(bindee).tyck(tycker, Action::syn())?;
        let env = match bindee_out_ann {
            | TermAnnId::Type(bindee_out, bindee_kd) => {
                self.bind_type_alias(tycker, binder, bindee_out, bindee_kd, is_sealed)?
            }
            | TermAnnId::Kind(bindee_out) => self.bind_kind_alias(tycker, binder, bindee_out)?,
            | TermAnnId::Value(bindee_out, bindee_ty) => {
                return self.check_let_value(
                    tycker,
                    binder,
                    bindee_out,
                    bindee_ty,
                    tail,
                    tail_action,
                );
            }
            | TermAnnId::Hole(_) | TermAnnId::Compu(_, _) => {
                return tycker.err(TyckError::SortMismatch);
            }
        };
        // finally, we tyck the tail; type and kind aliases are static, so the
        // result is the checked tail itself
        match env.mk(tail).tyck(tycker, tail_action)? {
            | tail_out @ (TermAnnId::Type(_, _)
            | TermAnnId::Value(_, _)
            | TermAnnId::Compu(_, _)) => Ok(tail_out),
            | TermAnnId::Hole(_) | TermAnnId::Kind(_) => tycker.err(TyckError::SortMismatch),
        }
    }

    /// Bind a type alias, sealing it behind a fresh abstract type when the
    /// bindee is syntactically sealed; the type is effectively inlined.
    fn bind_type_alias(
        &self, tycker: &mut Tycker<'_>, binder: su::PatId, bindee_out: ss::TypeId,
        bindee_kd: ss::KindId, is_sealed: bool,
    ) -> Result<TyEnvT<()>> {
        // ana binder with bindee_kd
        let binder_out_ann = self.mk(binder).tyck(tycker, PatternAction::ana(bindee_kd.into()))?;
        let (binder_out, _binder_kd) = binder_out_ann.as_type();
        if let (Some(def), _) = binder_out.try_destruct_def(tycker) {
            let _ = tycker.statics.type_definitions.upsert(def, bindee_out);
        }
        let bindee_out = if is_sealed {
            let abst = Alloc::alloc(tycker, binder_out.try_destruct_def(tycker).0, bindee_kd, &());
            tycker.record_seal(abst, bindee_out);
            Alloc::alloc(tycker, abst, bindee_kd, &())
        } else {
            bindee_out
        };
        // and then assign bindee_out to binder_out
        let env = self.mk(Assign(binder_out, bindee_out)).tyck(tycker, ())?;
        if let (Some(def), _) = binder_out.try_destruct_def(tycker) {
            // consider adding it to the globals if bindee is global
            if tycker.statics.global_terms.contains(&ss::TermId::from(bindee_out)) {
                tycker.statics.global_defs.ensure(def);
            }
        }
        Ok(env)
    }

    /// Bind a kind alias.
    fn bind_kind_alias(
        &self, tycker: &mut Tycker<'_>, binder: su::PatId, bindee_out: ss::KindId,
    ) -> Result<TyEnvT<()>> {
        // Kind binders have no classifier, so check the
        // binder against the set of kinds.
        let binder_out_ann = self.mk(binder).tyck(tycker, PatternAction::ana(AnnId::Set))?;
        let ss::PatId::Kind(binder_out) = binder_out_ann.annotation.as_pat() else {
            tycker.err(TyckError::SortMismatch)?
        };
        self.mk(Assign(binder_out, bindee_out)).tyck(tycker, ())
    }

    /// Bind a value, registering aliases of named binders, then check the tail.
    fn check_let_value(
        &self, tycker: &mut Tycker<'_>, binder: su::PatId, bindee_out: ss::ValueId,
        bindee_ty: ss::TypeId, tail: su::TermId, tail_action: Action<AnnId>,
    ) -> Result<TermAnnId> {
        // ana binder with bindee_ty
        let binder_elaboration =
            self.mk(binder).tyck(tycker, PatternAction::ana(bindee_ty.into()))?;
        let (binder_out, _binder_ty) = binder_elaboration.as_value();
        if let (Some(def), _) = binder_out.try_destruct_def(tycker) {
            let _ = tycker.statics.value_aliases.upsert(def, bindee_out);
            // consider adding it to the globals if bindee is global
            if tycker.statics.global_terms.contains(&ss::TermId::from(bindee_out)) {
                tycker.statics.global_defs.ensure(def);
                // consider adding it to the inlinables as well
                let _ = tycker.statics.inlinables.upsert(def, bindee_out);
            }
        }
        // finally, we tyck the tail
        let tail_out_ann =
            TyEnvT::new(binder_elaboration.info.clone(), tail).tyck(tycker, tail_action)?;
        self.check_let_value_tail(
            tycker,
            binder,
            &binder_elaboration,
            binder_out,
            bindee_out,
            tail_out_ann,
        )
    }

    /// Close the scope of a value binding over its checked tail and record the
    /// let, unless the binding is a static opening with no runtime payload.
    fn check_let_value_tail(
        &self, tycker: &mut Tycker<'_>, binder: su::PatId, binder_elaboration: &CheckedPattern,
        binder_out: ss::VPatId, bindee_out: ss::ValueId, tail_out_ann: TermAnnId,
    ) -> Result<TermAnnId> {
        match tail_out_ann {
            | tail if ValuePatternShape::is_static_opening(tycker, binder_out)
                && (binder_elaboration.inner.opened.is_empty()
                    || matches!(tail, TermAnnId::Kind(_) | TermAnnId::Type(_, _))) =>
            {
                // All selected components are static, and the runtime payload
                // is discarded. Preserve checking and witness escape validation,
                // but introduce no runtime let for a wholly manifest opening.
                // Fresh witnesses still need their opening evidence in runtime
                // terms until required static elaboration specializes them.
                match tail {
                    | TermAnnId::Value(_, ty)
                    | TermAnnId::Compu(_, ty)
                    | TermAnnId::Type(ty, _) => {
                        binder_elaboration.close_scope(tycker, ty)?;
                    }
                    | TermAnnId::Kind(_) => {}
                    | TermAnnId::Hole(_) => tycker.err(TyckError::SortMismatch)?,
                }
                Ok(tail)
            }
            | TermAnnId::Value(tail_out, tail_ty) => {
                if !ValuePatternShape::is_irrefutable(tycker, binder_out) {
                    tycker.err(TyckError::Expressivity(
                        "value bindings must be irrefutable patterns",
                    ))?
                }
                binder_elaboration.close_scope(tycker, tail_ty)?;
                let id = Alloc::alloc(
                    tycker,
                    ss::Let { binder: binder_out, bindee: bindee_out, tail: tail_out },
                    tail_ty,
                    &(),
                );
                Ok(TermAnnId::Value(id, tail_ty))
            }
            | TermAnnId::Compu(tail_out, tail_ty) => {
                binder_elaboration.close_scope(tycker, tail_ty)?;
                ValuePatternShape::require_binding(tycker, binder, binder_out)?;
                let id = Alloc::alloc(
                    tycker,
                    ss::Let { binder: binder_out, bindee: bindee_out, tail: tail_out },
                    tail_ty,
                    &(),
                );
                Ok(TermAnnId::Compu(id, tail_ty))
            }
            | TermAnnId::Hole(_) | TermAnnId::Kind(_) | TermAnnId::Type(_, _) => {
                tycker.err(TyckError::SortMismatch)
            }
        }
    }
}
