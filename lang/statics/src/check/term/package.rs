//! Existential classifiers, manifest equations, and package introduction.

use super::*;

impl TermChecker<'_> {
    pub(super) fn check_sigma_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Sigma, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::Sigma(binder, body) = term;
            match switch {
                | Switch::Syn => {
                    // either a prod or an exists
                    let binder_out_ann = self.mk(binder).tyck_k(tycker, PatternAction::syn())?;
                    match binder_out_ann.annotation {
                        | PatAnnId::Kind(_) => {
                            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                            let input = crate::query::InternedSigmaSyn::new(
                                tycker.db,
                                crate::query::SigmaSynArm::Expressivity,
                            );
                            let Some(crate::query::SigmaSynOutcome::Error(error)) =
                                crate::query::sigma_syn_judgment(
                                    tycker.db,
                                    tycker.data,
                                    term,
                                    input,
                                    tycker.site_occurrence(),
                                )
                            else {
                                unreachable!("the kind arm of sigma judgments is query-produced")
                            };
                            tycker.err_k(error, std::panic::Location::caller())?
                        }
                        | PatAnnId::Type(tpat, _kd) => {
                            // exists
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
                            let (body_ty, body_kd) = body.try_as_type(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            // body_kd should be of vtype
                            let vtype = ss::VType.build(tycker, &self.info);
                            Lub::lub_k(vtype, body_kd, tycker)?;
                            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                            let input = crate::query::InternedSigmaSyn::new(
                                tycker.db,
                                crate::query::SigmaSynArm::Exists { tpat, abst, body_ty },
                            );
                            let Some(crate::query::SigmaSynOutcome::Type { id, ty, kd }) =
                                crate::query::sigma_syn_judgment(
                                    tycker.db,
                                    tycker.data,
                                    term,
                                    input,
                                    tycker.site_occurrence(),
                                )
                            else {
                                unreachable!("the exists arm of sigma judgments is query-produced")
                            };
                            tycker.statics.types_pre.insert_new(id, ss::Fillable::Done(ty), kd);
                            tycker.store_env(id, &self.info);
                            TermAnnId::Type(id, kd)
                        }
                        | PatAnnId::Value(_, ty_1) => {
                            // ty should be of vtype
                            let kd_1 = tycker.statics.type_kind(ty_1);
                            let vtype = ss::VType.build(tycker, &self.info);
                            Lub::lub_k(vtype, kd_1, tycker)?;
                            let ty_2 = TyEnvT::new(binder_out_ann.info.clone(), body)
                                .tyck_k(tycker, Action::syn())?;
                            let (ty_2, kd_2) = ty_2.try_as_type(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            binder_out_ann.close_scope_k(tycker, ty_2)?;
                            // kd_2 should be of vtype
                            Lub::lub_k(vtype, kd_2, tycker)?;
                            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                            let input = crate::query::InternedSigmaSyn::new(
                                tycker.db,
                                crate::query::SigmaSynArm::Prod { ty_1, ty_2 },
                            );
                            let Some(crate::query::SigmaSynOutcome::Type { id, ty, kd }) =
                                crate::query::sigma_syn_judgment(
                                    tycker.db,
                                    tycker.data,
                                    term,
                                    input,
                                    tycker.site_occurrence(),
                                )
                            else {
                                unreachable!("the product arm of sigma judgments is query-produced")
                            };
                            tycker.statics.types_pre.insert_new(id, ss::Fillable::Done(ty), kd);
                            tycker.store_env(id, &self.info);
                            TermAnnId::Type(id, kd)
                        }
                    }
                }
                | Switch::Ana(ana) => match ana {
                    | AnnId::Kind(kd) => {
                        let vtype = ss::VType.build(tycker, &self.info);
                        // prod or exists; should be of vtype
                        Lub::lub_k(vtype, kd, tycker)?;
                        // just synthesize the whole thing
                        self.tyck_k(tycker, Action::syn())?
                    }
                    | AnnId::Set | AnnId::Type(_) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                },
            }
        })
    }

    pub(super) fn check_manifest_exists_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::ManifestExists, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::ManifestExists { binder, definition, body } = term;
            match switch {
                | Switch::Syn => {
                    let definition = self.mk(definition).tyck_k(tycker, Action::syn())?;
                    match definition {
                        | TermAnnId::Kind(definition) => {
                            let binder =
                                self.mk(binder).tyck_k(tycker, PatternAction::ana(AnnId::Set))?;
                            let pattern = binder.annotation.try_as_kind(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            let body_env =
                                self.mk(Assign(pattern, definition)).tyck_k(tycker, ())?.info;
                            let body = TyEnvT::new(body_env, body).tyck_k(tycker, Action::syn())?;
                            let (body, body_kind) = body.try_as_type(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            let vtype = ss::VType.build(tycker, &self.info);
                            Lub::lub_k(vtype, body_kind, tycker)?;
                            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                            let input = crate::query::InternedManifestSyn::new(
                                tycker.db,
                                crate::query::ManifestSynArm::Kind { pattern, definition, body },
                            );
                            let Some(crate::query::ManifestSynOutcome::Type { id, ty, kd }) =
                                crate::query::manifest_exists_syn_judgment(
                                    tycker.db,
                                    tycker.data,
                                    term,
                                    input,
                                    tycker.site_occurrence(),
                                )
                            else {
                                unreachable!(
                                    "the kind arm of manifest-exists judgments is query-produced"
                                )
                            };
                            tycker.statics.types_pre.insert_new(id, ss::Fillable::Done(ty), kd);
                            tycker.store_env(id, &self.info);
                            TermAnnId::Type(id, kd)
                        }
                        | TermAnnId::Type(definition, definition_kind) => {
                            let binder_action = if tycker.pattern_has_payload_annotation(binder) {
                                PatternAction::syn()
                            } else {
                                PatternAction::ana(definition_kind.into())
                            };
                            let binder = self.mk(binder).tyck_k(tycker, binder_action)?;
                            let (pattern, _domain_kind) = binder.try_as_type(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            let witness = Alloc::alloc(tycker, pattern, (), &());
                            let payload_kind = tycker.statics.annotations_abst[&witness];
                            Lub::lub_k(payload_kind, definition_kind, tycker)?;

                            let full_definition = pattern.introduce_payload(tycker, definition);
                            let full_definition = tycker.err_p_to_k(full_definition)?;
                            let body_env =
                                self.mk(Assign(pattern, full_definition)).tyck_k(tycker, ())?.info;
                            let body = TyEnvT::new(body_env, body).tyck_k(tycker, Action::syn())?;
                            let (body, body_kind) = body.try_as_type(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            let vtype = ss::VType.build(tycker, &self.info);
                            Lub::lub_k(vtype, body_kind, tycker)?;

                            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                            let input = crate::query::InternedManifestSyn::new(
                                tycker.db,
                                crate::query::ManifestSynArm::Type {
                                    pattern,
                                    witness,
                                    definition,
                                    body,
                                },
                            );
                            let Some(crate::query::ManifestSynOutcome::Type { id, ty, kd }) =
                                crate::query::manifest_exists_syn_judgment(
                                    tycker.db,
                                    tycker.data,
                                    term,
                                    input,
                                    tycker.site_occurrence(),
                                )
                            else {
                                unreachable!(
                                    "the type arm of manifest-exists judgments is query-produced"
                                )
                            };
                            tycker.statics.types_pre.insert_new(id, ss::Fillable::Done(ty), kd);
                            tycker.store_env(id, &self.info);
                            TermAnnId::Type(id, kd)
                        }
                        | TermAnnId::Hole(_) | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                            let input = crate::query::InternedManifestSyn::new(
                                tycker.db,
                                crate::query::ManifestSynArm::SortMismatch,
                            );
                            let Some(crate::query::ManifestSynOutcome::Error(error)) =
                                crate::query::manifest_exists_syn_judgment(
                                    tycker.db,
                                    tycker.data,
                                    term,
                                    input,
                                    tycker.site_occurrence(),
                                )
                            else {
                                unreachable!(
                                    "the sort arm of manifest-exists judgments is query-produced"
                                )
                            };
                            tycker.err_k(error, std::panic::Location::caller())?
                        }
                    }
                }
                | Switch::Ana(AnnId::Kind(kind)) => {
                    let vtype = ss::VType.build(tycker, &self.info);
                    Lub::lub_k(vtype, kind, tycker)?;
                    self.tyck_k(tycker, Action::syn())?
                }
                | Switch::Ana(AnnId::Set | AnnId::Type(_)) => {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                }
            }
        })
    }

    pub(super) fn check_pack_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Pack, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::Pack { mode, binder, definition: evidence, body } = term;
            match switch {
                | Switch::Syn => {
                    let definition = self.mk(evidence).tyck_k(tycker, Action::syn())?;
                    match definition {
                        | TermAnnId::Type(definition, definition_kind) => {
                            let binder_action = if tycker.pattern_has_payload_annotation(binder) {
                                PatternAction::syn()
                            } else {
                                PatternAction::ana(definition_kind.into())
                            };
                            let binder = self.mk(binder).tyck_k(tycker, binder_action)?;
                            let (pattern, _domain_kind) = binder.try_as_type(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            let witness = Alloc::alloc(tycker, pattern, (), &());
                            let payload_kind = tycker.statics.annotations_abst[&witness];
                            Lub::lub_k(payload_kind, definition_kind, tycker)?;

                            let full_definition = pattern.introduce_payload(tycker, definition);
                            let full_definition = tycker.err_p_to_k(full_definition)?;
                            let body_env =
                                self.mk(Assign(pattern, full_definition)).tyck_k(tycker, ())?.info;
                            // Sealing replaces the payload's references to the
                            // witness with the abstract binder, so the emitted
                            // existential body stays dependent on the seal while the
                            // payload itself checked against the disclosed witness.
                            let seal_absts = match mode {
                                | su::PackMode::Disclosed => None,
                                | su::PackMode::Sealed => {
                                    match tycker.statics.types_pre[&definition].to_owned() {
                                        | ss::Fillable::Done(ss::Type::Abst(evidence_abst)) => {
                                            let binder_ty = Alloc::alloc(
                                                tycker,
                                                witness,
                                                payload_kind,
                                                &self.info,
                                            );
                                            Some(vec![(evidence_abst, binder_ty)])
                                        }
                                        | _ => None,
                                    }
                                }
                            };
                            let body = TyEnvT::new(body_env, body).tyck_k(tycker, Action::syn())?;
                            match body {
                                | TermAnnId::Value(body, raw_body_ty) => {
                                    let body_ty = match &seal_absts {
                                        | Some(absts) => {
                                            raw_body_ty.subst_absts_k(tycker, absts)?
                                        }
                                        | None => raw_body_ty,
                                    };
                                    let vtype = ss::VType.build(tycker, &self.info);
                                    let term =
                                        crate::query::InternedTerm::new(tycker.db, self.inner);
                                    let arm = match mode {
                                        | su::PackMode::Disclosed => {
                                            crate::query::PackSynArm::Package {
                                                pattern,
                                                witness,
                                                definition,
                                                body,
                                                body_ty,
                                            }
                                        }
                                        | su::PackMode::Sealed => {
                                            crate::query::PackSynArm::Sealed {
                                                pattern,
                                                witness,
                                                definition,
                                                body,
                                                body_ty,
                                            }
                                        }
                                    };
                                    let input = crate::query::InternedPackSyn::new(tycker.db, arm);
                                    let Some(crate::query::PackSynOutcome::Package {
                                        exists_id,
                                        exists,
                                        cons_id,
                                        cons,
                                    }) = crate::query::pack_syn_judgment(
                                        tycker.db,
                                        tycker.data,
                                        term,
                                        input,
                                        tycker.site_occurrence(),
                                    )
                                    else {
                                        unreachable!(
                                            "the package arm of pack judgments is query-produced"
                                        )
                                    };
                                    tycker.statics.types_pre.insert_new(
                                        exists_id,
                                        ss::Fillable::Done(exists),
                                        vtype,
                                    );
                                    tycker.store_env(exists_id, &self.info);
                                    tycker.statics.values.insert_new(cons_id, cons);
                                    tycker.statics.annotations_value.insert_new(cons_id, exists_id);
                                    tycker.statics.env_value.insert_new(cons_id, self.info.clone());
                                    TermAnnId::Value(cons_id, exists_id)
                                }
                                | TermAnnId::Hole(_)
                                | TermAnnId::Kind(_)
                                | TermAnnId::Type(_, _)
                                | TermAnnId::Compu(_, _) => {
                                    let term =
                                        crate::query::InternedTerm::new(tycker.db, self.inner);
                                    let input = crate::query::InternedPackSyn::new(
                                        tycker.db,
                                        crate::query::PackSynArm::PayloadNotValue,
                                    );
                                    let Some(crate::query::PackSynOutcome::Error(error)) =
                                        crate::query::pack_syn_judgment(
                                            tycker.db,
                                            tycker.data,
                                            term,
                                            input,
                                            tycker.site_occurrence(),
                                        )
                                    else {
                                        unreachable!(
                                            "the payload arm of pack judgments is query-produced"
                                        )
                                    };
                                    tycker.err_k(error, std::panic::Location::caller())?
                                }
                            }
                        }
                        | TermAnnId::Hole(_)
                        | TermAnnId::Kind(_)
                        | TermAnnId::Value(_, _)
                        | TermAnnId::Compu(_, _) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                    }
                }
                | Switch::Ana(AnnId::Type(expected)) => {
                    let checked = self.tyck_k(tycker, Action::syn())?;
                    let (cons, ty) = checked.try_as_value(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?;
                    let ty = Lub::lub_k(expected, ty, tycker)?;
                    TermAnnId::Value(cons, ty)
                }
                | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                }
            }
        })
    }
}
