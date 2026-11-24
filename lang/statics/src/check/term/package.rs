//! Existential classifiers, manifest equations, and package introduction.

use super::*;

impl TermChecker<'_> {
    pub(super) fn check_sigma<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Sigma, switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        Ok({
            let su::Sigma(binder, body) = term;
            match switch {
                | Switch::Syn => {
                    // either a prod or an exists
                    let binder_out_ann = self.mk(binder).tyck(tycker, PatternAction::syn())?;
                    match binder_out_ann.annotation {
                        | PatAnnId::Kind(_) => tycker.err(TyckError::Expressivity(
                            "abstract existential kinds are not supported",
                        ))?,
                        | PatAnnId::Type(tpat, _kd) => {
                            // exists
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
                            let (body_ty, body_kd) =
                                body.try_as_type(tycker, TyckError::SortMismatch)?;
                            // body_kd should be of vtype
                            let vtype = ss::VType.build(tycker, &self.info);
                            Lub::lub(vtype, body_kd, tycker)?;
                            let id = Alloc::alloc(
                                tycker,
                                ss::Exists::new(
                                    ss::TypeBinder { pattern: tpat, witness: abst },
                                    body_ty,
                                ),
                                vtype,
                                &(),
                            );
                            TermAnnId::Type(id, vtype)
                        }
                        | PatAnnId::Value(_, ty_1) => {
                            // ty should be of vtype
                            let kd_1 = tycker.statics.type_kind(ty_1);
                            let vtype = ss::VType.build(tycker, &self.info);
                            Lub::lub(vtype, kd_1, tycker)?;
                            let ty_2 = TyEnvT::new(binder_out_ann.info.clone(), body)
                                .tyck(tycker, Action::syn())?;
                            let (ty_2, kd_2) = ty_2.try_as_type(tycker, TyckError::SortMismatch)?;
                            binder_out_ann.close_scope(tycker, ty_2)?;
                            // kd_2 should be of vtype
                            Lub::lub(vtype, kd_2, tycker)?;
                            let id = Alloc::alloc(
                                tycker,
                                ss::Type::Prod(ss::Prod(vec![ty_1, ty_2])),
                                vtype,
                                &(),
                            );
                            TermAnnId::Type(id, vtype)
                        }
                    }
                }
                | Switch::Ana(ana) => match ana {
                    | AnnId::Kind(kd) => {
                        let vtype = ss::VType.build(tycker, &self.info);
                        // prod or exists; should be of vtype
                        Lub::lub(vtype, kd, tycker)?;
                        // just synthesize the whole thing
                        self.tyck(tycker, Action::syn())?
                    }
                    | AnnId::Set | AnnId::Type(_) => tycker.err(TyckError::SortMismatch)?,
                },
            }
        })
    }

    pub(super) fn check_manifest_exists<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::ManifestExists, switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        Ok({
            let su::ManifestExists { binder, definition, body } = term;
            match switch {
                | Switch::Syn => {
                    let definition = self.mk(definition).tyck(tycker, Action::syn())?;
                    match definition {
                        | TermAnnId::Kind(definition) => {
                            let binder =
                                self.mk(binder).tyck(tycker, PatternAction::ana(AnnId::Set))?;
                            let pattern =
                                binder.annotation.try_as_kind(tycker, TyckError::SortMismatch)?;
                            let body_env =
                                self.mk(Assign(pattern, definition)).tyck(tycker, ())?.info;
                            let body = TyEnvT::new(body_env, body).tyck(tycker, Action::syn())?;
                            let (body, body_kind) =
                                body.try_as_type(tycker, TyckError::SortMismatch)?;
                            let vtype = ss::VType.build(tycker, &self.info);
                            Lub::lub(vtype, body_kind, tycker)?;
                            let id = Alloc::alloc(
                                tycker,
                                ss::Type::ManifestKind(ss::ManifestKind {
                                    binder: pattern,
                                    definition,
                                    body,
                                }),
                                vtype,
                                &(),
                            );
                            TermAnnId::Type(id, vtype)
                        }
                        | TermAnnId::Type(definition, definition_kind) => {
                            let binder = tycker
                                .check_type_payload_pattern(self.mk(binder), definition_kind)?;
                            let (pattern, _domain_kind) =
                                binder.try_as_type(tycker, TyckError::SortMismatch)?;
                            let witness = Alloc::alloc(tycker, pattern, (), &());
                            let payload_kind = tycker.statics.annotations_abst[&witness];
                            Lub::lub(payload_kind, definition_kind, tycker)?;

                            let full_definition = pattern.introduce_payload(tycker, definition);
                            let full_definition = full_definition?;
                            let body_env =
                                self.mk(Assign(pattern, full_definition)).tyck(tycker, ())?.info;
                            let body = TyEnvT::new(body_env, body).tyck(tycker, Action::syn())?;
                            let (body, body_kind) =
                                body.try_as_type(tycker, TyckError::SortMismatch)?;
                            let vtype = ss::VType.build(tycker, &self.info);
                            Lub::lub(vtype, body_kind, tycker)?;

                            let id = Alloc::alloc(
                                tycker,
                                ss::Exists::with_manifest(
                                    ss::TypeBinder { pattern, witness },
                                    definition,
                                    body,
                                ),
                                vtype,
                                &(),
                            );
                            TermAnnId::Type(id, vtype)
                        }
                        | TermAnnId::Hole(_) | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                            tycker.err(TyckError::SortMismatch)?
                        }
                    }
                }
                | Switch::Ana(AnnId::Kind(kind)) => {
                    let vtype = ss::VType.build(tycker, &self.info);
                    Lub::lub(vtype, kind, tycker)?;
                    self.tyck(tycker, Action::syn())?
                }
                | Switch::Ana(AnnId::Set | AnnId::Type(_)) => {
                    tycker.err(TyckError::SortMismatch)?
                }
            }
        })
    }

    pub(super) fn check_pack<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Pack, switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        Ok({
            let su::Pack { mode, binder, definition: evidence, body } = term;
            match switch {
                | Switch::Syn => {
                    let definition = self.mk(evidence).tyck(tycker, Action::syn())?;
                    match definition {
                        | TermAnnId::Type(definition, definition_kind) => {
                            let binder = tycker
                                .check_type_payload_pattern(self.mk(binder), definition_kind)?;
                            let (pattern, _domain_kind) =
                                binder.try_as_type(tycker, TyckError::SortMismatch)?;
                            let witness = Alloc::alloc(tycker, pattern, (), &());
                            let payload_kind = tycker.statics.annotations_abst[&witness];
                            Lub::lub(payload_kind, definition_kind, tycker)?;

                            let full_definition = pattern.introduce_payload(tycker, definition);
                            let full_definition = full_definition?;
                            let body_env =
                                self.mk(Assign(pattern, full_definition)).tyck(tycker, ())?.info;
                            // Sealing replaces the payload's references to the
                            // witness with the abstract binder, so the emitted
                            // existential body stays dependent on the seal while the
                            // payload itself checked against the disclosed witness.
                            let seal_absts = match mode {
                                | su::PackMode::Disclosed => None,
                                | su::PackMode::Sealed => {
                                    match tycker.statics.types_pre[&definition].to_owned() {
                                        | ss::Fillable::Done(ss::Type::Abst(evidence_abst)) => {
                                            let binder_ty =
                                                Alloc::alloc(tycker, witness, payload_kind, &());
                                            Some(vec![(evidence_abst, binder_ty)])
                                        }
                                        | _ => None,
                                    }
                                }
                            };
                            let body = TyEnvT::new(body_env, body).tyck(tycker, Action::syn())?;
                            match body {
                                | TermAnnId::Value(body, raw_body_ty) => {
                                    let body_ty = match &seal_absts {
                                        | Some(absts) => raw_body_ty.subst_absts(tycker, absts)?,
                                        | None => raw_body_ty,
                                    };
                                    let exists_ty = match mode {
                                        | su::PackMode::Disclosed => ss::Exists::with_manifest(
                                            ss::TypeBinder { pattern, witness },
                                            definition,
                                            body_ty,
                                        ),
                                        | su::PackMode::Sealed => ss::Exists::new(
                                            ss::TypeBinder { pattern, witness },
                                            body_ty,
                                        ),
                                    };
                                    let exists_id = Alloc::alloc(
                                        tycker,
                                        exists_ty,
                                        tycker.intrinsics.vtype,
                                        &(),
                                    );
                                    let cons_id = Alloc::alloc(
                                        tycker,
                                        ss::Value::SCons(ss::ConsN(
                                            vec![full_definition.into()],
                                            body,
                                        )),
                                        exists_id,
                                        &(),
                                    );
                                    TermAnnId::Value(cons_id, exists_id)
                                }
                                | TermAnnId::Hole(_)
                                | TermAnnId::Kind(_)
                                | TermAnnId::Type(_, _)
                                | TermAnnId::Compu(_, _) => tycker.err(TyckError::Expressivity(
                                    "pack payloads must synthesize as values",
                                ))?,
                            }
                        }
                        | TermAnnId::Hole(_)
                        | TermAnnId::Kind(_)
                        | TermAnnId::Value(_, _)
                        | TermAnnId::Compu(_, _) => tycker.err(TyckError::SortMismatch)?,
                    }
                }
                | Switch::Ana(AnnId::Type(expected)) => {
                    let checked = self.tyck(tycker, Action::syn())?;
                    let (cons, ty) = checked.try_as_value(tycker, TyckError::SortMismatch)?;
                    let ty = Lub::lub(expected, ty, tycker)?;
                    TermAnnId::Value(cons, ty)
                }
                | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                    tycker.err(TyckError::SortMismatch)?
                }
            }
        })
    }

    pub(super) fn check_seal<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Seal, switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        let su::Seal { tm, ty } = term;
        match switch {
            | Switch::Syn => self.check_seal_syn(tycker, tm, ty),
            | Switch::Ana(AnnId::Type(expected)) => {
                let checked = self.check_seal_syn(tycker, tm, ty)?;
                let (value, sealed_ty) = checked.try_as_value(tycker, TyckError::SortMismatch)?;
                let merged_ty = Lub::lub(expected, sealed_ty, tycker)?;
                Ok(TermAnnId::Value(value, merged_ty))
            }
            | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => tycker.err(TyckError::SortMismatch),
        }
    }

    fn check_seal_syn<'db>(
        &self, tycker: &mut Tycker<'db>, tm: su::TermId, ty: su::TermId,
    ) -> Result<TermAnnId> {
        enum SealedPrefixLayer {
            ManifestKind { binder: ss::KPatId, definition: ss::KindId },
            Exists { binder: ss::TypeBinder, definition: ss::TypeId },
        }

        let vtype = ss::VType.build(tycker, &self.info);
        let sig = self.mk(ty).tyck(tycker, Action::ana(vtype.into()))?;
        let (sig_ty, _) = sig.try_as_type(tycker, TyckError::SortMismatch)?;
        let sig_ty = sig_ty.unroll(tycker)?.subst_env(tycker, &self.info)?;

        let checked_tm =
            self.mk(tm).tyck(tycker, Action::ana_prepared(sig_ty.into(), &self.info))?;
        let (checked_val, _) = checked_tm.try_as_value(tycker, TyckError::SortMismatch)?;

        let mut witnesses = Vec::new();
        let mut shape = checked_val.static_shape(tycker, self.info.skolem_scope());
        while let crate::elaborate::static_values::StaticShape::Package(prefix, body) = shape {
            witnesses.extend(prefix);
            shape = *body;
        }
        let mut witness_iter = witnesses.into_iter();

        let mut cursor = DeferredTelescopeType::new(sig_ty);
        let mut layers = Vec::new();
        let mut sealed_abstract_count = 0usize;
        let final_body = loop {
            match cursor.with_environment(&self.info).reveal(tycker)? {
                | DeferredTelescopeView::ManifestKind { binder, definition, body } => {
                    match witness_iter.next() {
                        | Some(ss::StaticTermId::Kind(_)) => {}
                        | Some(ss::StaticTermId::Type(_)) | None => {
                            return tycker.err(TyckError::PackageWitnessesUnavailable {
                                package: checked_val,
                            });
                        }
                    }
                    layers.push(SealedPrefixLayer::ManifestKind { binder, definition });
                    cursor = body;
                }
                | DeferredTelescopeView::Exists { binder, mode, body } => {
                    let Some(ss::StaticTermId::Type(witness_ty)) = witness_iter.next() else {
                        return tycker
                            .err(TyckError::PackageWitnessesUnavailable { package: checked_val });
                    };
                    match mode {
                        | DeferredTelescopeExistsMode::Manifest(definition) => {
                            let definition =
                                definition.with_environment(&self.info).materialize(tycker)?;
                            layers.push(SealedPrefixLayer::Exists {
                                binder: binder.clone(),
                                definition,
                            });
                            cursor = body.with_abstract(binder.witness, definition);
                        }
                        | DeferredTelescopeExistsMode::Abstract => {
                            sealed_abstract_count += 1;
                            let raw_payload = binder
                                .pattern
                                .bind_argument(tycker, witness_ty)?
                                .subst_env(tycker, &self.info)?;
                            let sealed_def = self.seal_type_family(tycker, &binder, raw_payload)?;
                            layers.push(SealedPrefixLayer::Exists {
                                binder: binder.clone(),
                                definition: sealed_def,
                            });
                            cursor = body.with_abstract(binder.witness, sealed_def);
                        }
                    }
                }
                | DeferredTelescopeView::Other(body) => {
                    break body.with_environment(&self.info).materialize(tycker)?;
                }
            }
        };

        if sealed_abstract_count == 0 {
            return tycker.err(TyckError::TypeExpected {
                expected: "an existential signature with an abstract type witness".to_string(),
                found: sig_ty,
            });
        }

        let sealed_ty = layers.into_iter().rev().fold(final_body, |body, layer| match layer {
            | SealedPrefixLayer::ManifestKind { binder, definition } => Alloc::alloc(
                tycker,
                ss::Type::ManifestKind(ss::ManifestKind { binder, definition, body }),
                vtype,
                &(),
            ),
            | SealedPrefixLayer::Exists { binder, definition } => Alloc::alloc(
                tycker,
                ss::Exists::with_manifest(binder, definition, body),
                vtype,
                &(),
            ),
        });
        let seal_val = Alloc::alloc(tycker, ss::Value::Seal(checked_val), sealed_ty, &());
        Ok(TermAnnId::Value(seal_val, sealed_ty))
    }

    fn seal_type_family<'db>(
        &self, tycker: &mut Tycker<'db>, binder: &ss::TypeBinder, raw_payload: ss::TypeId,
    ) -> Result<ss::TypeId> {
        let mut params = Vec::new();
        for witness in self.info.val_witnesses() {
            if !params.contains(&witness) {
                params.push(witness);
            }
        }
        let extra: Vec<_> = raw_payload
            .free_witnesses(tycker)?
            .into_iter()
            .filter(|witness| {
                (self.info.is_local_witness(witness)
                    || tycker.statics.existential_skolems.contains(*witness))
                    && !params.contains(witness)
            })
            .collect();
        params.extend(extra);

        let payload_kind = binder.payload_kind(tycker);
        let fam_kind = params.iter().rev().fold(payload_kind, |acc, param| {
            let param_kind = tycker.statics.annotations_abst[param];
            Alloc::alloc(tycker, ss::Kind::Arrow(ss::Arrow(param_kind, acc)), (), &())
        });
        let (hint, _) = binder.pattern.try_destruct_def(tycker);
        let fam_abst: ss::AbstId = Alloc::alloc(tycker, hint, fam_kind, &());

        let repr_ty = if params.is_empty() {
            raw_payload
        } else {
            let param_abstractions = params
                .iter()
                .map(|&param| {
                    let param_kind = tycker.statics.annotations_abst[&param];
                    let fresh: ss::AbstId =
                        Alloc::alloc(tycker, None::<su::DefId>, param_kind, &());
                    let fresh_ty: ss::TypeId = Alloc::alloc(tycker, fresh, param_kind, &());
                    let hole_pat: ss::TPatId =
                        Alloc::alloc(tycker, ss::TypePattern::Hole(ss::Hole), param_kind, &());
                    (param, fresh, fresh_ty, hole_pat, param_kind)
                })
                .collect::<Vec<_>>();
            let renamings = param_abstractions
                .iter()
                .map(|&(param, _, fresh_ty, _, _)| (param, fresh_ty))
                .collect::<Vec<_>>();
            let closed_body = raw_payload.subst_absts(tycker, &renamings)?;
            param_abstractions
                .into_iter()
                .rev()
                .fold(
                    (payload_kind, closed_body),
                    |(codomain_kind, body), (_, witness, _, pattern, domain_kind)| {
                        let abs_kind = Alloc::alloc(
                            tycker,
                            ss::Kind::Arrow(ss::Arrow(domain_kind, codomain_kind)),
                            (),
                            &(),
                        );
                        let abs = Alloc::alloc(
                            tycker,
                            ss::Type::Abs(ss::TypeAbstraction {
                                binder: ss::TypeBinder { pattern, witness },
                                body,
                            }),
                            abs_kind,
                            &(),
                        );
                        (abs_kind, abs)
                    },
                )
                .1
        };
        tycker.statics.opaque_representations.insert_new(fam_abst, repr_ty);
        tycker.invalidate_field_materializations();

        let applied = params.into_iter().try_fold(
            (fam_kind, Alloc::alloc(tycker, fam_abst, fam_kind, &())),
            |(fun_kind, fun_ty), param| -> Result<_> {
                let ss::Kind::Arrow(ss::Arrow(_, next_kind)) =
                    tycker.kind_filled(&fun_kind)?.to_owned()
                else {
                    unreachable!("family kind carries one arrow per parameter")
                };
                let param_kind = tycker.statics.annotations_abst[&param];
                let param_ty = Alloc::alloc(tycker, param, param_kind, &());
                let app_ty =
                    Alloc::alloc(tycker, ss::Type::App(ss::App(fun_ty, param_ty)), next_kind, &());
                Ok((next_kind, app_ty))
            },
        )?;
        Ok(applied.1)
    }
}
