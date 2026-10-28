//! Named values, labels, products, and structural projections.

use super::*;

impl TermChecker<'_> {
    pub(super) fn check_named_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Named<FieldName, su::TermId>,
        switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::Named(name, inner) = term;
            match switch {
                | Switch::Syn => match self.mk(inner).tyck_k(tycker, Action::syn())? {
                    | inner_out @ (TermAnnId::Type(..)
                    | TermAnnId::Hole(_)
                    | TermAnnId::Kind(_)
                    | TermAnnId::Compu(_, _)) => {
                        let inner_interned =
                            crate::query::InternedTermAnn::new(tycker.db, inner_out);
                        match tycker.judge(
                            self.inner,
                            inner_interned,
                            crate::query::named_syn_judgment,
                        ) {
                            | crate::query::NamedSynOutcome::Type {
                                kind_id,
                                kind,
                                named_id,
                                named,
                            } => {
                                tycker
                                    .statics
                                    .kinds_pre
                                    .insert_new(kind_id, ss::Fillable::Done(kind));
                                tycker.statics.types_pre.insert_new(
                                    named_id,
                                    ss::Fillable::Done(named),
                                    kind_id,
                                );
                                tycker.store_env(named_id, &self.info);
                                TermAnnId::Type(named_id, kind_id)
                            }
                            | crate::query::NamedSynOutcome::Error(error) => {
                                tycker.err_k(error, std::panic::Location::caller())?
                            }
                        }
                    }
                    | TermAnnId::Value(inner, inner_ty) => {
                        let inner_kind = tycker.statics.type_kind(inner_ty);
                        let vtype = ss::VType.build(tycker, &self.info);
                        Lub::lub_k(vtype, inner_kind, tycker)?;
                        let named_ty = Alloc::alloc(
                            tycker,
                            ss::Label(name.clone(), inner_ty),
                            vtype,
                            &self.info,
                        );
                        let named =
                            Alloc::alloc(tycker, ss::Named(name, inner), named_ty, &self.info);
                        TermAnnId::Value(named, named_ty)
                    }
                },
                | Switch::Ana(AnnId::Kind(kd)) => {
                    let ss::Kind::Label(ss::Label(expected_name, inner_kind)) =
                        tycker.kind_filled_k(&kd)?.to_owned()
                    else {
                        tycker.err_k(TyckError::KindMismatch, std::panic::Location::caller())?
                    };
                    if name != expected_name {
                        tycker.err_k(
                            TyckError::NamedLabelMismatch {
                                expected: expected_name,
                                found: name.clone(),
                            },
                            std::panic::Location::caller(),
                        )?
                    }
                    let checked = self.mk(inner).tyck_k(tycker, Action::ana(inner_kind.into()))?;
                    let (inner, _) = checked.try_as_type(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?;
                    let named = Alloc::alloc(tycker, ss::Named(name, inner), kd, &self.info);
                    TermAnnId::Type(named, kd)
                }
                | Switch::Ana(AnnId::Type(expected)) => {
                    let unrolled = expected.unroll_k(tycker)?;
                    let expected_view = if unrolled == expected {
                        expected
                    } else {
                        unrolled.subst_env_k(tycker, &self.info)?
                    };
                    let ss::Type::Label(ss::Label(expected_name, inner_ty)) =
                        tycker.type_filled_k(&expected_view)?.to_owned()
                    else {
                        tycker.err_k(
                            TyckError::TypeExpected {
                                expected: "a named value type".to_string(),
                                found: expected,
                            },
                            std::panic::Location::caller(),
                        )?
                    };
                    if name != expected_name {
                        tycker.err_k(
                            TyckError::NamedLabelMismatch {
                                expected: expected_name,
                                found: name.clone(),
                            },
                            std::panic::Location::caller(),
                        )?
                    }
                    let checked = self
                        .mk(inner)
                        .tyck_k(tycker, Action::ana_prepared(inner_ty.into(), &self.info))?;
                    let (inner, _) = checked.try_as_value(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?;
                    let named = Alloc::alloc(tycker, ss::Named(name, inner), expected, &self.info);
                    TermAnnId::Value(named, expected)
                }
                | Switch::Ana(AnnId::Set) => {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                }
            }
        })
    }

    pub(super) fn check_label_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Label<FieldName, su::TermId>,
        switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::Label(name, inner) = term;
            match switch {
                | Switch::Syn => match self.mk(inner).tyck_k(tycker, Action::syn())? {
                    | inner_out @ (TermAnnId::Kind(_)
                    | TermAnnId::Hole(_)
                    | TermAnnId::Value(_, _)
                    | TermAnnId::Compu(_, _)) => {
                        let inner_interned =
                            crate::query::InternedTermAnn::new(tycker.db, inner_out);
                        match tycker.judge(
                            self.inner,
                            inner_interned,
                            crate::query::label_syn_judgment,
                        ) {
                            | crate::query::LabelSynOutcome::Kind { id, kind } => {
                                tycker.statics.kinds_pre.insert_new(id, ss::Fillable::Done(kind));
                                TermAnnId::Kind(id)
                            }
                            | crate::query::LabelSynOutcome::Error(error) => {
                                tycker.err_k(error, std::panic::Location::caller())?
                            }
                        }
                    }
                    | TermAnnId::Type(inner, kind) => {
                        let vtype = ss::VType.build(tycker, &self.info);
                        Lub::lub_k(vtype, kind, tycker)?;
                        let label = Alloc::alloc(tycker, ss::Label(name, inner), vtype, &self.info);
                        TermAnnId::Type(label, vtype)
                    }
                },
                | Switch::Ana(AnnId::Set) => {
                    let inner =
                        self.mk(inner).tyck_k(tycker, Action::ana(AnnId::Set))?.try_as_kind(
                            tycker,
                            TyckError::SortMismatch,
                            std::panic::Location::caller(),
                        )?;
                    let label = Alloc::alloc(tycker, ss::Label(name, inner), (), &());
                    TermAnnId::Kind(label)
                }
                | Switch::Ana(AnnId::Kind(kind)) => {
                    let vtype = ss::VType.build(tycker, &self.info);
                    Lub::lub_k(vtype, kind, tycker)?;
                    let (inner, _) =
                        self.mk(inner).tyck_k(tycker, Action::ana(vtype.into()))?.try_as_type(
                            tycker,
                            TyckError::SortMismatch,
                            std::panic::Location::caller(),
                        )?;
                    let label = Alloc::alloc(tycker, ss::Label(name, inner), vtype, &self.info);
                    TermAnnId::Type(label, vtype)
                }
                | Switch::Ana(AnnId::Type(_)) => {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                }
            }
        })
    }

    pub(super) fn check_cons_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: Vec<su::TermId>, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        let mut items = term;
        let tail = items.pop().expect("a cons term carries at least two components");
        match switch {
            | Switch::Syn => self.check_cons_syn_k(tycker, items, tail),
            | Switch::Ana(AnnId::Type(expected)) => {
                match expected.reveal_or_refine_prepared_product_k(tycker, &self.info)? {
                    | ss::Type::Prod(_) => {
                        self.check_cons_ana_product_k(tycker, items, tail, expected)
                    }
                    | ss::Type::Exists(_) | ss::Type::ManifestKind(_) => {
                        self.check_cons_ana_package_k(tycker, items, tail, expected)
                    }
                    | _ => tycker.err_k(
                        TyckError::TypeExpected {
                            expected: "one of `_ * _` or `exists _ . _`".to_string(),
                            found: expected,
                        },
                        std::panic::Location::caller(),
                    ),
                }
            }
            | Switch::Ana(AnnId::Kind(kd)) => {
                let vtype = ss::VType.build(tycker, &self.info);
                Lub::lub_k(vtype, kd, tycker)?;
                self.tyck_k(tycker, Action::syn())
            }
            | Switch::Ana(AnnId::Set) => {
                tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())
            }
        }
    }

    /// Synthesize a cons term from its components, which must all be types
    /// (an infix product type) or all be values (a product value).
    fn check_cons_syn_k(
        &self, tycker: &mut Tycker<'_>, items: Vec<su::TermId>, tail: su::TermId,
    ) -> ResultKont<TermAnnId> {
        let mut components = items;
        components.push(tail);
        let outcomes = components
            .iter()
            .copied()
            .map(|component| self.mk(component).tyck_k(tycker, Action::syn()))
            .collect_k::<Vec<_>>()?;
        let typed = matches!(outcomes.first(), Some(TermAnnId::Type(_, _)));
        let mixed = outcomes.iter().any(|outcome| match outcome {
            | TermAnnId::Type(_, _) => !typed,
            | TermAnnId::Value(_, _) => typed,
            | TermAnnId::Hole(_) | TermAnnId::Kind(_) | TermAnnId::Compu(_, _) => true,
        });
        if mixed {
            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
        }
        if typed {
            self.check_cons_syn_type_k(tycker, outcomes, components)
        } else {
            Ok(self.check_cons_syn_value(tycker, outcomes))
        }
    }

    /// An infix product type: one n-ary product over the synthesized
    /// component types.
    fn check_cons_syn_type_k(
        &self, tycker: &mut Tycker<'_>, outcomes: Vec<TermAnnId>, components: Vec<su::TermId>,
    ) -> ResultKont<TermAnnId> {
        let vtype = ss::VType.build(tycker, &self.info);
        let component_tys = outcomes
            .into_iter()
            .zip(components)
            .map(|(outcome, source)| {
                tycker.guarded(|tycker| {
                    tycker.tasks.push_back_mut(TyckTask::Term(source, Switch::Syn));
                    match outcome {
                        | TermAnnId::Type(ty, kd) => {
                            Lub::lub_k(vtype, kd, tycker)?;
                            Ok(ty)
                        }
                        | _ => unreachable!("sorted outcomes are uniform"),
                    }
                })
            })
            .collect_k::<Vec<_>>()?;
        let prod = Alloc::alloc(tycker, ss::Prod(component_tys), vtype, &self.info);
        Ok(TermAnnId::Type(prod, vtype))
    }

    /// A product value over synthesized component values, recorded through
    /// the cons judgment.
    fn check_cons_syn_value(&self, tycker: &mut Tycker<'_>, outcomes: Vec<TermAnnId>) -> TermAnnId {
        let (mut output, mut annotations): (Vec<_>, Vec<_>) = outcomes
            .into_iter()
            .map(|outcome| match outcome {
                | TermAnnId::Value(item, item_ty) => (item, item_ty),
                | _ => unreachable!("sorted outcomes are uniform"),
            })
            .unzip();
        let tail = output.pop().expect("a cons term carries at least two components");
        let tail_ty = annotations.pop().expect("a cons term carries at least two components");
        let item_outcomes = output
            .iter()
            .zip(&annotations)
            .map(|(value, ty)| TermAnnId::Value(*value, *ty))
            .collect::<Vec<_>>();
        let items_interned = crate::query::InternedConsItems::new(tycker.db, item_outcomes);
        let tail_interned =
            crate::query::InternedTermAnn::new(tycker.db, TermAnnId::Value(tail, tail_ty));
        let outcome = tycker.judge(
            self.inner,
            (items_interned, tail_interned),
            |db, data, term, (items, tail), occurrence| {
                crate::query::cons_syn_judgment(db, data, term, items, tail, occurrence)
            },
        );
        for (id, prod) in outcome.prods {
            tycker.statics.types_pre.insert_new(id, ss::Fillable::Done(prod), outcome.vtype);
            tycker.store_env(id, &self.info);
        }
        tycker.statics.values.insert_new(outcome.cons_id, outcome.cons);
        tycker.statics.annotations_value.insert_new(outcome.cons_id, outcome.ann);
        tycker.statics.env_value.insert_new(outcome.cons_id, self.info.clone());
        TermAnnId::Value(outcome.cons_id, outcome.ann)
    }

    /// Check a cons term against a product type with matching components.
    fn check_cons_ana_product_k(
        &self, tycker: &mut Tycker<'_>, items: Vec<su::TermId>, tail: su::TermId,
        expected: ss::TypeId,
    ) -> ResultKont<TermAnnId> {
        let ss::Prod(component_tys) = expected.view_prepared_product_k(tycker, &self.info)?;
        if component_tys.len() != items.len() + 1 {
            tycker.err_k(
                TyckError::TypeExpected {
                    expected: "a product with matching components".to_string(),
                    found: expected,
                },
                std::panic::Location::caller(),
            )?
        }
        let components = items.into_iter().chain([tail]).zip(component_tys);
        let (cons, ann) = self.check_value_product_k(tycker, components)?;
        Ok(TermAnnId::Value(cons, ann))
    }

    /// Check each component against its prepared type, then allocate the
    /// product value and its product type.
    fn check_value_product_k(
        &self, tycker: &mut Tycker<'_>,
        components: impl IntoIterator<Item = (su::TermId, ss::TypeId)>,
    ) -> ResultKont<(ss::ValueId, ss::TypeId)> {
        let (components, annotations): (Vec<_>, Vec<_>) = components
            .into_iter()
            .map(|(item, item_ty)| -> ResultKont<_> {
                let checked = self
                    .mk(item)
                    .tyck_k(tycker, Action::ana_prepared(item_ty.into(), &self.info))?;
                checked.try_as_value(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )
            })
            .collect_k::<Vec<_>>()?
            .into_iter()
            .unzip();
        let vtype = ss::VType.build(tycker, &self.info);
        let ann = Alloc::alloc(tycker, ss::Prod(annotations), vtype, &self.info);
        let cons = Alloc::alloc(tycker, ss::Value::VCons(components), ann, &self.info);
        Ok((cons, ann))
    }

    /// Check a package against an existential or manifest-kind telescope:
    /// check its static components in order, then the remaining components
    /// against the package body.
    fn check_cons_ana_package_k(
        &self, tycker: &mut Tycker<'_>, items: Vec<su::TermId>, tail: su::TermId,
        expected: ss::TypeId,
    ) -> ResultKont<TermAnnId> {
        let mut body_ty = DeferredTelescopeType::new(expected);
        let mut body_index = items.len();
        let mut witnesses: Vec<ss::StaticTermId> = Vec::new();

        for (index, item) in items.iter().copied().enumerate() {
            let (witness, next_ty) = match body_ty.with_environment(&self.info).reveal_k(tycker)? {
                | DeferredTelescopeView::ManifestKind { definition, body, .. } => {
                    (self.check_manifest_kind_witness_k(tycker, item, definition)?, body)
                }
                | DeferredTelescopeView::Exists { binder, mode, body } => {
                    self.check_exists_witness_k(tycker, item, binder, mode, body)?
                }
                | DeferredTelescopeView::Other(body) => {
                    body_ty = body;
                    body_index = index;
                    break;
                }
            };
            body_ty = next_ty;
            witnesses.push(witness);
        }

        if witnesses.is_empty() {
            tycker.err_k(
                TyckError::TypeExpected {
                    expected: "an existential package".to_string(),
                    found: expected,
                },
                std::panic::Location::caller(),
            )?
        }

        let body_ty = body_ty.materialize_k(tycker)?;
        let body = self.check_package_body_k(tycker, &items[body_index..], tail, body_ty)?;
        let cons = Alloc::alloc(tycker, ss::ConsN(witnesses, body), expected, &self.info);
        Ok(TermAnnId::Value(cons, expected))
    }

    /// Check a manifest kind component against its definition.
    fn check_manifest_kind_witness_k(
        &self, tycker: &mut Tycker<'_>, item: su::TermId, definition: ss::KindId,
    ) -> ResultKont<ss::StaticTermId> {
        let checked = self.mk(item).tyck_k(tycker, Action::ana(AnnId::Set))?;
        let witness =
            checked.try_as_kind(tycker, TyckError::SortMismatch, std::panic::Location::caller())?;
        let witness = Lub::lub_k(definition, witness, tycker)?;
        Ok(witness.into())
    }

    /// Check an existential witness, returning it with the telescope that
    /// remains after its payload is substituted.
    fn check_exists_witness_k(
        &self, tycker: &mut Tycker<'_>, item: su::TermId, binder: ss::TypeBinder,
        mode: DeferredTelescopeExistsMode, body: DeferredTelescopeType,
    ) -> ResultKont<(ss::StaticTermId, DeferredTelescopeType)> {
        let domain_kind = binder.domain_kind(tycker);
        let checked = self.mk(item).tyck_k(tycker, Action::ana(domain_kind.into()))?;
        let (witness, _) =
            checked.try_as_type(tycker, TyckError::SortMismatch, std::panic::Location::caller())?;
        let payload = binder.pattern.bind_argument_k(tycker, witness)?;
        let payload = match mode {
            | DeferredTelescopeExistsMode::Abstract => payload,
            | DeferredTelescopeExistsMode::Manifest(definition) => {
                let definition = definition.materialize_k(tycker)?;
                Lub::lub_k(definition, payload, tycker)?
            }
        };
        Ok((witness.into(), body.with_abstract(binder.witness, payload)))
    }

    /// Check the value components of a package against its body type.
    ///
    /// A lone tail component takes the whole body; several components split
    /// the body as a product.
    fn check_package_body_k(
        &self, tycker: &mut Tycker<'_>, body_items: &[su::TermId], tail: su::TermId,
        body_ty: ss::TypeId,
    ) -> ResultKont<ss::ValueId> {
        if body_items.is_empty() {
            let checked =
                self.mk(tail).tyck_k(tycker, Action::ana_prepared(body_ty.into(), &self.info))?;
            let (body, _) = checked.try_as_value(
                tycker,
                TyckError::SortMismatch,
                std::panic::Location::caller(),
            )?;
            return Ok(body);
        }
        let ss::Prod(body_component_tys) = body_ty.view_prepared_product_k(tycker, &self.info)?;
        if body_component_tys.len() < body_items.len() {
            tycker.err_k(
                TyckError::TypeExpected {
                    expected: "a product with matching components".to_string(),
                    found: body_ty,
                },
                std::panic::Location::caller(),
            )?
        }
        let remaining =
            tycker.rest_product_k(&body_component_tys[body_items.len()..], &self.info)?;
        let components = body_items
            .iter()
            .copied()
            .zip(body_component_tys.iter().copied())
            .chain([(tail, remaining)]);
        let (body, _) = self.check_value_product_k(tycker, components)?;
        Ok(body)
    }

    pub(super) fn check_proj_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Proj<su::TermId, FieldName>,
        switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::Proj(head, name) = term;
            let checked = self.mk(head).tyck_k(tycker, Action::syn())?;
            match checked {
                | TermAnnId::Type(head, head_kind) => {
                    let candidate = FieldProjectionResolver::r#type(tycker, head_kind, &name)?;
                    let payload_kind = match switch {
                        | Switch::Syn => candidate.projected,
                        | Switch::Ana(AnnId::Kind(expected)) => {
                            Lub::lub_k(expected, candidate.projected, tycker)?
                        }
                        | Switch::Ana(AnnId::Set | AnnId::Type(_)) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                    };
                    let projected = FieldProjectionResolver::project_type_k(
                        tycker,
                        head,
                        candidate,
                        payload_kind,
                    )?;
                    TermAnnId::Type(projected, payload_kind)
                }
                | TermAnnId::Value(head, head_ty) => {
                    let candidate =
                        FieldProjectionResolver::value_term_k(tycker, &self.info, head_ty, &name)?;
                    let target = FieldProjectionResolver::value_target(&candidate);
                    let projected_ty = candidate.projected;
                    let projected_ty = match switch {
                        | Switch::Syn => projected_ty,
                        | Switch::Ana(AnnId::Type(expected)) => {
                            Lub::lub_k(expected, projected_ty, tycker)?
                        }
                        | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                    };
                    let field = ss::ResolvedField { name, target };
                    let input = crate::query::InternedProjInput::new(
                        tycker.db,
                        head,
                        field.name,
                        field
                            .target
                            .products
                            .iter()
                            .map(|product| (product.product, product.position))
                            .collect::<Vec<_>>(),
                        projected_ty,
                    );
                    let outcome = tycker.judge(self.inner, input, crate::query::proj_syn_judgment);
                    tycker.statics.values.insert_new(outcome.id, outcome.value);
                    tycker.statics.annotations_value.insert_new(outcome.id, outcome.ann);
                    tycker.statics.env_value.insert_new(outcome.id, self.info.clone());
                    TermAnnId::Value(outcome.id, outcome.ann)
                }
                | TermAnnId::Hole(_) | TermAnnId::Kind(_) | TermAnnId::Compu(_, _) => {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                }
            }
        })
    }
}

impl Tycker<'_> {
    /// The product over the components remaining after a prefix has been
    /// consumed: no components is `Unit`, one component is itself, and
    /// several rejoin as one product.
    pub(in crate::check) fn rest_product_k(
        &mut self, components: &[ss::TypeId], env: &ss::TyEnv,
    ) -> ResultKont<ss::TypeId> {
        match components {
            | [] => {
                let vtype = ss::VType.build(self, env);
                Ok(Alloc::alloc(self, ss::UnitTy, vtype, env))
            }
            | [only] => Ok(*only),
            | components => {
                let vtype = ss::VType.build(self, env);
                Ok(Alloc::alloc(self, ss::Prod(components.to_vec()), vtype, env))
            }
        }
    }
}
