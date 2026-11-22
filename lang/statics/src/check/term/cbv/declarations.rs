//! Type, kind, recursive-group, and sealed-declaration elaboration for `@[cbv]` blocks.

use super::*;

impl TermChecker<'_> {
    /// The kind a binding declares, when it declares one.
    ///
    /// A block's `let` has to choose between two judgments before it runs
    /// either: a type binding translates its bindee with `[[·]]v` on types, a
    /// term binding with `[[·]]v` on terms, and the two disagree about what the
    /// same syntax means. The host chooses by synthesizing the bindee and
    /// reading off its sort, which is not available here — a source arrow is
    /// ill-kinded in the host, so synthesizing a term binding's bindee or its
    /// annotation can fail rather than answer.
    ///
    /// So the signal is the **declared kind**: `let X : K = A` is a type
    /// binding exactly when [`Self::cbv_annotation_kind`] finds that `K`
    /// denotes a kind, which it decides without synthesizing anything that
    /// could be a source type. An annotation that is not a kind leaves the
    /// binding a term binding, which is what it was.
    ///
    /// Requiring the kind also matches what the fragment asks everywhere else:
    /// a parameter needs its type for the same reason, so that every judgment
    /// here synthesizes.
    pub(super) fn cbv_declared_kind<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, bindee: su::TermId,
    ) -> Result<Option<ss::KindId>> {
        let su::Term::Ann(su::Ann { ty: annotation, .. }) = tycker.scoped.terms[&bindee].to_owned()
        else {
            return Ok(None);
        };
        self.cbv_annotation_kind(tycker, scope, annotation)
    }

    /// The kind a `let`'s bindee is, when it is one: `VType` or `CType` written
    /// with its intrinsic, a kind arrow, a named kind, or a kind name.
    ///
    /// None of these is a source term, so recognizing one decides the binding
    /// before either term judgment runs, as a declared kind decides a type
    /// binding. A kind name is told from a term name by its recorded
    /// annotation, so a term variable is not synthesized here ahead of the term
    /// judgment; one bound outside the block is refused by the kind judgment.
    ///
    /// Desugaring writes an omitted classifier as an ascription to a hole,
    /// `let K = (VType : _)`, which says nothing and is looked through.
    pub(super) fn cbv_kind_bindee<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, bindee: su::TermId,
    ) -> Result<Option<ss::KindId>> {
        zydeco_foundation::stack::maybe_grow(|| match tycker.scoped.terms[&bindee].to_owned() {
            | su::Term::Ann(su::Ann { tm, ty })
                if matches!(tycker.scoped.terms[&ty], su::Term::Hole(_)) =>
            {
                self.cbv_kind_bindee(tycker, scope, tm)
            }
            | su::Term::Internal(su::Internal::VType | su::Internal::CType)
            | su::Term::Pi(_)
            | su::Term::Label(_) => self.cbv_annotation_kind(tycker, scope, bindee),
            | su::Term::Var(def)
                if matches!(tycker.statics.annotations_var.get(&def), Some(AnnId::Set)) =>
            {
                self.cbv_annotation_kind(tycker, scope, bindee)
            }
            | _ => Ok(None),
        })
    }

    /// The kind-binding clause: `[[let K = κ in M]]v = [[M]]v[κ/K]`.
    ///
    /// It emits nothing, as a type binding does, and binds as the host's kind
    /// alias binds: the binder is checked against the set of kinds, and the
    /// name stands for the kind in the tail. A block closed over kinds needs
    /// it, since `VType` named outside is not the block's to name.
    pub(super) fn cbv_kind_binding<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId, kind: ss::KindId,
        tail: su::TermId, expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        self.cbv_require_type_binding_name(tycker, binder)?;
        let bound =
            TyEnvT::new(scope.env.clone(), binder).tyck(tycker, PatternAction::ana(AnnId::Set))?;
        let ss::PatId::Kind(pattern) = bound.inner.annotation.as_pat() else {
            return tycker.err(TyckError::SortMismatch);
        };
        let assigned = TyEnvT::new(bound.info.clone(), Assign(pattern, kind)).tyck(tycker, ())?;
        let tail_scope = scope.type_binding(assigned.info.clone());
        self.cbv_term(tycker, &tail_scope, tail, expected)
    }

    /// The type-binding clause: `[[let X : K = A in M]]v = [[M]]v[[[A]]v/X]`.
    ///
    /// A type binding emits nothing, so the block's computation is the tail's.
    /// What it contributes is an entry in the typing environment, which is why
    /// this mirrors the host's type-alias path rather than the `do` the term
    /// binding compiles to: the name is *inlined*, and every occurrence in the
    /// tail resolves to the translated type.
    ///
    /// This is the only way a block can name a source arrow. `Int -> Int` is
    /// ill-kinded in the host, so no host binding can stand for it; here the
    /// arrow is translated first and the name stands for `Thk (Int -> Ret Int)`.
    pub(super) fn cbv_type_binding<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binding: CbvTypeBinding,
        tail: su::TermId, expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        let CbvTypeBinding { binder, source, declared } = binding;
        self.cbv_require_type_binding_name(tycker, binder)?;
        let translated = self.cbv_type_against(tycker, scope, source, declared)?;
        let bound = TyEnvT::new(scope.env.clone(), binder)
            .tyck(tycker, PatternAction::ana(declared.into()))?;
        let (pattern, _kind) = bound.inner.as_type();
        let (def, _) = pattern.try_destruct_def(tycker);
        if let Some(def) = def {
            let _ = tycker.statics.type_definitions.upsert(def, translated);
        }
        let assigned =
            TyEnvT::new(bound.info.clone(), Assign(pattern, translated)).tyck(tycker, ())?;
        let tail_scope = scope.type_binding(assigned.info.clone());
        self.cbv_term(tycker, &tail_scope, tail, expected)
    }

    /// A type binder names one type or ignores it. A product binder, which
    /// the term rule admits, would be destructuring a type.
    pub(super) fn cbv_require_type_binding_name<'db>(
        &self, tycker: &mut Tycker<'db>, binder: su::PatId,
    ) -> Result<()> {
        if !matches!(tycker.scoped.pats[&binder], su::Pattern::Var(_) | su::Pattern::Hole(_)) {
            tycker.err(TyckError::Expressivity(
                "a call-by-value type binding must be named by a variable or a wildcard",
            ))?
        }
        Ok(())
    }

    /// The recursive-group clause: sealed definitions that mention themselves
    /// or one another.
    ///
    /// The host admits a cycle only among nominal type definitions with known
    /// kinds, and so does this clause, with the host's own diagnostics: a
    /// value cycle is directed to `let fix`, a transparent `let` in the group
    /// is missing its seal, and a kind that is absent, or that mentions the
    /// group itself, cannot be known before the group is. A group of more
    /// than one member is mutual recursion, and it is translated as the host's
    /// fixpoint checks one: every member is declared before any is defined.
    pub(super) fn cbv_rec_group<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, group: su::RecGroup,
        expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        let su::RecGroup { definitions, tail } = group;
        // These checks run member by member in the host's order, so each gets
        // the host's diagnostic, and for every member before what only a
        // block requires of one, which `cbv_sealed_bindings` checks. The
        // kind has to be known before the group is, which is also what keeps
        // reading it from synthesizing the group's own name before its binder
        // has been checked.
        for su::RecursiveDefinition { binder, bindee } in &definitions {
            if BindingCycle::has_value_body(tycker, *bindee) {
                tycker.err(TyckError::InvalidBindingCycle(*binder))?
            }
            let known = bindee.syntactically_annotated(tycker).filter(|annotation| {
                !matches!(tycker.scoped.terms[annotation], su::Term::Hole(_))
                    && tycker
                        .source_free_variables(annotation)
                        .iter()
                        .all(|definition| tycker.statics.annotations_var.get(definition).is_some())
            });
            // A known name is not a known kind: the host requires the
            // annotation to denote one before it asks about the seal. One that
            // reaches outside the block is still reported as that first.
            let kind = match known {
                | Some(annotation) => {
                    let kind = self.cbv_annotation_kind(tycker, scope, annotation)?;
                    if kind.is_none() {
                        self.cbv_classifier_shape(tycker, scope, annotation)?;
                    }
                    kind
                }
                | None => None,
            };
            if kind.is_none() {
                tycker.err(TyckError::InvalidBindingCycle(*binder))?
            }
            if bindee.syntactically_sealed(tycker).is_none() {
                tycker.err(TyckError::MissingSeal)?
            }
        }
        let bindings = definitions
            .iter()
            .map(|su::RecursiveDefinition { binder, bindee }| {
                let Some(sealed) = bindee.syntactically_sealed(tycker) else {
                    unreachable!("every member was required to be sealed")
                };
                CbvSealedBinding { binder: *binder, sealed }
            })
            .collect();
        self.cbv_sealed_bindings(tycker, scope, bindings, tail, expected)
    }

    /// `[[def X1 : K1 = D1 … def Xn : Kn = Dn in M]]v`: declare every seal,
    /// define each, then translate the tail with each `Xi` denoting its own,
    /// and refuse a tail whose type mentions any of them.
    ///
    /// Every name is bound before any declaration is translated, which is what
    /// lets the members of a group mention themselves and one another. A
    /// `def` that does not mention itself is translated as a group of one,
    /// with a fresh identity.
    ///
    /// A declaration's scope is the rest of its block, so the tail's type is
    /// where it would escape, as an unpack's witness would. Packing the
    /// result as an existential exports it: an abstract package hides the
    /// seal, and a manifest one discloses it and is refused.
    pub(super) fn cbv_sealed_bindings<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, bindings: Vec<CbvSealedBinding>,
        tail: su::TermId, expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        let (declared_scope, declared) = bindings.into_iter().try_fold(
            (scope.clone(), Vec::new()),
            |(scope, mut declared), CbvSealedBinding { binder, sealed }| {
                self.cbv_require_type_binding_name(tycker, binder)?;
                let definition = self.cbv_sealed_definition(tycker, sealed)?;
                let (scope, seal) = self.cbv_declare_seal(tycker, &scope, binder, definition)?;
                declared.push(seal);
                Ok((scope, declared))
            },
        )?;
        let seals = declared
            .into_iter()
            .map(|seal| self.cbv_define_seal(tycker, &declared_scope, seal))
            .collect::<Result<Vec<_>>>()?;
        let (tail, tail_ty) = self.cbv_term(tycker, &declared_scope, tail, expected)?;
        let escaped = seals
            .into_iter()
            .map(|seal| {
                let mentioned = tail_ty.mentions_declaration(tycker, seal);
                Ok((mentioned)?.then_some(seal))
            })
            .collect::<Result<Vec<_>>>()?
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();
        if !escaped.is_empty() {
            tycker.err(TyckError::EscapingDeclaration { declarations: escaped, result: tail_ty })?
        }
        Ok((tail, tail_ty))
    }

    /// Read a sealed definition's parts off its bindee, the term inside the
    /// seal: an optional kind, type parameters, and a `data` or `codata`
    /// declaration. Any other body is refused, because a seal is inhabited
    /// only through the `data` or `codata` shape its typing exposes.
    pub(super) fn cbv_sealed_definition<'db>(
        &self, tycker: &mut Tycker<'db>, sealed: su::TermId,
    ) -> Result<CbvSealedDefinition> {
        use su::Term as Tm;
        // Only the structure is read here; no annotation is. A kind may
        // mention a parameter, wrongly, and reading it before the parameter is
        // bound would reach the parameter ahead of its binder, which the
        // checker refuses by panicking. The ascription desugaring puts around
        // a declaration states its host sort and is no classifier.
        let (classifier, mut body) = match tycker.scoped.terms[&sealed].to_owned() {
            | Tm::Ann(su::Ann { tm, ty }) if Self::cbv_declaration_shape(tycker, tm).is_none() => {
                (Some(ty), tm)
            }
            | _ => (None, sealed),
        };
        // `def List (X : VType) : VType = D` desugars to `fn (X : VType) => D`
        // under the kind, so the parameters are the abstractions around `D`.
        // An ascription the author writes at any depth is an ordinary one, kept
        // to be checked where the parameters outside it are bound; the
        // innermost ascription is desugaring's own, around the declaration.
        let mut parameters = Vec::new();
        let mut ascriptions = Vec::new();
        let declaration = loop {
            match tycker.scoped.terms[&body].to_owned() {
                | Tm::Abs(su::Abs(binders, inner)) => {
                    parameters.extend(binders);
                    body = inner;
                }
                | Tm::Ann(su::Ann { tm, .. })
                    if Self::cbv_declaration_shape(tycker, tm).is_some() =>
                {
                    break tm;
                }
                | Tm::Ann(su::Ann { tm, ty }) => {
                    ascriptions.push((parameters.len(), ty));
                    body = tm;
                }
                | _ => break body,
            }
        };
        let Some(shape) = Self::cbv_declaration_shape(tycker, declaration) else {
            return tycker.err(
                TyckError::Expressivity(
                    "a sealed definition inside a call-by-value block declares a data or codata type, possibly under type parameters",
                ));
        };
        Ok(CbvSealedDefinition { classifier, parameters, ascriptions, declaration, shape })
    }

    /// The kind a sealed definition declares, read off its classifier.
    ///
    /// A hole stands for a part left to inference, and for nothing else.
    /// Desugaring writes one for an omitted classifier — after the parameters'
    /// telescope when the definition writes `: _` after them — and it is read
    /// as the kinds of the parameters it covers over `VType`, the kind a
    /// source `data` or `codata` has. Every part written before the hole is
    /// read and guarded as any kind is, so `CType -> _` is refused rather than
    /// taken for an omitted classifier.
    pub(super) fn cbv_definition_kind<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, classifier: Option<su::TermId>,
        parameters: &[su::PatId],
    ) -> Result<ss::KindId> {
        let Some(classifier) = classifier else {
            return self.cbv_parameter_kinds(tycker, scope, parameters);
        };
        match tycker.scoped.terms[&classifier].to_owned() {
            | su::Term::Hole(_) => self.cbv_parameter_kinds(tycker, scope, parameters),
            | su::Term::Pi(su::Pi(binders, codomain))
                if Self::cbv_result_inferred(tycker, codomain) =>
            {
                self.cbv_sourced(
                    tycker,
                    classifier,
                    |tycker| {
                        self.cbv_definition_telescope(tycker, scope, &binders, codomain, parameters)
                    },
                    |_tycker, kind| Some(TermAnnId::Kind(*kind)),
                )
            }
            | _ => self.cbv_require_annotation_kind(tycker, scope, classifier),
        }
    }

    /// The kind arrows of a definition's telescope, one per binder.
    pub(super) fn cbv_definition_telescope<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binders: &[su::PatId],
        codomain: su::TermId, parameters: &[su::PatId],
    ) -> Result<ss::KindId> {
        let Some((binder, later)) = binders.split_first() else {
            return self.cbv_definition_kind(tycker, scope, Some(codomain), parameters);
        };
        let binder = *binder;
        if self.cbv_optional_binder_annotation(tycker, binder).is_none() {
            return tycker.err(TyckError::Expressivity(
                "a sealed definition inside a call-by-value block needs its kind, written or given by its parameters' kinds",
            ));
        }
        // The binder is checked whole, through the entrance every type binder
        // in a block goes through, which guards and settles each annotation in
        // it. It is checked before the rest of the telescope is read, so a later
        // domain that names it, as `(Y : X)` does, meets a checked type variable
        // and is refused by sort.
        let (_, domain, _) = self.cbv_check_type_binder(
            tycker,
            scope,
            binder,
            None,
            CbvTypeBinderSite::Abstraction,
        )?;
        let rest = parameters.get(1..).unwrap_or_default();
        let codomain = self.cbv_definition_telescope(tycker, scope, later, codomain, rest)?;
        Ok(Arrow(domain, codomain).build(tycker, &scope.env))
    }

    /// Whether a classifier's result is left to inference: a hole, possibly
    /// after arrows.
    pub(super) fn cbv_result_inferred(tycker: &Tycker<'_>, classifier: su::TermId) -> bool {
        match &tycker.scoped.terms[&classifier] {
            | su::Term::Hole(_) => true,
            | su::Term::Pi(su::Pi(_, codomain)) => Self::cbv_result_inferred(tycker, *codomain),
            | _ => false,
        }
    }

    /// The parameters' kinds over `VType`, for a definition whose kind is left
    /// to inference. Each parameter must declare its kind, and a kind mentions
    /// no type, so an annotation naming an earlier parameter is refused before
    /// it is read: read, it would reach that parameter ahead of its binder.
    pub(super) fn cbv_parameter_kinds<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, parameters: &[su::PatId],
    ) -> Result<ss::KindId> {
        let domains = parameters
            .iter()
            .enumerate()
            .map(|(index, &parameter)| {
                let Some((_, annotation)) = self.cbv_optional_binder_annotation(tycker, parameter)
                else {
                    return tycker.err(
                        TyckError::Expressivity(
                            "a sealed definition inside a call-by-value block needs its kind, written or given by its parameters' kinds",
                        ));
                };
                let earlier = parameters[..index]
                    .iter()
                    .flat_map(|&earlier| self.cbv_pattern_definitions(tycker, earlier))
                    .collect::<std::collections::HashSet<_>>();
                self.cbv_require_unmentioned(tycker, scope, annotation, annotation, &earlier)?;
                self.cbv_require_annotation_kind(tycker, scope, annotation)
            })
            .collect::<Result<Vec<_>>>()?;
        let vtype = ss::VType.build(tycker, &scope.env);
        Ok(domains
            .into_iter()
            .rev()
            .fold(vtype, |codomain, domain| Arrow(domain, codomain).build(tycker, &scope.env)))
    }

    /// Refuse a kind `annotation` when `within` mentions any of `names`. A kind
    /// mentions no type, so a mention makes the annotation the wrong sort; it
    /// is refused before anything is read, since reading it could reach a name
    /// ahead of its binder. One that reaches outside the block is reported as
    /// that, by the shape guard, first.
    pub(super) fn cbv_require_unmentioned<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, annotation: su::TermId,
        within: su::TermId, names: &std::collections::HashSet<DefId>,
    ) -> Result<()> {
        if tycker.source_contexts.mentions_any(within.into(), names)
            && let ControlFlow::Break(CbvMention::Found(_)) = Traversal::new(tycker.scoped)
                .run(within.into(), &mut CbvMentions { defs: names })
                .expect("resolved source syntax is acyclic")
        {
            self.cbv_classifier_shape(tycker, scope, annotation)?;
            tycker.err(TyckError::SortMismatch)?
        }
        Ok(())
    }

    /// Check a kind arrow's binder against its domain, for the facts ordinary
    /// checking records there: a representative for the binder pattern and the
    /// annotation of the name it binds. Its annotation is settled first, so it
    /// is not synthesized a second time.
    ///
    /// Only for a binder `p : K` whose `p` is a name or a wildcard, possibly
    /// under annotations the caller has settled, as the caller has established,
    /// so that the pattern rule reads no annotation it would have to
    /// synthesize. A binder that may carry more goes through
    /// [`Self::cbv_check_type_binder`], which guards each annotation.
    pub(super) fn cbv_record_kind_binder<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId,
        annotation: su::TermId, domain: ss::KindId,
    ) -> Result<()> {
        let _ = tycker.settled_pattern_annotations.upsert(annotation, domain.into());
        TyEnvT::new(scope.env.clone(), binder).tyck(tycker, PatternAction::ana(domain.into()))?;
        Ok(())
    }

    /// The kind an annotation that must be one denotes. One that is not a kind
    /// goes through the shape guard first, so one reaching outside the block
    /// is reported as that rather than as the wrong sort.
    pub(super) fn cbv_require_annotation_kind<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, annotation: su::TermId,
    ) -> Result<ss::KindId> {
        match self.cbv_annotation_kind(tycker, scope, annotation)? {
            | Some(kind) => Ok(kind),
            | None => {
                self.cbv_classifier_shape(tycker, scope, annotation)?;
                tycker.err(TyckError::SortMismatch)
            }
        }
    }

    pub(super) fn cbv_declaration_shape(
        tycker: &Tycker<'_>, term: su::TermId,
    ) -> Option<CbvDeclarationShape> {
        match tycker.scoped.terms[&term] {
            | su::Term::Data(_) => Some(CbvDeclarationShape::Data),
            | su::Term::CoData(_) => Some(CbvDeclarationShape::CoData),
            | _ => None,
        }
    }

    /// The first half of `[[def X : K = fn (X1 : K1) … => D]]v`: a seal of
    /// kind `K`, and `X` bound to the source type it stands for, with `D` left
    /// for [`Self::cbv_define_seal`].
    ///
    /// For `data` the seal is on the value type, `σ : K`, and `X` denotes `σ`.
    /// For `codata` it is on the computation type inside the thunk, `σ : K1
    /// -> … -> CType`, and `X` denotes `fn (Y1 : K1) … => Thk (σ Y1 …)`: a
    /// seal on the thunk would never be met, because a codata value is
    /// introduced only by a thunk, whose type is synthesized as `Thk B` with
    /// the codata seal inside it.
    ///
    /// Only the kind is read here, which is why a group can declare every
    /// member before it defines any: what `X` denotes depends on `K` and on
    /// which declaration `D` is, never on what `D` says.
    pub(super) fn cbv_declare_seal<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId,
        definition: CbvSealedDefinition,
    ) -> Result<(CbvScope, CbvDeclaredSeal)> {
        let CbvSealedDefinition { classifier, ref parameters, shape, .. } = definition;
        let kind = self.cbv_definition_kind(tycker, scope, classifier, parameters)?;
        self.cbv_require_source_kind(tycker, kind)?;
        let domains =
            parameters.iter().try_fold((Vec::new(), kind), |(mut domains, remaining), _| {
                match tycker.kind_filled(&remaining)?.to_owned() {
                    | ss::Kind::Arrow(Arrow(domain, codomain)) => {
                        domains.push(domain);
                        Ok((domains, codomain))
                    }
                    | _ => tycker.err(TyckError::KindMismatch),
                }
            });
        let (domains, result) = domains?;
        let seal_kind = match shape {
            | CbvDeclarationShape::Data => kind,
            | CbvDeclarationShape::CoData => {
                let vtype = ss::VType.build(tycker, &scope.env);
                Lub::lub(vtype, result, tycker)?;
                let ctype = ss::CType.build(tycker, &scope.env);
                domains.iter().rev().fold(ctype, |codomain, &domain| {
                    Arrow(domain, codomain).build(tycker, &scope.env)
                })
            }
        };
        let seal = tycker.alloc_abst();
        let sealed: ss::TypeId = Alloc::alloc(tycker, seal, seal_kind, &());
        let denoted = match shape {
            | CbvDeclarationShape::Data => sealed,
            | CbvDeclarationShape::CoData => {
                self.cbv_codata_name(tycker, &scope.env, sealed, &domains)
            }
        };
        let bound =
            TyEnvT::new(scope.env.clone(), binder).tyck(tycker, PatternAction::ana(kind.into()))?;
        let (pattern, _kind) = bound.inner.as_type();
        let (def, _) = pattern.try_destruct_def(tycker);
        if let Some(def) = def {
            tycker.statics.abst_hints.insert_new(seal, def);
        }
        let assigned =
            TyEnvT::new(bound.info.clone(), Assign(pattern, denoted)).tyck(tycker, ())?;
        let declared_scope = scope.type_binding(assigned.info.clone());
        Ok((declared_scope, CbvDeclaredSeal { seal, seal_kind, domains, def, definition }))
    }

    /// The second half: `D` translated where every name of its group is
    /// bound, and recorded as what the seal is over.
    pub(super) fn cbv_define_seal<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, declared: CbvDeclaredSeal,
    ) -> Result<ss::AbstId> {
        let CbvDeclaredSeal { seal, seal_kind, domains, def, definition } = declared;
        let translated = self.cbv_seal_definition(tycker, scope, &definition, &domains, 0)?;
        let translated_kind = tycker.statics.type_kind(translated);
        Lub::lub(seal_kind, translated_kind, tycker)?;
        tycker.record_seal(seal, translated);
        // The definition table holds the checked body, which the editor shows
        // as the definition, and the host records the body before sealing it.
        // A `data` body is what the seal is over. A `codata` body is its
        // translation as a source type, the thunk of what the seal is over,
        // which has the source name's kind.
        if let Some(def) = def {
            let body = match definition.shape {
                | CbvDeclarationShape::Data => translated,
                | CbvDeclarationShape::CoData => {
                    Self::cbv_thunk_beneath_parameters(tycker, &scope.env, translated)?
                }
            };
            let _ = tycker.statics.type_definitions.upsert(def, body);
        }
        Ok(seal)
    }

    /// `fn (X1 : K1) … => Thk C` from `fn (X1 : K1) … => C`, keeping each
    /// parameter's binder.
    pub(super) fn cbv_thunk_beneath_parameters(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, operator: ss::TypeId,
    ) -> Result<ss::TypeId> {
        let filled = tycker.type_filled(&operator);
        let ss::Type::Abs(ss::TypeAbstraction { binder, body }) = (filled)?.to_owned() else {
            return Ok(cs::Thk(operator).build(tycker, env));
        };
        let body = Self::cbv_thunk_beneath_parameters(tycker, env, body)?;
        let domain = binder.domain_kind(tycker);
        let body_kind = tycker.statics.type_kind(body);
        let arrow: ss::KindId = Arrow(domain, body_kind).build(tycker, env);
        Ok(Alloc::alloc(tycker, ss::TypeAbstraction { binder, body }, arrow, &()))
    }

    /// The type a codata declaration's source name denotes:
    /// `fn (Y1 : K1) … => Thk (σ Y1 …)`, so that `S A` translates to
    /// `Thk (σ A')` by the ordinary application rule.
    pub(super) fn cbv_codata_name<'db>(
        &self, tycker: &mut Tycker<'db>, env: &ss::TyEnv, sealed: ss::TypeId,
        domains: &[ss::KindId],
    ) -> ss::TypeId {
        let (applied, binders) =
            domains.iter().fold((sealed, Vec::new()), |(applied, mut binders), &domain| {
                let pattern: ss::TPatId = cs::Pat(ss::Hole, domain).build(tycker, env);
                let witness: ss::AbstId = Alloc::alloc(tycker, pattern, (), &());
                let variable: ss::TypeId = Alloc::alloc(tycker, witness, domain, &());
                binders.push((ss::TypeBinder { pattern, witness }, domain));
                (App(applied, variable).build(tycker, env), binders)
            });
        let thunk = cs::Thk(applied).build(tycker, env);
        binders.into_iter().rev().fold(thunk, |body, (binder, domain)| {
            let body_kind = tycker.statics.type_kind(body);
            let arrow: ss::KindId = Arrow(domain, body_kind).build(tycker, env);
            Alloc::alloc(tycker, ss::TypeAbstraction { binder, body }, arrow, &())
        })
    }

    /// The seal's definition: each parameter opened at the domain the kind
    /// gives it, and the declaration translated beneath them. A `codata`
    /// declaration is translated without its thunk, since the seal goes
    /// inside it.
    ///
    /// `depth` parameters are open in `scope`, and `domains` gives every
    /// parameter's.
    pub(super) fn cbv_seal_definition<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, definition: &CbvSealedDefinition,
        domains: &[ss::KindId], depth: usize,
    ) -> Result<ss::TypeId> {
        let CbvSealedDefinition { parameters, ascriptions, declaration, shape, .. } = definition;
        let (declaration, shape) = (*declaration, *shape);
        // The author's ascriptions at this depth, read where the parameters
        // outside them are bound, and checked as the type clause checks one:
        // against the source kind of what they ascribe, the remaining
        // parameters over `VType`, whichever declaration it is. Read before
        // the parameters were bound, one naming a parameter reached it ahead
        // of its binder.
        for &(_, ascription) in ascriptions.iter().filter(|(at, _)| *at == depth) {
            let ascribed = self.cbv_require_annotation_kind(tycker, scope, ascription)?;
            let vtype = ss::VType.build(tycker, &scope.env);
            let expected = domains[depth..]
                .iter()
                .rev()
                .fold(vtype, |codomain, &domain| Arrow(domain, codomain).build(tycker, &scope.env));
            Lub::lub(ascribed, expected, tycker)?;
        }
        let (Some(&parameter), Some(&domain)) = (parameters.get(depth), domains.get(depth)) else {
            return match shape {
                | CbvDeclarationShape::Data => self.cbv_type(tycker, scope, declaration),
                | CbvDeclarationShape::CoData => self.cbv_sourced(
                    tycker,
                    declaration,
                    |tycker| self.cbv_codata_type(tycker, scope, declaration),
                    |tycker, translated| {
                        let kind = tycker.statics.type_kind(*translated);
                        Some(TermAnnId::Type(*translated, kind))
                    },
                ),
            };
        };
        let opened = self.cbv_open_type_binder(
            tycker,
            scope,
            parameter,
            Some(domain),
            CbvTypeBinderSite::Abstraction,
        )?;
        Lub::lub(domain, opened.kind, tycker)?;
        let body =
            self.cbv_seal_definition(tycker, &opened.scope, definition, domains, depth + 1)?;
        let body_kind = tycker.statics.type_kind(body);
        let arrow: ss::KindId = Arrow(opened.kind, body_kind).build(tycker, &scope.env);
        let binder = ss::TypeBinder { pattern: opened.pattern, witness: opened.witness };
        Ok(Alloc::alloc(tycker, ss::TypeAbstraction { binder, body }, arrow, &()))
    }
}
