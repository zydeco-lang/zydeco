//! Kind and type elaboration for `@[cbv]` blocks.

use super::*;

impl TermChecker<'_> {
    /// The kind a *binder* declares, when it declares one.
    ///
    /// The same test [`Self::cbv_declared_kind`] makes of a binding's
    /// annotation, asked of a binder's: an annotation denoting a kind means the
    /// binder binds a type. A field binder `#C = p` declares `#C :: K` when `p`
    /// declares `K`, as the host's named pattern does; where one may be
    /// written is [`CbvTypeBinderSite`]'s question, asked when it is opened.
    pub(super) fn cbv_type_binder_kind<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId,
    ) -> Result<Option<ss::KindId>> {
        match tycker.scoped.pats[&binder].to_owned() {
            | su::Pattern::Ann(su::Ann { ty, .. }) => self.cbv_annotation_kind(tycker, scope, ty),
            | su::Pattern::Named(su::Named(name, inner)) => {
                let Some(payload) = self.cbv_type_binder_kind(tycker, scope, inner)? else {
                    return Ok(None);
                };
                Ok(Some(Alloc::alloc(tycker, ss::Label(name, payload), (), &())))
            }
            | _ => Ok(None),
        }
    }

    /// The kind an annotation denotes, when it denotes one: a name for a kind,
    /// an arrow between kinds, or a named kind.
    ///
    /// This is the block's **one kind judgment**. A type binding's declared
    /// kind, a type binder's annotation and an ascription's classifier are all
    /// decided here, and a binder's kind is then recorded for the pattern
    /// rule rather than synthesized again by it, so the three places a block
    /// writes a kind cannot disagree about which kinds it may write.
    ///
    /// Only the **names** are handed to ordinary synthesis: one the block
    /// bound, and the intrinsics for `VType` and `CType`. A name is a leaf, so
    /// delegating it reaches nothing a block may not reach, which is the
    /// hazard the classifier guard exists for, and a name always resolves to
    /// something. An arrow is never delegated whole: it could be a *source*
    /// arrow, ill-kinded in the host, whose synthesis would report an error
    /// instead of returning a sort. So `K1 -> K2`, which desugars as `A -> B`
    /// does to a `Pi` over a hole annotated with `K1`, is recognized by its
    /// parts, and is a kind arrow exactly when both of them are kinds, so
    /// `fn (F : VType -> VType) => 0` and the same binder at a name for
    /// `VType -> VType` mean the same thing.
    pub(super) fn cbv_annotation_kind<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, annotation: su::TermId,
    ) -> Result<Option<ss::KindId>> {
        use su::Term as Tm;
        match tycker.scoped.terms[&annotation].to_owned() {
            // A kind name is closed over as a type name is: the block must
            // have bound it, so `VType` is written with its intrinsic or behind
            // a kind binding of the block's own.
            | Tm::Var(def) => {
                self.cbv_require_block_name(tycker, scope, annotation, def)?;
                let checked =
                    TyEnvT::new(scope.env.clone(), annotation).tyck(tycker, Action::syn())?;
                Ok(match checked {
                    | TermAnnId::Kind(kind) => Some(kind),
                    | _ => None,
                })
            }
            | Tm::Internal(su::Internal::VType | su::Internal::CType) => {
                let checked =
                    TyEnvT::new(scope.env.clone(), annotation).tyck(tycker, Action::syn())?;
                Ok(match checked {
                    | TermAnnId::Kind(kind) => Some(kind),
                    | _ => None,
                })
            }
            // A kind the judgment forms itself is formed at its own source
            // node, as every nested judgment is, so the node carries the fact
            // ordinary checking would have recorded. A name above is checked
            // by ordinary checking, which records its own.
            | Tm::Pi(_) | Tm::Label(_) => self.cbv_sourced(
                tycker,
                annotation,
                |tycker| self.cbv_composite_kind(tycker, scope, annotation),
                |_tycker, kind| kind.map(TermAnnId::Kind),
            ),
            | _ => Ok(None),
        }
    }

    /// The composite rows of [`Self::cbv_annotation_kind`]: a kind arrow and
    /// a named kind.
    pub(super) fn cbv_composite_kind<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, annotation: su::TermId,
    ) -> Result<Option<ss::KindId>> {
        use su::Term as Tm;
        match tycker.scoped.terms[&annotation].to_owned() {
            // The binder may be named: `def List (X : VType) : VType = …`
            // declares its kind as `pi (X : VType) . VType`. A kind mentions
            // no type, so a named binder makes a kind arrow only when its
            // codomain does not mention it — and that is decided before the
            // codomain is read, since reading `forall (X : VType) . X -> X`
            // as a kind would synthesize `X` ahead of its binder, which the
            // checker refuses by panicking.
            | Tm::Pi(su::Pi(binders, codomain)) => {
                self.cbv_kind_telescope(tycker, scope, &binders, codomain)
            }
            // `#C :: K`, a named kind, recognized by its payload as an arrow is
            // by its sides: `#f :: A` writes a field *type* with the same node,
            // and is a kind exactly when its payload is one.
            | Tm::Label(su::Label(name, inner)) => {
                let Some(payload) = self.cbv_annotation_kind(tycker, scope, inner)? else {
                    return Ok(None);
                };
                Ok(Some(Alloc::alloc(tycker, ss::Label(name, payload), (), &())))
            }
            | _ => Ok(None),
        }
    }

    /// The kind arrows of a telescope, or `None` if it does not read as one.
    pub(super) fn cbv_kind_telescope<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binders: &[su::PatId],
        codomain: su::TermId,
    ) -> Result<Option<ss::KindId>> {
        let Some((binder, later)) = binders.split_first() else {
            return self.cbv_annotation_kind(tycker, scope, codomain);
        };
        let binder = *binder;
        let Some((mut bare, domain)) = self.cbv_optional_binder_annotation(tycker, binder) else {
            return Ok(None);
        };
        // An annotation inside the binder leaves it the binder it annotates, as
        // at every binder, and is one more kind the domain must agree with.
        let mut inner_annotations = Vec::new();
        while let su::Pattern::Ann(su::Ann { tm, ty }) = tycker.scoped.pats[&bare] {
            inner_annotations.push(ty);
            bare = tm;
        }
        match tycker.scoped.pats[&bare] {
            | su::Pattern::Hole(_) => {}
            | su::Pattern::Var(def) => {
                let mentioned = std::iter::once(su::EntityId::from(codomain))
                    .chain(later.iter().map(|later| su::EntityId::from(*later)))
                    .any(|entity| {
                        if !tycker.source_contexts.mentions_def(entity, &def)
                            && !tycker.source_contexts.has_source_boundary(entity)
                        {
                            return false;
                        }
                        let defs = std::collections::HashSet::from([def]);
                        Traversal::new(tycker.scoped)
                            .run(entity, &mut CbvMentions { defs: &defs })
                            .expect("resolved source syntax is acyclic")
                            .is_break()
                    });
                if mentioned {
                    return Ok(None);
                }
            }
            | _ => return Ok(None),
        }
        let Some(domain_kind) = self.cbv_annotation_kind(tycker, scope, domain)? else {
            return Ok(None);
        };
        let mut inner_kinds = Vec::with_capacity(inner_annotations.len());
        for annotation in inner_annotations {
            let Some(kind) = self.cbv_annotation_kind(tycker, scope, annotation)? else {
                return Ok(None);
            };
            inner_kinds.push((annotation, kind));
        }
        let Some(codomain) = self.cbv_kind_telescope(tycker, scope, later, codomain)? else {
            return Ok(None);
        };
        // A kind arrow formed, so its binder is one: it gets the facts
        // ordinary checking would have recorded for it, and each annotation in
        // it is settled for the pattern rule, which compares them.
        for (annotation, kind) in inner_kinds {
            let _ = tycker.settled_pattern_annotations.upsert(annotation, kind.into());
        }
        self.cbv_record_kind_binder(tycker, scope, binder, domain, domain_kind)?;
        Ok(Some(Arrow(domain_kind, codomain).build(tycker, &scope.env)))
    }

    /// Open a type binder: guard its shape, check that its kind is a source
    /// kind, allocate its witness, and extend the scope with it.
    ///
    /// **Shared by every type binder a block writes** — `fn (X : K) => M`
    /// over a term, `fn (X : K) => T` over a type, a quantifier and an
    /// existential — so that the two guards hold at every entrance by
    /// construction: an inner annotation may not read a term bound outside the
    /// block, and `fn (R : CType) => 0` is refused, wherever the binder sits.
    ///
    /// A binder that declares no kind takes `expected`, as `fn x => M` takes
    /// its domain from an expected arrow; one that declares a kind is checked
    /// at it, and the caller compares what it builds with what it expected.
    ///
    /// A field binder `#C = (X : K)`, admitted at a package's binder, binds
    /// `X` at the payload kind `K`, as the host's existential does: the
    /// witness stands for the whole `#C :: K`, and `X` for what it carries.
    pub(super) fn cbv_open_type_binder<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId,
        expected: Option<ss::KindId>, site: CbvTypeBinderSite,
    ) -> Result<CbvTypeBinder> {
        let (pattern, kind, mut env) =
            self.cbv_check_type_binder(tycker, scope, binder, expected, site)?;
        let witness: ss::AbstId = Alloc::alloc(tycker, pattern, (), &());
        if let (Some(def), payload_kind) = pattern.try_destruct_def(tycker) {
            let variable: ss::TypeId = Alloc::alloc(tycker, witness, payload_kind, &());
            env += [(def, variable.into())];
        }
        let scope = scope.type_binding(env);
        Ok(CbvTypeBinder { pattern, kind, witness, scope })
    }

    /// Check a type binder's pattern without opening it: the half of
    /// [`Self::cbv_open_type_binder`] a manifest existential shares, whose
    /// binder is assigned its disclosed definition instead of a skolem, and so
    /// does a kind arrow's binder before an inferred result, which binds
    /// nothing the definition reads.
    ///
    /// The binder's kind annotations are settled first, by
    /// [`Self::cbv_settle_type_binder`], so the pattern rule checks the
    /// pattern against the kind the block decided and synthesizes none of
    /// them itself.
    pub(super) fn cbv_check_type_binder<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId,
        expected: Option<ss::KindId>, site: CbvTypeBinderSite,
    ) -> Result<(ss::TPatId, ss::KindId, ss::TyEnv)> {
        let declared = self.cbv_settle_type_binder(tycker, scope, binder, site)?;
        let action = match (declared, expected) {
            | (Some(kind), _) | (None, Some(kind)) => PatternAction::ana(kind.into()),
            | (None, None) => PatternAction::syn(),
        };
        let bound = TyEnvT::new(scope.env.clone(), binder).tyck(tycker, action)?;
        let (pattern, kind) = bound.inner.as_type();
        self.cbv_require_source_kind(tycker, kind)?;
        Ok((pattern, kind, bound.info))
    }

    /// Guard a type binder's shape, decide the kind it declares, and settle
    /// each kind annotation in it for the pattern rule.
    ///
    /// The shape is a name or a wildcard, an annotation of one, and — at a
    /// package's binder — a field binder `#C = p` over one. Each annotation's
    /// kind comes from [`Self::cbv_annotation_kind`], and an annotation
    /// that is not a kind is refused here. One that reaches outside the block
    /// is reported as that, by the classifier guard, before it is reported as
    /// the wrong sort.
    pub(super) fn cbv_settle_type_binder<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId,
        site: CbvTypeBinderSite,
    ) -> Result<Option<ss::KindId>> {
        match tycker.scoped.pats[&binder].to_owned() {
            | su::Pattern::Var(_) | su::Pattern::Hole(_) => Ok(None),
            | su::Pattern::Ann(su::Ann { tm, ty }) => {
                let Some(kind) = self.cbv_annotation_kind(tycker, scope, ty)? else {
                    self.cbv_classifier_shape(tycker, scope, ty)?;
                    return tycker.err(TyckError::SortMismatch);
                };
                let _ = tycker.settled_pattern_annotations.upsert(ty, kind.into());
                self.cbv_settle_type_binder(tycker, scope, tm, site)?;
                Ok(Some(kind))
            }
            | su::Pattern::Named(su::Named(name, inner)) if site == CbvTypeBinderSite::Package => {
                let Some(payload) = self.cbv_settle_type_binder(tycker, scope, inner, site)?
                else {
                    return Ok(None);
                };
                Ok(Some(Alloc::alloc(tycker, ss::Label(name, payload), (), &())))
            }
            | _ => tycker.err(
                TyckError::Expressivity(
                    "a call-by-value type binder must be a name, a wildcard, or an annotation of one, and only a package's may name its field",
                )),
        }
    }

    /// Require a translated source type to be a **value** type.
    ///
    /// Every position a core former fills with a value type is checked,
    /// because a source type may be a type operator, whose kind is an arrow.
    pub(super) fn cbv_require_value_type<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, ty: ss::TypeId,
    ) -> Result<()> {
        let vtype = ss::VType.build(tycker, &scope.env);
        let actual = tycker.statics.type_kind(ty);
        Lub::lub(vtype, actual, tycker)?;
        Ok(())
    }

    /// `[[exists (X : K) . A]]v = exists (X : K) . [[A]]v`.
    ///
    /// Positive, so the body is a value-type position and takes no shift.
    pub(super) fn cbv_exists<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId, body: su::TermId,
    ) -> Result<ss::TypeId> {
        let opened =
            self.cbv_open_type_binder(tycker, scope, binder, None, CbvTypeBinderSite::Package)?;
        let body = self.cbv_type(tycker, &opened.scope, body)?;
        self.cbv_require_value_type(tycker, &opened.scope, body)?;
        let vtype = ss::VType.build(tycker, &scope.env);
        let binder = ss::TypeBinder { pattern: opened.pattern, witness: opened.witness };
        Ok(Alloc::alloc(tycker, ss::Exists::new(binder, body), vtype, &()))
    }

    /// `[[exists (X as S : K) . A]]v = exists (X as [[S]]v : K) . [[A]]v`.
    ///
    /// The manifest form discloses its witness, so the binder is not opened
    /// to a skolem: it is assigned the translated definition, and the body
    /// sees `X` as that type, which is also what keeps the equation true on
    /// the translated side. The binder may name its field, `#C = (X : K)`,
    /// and then the definition is the payload of `(#C = S)`. A definition that
    /// is a kind, as in `(VType as @(intrinsic(vtype)))`, is no source type
    /// and is refused by sort.
    ///
    /// The steps are the host's for a type definition, with each source part
    /// read by the block: the definition by the type clause, the binder by
    /// the same check every type binder takes, and the body under the
    /// assignment. The type is then built by the host's own judgment.
    pub(super) fn cbv_manifest_exists<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, _term: su::TermId,
        su::ManifestExists { binder, definition, body }: su::ManifestExists,
    ) -> Result<ss::TypeId> {
        // The kind the binder declares for its payload is known before the
        // definition is read, as at a pack, so an operator definition may
        // leave its own binder's kind unwritten.
        let payload_binder = match tycker.scoped.pats[&binder] {
            | su::Pattern::Named(su::Named(_, inner)) => inner,
            | _ => binder,
        };
        let definition = match self.cbv_type_binder_kind(tycker, scope, payload_binder)? {
            | Some(kind) => self.cbv_type_against(tycker, scope, definition, kind)?,
            | None => self.cbv_type(tycker, scope, definition)?,
        };
        let definition_kind = tycker.statics.type_kind(definition);
        let (pattern, _kind, env) = self.cbv_check_type_binder(
            tycker,
            scope,
            binder,
            Some(definition_kind),
            CbvTypeBinderSite::Package,
        )?;
        let witness: ss::AbstId = Alloc::alloc(tycker, pattern, (), &());
        let payload_kind = tycker.statics.annotations_abst[&witness];
        Lub::lub(payload_kind, definition_kind, tycker)?;
        let full_definition = pattern.introduce_payload(tycker, definition);
        let full_definition = (full_definition)?;
        let assigned = TyEnvT::new(env, Assign(pattern, full_definition)).tyck(tycker, ())?;
        let body_scope = scope.type_binding(assigned.info.clone());
        let body = self.cbv_type(tycker, &body_scope, body)?;
        self.cbv_require_value_type(tycker, &body_scope, body)?;
        let vtype = ss::VType.build(tycker, &scope.env);
        let exists =
            ss::Exists::with_manifest(ss::TypeBinder { pattern, witness }, definition, body);
        Ok(Alloc::alloc(tycker, exists, vtype, &()))
    }

    /// The type-level abstraction: `[[fn (X : K) => T]]v = fn (X : K) => [[T]]v`.
    ///
    /// The type-level STLC's abstraction, beside the application `F A` the
    /// block already translated. It is reached through a type binding whose
    /// kind is an arrow, `let F : VType -> VType = fn (X : VType) => X * X`,
    /// as a type argument, or at the head of an application.
    ///
    /// `against` is the expected arrow kind's domain and codomain, when
    /// [`Self::cbv_type_against`] has one: the binder may then leave its kind
    /// unwritten, and the body is translated against the codomain, so a curried
    /// operator needs its kinds written nowhere.
    pub(super) fn cbv_type_operator<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binders: &[su::PatId], body: su::TermId,
        against: Option<(ss::KindId, ss::KindId)>,
    ) -> Result<ss::TypeId> {
        let (binder, later) = binders.split_first().expect("a telescope has a binder");
        let binder = *binder;
        if against.is_none() && self.cbv_optional_binder_annotation(tycker, binder).is_none() {
            return tycker.err(TyckError::Expressivity(
                "a type operator's binder needs its kind, written or taken from an expected arrow kind",
            ));
        }
        let opened = self.cbv_open_type_binder(
            tycker,
            scope,
            binder,
            against.map(|(domain, _)| domain),
            CbvTypeBinderSite::Abstraction,
        )?;
        let body = match (later, against) {
            | ([], Some((_, codomain))) => {
                self.cbv_type_against(tycker, &opened.scope, body, codomain)?
            }
            | ([], None) => self.cbv_type(tycker, &opened.scope, body)?,
            | (_, Some((_, codomain))) => {
                let ss::Kind::Arrow(Arrow(next_domain, next_codomain)) =
                    tycker.kind_filled(&codomain)?.to_owned()
                else {
                    return tycker.err(TyckError::KindMismatch);
                };
                let against = Some((next_domain, next_codomain));
                self.cbv_type_operator(tycker, &opened.scope, later, body, against)?
            }
            | (_, None) => self.cbv_type_operator(tycker, &opened.scope, later, body, None)?,
        };
        let body_kind = tycker.statics.type_kind(body);
        let arrow: ss::KindId = Arrow(opened.kind, body_kind).build(tycker, &scope.env);
        let binder = ss::TypeBinder { pattern: opened.pattern, witness: opened.witness };
        Ok(Alloc::alloc(tycker, ss::TypeAbstraction { binder, body }, arrow, &()))
    }

    /// Type translation: a source type read as scoped syntax becomes the
    /// statics type its call-by-value translation denotes.
    pub(super) fn cbv_type<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, ty: su::TermId,
    ) -> Result<ss::TypeId> {
        self.cbv_sourced(
            tycker,
            ty,
            |tycker| self.cbv_type_inner(tycker, scope, ty),
            |tycker, translated| {
                let kind = tycker.statics.type_kind(*translated);
                Some(TermAnnId::Type(*translated, kind))
            },
        )
    }

    /// Translate a source type against an expected kind: `[[T]]v ⇐ K`.
    ///
    /// The type level's analysis direction, as an expected arrow is the term
    /// level's. A type operator against an arrow kind may leave its binder's
    /// kind unwritten and takes the domain instead, and its body is translated
    /// against the codomain, so `let F : VType -> VType = fn X => X * X` reads
    /// as the host reads it. Every other form synthesizes. Either way the
    /// result's kind is then compared with the expectation, which is also what
    /// checks a binder's written kind against the expected domain.
    ///
    /// It is used wherever a kind is known before the type is translated: a
    /// type binding's declared kind, a quantifier's binder at an instantiation,
    /// an existential's binder at a pack, a manifest existential's binder at
    /// its definition, an operator's domain at an application, and an
    /// ascription. So an operator written as the host writes it, under a
    /// declared kind, is read as the host reads it.
    pub(super) fn cbv_type_against<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, ty: su::TermId, expected: ss::KindId,
    ) -> Result<ss::TypeId> {
        if let su::Term::Named(su::Named(name, inner)) = tycker.scoped.terms[&ty].to_owned()
            && let ss::Kind::Label(Label(label, payload)) =
                tycker.kind_filled(&expected)?.to_owned()
            && label == name
        {
            let translated = self.cbv_sourced(
                tycker,
                ty,
                |tycker| {
                    let inner = self.cbv_type_against(tycker, scope, inner, payload)?;
                    self.cbv_named_type(tycker, scope, ty, inner)
                },
                |tycker, translated| {
                    let kind = tycker.statics.type_kind(*translated);
                    Some(TermAnnId::Type(*translated, kind))
                },
            )?;
            let actual = tycker.statics.type_kind(translated);
            Lub::lub(expected, actual, tycker)?;
            return Ok(translated);
        }
        let operator = match tycker.scoped.terms[&ty].to_owned() {
            | su::Term::Abs(su::Abs(binders, body)) => {
                let binder = binders[0];
                match tycker.kind_filled(&expected)? {
                    | ss::Kind::Arrow(Arrow(domain, codomain))
                        if self.cbv_optional_binder_annotation(tycker, binder).is_none()
                            || self.cbv_type_binder_kind(tycker, scope, binder)?.is_some() =>
                    {
                        Some((binders, body, domain, codomain))
                    }
                    // A binder that declares nothing makes this an operator, and the
                    // expectation is not an arrow kind: an operator stands where a
                    // non-operator is expected. That is a kind error; synthesizing it
                    // instead reported a binder missing its kind, which was not what
                    // went wrong.
                    | _ if self.cbv_optional_binder_annotation(tycker, binder).is_none() => {
                        return tycker.err(TyckError::KindMismatch);
                    }
                    | _ => None,
                }
            }
            | _ => None,
        };
        let translated = match operator {
            | Some((binders, body, domain, codomain)) => self.cbv_sourced(
                tycker,
                ty,
                |tycker| {
                    self.cbv_type_operator(tycker, scope, &binders, body, Some((domain, codomain)))
                },
                |tycker, translated| {
                    let kind = tycker.statics.type_kind(*translated);
                    Some(TermAnnId::Type(*translated, kind))
                },
            )?,
            | None => self.cbv_type(tycker, scope, ty)?,
        };
        let actual = tycker.statics.type_kind(translated);
        Lub::lub(expected, actual, tycker)?;
        Ok(translated)
    }

    /// `#l = S'` from a translated payload, built by the host's own judgment.
    pub(super) fn cbv_named_type<'db>(
        &self, tycker: &mut Tycker<'db>, _scope: &CbvScope, ty: su::TermId, inner: ss::TypeId,
    ) -> Result<ss::TypeId> {
        let su::Term::Named(su::Named(name, _)) = tycker.scoped.terms[&ty].to_owned() else {
            unreachable!("the type and rejection arms of named judgments only run on Named terms")
        };
        let kind = tycker.statics.type_kind(inner);
        let kind_id = Alloc::alloc(tycker, ss::Kind::Label(ss::Label(name, kind)), (), &());
        Ok(Alloc::alloc(tycker, ss::Type::Named(ss::Named(name, inner)), kind_id, &()))
    }

    pub(super) fn cbv_type_inner<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, ty: su::TermId,
    ) -> Result<ss::TypeId> {
        use su::Term as Tm;
        match tycker.scoped.terms[&ty].to_owned() {
            // [[forall (X : K) . A]]v = Thk (forall (X : K) . Ret [[A]]v)
            //
            // The same `Pi` node as an arrow; a binder that declares a kind
            // makes it a quantifier. Structural, so it needs no seal and no
            // query.
            | Tm::Pi(su::Pi(binders, body)) => {
                let computation =
                    self.cbv_function_computation(tycker, scope, &binders, body)?;
                Ok(cs::Thk(computation).build(tycker, &scope.env))
            }
            // [[exists (X : K) . A]]v = exists (X : K) . [[A]]v
            //
            // A `sigma` binder that declares a kind. Positive, so no shift. The
            // manifest form `exists (X as T : K) . A` is a different node, with
            // a clause of its own below.
            | Tm::Sigma(su::Sigma(binder, body))
                if self.cbv_type_binder_kind(tycker, scope, binder)?.is_some() =>
            {
                self.cbv_exists(tycker, scope, binder, body)
            }
            // [[fn (X : K) => T]]v = fn (X : K) => [[T]]v
            //
            // A binder that declares a kind makes this a type operator. One that
            // declares a type is a value lambda, which no type position admits,
            // and is left to the leaf guard to refuse; one that declares nothing
            // is the next arm's.
            | Tm::Abs(su::Abs(ref binders, body))
                if self.cbv_type_binder_kind(tycker, scope, binders[0])?.is_some() =>
            {
                self.cbv_type_operator(tycker, scope, binders, body, None)
            }
            // A binder that declares nothing makes an operator only against an
            // expected arrow kind, which `cbv_type_against` supplies; reached
            // here, there was none to take the kind from. The leaf guard would
            // have refused it too, but by describing how a source type may be
            // built, which is not what went wrong.
            | Tm::Abs(su::Abs(ref binders, _))
                if self.cbv_optional_binder_annotation(tycker, binders[0]).is_none() =>
            {
                tycker.err(
                    TyckError::Expressivity(
                        "a type operator's binder needs its kind, written or taken from an expected arrow kind",
                    ))
            }
            // [[A1 * … * An]]v = [[A1]]v * … * [[An]]v
            //
            // A product is positive, so its translation needs no shift: both
            // sides are value types. The surface writes `A * B` and `(v, w)`
            // with one node, since checking decides the sort, so this clause and
            // the value clause below destructure the same shape.
            | Tm::Cons(components) => {
                // T-PROD's premise, for the same reason as the arrow's.
                let components = components
                    .into_iter()
                    .map(|component| {
                        let component = self.cbv_type(tycker, scope, component)?;
                        self.cbv_require_value_type(tycker, scope, component)?;
                        Ok(component)
                    })
                    .collect::<Result<Vec<_>>>()?;
                Ok(Prod(components).build(tycker, &scope.env))
            }
            // `data … end` and `codata … end` arrive wrapped in the ascription the
            // desugarer adds to tell the host their sort, `VType` and `CType`.
            // That is the declaration's host kind, not its source kind: a source
            // codata type is a value type whose translation is a thunk, so
            // checking the marker against the translation refused every codata
            // written in place. The declaration is translated by its own clause
            // instead. An ascription the author writes around it is a separate
            // node, checked by the next arm as usual.
            | Tm::Ann(su::Ann { tm, .. })
                if matches!(tycker.scoped.terms[&tm], Tm::Data(_) | Tm::CoData(_)) =>
            {
                self.cbv_type(tycker, scope, tm)
            }
            // `(A : K)`, an ascribed source type.
            //
            // Structural rather than left to the leaf rule, which resolves
            // what it is given by ordinary synthesis — and a source type is
            // not a host type. `((Int -> Int) : VType)` came back as a host
            // complaint about `val pi`, a diagnostic from outside the block
            // about syntax the block had already accepted unascribed. Two
            // spellings of one type must not get opposite verdicts.
            //
            // The classifier is read by the block's kind judgment, as a
            // binder's kind is. A classifier that is not a kind goes through
            // the shape guard first, so one reaching outside the block is
            // reported as that rather than as the wrong sort.
            | Tm::Ann(su::Ann { tm, ty }) => {
                let Some(declared) = self.cbv_annotation_kind(tycker, scope, ty)? else {
                    self.cbv_classifier_shape(tycker, scope, ty)?;
                    return tycker.err(TyckError::SortMismatch);
                };
                self.cbv_type_against(tycker, scope, tm, declared)
            }
            // [[data | +C1 : A1 | … end]]v = data | +C1 : [[A1]]v | … end
            //
            // A data type written in place. Positive, so the arms are value-type
            // positions and the whole takes no shift. It is transparent, and the
            // host compares transparent data by structure, so a block and its
            // host that write the same declaration agree on the type.
            | Tm::Data(su::Data { arms }) => {
                let arms = arms
                    .into_iter()
                    .map(|su::DataArm { name, param }| {
                        let param = self.cbv_type(tycker, scope, param)?;
                        self.cbv_require_value_type(tycker, scope, param)?;
                        Ok((name, param))
                    })
                    .collect::<Result<Vec<_>>>()?;
                let vtype = ss::VType.build(tycker, &scope.env);
                let data_id = tycker.statics.datas.alloc(ss::Data::new(arms));
                Ok(Alloc::alloc(tycker, data_id, vtype, &()))
            }
            // [[codata | .d1 : A1 | … end]]v = Thk (codata | .d1 : Ret [[A1]]v | … end)
            //
            // A codata type written in place. Negative, so the shift stays
            // outside and every field is a returner, as the image row it inverts
            // has it: a field is a suspended computation, run when observed.
            | Tm::CoData(_) => {
                let codata = self.cbv_codata_type(tycker, scope, ty)?;
                Ok(cs::Thk(codata).build(tycker, &scope.env))
            }
            // [[#f :: A]]v = #f :: [[A]]v
            //
            // A field type, of which a record is a product. A field name is not
            // a type former: it carries no polarity, so the payload is a
            // value-type position and the whole takes no shift. The same node
            // spells a named *kind*, `#C :: VType`, which is no type: its
            // payload is refused as one, by sort.
            | Tm::Label(su::Label(name, inner)) => {
                let inner = self.cbv_type(tycker, scope, inner)?;
                self.cbv_require_value_type(tycker, scope, inner)?;
                let vtype = ss::VType.build(tycker, &scope.env);
                Ok(Alloc::alloc(tycker, ss::Label(name, inner), vtype, &()))
            }
            // [[(l = S)]]v = (l = [[S]]v)
            //
            // A named type, at the named kind `l :: K` of its payload's kind
            // `K`: a package's witness at a labeled binder, written in place.
            // Built by the host's own judgment over the translated payload.
            | Tm::Named(su::Named(_, inner)) => {
                let inner = self.cbv_type(tycker, scope, inner)?;
                self.cbv_named_type(tycker, scope, ty, inner)
            }
            // [[S/l]]v = [[S]]v/l
            //
            // A type projection, selecting a named kind's payload. The field is
            // resolved by the host's resolver on the translated head's kind,
            // as a term projection's is on its head's type.
            | Tm::Proj(su::Proj(head, name)) => {
                let head = self.cbv_type(tycker, scope, head)?;
                let head_kind = tycker.statics.type_kind(head);
                let candidate = FieldProjectionResolver::r#type(tycker, head_kind, &name)?;
                let payload_kind = candidate.projected;
                FieldProjectionResolver::project_type(tycker, head, candidate, payload_kind)
            }
            // [[exists (X as S : K) . A]]v = exists (X as [[S]]v : K) . [[A]]v
            | Tm::ManifestExists(manifest) => {
                self.cbv_manifest_exists(tycker, scope, ty, *manifest)
            }
            // [[F A1 … An]]v = F [[A1]]v … [[An]]v
            | Tm::App(_) => self.cbv_type_app(tycker, scope, ty),
            // An import must be rejected before the leaf rule, which would hand
            // it to ordinary synthesis and could well get a primitive back.
            | Tm::SourceBoundary(_) => self.cbv_reject_import(tycker),
            // [[P]]v = P
            | _ => self.cbv_leaf_type(tycker, scope, ty),
        }
    }

    /// The computation type a `codata` declaration writes, before the thunk
    /// its translation as a source type adds: `codata | .d1 : Ret [[A1]]v |
    /// … end`. A sealed declaration puts its seal on this.
    pub(super) fn cbv_codata_type<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, ty: su::TermId,
    ) -> Result<ss::TypeId> {
        let su::Term::CoData(su::CoData { arms }) = tycker.scoped.terms[&ty].to_owned() else {
            unreachable!("only a codata declaration is translated as one")
        };
        let arms = arms
            .into_iter()
            .map(|su::CoDataArm { name, out }| {
                let out = self.cbv_type(tycker, scope, out)?;
                self.cbv_require_value_type(tycker, scope, out)?;
                Ok((name, cs::Ret(out).build(tycker, &scope.env)))
            })
            .collect::<Result<Vec<_>>>()?;
        let ctype = ss::CType.build(tycker, &scope.env);
        let codata_id = tycker.statics.codatas.alloc(ss::CoData::new(arms));
        Ok(Alloc::alloc(tycker, codata_id, ctype, &()))
    }

    /// A type operator applied to source types:
    /// `[[F A1 … An]]v = [[F]]v [[A1]]v … [[An]]v`, then normalized.
    ///
    /// The translation is a homomorphism on the type-level STLC, so the head is
    /// a source type like the arguments and is translated like them. Two
    /// spellings of one type therefore agree: `(fn (X : VType) => X * X) Int`
    /// and `F Int`, with `F` bound to the same operator, and `Id (Int * Int)`
    /// and `Id Pair`, with `Pair = Int * Int`.
    ///
    /// The spine is collected by iteration, so an operator's arity is not a
    /// limit on the stack, for the same reason the tuple clause is iterative.
    ///
    /// The applied result needs no test of its own. The head and the arguments
    /// are translations, every name in them the block's own, so the result is
    /// the translation of the source application; an operator a block binds
    /// cannot launder a type the block could not write. While a head could be
    /// named outside, the result was tested for membership in the image, since
    /// an outside `fn (X : VType) => Thk (Ret X)` applied normalized to a type
    /// no source type translates to.
    pub(super) fn cbv_type_app<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, ty: su::TermId,
    ) -> Result<ss::TypeId> {
        let mut arguments = Vec::new();
        let mut head = ty;
        while let su::Term::App(su::App(function, group)) = tycker.scoped.terms[&head].to_owned() {
            arguments.extend(group.into_iter().rev());
            head = function;
        }
        arguments.reverse();
        let operator = self.cbv_type(tycker, scope, head)?;
        // Each argument is translated against the domain the operator's kind
        // gives it, so an operator passed as an argument may leave its binder's
        // kind unwritten. Past the last arrow there is no domain to give, and
        // `normalize_apps` reports the over-application.
        let mut remaining = tycker.statics.type_kind(operator);
        let arguments = arguments
            .into_iter()
            .map(|argument| match tycker.kind_filled(&remaining)? {
                | ss::Kind::Arrow(Arrow(domain, codomain)) => {
                    remaining = codomain;
                    self.cbv_type_against(tycker, scope, argument, domain)
                }
                | _ => self.cbv_type(tycker, scope, argument),
            })
            .collect::<Result<Vec<_>>>()?;
        operator.normalize_apps(tycker, arguments)
    }

    /// Admit only type-level shapes in a classifier, before any of it is
    /// handed to ordinary synthesis.
    ///
    /// This is a whitelist, so a form nobody listed is refused. That direction
    /// is deliberate: the two defects it exists to prevent were both a subtree
    /// delegated to ordinary synthesis unexamined, and in both the subtree
    /// reached something a block may not reach while still producing a
    /// perfectly well-formed type. An annotation carried an import past the
    /// import rule, and a `@[typeof]` query read a term variable bound outside
    /// the block past the closedness rule.
    ///
    /// So this test is a **provenance guard, not a grammar of types**. Whether
    /// the classifier denotes an admissible type is decided afterwards, on the
    /// resolved type; all this has to do is stop the subtree reaching outward.
    /// Pure type structure reaches nothing and is therefore admitted.
    ///
    /// The test admits more than a bare name so that two spellings of one type
    /// get the same verdict: `fn (x : Id Int) => x` is accepted exactly when
    /// binding `Id Int` to a name and writing that name is.
    ///
    /// **The whitelist is still deliberately conservative**, and the cost is
    /// known: a classifier built from a type former other than application,
    /// ascription, a label or an arrow is refused even when it denotes an
    /// admissible type. Widening it is adding a row, and each row owes a probe
    /// showing that the form it admits cannot reach an import or a term.
    ///
    /// Most source type formers never reach it: each has a clause of its own
    /// in the type judgment, and a binder's or an ascription's kind is read
    /// by [`Self::cbv_annotation_kind`]. What still comes here is a name,
    /// for the leaf rule, and a classifier the kind judgment did not recognize,
    /// which this guard sees before it is refused by sort.
    pub(super) fn cbv_classifier_shape<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, ty: su::TermId,
    ) -> Result<()> {
        use su::Term as Tm;
        match tycker.scoped.terms[&ty].to_owned() {
            | Tm::Var(def) => self.cbv_require_block_name(tycker, scope, ty, def),
            | Tm::Internal(_) => Ok(()),
            | Tm::App(su::App(function, arguments)) => {
                self.cbv_classifier_shape(tycker, scope, function)?;
                arguments
                    .into_iter()
                    .try_for_each(|argument| self.cbv_classifier_shape(tycker, scope, argument))
            }
            | Tm::Ann(su::Ann { tm, ty }) => {
                self.cbv_classifier_shape(tycker, scope, tm)?;
                self.cbv_classifier_shape(tycker, scope, ty)
            }
            // A label reaches only what its payload reaches. A classifier that
            // is not a kind passes through here before being refused by sort,
            // so `(S : (#C :: Int))` is reported as a type where a kind belongs
            // rather than as built from a forbidden form.
            | Tm::Label(su::Label(_, inner)) => self.cbv_classifier_shape(tycker, scope, inner),
            // A `Pi` reaches this test as a kind arrow, in an ascription's
            // classifier or as a binder's kind; a source arrow is handled by
            // the arrow rule before the leaf rule runs.
            //
            // The binder is guarded whole, because the pattern under its
            // outermost annotation can itself be an annotation, which the
            // ordinary pattern checker descends into afterwards. In
            // `(Id : pi ((X : @[typeof] (@[typeof] outside)) : VType) . VType) Int`
            // the inner one reads a term variable bound outside the block,
            // and the result would still be a well-formed core term, so the
            // arena invariants cannot catch it.
            | Tm::Pi(su::Pi(binders, body)) => {
                binders.into_iter().try_for_each(|binder| {
                    self.cbv_classifier_binder_shape(tycker, scope, binder)
                })?;
                self.cbv_classifier_shape(tycker, scope, body)
            }
            | Tm::SourceBoundary(_) => self.cbv_reject_import(tycker),
            | _ => tycker.err(
                TyckError::Expressivity(
                    "a call-by-value source type must be built from names, type application and ascription",
                )),
        }
    }

    /// The same guard, over a classifier's binder.
    ///
    /// A kind arrow's binder names a kind variable or ignores it, so a name
    /// and a wildcard are admitted and an annotation is admitted by recursion
    /// on both of its parts. Everything else is refused — a view pattern, for
    /// one, carries a whole term, which is the shape this guard exists to stop.
    ///
    /// Like its term counterpart this is a provenance guard and not a grammar:
    /// all it has to establish is that nothing reachable from here reaches
    /// outward.
    pub(super) fn cbv_classifier_binder_shape<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId,
    ) -> Result<()> {
        match tycker.scoped.pats[&binder].to_owned() {
            | su::Pattern::Var(_) | su::Pattern::Hole(_) => Ok(()),
            | su::Pattern::Ann(su::Ann { tm, ty }) => {
                self.cbv_classifier_binder_shape(tycker, scope, tm)?;
                self.cbv_classifier_shape(tycker, scope, ty)
            }
            | _ => tycker.err(
                TyckError::Expressivity(
                    "a call-by-value source type's binder must be a name, a wildcard, or an annotation of one",
                )),
        }
    }

    /// A source type that none of the structural clauses translates, which is
    /// a **name**: one the block bound, or an intrinsic for a primitive.
    ///
    /// The classifier passes [`Self::cbv_classifier_shape`] before any of it
    /// is synthesized. Handing a subtree to ordinary synthesis unexamined would
    /// defeat the closure a block claims: an annotation wrapper could carry an
    /// import past the import rule (`(@(import("…")) : VType)`), and a
    /// classifier expression could read a term variable bound outside the
    /// block (`@[typeof] outside`). Both would produce a primitive, and the
    /// arena invariants cannot object: the emitted term is well formed, and
    /// only its *provenance* is forbidden. The guard also refuses a name the
    /// block did not bind.
    ///
    /// What a block-bound name resolves to needs no test of its own. A type
    /// binding's name denotes its translated bindee, a declaration's name its
    /// seal, and a type variable itself, so every type a block can name is the
    /// translation of a source type by construction. While a block could name
    /// a host declaration, the resolved type was tested for membership in the
    /// translation's image, row by row, which is what kept `Thk (Ret Int)` out.
    /// An intrinsic is now the one name that is not the block's, and it is
    /// admitted only for a source primitive: `@(intrinsic(thk))` reaches
    /// nothing outside the block, but the shift it denotes is no source type.
    pub(super) fn cbv_leaf_type<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, ty: su::TermId,
    ) -> Result<ss::TypeId> {
        self.cbv_classifier_shape(tycker, scope, ty)?;
        if let su::Term::Internal(internal) = tycker.scoped.terms[&ty] {
            Self::cbv_require_source_intrinsic(tycker, internal)?;
        }
        let checked = TyEnvT::new(scope.env.clone(), ty).tyck(tycker, Action::syn())?;
        let (ty, _kind) = checked.try_as_type(tycker, TyckError::SortMismatch)?;
        Ok(ty)
    }

    /// Refuse a type or kind name the block did not bind.
    ///
    /// The type-level twin of the term rule: an outside type enters a block
    /// by type abstraction, which the host instantiates, and a built-in one is
    /// written with its intrinsic.
    ///
    /// The refusal is raised at the name's own source site, so it points at the
    /// name whichever judgment asked. That matters for the kind judgment, which
    /// asks about a binder's annotation before anything has established a site
    /// for it: an outside kind in a definition's parameter is reported at the
    /// parameter, not at the declaration's body.
    pub(super) fn cbv_require_block_name<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, name: su::TermId, def: DefId,
    ) -> Result<()> {
        if scope.block_names.contains(&def) {
            return Ok(());
        }
        self.cbv_sourced(
            tycker,
            name,
            |tycker| {
                tycker.err(TyckError::Expressivity(
                    "a call-by-value block may not name a type or kind declared outside it",
                ))
            },
            |_tycker, ()| None,
        )
    }

    /// Refuse an intrinsic that names no source primitive: a primitive type,
    /// `unit`, or a kind, which the kind judgment and the sort decide further.
    #[track_caller]
    pub(super) fn cbv_require_source_intrinsic(
        tycker: &mut Tycker<'_>, internal: su::Internal,
    ) -> Result<()> {
        match internal {
            | su::Internal::Unit
            | su::Internal::Primitive(_)
            | su::Internal::VType
            | su::Internal::CType => Ok(()),
            | _ => tycker.err(
                TyckError::Expressivity(
                    "an intrinsic inside a call-by-value block must name a primitive source type, `unit`, or a kind",
                )),
        }
    }

    /// Whether a kind is one the source language has.
    ///
    /// A call-by-value source type is always a value type, so the source's
    /// kinds are `VType` and arrows between them. `CType` is not one: no source
    /// type is a computation, and a binder standing for one has no preimage, so
    /// a `forall` over it is not `[[A]]v` for any `A`.
    pub(super) fn cbv_source_kind<'db>(
        &self, tycker: &mut Tycker<'db>, kind: ss::KindId,
    ) -> Result<bool> {
        Ok(match tycker.kind_filled(&kind)? {
            | ss::Kind::VType(_) => true,
            | ss::Kind::Arrow(Arrow(domain, codomain)) => {
                self.cbv_source_kind(tycker, domain)? && self.cbv_source_kind(tycker, codomain)?
            }
            // A **labeled kind**, which a manifest-kind package binder carries:
            // `exists (#C = (Rep as Int) : VType) . A` binds at `#C :: VType`.
            // The kind translation is the identity, `K* = K`, so a label adds
            // nothing to decide and the payload settles it — the same reason
            // the three named-field type rows commute.
            | ss::Kind::Label(Label(_, payload)) => self.cbv_source_kind(tycker, payload)?,
            | _ => false,
        })
    }

    /// Refuse a type binder whose kind the source language does not have.
    #[track_caller]
    pub(super) fn cbv_require_source_kind<'db>(
        &self, tycker: &mut Tycker<'db>, kind: ss::KindId,
    ) -> Result<()> {
        if !self.cbv_source_kind(tycker, kind)? {
            tycker.err(TyckError::Expressivity(
                "a call-by-value type binder's kind must be built from VType",
            ))?
        }
        Ok(())
    }
}
