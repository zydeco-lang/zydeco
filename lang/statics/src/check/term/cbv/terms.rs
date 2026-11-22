//! Computation-term, match, and binding elaboration for `@[cbv]` blocks.

use super::*;

impl TermChecker<'_> {
    /// Term judgment: a source term of source type `A` becomes a computation of
    /// type `Ret [[A]]v`.
    ///
    /// A source form with no value clause goes through this judgment and is
    /// sequenced; one with a value clause is translated there and wrapped in
    /// `ret`. The forms with no value clause are application and instantiation,
    /// the eliminators `M.d`, `M/f` and `match`, the binding forms `let`,
    /// `let fix` and `let (X, x) = M in N`, and an ascription over any of them.
    ///
    /// This said *application is the only source term that is not a value*,
    /// which was true of the first version and of nothing since.
    pub(super) fn cbv_term<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, term: su::TermId,
        expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        self.cbv_sourced(
            tycker,
            term,
            |tycker| {
                let (compu, ty) = self.cbv_term_inner(tycker, scope, term, expected)?;
                self.cbv_meets_expectation(tycker, scope, ty, expected, true)?;
                Ok((compu, ty))
            },
            |_tycker, (compu, ty)| Some(TermAnnId::Compu(*compu, *ty)),
        )
    }

    // One arm per source form is the clearest shape for this judgment.
    #[allow(clippy::too_many_lines)]
    pub(super) fn cbv_term_inner<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, term: su::TermId,
        expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        use su::Term as Tm;
        match tycker.scoped.terms[&term].to_owned() {
            // [[M N]]v = do f <- [[M]]v; do a <- [[N]]v; ! f a
            | Tm::App(su::App(source_func, source_args)) => {
                self.cbv_app(tycker, scope, source_func, source_args, expected)
            }
            // An annotation, which desugaring inserts whether or not the source
            // wrote one: a binding's bindee is always wrapped, with a hole
            // where no classifier was authored. So handling this is structural
            // and not a convenience.
            //
            // A hole is transparent. An authored classifier is a *source* type,
            // so it goes through the type translation and is compared with what
            // the body synthesized.
            | Tm::Ann(su::Ann { tm, ty }) => {
                // An authored classifier *becomes* the expectation rather
                // than being compared with what the body synthesized. That is
                // the whole of the analysis direction: `(5 : Int64)` has to
                // reach the literal, because a literal's representation is
                // chosen when it is checked and cannot be corrected after.
                let inner = if matches!(tycker.scoped.terms[&ty], Tm::Hole(_)) {
                    expected
                } else {
                    let authored = self.cbv_type(tycker, scope, ty)?;
                    // A term's type is a value type, so an operator here is a
                    // kind error and is reported as one. Left to the net, it
                    // would become `Ret Opt` and fail as a type mismatch.
                    self.cbv_require_value_type(tycker, scope, authored)?;
                    if let Some(outer) = expected {
                        Lub::lub(outer, authored, tycker)?;
                    }
                    Some(authored)
                };
                self.cbv_term(tycker, scope, tm, inner)
            }
            // `@[doc]`, `@[debug]` and `@[format]`, which the host reads without
            // changing what the term means, are transparent here too: the body is
            // translated as though the annotation were absent, and `@[debug]`
            // records the host's observation of the *translation*, so what it
            // prints is the core term this part of the block became. Every other
            // annotation stays refused, by the catch-all below.
            | Tm::Meta(annotated)
                if matches!(
                    MetadataKind::of(&annotated.0),
                    Some(MetadataKind::Doc | MetadataKind::Debug | MetadataKind::Format)
                ) =>
            {
                let su::MetaT(meta, body) = *annotated;
                let (compu, ty) = self.cbv_term(tycker, scope, body, expected)?;
                if meta.is(MetadataKind::Debug.name()) {
                    tycker.observations.push(TyckObservation::Debug {
                        metadata: meta,
                        result: TermAnnId::Compu(compu, ty),
                    });
                }
                Ok((compu, ty))
            }
            // [[(M1, …, Mn)]]v_term = do a1 <- [[M1]]v; … ; ret (a1, …, an)
            //
            // The term clause for a tuple, which sequences its components left
            // to right. A tuple was the *first* source form to land in both
            // judgments, and this comment called it the only one until
            // 2026-10-04; a constructor, a named field and a pack all do now.
            // Which of the two applies is decided by `cbv_is_source_value`,
            // and that is what the value/term split is for: when every
            // component is a source value the tuple is one too, and the term
            // clause for a value is `ret [[V]]v`, with no administrative
            // binding at all.
            //
            // The two agree up to `do x <- ret V; M` being `M[V/x]`, so the
            // choice is which translation the clause *is*, not what it means.
            // Nothing in this fragment observes the difference, and a tuple of
            // values that sequences would still run correctly; the reason to
            // dispatch is that a later clause whose source form demands a value
            // can then reuse the value judgment on a tuple.
            | Tm::Cons(components) => {
                if components.iter().all(|component| self.cbv_is_source_value(tycker, *component)) {
                    self.cbv_value_as_term(tycker, scope, term, expected)
                } else if let Some((witnesses, body_ty, index)) =
                    self.cbv_pack_telescope(tycker, scope, &components, expected)?
                {
                    // [[(T, M)]]v_term = do a <- [[M]]v; ret ([[T]]v, a)
                    //
                    // A pack whose payload is a computation. The witnesses are
                    // types and are not sequenced; only the payload is, which
                    // is the whole difference from the value clause above.
                    self.cbv_pack_term(
                        tycker,
                        scope,
                        &components[index..],
                        witnesses,
                        body_ty,
                        expected.expect("a telescope was peeled from an expectation"),
                    )
                } else {
                    let wanted = self.cbv_expected_components(tycker, expected, components.len());
                    let mut translated =
                        std::collections::VecDeque::with_capacity(components.len());
                    for (component, wanted) in components.into_iter().zip(wanted) {
                        let (compu, ty) = self.cbv_term(tycker, scope, component, wanted)?;
                        let payload = Self::cbv_returned(tycker, ty);
                        translated.push_back((compu, payload));
                    }
                    self.cbv_sequence(tycker, scope, translated)
                }
            }
            // [[#f = V]]v_term = ret (#f = [[V]]v)
            | Tm::Named(_) if self.cbv_is_source_value(tycker, term) => {
                self.cbv_value_as_term(tycker, scope, term, expected)
            }
            // [[#f = M]]v_term = do a <- [[M]]v; ret (#f = a)
            //
            // A field whose payload is a computation is sequenced, and the
            // label is put back on the bound result.
            | Tm::Named(su::Named(name, inner)) => {
                let wanted = expected.and_then(|expected| {
                    match tycker.type_filled(&expected).ok().map(|ty| ty.to_owned()) {
                        | Some(ss::Type::Label(Label(found, payload))) if found == name => {
                            Some(payload)
                        }
                        | _ => None,
                    }
                });
                let (bindee, bindee_ty) = self.cbv_term(tycker, scope, inner, wanted)?;
                let payload_ty = Self::cbv_returned(tycker, bindee_ty);
                Self::cbv_bind_fresh(tycker, scope, "field", bindee, payload_ty, |tycker, def| {
                    let payload: ss::ValueId = def.build(tycker, &scope.env);
                    let vtype = ss::VType.build(tycker, &scope.env);
                    let ty = Alloc::alloc(tycker, ss::Label(name, payload_ty), vtype, &());
                    let named: ss::ValueId =
                        Alloc::alloc(tycker, ss::Named(name, payload), ty, &());
                    Ok(Return(named).build(tycker, &scope.env))
                })
            }
            // [[M/f]]v_term = do a <- [[M]]v; ret (a/f)
            //
            // A field projection, which synthesizes: the head type says what
            // the field holds, so nothing is pushed inward and the net checks
            // the result. The field is resolved by the host resolver rather
            // than looked up here, because a field may sit behind a product
            // prefix or a chain of them and the route to it is what
            // `ProjTarget` records; the resolver also owns the diagnostics for
            // a field that is missing or reachable by more than one route.
            | Tm::Proj(su::Proj(head, name)) => {
                let (bindee, bindee_ty) = self.cbv_term(tycker, scope, head, None)?;
                let head_ty = Self::cbv_returned(tycker, bindee_ty);
                let candidate =
                    FieldProjectionResolver::value_term(tycker, &scope.env, head_ty, &name)?;
                let target = FieldProjectionResolver::value_target(&candidate);
                let projected_ty = candidate.projected;
                Self::cbv_bind_fresh(tycker, scope, "record", bindee, head_ty, |tycker, def| {
                    let record: ss::ValueId = def.build(tycker, &scope.env);
                    let field = ss::ResolvedField { name, target };
                    let projected: ss::ValueId =
                        Alloc::alloc(tycker, ss::Proj(record, field), projected_ty, &());
                    Ok(Return(projected).build(tycker, &scope.env))
                })
            }
            // [[+C M]]v_term = do a <- [[M]]v; ret (+C a)
            //
            // A constructor of a source value is itself one, and its term
            // clause is `ret [[V]]v`; dispatching here keeps the value clause
            // reachable, as the same guard on the tuple rows does.
            | Tm::Ctor(_) if self.cbv_is_source_value(tycker, term) => {
                self.cbv_value_as_term(tycker, scope, term, expected)
            }
            | Tm::Ctor(su::Ctor(name, payload)) => {
                let Some(expected) = expected else { tycker.err(TyckError::MissingAnnotation)? };
                let (data_id, arm) = self.cbv_data_arm(tycker, scope, expected, &name)?;
                let (bindee, ty) = self.cbv_term(tycker, scope, payload, Some(arm))?;
                let arm = Self::cbv_returned(tycker, ty);
                Self::cbv_bind_fresh(tycker, scope, "payload", bindee, arm, |tycker, def| {
                    let payload = def.build(tycker, &scope.env);
                    let value = Alloc::alloc(
                        tycker,
                        ss::Value::Ctor(ss::Ctor(name, payload)),
                        expected,
                        &(),
                    );
                    let _ = tycker.statics.data_hints.upsert(value, data_id);
                    Ok(Return(value).build(tycker, &scope.env))
                })
            }
            // [[M.d]]v_term = do o <- [[M]]v; ! o .d
            //
            // An observation forces the thunk the translation of a codata value
            // put there, so the field's computation runs here and not where the
            // value was built.
            | Tm::Dtor(su::Dtor(body, dtor)) => {
                let (bindee, ty) = self.cbv_term(tycker, scope, body, None)?;
                let payload = Self::cbv_returned(tycker, ty);
                let (codata_id, fields, observed) = self.cbv_codata(tycker, scope, payload)?;
                let Some(field) = fields.get(&dtor) else {
                    tycker.err(TyckError::UnknownCoDataDestructor(dtor))?
                };
                Self::cbv_bind_fresh(tycker, scope, "observed", bindee, payload, |tycker, def| {
                    // `Force` is allocated rather than built: the recipe reads
                    // the thunk off the value's own type, and here that type is
                    // the *seal* the declaration gave it, whose `Thk` is one
                    // unrolling away. `cbv_codata` already did that unrolling,
                    // so the forced computation's type is what it returned.
                    let value: ss::ValueId = def.build(tycker, &scope.env);
                    let forced = Alloc::alloc(
                        tycker,
                        ss::Computation::Force(ss::Force(value)),
                        observed,
                        &(),
                    );
                    let _ = tycker.statics.codata_hints.upsert(forced, codata_id);
                    Ok(Alloc::alloc(
                        tycker,
                        ss::Computation::Dtor(ss::Dtor(forced, dtor)),
                        field,
                        &(),
                    ))
                })
            }
            // [[match M | +Ci pi => Ni end]]v = do s <- [[M]]v; match s | +Ci pi => [[Ni]]v end
            | Tm::Match(term) => self.cbv_match(tycker, scope, term, expected),
            // [[let x = M in N]]v = do x <- [[M]]v; [[N]]v
            //
            // Call-by-value's `let` evaluates its bindee and substitutes, which
            // is what `do` does. The binder is bound at the *translated* type,
            // so the tail's variables resolve the way the lambda's body's do.
            | Tm::Let(term) => {
                let su::Let { binder, bindee, tail } = *term;
                // A sealed definition, written `def`, that does not mention
                // itself; one that does arrives as a recursive group.
                if let Some(sealed) = bindee.syntactically_sealed(tycker) {
                    let binding = CbvSealedBinding { binder, sealed };
                    return self.cbv_sealed_bindings(tycker, scope, vec![binding], tail, expected);
                }
                // A recursive binding, written `let fix f (x : A) : B = M`.
                // Recognized before anything else a `let` can be, because its
                // bindee is a shape no other binding produces.
                if let Some(header) = self.cbv_fix_header(tycker, bindee) {
                    return self.cbv_fix_binding(tycker, scope, binder, header, tail, expected);
                }
                // A type binding, written `let X : K = A`. Decided before the
                // term path runs, because the two translate their bindee with
                // different judgments and the choice cannot be revisited.
                if let Some(declared) = self.cbv_declared_kind(tycker, scope, bindee)? {
                    let su::Term::Ann(su::Ann { tm: source, .. }) =
                        tycker.scoped.terms[&bindee].to_owned()
                    else {
                        unreachable!("a declared kind was read from an annotation")
                    };
                    let binding = CbvTypeBinding { binder, source, declared };
                    return self.cbv_type_binding(tycker, scope, binding, tail, expected);
                }
                // A kind binding, written `let K = VType`. A kind has no
                // classifier to declare, so the bindee itself decides, and it
                // can: no kind is a source term.
                if let Some(kind) = self.cbv_kind_bindee(tycker, scope, bindee)? {
                    return self.cbv_kind_binding(tycker, scope, binder, kind, tail, expected);
                }
                self.cbv_require_binding_pattern(tycker, binder)?;
                let (bindee, bindee_ty) = self.cbv_term(tycker, scope, bindee, None)?;
                let payload = Self::cbv_returned(tycker, bindee_ty);
                self.cbv_settle_pattern_annotations(tycker, scope, binder)?;
                let bound = TyEnvT::new(scope.env.clone(), binder)
                    .tyck(tycker, PatternAction::ana(payload.into()))?;
                let (pattern, _payload) = bound.inner.as_value();
                ValuePatternShape::require_binding(tycker, binder, pattern)?;
                let defs = self.cbv_pattern_bindings(tycker, pattern);
                // `[[let (X, x) = M in N]]term = [[M]]term to (X, x) . [[N]]term`
                //
                // An **unpack** is this same clause: the binder is a product
                // whose leading component binds a type, and the pattern
                // judgment reports the witnesses it opened. Nothing here
                // dispatches on the shape, because nothing needs to — the
                // bindee's type decides whether a `(X, x)` is a pair or a
                // package, exactly as it decides whether `(T, V)` is.
                let tail_scope = scope.binding(bound.info.clone(), defs);
                let (tail, tail_ty) = self.cbv_term(tycker, &tail_scope, tail, expected)?;
                // A witness opened by the binder may not outlive the binding.
                bound.close_scope(tycker, tail_ty)?;
                let compu =
                    Alloc::alloc(tycker, ss::Bind { binder: pattern, bindee, tail }, tail_ty, &());
                Ok((compu, tail_ty))
            }
            // Desugaring wraps a call-by-value block's payload in a `Block`,
            // which is what makes the call-by-value block a binding boundary:
            // mobile bindings are owned by the nearest `Block`, and the
            // candidate collector stops at one. So this arm is not an extra
            // feature — it is how the call-by-value block's own body is reached.
            //
            // Name resolution replaces a `Block`'s body with its elaborated
            // form, so what arrives here already has the call-by-value block's
            // own bindings woven in. One with no mobile bindings elaborates to
            // its payload.
            //
            // The inference region is the boundary's other half, and it has to
            // be opened here rather than inherited. The elaboration dispatches
            // on the `Block` node itself and so never reaches `check_block`,
            // which is where an ordinary block opens one; without this, a
            // flexible metavariable created inside a call-by-value block would be left for
            // an enclosing boundary to solve.
            | Tm::Block(su::Block(body)) => {
                let inference = InferenceRegion::enter(tycker);
                let translated = self.cbv_term(tycker, scope, body, expected)?;
                inference.close(tycker)?;
                Ok(translated)
            }
            // What a `that` binding leaves behind.
            //
            // Name resolution lifts the binding to the block that owns it and
            // replaces the binding node with a `Residual` marking where the
            // body continues, so the chain the arm above descends into ends in
            // one. It carries no source meaning of its own, which is why the
            // ordinary judgment forwards through it as well.
            | Tm::Residual(su::Residual(body)) => self.cbv_term(tycker, scope, body, expected),
            // A `that` binding whose bindee mentions a name the group binds
            // elaborates to a recursive group rather than to a `Let`.
            //
            // Translating one is not a variation on the `Let` clause: a group's
            // members are bound before any of them is checked, so a member can
            // mention itself, and that needs a judgment over the whole group.
            | Tm::RecGroup(group) => self.cbv_rec_group(tycker, scope, group, expected),
            | Tm::SourceBoundary(_) => self.cbv_reject_import(tycker),
            // [[V]]v_term = ret [[V]]v
            | _ => self.cbv_value_as_term(tycker, scope, term, expected),
        }
    }

    /// The term clause for a source value: `[[V]]v_term = ret [[V]]v`.
    ///
    /// The expectation passes straight through, because both judgments expect
    /// the same thing: `[[A]]v` for the source type `A`. That is why the term
    /// judgment's expectation is not the returner it produces.
    pub(super) fn cbv_value_as_term<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, term: su::TermId,
        expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        let (value, ty) = self.cbv_value(tycker, scope, term, expected)?;
        let compu = Return(value).build(tycker, &scope.env);
        let returner = cs::Ret(ty).build(tycker, &scope.env);
        Ok((compu, returner))
    }

    /// Whether a source term belongs to the value sublanguage
    ///
    /// ```text
    /// V ::= x | n | () | _ | fn p => M | Λ(X : K) . M
    ///     | (V1, …, Vn) | +C V | #f = V
    /// ```
    ///
    /// A term hole stands for a value of the type expected, so it is a value
    /// hole and its term translation is `ret _`; a hole inside a tuple is a
    /// component like any other value.
    ///
    /// **Three forms have to ask**, not one: a tuple, a constructor and a named
    /// field are values exactly when their payloads are, so each recurses. The
    /// rest are values in every position or in none, and the term judgment
    /// dispatches on the head alone.
    ///
    /// `comatch` is deliberately absent even though it has a value clause: its
    /// clauses hold source *terms*, so asking whether they are values would mean
    /// descending into bodies that the value judgment does not translate. A
    /// pack is absent for the same reason as its node — `(T, V)` is a `Cons`,
    /// and the tuple arm already answers for it.
    ///
    /// This listed four forms and said only a tuple recurses, which stopped
    /// being true when constructors, named fields and type abstractions landed.
    ///
    /// The forms admitted here are exactly the forms [`Self::cbv_value`] has
    /// a clause for, which is what keeps a `true` from turning into a
    /// rejection. An administrative wrapper — a block, a residual, an
    /// ascription — is deliberately not a value even when it wraps one, so it
    /// takes the sequencing clause, which is correct for any component.
    pub(super) fn cbv_is_source_value(&self, tycker: &Tycker<'_>, term: su::TermId) -> bool {
        use su::Term as Tm;
        match &tycker.scoped.terms[&term] {
            | Tm::Var(_) | Tm::Lit(_) | Tm::Triv(_) | Tm::Hole(_) | Tm::Abs(_) => true,
            | Tm::Ctor(su::Ctor(_, payload)) | Tm::Named(su::Named(_, payload)) => {
                self.cbv_is_source_value(tycker, *payload)
            }
            | Tm::Cons(components) => {
                components.iter().all(|component| self.cbv_is_source_value(tycker, *component))
            }
            | _ => false,
        }
    }

    /// Recognize a `let fix` header, exactly.
    ///
    /// Every part of the shape is required, so a bindee that merely happens to
    /// be a thunked fixed point is not mistaken for one.
    pub(super) fn cbv_fix_header(
        &self, tycker: &Tycker<'_>, bindee: su::TermId,
    ) -> Option<CbvFixHeader> {
        use su::Term as Tm;
        let Tm::Ann(su::Ann { tm, ty }) = tycker.scoped.terms[&bindee] else { return None };
        let Tm::Thunk(su::Thunk(fixed)) = tycker.scoped.terms[&tm] else { return None };
        let Tm::Fix(ref fixed) = tycker.scoped.terms[&fixed] else { return None };
        let su::Fix(recursive, abstraction) = *fixed;
        let Tm::Abs(su::Abs(parameters, body)) = &tycker.scoped.terms[&abstraction] else {
            return None;
        };
        let Tm::App(su::App(function, arguments)) = &tycker.scoped.terms[&ty] else { return None };
        let [telescope] = arguments[..] else { return None };
        if !matches!(tycker.scoped.terms[function], Tm::Internal(su::Internal::Thk)) {
            return None;
        }
        let Tm::Pi(su::Pi(signature_binders, codomain)) = &tycker.scoped.terms[&telescope] else {
            return None;
        };
        Some(CbvFixHeader {
            recursive,
            parameters: parameters.clone(),
            signature_binders: signature_binders.clone(),
            codomain: *codomain,
            body: *body,
        })
    }

    /// The recursion clause:
    /// `[[let fix f p1 … pn : B = M in N]]v = do f <- ret { fix g => fn p1' … pn' => [[M]]v }; [[N]]v`
    ///
    /// No coercion is needed anywhere in it: a function's type is already a
    /// thunk type and call-by-push-value's `fix` binds a thunk, so the source's
    /// recursive name maps straight onto the `fix` binder.
    ///
    /// The signature is settled **before** the body is read, and the body is
    /// checked at the declared codomain rather than at anything the
    /// surrounding expectation suggests. Recovering a recursive function's type
    /// from its own body is inference, and this judgment does not do inference.
    pub(super) fn cbv_fix_binding<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId, header: CbvFixHeader,
        tail: su::TermId, expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        let CbvFixHeader { recursive, parameters, signature_binders, codomain, body } = header;
        self.cbv_require_plain_binder(tycker, binder)?;
        self.cbv_require_plain_binder(tycker, recursive)?;
        // The `let !` spelling writes the parameters under the `fix`, so an
        // annotation can name the recursive binder, which is bound below.
        for parameter in &parameters {
            if let Some((_, domain)) = self.cbv_optional_binder_annotation(tycker, *parameter) {
                self.cbv_require_binder_unmentioned(tycker, recursive, &[], domain)?;
            }
        }
        self.cbv_require_same_first_sort(tycker, scope, &parameters, &signature_binders)?;
        let computation =
            self.cbv_function_computation(tycker, scope, &signature_binders, codomain)?;
        let signature = cs::Thk(computation).build(tycker, &scope.env);
        let (recursive, recursive_scope) =
            self.cbv_fix_recursive_name(tycker, scope, recursive, signature)?;
        let abstraction =
            self.cbv_abstraction(tycker, &recursive_scope, &parameters, body, Some(computation))?;
        let actual = tycker.statics.annotations_compu[&abstraction];
        Lub::lub(computation, actual, tycker)?;
        let fixed = Alloc::alloc(tycker, ss::Fix(recursive, abstraction), computation, &());
        self.cbv_fix_continue(tycker, scope, binder, fixed, tail, expected)
    }

    /// Whether the first parameter binds a type is asked of both its
    /// declarations. In a real header they are one freshened pattern; the
    /// `let !` spelling writes them apart. Only the first can be read here, since
    /// a later annotation may name an earlier parameter, which is not bound yet;
    /// the later ones are compared when the abstraction meets its signature.
    pub(super) fn cbv_require_same_first_sort<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, parameters: &[su::PatId],
        signature_binders: &[su::PatId],
    ) -> Result<()> {
        let domains = [parameters, signature_binders]
            .map(|binders| self.cbv_optional_binder_annotation(tycker, binders[0]));
        let [Some((_, parameter)), Some((_, declared))] = domains else { return Ok(()) };
        let parameter_kind = self.cbv_annotation_kind(tycker, scope, parameter)?;
        let declared_kind = self.cbv_annotation_kind(tycker, scope, declared)?;
        if parameter_kind.is_some() != declared_kind.is_some() {
            tycker.err(TyckError::SortMismatch)?
        }
        Ok(())
    }

    /// Bind a `let fix`'s recursive name at its signature, giving the scope
    /// its body is read in.
    ///
    /// The name stands at the signature, so a call in the body is an ordinary
    /// application or instantiation of a translated function, and those
    /// clauses handle it unchanged — including, under a quantifier, a call at
    /// another type, which is what makes the recursion polymorphic.
    pub(super) fn cbv_fix_recursive_name<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, recursive: su::PatId,
        signature: ss::TypeId,
    ) -> Result<(ss::VPatId, CbvScope)> {
        self.cbv_settle_pattern_annotations(tycker, scope, recursive)?;
        let bound = TyEnvT::new(scope.env.clone(), recursive)
            .tyck(tycker, PatternAction::ana(signature.into()))?;
        let (recursive, _) = bound.inner.as_value();
        let defs = self.cbv_pattern_bindings(tycker, recursive);
        Ok((recursive, scope.binding(bound.info.clone(), defs)))
    }

    /// `do f <- ret { fixed }; [[N]]v`: bind the fixed point under the
    /// binding's own name, at the thunk's type, which is the signature, and
    /// continue with the tail.
    pub(super) fn cbv_fix_continue<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId, fixed: ss::CompuId,
        tail: su::TermId, expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        let thunk = Thunk(fixed).build(tycker, &scope.env);
        let signature = tycker.statics.annotations_value[&thunk];
        let bindee = Return(thunk).build(tycker, &scope.env);
        self.cbv_settle_pattern_annotations(tycker, scope, binder)?;
        let bound = TyEnvT::new(scope.env.clone(), binder)
            .tyck(tycker, PatternAction::ana(signature.into()))?;
        let (binder, _) = bound.inner.as_value();
        let defs = self.cbv_pattern_bindings(tycker, binder);
        let tail_scope = scope.binding(bound.info.clone(), defs);
        let (tail, tail_ty) = self.cbv_term(tycker, &tail_scope, tail, expected)?;
        let compu = Alloc::alloc(tycker, ss::Bind { binder, bindee, tail }, tail_ty, &());
        Ok((compu, tail_ty))
    }

    /// Refuse a later binder or a body that names something `binder` binds.
    ///
    /// Desugaring puts the rest of a signature in the scope of each parameter,
    /// and a `fix` around its own parameter, so a name there resolves, but it
    /// is a term where a type belongs. It is refused before the type is read:
    /// the binder has not been checked yet, and reading the name would reach
    /// it ahead of its binder, which the checker refuses by panicking.
    pub(super) fn cbv_require_binder_unmentioned<'db>(
        &self, tycker: &mut Tycker<'db>, binder: su::PatId, later: &[su::PatId], body: su::TermId,
    ) -> Result<()> {
        let names = self.cbv_pattern_definitions(tycker, binder);
        let found = later
            .iter()
            .map(|later| su::EntityId::from(*later))
            .chain(std::iter::once(body.into()))
            .find_map(|entity| {
                if !tycker.source_contexts.mentions_any(entity, &names) {
                    return None;
                }
                match Traversal::new(tycker.scoped)
                    .run(entity, &mut CbvMentions { defs: &names })
                    .expect("resolved source syntax is acyclic")
                {
                    | ControlFlow::Break(CbvMention::Found(name)) => Some(name),
                    | _ => None,
                }
            });
        match found {
            | Some(name) => self.cbv_sourced(
                tycker,
                name,
                |tycker| tycker.err(TyckError::SortMismatch),
                |_tycker, ()| None,
            ),
            | None => Ok(()),
        }
    }

    /// The payload type a constructor carries, read off the data type the
    /// expectation names.
    ///
    /// The arms are already translated types: a data type a block can name is
    /// one it wrote, whose arms it translated, so each arm is `[[A]]v` for the
    /// source arm type `A`. That is why a payload can be translated directly
    /// against one.
    pub(super) fn cbv_data_arm<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, expected: ss::TypeId, name: &CtorName,
    ) -> Result<(ss::DataId, ss::TypeId)> {
        let unrolled = expected.unroll(tycker)?.subst_env(tycker, &scope.env)?;
        let ss::Type::Data(data_id) = tycker.type_filled(&unrolled)? else {
            tycker.err(TyckError::TypeExpected {
                expected: "data type definition".to_string(),
                found: unrolled,
            })?
        };
        match tycker.statics.datas[&data_id].get(name) {
            | Some(arm) => Ok((data_id, arm)),
            | None => tycker.err(TyckError::UnknownDataConstructor(*name)),
        }
    }

    /// The declaration behind `Thk (codata …)`, and its fields.
    ///
    /// A codata type is negative, so its translation keeps the shift outside;
    /// a block meets it as a thunk and has to look through that thunk before
    /// the declaration is visible.
    pub(super) fn cbv_codata<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, ty: ss::TypeId,
    ) -> Result<(ss::CoDataId, ss::CoData, ss::TypeId)> {
        let not_codata = || TyckError::TypeExpected {
            expected: "a thunked codata type definition".to_string(),
            found: ty,
        };
        // Unrolled before the thunk is destructured, not after: a declaration
        // made outside the block is sealed, so the `Thk` is inside the seal
        // and `destruct_thk_app` does not look through one.
        let outer = ty.unroll(tycker)?.subst_env(tycker, &scope.env)?;
        let Some(payload) = outer.destruct_thk_app(tycker) else { tycker.err(not_codata())? };
        let unrolled = payload.unroll(tycker)?.subst_env(tycker, &scope.env)?;
        let ss::Type::CoData(codata_id) = tycker.type_filled(&unrolled)? else {
            tycker.err(not_codata())?
        };
        let fields = tycker.statics.codatas[&codata_id].clone();
        Ok((codata_id, fields, payload))
    }

    /// The elimination clause:
    /// `[[match M | +Ci pi => Ni end]]v = do s <- [[M]]v; match s | +Ci pi => [[Ni]]v end`.
    ///
    /// Call-by-value evaluates the scrutinee before it can be examined, and in
    /// the core a positive type's elimination needs a *value* — so the clause
    /// is a `do` and then a match, which is the shape every positive
    /// elimination takes.
    ///
    /// The arms receive the expectation; an **empty** match has no arm to
    /// synthesize from and needs it, which is the second row that requires the
    /// direction rather than merely using it.
    pub(super) fn cbv_match<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope,
        term: su::Match<su::TermId, su::PatId, su::TermId>, expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        let su::Match { scrut, arms } = term;
        let (bindee, scrut_ty) = self.cbv_term(tycker, scope, scrut, None)?;
        let payload = Self::cbv_returned(tycker, scrut_ty);
        let (binder, def) = Self::cbv_fresh_binder(tycker, scope, "scrutinee", payload);
        let scrutinee: ss::ValueId = def.build(tycker, &scope.env);
        // The hint is what associates a bound scrutinee with its declaration,
        // as the ordinary judgment does; without it a later pass has to
        // rediscover which data type a match is over.
        let unrolled = payload.unroll(tycker)?.subst_env(tycker, &scope.env)?;
        if let ss::Type::Data(data_id) = tycker.type_filled(&unrolled)? {
            let _ = tycker.statics.data_hints.upsert(scrutinee, data_id);
        }
        let mut result = expected.map(|expected| cs::Ret(expected).build(tycker, &scope.env));
        let mut matchers = Vec::with_capacity(arms.len());
        for su::Matcher { binder: arm_binder, tail } in arms {
            self.cbv_require_arm_binder(tycker, arm_binder)?;
            self.cbv_settle_pattern_annotations(tycker, scope, arm_binder)?;
            let bound = TyEnvT::new(scope.env.clone(), arm_binder)
                .tyck(tycker, PatternAction::ana(unrolled.into()))?;
            let (arm_binder, _) = bound.inner.as_value();
            let defs = self.cbv_pattern_bindings(tycker, arm_binder);
            let arm_scope = scope.binding(bound.info.clone(), defs);
            let (arm_tail, arm_ty) = self.cbv_term(tycker, &arm_scope, tail, expected)?;
            // A witness opened by the arm's binder may not outlive the arm.
            bound.close_scope(tycker, arm_ty)?;
            result = Some(match result {
                | Some(previous) => Lub::lub(previous, arm_ty, tycker)?,
                | None => arm_ty,
            });
            matchers.push(ss::Matcher { binder: arm_binder, tail: arm_tail });
        }
        let Some(returner) = result else { tycker.err(TyckError::MissingAnnotation)? };
        let tail = Alloc::alloc(
            tycker,
            ss::Computation::Match(ss::Match { scrut: scrutinee, arms: matchers }),
            returner,
            &(),
        );
        let compu = Alloc::alloc(tycker, ss::Bind { binder, bindee, tail }, returner, &());
        Ok((compu, returner))
    }

    /// The binder shape a match arm may take: a constructor over a plain
    /// binder.
    ///
    /// Separate from [`Self::cbv_require_binding_pattern`], which governs a
    /// lambda's and a binding's binders and admits a constructor only where it
    /// cannot fail. An arm is where a refutable pattern belongs, and the one
    /// refutation the source grammar has is a constructor tag.
    pub(super) fn cbv_require_arm_binder<'db>(
        &self, tycker: &mut Tycker<'db>, binder: su::PatId,
    ) -> Result<()> {
        match tycker.scoped.pats[&binder].to_owned() {
            | su::Pattern::Ctor(su::Ctor(_, inner)) => self.cbv_require_plain_binder(tycker, inner),
            // An annotation leaves the arm's shape to what it annotates.
            | su::Pattern::Ann(su::Ann { tm, .. }) => self.cbv_require_arm_binder(tycker, tm),
            | _ => self.cbv_require_plain_binder(tycker, binder),
        }
    }

    /// The binder shape a `let`, a lambda and a `let fix` parameter may take:
    /// the plain shapes, and a constructor tag over a binder of this shape, as
    /// in `let +Box x = b`.
    ///
    /// Whether a tag can fail is not a question of shape. The core calls `+C p`
    /// irrefutable when `C`'s declaration has no other constructor and `p` is
    /// irrefutable, which is known only once the pattern has been checked
    /// against its type. So this admits the tag, and each clause that uses it
    /// asks the host's own question afterwards with
    /// [`ValuePatternShape::require_binding`], as the host's `do` and
    /// abstraction do; `let +A x = t` at a type with a second constructor is
    /// refused there, with `tyck.refutable-binding`. Without that call the
    /// block's hand-built `Bind` would carry a refutable binder the host never
    /// admits.
    pub(super) fn cbv_require_binding_pattern<'db>(
        &self, tycker: &mut Tycker<'db>, binder: su::PatId,
    ) -> Result<()> {
        match tycker.scoped.pats[&binder].to_owned() {
            | su::Pattern::Ctor(su::Ctor(_, inner)) => {
                self.cbv_require_binding_pattern(tycker, inner)
            }
            | su::Pattern::Cons(components) => components
                .into_iter()
                .try_for_each(|component| self.cbv_require_binding_pattern(tycker, component)),
            | su::Pattern::Named(su::Named(_, inner))
            | su::Pattern::Project(su::ProjectionPattern(_, inner))
            | su::Pattern::Ann(su::Ann { tm: inner, .. }) => {
                self.cbv_require_binding_pattern(tycker, inner)
            }
            | _ => self.cbv_require_plain_binder(tycker, binder),
        }
    }
}
