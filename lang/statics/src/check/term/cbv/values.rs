//! Value, function, application, and pack elaboration for `@[cbv]` blocks.

use super::*;

impl TermChecker<'_> {
    /// Check a synthesized type against the expectation, if there was one.
    ///
    /// Pushing an expectation inward is what lets a clause that cannot
    /// synthesize work at all, but it is not what makes the expectation
    /// *binding*. A clause pushes only the part of it that its own shape
    /// explains — an arrow's domain into its binder, a product's components
    /// into a tuple — and a clause whose expectation does not have the shape it
    /// was looking for pushes nothing. So without this, `(fn (x : Int) => x) :
    /// Int` would be accepted: the abstraction would look for an arrow, find
    /// none, and synthesize unchecked.
    ///
    /// Both judgments expect `[[A]]v`, while the term judgment produces
    /// `Ret [[A]]v`, which is what `returner` selects between.
    pub(super) fn cbv_meets_expectation<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, synthesized: ss::TypeId,
        expected: Option<ss::TypeId>, returner: bool,
    ) -> Result<()> {
        let Some(expected) = expected else { return Ok(()) };
        let expected =
            if returner { cs::Ret(expected).build(tycker, &scope.env) } else { expected };
        Lub::lub(expected, synthesized, tycker)?;
        Ok(())
    }

    /// Peel an expected existential telescope, translating each leading
    /// component as a **source type**.
    ///
    /// `[[exists X . A]]v = exists X . [[A]]v` leaves the body already
    /// translated, so a witness substituted into it yields `[[A[T/X]]]v` and
    /// the payload can be checked against it directly. Returns the witnesses,
    /// the body type after substitution, and how many components the telescope
    /// consumed; `None` when the expectation is not an existential at all, in
    /// which case the ordinary product clause applies.
    ///
    /// Both judgments share this, because a witness is a type in either: only
    /// the payload differs, as a value in one and a sequenced computation in
    /// the other.
    pub(super) fn cbv_pack_telescope<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, components: &[su::TermId],
        expected: Option<ss::TypeId>,
    ) -> Result<Option<(Vec<ss::TypeId>, ss::TypeId, usize)>> {
        let Some(expected) = expected else { return Ok(None) };
        let mut witnesses = Vec::new();
        let mut body_ty = expected;
        let mut index = 0;
        loop {
            let filled = tycker.type_filled(&body_ty).ok().map(|ty| ty.to_owned());
            let Some(ss::Type::Exists(exists)) = filled else { break };
            let Some(&item) = components.get(index) else { break };
            let ss::Exists { binder, mode, body } = *exists;
            // The binder's kind is known before the witness is translated, as a
            // quantifier's is at an instantiation, so an operator witness may
            // leave its binder's kind unwritten here too.
            let domain = binder.domain_kind(tycker);
            let witness = self.cbv_type_against(tycker, scope, item, domain)?;
            let payload = binder.pattern.bind_argument(tycker, witness)?;
            let payload = match mode {
                | ss::ExistsMode::Abstract => payload,
                | ss::ExistsMode::Manifest(definition) => Lub::lub(definition, payload, tycker)?,
            };
            body_ty = body.subst_abst(tycker, (binder.witness, payload))?;
            witnesses.push(witness);
            index += 1;
        }
        if witnesses.is_empty() {
            return Ok(None);
        }
        if index >= components.len() {
            tycker.err(TyckError::Expressivity(
                "a call-by-value package needs a payload after its witnesses",
            ))?
        }
        Ok(Some((witnesses, body_ty, index)))
    }

    /// The payload components of a pack, as one value.
    ///
    /// A telescope may be followed by several components, which stand for the
    /// product the body is rather than for further witnesses, so this is the
    /// ordinary product clause restricted to the body expectation.
    ///
    /// **The payload is checked against the body, and the check cannot be left to
    /// the net.** Elsewhere a clause synthesizes a type and the net compares it
    /// with the expectation; the pack clause instead *stamps* the expected
    /// existential onto the value it builds, so the net would compare the
    /// expectation with itself and pass any payload, such as the function in
    /// `((Int, fn (x : Int) => x) : exists (X as Int) . X)`. The arena
    /// invariants defer their witness-prefix validation and would not catch it
    /// either. Excess components need the same check, because
    /// [`Self::cbv_expected_components`] degrades to no expectation when the
    /// arity disagrees rather than rejecting.
    pub(super) fn cbv_pack_payload<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, rest: &[su::TermId], body_ty: ss::TypeId,
    ) -> Result<ss::ValueId> {
        let (payload, payload_ty) = if let [only] = rest {
            self.cbv_value_at(tycker, scope, *only, Some(body_ty))?
        } else {
            let wanted = self.cbv_expected_components(tycker, Some(body_ty), rest.len());
            let mut values = Vec::with_capacity(rest.len());
            let mut types = Vec::with_capacity(rest.len());
            for (component, wanted) in rest.iter().zip(wanted) {
                let (value, ty) = self.cbv_value_at(tycker, scope, *component, wanted)?;
                values.push(value);
                types.push(ty);
            }
            let ty = Prod(types).build(tycker, &scope.env);
            (Alloc::alloc(tycker, ss::Value::VCons(values), ty, &()), ty)
        };
        self.cbv_meets_expectation(tycker, scope, payload_ty, Some(body_ty), false)?;
        Ok(payload)
    }

    /// A pack whose payload is a computation: `do a <- [[M]]v; ret (T, a)`.
    ///
    /// The `Bind` is allocated by hand because the pack it returns is built
    /// from a witness list of arbitrary length, which no recipe accepts: the
    /// one for `cs::Ann(Cons(cs::Ty(_), _), _)` carries a single witness.
    pub(super) fn cbv_pack_term<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, rest: &[su::TermId],
        witnesses: Vec<ss::TypeId>, body_ty: ss::TypeId, expected: ss::TypeId,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        // Several payload components stand for the body product, and the term
        // clause for a product is already the sequencing one, so they are
        // sequenced first and the result is bound as a single payload.
        let (bindee, bindee_ty) = if let [only] = rest {
            self.cbv_term(tycker, scope, *only, Some(body_ty))?
        } else {
            let wanted = self.cbv_expected_components(tycker, Some(body_ty), rest.len());
            let mut translated = std::collections::VecDeque::with_capacity(rest.len());
            for (component, wanted) in rest.iter().zip(wanted) {
                let (compu, ty) = self.cbv_term(tycker, scope, *component, wanted)?;
                let payload = Self::cbv_returned(tycker, ty);
                translated.push_back((compu, payload));
            }
            self.cbv_sequence(tycker, scope, translated)?
        };
        let payload_ty = Self::cbv_returned(tycker, bindee_ty);
        // As on the value side: the pack is labelled with the expectation, so the
        // net cannot see a payload that disagrees with the body. A single payload
        // is already checked by the term judgment's own net, but a sequenced
        // product is not -- `cbv_sequence` assembles whatever it was given.
        self.cbv_meets_expectation(tycker, scope, payload_ty, Some(body_ty), false)?;
        Self::cbv_bind_fresh(tycker, scope, "payload", bindee, payload_ty, |tycker, def| {
            let payload: ss::ValueId = def.build(tycker, &scope.env);
            let witnesses = witnesses.into_iter().map(ss::StaticTermId::from).collect::<Vec<_>>();
            let pack: ss::ValueId = Alloc::alloc(tycker, ConsN(witnesses, payload), expected, &());
            Ok(Return(pack).build(tycker, &scope.env))
        })
    }

    /// The components of an expected product, one per source component.
    ///
    /// An arity disagreement yields no expectations rather than an error, for
    /// the same reason as the arrow above: the net reports it once, against the
    /// product, instead of once per component.
    pub(super) fn cbv_expected_components(
        &self, tycker: &mut Tycker<'_>, expected: Option<ss::TypeId>, arity: usize,
    ) -> Vec<Option<ss::TypeId>> {
        let components = expected
            .and_then(|expected| tycker.type_filled(&expected).ok().map(|ty| ty.to_owned()))
            .and_then(|filled| match filled {
                | ss::Type::Prod(Prod(components)) => Some(components),
                | _ => None,
            })
            .filter(|components| components.len() == arity);
        match components {
            | Some(components) => components.into_iter().map(Some).collect(),
            | None => vec![None; arity],
        }
    }

    /// The type `A` that a translated term of type `Ret A` returns. The term
    /// judgment always produces a returner.
    pub(super) fn cbv_returned(tycker: &mut Tycker<'_>, returner: ss::TypeId) -> ss::TypeId {
        let Some(returned) = returner.destruct_ret_app(tycker) else {
            unreachable!("the term judgment always produces a returner")
        };
        returned
    }

    /// A fresh variable named `name` at type `ty`: the binder that introduces
    /// it and the definition it binds.
    pub(super) fn cbv_fresh_binder(
        tycker: &mut Tycker<'_>, scope: &CbvScope, name: &str, ty: ss::TypeId,
    ) -> (ss::VPatId, DefId) {
        let binder: ss::VPatId = cs::Pat(name, ty).build(tycker, &scope.env);
        let (def, _) = binder.try_destruct_def(tycker);
        (binder, def.expect("a named binder is a definition"))
    }

    /// `do x <- bindee; tail`, with `x` a fresh variable named `name` at
    /// `returned`, the type `bindee` returns. `tail` builds the rest from `x`,
    /// and the binding has the tail's type.
    pub(super) fn cbv_bind_fresh<'db>(
        tycker: &mut Tycker<'db>, scope: &CbvScope, name: &str, bindee: ss::CompuId,
        returned: ss::TypeId, tail: impl FnOnce(&mut Tycker<'db>, DefId) -> Result<ss::CompuId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        let (binder, def) = Self::cbv_fresh_binder(tycker, scope, name, returned);
        let tail = tail(tycker, def)?;
        let ty = tycker.statics.annotations_compu[&tail];
        let compu = Alloc::alloc(tycker, ss::Bind { binder, bindee, tail }, ty, &());
        Ok((compu, ty))
    }

    /// Bind a list of translated components left to right, then build the
    /// tuple of their results.
    ///
    /// The nesting of the `do`s *is* the evaluation order — the leftmost
    /// component's effects happen first — so the chain is built from its tail
    /// outward. It is built by iteration and not by recursion over the
    /// components, because one frame per component would make a flat tuple's
    /// *width* a stack limit, where every other depth limit in the checker is a
    /// limit on nesting.
    pub(super) fn cbv_sequence<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope,
        pending: std::collections::VecDeque<(ss::CompuId, ss::TypeId)>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        // One binder per component, allocated in source order. They are
        // distinct definitions whatever they are called, so the numbering is
        // about what a diagnostic or a dump reads back: one repeated name
        // looks like a capture bug and is not one.
        let mut binders = Vec::with_capacity(pending.len());
        let mut bound = Vec::with_capacity(pending.len());
        for (index, (bindee, payload)) in pending.into_iter().enumerate() {
            let (binder, def) =
                Self::cbv_fresh_binder(tycker, scope, &format!("component{index}"), payload);
            binders.push((binder, bindee));
            bound.push((def, payload));
        }
        let types = bound.iter().map(|(_, ty)| *ty).collect::<Vec<_>>();
        let mut values = Vec::with_capacity(bound.len());
        for (def, _) in &bound {
            values.push(def.build(tycker, &scope.env));
        }
        let ty = Prod(types).build(tycker, &scope.env);
        let value = Alloc::alloc(tycker, ss::Value::VCons(values), ty, &());
        let mut compu = Return(value).build(tycker, &scope.env);
        let returner = tycker.statics.annotations_compu[&compu];
        for (binder, bindee) in binders.into_iter().rev() {
            compu = Alloc::alloc(tycker, ss::Bind { binder, bindee, tail: compu }, returner, &());
        }
        Ok((compu, returner))
    }

    /// The application clause: `[[M N1 … Nn]]v`, one call for each function the
    /// arguments reach.
    ///
    /// The surrounding expectation describes the *result*, and turning it into
    /// an expectation for the function would mean inventing a domain to pair it
    /// with, so it is left to the net in `cbv_term`.
    pub(super) fn cbv_app<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, source_func: su::TermId,
        source_args: Vec<su::TermId>, expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        let _ = expected;
        let mut call = self.cbv_term(tycker, scope, source_func, None)?;
        let mut arguments = std::collections::VecDeque::from(source_args);
        while !arguments.is_empty() {
            let (func, func_ty) = call;
            call = self.cbv_call(tycker, scope, func, func_ty, &mut arguments)?;
        }
        Ok(call)
    }

    /// One call: `do f <- [[M]]v; do a1 <- [[N1]]v; …; ! f a1 … an`.
    ///
    /// The function's type says how many arguments the call takes, and what
    /// each one is: `Thk (p1 … pn . Ret B)` takes one for each `p`, a `forall`
    /// taking a source type and an arrow a source term. Nothing about an
    /// argument's own syntax decides it, since the host spells both as one
    /// application. A function's domain is pushed into its argument, which is
    /// why the function is read first: nothing about `+Succ` says which
    /// declaration it belongs to.
    ///
    /// Arguments are taken from the front of `arguments`, and any left over
    /// apply to what the call returns.
    pub(super) fn cbv_call<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, func: ss::CompuId, func_ty: ss::TypeId,
        arguments: &mut std::collections::VecDeque<su::TermId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        let not_a_function = TyckError::Expressivity(
            "only a call-by-value function can be applied inside a call-by-value block",
        );
        let Some(payload) = func_ty.destruct_ret_app(tycker) else { tycker.err(not_a_function)? };
        let Some(mut residual) = payload.destruct_thk_app(tycker) else {
            tycker.err(not_a_function)?
        };
        let (function_binder, function) = Self::cbv_fresh_binder(tycker, scope, "f", payload);
        let mut call: ss::CompuId = Force(function).build(tycker, &scope.env);
        let mut bindings = vec![(function_binder, func)];
        while residual.destruct_ret_app(tycker).is_none() {
            let Some(argument) = arguments.pop_front() else {
                tycker.err(TyckError::Expressivity(
                    "a call-by-value function must be applied to all of its parameters at once",
                ))?
            };
            if let Some((binder, _)) = residual.destruct_forall_binder(tycker) {
                // The recipe below reaches `unreachable!()` on a kind mismatch,
                // so the kinds are compared here rather than by it; the
                // binder's kind is also what an operator argument written
                // without its own binder kind takes it from.
                let domain = binder.domain_kind(tycker);
                let argument = self.cbv_type_against(tycker, scope, argument, domain)?;
                call = App(call, cs::Ty(argument)).build(tycker, &scope.env);
            } else {
                let Some((domain, _)) = residual.destruct_arrow(tycker) else {
                    tycker.err(not_a_function)?
                };
                let (bindee, bindee_ty) = self.cbv_term(tycker, scope, argument, Some(domain))?;
                let Some(argument_ty) = bindee_ty.destruct_ret_app(tycker) else {
                    tycker.err(not_a_function)?
                };
                Lub::lub(domain, argument_ty, tycker)?;
                let (binder, variable) = Self::cbv_fresh_binder(tycker, scope, "a", argument_ty);
                bindings.push((binder, bindee));
                call = App(call, variable).build(tycker, &scope.env);
            }
            residual = tycker.statics.annotations_compu[&call];
        }
        let compu = bindings.into_iter().rev().fold(call, |tail, (binder, bindee)| {
            Alloc::alloc(tycker, ss::Bind { binder, bindee, tail }, residual, &())
        });
        Ok((compu, residual))
    }

    /// Value judgment: a source value of source type `A` becomes a value of
    /// type `[[A]]v`.
    // One arm per source form is the clearest shape for this judgment.
    #[allow(clippy::too_many_lines)]
    pub(super) fn cbv_value<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, term: su::TermId,
        expected: Option<ss::TypeId>,
    ) -> Result<(ss::ValueId, ss::TypeId)> {
        use su::Term as Tm;
        match tycker.scoped.terms[&term].to_owned() {
            // [[fn (x : A) => M]]v = { fn (x : [[A]]v) => [[M]]v_term }
            // [[Λ(X : K) . M]]v = { fn (X : K) => [[M]]v_term }
            //
            // Written `fn (X : K) => M`, the same `fn` as a value lambda: the
            // binder's *sort* is what distinguishes them, which is how the host
            // reads `pi` as well. With the kind written the clause synthesizes,
            // so it needs no expectation; `{ fn X => ret 0 }` fails for want of
            // the kind, not of an expectation.
            | Tm::Abs(su::Abs(source_binders, source_body)) => {
                let wanted = expected.and_then(|expected| expected.destruct_thk_app(tycker));
                let abs =
                    self.cbv_abstraction(tycker, scope, &source_binders, source_body, wanted)?;
                let thunk = Thunk(abs).build(tycker, &scope.env);
                let ty = tycker.statics.annotations_value[&thunk];
                Ok((thunk, ty))
            }
            // [[#f = V]]v_value = #f = [[V]]v
            //
            // A field name is not a type former, so this row commutes: the
            // label moves through the translation untouched and only the
            // payload is translated. The expectation is peeled the same way,
            // when it is a label of the same name — a label of a *different*
            // name pushes nothing, and the net refuses the result, which keeps
            // the mismatch one diagnostic rather than two.
            | Tm::Named(su::Named(name, inner)) => {
                let wanted = expected.and_then(|expected| {
                    match tycker.type_filled(&expected).ok().map(|ty| ty.to_owned()) {
                        | Some(ss::Type::Label(Label(found, payload))) if found == name => {
                            Some(payload)
                        }
                        | _ => None,
                    }
                });
                let (value, value_ty) = self.cbv_value_at(tycker, scope, inner, wanted)?;
                let vtype = ss::VType.build(tycker, &scope.env);
                let ty = Alloc::alloc(tycker, ss::Label(name, value_ty), vtype, &());
                let value = Alloc::alloc(tycker, ss::Named(name, value), ty, &());
                Ok((value, ty))
            }
            // [[(V1, …, Vn)]]v_value = ([[V1]]v, …, [[Vn]]v)
            //
            // The value clause. A tuple whose components are all source values
            // is itself a source value, so no sequencing is needed. When a
            // component is not a value the term judgment's clause applies
            // instead, which is the first source form to land in both
            // judgments.
            | Tm::Cons(components) => {
                // [[(T, V)]]v_value = ([[T]]v, [[V]]v)
                //
                // A **pack**, which is this same node: the expected type is
                // what says a leading component is a type and not a value, so
                // the dispatch is on the expectation rather than on the syntax.
                // A pack cannot synthesize — nothing in `(Int, 7)` says which
                // existential it inhabits — so with no expectation this falls
                // through to the product clause and is refused there, by its
                // first component failing as a value.
                if let Some((witnesses, body_ty, index)) =
                    self.cbv_pack_telescope(tycker, scope, &components, expected)?
                {
                    let payload =
                        self.cbv_pack_payload(tycker, scope, &components[index..], body_ty)?;
                    let expected = expected.expect("a telescope was peeled from an expectation");
                    let witnesses =
                        witnesses.into_iter().map(ss::StaticTermId::from).collect::<Vec<_>>();
                    let value = Alloc::alloc(tycker, ConsN(witnesses, payload), expected, &());
                    return Ok((value, expected));
                }
                let wanted = self.cbv_expected_components(tycker, expected, components.len());
                let mut values = Vec::with_capacity(components.len());
                let mut types = Vec::with_capacity(components.len());
                for (component, wanted) in components.into_iter().zip(wanted) {
                    let (value, ty) = self.cbv_value_at(tycker, scope, component, wanted)?;
                    values.push(value);
                    types.push(ty);
                }
                let ty = Prod(types).build(tycker, &scope.env);
                let value = Alloc::alloc(tycker, ss::Value::VCons(values), ty, &());
                Ok((value, ty))
            }
            // [[+C V]]v = +C [[V]]v
            //
            // A constructor cannot synthesize: nothing about `+On` says which
            // declaration it belongs to, so the expectation is what names the
            // data type. This is the first row that *requires* the analysis
            // direction rather than merely using it.
            | Tm::Ctor(su::Ctor(name, payload)) => {
                let Some(expected) = expected else { tycker.err(TyckError::MissingAnnotation)? };
                let (data_id, arm) = self.cbv_data_arm(tycker, scope, expected, &name)?;
                let (payload, _) = self.cbv_value_at(tycker, scope, payload, Some(arm))?;
                let value =
                    Alloc::alloc(tycker, ss::Value::Ctor(ss::Ctor(name, payload)), expected, &());
                // The declaration a constructor belongs to, which the ordinary
                // judgment records and the backend reads to find the tag's
                // index. Without it the stack-IR lowering of a well-typed term
                // aborts, and neither the checker nor the arena invariants
                // notice.
                let _ = tycker.statics.data_hints.upsert(value, data_id);
                Ok((value, expected))
            }
            // [[comatch {.di => Mi}]]v = { comatch {.di => [[Mi]]v_term} }
            //
            // A comatch cannot synthesize, for the same reason a constructor
            // cannot: nothing in the arms says which declaration they answer.
            // The thunk is where the by-name corner comes from — each field is
            // a suspended computation, so it runs when observed and runs again
            // on the next observation.
            | Tm::CoMatchClauses(su::CoMatchClauses { clauses }) => {
                let Some(expected) = expected else { tycker.err(TyckError::MissingAnnotation)? };
                let (codata_id, fields, payload) = self.cbv_codata(tycker, scope, expected)?;
                let mut comatchers = Vec::with_capacity(clauses.len());
                for su::CoPatternClause { spine, tail } in clauses {
                    // One observation per clause. An **interleaved** spine,
                    // `.route n .result`, is the same clause written with its
                    // arguments pushed into the copattern; un-interleaving it
                    // into `.route => ret { fn n => … }` is mechanical and is
                    // deferred rather than guessed at here.
                    let su::CoPatternItem::Dtor(dtor) = spine.head else {
                        tycker.err(TyckError::Expressivity(
                            "a call-by-value comatch clause must begin with an observation",
                        ))?
                    };
                    if !spine.tail.is_empty() {
                        tycker.err(TyckError::Expressivity(
                            "an interleaved call-by-value copattern is not translated yet",
                        ))?
                    }
                    let Some(field) = fields.get(&dtor) else {
                        tycker.err(TyckError::UnknownCoDataDestructor(dtor))?
                    };
                    let Some(field) = field.destruct_ret_app(tycker) else {
                        tycker.err(TyckError::Expressivity(
                            "a call-by-value codata field must be a translated source type",
                        ))?
                    };
                    let (tail, _) = self.cbv_term(tycker, scope, tail, Some(field))?;
                    comatchers.push(ss::CoMatcher { dtor, tail });
                }
                let compu = Alloc::alloc(
                    tycker,
                    ss::Computation::CoMatch(ss::CoMatch { arms: comatchers }),
                    payload,
                    &(),
                );
                let _ = tycker.statics.codata_hints.upsert(compu, codata_id);
                // The thunk carries `Thk` of what the comatch carries, and the
                // clause reports exactly that rather than the expectation it
                // was given. For a `def` codata the comatch carries the seal
                // applied to its arguments, which `cbv_codata` found inside the
                // expectation's thunk, so the seal stays inside the thunk, where
                // the declaration's name puts it.
                let value = Thunk(compu).build(tycker, &scope.env);
                let ty = tycker.statics.annotations_value[&value];
                Ok((value, ty))
            }
            // [[x]]v = x, [[n]]v = n, [[()]]v = ()
            //
            // These rows are the identity, so ordinary synthesis computes them.
            // Reusing it is not a shortcut: it is what resolves a literal's
            // representation and checks its range, and what gives a
            // lambda-bound variable the translated type it was bound at.
            | Tm::Var(def) => {
                // The closed-term restriction, and the only place it bites a term.
                //
                // A variable the block did not bind resolves successfully to an
                // outer definition, and ordinary synthesis would hand back its
                // *host* type as though it were a translated one. Nothing
                // downstream would notice, which is why this is checked here and
                // not left to a later pass.
                //
                // This is the rule for term position. A type or kind name is
                // closed over by its twin, `cbv_require_block_name`, which
                // the leaf rule and the kind judgment run.
                if !scope.bound.contains(&def) {
                    // A name denoting a type reaches here when a binding meant
                    // to be a type binding did not declare its kind, as in
                    // `let MyInt = Int that …`: with no kind the binding is a
                    // term binding, and `Int` is then a term-position use of a
                    // type, bound in the block or outside it. The rejection
                    // is right either way, and resolving a bare name is a leaf
                    // lookup that reaches nothing, so it is worth doing to say
                    // which of the two the author ran into.
                    let checked =
                        TyEnvT::new(scope.env.clone(), term).tyck(tycker, Action::syn())?;
                    let message = match checked {
                        | TermAnnId::Type(_, _) | TermAnnId::Kind(_) => {
                            "a call-by-value block may not use a type as a term; a type binding is written `let X : VType = A`"
                        }
                        | _ => "a call-by-value block may not refer to a variable bound outside it",
                    };
                    tycker.err(TyckError::Expressivity(message))?
                }
                self.cbv_reuse_check(tycker, scope, term, expected)
            }
            | Tm::Lit(_) | Tm::Triv(_) => self.cbv_reuse_check(tycker, scope, term, expected),
            // A term hole, `_`, is what the host makes of one: a typed hole,
            // reported with the type it stands at when the program is checked and
            // refused when it is run. That type is the translation of the source
            // type expected, so a hole where `Int -> Int` is expected reports
            // `Thk (Int -> Ret Int)`. Analysis only: with nothing expected the
            // report would be a fresh unknown, which says nothing, so a hole needs
            // an expectation as a constructor does. It is a leaf, so ordinary
            // checking reaches nothing outside the block.
            | Tm::Hole(_) => match expected {
                | Some(_) => self.cbv_reuse_check(tycker, scope, term, expected),
                | None => tycker.err(TyckError::MissingAnnotation),
            },
            | Tm::SourceBoundary(_) => self.cbv_reject_import(tycker),
            | _ => tycker.err(TyckError::Expressivity(
                "this form is not supported inside a call-by-value block yet",
            )),
        }
    }

    /// The value judgment at a component's own source site.
    ///
    /// The term judgment has already wrapped the site it dispatched on, so the
    /// fallthrough into the value judgment must not wrap it again; a tuple's
    /// components are different sites, and each one needs the facts
    /// [`Self::cbv_sourced`] records or the nodes emitted for it derive their
    /// identifiers from the tuple and the component gets no back mapping.
    ///
    /// A component that ordinary synthesis computes — a variable, a literal,
    /// `()` — is therefore entered twice, once here and once by `tyck`. Both
    /// records are the same pair, since provenance is keyed by the node that
    /// was emitted, and the nested allocator scope is deterministic; the cost
    /// is one redundant frame, against a missing fact for every lambda written
    /// inside a tuple of values.
    pub(super) fn cbv_value_at<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, term: su::TermId,
        expected: Option<ss::TypeId>,
    ) -> Result<(ss::ValueId, ss::TypeId)> {
        self.cbv_sourced(
            tycker,
            term,
            |tycker| {
                let (value, ty) = self.cbv_value(tycker, scope, term, expected)?;
                self.cbv_meets_expectation(tycker, scope, ty, expected, false)?;
                Ok((value, ty))
            },
            |_tycker, (value, ty)| Some(TermAnnId::Value(*value, *ty)),
        )
    }

    /// `[[pi p1 … pn . B]]v = Thk (p1' … pn' . Ret [[B]]v)`: the computation type
    /// inside the thunk.
    ///
    /// A telescope is one function, so its binders sit together with a single
    /// returner after the last. A binder that declares a kind is a quantifier
    /// and a binder that declares a type is an arrow, which is how the host
    /// reads `pi` as well; `A -> B` and `forall (X : K) . A` are the telescopes
    /// of one binder. A value binder may name its parameter, as a function
    /// header's signature does, but a source type never mentions a term, so a
    /// later binder or the result that mentions the name is refused by sort and
    /// an unmentioned name denotes what `_` does. Below its outermost
    /// annotation the binder is checked as a parameter is, so each annotation
    /// inside it is translated and compared with the domain, though nothing it
    /// names enters scope.
    pub(super) fn cbv_function_computation<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binders: &[su::PatId], body: su::TermId,
    ) -> Result<ss::TypeId> {
        let Some((binder, later)) = binders.split_first() else {
            let result = self.cbv_type(tycker, scope, body)?;
            self.cbv_require_value_type(tycker, scope, result)?;
            return Ok(cs::Ret(result).build(tycker, &scope.env));
        };
        let binder = *binder;
        if self.cbv_type_binder_kind(tycker, scope, binder)?.is_some() {
            let opened = self.cbv_open_type_binder(
                tycker,
                scope,
                binder,
                None,
                CbvTypeBinderSite::Abstraction,
            )?;
            let inner = self.cbv_function_computation(tycker, &opened.scope, later, body)?;
            let ctype = ss::CType.build(tycker, &scope.env);
            let binder = ss::TypeBinder { pattern: opened.pattern, witness: opened.witness };
            return Ok(Alloc::alloc(tycker, ss::Forall(binder, inner), ctype, &()));
        }
        let (bare, source_domain) = self.cbv_annotated_binder(tycker, binder)?;
        self.cbv_require_binder_unmentioned(tycker, binder, later, body)?;
        let domain = self.cbv_type(tycker, scope, source_domain)?;
        self.cbv_require_value_type(tycker, scope, domain)?;
        self.cbv_require_binding_pattern(tycker, bare)?;
        self.cbv_settle_pattern_annotations(tycker, scope, bare)?;
        let checked =
            TyEnvT::new(scope.env.clone(), bare).tyck(tycker, PatternAction::ana(domain.into()))?;
        let (pattern, _domain) = checked.inner.as_value();
        ValuePatternShape::require_binding(tycker, bare, pattern)?;
        let inner = self.cbv_function_computation(tycker, scope, later, body)?;
        Ok(Arrow(domain, inner).build(tycker, &scope.env))
    }

    /// `fn p1 … pn => M` as the computation inside its thunk, against the
    /// computation type it is expected at, if any.
    ///
    /// A binder that declares a kind is a type parameter, and so is one that
    /// declares nothing where a quantifier is expected; any other is a value
    /// parameter, which takes its type from an annotation or the expected arrow.
    ///
    /// The group and its expected type take their parameters together, so when
    /// one of them runs out first the two disagree on arity. That is reported
    /// where it happens, as a call with too few arguments is, and names the
    /// side with more parameters; otherwise an unannotated binder past the end
    /// of the expected type would be blamed for having no type to take.
    pub(super) fn cbv_abstraction<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binders: &[su::PatId], body: su::TermId,
        wanted: Option<ss::TypeId>,
    ) -> Result<ss::CompuId> {
        let Some((binder, later)) = binders.split_first() else {
            let takes_more = wanted.is_some_and(|wanted| {
                wanted.destruct_arrow(tycker).is_some()
                    || wanted.destruct_forall_binder(tycker).is_some()
            });
            if takes_more {
                tycker.err(TyckError::Expressivity(
                    "the expected type takes more parameters at once than this call-by-value function; group them, as in `fn x y => e`",
                ))?
            }
            let wanted = wanted.and_then(|wanted| wanted.destruct_ret_app(tycker));
            let (body, _returner) = self.cbv_term(tycker, scope, body, wanted)?;
            return Ok(body);
        };
        if wanted.is_some_and(|wanted| wanted.destruct_ret_app(tycker).is_some()) {
            tycker.err(TyckError::Expressivity(
                "the expected type takes fewer parameters at once than this call-by-value function; nest the rest, as in `fn x => fn y => e`",
            ))?
        }
        let binder = *binder;
        let quantifier = wanted.and_then(|wanted| wanted.destruct_forall_binder(tycker));
        let undeclared = self.cbv_optional_binder_annotation(tycker, binder).is_none();
        if self.cbv_type_binder_kind(tycker, scope, binder)?.is_some()
            || (undeclared && quantifier.is_some())
        {
            return self.cbv_type_parameter(tycker, scope, binder, later, body, quantifier);
        }
        self.cbv_value_parameter(tycker, scope, binder, later, body, wanted)
    }

    /// A type parameter. The skolem is allocated here rather than by the recipe,
    /// because what follows has to be translated *under* it: its occurrences of
    /// the source name resolve to the skolem through the environment.
    pub(super) fn cbv_type_parameter<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId, later: &[su::PatId],
        body: su::TermId, quantifier: Option<(ss::TypeBinder, ss::TypeId)>,
    ) -> Result<ss::CompuId> {
        let expected_kind = quantifier.as_ref().map(|(binder, _)| binder.domain_kind(tycker));
        let opened = self.cbv_open_type_binder(
            tycker,
            scope,
            binder,
            expected_kind,
            CbvTypeBinderSite::Abstraction,
        )?;
        let witness = opened.witness;
        let wanted = match quantifier {
            | Some((quantifier, quantified)) => {
                Lub::lub(quantifier.domain_kind(tycker), opened.kind, tycker)?;
                let argument: ss::TypeId = Alloc::alloc(tycker, witness, opened.kind, &());
                let payload = quantifier.pattern.bind_argument(tycker, argument)?;
                Some(quantified.subst_abst(tycker, (quantifier.witness, payload))?)
            }
            | None => None,
        };
        let inner = self.cbv_abstraction(tycker, &opened.scope, later, body, wanted)?;
        Ok(Abs(cs::Ty(witness), move |_: Option<DefId>, _: ss::AbstId| inner)
            .build(tycker, &scope.env))
    }

    /// A value parameter, whose type comes from its outermost annotation, from
    /// the expected arrow, or from the annotations inside it when they give the
    /// type of every part. Otherwise there is nothing to take it from: a part
    /// with no annotation would need inference, which this judgment does not do.
    pub(super) fn cbv_value_parameter<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, source_binder: su::PatId,
        later: &[su::PatId], body: su::TermId, wanted: Option<ss::TypeId>,
    ) -> Result<ss::CompuId> {
        const NEEDS_TYPE: &str = "a call-by-value parameter needs an explicit type annotation, or an expected type to take one from";
        let (wanted_domain, wanted_rest) =
            match wanted.and_then(|wanted| wanted.destruct_arrow(tycker)) {
                | Some((domain, rest)) => (Some(domain), Some(rest)),
                | None => (None, None),
            };
        let (bare_binder, domain) = match self.cbv_optional_binder_annotation(tycker, source_binder)
        {
            // `(x : _)` asks for the domain the way `x` does.
            | Some((bare, source_domain))
                if matches!(tycker.scoped.terms[&source_domain], su::Term::Hole(_)) =>
            {
                (bare, wanted_domain)
            }
            | Some((bare, source_domain)) => {
                let domain = self.cbv_type(tycker, scope, source_domain)?;
                // Required here because a binder need not check it: a name
                // compares its kind with the domain's, and a wildcard binds
                // nothing and compares nothing.
                self.cbv_require_value_type(tycker, scope, domain)?;
                if let Some(wanted) = wanted_domain {
                    Lub::lub(wanted, domain, tycker)?;
                }
                (bare, Some(domain))
            }
            | None => (source_binder, wanted_domain),
        };
        if domain.is_none() && !Self::cbv_binder_determines_its_type(tycker, bare_binder) {
            tycker.err(TyckError::Expressivity(NEEDS_TYPE))?
        }
        self.cbv_require_binding_pattern(tycker, bare_binder)?;
        self.cbv_settle_pattern_annotations(tycker, scope, bare_binder)?;
        // With no domain the binder gives its own, by the host's synthesis over
        // the annotations just settled, each of them a translated value type.
        let action = match domain {
            | Some(domain) => PatternAction::ana(domain.into()),
            | None => PatternAction::syn(),
        };
        let binder = TyEnvT::new(scope.env.clone(), bare_binder).tyck(tycker, action)?;
        let (pattern, _domain) = binder.inner.as_value();
        ValuePatternShape::require_binding(tycker, bare_binder, pattern)?;
        let defs = self.cbv_pattern_bindings(tycker, pattern);
        let body_scope = scope.binding(binder.info.clone(), defs);
        let inner = self.cbv_abstraction(tycker, &body_scope, later, body, wanted_rest)?;
        // A witness this binder opened may not outlive it, exactly as in the
        // `let` and `match` clauses.
        let result = tycker.statics.annotations_compu[&inner];
        binder.close_scope(tycker, result)?;
        Ok(Abs(pattern, |_: ss::VPatId| inner).build(tycker, &scope.env))
    }

    /// Whether a binder's own annotations give its type: every part of it is
    /// annotated, or is `()`, or is a product or a named field of such parts.
    ///
    /// This is where the host's pattern synthesis finds a type without
    /// inference. It also gives a bare variable a stand-in for inference to
    /// solve, which a block does not do, and it finds none for a wildcard, a
    /// constructor tag or a field projection.
    pub(super) fn cbv_binder_determines_its_type(tycker: &Tycker<'_>, binder: su::PatId) -> bool {
        match &tycker.scoped.pats[&binder] {
            | su::Pattern::Ann(_) | su::Pattern::Triv(_) => true,
            | su::Pattern::Cons(components) => components
                .iter()
                .all(|component| Self::cbv_binder_determines_its_type(tycker, *component)),
            | su::Pattern::Named(su::Named(_, inner)) => {
                Self::cbv_binder_determines_its_type(tycker, *inner)
            }
            | _ => false,
        }
    }
}
