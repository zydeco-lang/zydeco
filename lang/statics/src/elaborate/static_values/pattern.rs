//! Bind static patterns and residualize runtime matching and views.

use super::*;

impl StaticElaborator<'_, '_> {
    pub(super) fn bind(
        &mut self, pattern: VPatId, value: StaticValue, env: &mut Environment,
        bindings: &mut Vec<Binding>,
    ) -> ResultKont<()> {
        match self.tycker.statics.vpats[&pattern].clone() {
            | ValuePattern::Hole(_) | ValuePattern::Triv(_) => {}
            | ValuePattern::Var(definition) => env.values += [(definition, value)],
            | ValuePattern::Named(Named(name, inner)) => {
                let payload = self.open_named(pattern, name, inner, value, env, bindings)?;
                self.bind(inner, payload, env, bindings)?;
            }
            | ValuePattern::Alias(Alias(patterns)) => {
                for pattern in patterns {
                    self.bind(pattern, value.clone(), env, bindings)?;
                }
            }
            | ValuePattern::VCons(patterns) => {
                let product = self.ty(self.tycker.statics.annotations_vpat[&pattern], env)?;
                for (position, pattern) in patterns.into_iter().enumerate() {
                    let field =
                        self.project(value.clone(), product, position, value.0.source, bindings)?;
                    self.bind(pattern, field, env, bindings)?;
                }
            }
            | ValuePattern::SCons(ConsN(prefix, tail)) => {
                let payload = self.open_package(pattern, prefix, tail, value, env, bindings)?;
                self.bind(tail, payload, env, bindings)?;
            }
            | ValuePattern::View(view) => {
                let viewed = self.view(view.function, value, env, bindings)?;
                self.bind(view.pattern, viewed, env, bindings)?;
            }
            | ValuePattern::Ctor(Ctor(name, tail)) => {
                let value = value.unnamed();
                if let ValueForm::Constructor(found, payload) = &value.0.form {
                    assert_eq!(&name, found, "an irrefutable constructor binding must match");
                    self.bind(tail, payload.clone(), env, bindings)?;
                } else {
                    let ty = self.ty(self.tycker.statics.annotations_vpat[&tail], env)?;
                    if self.purpose == Purpose::Inspect {
                        let payload = StaticValue::with_form(
                            value.0.source,
                            ty,
                            ValueForm::Runtime(value.0.source),
                        );
                        return self.bind(tail, payload, env, bindings);
                    }
                    let (payload_pattern, payload) = self.variable(Some(value.0.source), ty);
                    let binder = self.alloc_pattern(
                        pattern,
                        ValuePattern::Ctor(Ctor(name, payload_pattern)),
                        value.0.ty,
                    );
                    let bindee = self.reify(&value)?;
                    bindings.push(Binding { binder, bindee });
                    self.bind(tail, payload, env, bindings)?;
                }
            }
            | ValuePattern::Lit(_) => unreachable!("literal patterns require a match"),
        }
        Ok(())
    }

    fn view(
        &mut self, source: ValueId, value: StaticValue, env: &Environment,
        bindings: &mut Vec<Binding>,
    ) -> ResultKont<StaticValue> {
        let function = self.value(source, env, bindings)?.unnamed();
        let ValueForm::Function { binder: ValBinder::Value(parameter), body, env: closure } =
            &function.0.form
        else {
            return self.fail(StaticEliminationError::UnresolvedApplication { function: source });
        };
        self.enter_reduction(source)?;
        let result = (|| {
            let mut local = closure.clone();
            let value = self.share(value, bindings)?;
            self.bind(*parameter, value, &mut local, bindings)?;
            self.value(*body, &local, bindings)
        })();
        self.reduction_depth -= 1;
        result
    }

    fn open_named(
        &mut self, pattern: VPatId, name: FieldName, tail: VPatId, value: StaticValue,
        env: &Environment, bindings: &mut Vec<Binding>,
    ) -> ResultKont<StaticValue> {
        if let ValueForm::Named(_, payload) = &value.0.form {
            return Ok(payload.clone());
        }
        let ty = self.ty(self.tycker.statics.annotations_vpat[&tail], env)?;
        if self.purpose == Purpose::Inspect {
            return Ok(StaticValue::with_form(
                value.0.source,
                ty,
                ValueForm::Runtime(value.0.source),
            ));
        }
        let (payload_pattern, payload) = self.variable(Some(value.0.source), ty);
        let binder = self.alloc_pattern(
            pattern,
            ValuePattern::Named(Named(name, payload_pattern)),
            value.0.ty,
        );
        let bindee = self.reify(&value)?;
        bindings.push(Binding { binder, bindee });
        Ok(payload)
    }

    fn open_package(
        &mut self, pattern: VPatId, prefix: Vec<StaticPatId>, tail: VPatId, value: StaticValue,
        env: &mut Environment, bindings: &mut Vec<Binding>,
    ) -> ResultKont<StaticValue> {
        let value = value.unnamed();
        match &value.0.form {
            | ValueForm::Package(ConsN(witnesses, payload)) => {
                self.bind_witnesses(pattern, &prefix, witnesses, env)?;
                if self.purpose == Purpose::Inspect
                    && matches!(payload.0.form, ValueForm::Runtime(_))
                {
                    let ty = self.ty(self.tycker.statics.annotations_vpat[&tail], env)?;
                    return Ok(StaticValue::with_form(
                        payload.0.source,
                        ty,
                        payload.0.form.clone(),
                    ));
                }
                Ok(payload.clone())
            }
            | ValueForm::Runtime(_) => {
                let ty = self.ty(self.tycker.statics.annotations_vpat[&tail], env)?;
                if self.purpose == Purpose::Inspect {
                    return Ok(StaticValue::with_form(value.0.source, ty, value.0.form.clone()));
                }
                let (payload_pattern, payload) = self.variable(Some(value.0.source), ty);
                let binder = self.alloc_pattern(
                    pattern,
                    ValuePattern::SCons(ConsN(prefix, payload_pattern)),
                    value.0.ty,
                );
                let bindee = self.reify(&value)?;
                bindings.push(Binding { binder, bindee });
                Ok(payload)
            }
            | _ => unreachable!("package pattern has a package value"),
        }
    }

    pub(super) fn pattern_witness(&self, pattern: TPatId) -> Option<AbstId> {
        match self.tycker.statics.tpats[&pattern] {
            | TypePattern::Named(Named(_, tail)) => self.pattern_witness(tail),
            | TypePattern::Var(definition) => {
                let AnnId::Type(ty) = self.tycker.statics.annotations_var[&definition] else {
                    return None;
                };
                match self.tycker.statics.normalized_at(ty) {
                    | Some(Type::Abst(witness)) => Some(*witness),
                    | _ => None,
                }
            }
            | TypePattern::Hole(_) => None,
        }
    }

    fn bind_witnesses(
        &mut self, pattern: VPatId, prefix: &[StaticPatId], witnesses: &[StaticTermId],
        env: &mut Environment,
    ) -> ResultKont<()> {
        let mut domain = self.tycker.statics.annotations_vpat[&pattern].unroll_k(self.tycker)?;
        let mut assignments = Vec::new();
        for (pattern, argument) in prefix.iter().zip(witnesses.iter()) {
            match self.tycker.statics.normalized_at(domain).cloned() {
                | Some(Type::Exists(exists)) => {
                    let StaticTermId::Type(argument) = argument else { unreachable!() };
                    assignments.push((exists.binder.witness, *argument));
                    if let StaticPatId::Type(pattern) = pattern
                        && let Some(witness) = self.pattern_witness(*pattern)
                    {
                        assignments.push((witness, *argument));
                    }
                    domain = exists.body;
                }
                | Some(Type::ManifestKind(manifest)) => domain = manifest.body,
                | _ => unreachable!("package witness prefix follows its signature"),
            }
        }
        *env = env.with_types(assignments);
        Ok(())
    }

    pub(super) fn has_view(&self, pattern: VPatId) -> bool {
        match &self.tycker.statics.vpats[&pattern] {
            | ValuePattern::View(_) => true,
            | ValuePattern::Named(Named(_, inner))
            | ValuePattern::Ctor(Ctor(_, inner))
            | ValuePattern::SCons(ConsN(_, inner)) => self.has_view(*inner),
            | ValuePattern::Alias(Alias(patterns)) => {
                patterns.iter().any(|pattern| self.has_view(*pattern))
            }
            | ValuePattern::VCons(patterns) => {
                patterns.iter().any(|pattern| self.has_view(*pattern))
            }
            | _ => false,
        }
    }

    pub(super) fn runtime_pattern(
        &mut self, pattern: VPatId, env: &mut Environment,
    ) -> ResultKont<VPatId> {
        let ty = self.ty(self.tycker.statics.annotations_vpat[&pattern], env)?;
        let node = match self.tycker.statics.vpats[&pattern].clone() {
            | ValuePattern::Var(definition) => {
                let (pattern, value) = self.variable(None, ty);
                env.values += [(definition, value)];
                return Ok(pattern);
            }
            | ValuePattern::Named(Named(name, inner)) => {
                ValuePattern::Named(Named(name, self.runtime_pattern(inner, env)?))
            }
            | ValuePattern::Ctor(Ctor(name, inner)) => {
                ValuePattern::Ctor(Ctor(name, self.runtime_pattern(inner, env)?))
            }
            | ValuePattern::SCons(ConsN(prefix, inner)) => {
                ValuePattern::SCons(ConsN(prefix, self.runtime_pattern(inner, env)?))
            }
            | ValuePattern::VCons(patterns) => ValuePattern::VCons(
                patterns
                    .into_iter()
                    .map(|pattern| self.runtime_pattern(pattern, env))
                    .collect::<ResultKont<_>>()?,
            ),
            | ValuePattern::Alias(Alias(patterns)) => {
                let patterns = patterns
                    .into_iter()
                    .map(|pattern| self.runtime_pattern(pattern, env))
                    .collect::<ResultKont<Vec<_>>>()?;
                ValuePattern::Alias(Alias(ConsN::from_vec(patterns).unwrap()))
            }
            | ValuePattern::View(_) => unreachable!("views require static pattern elaboration"),
            | node => node,
        };
        Ok(self.alloc_pattern(pattern, node, ty))
    }

    pub(super) fn match_arms(
        &mut self, value: StaticValue, arms: &[Matcher<VPatId, CompuId>], env: &Environment,
        source: CompuId,
    ) -> ResultKont<CompuId> {
        let Some((first, rest)) = arms.split_first() else {
            let ty = self.ty(self.tycker.statics.annotations_compu[&source], env)?;
            let scrut = self.reify(&value)?;
            return Ok(self.alloc_compu(
                source,
                Computation::Match(Match { scrut, arms: Vec::new() }),
                ty,
            ));
        };
        self.match_steps(
            vec![(first.binder, value.clone())],
            first.tail,
            env.clone(),
            &MatchFailure { scrutinee: value, arms: rest, env: env.clone(), source },
        )
    }

    fn match_steps(
        &mut self, mut pending: Vec<(VPatId, StaticValue)>, tail: CompuId, mut env: Environment,
        failure: &MatchFailure<'_>,
    ) -> ResultKont<CompuId> {
        let mut bindings = Vec::new();
        while let Some((pattern, value)) = pending.pop() {
            match self.tycker.statics.vpats[&pattern].clone() {
                | ValuePattern::Alias(Alias(patterns)) => {
                    pending
                        .extend(patterns.into_iter().rev().map(|pattern| (pattern, value.clone())));
                }
                | ValuePattern::Named(Named(name, inner)) => {
                    let payload =
                        self.open_named(pattern, name, inner, value, &env, &mut bindings)?;
                    pending.push((inner, payload));
                }
                | ValuePattern::SCons(ConsN(prefix, inner)) => {
                    let payload =
                        self.open_package(pattern, prefix, inner, value, &mut env, &mut bindings)?;
                    pending.push((inner, payload));
                }
                | ValuePattern::VCons(patterns) => {
                    let product = self.ty(self.tycker.statics.annotations_vpat[&pattern], &env)?;
                    let values = patterns
                        .into_iter()
                        .enumerate()
                        .map(|(position, pattern)| {
                            Ok((
                                pattern,
                                self.project(
                                    value.clone(),
                                    product,
                                    position,
                                    value.0.source,
                                    &mut bindings,
                                )?,
                            ))
                        })
                        .collect::<ResultKont<Vec<_>>>()?;
                    pending.extend(values.into_iter().rev());
                }
                | ValuePattern::Ctor(Ctor(name, payload)) => {
                    if let ValueForm::Constructor(found, inner) = &value.0.form {
                        if &name != found {
                            let body = self.match_arms(
                                failure.scrutinee.clone(),
                                failure.arms,
                                &failure.env,
                                failure.source,
                            )?;
                            return Ok(self.compu_bindings(bindings, body));
                        }
                        pending.push((payload, inner.clone()));
                    } else {
                        let payload_ty =
                            self.ty(self.tycker.statics.annotations_vpat[&payload], &env)?;
                        let (payload_pattern, inner) =
                            self.variable(Some(value.0.source), payload_ty);
                        pending.push((payload, inner));
                        let success = self.match_steps(pending, tail, env, failure)?;
                        let fallback = self.match_arms(
                            failure.scrutinee.clone(),
                            failure.arms,
                            &failure.env,
                            failure.source,
                        )?;
                        let scrut = self.reify(&value)?;
                        let ty = self.tycker.statics.annotations_compu[&success];
                        let data = self.tycker.statics.data_pat_hints[&pattern];
                        let constructors = self.tycker.statics.datas[&data].clone();
                        // Several tags take the same failure continuation.
                        // Name it once so lowering does not duplicate the
                        // remaining pattern rows for every alternative tag.
                        let fallback = if constructors.iter().count() > 2 {
                            let thunk_ty = self.tycker.thk_arg(&TyEnv::default(), ty);
                            let thunk = self.alloc_value(
                                value.0.source,
                                Value::Thunk(Thunk(fallback)),
                                thunk_ty,
                            );
                            let (binder, continuation) = self.variable(None, thunk_ty);
                            bindings.push(Binding { binder, bindee: thunk });
                            let continuation = self.reify(&continuation)?;
                            self.alloc_compu(
                                failure.source,
                                Computation::Force(Force(continuation)),
                                ty,
                            )
                        } else {
                            fallback
                        };
                        let arms = constructors
                            .iter()
                            .map(|(candidate, payload_ty)| {
                                let (payload, tail) = if candidate == &name {
                                    (payload_pattern, success)
                                } else {
                                    (
                                        self.alloc_pattern(
                                            payload,
                                            ValuePattern::Hole(Hole),
                                            *payload_ty,
                                        ),
                                        fallback,
                                    )
                                };
                                let binder = self.alloc_pattern(
                                    pattern,
                                    ValuePattern::Ctor(Ctor(candidate.clone(), payload)),
                                    value.0.ty,
                                );
                                Matcher { binder, tail }
                            })
                            .collect();
                        let body = self.alloc_compu(
                            failure.source,
                            Computation::Match(Match { scrut, arms }),
                            ty,
                        );
                        return Ok(self.compu_bindings(bindings, body));
                    }
                }
                | ValuePattern::Lit(literal) => {
                    let scrut = self.reify(&value)?;
                    let success = self.match_steps(pending, tail, env, failure)?;
                    let fallback = self.match_arms(
                        failure.scrutinee.clone(),
                        failure.arms,
                        &failure.env,
                        failure.source,
                    )?;
                    let binder =
                        self.alloc_pattern(pattern, ValuePattern::Lit(literal), value.0.ty);
                    let hole = self.alloc_pattern(pattern, ValuePattern::Hole(Hole), value.0.ty);
                    let ty = self.tycker.statics.annotations_compu[&success];
                    let body = self.alloc_compu(
                        failure.source,
                        Computation::Match(Match {
                            scrut,
                            arms: vec![
                                Matcher { binder, tail: success },
                                Matcher { binder: hole, tail: fallback },
                            ],
                        }),
                        ty,
                    );
                    return Ok(self.compu_bindings(bindings, body));
                }
                | ValuePattern::View(view) => {
                    let viewed = self.view(view.function, value, &env, &mut bindings)?;
                    pending.push((view.pattern, viewed));
                }
                | _ => self.bind(pattern, value, &mut env, &mut bindings)?,
            }
        }
        let body = self.computation(tail, &env)?;
        Ok(self.compu_bindings(bindings, body))
    }
}
