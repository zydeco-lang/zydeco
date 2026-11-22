//! Bind static patterns and residualize runtime matching and views.

use super::*;

impl StaticElaborator<'_, '_> {
    /// Select a value arm without entering computations or guessing an unknown tag.
    pub(super) fn value_pattern(
        &mut self, source: ValueId, pattern: VPatId, value: StaticValue, env: &mut Environment,
    ) -> Result<bool> {
        zydeco_foundation::stack::maybe_grow(|| {
            self.value_pattern_inner(source, pattern, value, env)
        })
    }

    fn value_pattern_inner(
        &mut self, source: ValueId, pattern: VPatId, value: StaticValue, env: &mut Environment,
    ) -> Result<bool> {
        match self.tycker.statics.vpats[&pattern].clone() {
            | ValuePattern::Ctor(Ctor(name, tail)) => {
                let value = value.unnamed();
                match &value.0.form {
                    | ValueForm::Constructor { name: found, payload, .. } if found == &name => {
                        self.value_pattern(source, tail, payload.clone(), env)
                    }
                    | ValueForm::Constructor { .. } => Ok(false),
                    | _ => self.fail(StaticEliminationError::UnresolvedMatch {
                        value: self.application_site.unwrap_or(source),
                    }),
                }
            }
            | ValuePattern::Lit(literal) => {
                let value = value.unnamed();
                if let ValueForm::Lit(found) = &value.0.form {
                    Ok(found == &literal)
                } else {
                    self.fail(StaticEliminationError::UnresolvedMatch {
                        value: self.application_site.unwrap_or(source),
                    })
                }
            }
            | ValuePattern::VCons(patterns) => {
                let product = self.ty(self.tycker.statics.annotations_vpat[&pattern], env)?;
                for (position, pattern) in patterns.into_iter().enumerate() {
                    let field = self.project(value.clone(), product, position, value.0.source)?;
                    if !self.value_pattern(source, pattern, field, env)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            | ValuePattern::Alias(Alias(patterns)) => {
                for pattern in patterns {
                    if !self.value_pattern(source, pattern, value.clone(), env)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            | ValuePattern::Named(Named(name, inner)) => {
                let payload = self.open_named(pattern, name, inner, value, env)?;
                self.value_pattern(source, inner, payload, env)
            }
            | ValuePattern::SCons(ConsN(prefix, inner)) => {
                let payload = self.open_package(pattern, prefix, inner, value, env)?;
                self.value_pattern(source, inner, payload, env)
            }
            | ValuePattern::View(view) => {
                let viewed = self.view(view.function, value, env)?;
                self.value_pattern(source, view.pattern, viewed, env)
            }
            | _ => {
                self.bind(pattern, value, env)?;
                Ok(true)
            }
        }
    }

    pub(super) fn bind(
        &mut self, pattern: VPatId, value: StaticValue, env: &mut Environment,
    ) -> Result<()> {
        zydeco_foundation::stack::maybe_grow(|| self.bind_inner(pattern, value, env))
    }

    fn bind_inner(
        &mut self, pattern: VPatId, value: StaticValue, env: &mut Environment,
    ) -> Result<()> {
        match self.tycker.statics.vpats[&pattern].clone() {
            | ValuePattern::Hole(_) | ValuePattern::Triv(_) => {}
            | ValuePattern::Var(definition) => env.values += [(definition, value)],
            | ValuePattern::Named(Named(name, inner)) => {
                let payload = self.open_named(pattern, name, inner, value, env)?;
                self.bind(inner, payload, env)?;
            }
            | ValuePattern::Alias(Alias(patterns)) => {
                for pattern in patterns {
                    self.bind(pattern, value.clone(), env)?;
                }
            }
            | ValuePattern::VCons(patterns) => {
                let product = self.ty(self.tycker.statics.annotations_vpat[&pattern], env)?;
                for (position, pattern) in patterns.into_iter().enumerate() {
                    let field = self.project(value.clone(), product, position, value.0.source)?;
                    self.bind(pattern, field, env)?;
                }
            }
            | ValuePattern::SCons(ConsN(prefix, tail)) => {
                let payload = self.open_package(pattern, prefix, tail, value, env)?;
                self.bind(tail, payload, env)?;
            }
            | ValuePattern::View(view) => {
                let viewed = self.view(view.function, value, env)?;
                self.bind(view.pattern, viewed, env)?;
            }
            | ValuePattern::Ctor(Ctor(name, tail)) => {
                let value = value.unnamed();
                if let ValueForm::Constructor { name: found, payload, .. } = &value.0.form {
                    assert_eq!(&name, found, "an irrefutable constructor binding must match");
                    self.bind(tail, payload.clone(), env)?;
                } else {
                    let ty = self.ty(self.tycker.statics.annotations_vpat[&tail], env)?;
                    if self.purpose == Purpose::Inspect {
                        let payload = self.make_value(
                            value.0.source,
                            ty,
                            0,
                            ValueForm::RuntimeTerm(Rc::new(Cell::new(None))),
                        );
                        return self.bind(tail, payload, env);
                    }
                    let scope = self.current_scope();
                    let (payload_pattern, payload) = self.variable(None, ty, scope);
                    let source_data = self.tycker.statics.data_pat_hints[&pattern];
                    let (ir_data, tag) = self.ctor_idx(source_data, name);
                    let binder = self.alloc_core_vpat(
                        self.pat_span(pattern),
                        ir::ValuePattern::Ctor { data: ir_data, tag, payload: payload_pattern },
                        value.0.ty,
                    );
                    let span = self.term_span(value.0.source);
                    let bindee = self.reify(&value)?;
                    self.push_binding(CoreBinding { binder, bindee, span });
                    self.bind(tail, payload, env)?;
                }
            }
            | ValuePattern::Lit(_) => unreachable!("literal patterns require a match"),
        }
        Ok(())
    }

    fn view(
        &mut self, source: ValueId, value: StaticValue, env: &Environment,
    ) -> Result<StaticValue> {
        let function = self.value(source, env)?.unnamed();
        let ValueForm::Function { binder: ValBinder::Value(parameter), body, env: closure } =
            &function.0.form
        else {
            return self.fail(StaticEliminationError::UnresolvedApplication { function: source });
        };
        let parameter = *parameter;
        let body = *body;
        let mut local = closure.clone();
        let value = self.share(value)?;
        self.enter_reduction(source)?;
        let result = (|| {
            self.bind(parameter, value, &mut local)?;
            self.value(body, &local)
        })();
        self.reduction_depth -= 1;
        result
    }

    fn open_named(
        &mut self, _pattern: VPatId, _name: FieldName, tail: VPatId, value: StaticValue,
        env: &Environment,
    ) -> Result<StaticValue> {
        if let ValueForm::Named(_, payload) = &value.0.form {
            return Ok(payload.clone());
        }
        let ty = self.ty(self.tycker.statics.annotations_vpat[&tail], env)?;
        Ok(StaticValue(Rc::new(ValueInfo {
            uid: value.0.uid,
            source: value.0.source,
            ty,
            max_scope: value.0.max_scope,
            form: value.0.form.clone(),
            shared: value.0.shared,
        })))
    }

    fn open_package(
        &mut self, pattern: VPatId, prefix: Vec<StaticPatId>, tail: VPatId, value: StaticValue,
        env: &mut Environment,
    ) -> Result<StaticValue> {
        let mut value = value.unnamed();
        let mut witnesses = Vec::with_capacity(prefix.len());
        // A projection pattern can open a whole telescope even when `pack`
        // introduced its entries as nested singleton packages. Consume exactly
        // the pattern's prefix, retaining any unopened package structure.
        while witnesses.len() < prefix.len() {
            match &value.0.form {
                | ValueForm::Package(ConsN(arguments, payload)) => {
                    let remaining = prefix.len() - witnesses.len();
                    witnesses.extend(arguments.iter().take(remaining).copied());
                    if arguments.len() > remaining {
                        self.bind_witnesses(pattern, &prefix, &witnesses, env)?;
                        let ty = self.ty(self.tycker.statics.annotations_vpat[&tail], env)?;
                        let max_scope = payload.0.max_scope;
                        return Ok(self.make_value(
                            value.0.source,
                            ty,
                            max_scope,
                            ValueForm::Package(ConsN(
                                arguments[remaining..].to_vec(),
                                payload.clone(),
                            )),
                        ));
                    }
                    value = payload.clone();
                }
                | _ if value.is_opaque_runtime() => break,
                | _ => unreachable!("package pattern has a package value"),
            }
        }
        self.bind_witnesses(pattern, &prefix, &witnesses, env)?;
        let ty = self.ty(self.tycker.statics.annotations_vpat[&tail], env)?;
        Ok(StaticValue(Rc::new(ValueInfo {
            uid: value.0.uid,
            source: value.0.source,
            ty,
            max_scope: value.0.max_scope,
            form: value.0.form.clone(),
            shared: value.0.shared,
        })))
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
    ) -> Result<()> {
        let mut domain = self.tycker.statics.annotations_vpat[&pattern].unroll(self.tycker)?;
        let mut assignments = Vec::new();
        for (pattern, argument) in prefix.iter().zip(witnesses.iter()) {
            match self.tycker.statics.normalized_at(domain).cloned() {
                | Some(Type::Exists(exists)) => {
                    let StaticTermId::Type(argument) = argument else { unreachable!() };
                    let payload = exists.binder.pattern.bind_argument(self.tycker, *argument)?;
                    assignments.push((exists.binder.witness, payload));
                    if let StaticPatId::Type(pattern) = pattern
                        && let Some(witness) = self.pattern_witness(*pattern)
                    {
                        let payload = pattern.bind_argument(self.tycker, *argument)?;
                        assignments.push((witness, payload));
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
        zydeco_foundation::stack::maybe_grow(|| match &self.tycker.statics.vpats[&pattern] {
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
        })
    }

    pub(super) fn runtime_pattern(
        &mut self, pattern: VPatId, env: &mut Environment,
    ) -> Result<ir::VPatId> {
        zydeco_foundation::stack::maybe_grow(|| self.runtime_pattern_inner(pattern, env))
    }

    fn runtime_pattern_inner(
        &mut self, pattern: VPatId, env: &mut Environment,
    ) -> Result<ir::VPatId> {
        let ty = self.ty(self.tycker.statics.annotations_vpat[&pattern], env)?;
        let node = match self.tycker.statics.vpats[&pattern].clone() {
            | ValuePattern::Hole(Hole) => ir::ValuePattern::Hole(Hole),
            | ValuePattern::Triv(Triv) => ir::ValuePattern::Triv(Triv),
            | ValuePattern::Lit(literal) => ir::ValuePattern::Lit(literal),
            | ValuePattern::Var(definition) => {
                let scope = self.current_scope();
                let name = self.var_name(definition);
                let (ir_pat, value) = self.named_variable(Some(name), Some(pattern), ty, scope);
                env.values += [(definition, value)];
                return Ok(ir_pat);
            }
            | ValuePattern::Named(Named(_, inner)) | ValuePattern::SCons(ConsN(_, inner)) => {
                return self.runtime_pattern(inner, env);
            }
            | ValuePattern::Ctor(Ctor(name, inner)) => {
                let source_data = self.tycker.statics.data_pat_hints[&pattern];
                let (data, tag) = self.ctor_idx(source_data, name);
                let payload = self.runtime_pattern(inner, env)?;
                ir::ValuePattern::Ctor { data, tag, payload }
            }
            | ValuePattern::VCons(patterns) => {
                let layout = self.product_layout(ty)?;
                let items = patterns
                    .into_iter()
                    .map(|pattern| self.runtime_pattern(pattern, env))
                    .collect::<Result<Vec<_>>>()?;
                if items.is_empty() && layout.arity == 0 {
                    ir::ValuePattern::Triv(Triv)
                } else {
                    ir::ValuePattern::VCons(IrVCons::new(items, layout))
                }
            }
            | ValuePattern::Alias(Alias(patterns)) => {
                let patterns = patterns
                    .into_iter()
                    .map(|pattern| self.runtime_pattern(pattern, env))
                    .collect::<Result<Vec<_>>>()?;
                ir::ValuePattern::Alias(Alias(ConsN::from_vec(patterns).unwrap()))
            }
            | ValuePattern::View(_) => unreachable!("views require static pattern elaboration"),
        };
        Ok(self.alloc_core_vpat(self.pat_span(pattern), node, ty))
    }

    pub(super) fn match_arms(
        &mut self, value: StaticValue, arms: &[Matcher<VPatId, CompuId>], env: &Environment,
        source: CompuId,
    ) -> Result<ir::CompuId> {
        let Some((first, rest)) = arms.split_first() else {
            let ty = self.ty(self.tycker.statics.annotations_compu[&source], env)?;
            let scrut = self.reify(&value)?;
            return Ok(self.alloc_core_compu(
                source,
                ir::Computation::Match(Match { scrut, arms: Vec::new() }),
                ty,
                None,
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
        &mut self, pending: Vec<(VPatId, StaticValue)>, tail: CompuId, env: Environment,
        failure: &MatchFailure<'_>,
    ) -> Result<ir::CompuId> {
        zydeco_foundation::stack::maybe_grow(|| {
            let step_scope = self.enter_scope();
            let result = self.match_steps_in_scope(pending, tail, env, failure, step_scope);
            let frame = self.exit_scope();
            result.map(|residual| self.wrap_compu_bindings(frame.bindings, residual))
        })
    }

    #[allow(clippy::too_many_lines)]
    fn match_steps_in_scope(
        &mut self, mut pending: Vec<(VPatId, StaticValue)>, tail: CompuId, mut env: Environment,
        failure: &MatchFailure<'_>, step_scope: usize,
    ) -> Result<ir::CompuId> {
        while let Some((pattern, value)) = pending.pop() {
            match self.tycker.statics.vpats[&pattern].clone() {
                | ValuePattern::Alias(Alias(patterns)) => {
                    pending
                        .extend(patterns.into_iter().rev().map(|pattern| (pattern, value.clone())));
                }
                | ValuePattern::Named(Named(name, inner)) => {
                    let payload = self.open_named(pattern, name, inner, value, &env)?;
                    pending.push((inner, payload));
                }
                | ValuePattern::SCons(ConsN(prefix, inner)) => {
                    let payload = self.open_package(pattern, prefix, inner, value, &mut env)?;
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
                                self.project(value.clone(), product, position, value.0.source)?,
                            ))
                        })
                        .collect::<Result<Vec<_>>>()?;
                    pending.extend(values.into_iter().rev());
                }
                | ValuePattern::Ctor(Ctor(name, payload)) => {
                    if let ValueForm::Constructor { name: found, payload: inner, .. } =
                        &value.0.form
                    {
                        if &name != found {
                            return self.match_arms(
                                failure.scrutinee.clone(),
                                failure.arms,
                                &failure.env,
                                failure.source,
                            );
                        }
                        pending.push((payload, inner.clone()));
                    } else {
                        let _ = step_scope;
                        let payload_ty =
                            self.ty(self.tycker.statics.annotations_vpat[&payload], &env)?;
                        let success_scope = self.enter_scope();
                        let (payload_pattern, inner) =
                            self.variable(None, payload_ty, success_scope);
                        pending.push((payload, inner));
                        let success = self.match_steps(pending, tail, env, failure);
                        let success_frame = self.exit_scope();
                        let success = self.wrap_compu_bindings(success_frame.bindings, success?);

                        let data = self.tycker.statics.data_pat_hints[&pattern];
                        let ir_data = self.intern_data(data);
                        let constructors = self.tycker.statics.datas[&data].clone();
                        let ctor_count = constructors.iter().count();
                        let ty =
                            self.ty(self.tycker.statics.annotations_compu[&tail], &failure.env)?;
                        let mut single_fallback = if ctor_count == 2 {
                            Some(self.match_arms(
                                failure.scrutinee.clone(),
                                failure.arms,
                                &failure.env,
                                failure.source,
                            )?)
                        } else {
                            None
                        };
                        // Several tags take the same failure continuation.
                        // Name it once so lowering does not duplicate the
                        // remaining pattern rows for every alternative tag.
                        let shared_fallback = if ctor_count > 2 {
                            let fallback = self.match_arms(
                                failure.scrutinee.clone(),
                                failure.arms,
                                &failure.env,
                                failure.source,
                            )?;
                            let thunk_ty = self.tycker.thk_arg(&TyEnv::default(), ty);
                            let thunk_span = self.term_span(failure.source);
                            let thunk_id = self.alloc_core_value_span(
                                thunk_span,
                                ir::Value::Thunk(Thunk(fallback)),
                                thunk_ty,
                            );
                            let var = self.core.vars.alloc(VarName::new("__match_fallback__"));
                            let binder = self.alloc_core_vpat(
                                Span::dummy(),
                                ir::ValuePattern::Var(var),
                                thunk_ty,
                            );
                            self.push_binding(CoreBinding {
                                binder,
                                bindee: thunk_id,
                                span: thunk_span,
                            });
                            Some((var, thunk_ty))
                        } else {
                            None
                        };

                        let scrut = self.reify(&value)?;
                        let mut success_arm = Some(success);
                        let arms = constructors
                            .iter()
                            .enumerate()
                            .map(|(idx, (candidate, payload_ty))| {
                                let tag = ir::CtorIdx { idx, name: *candidate };
                                let (arm_payload, arm_tail) = if candidate == &name {
                                    (
                                        payload_pattern,
                                        success_arm
                                            .take()
                                            .expect("matched constructor appears once"),
                                    )
                                } else {
                                    let hole = self.alloc_core_vpat(
                                        Span::dummy(),
                                        ir::ValuePattern::Hole(Hole),
                                        *payload_ty,
                                    );
                                    let arm_tail = match shared_fallback {
                                        | Some((fallback_var, thunk_ty)) => {
                                            let var_val = self.alloc_core_value_span(
                                                Span::dummy(),
                                                ir::Value::Var(fallback_var),
                                                thunk_ty,
                                            );
                                            self.alloc_core_compu(
                                                failure.source,
                                                ir::Computation::Force(Force(var_val)),
                                                ty,
                                                None,
                                            )
                                        }
                                        | None => single_fallback
                                            .take()
                                            .expect("two-constructor fallback is used once"),
                                    };
                                    (hole, arm_tail)
                                };
                                let binder = self.alloc_core_vpat(
                                    self.pat_span(pattern),
                                    ir::ValuePattern::Ctor {
                                        data: ir_data,
                                        tag,
                                        payload: arm_payload,
                                    },
                                    value.0.ty,
                                );
                                Matcher { binder, tail: arm_tail }
                            })
                            .collect();
                        return Ok(self.alloc_core_compu(
                            failure.source,
                            ir::Computation::Match(Match { scrut, arms }),
                            ty,
                            None,
                        ));
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
                    let binder = self.alloc_core_vpat(
                        self.pat_span(pattern),
                        ir::ValuePattern::Lit(literal),
                        value.0.ty,
                    );
                    let hole = self.alloc_core_vpat(
                        self.pat_span(pattern),
                        ir::ValuePattern::Hole(Hole),
                        value.0.ty,
                    );
                    let ty = self.ty(self.tycker.statics.annotations_compu[&tail], &failure.env)?;
                    return Ok(self.alloc_core_compu(
                        failure.source,
                        ir::Computation::Match(Match {
                            scrut,
                            arms: vec![
                                Matcher { binder, tail: success },
                                Matcher { binder: hole, tail: fallback },
                            ],
                        }),
                        ty,
                        None,
                    ));
                }
                | ValuePattern::View(view) => {
                    let viewed = self.view(view.function, value, &env)?;
                    pending.push((view.pattern, viewed));
                }
                | _ => self.bind(pattern, value, &mut env)?,
            }
        }
        self.computation(tail, &env)
    }
}
