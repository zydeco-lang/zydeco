//! Evaluate lexical static closures, value applications, and structural projections.

use super::*;

impl StaticElaborator<'_, '_> {
    #[allow(clippy::too_many_lines)]
    pub(super) fn value(&mut self, source: ValueId, env: &Environment) -> Result<StaticValue> {
        zydeco_foundation::stack::maybe_grow(|| self.value_inner(source, env))
    }

    #[allow(clippy::too_many_lines)]
    fn value_inner(&mut self, source: ValueId, env: &Environment) -> Result<StaticValue> {
        let ty = self.ty(self.tycker.statics.annotations_value[&source], env)?;
        if let Some(import) = self.tycker.statics.foreign_imports.get(source).cloned() {
            return Ok(self.make_value(source, ty, 0, ValueForm::Foreign(import)));
        }
        match self.tycker.statics.values[&source].clone() {
            | Value::Var(definition) => {
                if let Some(value) = env.values.get(&definition) {
                    return Ok(StaticValue(Rc::new(ValueInfo {
                        uid: value.0.uid,
                        source,
                        ty: value.0.ty,
                        max_scope: value.0.max_scope,
                        form: value.0.form.clone(),
                        shared: value.0.shared,
                    })));
                }
                if self.purpose == Purpose::Inspect {
                    return self.inspect_alias(source, definition, ty, env);
                }
                let name = self.var_name(definition);
                let var =
                    *self.free_vars.entry(definition).or_insert_with(|| self.core.vars.alloc(name));
                Ok(self.make_value(source, ty, 0, ValueForm::RuntimeVar(var)))
            }
            | Value::Hole(_) => {
                let preallocated =
                    if self.purpose == Purpose::Residualize && self.reduction_depth == 0 {
                        let id = self.alloc_core_value(source, ir::Value::Hole(Hole), ty);
                        self.core.retained_holes.push(ir::TermId::Value(id));
                        Some(id)
                    } else {
                        None
                    };
                Ok(self.make_value(
                    source,
                    ty,
                    0,
                    ValueForm::Hole { preallocated: Rc::new(Cell::new(preallocated)) },
                ))
            }
            | Value::Triv(_) => Ok(self.make_value(source, ty, 0, ValueForm::Triv)),
            | Value::Lit(literal) => Ok(self.make_value(source, ty, 0, ValueForm::Lit(literal))),
            | Value::ValAbs(Abs(binder, body)) => {
                let max_scope = self.function_max_scope(body, &binder, env);
                Ok(self.make_value(
                    source,
                    ty,
                    max_scope,
                    ValueForm::Function { binder, body, env: env.clone() },
                ))
            }
            | Value::ValApp(App(function, argument)) => {
                let function = self.value(function, env)?;
                self.apply(source, function, argument, env)
            }
            | Value::IntOp(IntValueOp { operation, operands }) => {
                let operands = operands
                    .into_iter()
                    .map(|operand| {
                        let operand = self.value(operand, env)?.unnamed();
                        if let ValueForm::Lit(Literal::Integer(IntegerLiteral::Int(value))) =
                            operand.0.form
                        {
                            Ok(value)
                        } else {
                            self.fail(StaticEliminationError::UnresolvedInteger {
                                value: self.application_site.unwrap_or(source),
                            })
                        }
                    })
                    .collect::<Result<Vec<_>>>()?;
                let literal = Literal::Integer(IntegerLiteral::Int(
                    operation.evaluate([operands[0], operands[1]]),
                ));
                Ok(self.make_value(source, ty, 0, ValueForm::Lit(literal)))
            }
            | Value::Match(Match { scrut, arms }) => {
                let value = self.value(scrut, env)?;
                let value = self.share(value)?;
                for Matcher { binder, tail } in arms {
                    let mut local = env.clone();
                    if self.value_pattern(source, binder, value.clone(), &mut local)? {
                        return self.value(tail, &local);
                    }
                }
                self.fail(StaticEliminationError::UnresolvedMatch { value: source })
            }
            | Value::Let(Let { binder, bindee, tail }) => {
                let value = self.value(bindee, env)?;
                let value = self.share(value)?;
                let mut local = env.clone();
                self.bind(binder, value, &mut local)?;
                self.value(tail, &local)
            }
            | Value::Thunk(Thunk(body)) => {
                if self.purpose == Purpose::Inspect {
                    return Ok(self.make_value(
                        source,
                        ty,
                        0,
                        ValueForm::RuntimeTerm(Rc::new(Cell::new(None))),
                    ));
                }
                // Top-level thunks authored directly in executable code still validate
                // static-elimination boundaries and preserve unreferenced holes. Thunks inside
                // static value functions or export candidates stay lazy until demanded.
                if self.purpose == Purpose::Residualize && self.reduction_depth == 0 {
                    if !self.runtime_type(ty) {
                        return self
                            .fail(StaticEliminationError::RuntimeValue { value: source, ty });
                    }
                    let body_id = self.computation(body, env)?;
                    let thunk_id =
                        self.alloc_core_value(source, ir::Value::Thunk(Thunk(body_id)), ty);
                    let max_scope = self.thunk_max_scope(body, env);
                    Ok(self.make_value(
                        source,
                        ty,
                        max_scope,
                        ValueForm::RuntimeTerm(Rc::new(Cell::new(Some(thunk_id)))),
                    ))
                } else {
                    let max_scope = self.thunk_max_scope(body, env);
                    Ok(self.make_value(
                        source,
                        ty,
                        max_scope,
                        ValueForm::Thunk { body, env: env.clone() },
                    ))
                }
            }
            | Value::VCons(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|field| self.value(field, env))
                    .collect::<Result<Vec<_>>>()?;
                let max_scope = fields.iter().map(|field| field.0.max_scope).max().unwrap_or(0);
                Ok(self.make_value(source, ty, max_scope, ValueForm::Product(fields)))
            }
            | Value::Ctor(Ctor(name, payload)) => {
                let payload = self.value(payload, env)?;
                let max_scope = payload.0.max_scope;
                let data = self.resolve_data_id(source, ty);
                Ok(self.make_value(
                    source,
                    ty,
                    max_scope,
                    ValueForm::Constructor { data, name, payload },
                ))
            }
            | Value::Named(Named(name, payload)) => {
                let payload = self.value(payload, env)?;
                let max_scope = payload.0.max_scope;
                Ok(self.make_value(source, ty, max_scope, ValueForm::Named(name, payload)))
            }
            | Value::SCons(ConsN(prefix, payload)) => {
                let prefix = prefix
                    .into_iter()
                    .map(|witness| match witness {
                        | StaticTermId::Type(ty) => self.ty(ty, env).map(StaticTermId::Type),
                        | StaticTermId::Kind(_) => Ok(witness),
                    })
                    .collect::<Result<Vec<_>>>()?;
                let payload = self.value(payload, env)?;
                let max_scope = payload.0.max_scope;
                Ok(self.make_value(
                    source,
                    ty,
                    max_scope,
                    ValueForm::Package(ConsN(prefix, payload)),
                ))
            }
            | Value::Proj(Proj(head, field)) => {
                let mut value = self.value(head, env)?;
                for (index, projection) in field.target.products.iter().enumerate() {
                    if value.is_opaque_runtime() {
                        if self.purpose == Purpose::Inspect {
                            return Ok(self.make_value(
                                source,
                                ty,
                                0,
                                ValueForm::RuntimeTerm(Rc::new(Cell::new(None))),
                            ));
                        }
                        let steps = field.target.products[index..]
                            .iter()
                            .map(|step| {
                                let product = self.ty(step.product, env)?;
                                let layout = self.product_layout(product)?;
                                Ok(ProductProjection { layout, position: step.position })
                            })
                            .collect::<Result<Vec<_>>>()?;
                        let scope = self.current_scope();
                        let head = self.reify(&value)?;
                        let proj_id =
                            self.alloc_core_value(source, ir::Value::Proj { head, steps }, ty);
                        return Ok(self.make_value(
                            source,
                            ty,
                            scope,
                            ValueForm::RuntimeTerm(Rc::new(Cell::new(Some(proj_id)))),
                        ));
                    }
                    let product = self.ty(projection.product, env)?;
                    value = self.project(value, product, projection.position, source)?;
                }
                let value = value.unnamed();
                if value.0.ty != ty {
                    Ok(StaticValue(Rc::new(ValueInfo {
                        uid: value.0.uid,
                        source,
                        ty,
                        max_scope: value.0.max_scope,
                        form: value.0.form.clone(),
                        shared: value.0.shared,
                    })))
                } else {
                    Ok(value)
                }
            }
        }
    }

    fn apply(
        &mut self, source: ValueId, function: StaticValue, argument: ValArgument,
        caller: &Environment,
    ) -> Result<StaticValue> {
        let function = function.unnamed();
        let ValueForm::Function { binder, body, env } = &function.0.form else {
            return self.fail(StaticEliminationError::UnresolvedApplication {
                function: function.0.source,
            });
        };
        let binder = binder.clone();
        let body = *body;
        let mut local = env.clone();
        // Evaluate value arguments in caller depth before entering the callee body so
        // top-level arguments retain their eager validation semantics.
        match (&binder, argument) {
            | (ValBinder::Type(pattern), ValArgument::Type(argument)) => {
                let argument = self.ty(argument, caller)?;
                let classifier = function.0.ty.unroll(self.tycker)?;
                let Some(Type::ValPi(pi)) = self.tycker.statics.normalized_at(classifier) else {
                    unreachable!()
                };
                let ValPiBinder::Type(pi_binder) = &pi.binder else { unreachable!() };
                let pi_binder = pi_binder.clone();
                let payload = pi_binder.pattern.bind_argument(self.tycker, argument)?;
                let body_payload = pattern.bind_argument(self.tycker, argument)?;
                let assignments = std::iter::once((pi_binder.witness, payload))
                    .chain(self.pattern_witness(*pattern).map(|witness| (witness, body_payload)));
                local = local.with_types(assignments);
            }
            | (ValBinder::Value(pattern), ValArgument::Value(argument)) => {
                let argument = self.value(argument, caller)?;
                let argument = self.share(argument)?;
                self.bind(*pattern, argument, &mut local)?;
            }
            | _ => unreachable!("typed value application follows its binder sort"),
        }
        self.enter_reduction(function.0.source)?;
        let previous = self.application_site;
        self.application_site = previous.or(Some(source));
        let result = self.value(body, &local);
        self.application_site = previous;
        self.reduction_depth -= 1;
        result
    }

    fn inspect_alias(
        &mut self, source: ValueId, definition: DefId, ty: TypeId, env: &Environment,
    ) -> Result<StaticValue> {
        if !self.aliases.insert(definition) {
            return Err(ReportedError);
        }
        let alias = self.tycker.statics.value_aliases.get(&definition).copied();
        let result = match alias {
            | Some(alias) => self.value(alias, env),
            | None => {
                Ok(self.make_value(source, ty, 0, ValueForm::RuntimeTerm(Rc::new(Cell::new(None)))))
            }
        };
        self.aliases.remove(&definition);
        let value = result?;
        if let Some(witnesses) = self.tycker.statics.package_aliases.get(&definition).cloned() {
            let value = value.unnamed();
            let payload = match &value.0.form {
                | ValueForm::Package(ConsN(_, payload)) => payload.clone(),
                | _ => value.clone(),
            };
            let max_scope = payload.0.max_scope;
            Ok(self.make_value(
                source,
                ty,
                max_scope,
                ValueForm::Package(ConsN(witnesses, payload)),
            ))
        } else {
            Ok(value)
        }
    }

    pub(super) fn project(
        &mut self, value: StaticValue, product: TypeId, position: usize, source: ValueId,
    ) -> Result<StaticValue> {
        match &value.0.form {
            | ValueForm::Named(_, inner) | ValueForm::Package(ConsN(_, inner)) => {
                self.project(inner.clone(), product, position, source)
            }
            | ValueForm::Product(fields) => Ok(fields[position].clone()),
            | form if value.is_opaque_runtime() => {
                let _ = form;
                let product = product.unroll(self.tycker)?;
                let Some(Type::Prod(Prod(fields))) =
                    self.tycker.statics.normalized_at(product).cloned()
                else {
                    unreachable!("projection has a product receiver")
                };
                if self.purpose == Purpose::Inspect {
                    return Ok(self.make_value(
                        source,
                        fields[position],
                        0,
                        ValueForm::RuntimeTerm(Rc::new(Cell::new(None))),
                    ));
                }
                let layout = ProductLayout { arity: fields.len() };
                let mut patterns = Vec::with_capacity(fields.len());
                let mut selected = None;
                let scope = self.current_scope();
                let source_span = self.term_span(source);
                for (index, field_ty) in fields.into_iter().enumerate() {
                    if index == position {
                        let (pattern, proj_value) = self.variable(None, field_ty, scope);
                        patterns.push(pattern);
                        selected = Some(proj_value);
                    } else {
                        patterns.push(self.alloc_core_vpat(
                            Span::dummy(),
                            ir::ValuePattern::Hole(Hole),
                            field_ty,
                        ));
                    }
                }
                let binder = self.alloc_core_vpat(
                    Span::dummy(),
                    ir::ValuePattern::VCons(IrVCons::new(patterns, layout)),
                    product,
                );
                let bindee = self.reify(&value)?;
                self.push_binding(CoreBinding { binder, bindee, span: source_span });
                Ok(selected.expect("selected projection field"))
            }
            | _ => unreachable!("projection has a product receiver"),
        }
    }
}
