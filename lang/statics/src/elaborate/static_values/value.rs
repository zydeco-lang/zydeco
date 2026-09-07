//! Evaluate lexical static closures, value applications, and structural projections.

use super::*;

impl StaticElaborator<'_, '_> {
    pub(super) fn value(
        &mut self, source: ValueId, env: &Environment, bindings: &mut Vec<Binding>,
    ) -> ResultKont<StaticValue> {
        let ty = self.ty(self.tycker.statics.annotations_value[&source], env)?;
        if self.tycker.statics.foreign_imports.get(&source).is_some() {
            return Ok(StaticValue::with_form(source, ty, ValueForm::Runtime(source)));
        }
        let form = match self.tycker.statics.values[&source].clone() {
            | Value::Var(definition) => {
                if let Some(value) = env.values.get(&definition) {
                    return Ok(StaticValue(Arc::new(ValueInfo {
                        source,
                        ty: value.0.ty,
                        form: value.0.form.clone(),
                        shared: value.0.shared,
                    })));
                }
                if self.purpose == Purpose::Inspect {
                    return self.inspect_alias(source, definition, ty, env, bindings);
                }
                ValueForm::Runtime(source)
            }
            | Value::Hole(_) | Value::Triv(_) | Value::Lit(_) => ValueForm::Runtime(source),
            | Value::ValAbs(Abs(binder, body)) => {
                ValueForm::Function { binder, body, env: env.clone() }
            }
            | Value::ValApp(App(function, argument)) => {
                let function = self.value(function, env, bindings)?;
                return self.apply(function, argument, env, bindings);
            }
            | Value::Let(Let { binder, bindee, tail }) => {
                let value = self.value(bindee, env, bindings)?;
                let value = self.share(value, bindings)?;
                let mut local = env.clone();
                self.bind(binder, value, &mut local, bindings)?;
                return self.value(tail, &local, bindings);
            }
            | Value::Thunk(Thunk(body)) => {
                if self.purpose == Purpose::Inspect {
                    return Ok(StaticValue::with_form(source, ty, ValueForm::Runtime(source)));
                }
                let body = self.computation(body, env)?;
                ValueForm::Runtime(self.alloc_value(source, Value::Thunk(Thunk(body)), ty))
            }
            | Value::VCons(fields) => ValueForm::Product(
                fields
                    .into_iter()
                    .map(|field| self.value(field, env, bindings))
                    .collect::<ResultKont<_>>()?,
            ),
            | Value::Ctor(Ctor(name, payload)) => {
                ValueForm::Constructor(name, self.value(payload, env, bindings)?)
            }
            | Value::Named(Named(name, payload)) => {
                ValueForm::Named(name, self.value(payload, env, bindings)?)
            }
            | Value::SCons(ConsN(prefix, payload)) => {
                let prefix = prefix
                    .into_iter()
                    .map(|witness| match witness {
                        | StaticTermId::Type(ty) => self.ty(ty, env).map(StaticTermId::Type),
                        | StaticTermId::Kind(_) => Ok(witness),
                    })
                    .collect::<ResultKont<Vec<_>>>()?;
                ValueForm::Package(ConsN(prefix, self.value(payload, env, bindings)?))
            }
            | Value::Proj(Proj(head, field)) => {
                let mut value = self.value(head, env, bindings)?;
                for (index, projection) in field.target.products.iter().enumerate() {
                    if matches!(value.0.form, ValueForm::Runtime(_)) {
                        if self.purpose == Purpose::Inspect {
                            return Ok(StaticValue::with_form(
                                source,
                                ty,
                                ValueForm::Runtime(source),
                            ));
                        }
                        let products = field.target.products[index..]
                            .iter()
                            .map(|step| {
                                Ok(ProductProjection {
                                    product: self.ty(step.product, env)?,
                                    position: step.position,
                                })
                            })
                            .collect::<ResultKont<_>>()?;
                        let head = self.reify(&value)?;
                        let node = Value::Proj(Proj(
                            head,
                            ResolvedField { name: field.name, target: ProjTarget { products } },
                        ));
                        let residual = self.alloc_value(source, node, ty);
                        return Ok(StaticValue::with_form(
                            source,
                            ty,
                            ValueForm::Runtime(residual),
                        ));
                    }
                    let product = self.ty(projection.product, env)?;
                    value = self.project(value, product, projection.position, source, bindings)?;
                }
                let value = value.unnamed();
                if matches!(value.0.form, ValueForm::Runtime(_)) && value.0.ty != ty {
                    if self.purpose == Purpose::Inspect {
                        return Ok(StaticValue::with_form(source, ty, ValueForm::Runtime(source)));
                    }
                    let head = self.reify(&value)?;
                    let node = Value::Proj(Proj(
                        head,
                        ResolvedField {
                            name: field.name,
                            target: ProjTarget { products: Vec::new() },
                        },
                    ));
                    let residual = self.alloc_value(source, node, ty);
                    return Ok(StaticValue::with_form(source, ty, ValueForm::Runtime(residual)));
                }
                return Ok(value);
            }
        };
        Ok(StaticValue::with_form(source, ty, form))
    }

    fn apply(
        &mut self, function: StaticValue, argument: ValArgument, caller: &Environment,
        bindings: &mut Vec<Binding>,
    ) -> ResultKont<StaticValue> {
        self.enter_reduction(function.0.source)?;
        let result = self.apply_inner(function, argument, caller, bindings);
        self.reduction_depth -= 1;
        result
    }

    fn inspect_alias(
        &mut self, source: ValueId, definition: DefId, ty: TypeId, env: &Environment,
        bindings: &mut Vec<Binding>,
    ) -> ResultKont<StaticValue> {
        if !self.aliases.insert(definition) {
            return Err(KontFailure);
        }
        let alias = self.tycker.statics.value_aliases.get(&definition).copied();
        let result = match alias {
            | Some(alias) => self.value(alias, env, bindings),
            | None => Ok(StaticValue::with_form(source, ty, ValueForm::Runtime(source))),
        };
        self.aliases.remove(&definition);
        let value = result?;
        if let Some(witnesses) = self.tycker.statics.package_aliases.get(&definition).cloned() {
            let value = value.unnamed();
            let payload = match &value.0.form {
                | ValueForm::Package(ConsN(_, payload)) => payload.clone(),
                | _ => value.clone(),
            };
            Ok(StaticValue::with_form(source, ty, ValueForm::Package(ConsN(witnesses, payload))))
        } else {
            Ok(value)
        }
    }

    fn apply_inner(
        &mut self, function: StaticValue, argument: ValArgument, caller: &Environment,
        bindings: &mut Vec<Binding>,
    ) -> ResultKont<StaticValue> {
        let function = function.unnamed();
        let ValueForm::Function { binder, body, env } = &function.0.form else {
            return self.fail(StaticEliminationError::UnresolvedApplication {
                function: function.0.source,
            });
        };
        let mut local = env.clone();
        match (binder, argument) {
            | (ValBinder::Type(pattern), ValArgument::Type(argument)) => {
                let argument = self.ty(argument, caller)?;
                let classifier = function.0.ty.unroll_k(self.tycker)?;
                let Some(Type::ValPi(pi)) = self.tycker.statics.normalized_at(classifier) else {
                    unreachable!()
                };
                let ValPiBinder::Type(binder) = &pi.binder else { unreachable!() };
                let assignments = std::iter::once((binder.witness, argument))
                    .chain(self.pattern_witness(*pattern).map(|witness| (witness, argument)));
                local = local.with_types(assignments);
            }
            | (ValBinder::Value(pattern), ValArgument::Value(argument)) => {
                let argument = self.value(argument, caller, bindings)?;
                let argument = self.share(argument, bindings)?;
                self.bind(*pattern, argument, &mut local, bindings)?;
            }
            | _ => unreachable!("typed value application follows its binder sort"),
        }
        self.value(*body, &local, bindings)
    }

    pub(super) fn project(
        &mut self, value: StaticValue, product: TypeId, position: usize, source: ValueId,
        bindings: &mut Vec<Binding>,
    ) -> ResultKont<StaticValue> {
        match &value.0.form {
            | ValueForm::Named(_, inner) | ValueForm::Package(ConsN(_, inner)) => {
                self.project(inner.clone(), product, position, source, bindings)
            }
            | ValueForm::Product(fields) => Ok(fields[position].clone()),
            | ValueForm::Runtime(_) => {
                let product = product.unroll_k(self.tycker)?;
                let Some(Type::Prod(Prod(fields))) =
                    self.tycker.statics.normalized_at(product).cloned()
                else {
                    unreachable!("projection has a product receiver")
                };
                if self.purpose == Purpose::Inspect {
                    return Ok(StaticValue::with_form(
                        source,
                        fields[position],
                        ValueForm::Runtime(source),
                    ));
                }
                let mut patterns = Vec::new();
                let mut selected = None;
                for (index, ty) in fields.into_iter().enumerate() {
                    if index == position {
                        let (pattern, value) = self.variable(Some(source), ty);
                        patterns.push(pattern);
                        selected = Some(value);
                    } else {
                        patterns.push(Alloc::alloc(
                            self.tycker,
                            ValuePattern::Hole(Hole),
                            ty,
                            &TyEnv::default(),
                        ));
                    }
                }
                let binder = Alloc::alloc(
                    self.tycker,
                    ValuePattern::VCons(patterns),
                    product,
                    &TyEnv::default(),
                );
                let bindee = self.reify(&value)?;
                bindings.push(Binding { binder, bindee });
                Ok(selected.unwrap())
            }
            | _ => unreachable!("projection has a product receiver"),
        }
    }
}
