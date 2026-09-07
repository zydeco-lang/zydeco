//! Check runtime representations and construct shared, typed residual bindings.

use super::*;

impl StaticElaborator<'_, '_> {
    pub(super) fn runtime_type(&mut self, ty: TypeId) -> bool {
        if let Some(result) = self.representations.get(&ty) {
            return *result;
        }
        let result = self.runtime_type_inner(ty, &mut HashSet::new());
        self.representations.insert(ty, result);
        result
    }

    fn runtime_type_inner(&self, ty: TypeId, visited: &mut HashSet<TypeId>) -> bool {
        if !visited.insert(ty) {
            return true;
        }
        match self.tycker.statics.normalized_at(ty).cloned() {
            | Some(Type::ValPi(_)) => false,
            | Some(Type::Abst(witness)) => match self.tycker.statics.seals.get(&witness).copied() {
                | Some(body) => self.runtime_type_inner(body, visited),
                | None => true,
            },
            | Some(Type::Prod(Prod(fields))) => {
                fields.into_iter().all(|ty| self.runtime_type_inner(ty, visited))
            }
            | Some(Type::Named(Named(_, body)))
            | Some(Type::Label(Label(_, body)))
            | Some(Type::ManifestKind(ManifestKind { body, .. }))
            | Some(Type::Forall(Forall(_, body))) => self.runtime_type_inner(body, visited),
            | Some(Type::Exists(exists)) => self.runtime_type_inner(exists.body, visited),
            | Some(Type::Arrow(Arrow(domain, codomain))) => {
                self.runtime_type_inner(domain, visited)
                    && self.runtime_type_inner(codomain, visited)
            }
            | Some(Type::PackPi(pi)) => {
                self.runtime_type_inner(pi.domain, visited)
                    && self.runtime_type_inner(pi.codomain, visited)
            }
            | Some(Type::App(App(function, argument))) => {
                self.runtime_type_inner(function, visited)
                    && self.runtime_type_inner(argument, visited)
            }
            | Some(Type::Data(data)) => {
                let arms = self.tycker.statics.datas[&data].clone();
                arms.into_iter().all(|(_, ty)| self.runtime_type_inner(ty, visited))
            }
            | Some(Type::CoData(codata)) => {
                let arms = self.tycker.statics.codatas[&codata].clone();
                arms.into_iter().all(|(_, ty)| self.runtime_type_inner(ty, visited))
            }
            | _ => true,
        }
    }

    pub(super) fn alloc_value(&mut self, source: ValueId, node: Value, ty: TypeId) -> ValueId {
        let constructor = matches!(node, Value::Ctor(_));
        let value = Alloc::alloc(self.tycker, node, ty, &TyEnv::default());
        if let Some(origin) = self.tycker.statics.terms.source(&source.into()) {
            self.tycker.statics.terms.record(origin, TermId::Value(value));
        }
        if constructor && let Some(hint) = self.tycker.statics.data_hints.get(&source).copied() {
            self.tycker.statics.data_hints.insert_new(value, hint);
        }
        value
    }

    pub(super) fn alloc_compu(
        &mut self, source: CompuId, node: Computation, ty: TypeId,
    ) -> CompuId {
        let compu = Alloc::alloc(self.tycker, node, ty, &TyEnv::default());
        if let Some(origin) = self.tycker.statics.terms.source(&source.into()) {
            self.tycker.statics.terms.record(origin, TermId::Compu(compu));
        }
        if let Some(hint) = self.tycker.statics.codata_hints.get(&source).copied() {
            self.tycker.statics.codata_hints.insert_new(compu, hint);
        }
        compu
    }

    pub(super) fn alloc_pattern(
        &mut self, source: VPatId, node: ValuePattern, ty: TypeId,
    ) -> VPatId {
        let constructor = matches!(node, ValuePattern::Ctor(_));
        let pattern = Alloc::alloc(self.tycker, node, ty, &TyEnv::default());
        if let Some(origin) = self.tycker.statics.pats.source(&source.into()) {
            self.tycker.statics.pats.record(origin, PatId::Value(pattern));
        }
        if constructor && let Some(hint) = self.tycker.statics.data_pat_hints.get(&source).copied()
        {
            self.tycker.statics.data_pat_hints.insert_new(pattern, hint);
        }
        pattern
    }

    pub(super) fn variable(
        &mut self, source: Option<ValueId>, ty: TypeId,
    ) -> (VPatId, StaticValue) {
        let definition =
            Alloc::alloc(self.tycker, VarName("__static_value__".into()), AnnId::Type(ty), &());
        let pattern =
            Alloc::alloc(self.tycker, ValuePattern::Var(definition), ty, &TyEnv::default());
        let value = match source {
            | Some(source) => self.alloc_value(source, Value::Var(definition), ty),
            | None => Alloc::alloc(self.tycker, Value::Var(definition), ty, &TyEnv::default()),
        };
        (pattern, StaticValue::with_form(source.unwrap_or(value), ty, ValueForm::Runtime(value)))
    }

    pub(super) fn value_bindings(&mut self, bindings: Vec<Binding>, mut tail: ValueId) -> ValueId {
        for Binding { binder, bindee } in bindings.into_iter().rev() {
            let ty = self.tycker.statics.annotations_value[&tail];
            tail = self.alloc_value(tail, Value::Let(Let { binder, bindee, tail }), ty);
        }
        tail
    }

    pub(super) fn compu_bindings(&mut self, bindings: Vec<Binding>, mut tail: CompuId) -> CompuId {
        for Binding { binder, bindee } in bindings.into_iter().rev() {
            let ty = self.tycker.statics.annotations_compu[&tail];
            tail = self.alloc_compu(tail, Computation::Let(Let { binder, bindee, tail }), ty);
        }
        tail
    }

    pub(super) fn reify(&mut self, value: &StaticValue) -> ResultKont<ValueId> {
        if self.purpose == Purpose::Inspect {
            return Err(KontFailure);
        }
        if !self.runtime_type(value.0.ty) {
            return self.fail(StaticEliminationError::RuntimeValue {
                value: value.0.source,
                ty: value.0.ty,
            });
        }
        if let Some(shared) = value.0.shared {
            return Ok(shared);
        }
        let node = match &value.0.form {
            | ValueForm::Function { .. } => {
                return self.fail(StaticEliminationError::RuntimeValue {
                    value: value.0.source,
                    ty: value.0.ty,
                });
            }
            | ValueForm::Runtime(runtime) => {
                return Ok(*runtime);
            }
            | ValueForm::Product(fields) => Value::VCons(
                fields.iter().map(|value| self.reify(value)).collect::<ResultKont<_>>()?,
            ),
            | ValueForm::Constructor(name, payload) => {
                Value::Ctor(Ctor(name.clone(), self.reify(payload)?))
            }
            | ValueForm::Package(ConsN(prefix, payload)) => {
                Value::SCons(ConsN(prefix.clone(), self.reify(payload)?))
            }
            | ValueForm::Named(name, payload) => {
                Value::Named(Named(name.clone(), self.reify(payload)?))
            }
        };
        Ok(self.alloc_value(value.0.source, node, value.0.ty))
    }

    pub(super) fn share(
        &mut self, value: StaticValue, bindings: &mut Vec<Binding>,
    ) -> ResultKont<StaticValue> {
        if self.purpose == Purpose::Inspect
            || value.0.shared.is_some()
            || matches!(value.0.form, ValueForm::Function { .. })
        {
            return Ok(value);
        }
        // Share the fields first, so opening a known product or package uses
        // the same runtime values as materializing the aggregate.
        let form = match &value.0.form {
            | ValueForm::Product(fields) => ValueForm::Product(
                fields
                    .iter()
                    .map(|field| self.share(field.clone(), bindings))
                    .collect::<ResultKont<_>>()?,
            ),
            | ValueForm::Constructor(name, payload) => {
                ValueForm::Constructor(name.clone(), self.share(payload.clone(), bindings)?)
            }
            | ValueForm::Package(ConsN(prefix, payload)) => {
                ValueForm::Package(ConsN(prefix.clone(), self.share(payload.clone(), bindings)?))
            }
            | ValueForm::Named(name, payload) => {
                ValueForm::Named(name.clone(), self.share(payload.clone(), bindings)?)
            }
            | ValueForm::Runtime(_) => value.0.form.clone(),
            | ValueForm::Function { .. } => unreachable!(),
        };
        let value = StaticValue::with_form(value.0.source, value.0.ty, form);
        if value.is_runtime() && self.runtime_type(value.0.ty) {
            let bindee = self.reify(&value)?;
            let (binder, variable) = self.variable(Some(value.0.source), value.0.ty);
            let ValueForm::Runtime(shared) = variable.0.form else { unreachable!() };
            bindings.push(Binding { binder, bindee });
            Ok(StaticValue(Arc::new(ValueInfo {
                source: value.0.source,
                ty: value.0.ty,
                form: value.0.form.clone(),
                shared: Some(shared),
            })))
        } else {
            Ok(value)
        }
    }
}
