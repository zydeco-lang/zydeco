//! Check runtime representations and construct shared, demand-driven [`zydeco_ir::CoreProgram`] nodes.

use super::*;

impl StaticElaborator<'_, '_> {
    pub(super) fn runtime_type(&mut self, ty: TypeId) -> bool {
        if let Some(result) = self.representations.get(&ty) {
            return *result;
        }
        let result = self.runtime_type_inner(ty, &mut FxHashSet::default());
        self.representations.insert(ty, result);
        result
    }

    fn runtime_type_inner(&self, ty: TypeId, visited: &mut FxHashSet<TypeId>) -> bool {
        if !visited.insert(ty) {
            return true;
        }
        zydeco_foundation::stack::maybe_grow(|| {
            match self.tycker.statics.normalized_at(ty).cloned() {
                | Some(Type::ValPi(_)) => false,
                | Some(Type::Abst(witness)) => {
                    match self
                        .tycker
                        .statics
                        .seals
                        .get(witness)
                        .or_else(|| self.tycker.statics.opaque_representations.get(witness))
                        .copied()
                    {
                        | Some(body) => self.runtime_type_inner(body, visited),
                        | None => true,
                    }
                }
                | Some(Type::Abs(TypeAbstraction { body, .. })) => {
                    self.runtime_type_inner(body, visited)
                }
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
        })
    }

    pub(super) fn make_value(
        &mut self, source: ValueId, ty: TypeId, max_scope: usize, form: ValueForm,
    ) -> StaticValue {
        let uid = self.next_uid;
        self.next_uid += 1;
        StaticValue(Rc::new(ValueInfo { uid, source, ty, max_scope, form, shared: None }))
    }

    pub(super) fn current_scope(&self) -> usize {
        self.scopes.len().saturating_sub(1)
    }

    pub(super) fn enter_scope(&mut self) -> usize {
        let idx = self.scopes.len();
        self.scopes.push(ScopeFrame::default());
        idx
    }

    pub(super) fn exit_scope(&mut self) -> ScopeFrame {
        let frame = self.scopes.pop().expect("balanced elaboration scope stack");
        for key in &frame.memo_keys {
            self.memoized_thunks.remove(key);
        }
        frame
    }

    pub(super) fn push_binding(&mut self, binding: CoreBinding) {
        let idx = self.current_scope();
        self.scopes[idx].bindings.push(binding);
    }

    pub(super) fn term_span(&self, term: impl Into<TermId>) -> Span {
        self.tycker.statics_term_span_or_dummy(term.into())
    }

    pub(super) fn pat_span(&self, pat: VPatId) -> Span {
        self.tycker.statics_pat_span_or_dummy(PatId::Value(pat))
    }

    pub(super) fn var_name(&self, definition: DefId) -> VarName {
        *self.tycker.statics.def_name(self.tycker.scoped, &definition)
    }

    pub(super) fn pattern_var_hint(&self, pattern: VPatId) -> Option<VarName> {
        match self.tycker.statics.vpats[&pattern] {
            | ValuePattern::Var(definition) => Some(self.var_name(definition)),
            | _ => None,
        }
    }

    pub(super) fn intern_data(&mut self, data: DataId) -> IrDataId {
        if let Some(&id) = self.data_map.get(&data) {
            return id;
        }
        let ctors = self.tycker.statics.datas[&data].iter().map(|(name, _)| *name).collect();
        let id = self.core.datas.alloc(ctors);
        self.data_map.insert(data, id);
        id
    }

    pub(super) fn ctor_idx(&mut self, data: DataId, name: CtorName) -> (IrDataId, ir::CtorIdx) {
        let idx = self.tycker.statics.datas[&data]
            .iter()
            .position(|(candidate, _)| candidate == &name)
            .expect("checked constructor belongs to its data declaration");
        let ir_data = self.intern_data(data);
        (ir_data, ir::CtorIdx { idx, name })
    }

    pub(super) fn resolve_data_id(&mut self, source: ValueId, ty: TypeId) -> Option<DataId> {
        if let Some(data) = self.tycker.statics.data_hints.get(source).copied() {
            return Some(data);
        }
        let mut current = ty.unroll(self.tycker).ok().unwrap_or(ty);
        let mut visited = FxHashSet::default();
        while visited.insert(current) {
            match self.tycker.statics.normalized_at(current).cloned() {
                | Some(Type::Data(data)) => return Some(data),
                | Some(Type::Named(Named(_, inner))) | Some(Type::Label(Label(_, inner))) => {
                    current = inner;
                }
                | Some(Type::Exists(exists)) => current = exists.body,
                | Some(Type::ManifestKind(manifest)) => current = manifest.body,
                | Some(Type::Abst(witness)) => {
                    current = self
                        .tycker
                        .statics
                        .seals
                        .get(witness)
                        .or_else(|| self.tycker.statics.opaque_representations.get(witness))
                        .copied()?;
                }
                | _ => break,
            }
        }
        None
    }

    pub(super) fn resolve_codata_id(&mut self, ty: TypeId) -> Option<CoDataId> {
        let mut current = ty.unroll(self.tycker).ok().unwrap_or(ty);
        let mut visited = FxHashSet::default();
        while visited.insert(current) {
            match self.tycker.statics.normalized_at(current).cloned() {
                | Some(Type::CoData(codata)) => return Some(codata),
                | Some(Type::Named(Named(_, inner)))
                | Some(Type::Label(Label(_, inner)))
                | Some(Type::Forall(Forall(_, inner))) => current = inner,
                | Some(Type::Abst(witness)) => {
                    current = self
                        .tycker
                        .statics
                        .seals
                        .get(witness)
                        .or_else(|| self.tycker.statics.opaque_representations.get(witness))
                        .copied()?;
                }
                | _ => break,
            }
        }
        None
    }

    pub(super) fn product_layout(&mut self, ty: TypeId) -> Result<ProductLayout> {
        let mut current = if let Some(&cached) = self.unrolled_cache.get(&ty) {
            cached
        } else {
            let unrolled = match self.tycker.statics.normalized_at(ty) {
                | Some(Type::Unit(_) | Type::Prod(_)) => ty,
                | _ => ty.unroll(self.tycker).unwrap_or(ty),
            };
            self.unrolled_cache.insert(ty, unrolled);
            unrolled
        };
        let mut visited = FxHashSet::default();
        while visited.insert(current) {
            match self.tycker.statics.normalized_at(current) {
                | Some(Type::Unit(_)) => return Ok(ProductLayout { arity: 0 }),
                | Some(Type::Prod(Prod(fields))) => {
                    return Ok(ProductLayout { arity: fields.len() });
                }
                | Some(Type::Named(Named(_, inner))) | Some(Type::Label(Label(_, inner))) => {
                    current = *inner;
                }
                | Some(Type::Exists(exists)) => current = exists.body,
                | Some(Type::ManifestKind(manifest)) => current = manifest.body,
                | Some(Type::Abst(witness)) => {
                    let Some(next) = self
                        .tycker
                        .statics
                        .seals
                        .get(witness)
                        .or_else(|| self.tycker.statics.opaque_representations.get(witness))
                        .copied()
                    else {
                        break;
                    };
                    current = next;
                }
                | _ => break,
            }
        }
        unreachable!("VCons must have Unit or product type")
    }

    pub(super) fn alloc_core_vpat(
        &mut self, span: Span, node: ir::ValuePattern, ty: TypeId,
    ) -> ir::VPatId {
        let id = self.core.vpats.alloc(node);
        let protocol = self.protocols.value(&self.tycker.statics, ty);
        self.core.pattern_protocols.insert_new(id, protocol);
        if !span.is_dummy() {
            self.core.vpat_spans.insert_new(id, span);
        }
        id
    }

    pub(super) fn alloc_core_value(
        &mut self, source: ValueId, node: ir::Value, ty: TypeId,
    ) -> ir::ValueId {
        self.alloc_core_value_span(self.term_span(source), node, ty)
    }

    pub(super) fn alloc_core_value_span(
        &mut self, span: Span, node: ir::Value, ty: TypeId,
    ) -> ir::ValueId {
        let id = self.core.values.alloc(node);
        let protocol = self.protocols.value(&self.tycker.statics, ty);
        self.core.value_protocols.insert_new(id, protocol);
        if !span.is_dummy() {
            self.core.value_spans.insert_new(id, span);
        }
        id
    }

    pub(super) fn alloc_core_compu(
        &mut self, source: CompuId, node: ir::Computation, ty: TypeId, codata: Option<CoDataId>,
    ) -> ir::CompuId {
        let span = self.term_span(source);
        let is_fix = matches!(node, ir::Computation::Fix { .. });
        let id = self.core.compus.alloc(node);
        if let Some(codata) = codata {
            let protocol = self.protocols.codata(&self.tycker.statics, codata);
            self.core.compu_protocols.insert_new(id, protocol);
        } else if is_fix {
            let protocol = self.protocols.stack(&self.tycker.statics, ty);
            self.core.compu_protocols.insert_new(id, protocol);
        }
        if !span.is_dummy() {
            self.core.compu_spans.insert_new(id, span);
        }
        id
    }

    pub(super) fn variable(
        &mut self, pat: Option<VPatId>, ty: TypeId, scope: usize,
    ) -> (ir::VPatId, StaticValue) {
        self.named_variable(None, pat, ty, scope)
    }

    pub(super) fn named_var_only(
        &mut self, name: Option<VarName>, ty: TypeId, scope: usize,
    ) -> (ir::VarId, StaticValue) {
        let var = self.core.vars.alloc(name.unwrap_or_else(|| VarName::new("__static_value__")));
        let source = ValueId::from_index(0);
        let val = self.make_value(source, ty, scope, ValueForm::RuntimeVar(var));
        (var, val)
    }

    pub(super) fn named_variable(
        &mut self, name: Option<VarName>, pat: Option<VPatId>, ty: TypeId, scope: usize,
    ) -> (ir::VPatId, StaticValue) {
        let (var, val) = self.named_var_only(name, ty, scope);
        let span = pat.map_or_else(Span::dummy, |pat| self.pat_span(pat));
        let ir_pat = self.alloc_core_vpat(span, ir::ValuePattern::Var(var), ty);
        (ir_pat, val)
    }

    pub(super) fn wrap_value_bindings(
        &mut self, bindings: Vec<CoreBinding>, mut tail: ir::ValueId,
    ) -> ir::ValueId {
        let tail_protocol = self.core.value_protocols.get(tail).cloned();
        for CoreBinding { binder, bindee, span } in bindings.into_iter().rev() {
            let id = self.core.values.alloc(ir::Value::Let(Let { binder, bindee, tail }));
            if let Some(protocol) = &tail_protocol {
                self.core.value_protocols.insert_new(id, protocol.clone());
            }
            if !span.is_dummy() {
                self.core.value_spans.insert_new(id, span);
            }
            tail = id;
        }
        tail
    }

    pub(super) fn wrap_compu_bindings(
        &mut self, bindings: Vec<CoreBinding>, mut tail: ir::CompuId,
    ) -> ir::CompuId {
        let tail_protocol = self.core.compu_protocols.get(tail).cloned();
        for CoreBinding { binder, bindee, span } in bindings.into_iter().rev() {
            let id = self.core.compus.alloc(ir::Computation::Let(Let { binder, bindee, tail }));
            if let Some(protocol) = &tail_protocol {
                self.core.compu_protocols.insert_new(id, protocol.clone());
            }
            if !span.is_dummy() {
                self.core.compu_spans.insert_new(id, span);
            }
            tail = id;
        }
        tail
    }

    pub(super) fn reify(&mut self, value: &StaticValue) -> Result<ir::ValueId> {
        zydeco_foundation::stack::maybe_grow(|| self.reify_inner(value))
    }

    fn reify_inner(&mut self, value: &StaticValue) -> Result<ir::ValueId> {
        if self.purpose == Purpose::Inspect {
            return Err(ReportedError);
        }
        if !self.runtime_type(value.0.ty) {
            return self.fail(StaticEliminationError::RuntimeValue {
                value: value.0.source,
                ty: value.0.ty,
            });
        }
        if let Some(share_id) = value.0.shared {
            if let Some(var) = self.shares[share_id].materialized {
                return Ok(self.alloc_core_value(value.0.source, ir::Value::Var(var), value.0.ty));
            }
            return self.materialize_share(share_id, value.0.source, value.0.ty);
        }
        self.reify_unshared(value)
    }

    fn materialize_share(
        &mut self, share_id: ShareId, use_site: ValueId, use_ty: TypeId,
    ) -> Result<ir::ValueId> {
        let (owner_scope, span, inner) = {
            let slot = &self.shares[share_id];
            (slot.owner_scope, slot.span, slot.value.clone())
        };
        let bindee = self.reify_unshared(&inner)?;
        let var = self.core.vars.alloc(VarName::new("__static_value__"));
        let binder = self.alloc_core_vpat(Span::dummy(), ir::ValuePattern::Var(var), inner.0.ty);
        self.scopes[owner_scope].bindings.push(CoreBinding { binder, bindee, span });
        self.shares[share_id].materialized = Some(var);
        Ok(self.alloc_core_value(use_site, ir::Value::Var(var), use_ty))
    }

    fn reify_unshared(&mut self, value: &StaticValue) -> Result<ir::ValueId> {
        let node = match &value.0.form {
            | ValueForm::Function { .. } => {
                return self.fail(StaticEliminationError::RuntimeValue {
                    value: value.0.source,
                    ty: value.0.ty,
                });
            }
            | ValueForm::Hole { preallocated } => {
                if let Some(id) = preallocated.take() {
                    return Ok(id);
                }
                ir::Value::Hole(Hole)
            }
            | ValueForm::Triv => ir::Value::Triv(Triv),
            | ValueForm::Lit(literal) => ir::Value::Lit(literal.clone()),
            | ValueForm::IntrinsicOp(operation) => ir::Value::IntrinsicOp(*operation),
            | ValueForm::Foreign(import) => ir::Value::Foreign(import.clone()),
            | ValueForm::RuntimeVar(var) => ir::Value::Var(*var),
            | ValueForm::RuntimeTerm(id) => {
                return Ok(id.take().expect("unshared runtime term cannot be reified twice"));
            }
            | ValueForm::Thunk { body, env } => {
                return self.reify_thunk(value.0.source, value.0.ty, *body, env);
            }
            | ValueForm::Product(fields) => {
                let layout = self.product_layout(value.0.ty)?;
                let items =
                    fields.iter().map(|field| self.reify(field)).collect::<Result<Vec<_>>>()?;
                if items.is_empty() && layout.arity == 0 {
                    ir::Value::Triv(Triv)
                } else {
                    ir::Value::VCons(IrVCons::new(items, layout))
                }
            }
            | ValueForm::Constructor { data, name, payload } => {
                let source_data = data
                    .or_else(|| self.resolve_data_id(value.0.source, value.0.ty))
                    .expect("checked constructor has a data declaration");
                let (_, tag) = self.ctor_idx(source_data, *name);
                let payload = self.reify(payload)?;
                ir::Value::Ctor { tag, payload }
            }
            | ValueForm::Package(ConsN(_, payload)) | ValueForm::Named(_, payload) => {
                return self.reify(payload);
            }
        };
        Ok(self.alloc_core_value(value.0.source, node, value.0.ty))
    }

    fn reify_thunk(
        &mut self, source: ValueId, ty: TypeId, body: CompuId, env: &Environment,
    ) -> Result<ir::ValueId> {
        let env_key = self.thunk_env_key(body, env);
        let key = (body, env_key);
        if let Some(&var) = self.memoized_thunks.get(&key) {
            return Ok(self.alloc_core_value(source, ir::Value::Var(var), ty));
        }
        let target_scope = self.thunk_max_scope(body, env).min(self.current_scope());
        let saved_purpose = self.purpose;
        self.purpose = Purpose::Residualize;
        let compu = self.computation(body, env);
        self.purpose = saved_purpose;
        let compu = compu?;
        let thunk_id = self.alloc_core_value(source, ir::Value::Thunk(Thunk(compu)), ty);
        let var = self.core.vars.alloc(VarName::new("__thunk__"));
        let binder = self.alloc_core_vpat(Span::dummy(), ir::ValuePattern::Var(var), ty);
        let span = self.term_span(source);
        self.scopes[target_scope].bindings.push(CoreBinding { binder, bindee: thunk_id, span });
        self.memoized_thunks.insert(key.clone(), var);
        self.scopes[target_scope].memo_keys.push(key);
        Ok(self.alloc_core_value(source, ir::Value::Var(var), ty))
    }

    pub(super) fn share(&mut self, value: StaticValue) -> Result<StaticValue> {
        zydeco_foundation::stack::maybe_grow(|| self.share_inner(value))
    }

    fn share_inner(&mut self, value: StaticValue) -> Result<StaticValue> {
        if self.purpose == Purpose::Inspect
            || value.0.shared.is_some()
            || matches!(
                value.0.form,
                ValueForm::Function { .. }
                    | ValueForm::Triv
                    | ValueForm::Lit(_)
                    | ValueForm::IntrinsicOp(_)
                    | ValueForm::Foreign(_)
                    | ValueForm::RuntimeVar { .. }
                    | ValueForm::Thunk { .. }
            )
        {
            return Ok(value);
        }
        // Share sub-components first so projections bypass wrapping the entire aggregate
        // while keeping element identities consistent if both aggregate and field are demanded.
        let form = match &value.0.form {
            | ValueForm::Product(fields) => ValueForm::Product(
                fields.iter().map(|field| self.share(field.clone())).collect::<Result<Vec<_>>>()?,
            ),
            | ValueForm::Constructor { data, name, payload } => ValueForm::Constructor {
                data: *data,
                name: *name,
                payload: self.share(payload.clone())?,
            },
            | ValueForm::Package(ConsN(prefix, payload)) => {
                ValueForm::Package(ConsN(prefix.clone(), self.share(payload.clone())?))
            }
            | ValueForm::Named(name, payload) => {
                ValueForm::Named(*name, self.share(payload.clone())?)
            }
            | ValueForm::Hole { .. } | ValueForm::RuntimeTerm(_) => value.0.form.clone(),
            | ValueForm::Triv
            | ValueForm::Lit(_)
            | ValueForm::IntrinsicOp(_)
            | ValueForm::Foreign(_)
            | ValueForm::RuntimeVar(_)
            | ValueForm::Thunk { .. }
            | ValueForm::Function { .. } => unreachable!(),
        };
        let max_scope = match &form {
            | ValueForm::Product(fields) => {
                fields.iter().map(|field| field.0.max_scope).max().unwrap_or(0)
            }
            | ValueForm::Constructor { payload, .. }
            | ValueForm::Package(ConsN(_, payload))
            | ValueForm::Named(_, payload) => payload.0.max_scope,
            | _ => value.0.max_scope,
        };
        let inner = StaticValue(Rc::new(ValueInfo {
            uid: self.next_uid,
            source: value.0.source,
            ty: value.0.ty,
            max_scope,
            form,
            shared: None,
        }));
        self.next_uid += 1;
        if inner.is_runtime() && self.runtime_type(inner.0.ty) {
            let owner_scope = self.current_scope();
            let span = self.term_span(inner.0.source);
            let share_id = self.shares.alloc(ShareSlot {
                owner_scope,
                span,
                value: inner.clone(),
                materialized: None,
            });
            let uid = self.next_uid;
            self.next_uid += 1;
            Ok(StaticValue(Rc::new(ValueInfo {
                uid,
                source: inner.0.source,
                ty: inner.0.ty,
                max_scope: owner_scope,
                form: inner.0.form.clone(),
                shared: Some(share_id),
            })))
        } else {
            Ok(inner)
        }
    }

    fn thunk_env_key(&mut self, body: CompuId, env: &Environment) -> Arc<EnvKey> {
        let free = self.compu_free_defs(body);
        let values = free
            .iter()
            .filter_map(|def| env.values.get(def).map(|value| (*def, value.0.uid)))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        Arc::new(EnvKey { types: env.types.clone(), values })
    }

    pub(super) fn thunk_max_scope(&mut self, body: CompuId, env: &Environment) -> usize {
        let free = self.compu_free_defs(body);
        free.iter()
            .filter_map(|def| env.values.get(def).map(|value| value.0.max_scope))
            .max()
            .unwrap_or(0)
    }

    pub(super) fn function_max_scope(
        &mut self, body: ValueId, binder: &ValBinder, env: &Environment,
    ) -> usize {
        let free = self.value_free_defs(body);
        let mut bound = FxHashSet::default();
        if let ValBinder::Value(pat) = binder {
            self.collect_vpat_bound(*pat, &mut FxHashSet::default(), &mut bound);
        }
        free.iter()
            .filter(|def| !bound.contains(*def))
            .filter_map(|def| env.values.get(def).map(|value| value.0.max_scope))
            .max()
            .unwrap_or(0)
    }

    fn compu_free_defs(&mut self, compu: CompuId) -> Arc<[DefId]> {
        if let Some(cached) = self.thunk_free_defs.get(&compu) {
            return cached.clone();
        }
        let mut collector = FreeDefCollector {
            statics: &self.tycker.statics,
            visited_compus: FxHashSet::default(),
            visited_values: FxHashSet::default(),
            visited_vpats: FxHashSet::default(),
            refs: Vec::new(),
            bound: FxHashSet::default(),
        };
        collector.visit_compu(compu);
        let result = collector.finish();
        self.thunk_free_defs.insert(compu, result.clone());
        result
    }

    fn value_free_defs(&mut self, value: ValueId) -> Arc<[DefId]> {
        if let Some(cached) = self.value_free_defs.get(&value) {
            return cached.clone();
        }
        let mut collector = FreeDefCollector {
            statics: &self.tycker.statics,
            visited_compus: FxHashSet::default(),
            visited_values: FxHashSet::default(),
            visited_vpats: FxHashSet::default(),
            refs: Vec::new(),
            bound: FxHashSet::default(),
        };
        collector.visit_value(value);
        let result = collector.finish();
        self.value_free_defs.insert(value, result.clone());
        result
    }

    fn collect_vpat_bound(
        &self, pat: VPatId, visited: &mut FxHashSet<VPatId>, bound: &mut FxHashSet<DefId>,
    ) {
        let mut collector = FreeDefCollector {
            statics: &self.tycker.statics,
            visited_compus: FxHashSet::default(),
            visited_values: FxHashSet::default(),
            visited_vpats: std::mem::take(visited),
            refs: Vec::new(),
            bound: std::mem::take(bound),
        };
        collector.visit_vpat(pat);
        *visited = collector.visited_vpats;
        *bound = collector.bound;
    }
}

struct FreeDefCollector<'a> {
    statics: &'a crate::arena::StaticsArena,
    visited_compus: FxHashSet<CompuId>,
    visited_values: FxHashSet<ValueId>,
    visited_vpats: FxHashSet<VPatId>,
    refs: Vec<DefId>,
    bound: FxHashSet<DefId>,
}

impl FreeDefCollector<'_> {
    fn finish(self) -> Arc<[DefId]> {
        let bound = self.bound;
        let mut free: Vec<DefId> =
            self.refs.into_iter().filter(|def| !bound.contains(def)).collect();
        free.sort_unstable();
        free.dedup();
        Arc::from(free)
    }

    fn visit_vpat(&mut self, pat: VPatId) {
        if !self.visited_vpats.insert(pat) {
            return;
        }
        zydeco_foundation::stack::maybe_grow(|| match &self.statics.vpats[&pat] {
            | ValuePattern::Var(def) => {
                self.bound.insert(*def);
            }
            | ValuePattern::Hole(_) | ValuePattern::Triv(_) | ValuePattern::Lit(_) => {}
            | ValuePattern::Named(Named(_, inner))
            | ValuePattern::Ctor(Ctor(_, inner))
            | ValuePattern::SCons(ConsN(_, inner)) => self.visit_vpat(*inner),
            | ValuePattern::Alias(Alias(ConsN(head, tail))) => {
                for pat in head {
                    self.visit_vpat(*pat);
                }
                self.visit_vpat(*tail);
            }
            | ValuePattern::VCons(items) => {
                for pat in items {
                    self.visit_vpat(*pat);
                }
            }
            | ValuePattern::View(view) => {
                self.visit_value(view.function);
                self.visit_vpat(view.pattern);
            }
        });
    }

    fn visit_value(&mut self, value: ValueId) {
        if !self.visited_values.insert(value) {
            return;
        }
        zydeco_foundation::stack::maybe_grow(|| match &self.statics.values[&value] {
            | Value::Var(def) => self.refs.push(*def),
            | Value::Hole(_) | Value::Triv(_) | Value::Lit(_) | Value::IntrinsicOp(_) => {}
            | Value::Named(Named(_, inner))
            | Value::Ctor(Ctor(_, inner))
            | Value::SCons(ConsN(_, inner))
            | Value::Seal(inner)
            | Value::Proj(Proj(inner, _)) => self.visit_value(*inner),
            | Value::Thunk(Thunk(body)) => self.visit_compu(*body),
            | Value::VCons(fields) => {
                for field in fields {
                    self.visit_value(*field);
                }
            }
            | Value::ValAbs(Abs(binder, body)) => {
                if let ValBinder::Value(pat) = binder {
                    self.visit_vpat(*pat);
                }
                self.visit_value(*body);
            }
            | Value::ValApp(App(function, argument)) => {
                self.visit_value(*function);
                if let ValArgument::Value(argument) = argument {
                    self.visit_value(*argument);
                }
            }
            | Value::Let(Let { binder, bindee, tail }) => {
                self.visit_vpat(*binder);
                self.visit_value(*bindee);
                self.visit_value(*tail);
            }
            | Value::Match(Match { scrut, arms }) => {
                self.visit_value(*scrut);
                for arm in arms {
                    self.visit_vpat(arm.binder);
                    self.visit_value(arm.tail);
                }
            }
            | Value::IntOp(IntValueOp { operands, .. }) => {
                for operand in operands {
                    self.visit_value(*operand);
                }
            }
        });
    }

    fn visit_compu(&mut self, compu: CompuId) {
        if !self.visited_compus.insert(compu) {
            return;
        }
        zydeco_foundation::stack::maybe_grow(|| match &self.statics.compus[&compu] {
            | Computation::Hole(_) => {}
            | Computation::VAbs(Abs(binder, body)) | Computation::Fix(Fix(binder, body)) => {
                self.visit_vpat(*binder);
                self.visit_compu(*body);
            }
            | Computation::TAbs(Abs(_, body))
            | Computation::TApp(App(body, _))
            | Computation::Dtor(Dtor(body, _)) => self.visit_compu(*body),
            | Computation::VApp(App(function, argument)) => {
                self.visit_compu(*function);
                self.visit_value(*argument);
            }
            | Computation::Force(Force(value)) | Computation::Ret(Return(value)) => {
                self.visit_value(*value);
            }
            | Computation::Do(Bind { binder, bindee, tail }) => {
                self.visit_vpat(*binder);
                self.visit_compu(*bindee);
                self.visit_compu(*tail);
            }
            | Computation::Let(Let { binder, bindee, tail }) => {
                self.visit_vpat(*binder);
                self.visit_value(*bindee);
                self.visit_compu(*tail);
            }
            | Computation::Match(Match { scrut, arms }) => {
                self.visit_value(*scrut);
                for arm in arms {
                    self.visit_vpat(arm.binder);
                    self.visit_compu(arm.tail);
                }
            }
            | Computation::CoMatch(CoMatch { arms }) => {
                for arm in arms {
                    self.visit_compu(arm.tail);
                }
            }
        });
    }
}
