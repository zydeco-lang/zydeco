//! Interface extraction for accepted compilation units.

use super::*;

pub(super) struct InterfaceExtractor<'a> {
    pub(super) ctx: &'a ExtractContext<'a>,
    pub(super) out: &'a mut IfTables,
}

impl InterfaceExtractor<'_> {
    pub(super) fn extract_root(mut self, root: TermAnnId) -> RelTermAnnId {
        let rel_root = match root {
            | TermAnnId::Hole(_) => unreachable!("accepted unit root cannot be a hole"),
            | TermAnnId::Kind(k) => RelTermAnnId::Kind(self.kind_id(k)),
            | TermAnnId::Type(t, k) => RelTermAnnId::Type(self.type_id(t), self.kind_id(k)),
            | TermAnnId::Value(v, t) => RelTermAnnId::Value(self.value_id(v), self.type_id(t)),
            | TermAnnId::Compu(c, t) => {
                let slot = self.out.thunk_bodies.len() as u32;
                self.out.thunk_bodies.push(c);
                let rel_ty = self.type_id(t);
                self.out.thunk_types.push(rel_ty);
                RelTermAnnId::CompuStub(slot, rel_ty)
            }
        };
        self.drain_pending();
        rel_root
    }

    fn drain_pending(&mut self) {
        loop {
            while let Some((slot, def)) = self.out.pending_defs.pop_front() {
                let annotation =
                    self.ctx.statics.annotations_var.get(&def).copied().map(|a| self.ann_id(a));
                let value_alias =
                    self.ctx.statics.value_aliases.get(&def).copied().map(|v| self.value_id(v));
                let package_alias = self
                    .ctx
                    .statics
                    .package_aliases
                    .get(&def)
                    .map(|ws| ws.iter().copied().map(|w| self.static_term_id(w)).collect());
                let type_definition =
                    self.ctx.statics.type_definitions.get(&def).copied().map(|t| self.type_id(t));
                let inlinable =
                    self.ctx.statics.inlinables.get(&def).copied().map(|v| self.value_id(v));
                let global = self.ctx.statics.global_defs.get(&def).is_some();
                self.out.def_facts.push((
                    slot,
                    RelDefFacts {
                        annotation,
                        value_alias,
                        package_alias,
                        type_definition,
                        inlinable,
                        global,
                    },
                ));
            }
            if let Some((slot, abst)) = self.out.pending_absts.pop_front() {
                let s = self.ctx.statics;
                let kind = self.kind_id(s.annotations_abst[&abst]);
                let seal = s.seals.get(abst).copied().map(|t| self.type_id(t));
                let opaque_representation =
                    s.opaque_representations.get(abst).copied().map(|t| self.type_id(t));
                let hint = s.abst_hints.get(abst).copied().map(|d| self.def_id(d));
                let existential_skolem = s.existential_skolems.contains(abst);
                let builtin_role = s.builtin_roles.witness(abst);
                self.out.absts[slot as usize] = Some(RelAbstEntry {
                    kind,
                    seal,
                    opaque_representation,
                    hint,
                    existential_skolem,
                    builtin_role,
                });
            }
            if self.out.pending_defs.is_empty() && self.out.pending_absts.is_empty() {
                break;
            }
        }
    }

    fn def_id(&mut self, def: su::DefId) -> RelDefId {
        if let Some(&dep) = self.ctx.dep_def_spaces.get(&def.key_space()) {
            return RelDefId::DepInterface { dep, offset: def.raw().into_u32() };
        }
        if let Some(&slot) = self.out.def_map.get(&def) {
            return RelDefId::LocalInterface(slot);
        }
        let slot = self.out.local_defs.len() as u32;
        let name = *self.ctx.statics.def_name(self.ctx.scoped, &def);
        self.out.def_map.insert(def, slot);
        self.out.local_defs.push((def, name));
        self.out.pending_defs.push_back((slot, def));
        RelDefId::LocalInterface(slot)
    }

    fn ann_id(&mut self, ann: AnnId) -> RelAnnId {
        match ann {
            | AnnId::Set => RelAnnId::Set,
            | AnnId::Kind(k) => RelAnnId::Kind(self.kind_id(k)),
            | AnnId::Type(t) => RelAnnId::Type(self.type_id(t)),
        }
    }

    fn static_term_id(&mut self, term: StaticTermId) -> RelStaticTermId {
        match term {
            | StaticTermId::Kind(k) => RelStaticTermId::Kind(self.kind_id(k)),
            | StaticTermId::Type(t) => RelStaticTermId::Type(self.type_id(t)),
        }
    }

    fn static_pat_id(&mut self, pat: StaticPatId) -> RelStaticPatId {
        match pat {
            | StaticPatId::Kind(k) => RelStaticPatId::Kind(self.kpat_id(k)),
            | StaticPatId::Type(t) => RelStaticPatId::Type(self.tpat_id(t)),
        }
    }

    fn kind_id(&mut self, id: KindId) -> RelId<KindId> {
        let idx = id.index();
        if idx < INTRINSIC_KIND_COUNT {
            return RelId::Intrinsic(id);
        }
        if idx < self.ctx.local_base.kinds {
            return find_dep_id(self.ctx.dep_ranges, idx, |r| r.kinds);
        }
        if let Some(&slot) = self.out.kind_map.get(&id) {
            return RelId::LocalInterface(slot);
        }
        let slot = self.out.kinds.len() as u32;
        self.out.kinds.push(None);
        self.out.kind_map.insert(id, slot);
        let node = self
            .ctx
            .statics
            .normalized_kind_at(id)
            .expect("accepted interface kind must be normalized")
            .clone();
        let rel = zydeco_foundation::stack::maybe_grow(|| match node {
            | Kind::VType(_) => RelKind::VType,
            | Kind::CType(_) => RelKind::CType,
            | Kind::Arrow(Arrow(a, b)) => RelKind::Arrow(self.kind_id(a), self.kind_id(b)),
            | Kind::Label(Label(name, inner)) => RelKind::Label(name, self.kind_id(inner)),
        });
        self.out.kinds[slot as usize] = Some(rel);
        if self.ctx.statics.global_terms.contains(&TermId::Kind(id)) {
            self.out.global_terms.push(RelTermId::Kind(RelId::LocalInterface(slot)));
        }
        RelId::LocalInterface(slot)
    }

    fn kpat_id(&mut self, id: KPatId) -> RelId<KPatId> {
        let idx = id.index();
        if idx < self.ctx.local_base.kpats {
            return find_dep_id(self.ctx.dep_ranges, idx, |r| r.kpats);
        }
        if let Some(&slot) = self.out.kpat_map.get(&id) {
            return RelId::LocalInterface(slot);
        }
        let slot = self.out.kpats.len() as u32;
        self.out.kpats.push(None);
        self.out.kpat_map.insert(id, slot);
        let rel = match &self.ctx.statics.kpats[&id] {
            | KindPattern::Hole(_) => RelKindPattern::Hole,
            | KindPattern::Var(def) => RelKindPattern::Var(self.def_id(*def)),
        };
        self.out.kpats[slot as usize] = Some(rel);
        RelId::LocalInterface(slot)
    }

    fn tpat_id(&mut self, id: TPatId) -> RelId<TPatId> {
        let idx = id.index();
        if idx < self.ctx.local_base.tpats {
            return find_dep_id(self.ctx.dep_ranges, idx, |r| r.tpats);
        }
        if let Some(&slot) = self.out.tpat_map.get(&id) {
            return RelId::LocalInterface(slot);
        }
        let slot = self.out.tpats.len() as u32;
        self.out.tpats.push(None);
        self.out.tpat_map.insert(id, slot);
        let pat = self.ctx.statics.tpats[&id].clone();
        let kind = self.kind_id(self.ctx.statics.annotations_tpat[&id]);
        let rel = zydeco_foundation::stack::maybe_grow(|| match pat {
            | TypePattern::Hole(_) => RelTypePattern::Hole,
            | TypePattern::Var(def) => RelTypePattern::Var(self.def_id(def)),
            | TypePattern::Named(Named(name, inner)) => {
                RelTypePattern::Named(name, self.tpat_id(inner))
            }
        });
        self.out.tpats[slot as usize] = Some((rel, kind));
        RelId::LocalInterface(slot)
    }

    fn type_binder(&mut self, binder: TypeBinder) -> RelTypeBinder {
        RelTypeBinder {
            pattern: self.tpat_id(binder.pattern),
            witness: self.abst_id(binder.witness),
        }
    }

    fn abst_id(&mut self, id: AbstId) -> RelId<AbstId> {
        let idx = id.index();
        if idx < self.ctx.local_base.absts {
            return find_dep_id(self.ctx.dep_ranges, idx, |r| r.absts);
        }
        if let Some(&slot) = self.out.abst_map.get(&id) {
            return RelId::LocalInterface(slot);
        }
        let slot = self.out.absts.len() as u32;
        self.out.absts.push(None);
        self.out.abst_map.insert(id, slot);
        self.out.pending_absts.push_back((slot, id));
        RelId::LocalInterface(slot)
    }

    fn data_id(&mut self, id: DataId) -> RelId<DataId> {
        let idx = id.index();
        if idx < self.ctx.local_base.datas {
            return find_dep_id(self.ctx.dep_ranges, idx, |r| r.datas);
        }
        if let Some(&slot) = self.out.data_map.get(&id) {
            return RelId::LocalInterface(slot);
        }
        let slot = self.out.datas.len() as u32;
        self.out.datas.push(None);
        self.out.data_map.insert(id, slot);
        let arms = zydeco_foundation::stack::maybe_grow(|| {
            self.ctx.statics.datas[&id]
                .iter()
                .map(|(ctor, param)| (*ctor, self.type_id(*param)))
                .collect()
        });
        self.out.datas[slot as usize] = Some(RelData { arms });
        RelId::LocalInterface(slot)
    }

    fn codata_id(&mut self, id: CoDataId) -> RelId<CoDataId> {
        let idx = id.index();
        if idx < self.ctx.local_base.codatas {
            return find_dep_id(self.ctx.dep_ranges, idx, |r| r.codatas);
        }
        if let Some(&slot) = self.out.codata_map.get(&id) {
            return RelId::LocalInterface(slot);
        }
        let slot = self.out.codatas.len() as u32;
        self.out.codatas.push(None);
        self.out.codata_map.insert(id, slot);
        let arms = zydeco_foundation::stack::maybe_grow(|| {
            self.ctx.statics.codatas[&id]
                .iter()
                .map(|(dtor, out_ty)| (*dtor, self.type_id(*out_ty)))
                .collect()
        });
        self.out.codatas[slot as usize] = Some(RelCoData { arms });
        RelId::LocalInterface(slot)
    }

    fn type_id(&mut self, id: TypeId) -> RelId<TypeId> {
        let idx = id.index();
        if idx < self.ctx.intrinsic_types {
            return RelId::Intrinsic(id);
        }
        if idx < self.ctx.local_base.types {
            return find_dep_id(self.ctx.dep_ranges, idx, |r| r.types);
        }
        if let Some(&slot) = self.out.type_map.get(&id) {
            return RelId::LocalInterface(slot);
        }
        let slot = self.out.types.len() as u32;
        self.out.types.push(None);
        self.out.type_map.insert(id, slot);
        let kind = self.kind_id(self.ctx.statics.type_kind(id));
        let ty = self
            .ctx
            .statics
            .normalized_at(id)
            .expect("accepted interface type must be normalized")
            .clone();
        let rel = zydeco_foundation::stack::maybe_grow(|| self.rel_type(ty));
        self.out.types[slot as usize] = Some((rel, kind));
        if let Some(role) = self.ctx.statics.builtin_roles.value(id) {
            self.out.value_builtin_roles.push((slot, role));
        }
        if self.ctx.statics.global_terms.contains(&TermId::Type(id)) {
            self.out.global_terms.push(RelTermId::Type(RelId::LocalInterface(slot)));
        }
        RelId::LocalInterface(slot)
    }

    fn rel_type(&mut self, ty: Type) -> RelType {
        match ty {
            | Type::Var(def) => RelType::Var(self.def_id(def)),
            | Type::Abst(abst) => RelType::Abst(self.abst_id(abst)),
            | Type::Abs(TypeAbstraction { binder, body }) => {
                RelType::Abs { binder: self.type_binder(binder), body: self.type_id(body) }
            }
            | Type::App(App(f, a)) => RelType::App(self.type_id(f), self.type_id(a)),
            | Type::Named(Named(n, inner)) => RelType::Named(n, self.type_id(inner)),
            | Type::Label(Label(n, inner)) => RelType::Label(n, self.type_id(inner)),
            | Type::Proj(Proj(head, name)) => RelType::Proj(self.type_id(head), name),
            | Type::Thk(_) => RelType::Thk,
            | Type::Ret(_) => RelType::Ret,
            | Type::Unit(_) => RelType::Unit,
            | Type::Opaque(_) => RelType::Opaque,
            | Type::Primitive(PrimitiveTy(p)) => RelType::Primitive(p),
            | Type::OS(_) => RelType::OS,
            | Type::ValPi(pi) => {
                let binder = match pi.binder {
                    | ValPiBinder::Type(b) => RelValPiBinder::Type(self.type_binder(b)),
                    | ValPiBinder::Value(p) => RelValPiBinder::Value(RelValueParameter {
                        domain: self.type_id(p.domain),
                        witnesses: p
                            .witnesses
                            .map(|ws| ws.iter().copied().map(|a| self.abst_id(a)).collect()),
                        witness_projection: p.witness_projection,
                    }),
                };
                RelType::ValPi(Box::new(RelValPi { binder, codomain: self.type_id(pi.codomain) }))
            }
            | Type::Arrow(Arrow(a, b)) => RelType::Arrow(self.type_id(a), self.type_id(b)),
            | Type::Forall(Forall(b, body)) => {
                RelType::Forall(self.type_binder(b), self.type_id(body))
            }
            | Type::PackPi(pi) => RelType::PackPi(Box::new(RelPackPi {
                domain: self.type_id(pi.domain),
                witnesses: pi.witnesses.iter().copied().map(|a| self.abst_id(a)).collect(),
                codomain: self.type_id(pi.codomain),
            })),
            | Type::Prod(Prod(items)) => {
                RelType::Prod(items.into_iter().map(|t| self.type_id(t)).collect())
            }
            | Type::Exists(exists) => RelType::Exists {
                binder: self.type_binder(exists.binder),
                mode: match exists.mode {
                    | ExistsMode::Abstract => RelExistsMode::Abstract,
                    | ExistsMode::Manifest(d) => RelExistsMode::Manifest(self.type_id(d)),
                },
                body: self.type_id(exists.body),
            },
            | Type::ManifestKind(mk) => RelType::ManifestKind {
                binder: self.kpat_id(mk.binder),
                definition: self.kind_id(mk.definition),
                body: self.type_id(mk.body),
            },
            | Type::Data(d) => RelType::Data(self.data_id(d)),
            | Type::CoData(c) => RelType::CoData(self.codata_id(c)),
        }
    }

    fn vpat_id(&mut self, id: VPatId) -> RelId<VPatId> {
        let idx = id.index();
        if idx < self.ctx.local_base.vpats {
            return find_dep_id(self.ctx.dep_ranges, idx, |r| r.vpats);
        }
        if let Some(&slot) = self.out.vpat_map.get(&id) {
            return RelId::LocalInterface(slot);
        }
        let slot = self.out.vpats.len() as u32;
        self.out.vpats.push(None);
        self.out.vpat_map.insert(id, slot);
        let pat = self.ctx.statics.vpats[&id].clone();
        let ty = self.type_id(self.ctx.statics.annotations_vpat[&id]);
        let rel = zydeco_foundation::stack::maybe_grow(|| match pat {
            | ValuePattern::Hole(_) => RelValuePattern::Hole,
            | ValuePattern::Var(def) => RelValuePattern::Var(self.def_id(def)),
            | ValuePattern::Named(Named(f, p)) => RelValuePattern::Named(f, self.vpat_id(p)),
            | ValuePattern::Ctor(Ctor(c, p)) => RelValuePattern::Ctor(c, self.vpat_id(p)),
            | ValuePattern::Lit(l) => RelValuePattern::Lit(l),
            | ValuePattern::Alias(Alias(ps)) => {
                RelValuePattern::Alias(ps.into_iter().map(|p| self.vpat_id(p)).collect())
            }
            | ValuePattern::Triv(_) => RelValuePattern::Triv,
            | ValuePattern::VCons(ps) => {
                RelValuePattern::VCons(ps.into_iter().map(|p| self.vpat_id(p)).collect())
            }
            | ValuePattern::SCons(ConsN(sp, p)) => RelValuePattern::SCons(
                sp.into_iter().map(|x| self.static_pat_id(x)).collect(),
                self.vpat_id(p),
            ),
            | ValuePattern::View(view) => RelValuePattern::View {
                function: self.value_id(view.function),
                pattern: self.vpat_id(view.pattern),
            },
        });
        self.out.vpats[slot as usize] = Some((rel, ty));
        if let Some(&data) = self.ctx.statics.data_pat_hints.get(id) {
            let rel_data = self.data_id(data);
            self.out.data_pat_hints.push((slot, rel_data));
        }
        if let Some(&arity) = self.ctx.statics.package_pattern_opened_arity.get(id) {
            self.out.package_pattern_opened_arity.push((slot, arity));
        }
        RelId::LocalInterface(slot)
    }

    fn value_id(&mut self, id: ValueId) -> RelId<ValueId> {
        let idx = id.index();
        if idx < self.ctx.local_base.values {
            return find_dep_id(self.ctx.dep_ranges, idx, |r| r.values);
        }
        if let Some(&slot) = self.out.value_map.get(&id) {
            return RelId::LocalInterface(slot);
        }
        let slot = self.out.values.len() as u32;
        self.out.values.push(None);
        self.out.value_map.insert(id, slot);
        let val = self.ctx.statics.values[&id].clone();
        let ty = self.type_id(self.ctx.statics.annotations_value[&id]);
        let rel = zydeco_foundation::stack::maybe_grow(|| self.rel_value(val));
        self.out.values[slot as usize] = Some((rel, ty));
        if let Some(import) = self.ctx.statics.foreign_imports.get(id).cloned() {
            self.out.foreign_imports.push((slot, import));
        }
        if let Some(&data) = self.ctx.statics.data_hints.get(id) {
            let rel_data = self.data_id(data);
            self.out.data_hints.push((slot, rel_data));
        }
        if self.ctx.statics.global_terms.contains(&TermId::Value(id)) {
            self.out.global_terms.push(RelTermId::Value(RelId::LocalInterface(slot)));
        }
        RelId::LocalInterface(slot)
    }

    fn rel_value(&mut self, val: Value) -> RelValue {
        match val {
            | Value::Hole(_) => RelValue::Hole,
            | Value::Var(def) => RelValue::Var(self.def_id(def)),
            | Value::Named(Named(f, inner)) => RelValue::Named(f, self.value_id(inner)),
            | Value::Let(Let { binder, bindee, tail }) => RelValue::Let {
                binder: self.vpat_id(binder),
                bindee: self.value_id(bindee),
                tail: self.value_id(tail),
            },
            | Value::ValAbs(Abs(binder, body)) => {
                let binder = match binder {
                    | ValBinder::Type(t) => RelValBinder::Type(self.tpat_id(t)),
                    | ValBinder::Value(p) => RelValBinder::Value(self.vpat_id(p)),
                };
                RelValue::ValAbs(binder, self.value_id(body))
            }
            | Value::ValApp(App(f, a)) => {
                let f = self.value_id(f);
                let a = match a {
                    | ValArgument::Type(t) => RelValArgument::Type(self.type_id(t)),
                    | ValArgument::Value(v) => RelValArgument::Value(self.value_id(v)),
                };
                RelValue::ValApp(f, a)
            }
            | Value::Match(Match { scrut, arms }) => RelValue::Match {
                scrut: self.value_id(scrut),
                arms: arms
                    .into_iter()
                    .map(|Matcher { binder, tail }| (self.vpat_id(binder), self.value_id(tail)))
                    .collect(),
            },
            | Value::IntOp(IntValueOp { operation, operands: [a, b] }) => {
                RelValue::IntOp { operation, operands: [self.value_id(a), self.value_id(b)] }
            }
            | Value::IntrinsicOp(op) => RelValue::IntrinsicOp(op),
            | Value::Thunk(Thunk(compu)) => {
                // Abstract computation bodies behind a typed stub so body edits preserve `UnitInterface`.
                let stub = self.out.thunk_bodies.len() as u32;
                self.out.thunk_bodies.push(compu);
                let compu_ty = self.type_id(self.ctx.statics.annotations_compu[&compu]);
                self.out.thunk_types.push(compu_ty);
                RelValue::ThunkStub(stub)
            }
            | Value::Ctor(Ctor(c, v)) => RelValue::Ctor(c, self.value_id(v)),
            | Value::Triv(_) => RelValue::Triv,
            | Value::VCons(vs) => {
                RelValue::VCons(vs.into_iter().map(|v| self.value_id(v)).collect())
            }
            | Value::SCons(ConsN(st, v)) => RelValue::SCons(
                st.into_iter().map(|x| self.static_term_id(x)).collect(),
                self.value_id(v),
            ),
            | Value::Seal(v) => RelValue::Seal(self.value_id(v)),
            | Value::Proj(Proj(v, field)) => RelValue::Proj(
                self.value_id(v),
                RelResolvedField {
                    name: field.name,
                    products: field
                        .target
                        .products
                        .into_iter()
                        .map(|step| (self.type_id(step.product), step.position))
                        .collect(),
                },
            ),
            | Value::Lit(lit) => RelValue::Lit(lit),
        }
    }
}
