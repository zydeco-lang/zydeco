//! Full unit-body extraction for compilation, diagnostics, and editor facts.

use super::*;

pub(super) struct BodyDefFacts {
    value_aliases: Vec<(RelDefId, RelId<ValueId>)>,
    package_aliases: Vec<(RelDefId, Vec<RelStaticTermId>)>,
    type_definitions: Vec<(RelDefId, RelId<TypeId>)>,
    inlinables: Vec<(RelDefId, RelId<ValueId>)>,
    global_defs: Vec<RelDefId>,
    annotations_var: Vec<(RelDefId, RelAnnId)>,
    def_hints: Vec<(RelTermId, RelDefId)>,
    global_terms: Vec<RelTermId>,
}

pub(super) struct BodyExtractor<'a> {
    pub(super) ctx: &'a ExtractContext<'a>,
    pub(super) if_tables: &'a IfTables,
    pub(super) maps: &'a BodyIdMaps,
}

impl BodyExtractor<'_> {
    pub(super) fn extract(self, tycker: &Tycker<'_>, checked_root: Option<TermAnnId>) -> UnitBody {
        let s = self.ctx.statics;
        let b = &self.ctx.local_base;
        let mut seen_body_defs = FxHashSet::default();
        let mut body_defs = Vec::new();
        let mut record_def = |def: su::DefId| -> RelDefId {
            if let Some(&slot) = self.if_tables.def_map.get(&def) {
                return RelDefId::LocalInterface(slot);
            }
            if let Some(&dep) = self.ctx.dep_def_spaces.get(&def.key_space()) {
                return RelDefId::DepInterface { dep, offset: def.raw().into_u32() };
            }
            if seen_body_defs.insert(def) {
                body_defs.push((def, *s.def_name(self.ctx.scoped, &def)));
            }
            RelDefId::Raw(def)
        };

        let root = checked_root.map(|r| self.rel_term_ann(r));
        let thunk_bodies = self.if_tables.thunk_bodies.iter().map(|c| self.compu(*c)).collect();
        let kinds = self.extract_kinds(&b.kinds);
        let kinds_normalized = self.extract_kind_norms(&b.kinds);
        let kpats = self.extract_kpats(&mut record_def);
        let tpats = self.extract_tpats(&mut record_def);
        let types = self.extract_types(&mut record_def);
        let types_normalized = self.extract_type_norms(&b.types, &mut record_def);
        let vpats = self.extract_vpats(&mut record_def);
        let values = self.extract_values(&mut record_def);
        let compus = self.extract_compus();
        let absts = self.extract_absts(&mut record_def);
        let datas = self.extract_datas();
        let codatas = self.extract_codatas();
        let fills = s.fills.values()[b.fills..].to_vec();
        let solus = self.extract_solus();
        let fill_scopes = self.extract_fill_scopes();
        let fill_hints = (b.fills..s.fills.len())
            .filter_map(|idx| {
                let f = FillId::from_index(idx);
                s.fill_hints.contains(f).then(|| self.fill(f))
            })
            .collect();

        let (
            value_builtin_roles,
            foreign_imports,
            data_hints,
            data_pat_hints,
            codata_hints,
            copattern_matches,
            copattern_pack_pi_binders,
            package_pattern_opened_arity,
        ) = self.extract_node_metadata();
        let def_facts = self.extract_body_def_facts(&mut record_def);
        let generated_defs = s
            .generated_defs
            .iter()
            .filter(|(def, _)| !self.ctx.dep_def_spaces.contains_key(&def.key_space()))
            .map(|(def, name)| (*def, *name))
            .collect();
        let pat_provenance = self.extract_pat_provenance();
        let term_provenance = self.extract_term_provenance();
        let term_facts = s
            .term_facts
            .iter()
            .map(|(term, facts)| (term, self.rel_term_ann(facts.annotation())))
            .collect();
        let coverage_errors = s.coverage_errors.iter().map(|c| self.rel_coverage(c)).collect();
        let observations = tycker.observations.iter().map(|o| self.rel_observation(o)).collect();
        let errors = tycker
            .errors
            .iter()
            .map(|entry| self.rel_error_entry(entry, &mut record_def))
            .collect();
        let monadic_families = tycker
            .monadic_families
            .iter()
            .filter(|(_, fam)| fam.constructor.index() >= b.types)
            .map(|(abst, fam)| {
                (
                    self.abst(*abst),
                    RelNominalFamily {
                        constructor: self.ty(fam.constructor),
                        dependencies: fam
                            .dependencies
                            .iter()
                            .copied()
                            .map(|a| self.abst(a))
                            .collect(),
                        method: fam.method,
                    },
                )
            })
            .collect();

        UnitBody {
            root,
            interface_def_map: self.if_tables.local_defs.iter().map(|(d, _)| *d).collect(),
            thunk_bodies,
            kinds,
            kinds_normalized,
            kpats,
            tpats,
            types,
            types_normalized,
            vpats,
            values,
            compus,
            absts,
            datas,
            codatas,
            fills,
            solus,
            fill_scopes,
            fill_hints,
            value_builtin_roles,
            foreign_imports,
            data_hints,
            data_pat_hints,
            codata_hints,
            copattern_matches,
            copattern_pack_pi_binders,
            value_aliases: def_facts.value_aliases,
            package_aliases: def_facts.package_aliases,
            package_pattern_opened_arity,
            type_definitions: def_facts.type_definitions,
            inlinables: def_facts.inlinables,
            global_defs: def_facts.global_defs,
            global_terms: def_facts.global_terms,
            def_hints: def_facts.def_hints,
            annotations_var: def_facts.annotations_var,
            body_defs,
            generated_defs,
            pat_provenance,
            term_provenance,
            term_facts,
            coverage_errors,
            observations,
            errors,
            monadic_families,
        }
    }

    fn extract_body_def_facts(
        &self, record_def: &mut impl FnMut(su::DefId) -> RelDefId,
    ) -> BodyDefFacts {
        let s = self.ctx.statics;
        let value_aliases = s
            .value_aliases
            .iter()
            .filter(|(def, _)| !self.is_dep_or_if_def(**def))
            .map(|(def, val)| (record_def(*def), self.value(*val)))
            .collect();
        let package_aliases = s
            .package_aliases
            .iter()
            .filter(|(def, _)| !self.is_dep_or_if_def(**def))
            .map(|(def, ws)| {
                (record_def(*def), ws.iter().copied().map(|w| self.static_term(w)).collect())
            })
            .collect();
        let type_definitions = s
            .type_definitions
            .iter()
            .filter(|(def, _)| !self.is_dep_or_if_def(**def))
            .map(|(def, ty)| (record_def(*def), self.ty(*ty)))
            .collect();
        let inlinables = s
            .inlinables
            .iter()
            .filter(|(def, _)| !self.is_dep_or_if_def(**def))
            .map(|(def, val)| (record_def(*def), self.value(*val)))
            .collect();
        let global_defs = s
            .global_defs
            .iter()
            .filter(|(def, _)| !self.is_dep_or_if_def(**def))
            .map(|(def, ())| record_def(*def))
            .collect();
        let annotations_var = s
            .annotations_var
            .iter()
            .filter(|(def, _)| !self.is_dep_or_if_def(**def))
            .map(|(def, ann)| (record_def(*def), self.ann(*ann)))
            .collect();
        let def_hints = s
            .def_hints
            .iter()
            .filter(|(term, _)| self.is_local_term(**term))
            .map(|(term, def)| (self.term_id(*term), record_def(*def)))
            .collect();
        let global_terms = s
            .global_terms
            .iter()
            .copied()
            .filter(|term| self.is_local_body_term(*term))
            .map(|term| self.term_id(term))
            .collect();
        BodyDefFacts {
            value_aliases,
            package_aliases,
            type_definitions,
            inlinables,
            global_defs,
            annotations_var,
            def_hints,
            global_terms,
        }
    }

    fn is_dep_or_if_def(&self, def: su::DefId) -> bool {
        self.if_tables.def_map.contains_key(&def)
            || self.ctx.dep_def_spaces.contains_key(&def.key_space())
    }

    fn is_local_term(&self, term: TermId) -> bool {
        let b = &self.ctx.local_base;
        match term {
            | TermId::Kind(id) => id.index() >= b.kinds,
            | TermId::Type(id) => id.index() >= b.types,
            | TermId::Value(id) => id.index() >= b.values,
            | TermId::Compu(id) => id.index() >= b.compus,
        }
    }

    fn is_local_body_term(&self, term: TermId) -> bool {
        if !self.is_local_term(term) {
            return false;
        }
        match term {
            | TermId::Kind(id) => !self.if_tables.kind_map.contains_key(&id),
            | TermId::Type(id) => !self.if_tables.type_map.contains_key(&id),
            | TermId::Value(id) => !self.if_tables.value_map.contains_key(&id),
            | TermId::Compu(_) => true,
        }
    }

    pub(super) fn kind(&self, id: KindId) -> RelId<KindId> {
        self.ctx.map_kind(id, Some(&self.if_tables.kind_map), Some(&self.maps.kinds))
    }
    fn kpat(&self, id: KPatId) -> RelId<KPatId> {
        self.ctx.map_kpat(id, Some(&self.if_tables.kpat_map), Some(&self.maps.kpats))
    }
    fn tpat(&self, id: TPatId) -> RelId<TPatId> {
        self.ctx.map_tpat(id, Some(&self.if_tables.tpat_map), Some(&self.maps.tpats))
    }
    pub(super) fn ty(&self, id: TypeId) -> RelId<TypeId> {
        self.ctx.map_type(id, Some(&self.if_tables.type_map), Some(&self.maps.types))
    }
    fn vpat(&self, id: VPatId) -> RelId<VPatId> {
        self.ctx.map_vpat(id, Some(&self.if_tables.vpat_map), Some(&self.maps.vpats))
    }
    pub(super) fn value(&self, id: ValueId) -> RelId<ValueId> {
        self.ctx.map_value(id, Some(&self.if_tables.value_map), Some(&self.maps.values))
    }
    pub(super) fn compu(&self, id: CompuId) -> RelId<CompuId> {
        self.ctx.map_compu(id, &self.maps.compus)
    }
    pub(super) fn abst(&self, id: AbstId) -> RelId<AbstId> {
        self.ctx.map_abst(id, Some(&self.if_tables.abst_map), Some(&self.maps.absts))
    }
    fn data(&self, id: DataId) -> RelId<DataId> {
        self.ctx.map_data(id, Some(&self.if_tables.data_map), Some(&self.maps.datas))
    }
    fn codata(&self, id: CoDataId) -> RelId<CoDataId> {
        self.ctx.map_codata(id, Some(&self.if_tables.codata_map), Some(&self.maps.codatas))
    }
    pub(super) fn fill(&self, id: FillId) -> RelId<FillId> {
        self.ctx.map_fill(id, &self.maps.fills)
    }
    pub(super) fn ann(&self, id: AnnId) -> RelAnnId {
        match id {
            | AnnId::Set => RelAnnId::Set,
            | AnnId::Kind(k) => RelAnnId::Kind(self.kind(k)),
            | AnnId::Type(t) => RelAnnId::Type(self.ty(t)),
        }
    }
    pub(super) fn rel_term_ann(&self, id: TermAnnId) -> RelTermAnnId {
        match id {
            | TermAnnId::Hole(f) => RelTermAnnId::Hole(self.fill(f)),
            | TermAnnId::Kind(k) => RelTermAnnId::Kind(self.kind(k)),
            | TermAnnId::Type(t, k) => RelTermAnnId::Type(self.ty(t), self.kind(k)),
            | TermAnnId::Value(v, t) => RelTermAnnId::Value(self.value(v), self.ty(t)),
            | TermAnnId::Compu(c, t) => RelTermAnnId::Compu(self.compu(c), self.ty(t)),
        }
    }
    pub(super) fn term_id(&self, id: TermId) -> RelTermId {
        match id {
            | TermId::Kind(k) => RelTermId::Kind(self.kind(k)),
            | TermId::Type(t) => RelTermId::Type(self.ty(t)),
            | TermId::Value(v) => RelTermId::Value(self.value(v)),
            | TermId::Compu(c) => RelTermId::Compu(self.compu(c)),
        }
    }
    pub(super) fn pat_id(&self, id: PatId) -> RelPatId {
        match id {
            | PatId::Kind(k) => RelPatId::Kind(self.kpat(k)),
            | PatId::Type(t) => RelPatId::Type(self.tpat(t)),
            | PatId::Value(v) => RelPatId::Value(self.vpat(v)),
        }
    }
    fn static_term(&self, id: StaticTermId) -> RelStaticTermId {
        match id {
            | StaticTermId::Kind(k) => RelStaticTermId::Kind(self.kind(k)),
            | StaticTermId::Type(t) => RelStaticTermId::Type(self.ty(t)),
        }
    }
    fn static_pat(&self, id: StaticPatId) -> RelStaticPatId {
        match id {
            | StaticPatId::Kind(k) => RelStaticPatId::Kind(self.kpat(k)),
            | StaticPatId::Type(t) => RelStaticPatId::Type(self.tpat(t)),
        }
    }
    fn type_binder(&self, b: TypeBinder) -> RelTypeBinder {
        RelTypeBinder { pattern: self.tpat(b.pattern), witness: self.abst(b.witness) }
    }

    fn extract_kinds(&self, start: &usize) -> Vec<RelFillable<RelKind>> {
        (*start..self.ctx.statics.kinds_pre.len())
            .filter_map(|idx| {
                let id = KindId::from_index(idx);
                if self.if_tables.kind_map.contains_key(&id) {
                    return None;
                }
                Some(match self.ctx.statics.kinds_pre[&id].clone() {
                    | Fillable::Fill(f) => RelFillable::Fill(self.fill(f)),
                    | Fillable::Done(k) => RelFillable::Done(self.convert_kind(k)),
                })
            })
            .collect()
    }

    fn extract_kind_norms(&self, start: &usize) -> Vec<(RelId<KindId>, RelKind)> {
        (*start..self.ctx.statics.kinds_pre.len())
            .filter_map(|idx| {
                let id = KindId::from_index(idx);
                if self.if_tables.kind_map.contains_key(&id) {
                    return None;
                }
                self.ctx
                    .statics
                    .kinds_normalized
                    .get(id)
                    .cloned()
                    .map(|k| (self.kind(id), self.convert_kind(k)))
            })
            .collect()
    }

    fn convert_kind(&self, k: Kind) -> RelKind {
        match k {
            | Kind::VType(_) => RelKind::VType,
            | Kind::CType(_) => RelKind::CType,
            | Kind::Arrow(Arrow(a, b)) => RelKind::Arrow(self.kind(a), self.kind(b)),
            | Kind::Label(Label(n, inner)) => RelKind::Label(n, self.kind(inner)),
        }
    }

    fn extract_kpats(&self, def_cb: &mut impl FnMut(su::DefId) -> RelDefId) -> Vec<RelKindPattern> {
        (self.ctx.local_base.kpats..self.ctx.statics.kpats.len())
            .filter_map(|idx| {
                let id = KPatId::from_index(idx);
                if self.if_tables.kpat_map.contains_key(&id) {
                    return None;
                }
                Some(match &self.ctx.statics.kpats[id] {
                    | KindPattern::Hole(_) => RelKindPattern::Hole,
                    | KindPattern::Var(def) => RelKindPattern::Var(def_cb(*def)),
                })
            })
            .collect()
    }

    fn extract_tpats(
        &self, def_cb: &mut impl FnMut(su::DefId) -> RelDefId,
    ) -> Vec<(RelTypePattern, RelId<KindId>)> {
        (self.ctx.local_base.tpats..self.ctx.statics.tpats.len())
            .filter_map(|idx| {
                let id = TPatId::from_index(idx);
                if self.if_tables.tpat_map.contains_key(&id) {
                    return None;
                }
                let pat = match &self.ctx.statics.tpats[id] {
                    | TypePattern::Hole(_) => RelTypePattern::Hole,
                    | TypePattern::Var(def) => RelTypePattern::Var(def_cb(*def)),
                    | TypePattern::Named(Named(n, inner)) => {
                        RelTypePattern::Named(*n, self.tpat(*inner))
                    }
                };
                let kind = self.kind(self.ctx.statics.annotations_tpat[id]);
                Some((pat, kind))
            })
            .collect()
    }

    fn extract_types(
        &self, def_cb: &mut impl FnMut(su::DefId) -> RelDefId,
    ) -> Vec<(RelFillable<RelType>, RelId<KindId>)> {
        (self.ctx.local_base.types..self.ctx.statics.types_pre.len())
            .filter_map(|idx| {
                let id = TypeId::from_index(idx);
                if self.if_tables.type_map.contains_key(&id) {
                    return None;
                }
                let kind = self.kind(self.ctx.statics.type_kind(id));
                let ty = match self.ctx.statics.types_pre[&id].clone() {
                    | Fillable::Fill(f) => RelFillable::Fill(self.fill(f)),
                    | Fillable::Done(t) => RelFillable::Done(self.convert_type(t, def_cb)),
                };
                Some((ty, kind))
            })
            .collect()
    }

    fn extract_type_norms(
        &self, start: &usize, def_cb: &mut impl FnMut(su::DefId) -> RelDefId,
    ) -> Vec<(RelId<TypeId>, RelType)> {
        (*start..self.ctx.statics.types_pre.len())
            .filter_map(|idx| {
                let id = TypeId::from_index(idx);
                if self.if_tables.type_map.contains_key(&id) {
                    return None;
                }
                self.ctx
                    .statics
                    .types_normalized
                    .get(id)
                    .cloned()
                    .map(|t| (self.ty(id), self.convert_type(t, def_cb)))
            })
            .collect()
    }

    fn convert_type(&self, ty: Type, def_cb: &mut impl FnMut(su::DefId) -> RelDefId) -> RelType {
        match ty {
            | Type::Var(def) => RelType::Var(def_cb(def)),
            | Type::Abst(abst) => RelType::Abst(self.abst(abst)),
            | Type::Abs(TypeAbstraction { binder, body }) => {
                RelType::Abs { binder: self.type_binder(binder), body: self.ty(body) }
            }
            | Type::App(App(f, a)) => RelType::App(self.ty(f), self.ty(a)),
            | Type::Named(Named(n, inner)) => RelType::Named(n, self.ty(inner)),
            | Type::Label(Label(n, inner)) => RelType::Label(n, self.ty(inner)),
            | Type::Proj(Proj(head, name)) => RelType::Proj(self.ty(head), name),
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
                        domain: self.ty(p.domain),
                        witnesses: p
                            .witnesses
                            .map(|ws| ws.iter().copied().map(|a| self.abst(a)).collect()),
                        witness_projection: p.witness_projection,
                    }),
                };
                RelType::ValPi(Box::new(RelValPi { binder, codomain: self.ty(pi.codomain) }))
            }
            | Type::Arrow(Arrow(a, b)) => RelType::Arrow(self.ty(a), self.ty(b)),
            | Type::Forall(Forall(b, body)) => RelType::Forall(self.type_binder(b), self.ty(body)),
            | Type::PackPi(pi) => RelType::PackPi(Box::new(RelPackPi {
                domain: self.ty(pi.domain),
                witnesses: pi.witnesses.iter().copied().map(|a| self.abst(a)).collect(),
                codomain: self.ty(pi.codomain),
            })),
            | Type::Prod(Prod(items)) => {
                RelType::Prod(items.into_iter().map(|t| self.ty(t)).collect())
            }
            | Type::Exists(exists) => RelType::Exists {
                binder: self.type_binder(exists.binder),
                mode: match exists.mode {
                    | ExistsMode::Abstract => RelExistsMode::Abstract,
                    | ExistsMode::Manifest(d) => RelExistsMode::Manifest(self.ty(d)),
                },
                body: self.ty(exists.body),
            },
            | Type::ManifestKind(mk) => RelType::ManifestKind {
                binder: self.kpat(mk.binder),
                definition: self.kind(mk.definition),
                body: self.ty(mk.body),
            },
            | Type::Data(d) => RelType::Data(self.data(d)),
            | Type::CoData(c) => RelType::CoData(self.codata(c)),
        }
    }

    fn extract_vpats(
        &self, def_cb: &mut impl FnMut(su::DefId) -> RelDefId,
    ) -> Vec<(RelValuePattern, RelId<TypeId>)> {
        (self.ctx.local_base.vpats..self.ctx.statics.vpats.len())
            .filter_map(|idx| {
                let id = VPatId::from_index(idx);
                if self.if_tables.vpat_map.contains_key(&id) {
                    return None;
                }
                let ty = self.ty(self.ctx.statics.annotations_vpat[id]);
                let pat = match self.ctx.statics.vpats[id].clone() {
                    | ValuePattern::Hole(_) => RelValuePattern::Hole,
                    | ValuePattern::Var(def) => RelValuePattern::Var(def_cb(def)),
                    | ValuePattern::Named(Named(f, p)) => RelValuePattern::Named(f, self.vpat(p)),
                    | ValuePattern::Ctor(Ctor(c, p)) => RelValuePattern::Ctor(c, self.vpat(p)),
                    | ValuePattern::Lit(l) => RelValuePattern::Lit(l),
                    | ValuePattern::Alias(Alias(ps)) => {
                        RelValuePattern::Alias(ps.into_iter().map(|p| self.vpat(p)).collect())
                    }
                    | ValuePattern::Triv(_) => RelValuePattern::Triv,
                    | ValuePattern::VCons(ps) => {
                        RelValuePattern::VCons(ps.into_iter().map(|p| self.vpat(p)).collect())
                    }
                    | ValuePattern::SCons(ConsN(sp, p)) => RelValuePattern::SCons(
                        sp.into_iter().map(|x| self.static_pat(x)).collect(),
                        self.vpat(p),
                    ),
                    | ValuePattern::View(view) => RelValuePattern::View {
                        function: self.value(view.function),
                        pattern: self.vpat(view.pattern),
                    },
                };
                Some((pat, ty))
            })
            .collect()
    }

    fn extract_values(
        &self, def_cb: &mut impl FnMut(su::DefId) -> RelDefId,
    ) -> Vec<(RelValue, RelId<TypeId>)> {
        (self.ctx.local_base.values..self.ctx.statics.values.len())
            .filter_map(|idx| {
                let id = ValueId::from_index(idx);
                if self.if_tables.value_map.contains_key(&id) {
                    return None;
                }
                let ty = self.ty(self.ctx.statics.annotations_value[id]);
                let val = match self.ctx.statics.values[id].clone() {
                    | Value::Hole(_) => RelValue::Hole,
                    | Value::Var(def) => RelValue::Var(def_cb(def)),
                    | Value::Named(Named(f, inner)) => RelValue::Named(f, self.value(inner)),
                    | Value::Let(Let { binder, bindee, tail }) => RelValue::Let {
                        binder: self.vpat(binder),
                        bindee: self.value(bindee),
                        tail: self.value(tail),
                    },
                    | Value::ValAbs(Abs(binder, body)) => {
                        let binder = match binder {
                            | ValBinder::Type(t) => RelValBinder::Type(self.tpat(t)),
                            | ValBinder::Value(p) => RelValBinder::Value(self.vpat(p)),
                        };
                        RelValue::ValAbs(binder, self.value(body))
                    }
                    | Value::ValApp(App(f, a)) => RelValue::ValApp(
                        self.value(f),
                        match a {
                            | ValArgument::Type(t) => RelValArgument::Type(self.ty(t)),
                            | ValArgument::Value(v) => RelValArgument::Value(self.value(v)),
                        },
                    ),
                    | Value::Match(Match { scrut, arms }) => RelValue::Match {
                        scrut: self.value(scrut),
                        arms: arms
                            .into_iter()
                            .map(|Matcher { binder, tail }| (self.vpat(binder), self.value(tail)))
                            .collect(),
                    },
                    | Value::IntOp(IntValueOp { operation, operands: [a, b] }) => {
                        RelValue::IntOp { operation, operands: [self.value(a), self.value(b)] }
                    }
                    | Value::IntrinsicOp(op) => RelValue::IntrinsicOp(op),
                    | Value::Thunk(Thunk(compu)) => RelValue::Thunk(self.compu(compu)),
                    | Value::Ctor(Ctor(c, v)) => RelValue::Ctor(c, self.value(v)),
                    | Value::Triv(_) => RelValue::Triv,
                    | Value::VCons(vs) => {
                        RelValue::VCons(vs.into_iter().map(|v| self.value(v)).collect())
                    }
                    | Value::SCons(ConsN(st, v)) => RelValue::SCons(
                        st.into_iter().map(|x| self.static_term(x)).collect(),
                        self.value(v),
                    ),
                    | Value::Seal(v) => RelValue::Seal(self.value(v)),
                    | Value::Proj(Proj(v, field)) => RelValue::Proj(
                        self.value(v),
                        RelResolvedField {
                            name: field.name,
                            products: field
                                .target
                                .products
                                .into_iter()
                                .map(|step| (self.ty(step.product), step.position))
                                .collect(),
                        },
                    ),
                    | Value::Lit(lit) => RelValue::Lit(lit),
                };
                Some((val, ty))
            })
            .collect()
    }

    fn extract_compus(&self) -> Vec<(RelComputation, RelId<TypeId>)> {
        (self.ctx.local_base.compus..self.ctx.statics.compus.len())
            .map(|idx| {
                let id = CompuId::from_index(idx);
                let ty = self.ty(self.ctx.statics.annotations_compu[id]);
                let compu = match self.ctx.statics.compus[id].clone() {
                    | Computation::Hole(_) => RelComputation::Hole,
                    | Computation::VAbs(Abs(p, c)) => {
                        RelComputation::VAbs(self.vpat(p), self.compu(c))
                    }
                    | Computation::VApp(App(c, v)) => {
                        RelComputation::VApp(self.compu(c), self.value(v))
                    }
                    | Computation::TAbs(Abs(p, c)) => {
                        RelComputation::TAbs(self.tpat(p), self.compu(c))
                    }
                    | Computation::TApp(App(c, t)) => {
                        RelComputation::TApp(self.compu(c), self.ty(t))
                    }
                    | Computation::Fix(Fix(p, c)) => {
                        RelComputation::Fix(self.vpat(p), self.compu(c))
                    }
                    | Computation::Force(Force(v)) => RelComputation::Force(self.value(v)),
                    | Computation::Ret(Return(v)) => RelComputation::Ret(self.value(v)),
                    | Computation::Do(Bind { binder, bindee, tail }) => RelComputation::Do {
                        binder: self.vpat(binder),
                        bindee: self.compu(bindee),
                        tail: self.compu(tail),
                    },
                    | Computation::Let(Let { binder, bindee, tail }) => RelComputation::Let {
                        binder: self.vpat(binder),
                        bindee: self.value(bindee),
                        tail: self.compu(tail),
                    },
                    | Computation::Match(Match { scrut, arms }) => RelComputation::Match {
                        scrut: self.value(scrut),
                        arms: arms
                            .into_iter()
                            .map(|Matcher { binder, tail }| (self.vpat(binder), self.compu(tail)))
                            .collect(),
                    },
                    | Computation::CoMatch(CoMatch { arms }) => RelComputation::CoMatch(
                        arms.into_iter()
                            .map(|CoMatcher { dtor, tail }| (dtor, self.compu(tail)))
                            .collect(),
                    ),
                    | Computation::Dtor(Dtor(c, d)) => RelComputation::Dtor(self.compu(c), d),
                };
                (compu, ty)
            })
            .collect()
    }

    fn extract_absts(&self, def_cb: &mut impl FnMut(su::DefId) -> RelDefId) -> Vec<RelAbstEntry> {
        let s = self.ctx.statics;
        (self.ctx.local_base.absts..s.absts.len())
            .filter_map(|idx| {
                let id = AbstId::from_index(idx);
                if self.if_tables.abst_map.contains_key(&id) {
                    return None;
                }
                Some(RelAbstEntry {
                    kind: self.kind(s.annotations_abst[id]),
                    seal: s.seals.get(id).copied().map(|t| self.ty(t)),
                    opaque_representation: s
                        .opaque_representations
                        .get(id)
                        .copied()
                        .map(|t| self.ty(t)),
                    hint: s.abst_hints.get(id).copied().map(&mut *def_cb),
                    existential_skolem: s.existential_skolems.contains(id),
                    builtin_role: s.builtin_roles.witness(id),
                })
            })
            .collect()
    }

    fn extract_datas(&self) -> Vec<RelData> {
        (self.ctx.local_base.datas..self.ctx.statics.datas.len())
            .filter_map(|idx| {
                let id = DataId::from_index(idx);
                if self.if_tables.data_map.contains_key(&id) {
                    return None;
                }
                Some(RelData {
                    arms: self.ctx.statics.datas[id]
                        .iter()
                        .map(|(ctor, t)| (*ctor, self.ty(*t)))
                        .collect(),
                })
            })
            .collect()
    }

    fn extract_codatas(&self) -> Vec<RelCoData> {
        (self.ctx.local_base.codatas..self.ctx.statics.codatas.len())
            .filter_map(|idx| {
                let id = CoDataId::from_index(idx);
                if self.if_tables.codata_map.contains_key(&id) {
                    return None;
                }
                Some(RelCoData {
                    arms: self.ctx.statics.codatas[id]
                        .iter()
                        .map(|(dtor, t)| (*dtor, self.ty(*t)))
                        .collect(),
                })
            })
            .collect()
    }

    fn extract_solus(&self) -> Vec<(RelId<FillId>, RelAnnId)> {
        (self.ctx.local_base.fills..self.ctx.statics.fills.len())
            .filter_map(|idx| {
                let fill = FillId::from_index(idx);
                self.ctx
                    .statics
                    .solus
                    .get(fill)
                    .copied()
                    .map(|ann| (self.fill(fill), self.ann(ann)))
            })
            .collect()
    }

    fn extract_fill_scopes(&self) -> Vec<(RelId<FillId>, Vec<RelId<AbstId>>)> {
        (self.ctx.local_base.fills..self.ctx.statics.fills.len())
            .filter_map(|idx| {
                let fill = FillId::from_index(idx);
                self.ctx.statics.fill_scopes.get(fill).map(|scope| {
                    let witnesses = scope.iter().map(|a| self.abst(a)).collect();
                    (self.fill(fill), witnesses)
                })
            })
            .collect()
    }

    #[allow(clippy::type_complexity)]
    fn extract_node_metadata(
        &self,
    ) -> (
        Vec<(RelId<TypeId>, BuiltinValueRole)>,
        Vec<(RelId<ValueId>, CheckedImport)>,
        Vec<(RelId<ValueId>, RelId<DataId>)>,
        Vec<(RelId<VPatId>, RelId<DataId>)>,
        Vec<(RelId<CompuId>, RelId<CoDataId>)>,
        Vec<RelId<CompuId>>,
        Vec<(RelId<CompuId>, RelId<VPatId>)>,
        Vec<(RelId<VPatId>, usize)>,
    ) {
        let s = self.ctx.statics;
        let b = &self.ctx.local_base;
        let mut value_roles = Vec::new();
        for idx in b.types..s.types_pre.len() {
            let id = TypeId::from_index(idx);
            if !self.if_tables.type_map.contains_key(&id)
                && let Some(role) = s.builtin_roles.value(id)
            {
                value_roles.push((self.ty(id), role));
            }
        }
        let mut foreigns = Vec::new();
        let mut data_hints = Vec::new();
        for idx in b.values..s.values.len() {
            let id = ValueId::from_index(idx);
            if self.if_tables.value_map.contains_key(&id) {
                continue;
            }
            if let Some(import) = s.foreign_imports.get(id).cloned() {
                foreigns.push((self.value(id), import));
            }
            if let Some(&data) = s.data_hints.get(id) {
                data_hints.push((self.value(id), self.data(data)));
            }
        }
        let mut data_pat_hints = Vec::new();
        let mut opened_arity = Vec::new();
        for idx in b.vpats..s.vpats.len() {
            let id = VPatId::from_index(idx);
            if self.if_tables.vpat_map.contains_key(&id) {
                continue;
            }
            if let Some(&data) = s.data_pat_hints.get(id) {
                data_pat_hints.push((self.vpat(id), self.data(data)));
            }
            if let Some(&arity) = s.package_pattern_opened_arity.get(id) {
                opened_arity.push((self.vpat(id), arity));
            }
        }
        let mut codata_hints = Vec::new();
        let mut copattern_matches = Vec::new();
        let mut copattern_binders = Vec::new();
        for idx in b.compus..s.compus.len() {
            let id = CompuId::from_index(idx);
            if let Some(&codata) = s.codata_hints.get(id) {
                codata_hints.push((self.compu(id), self.codata(codata)));
            }
            if s.copattern_matches.contains(id) {
                copattern_matches.push(self.compu(id));
            }
            if let Some(&pat) = s.copattern_pack_pi_binders.get(id) {
                copattern_binders.push((self.compu(id), self.vpat(pat)));
            }
        }
        (
            value_roles,
            foreigns,
            data_hints,
            data_pat_hints,
            codata_hints,
            copattern_matches,
            copattern_binders,
            opened_arity,
        )
    }

    fn extract_pat_provenance(&self) -> Vec<(su::PatId, RelPatId)> {
        let s = self.ctx.statics;
        let b = &self.ctx.local_base;
        let mut out = Vec::new();
        for idx in b.kpats..s.kpats.len() {
            let p = PatId::Kind(KPatId::from_index(idx));
            if let Some(src) = s.pats.source(&p) {
                out.push((src, self.pat_id(p)));
            }
        }
        for idx in b.tpats..s.tpats.len() {
            let p = PatId::Type(TPatId::from_index(idx));
            if let Some(src) = s.pats.source(&p) {
                out.push((src, self.pat_id(p)));
            }
        }
        for idx in b.vpats..s.vpats.len() {
            let p = PatId::Value(VPatId::from_index(idx));
            if let Some(src) = s.pats.source(&p) {
                out.push((src, self.pat_id(p)));
            }
        }
        out
    }

    fn extract_term_provenance(&self) -> Vec<(su::TermId, RelTermId)> {
        let s = self.ctx.statics;
        let b = &self.ctx.local_base;
        let mut out = Vec::new();
        for idx in b.kinds..s.kinds_pre.len() {
            let t = TermId::Kind(KindId::from_index(idx));
            if let Some(src) = s.terms.source(&t) {
                out.push((src, self.term_id(t)));
            }
        }
        for idx in b.types..s.types_pre.len() {
            let t = TermId::Type(TypeId::from_index(idx));
            if let Some(src) = s.terms.source(&t) {
                out.push((src, self.term_id(t)));
            }
        }
        for idx in b.values..s.values.len() {
            let t = TermId::Value(ValueId::from_index(idx));
            if let Some(src) = s.terms.source(&t) {
                out.push((src, self.term_id(t)));
            }
        }
        for idx in b.compus..s.compus.len() {
            let t = TermId::Compu(CompuId::from_index(idx));
            if let Some(src) = s.terms.source(&t) {
                out.push((src, self.term_id(t)));
            }
        }
        out
    }
}
