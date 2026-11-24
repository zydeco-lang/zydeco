//! Extract relative `UnitInterface` and `UnitBody` artifacts from a finished unit check.

use super::digest::{compute_accepted_digest, compute_rejected_digest};
use super::rel::*;
use super::{
    AcceptedUnitInterface, CheckedUnitOutput, ImportedUnitDep, InterfaceDigest, RelAbstEntry,
    RelDefFacts, UnitBody, UnitInterface,
};
use crate::UnitClassifierError;
use crate::arena::StaticsArena;
use crate::builtin::{BuiltinClassifierError, DuplicateBuiltinRole};
use crate::check::intrinsics::{INTRINSIC_KIND_COUNT, intrinsic_type_count};
use crate::check::{TyckError, TyckTask, Tycker};
use crate::elaborate::static_values::StaticEliminationError;
use crate::foreign::ForeignClassifierError;
use crate::surface_syntax as su;
use crate::syntax::*;
use crate::validate::CoverageError;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::VecDeque;
use std::sync::Arc;
use zydeco_foundation::arena::{ArenaAccess, ArenaId, KeySpaceId};
use zydeco_foundation::dense::DenseId;

/// Dense arena extents allocated when preloading one dependency into a check arena.
#[derive(Copy, Clone, Debug, Default)]
pub(crate) struct DepArenaRanges {
    pub if_start: ArenaOffsets,
    pub if_end: ArenaOffsets,
    pub body_start: ArenaOffsets,
    pub body_end: ArenaOffsets,
}

#[derive(Default)]
struct IfTables {
    kind_map: FxHashMap<KindId, u32>,
    kinds: Vec<Option<RelKind>>,
    kpat_map: FxHashMap<KPatId, u32>,
    kpats: Vec<Option<RelKindPattern>>,
    tpat_map: FxHashMap<TPatId, u32>,
    tpats: Vec<Option<(RelTypePattern, RelId<KindId>)>>,
    type_map: FxHashMap<TypeId, u32>,
    types: Vec<Option<(RelType, RelId<KindId>)>>,
    vpat_map: FxHashMap<VPatId, u32>,
    vpats: Vec<Option<(RelValuePattern, RelId<TypeId>)>>,
    value_map: FxHashMap<ValueId, u32>,
    values: Vec<Option<(RelValue, RelId<TypeId>)>>,
    abst_map: FxHashMap<AbstId, u32>,
    absts: Vec<Option<RelAbstEntry>>,
    data_map: FxHashMap<DataId, u32>,
    datas: Vec<Option<RelData>>,
    codata_map: FxHashMap<CoDataId, u32>,
    codatas: Vec<Option<RelCoData>>,
    def_map: FxHashMap<su::DefId, u32>,
    local_defs: Vec<(su::DefId, VarName)>,
    pending_defs: VecDeque<(u32, su::DefId)>,
    pending_absts: VecDeque<(u32, AbstId)>,
    thunk_bodies: Vec<CompuId>,
    thunk_types: Vec<RelId<TypeId>>,
    value_builtin_roles: Vec<(u32, BuiltinValueRole)>,
    foreign_imports: Vec<(u32, CheckedImport)>,
    data_hints: Vec<(u32, RelId<DataId>)>,
    data_pat_hints: Vec<(u32, RelId<DataId>)>,
    package_pattern_opened_arity: Vec<(u32, usize)>,
    def_facts: Vec<(u32, RelDefFacts)>,
    global_terms: Vec<RelTermId>,
}

struct BodyIdMaps {
    kinds: Vec<RelId<KindId>>,
    kpats: Vec<RelId<KPatId>>,
    tpats: Vec<RelId<TPatId>>,
    types: Vec<RelId<TypeId>>,
    vpats: Vec<RelId<VPatId>>,
    values: Vec<RelId<ValueId>>,
    compus: Vec<RelId<CompuId>>,
    absts: Vec<RelId<AbstId>>,
    datas: Vec<RelId<DataId>>,
    codatas: Vec<RelId<CoDataId>>,
    fills: Vec<RelId<FillId>>,
}

struct ExtractContext<'a> {
    statics: &'a StaticsArena,
    scoped: &'a su::ScopedArena,
    local_base: ArenaOffsets,
    dep_ranges: &'a [DepArenaRanges],
    dummy_compus: &'a FxHashMap<CompuId, (u32, u32)>,
    dep_def_spaces: &'a FxHashMap<KeySpaceId, u32>,
    intrinsic_types: usize,
}

impl ExtractContext<'_> {
    fn map_kind(
        &self, id: KindId, if_map: Option<&FxHashMap<KindId, u32>>, body: Option<&[RelId<KindId>]>,
    ) -> RelId<KindId> {
        let idx = id.index();
        if idx < INTRINSIC_KIND_COUNT {
            return RelId::Intrinsic(id);
        }
        if idx < self.local_base.kinds {
            return find_dep_id(self.dep_ranges, idx, |r| r.kinds);
        }
        if let Some(map) = if_map
            && let Some(&slot) = map.get(&id)
        {
            return RelId::LocalInterface(slot);
        }
        body.expect("body slot map")[idx - self.local_base.kinds]
    }

    fn map_kpat(
        &self, id: KPatId, if_map: Option<&FxHashMap<KPatId, u32>>, body: Option<&[RelId<KPatId>]>,
    ) -> RelId<KPatId> {
        let idx = id.index();
        if idx < self.local_base.kpats {
            return find_dep_id(self.dep_ranges, idx, |r| r.kpats);
        }
        if let Some(map) = if_map
            && let Some(&slot) = map.get(&id)
        {
            return RelId::LocalInterface(slot);
        }
        body.expect("body slot map")[idx - self.local_base.kpats]
    }

    fn map_tpat(
        &self, id: TPatId, if_map: Option<&FxHashMap<TPatId, u32>>, body: Option<&[RelId<TPatId>]>,
    ) -> RelId<TPatId> {
        let idx = id.index();
        if idx < self.local_base.tpats {
            return find_dep_id(self.dep_ranges, idx, |r| r.tpats);
        }
        if let Some(map) = if_map
            && let Some(&slot) = map.get(&id)
        {
            return RelId::LocalInterface(slot);
        }
        body.expect("body slot map")[idx - self.local_base.tpats]
    }

    fn map_type(
        &self, id: TypeId, if_map: Option<&FxHashMap<TypeId, u32>>, body: Option<&[RelId<TypeId>]>,
    ) -> RelId<TypeId> {
        let idx = id.index();
        if idx < self.intrinsic_types {
            return RelId::Intrinsic(id);
        }
        if idx < self.local_base.types {
            return find_dep_id(self.dep_ranges, idx, |r| r.types);
        }
        if let Some(map) = if_map
            && let Some(&slot) = map.get(&id)
        {
            return RelId::LocalInterface(slot);
        }
        body.expect("body slot map")[idx - self.local_base.types]
    }

    fn map_vpat(
        &self, id: VPatId, if_map: Option<&FxHashMap<VPatId, u32>>, body: Option<&[RelId<VPatId>]>,
    ) -> RelId<VPatId> {
        let idx = id.index();
        if idx < self.local_base.vpats {
            return find_dep_id(self.dep_ranges, idx, |r| r.vpats);
        }
        if let Some(map) = if_map
            && let Some(&slot) = map.get(&id)
        {
            return RelId::LocalInterface(slot);
        }
        body.expect("body slot map")[idx - self.local_base.vpats]
    }

    fn map_value(
        &self, id: ValueId, if_map: Option<&FxHashMap<ValueId, u32>>,
        body: Option<&[RelId<ValueId>]>,
    ) -> RelId<ValueId> {
        let idx = id.index();
        if idx < self.local_base.values {
            return find_dep_id(self.dep_ranges, idx, |r| r.values);
        }
        if let Some(map) = if_map
            && let Some(&slot) = map.get(&id)
        {
            return RelId::LocalInterface(slot);
        }
        body.expect("body slot map")[idx - self.local_base.values]
    }

    fn map_compu(&self, id: CompuId, body: &[RelId<CompuId>]) -> RelId<CompuId> {
        if let Some(&(dep, slot)) = self.dummy_compus.get(&id) {
            return RelId::DepStub { dep, slot };
        }
        let idx = id.index();
        if idx < self.local_base.compus {
            return find_dep_id(self.dep_ranges, idx, |r| r.compus);
        }
        body[idx - self.local_base.compus]
    }

    fn map_abst(
        &self, id: AbstId, if_map: Option<&FxHashMap<AbstId, u32>>, body: Option<&[RelId<AbstId>]>,
    ) -> RelId<AbstId> {
        let idx = id.index();
        if idx < self.local_base.absts {
            return find_dep_id(self.dep_ranges, idx, |r| r.absts);
        }
        if let Some(map) = if_map
            && let Some(&slot) = map.get(&id)
        {
            return RelId::LocalInterface(slot);
        }
        body.expect("body slot map")[idx - self.local_base.absts]
    }

    fn map_data(
        &self, id: DataId, if_map: Option<&FxHashMap<DataId, u32>>, body: Option<&[RelId<DataId>]>,
    ) -> RelId<DataId> {
        let idx = id.index();
        if idx < self.local_base.datas {
            return find_dep_id(self.dep_ranges, idx, |r| r.datas);
        }
        if let Some(map) = if_map
            && let Some(&slot) = map.get(&id)
        {
            return RelId::LocalInterface(slot);
        }
        body.expect("body slot map")[idx - self.local_base.datas]
    }

    fn map_codata(
        &self, id: CoDataId, if_map: Option<&FxHashMap<CoDataId, u32>>,
        body: Option<&[RelId<CoDataId>]>,
    ) -> RelId<CoDataId> {
        let idx = id.index();
        if idx < self.local_base.codatas {
            return find_dep_id(self.dep_ranges, idx, |r| r.codatas);
        }
        if let Some(map) = if_map
            && let Some(&slot) = map.get(&id)
        {
            return RelId::LocalInterface(slot);
        }
        body.expect("body slot map")[idx - self.local_base.codatas]
    }

    fn map_fill(&self, id: FillId, body: &[RelId<FillId>]) -> RelId<FillId> {
        let idx = id.index();
        if idx < self.local_base.fills {
            return find_dep_id(self.dep_ranges, idx, |r| r.fills);
        }
        body[idx - self.local_base.fills]
    }
}

fn find_dep_id<Id: DenseId>(
    ranges: &[DepArenaRanges], idx: usize, field: fn(&ArenaOffsets) -> usize,
) -> RelId<Id> {
    for (dep, range) in ranges.iter().enumerate() {
        let if_s = field(&range.if_start);
        let if_e = field(&range.if_end);
        if (if_s..if_e).contains(&idx) {
            return RelId::DepInterface { dep: dep as u32, offset: (idx - if_s) as u32 };
        }
        let b_s = field(&range.body_start);
        let b_e = field(&range.body_end);
        if (b_s..b_e).contains(&idx) {
            return RelId::DepBody { dep: dep as u32, offset: (idx - b_s) as u32 };
        }
    }
    unreachable!("dependency index {idx} outside all dependency ranges")
}

/// Extract the unit's `UnitInterface` and `UnitBody` after type checking and validation.
pub(crate) fn extract_unit(
    tycker: &Tycker<'_>, checked_root: Option<TermAnnId>, local_base: ArenaOffsets,
    dep_ranges: &[DepArenaRanges], dummy_compus: &FxHashMap<CompuId, (u32, u32)>,
    dep_def_spaces: &FxHashMap<KeySpaceId, u32>, deps: &[ImportedUnitDep<'_>],
) -> CheckedUnitOutput {
    let ctx = ExtractContext {
        statics: &tycker.statics,
        scoped: tycker.scoped,
        local_base,
        dep_ranges,
        dummy_compus,
        dep_def_spaces,
        intrinsic_types: intrinsic_type_count(),
    };
    let dep_digests: Vec<_> = deps.iter().map(|dep| dep.interface.digest()).collect();
    let mut if_tables = IfTables::default();
    let if_root = checked_root
        .filter(|_| tycker.errors.is_empty())
        .map(|root| InterfaceExtractor { ctx: &ctx, out: &mut if_tables }.extract_root(root));

    let body_maps = build_body_maps(&ctx, &if_tables);
    let body = BodyExtractor { ctx: &ctx, if_tables: &if_tables, maps: &body_maps }
        .extract(tycker, checked_root);

    let interface = match if_root {
        | Some(root) => {
            let acc = finalize_accepted_interface(root, if_tables, &dep_digests);
            UnitInterface::Accepted(Arc::new(acc))
        }
        | None => {
            UnitInterface::Rejected { digest: compute_rejected_digest(&body.errors, &dep_digests) }
        }
    };
    CheckedUnitOutput { interface, body: Arc::new(body) }
}

fn finalize_accepted_interface(
    root: RelTermAnnId, t: IfTables, dep_digests: &[InterfaceDigest],
) -> AcceptedUnitInterface {
    let mut acc = AcceptedUnitInterface {
        root,
        local_defs: t.local_defs.into_iter().map(|(_, name)| name).collect(),
        kinds: t.kinds.into_iter().map(|k| k.expect("kind slot filled")).collect(),
        kpats: t.kpats.into_iter().map(|k| k.expect("kpat slot filled")).collect(),
        tpats: t.tpats.into_iter().map(|k| k.expect("tpat slot filled")).collect(),
        types: t.types.into_iter().map(|k| k.expect("type slot filled")).collect(),
        vpats: t.vpats.into_iter().map(|k| k.expect("vpat slot filled")).collect(),
        values: t.values.into_iter().map(|k| k.expect("value slot filled")).collect(),
        thunk_types: t.thunk_types,
        absts: t.absts.into_iter().map(|k| k.expect("abst slot filled")).collect(),
        datas: t.datas.into_iter().map(|k| k.expect("data slot filled")).collect(),
        codatas: t.codatas.into_iter().map(|k| k.expect("codata slot filled")).collect(),
        value_builtin_roles: t.value_builtin_roles,
        foreign_imports: t.foreign_imports,
        data_hints: t.data_hints,
        data_pat_hints: t.data_pat_hints,
        package_pattern_opened_arity: t.package_pattern_opened_arity,
        def_facts: t.def_facts,
        global_terms: t.global_terms,
        digest: InterfaceDigest([0; 32]),
    };
    acc.digest = compute_accepted_digest(&acc, dep_digests);
    acc
}

fn partition_ids<Id: DenseId + Eq + std::hash::Hash>(
    total: usize, start: usize, if_map: &FxHashMap<Id, u32>,
) -> Vec<RelId<Id>> {
    let mut next_body = 0u32;
    (start..total)
        .map(|idx| {
            let id = Id::from_index(idx);
            if let Some(&slot) = if_map.get(&id) {
                RelId::LocalInterface(slot)
            } else {
                let slot = next_body;
                next_body += 1;
                RelId::LocalBody(slot)
            }
        })
        .collect()
}

fn partition_all_body<Id: DenseId>(total: usize, start: usize) -> Vec<RelId<Id>> {
    (0..(total - start)).map(|offset| RelId::LocalBody(offset as u32)).collect()
}

fn build_body_maps(ctx: &ExtractContext<'_>, if_t: &IfTables) -> BodyIdMaps {
    let s = ctx.statics;
    let b = &ctx.local_base;
    BodyIdMaps {
        kinds: partition_ids(s.kinds_pre.len(), b.kinds, &if_t.kind_map),
        kpats: partition_ids(s.kpats.len(), b.kpats, &if_t.kpat_map),
        tpats: partition_ids(s.tpats.len(), b.tpats, &if_t.tpat_map),
        types: partition_ids(s.types_pre.len(), b.types, &if_t.type_map),
        vpats: partition_ids(s.vpats.len(), b.vpats, &if_t.vpat_map),
        values: partition_ids(s.values.len(), b.values, &if_t.value_map),
        compus: partition_all_body(s.compus.len(), b.compus),
        absts: partition_ids(s.absts.len(), b.absts, &if_t.abst_map),
        datas: partition_ids(s.datas.len(), b.datas, &if_t.data_map),
        codatas: partition_ids(s.codatas.len(), b.codatas, &if_t.codata_map),
        fills: partition_all_body(s.fills.len(), b.fills),
    }
}

struct InterfaceExtractor<'a> {
    ctx: &'a ExtractContext<'a>,
    out: &'a mut IfTables,
}

impl InterfaceExtractor<'_> {
    fn extract_root(mut self, root: TermAnnId) -> RelTermAnnId {
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

struct BodyDefFacts {
    value_aliases: Vec<(RelDefId, RelId<ValueId>)>,
    package_aliases: Vec<(RelDefId, Vec<RelStaticTermId>)>,
    type_definitions: Vec<(RelDefId, RelId<TypeId>)>,
    inlinables: Vec<(RelDefId, RelId<ValueId>)>,
    global_defs: Vec<RelDefId>,
    annotations_var: Vec<(RelDefId, RelAnnId)>,
    def_hints: Vec<(RelTermId, RelDefId)>,
    global_terms: Vec<RelTermId>,
}

struct BodyExtractor<'a> {
    ctx: &'a ExtractContext<'a>,
    if_tables: &'a IfTables,
    maps: &'a BodyIdMaps,
}

impl BodyExtractor<'_> {
    fn extract(self, tycker: &Tycker<'_>, checked_root: Option<TermAnnId>) -> UnitBody {
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

    fn kind(&self, id: KindId) -> RelId<KindId> {
        self.ctx.map_kind(id, Some(&self.if_tables.kind_map), Some(&self.maps.kinds))
    }
    fn kpat(&self, id: KPatId) -> RelId<KPatId> {
        self.ctx.map_kpat(id, Some(&self.if_tables.kpat_map), Some(&self.maps.kpats))
    }
    fn tpat(&self, id: TPatId) -> RelId<TPatId> {
        self.ctx.map_tpat(id, Some(&self.if_tables.tpat_map), Some(&self.maps.tpats))
    }
    fn ty(&self, id: TypeId) -> RelId<TypeId> {
        self.ctx.map_type(id, Some(&self.if_tables.type_map), Some(&self.maps.types))
    }
    fn vpat(&self, id: VPatId) -> RelId<VPatId> {
        self.ctx.map_vpat(id, Some(&self.if_tables.vpat_map), Some(&self.maps.vpats))
    }
    fn value(&self, id: ValueId) -> RelId<ValueId> {
        self.ctx.map_value(id, Some(&self.if_tables.value_map), Some(&self.maps.values))
    }
    fn compu(&self, id: CompuId) -> RelId<CompuId> {
        self.ctx.map_compu(id, &self.maps.compus)
    }
    fn abst(&self, id: AbstId) -> RelId<AbstId> {
        self.ctx.map_abst(id, Some(&self.if_tables.abst_map), Some(&self.maps.absts))
    }
    fn data(&self, id: DataId) -> RelId<DataId> {
        self.ctx.map_data(id, Some(&self.if_tables.data_map), Some(&self.maps.datas))
    }
    fn codata(&self, id: CoDataId) -> RelId<CoDataId> {
        self.ctx.map_codata(id, Some(&self.if_tables.codata_map), Some(&self.maps.codatas))
    }
    fn fill(&self, id: FillId) -> RelId<FillId> {
        self.ctx.map_fill(id, &self.maps.fills)
    }
    fn ann(&self, id: AnnId) -> RelAnnId {
        match id {
            | AnnId::Set => RelAnnId::Set,
            | AnnId::Kind(k) => RelAnnId::Kind(self.kind(k)),
            | AnnId::Type(t) => RelAnnId::Type(self.ty(t)),
        }
    }
    fn rel_term_ann(&self, id: TermAnnId) -> RelTermAnnId {
        match id {
            | TermAnnId::Hole(f) => RelTermAnnId::Hole(self.fill(f)),
            | TermAnnId::Kind(k) => RelTermAnnId::Kind(self.kind(k)),
            | TermAnnId::Type(t, k) => RelTermAnnId::Type(self.ty(t), self.kind(k)),
            | TermAnnId::Value(v, t) => RelTermAnnId::Value(self.value(v), self.ty(t)),
            | TermAnnId::Compu(c, t) => RelTermAnnId::Compu(self.compu(c), self.ty(t)),
        }
    }
    fn term_id(&self, id: TermId) -> RelTermId {
        match id {
            | TermId::Kind(k) => RelTermId::Kind(self.kind(k)),
            | TermId::Type(t) => RelTermId::Type(self.ty(t)),
            | TermId::Value(v) => RelTermId::Value(self.value(v)),
            | TermId::Compu(c) => RelTermId::Compu(self.compu(c)),
        }
    }
    fn pat_id(&self, id: PatId) -> RelPatId {
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

    fn rel_coverage(&self, err: &CoverageError) -> RelCoverageError {
        match err {
            | CoverageError::NonExhaustiveMatch { term, missing, truncated } => {
                RelCoverageError::NonExhaustiveMatch {
                    term: self.term_id(*term),
                    missing: missing.clone(),
                    truncated: *truncated,
                }
            }
            | CoverageError::NonExhaustiveCopatternMatch { computation, missing, truncated } => {
                RelCoverageError::NonExhaustiveCopatternMatch {
                    computation: self.compu(*computation),
                    missing: missing.clone(),
                    truncated: *truncated,
                }
            }
            | CoverageError::NonExhaustiveCoMatch { computation, missing } => {
                RelCoverageError::NonExhaustiveCoMatch {
                    computation: self.compu(*computation),
                    missing: missing.clone(),
                }
            }
            | CoverageError::DuplicateCoMatchArms { computation, duplicates } => {
                RelCoverageError::DuplicateCoMatchArms {
                    computation: self.compu(*computation),
                    duplicates: duplicates.clone(),
                }
            }
        }
    }

    fn rel_observation(&self, obs: &crate::check::TyckObservation) -> RelObservation {
        match obs {
            | crate::check::TyckObservation::HoleSolution { site, solution } => {
                RelObservation::HoleSolution {
                    site: *site,
                    solution: solution.map(|a| self.ann(a)),
                }
            }
            | crate::check::TyckObservation::Debug { metadata, result } => RelObservation::Debug {
                metadata: metadata.clone(),
                result: self.rel_term_ann(*result),
            },
        }
    }

    fn rel_error_entry(
        &self, entry: &crate::check::TyckErrorEntry, def_cb: &mut impl FnMut(su::DefId) -> RelDefId,
    ) -> RelErrorEntry {
        let error = self.rel_error(&entry.error, def_cb);
        let stack = entry
            .stack
            .iter()
            .map(|task| match task {
                | TyckTask::Pat(p, sw) => RelTyckTask::Pat(*p, self.rel_switch(*sw)),
                | TyckTask::Term(t, sw) => RelTyckTask::Term(*t, self.rel_switch(*sw)),
                | TyckTask::Lub(a, b) => RelTyckTask::Lub(self.ann(*a), self.ann(*b)),
                | TyckTask::SignatureGen(a) => RelTyckTask::SignatureGen(self.ann(*a)),
                | TyckTask::StructureGen(a) => RelTyckTask::StructureGen(self.ann(*a)),
                | TyckTask::MonadicLiftPat(p) => RelTyckTask::MonadicLiftPat(self.pat_id(*p)),
                | TyckTask::MonadicLiftTerm(t) => RelTyckTask::MonadicLiftTerm(self.term_id(*t)),
            })
            .collect();
        RelErrorEntry { error, stack }
    }

    fn rel_switch(&self, sw: crate::check::Switch<AnnId>) -> crate::check::Switch<RelAnnId> {
        match sw {
            | crate::check::Switch::Syn => crate::check::Switch::Syn,
            | crate::check::Switch::Ana(a) => crate::check::Switch::Ana(self.ann(a)),
        }
    }

    fn rel_unit_classifier_error(&self, err: &UnitClassifierError) -> RelUnitClassifierError {
        match err {
            | UnitClassifierError::Initializer => RelUnitClassifierError::Initializer,
            | UnitClassifierError::Unsupported { classifier } => {
                RelUnitClassifierError::Unsupported { classifier: self.ty(*classifier) }
            }
            | UnitClassifierError::Interface(i) => RelUnitClassifierError::Interface(i.clone()),
        }
    }

    fn rel_static_elimination(&self, err: &StaticEliminationError) -> RelStaticEliminationError {
        match err {
            | StaticEliminationError::UnresolvedApplication { function } => {
                RelStaticEliminationError::UnresolvedApplication { function: self.value(*function) }
            }
            | StaticEliminationError::UnresolvedMatch { value } => {
                RelStaticEliminationError::UnresolvedMatch { value: self.value(*value) }
            }
            | StaticEliminationError::UnresolvedInteger { value } => {
                RelStaticEliminationError::UnresolvedInteger { value: self.value(*value) }
            }
            | StaticEliminationError::RuntimeValue { value, ty } => {
                RelStaticEliminationError::RuntimeValue {
                    value: self.value(*value),
                    ty: self.ty(*ty),
                }
            }
            | StaticEliminationError::RuntimeComputation { computation, ty } => {
                RelStaticEliminationError::RuntimeComputation {
                    computation: self.compu(*computation),
                    ty: self.ty(*ty),
                }
            }
            | StaticEliminationError::ReductionLimit { function } => {
                RelStaticEliminationError::ReductionLimit { function: self.value(*function) }
            }
        }
    }

    fn rel_builtin_signature(
        &self, sig: &crate::builtin::BuiltinSignatureError,
    ) -> RelBuiltinSignatureError {
        RelBuiltinSignatureError {
            duplicates: sig
                .duplicates
                .iter()
                .map(|d| match d {
                    | DuplicateBuiltinRole::Type { role, witnesses } => {
                        RelDuplicateBuiltinRole::Type {
                            role: *role,
                            witnesses: witnesses.iter().copied().map(|a| self.abst(a)).collect(),
                        }
                    }
                    | DuplicateBuiltinRole::Value { role, entries } => {
                        RelDuplicateBuiltinRole::Value {
                            role: *role,
                            entries: entries.iter().copied().map(|t| self.ty(t)).collect(),
                        }
                    }
                })
                .collect(),
            classifiers: sig
                .classifiers
                .iter()
                .map(|c| match c {
                    | BuiltinClassifierError::Mismatch { role, entry, expected } => {
                        RelBuiltinClassifierError::Mismatch {
                            role: *role,
                            entry: self.ty(*entry),
                            expected: expected.clone(),
                        }
                    }
                })
                .collect(),
        }
    }

    fn rel_foreign_classifier(&self, fc: &ForeignClassifierError) -> RelForeignClassifierError {
        match fc {
            | ForeignClassifierError::Abi => RelForeignClassifierError::Abi,
            | ForeignClassifierError::Unit(u) => {
                RelForeignClassifierError::Unit(self.rel_unit_classifier_error(u))
            }
            | ForeignClassifierError::UnsupportedExportParameter { index, classifier } => {
                RelForeignClassifierError::UnsupportedExportParameter {
                    index: *index,
                    classifier: self.ty(*classifier),
                }
            }
            | ForeignClassifierError::ExpectedThunk { classifier } => {
                RelForeignClassifierError::ExpectedThunk { classifier: self.ty(*classifier) }
            }
            | ForeignClassifierError::UnsupportedParameter { index, classifier } => {
                RelForeignClassifierError::UnsupportedParameter {
                    index: *index,
                    classifier: self.ty(*classifier),
                }
            }
            | ForeignClassifierError::ExpectedReturn { classifier } => {
                RelForeignClassifierError::ExpectedReturn { classifier: self.ty(*classifier) }
            }
            | ForeignClassifierError::UnsupportedResult { classifier } => {
                RelForeignClassifierError::UnsupportedResult { classifier: self.ty(*classifier) }
            }
            | ForeignClassifierError::Signature(s) => {
                RelForeignClassifierError::Signature(s.clone())
            }
        }
    }

    fn rel_error(
        &self, err: &TyckError, def_cb: &mut impl FnMut(su::DefId) -> RelDefId,
    ) -> RelTyckError {
        match err {
            | TyckError::MissingAnnotation => RelTyckError::MissingAnnotation,
            | TyckError::MissingSeal => RelTyckError::MissingSeal,
            | TyckError::InvalidBindingCycle(p) => RelTyckError::InvalidBindingCycle(*p),
            | TyckError::MissingSolution(fs) => {
                RelTyckError::MissingSolution(fs.iter().copied().map(|f| self.fill(f)).collect())
            }
            | TyckError::UnconstrainedInference(fs) => RelTyckError::UnconstrainedInference(
                fs.iter().copied().map(|f| self.fill(f)).collect(),
            ),
            | TyckError::OccursCheck(f) => RelTyckError::OccursCheck(self.fill(*f)),
            | TyckError::MissingStructure(t) => RelTyckError::MissingStructure(self.ty(*t)),
            | TyckError::SortMismatch => RelTyckError::SortMismatch,
            | TyckError::SignatureNotType => RelTyckError::SignatureNotType,
            | TyckError::TypeOfKind => RelTyckError::TypeOfKind,
            | TyckError::KindMismatch => RelTyckError::KindMismatch,
            | TyckError::TypeMismatch { expected, found } => {
                RelTyckError::TypeMismatch { expected: self.ty(*expected), found: self.ty(*found) }
            }
            | TyckError::TypeExpected { expected, found } => {
                RelTyckError::TypeExpected { expected: expected.clone(), found: self.ty(*found) }
            }
            | TyckError::NamedLabelMismatch { expected, found } => {
                RelTyckError::NamedLabelMismatch { expected: *expected, found: *found }
            }
            | TyckError::MissingNamedTypeField { field, found } => {
                RelTyckError::MissingNamedTypeField { field: *field, found: self.kind(*found) }
            }
            | TyckError::AmbiguousNamedTypeField { field, found } => {
                RelTyckError::AmbiguousNamedTypeField { field: *field, found: self.kind(*found) }
            }
            | TyckError::MissingNamedField { field, found } => {
                RelTyckError::MissingNamedField { field: *field, found: self.ty(*found) }
            }
            | TyckError::DuplicateNamedField { field, found } => {
                RelTyckError::DuplicateNamedField { field: *field, found: self.ty(*found) }
            }
            | TyckError::SealedNamedField { field, found } => {
                RelTyckError::SealedNamedField { field: *field, found: self.ty(*found) }
            }
            | TyckError::StaticNamedField { field, found } => {
                RelTyckError::StaticNamedField { field: *field, found: self.ty(*found) }
            }
            | TyckError::PatternAliasRequiresValue => RelTyckError::PatternAliasRequiresValue,
            | TyckError::RefutablePatternAlias => RelTyckError::RefutablePatternAlias,
            | TyckError::RefutableBinding(p) => RelTyckError::RefutableBinding(*p),
            | TyckError::RefutableFieldProjectionPattern => {
                RelTyckError::RefutableFieldProjectionPattern
            }
            | TyckError::UnknownDataConstructor(c) => RelTyckError::UnknownDataConstructor(*c),
            | TyckError::UnknownCoDataDestructor(d) => RelTyckError::UnknownCoDataDestructor(*d),
            | TyckError::CopatternStepMismatch { expected, found } => {
                RelTyckError::CopatternStepMismatch { expected: *expected, found: found.clone() }
            }
            | TyckError::OverlappingCopatternClauses => RelTyckError::OverlappingCopatternClauses,
            | TyckError::MultiplePackPiCopatternClauses => {
                RelTyckError::MultiplePackPiCopatternClauses
            }
            | TyckError::NonExhaustiveCopattern { expected } => {
                RelTyckError::NonExhaustiveCopattern { expected: self.ty(*expected) }
            }
            | TyckError::Coverage(c) => RelTyckError::Coverage(self.rel_coverage(c)),
            | TyckError::StaticElimination(e) => {
                RelTyckError::StaticElimination(self.rel_static_elimination(e))
            }
            | TyckError::PackageWitnessesUnavailable { package } => {
                RelTyckError::PackageWitnessesUnavailable { package: self.value(*package) }
            }
            | TyckError::PackageWitnessArityMismatch { expected, found } => {
                RelTyckError::PackageWitnessArityMismatch { expected: *expected, found: *found }
            }
            | TyckError::EscapingExistential { witnesses, result } => {
                RelTyckError::EscapingExistential {
                    witnesses: witnesses.iter().copied().map(|a| self.abst(a)).collect(),
                    result: self.ty(*result),
                }
            }
            | TyckError::EscapingDeclaration { declarations, result } => {
                RelTyckError::EscapingDeclaration {
                    declarations: declarations.iter().copied().map(|a| self.abst(a)).collect(),
                    result: self.ty(*result),
                }
            }
            | TyckError::InvalidBuiltinAttachment { role, expected } => {
                RelTyckError::InvalidBuiltinAttachment { role: *role, expected }
            }
            | TyckError::InvalidBuiltinSignature(sig) => {
                RelTyckError::InvalidBuiltinSignature(self.rel_builtin_signature(sig))
            }
            | TyckError::ConflictingBuiltinRole { existing, found } => {
                RelTyckError::ConflictingBuiltinRole { existing: *existing, found: *found }
            }
            | TyckError::InvalidForeignAttachment => RelTyckError::InvalidForeignAttachment,
            | TyckError::InvalidForeignClassifier(fc) => {
                RelTyckError::InvalidForeignClassifier(self.rel_foreign_classifier(fc))
            }
            | TyckError::ConflictingForeignImport { existing, found } => {
                RelTyckError::ConflictingForeignImport {
                    existing: existing.clone(),
                    found: found.clone(),
                }
            }
            | TyckError::MissingBuiltinTypeRole { role } => {
                RelTyckError::MissingBuiltinTypeRole { role: *role }
            }
            | TyckError::AmbiguousBuiltinTypeRole { role, witnesses } => {
                RelTyckError::AmbiguousBuiltinTypeRole {
                    role: *role,
                    witnesses: witnesses.iter().copied().map(|a| self.abst(a)).collect(),
                }
            }
            | TyckError::IntegerLiteralOutOfRange { value, integer_type } => {
                RelTyckError::IntegerLiteralOutOfRange {
                    value: *value,
                    integer_type: *integer_type,
                }
            }
            | TyckError::FloatLiteralOutOfRange { value, float_type } => {
                RelTyckError::FloatLiteralOutOfRange { value: *value, float_type: *float_type }
            }
            | TyckError::Expressivity(s) => RelTyckError::Expressivity(s),
            | TyckError::NotInlinable(d) => RelTyckError::NotInlinable(def_cb(*d)),
            | TyckError::NotInlinableSeal(a) => RelTyckError::NotInlinableSeal(self.abst(*a)),
        }
    }
}
