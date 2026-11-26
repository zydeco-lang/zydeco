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

mod body;
mod errors;
mod interface;

use body::BodyExtractor;
use interface::InterfaceExtractor;

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
