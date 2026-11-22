//! Relocate `UnitInterface` and `UnitBody` artifacts into fresh `StaticsArena` instances.

use super::extract::DepArenaRanges;
use super::rel::*;
use super::{
    AcceptedUnitInterface, AssembledUnitInput, ImportedUnitDep, RelAbstEntry, RelDefFacts,
    UnitBody, UnitInterface,
};
use crate::UnitClassifierError;
use crate::arena::StaticsArena;
use crate::builtin::{BuiltinClassifierError, BuiltinSignatureError, DuplicateBuiltinRole};
use crate::check::intrinsics::IntrinsicStatics;
use crate::check::{Switch, TyckError, TyckErrorEntry, TyckObservation, TyckTask};
use crate::elaborate::monadic::nominal::NominalFamily;
use crate::elaborate::static_values::StaticEliminationError;
use crate::environment::SkolemScope;
use crate::foreign::ForeignClassifierError;
use crate::surface_syntax as su;
use crate::syntax::*;
use crate::validate::CoverageError;
use rustc_hash::FxHashMap;
use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;
use zydeco_foundation::arena::{ArenaAccess, KeySpaceId, derived_id};
use zydeco_foundation::dense::DenseId;

pub(crate) struct PreloadedUnit {
    pub statics: StaticsArena,
    pub intrinsics: IntrinsicStatics,
    pub ranges: Vec<DepArenaRanges>,
    pub dummy_compus: FxHashMap<CompuId, (u32, u32)>,
    pub def_spaces: FxHashMap<KeySpaceId, u32>,
    pub roots: Vec<std::result::Result<TermAnnId, ()>>,
    pub monadic_families: BTreeMap<AbstId, NominalFamily>,
    pub unit_owned_defs: Vec<Arc<[su::DefId]>>,
    pub dep_value_ranges: Vec<(std::ops::Range<usize>, Arc<[su::DefId]>)>,
}

pub(crate) struct AssembledProgramState {
    pub statics: StaticsArena,
    pub intrinsics: IntrinsicStatics,
    pub root: Option<TermAnnId>,
    pub errors: Vec<TyckErrorEntry>,
    pub observations: Vec<TyckObservation>,
}

#[derive(Copy, Clone)]
struct UnitSlice<'a> {
    interface: Option<&'a AcceptedUnitInterface>,
    body: Option<&'a UnitBody>,
    dep_to_global: &'a [usize],
    synthetic_def_space: Option<KeySpaceId>,
}

enum DefMode {
    Synthetic,
    Real,
}

struct Relocator<'a> {
    units: Vec<UnitSlice<'a>>,
    ranges: Vec<DepArenaRanges>,
    dummy_compu_base: Vec<usize>,
    load_bodies: bool,
    def_mode: DefMode,
}

pub(crate) fn preload_for_unit_check(
    deps: &[ImportedUnitDep<'_>], load_bodies: bool,
) -> PreloadedUnit {
    let mut statics = StaticsArena::default();
    let intrinsics = IntrinsicStatics::new(&mut statics);
    let mut def_spaces = FxHashMap::default();
    let slices: Vec<_> = deps
        .iter()
        .enumerate()
        .map(|(idx, dep)| {
            def_spaces.insert(dep.synthetic_def_space, idx as u32);
            let interface = match dep.interface {
                | UnitInterface::Accepted(acc) => Some(acc.as_ref()),
                | UnitInterface::Rejected { .. } => None,
            };
            let body = if load_bodies && interface.is_some() { dep.body } else { None };
            UnitSlice {
                interface,
                body,
                dep_to_global: dep.dep_to_global,
                synthetic_def_space: Some(dep.synthetic_def_space),
            }
        })
        .collect();

    let (relocator, base_offsets) =
        Relocator::plan(&statics, slices, load_bodies, DefMode::Synthetic);
    let _ = base_offsets;
    let mut dummy_compus = FxHashMap::default();
    let mut roots = Vec::with_capacity(deps.len());
    let mut monadic_families = BTreeMap::new();
    let mut own_defs = vec![Vec::new(); relocator.units.len()];

    for u in 0..relocator.units.len() {
        let slice = relocator.units[u];
        let Some(acc) = slice.interface else {
            roots.push(Err(()));
            continue;
        };
        if let Some(synth_space) = slice.synthetic_def_space {
            for (slot, name) in acc.local_defs.iter().enumerate() {
                let def: su::DefId = derived_id(synth_space, slot as u32);
                own_defs[u].push(def);
                if statics.generated_defs.get(&def).is_none() {
                    statics.generated_defs.insert_new(def, *name);
                }
            }
        }
        if let Some(body) = slice.body {
            for &(def, name) in &body.body_defs {
                own_defs[u].push(def);
                if statics.generated_defs.get(&def).is_none() {
                    statics.generated_defs.insert_new(def, name);
                }
            }
            for &(def, name) in &body.generated_defs {
                own_defs[u].push(def);
                if statics.generated_defs.get(&def).is_none() {
                    statics.generated_defs.insert_new(def, name);
                }
            }
        }
        relocator.install_unit(&mut statics, u, acc, slice.body);
        roots.push(Ok(relocator.term_ann(u, acc.root)));
        if !load_bodies {
            for (slot, rel_ty) in acc.thunk_types.iter().enumerate() {
                let id = CompuId::from_index(relocator.dummy_compu_base[u] + slot);
                dummy_compus.insert(id, (u as u32, slot as u32));
                statics.annotations_compu.insert_new(id, relocator.ty(u, *rel_ty));
            }
        } else if let Some(body) = slice.body {
            for (abst, fam) in &body.monadic_families {
                monadic_families.insert(
                    relocator.abst(u, *abst),
                    NominalFamily {
                        constructor: relocator.ty(u, fam.constructor),
                        dependencies: fam
                            .dependencies
                            .iter()
                            .map(|a| relocator.abst(u, *a))
                            .collect(),
                        method: fam.method,
                    },
                );
            }
        }
    }

    let unit_owned_defs: Vec<Arc<[su::DefId]>> = (0..relocator.units.len())
        .map(|u| {
            let mut defs = BTreeSet::new();
            defs.extend(own_defs[u].iter().copied());
            for &dep in relocator.units[u].dep_to_global {
                defs.extend(own_defs[dep].iter().copied());
            }
            Arc::from(defs.into_iter().collect::<Vec<_>>())
        })
        .collect();
    let dep_value_ranges: Vec<(std::ops::Range<usize>, Arc<[su::DefId]>)> =
        (0..relocator.units.len())
            .map(|u| {
                (
                    relocator.ranges[u].if_start.values..relocator.ranges[u].body_end.values,
                    Arc::clone(&unit_owned_defs[u]),
                )
            })
            .collect();

    PreloadedUnit {
        statics,
        intrinsics,
        ranges: relocator.ranges,
        dummy_compus,
        def_spaces,
        roots,
        monadic_families,
        unit_owned_defs,
        dep_value_ranges,
    }
}

pub(crate) fn assemble_for_program(units: &[AssembledUnitInput<'_>]) -> AssembledProgramState {
    let mut statics = StaticsArena::default();
    let intrinsics = IntrinsicStatics::new(&mut statics);
    let slices: Vec<_> = units
        .iter()
        .map(|unit| {
            let interface = match unit.interface {
                | UnitInterface::Accepted(acc) => Some(acc.as_ref()),
                | UnitInterface::Rejected { .. } => None,
            };
            UnitSlice {
                interface,
                body: Some(unit.body),
                dep_to_global: unit.dep_to_global,
                synthetic_def_space: None,
            }
        })
        .collect();

    let (relocator, _) = Relocator::plan(&statics, slices, true, DefMode::Real);
    let mut errors = Vec::new();
    let mut observations = Vec::new();
    let mut root = None;

    for u in 0..relocator.units.len() {
        let slice = relocator.units[u];
        let body = slice.body.expect("assembled units carry bodies");
        for &(def, name) in &body.generated_defs {
            if statics.generated_defs.get(&def).is_none() {
                statics.generated_defs.insert_new(def, name);
            }
        }
        if let Some(acc) = slice.interface {
            relocator.install_interface(&mut statics, u, acc);
        }
        relocator.install_body_nodes(&mut statics, u, body, false);
        relocator.install_body_facts(&mut statics, u, body);
        statics.types_pre.refresh_summaries_from(relocator.ranges[u].if_start.types);
        observations.extend(body.observations.iter().map(|obs| relocator.observation(u, obs)));
        errors.extend(body.errors.iter().map(|entry| relocator.error_entry(u, entry)));
        if u + 1 == relocator.units.len() {
            root = body.root.map(|r| relocator.term_ann(u, r));
        }
    }

    AssembledProgramState { statics, intrinsics, root, errors, observations }
}

impl<'a> Relocator<'a> {
    fn plan(
        statics: &StaticsArena, units: Vec<UnitSlice<'a>>, load_bodies: bool, def_mode: DefMode,
    ) -> (Self, ArenaOffsets) {
        let mut cursor = ArenaOffsets::current(statics);
        let start = cursor;
        let mut ranges = Vec::with_capacity(units.len());
        let mut dummy_compu_base = Vec::with_capacity(units.len());

        for slice in &units {
            let if_start = cursor;
            if let Some(acc) = slice.interface {
                cursor.kinds += acc.kinds.len();
                cursor.kpats += acc.kpats.len();
                cursor.tpats += acc.tpats.len();
                cursor.types += acc.types.len();
                cursor.vpats += acc.vpats.len();
                cursor.values += acc.values.len();
                cursor.absts += acc.absts.len();
                cursor.datas += acc.datas.len();
                cursor.codatas += acc.codatas.len();
            }
            let if_end = cursor;
            let body_start = cursor;
            if let Some(body) = slice.body {
                cursor.kinds += body.kinds.len();
                cursor.kpats += body.kpats.len();
                cursor.tpats += body.tpats.len();
                cursor.types += body.types.len();
                cursor.vpats += body.vpats.len();
                cursor.values += body.values.len();
                cursor.compus += body.compus.len();
                cursor.absts += body.absts.len();
                cursor.datas += body.datas.len();
                cursor.codatas += body.codatas.len();
                cursor.fills += body.fills.len();
            }
            let body_end = cursor;
            dummy_compu_base.push(cursor.compus);
            if !load_bodies && let Some(acc) = slice.interface {
                cursor.compus += acc.thunk_types.len();
            }
            ranges.push(DepArenaRanges { if_start, if_end, body_start, body_end });
        }

        (Self { units, ranges, dummy_compu_base, load_bodies, def_mode }, start)
    }

    fn install_unit(
        &self, statics: &mut StaticsArena, u: usize, acc: &AcceptedUnitInterface,
        body: Option<&UnitBody>,
    ) {
        self.install_interface(statics, u, acc);
        if let Some(body) = body {
            self.install_body_nodes(statics, u, body, true);
        } else if !self.load_bodies {
            for _ in &acc.thunk_types {
                statics.compus.alloc(Computation::Hole(Hole));
            }
        }
        statics.types_pre.refresh_summaries_from(self.ranges[u].if_start.types);
    }

    fn install_interface(&self, statics: &mut StaticsArena, u: usize, acc: &AcceptedUnitInterface) {
        for k in &acc.kinds {
            statics.kinds_pre.alloc(Fillable::Done(self.rel_kind(u, k)));
        }
        if let Some(body) = self.units[u].body {
            let norm_map = self.body_kind_norms(u, body);
            for (offset, k) in body.kinds.iter().enumerate() {
                if self.load_bodies
                    && matches!(self.def_mode, DefMode::Synthetic)
                    && let Some(norm) = norm_map.get(&(offset as u32))
                {
                    statics.kinds_pre.alloc(Fillable::Done(self.rel_kind(u, norm)));
                } else {
                    statics.kinds_pre.alloc(self.fillable_kind(u, k));
                }
            }
        }

        let if_types = self.ranges[u].if_start.types;
        let body_type_count = self.units[u].body.map_or(0, |b| b.types.len());
        for (_, rel_k) in &acc.types {
            statics.types_pre.alloc(Fillable::Done(Type::Unit(UnitTy)), self.kind(u, *rel_k));
        }
        if let Some(body) = self.units[u].body {
            for (_, rel_k) in &body.types {
                statics.types_pre.alloc(Fillable::Done(Type::Unit(UnitTy)), self.kind(u, *rel_k));
            }
        }
        let body_type_base = if_types + acc.types.len();
        debug_assert_eq!(statics.types_pre.len(), body_type_base + body_type_count);

        for (slot, (rel_t, _)) in acc.types.iter().enumerate() {
            let id = TypeId::from_index(if_types + slot);
            let ty = self.rel_type(u, rel_t);
            statics.types_pre.replace_existing(id, Fillable::Done(ty.clone()));
            statics.types_normalized.insert_new(id, ty);
        }

        self.install_interface_tables(statics, u, acc, if_types);
    }

    fn install_interface_tables(
        &self, statics: &mut StaticsArena, u: usize, acc: &AcceptedUnitInterface, if_types: usize,
    ) {
        for kp in &acc.kpats {
            statics.kpats.alloc(self.rel_kpat(u, kp));
        }
        for (tp, k) in &acc.tpats {
            let id = statics.tpats.alloc(self.rel_tpat(u, tp));
            statics.annotations_tpat.insert_new(id, self.kind(u, *k));
        }
        for abst in &acc.absts {
            self.alloc_abst(statics, u, abst);
        }
        for data in &acc.datas {
            let arms = data.arms.iter().map(|(c, t)| (*c, self.ty(u, *t)));
            statics.datas.alloc(Data::new(arms));
        }
        for codata in &acc.codatas {
            let arms = codata.arms.iter().map(|(d, t)| (*d, self.ty(u, *t)));
            statics.codatas.alloc(CoData::new(arms));
        }
        for (vp, t) in &acc.vpats {
            let id = statics.vpats.alloc(self.rel_vpat(u, vp));
            statics.annotations_vpat.insert_new(id, self.ty(u, *t));
        }
        for (val, t) in &acc.values {
            let id = statics.values.alloc(self.rel_value(u, val));
            statics.annotations_value.insert_new(id, self.ty(u, *t));
        }

        for &(slot, role) in &acc.value_builtin_roles {
            let _ = statics
                .builtin_roles
                .attach_value(TypeId::from_index(if_types + slot as usize), role);
        }
        let if_values = self.ranges[u].if_start.values;
        for (slot, import) in &acc.foreign_imports {
            statics
                .foreign_imports
                .insert_new(ValueId::from_index(if_values + *slot as usize), import.clone());
        }
        for &(slot, data) in &acc.data_hints {
            statics
                .data_hints
                .insert_new(ValueId::from_index(if_values + slot as usize), self.data(u, data));
        }
        let if_vpats = self.ranges[u].if_start.vpats;
        for &(slot, data) in &acc.data_pat_hints {
            statics
                .data_pat_hints
                .insert_new(VPatId::from_index(if_vpats + slot as usize), self.data(u, data));
        }
        for &(slot, arity) in &acc.package_pattern_opened_arity {
            statics
                .package_pattern_opened_arity
                .insert_new(VPatId::from_index(if_vpats + slot as usize), arity);
        }
        for (slot, facts) in &acc.def_facts {
            self.install_def_facts(statics, u, *slot, facts);
        }
        for &term in &acc.global_terms {
            statics.global_terms.insert(self.term_id(u, term));
        }
    }

    fn install_body_nodes(
        &self, statics: &mut StaticsArena, u: usize, body: &UnitBody, overwrite_norms: bool,
    ) {
        if self.units[u].interface.is_none() {
            for k in &body.kinds {
                statics.kinds_pre.alloc(self.fillable_kind(u, k));
            }
            for (_, rel_k) in &body.types {
                statics.types_pre.alloc(Fillable::Done(Type::Unit(UnitTy)), self.kind(u, *rel_k));
            }
        }
        for (id, rel_k) in &body.kinds_normalized {
            let _ = statics.kinds_normalized.upsert(self.kind(u, *id), self.rel_kind(u, rel_k));
        }
        let b_types = self.ranges[u].body_start.types;
        for (slot, (rel_t, _)) in body.types.iter().enumerate() {
            let id = TypeId::from_index(b_types + slot);
            statics.types_pre.replace_existing(id, self.fillable_type(u, rel_t));
        }
        for (rel_id, rel_t) in &body.types_normalized {
            let id = self.ty(u, *rel_id);
            let norm = self.rel_type(u, rel_t);
            if overwrite_norms {
                statics.types_pre.replace_existing(id, Fillable::Done(norm.clone()));
            }
            let _ = statics.types_normalized.upsert(id, norm);
        }
        for kp in &body.kpats {
            statics.kpats.alloc(self.rel_kpat(u, kp));
        }
        for (tp, k) in &body.tpats {
            let id = statics.tpats.alloc(self.rel_tpat(u, tp));
            statics.annotations_tpat.insert_new(id, self.kind(u, *k));
        }
        for abst in &body.absts {
            self.alloc_abst(statics, u, abst);
        }
        for data in &body.datas {
            let arms = data.arms.iter().map(|(c, t)| (*c, self.ty(u, *t)));
            statics.datas.alloc(Data::new(arms));
        }
        for codata in &body.codatas {
            let arms = codata.arms.iter().map(|(d, t)| (*d, self.ty(u, *t)));
            statics.codatas.alloc(CoData::new(arms));
        }
        for (vp, t) in &body.vpats {
            let id = statics.vpats.alloc(self.rel_vpat(u, vp));
            statics.annotations_vpat.insert_new(id, self.ty(u, *t));
        }
        for (val, t) in &body.values {
            let id = statics.values.alloc(self.rel_value(u, val));
            statics.annotations_value.insert_new(id, self.ty(u, *t));
        }
        for (compu, t) in &body.compus {
            let id = statics.compus.alloc(self.rel_compu(u, compu));
            statics.annotations_compu.insert_new(id, self.ty(u, *t));
        }
        self.install_body_annotations_and_fills(statics, u, body);
    }

    fn install_body_annotations_and_fills(
        &self, statics: &mut StaticsArena, u: usize, body: &UnitBody,
    ) {
        for &site in &body.fills {
            statics.fills.alloc(site);
        }
        for (f, ann) in &body.solus {
            let _ = statics.solus.upsert(self.fill(u, *f), self.ann(u, *ann));
        }
        for (f, ws) in &body.fill_scopes {
            let scope: SkolemScope = ws.iter().map(|a| self.abst(u, *a)).collect();
            let _ = statics.fill_scopes.upsert(self.fill(u, *f), scope);
        }
        for &f in &body.fill_hints {
            statics.fill_hints.ensure(self.fill(u, f));
        }
        for &(t, role) in &body.value_builtin_roles {
            let _ = statics.builtin_roles.attach_value(self.ty(u, t), role);
        }
        for (v, import) in &body.foreign_imports {
            let _ = statics.foreign_imports.upsert(self.value(u, *v), import.clone());
        }
        for &(v, d) in &body.data_hints {
            let _ = statics.data_hints.upsert(self.value(u, v), self.data(u, d));
        }
        for &(vp, d) in &body.data_pat_hints {
            let _ = statics.data_pat_hints.upsert(self.vpat(u, vp), self.data(u, d));
        }
        for &(c, cd) in &body.codata_hints {
            let _ = statics.codata_hints.upsert(self.compu(u, c), self.codata(u, cd));
        }
        for &c in &body.copattern_matches {
            statics.copattern_matches.ensure(self.compu(u, c));
        }
        for &(c, vp) in &body.copattern_pack_pi_binders {
            let _ = statics.copattern_pack_pi_binders.upsert(self.compu(u, c), self.vpat(u, vp));
        }
        for &(vp, arity) in &body.package_pattern_opened_arity {
            let _ = statics.package_pattern_opened_arity.upsert(self.vpat(u, vp), arity);
        }
        self.install_body_def_tables(statics, u, body);
    }

    fn install_body_def_tables(&self, statics: &mut StaticsArena, u: usize, body: &UnitBody) {
        for &(d, v) in &body.value_aliases {
            let _ = statics.value_aliases.upsert(self.def(u, d), self.value(u, v));
        }
        for (d, ws) in &body.package_aliases {
            let resolved = ws.iter().map(|w| self.static_term(u, *w)).collect();
            let _ = statics.package_aliases.upsert(self.def(u, *d), resolved);
        }
        for &(d, t) in &body.type_definitions {
            let _ = statics.type_definitions.upsert(self.def(u, d), self.ty(u, t));
        }
        for &(d, v) in &body.inlinables {
            let _ = statics.inlinables.upsert(self.def(u, d), self.value(u, v));
        }
        for &d in &body.global_defs {
            let _ = statics.global_defs.upsert(self.def(u, d), ());
        }
        for &t in &body.global_terms {
            statics.global_terms.insert(self.term_id(u, t));
        }
        for &(t, d) in &body.def_hints {
            statics.def_hints.insert(self.term_id(u, t), self.def(u, d));
        }
        for &(d, ann) in &body.annotations_var {
            let _ = statics.annotations_var.upsert(self.def(u, d), self.ann(u, ann));
        }
    }

    fn install_body_facts(&self, statics: &mut StaticsArena, u: usize, body: &UnitBody) {
        for &(src, pat) in &body.pat_provenance {
            statics.pats.record(src, self.pat_id(u, pat));
        }
        for &(src, term) in &body.term_provenance {
            statics.terms.record(src, self.term_id(u, term));
        }
        for &(term, ann) in &body.term_facts {
            statics.record_term_annotation(term, self.term_ann(u, ann));
        }
        for err in &body.coverage_errors {
            statics.coverage_errors.push(self.coverage_error(u, err));
        }
    }

    fn install_def_facts(
        &self, statics: &mut StaticsArena, u: usize, slot: u32, facts: &RelDefFacts,
    ) {
        let def = self.def(u, RelDefId::LocalInterface(slot));
        if let Some(ann) = facts.annotation {
            let _ = statics.annotations_var.upsert(def, self.ann(u, ann));
        }
        if let Some(val) = facts.value_alias {
            let _ = statics.value_aliases.upsert(def, self.value(u, val));
        }
        if let Some(ws) = &facts.package_alias {
            let resolved = ws.iter().map(|w| self.static_term(u, *w)).collect();
            let _ = statics.package_aliases.upsert(def, resolved);
        }
        if let Some(ty) = facts.type_definition {
            let _ = statics.type_definitions.upsert(def, self.ty(u, ty));
        }
        if let Some(val) = facts.inlinable {
            let _ = statics.inlinables.upsert(def, self.value(u, val));
        }
        if facts.global {
            let _ = statics.global_defs.upsert(def, ());
        }
    }

    fn alloc_abst(&self, statics: &mut StaticsArena, u: usize, entry: &RelAbstEntry) {
        let id = statics.absts.alloc(());
        statics.annotations_abst.insert_new(id, self.kind(u, entry.kind));
        if let Some(seal) = entry.seal {
            statics.seals.insert_new(id, self.ty(u, seal));
        }
        if let Some(hint) = entry.hint {
            statics.abst_hints.insert_new(id, self.def(u, hint));
        }
        if entry.existential_skolem {
            statics.existential_skolems.ensure(id);
        }
        if let Some(BuiltinRole::Type(role)) = entry.builtin_role {
            let _ = statics.builtin_roles.attach_type(id, role);
        }
    }

    fn body_kind_norms<'b>(&self, u: usize, body: &'b UnitBody) -> FxHashMap<u32, &'b RelKind> {
        let mut out = FxHashMap::default();
        if self.units[u].interface.is_some() {
            for (rel_id, rel_k) in &body.kinds_normalized {
                if let RelId::LocalBody(offset) = *rel_id {
                    out.insert(offset, rel_k);
                }
            }
        }
        out
    }

    fn resolve_id<Id: DenseId>(
        &self, u: usize, rel: RelId<Id>, field: fn(&ArenaOffsets) -> usize,
    ) -> Id {
        match rel {
            | RelId::Intrinsic(id) => id,
            | RelId::LocalInterface(offset) => {
                Id::from_index(field(&self.ranges[u].if_start) + offset as usize)
            }
            | RelId::LocalBody(offset) => {
                Id::from_index(field(&self.ranges[u].body_start) + offset as usize)
            }
            | RelId::DepInterface { dep, offset } => {
                let target = self.units[u].dep_to_global[dep as usize];
                Id::from_index(field(&self.ranges[target].if_start) + offset as usize)
            }
            | RelId::DepBody { dep, offset } => {
                let target = self.units[u].dep_to_global[dep as usize];
                Id::from_index(field(&self.ranges[target].body_start) + offset as usize)
            }
            | RelId::DepStub { .. } => unreachable!("DepStub only valid for CompuId"),
        }
    }

    fn kind(&self, u: usize, rel: RelId<KindId>) -> KindId {
        self.resolve_id(u, rel, |r| r.kinds)
    }
    fn kpat(&self, u: usize, rel: RelId<KPatId>) -> KPatId {
        self.resolve_id(u, rel, |r| r.kpats)
    }
    fn tpat(&self, u: usize, rel: RelId<TPatId>) -> TPatId {
        self.resolve_id(u, rel, |r| r.tpats)
    }
    fn ty(&self, u: usize, rel: RelId<TypeId>) -> TypeId {
        self.resolve_id(u, rel, |r| r.types)
    }
    fn vpat(&self, u: usize, rel: RelId<VPatId>) -> VPatId {
        self.resolve_id(u, rel, |r| r.vpats)
    }
    fn value(&self, u: usize, rel: RelId<ValueId>) -> ValueId {
        self.resolve_id(u, rel, |r| r.values)
    }
    fn abst(&self, u: usize, rel: RelId<AbstId>) -> AbstId {
        self.resolve_id(u, rel, |r| r.absts)
    }
    fn data(&self, u: usize, rel: RelId<DataId>) -> DataId {
        self.resolve_id(u, rel, |r| r.datas)
    }
    fn codata(&self, u: usize, rel: RelId<CoDataId>) -> CoDataId {
        self.resolve_id(u, rel, |r| r.codatas)
    }
    fn fill(&self, u: usize, rel: RelId<FillId>) -> FillId {
        self.resolve_id(u, rel, |r| r.fills)
    }

    fn compu(&self, u: usize, rel: RelId<CompuId>) -> CompuId {
        match rel {
            | RelId::DepStub { dep, slot } => {
                let target = self.units[u].dep_to_global[dep as usize];
                self.resolve_stub_compu(target, slot)
            }
            | other => self.resolve_id(u, other, |r| r.compus),
        }
    }

    fn resolve_stub_compu(&self, mut unit_idx: usize, mut slot: u32) -> CompuId {
        if !self.load_bodies {
            return CompuId::from_index(self.dummy_compu_base[unit_idx] + slot as usize);
        }
        loop {
            let body = self.units[unit_idx].body.expect("unit body required to resolve stub");
            match body.thunk_bodies[slot as usize] {
                | RelId::LocalBody(offset) => {
                    return CompuId::from_index(
                        self.ranges[unit_idx].body_start.compus + offset as usize,
                    );
                }
                | RelId::DepBody { dep, offset } => {
                    let target = self.units[unit_idx].dep_to_global[dep as usize];
                    return CompuId::from_index(
                        self.ranges[target].body_start.compus + offset as usize,
                    );
                }
                | RelId::DepStub { dep, slot: next_slot } => {
                    unit_idx = self.units[unit_idx].dep_to_global[dep as usize];
                    slot = next_slot;
                }
                | other => unreachable!("unexpected thunk_bodies target: {other:?}"),
            }
        }
    }

    fn def(&self, u: usize, rel: RelDefId) -> su::DefId {
        match rel {
            | RelDefId::Raw(def) => def,
            | RelDefId::LocalInterface(slot) => self.interface_def(u, slot),
            | RelDefId::DepInterface { dep, offset } => {
                let target = self.units[u].dep_to_global[dep as usize];
                self.interface_def(target, offset)
            }
        }
    }

    fn interface_def(&self, u: usize, slot: u32) -> su::DefId {
        match self.def_mode {
            | DefMode::Synthetic => {
                derived_id(self.units[u].synthetic_def_space.expect("synthetic def space"), slot)
            }
            | DefMode::Real => {
                self.units[u].body.expect("unit body").interface_def_map[slot as usize]
            }
        }
    }

    fn ann(&self, u: usize, rel: RelAnnId) -> AnnId {
        match rel {
            | RelAnnId::Set => AnnId::Set,
            | RelAnnId::Kind(k) => AnnId::Kind(self.kind(u, k)),
            | RelAnnId::Type(t) => AnnId::Type(self.ty(u, t)),
        }
    }

    fn term_ann(&self, u: usize, rel: RelTermAnnId) -> TermAnnId {
        match rel {
            | RelTermAnnId::Hole(f) => TermAnnId::Hole(self.fill(u, f)),
            | RelTermAnnId::Kind(k) => TermAnnId::Kind(self.kind(u, k)),
            | RelTermAnnId::Type(t, k) => TermAnnId::Type(self.ty(u, t), self.kind(u, k)),
            | RelTermAnnId::Value(v, t) => TermAnnId::Value(self.value(u, v), self.ty(u, t)),
            | RelTermAnnId::Compu(c, t) => TermAnnId::Compu(self.compu(u, c), self.ty(u, t)),
            | RelTermAnnId::CompuStub(slot, t) => {
                TermAnnId::Compu(self.resolve_stub_compu(u, slot), self.ty(u, t))
            }
        }
    }

    fn term_id(&self, u: usize, rel: RelTermId) -> TermId {
        match rel {
            | RelTermId::Kind(k) => TermId::Kind(self.kind(u, k)),
            | RelTermId::Type(t) => TermId::Type(self.ty(u, t)),
            | RelTermId::Value(v) => TermId::Value(self.value(u, v)),
            | RelTermId::Compu(c) => TermId::Compu(self.compu(u, c)),
        }
    }

    fn pat_id(&self, u: usize, rel: RelPatId) -> PatId {
        match rel {
            | RelPatId::Kind(k) => PatId::Kind(self.kpat(u, k)),
            | RelPatId::Type(t) => PatId::Type(self.tpat(u, t)),
            | RelPatId::Value(v) => PatId::Value(self.vpat(u, v)),
        }
    }

    fn static_term(&self, u: usize, rel: RelStaticTermId) -> StaticTermId {
        match rel {
            | RelStaticTermId::Kind(k) => StaticTermId::Kind(self.kind(u, k)),
            | RelStaticTermId::Type(t) => StaticTermId::Type(self.ty(u, t)),
        }
    }

    fn static_pat(&self, u: usize, rel: &RelStaticPatId) -> StaticPatId {
        match rel {
            | RelStaticPatId::Kind(k) => StaticPatId::Kind(self.kpat(u, *k)),
            | RelStaticPatId::Type(t) => StaticPatId::Type(self.tpat(u, *t)),
        }
    }

    fn fillable_kind(&self, u: usize, rel: &RelFillable<RelKind>) -> Fillable<Kind> {
        match rel {
            | RelFillable::Fill(f) => Fillable::Fill(self.fill(u, *f)),
            | RelFillable::Done(k) => Fillable::Done(self.rel_kind(u, k)),
        }
    }

    fn fillable_type(&self, u: usize, rel: &RelFillable<RelType>) -> Fillable<Type> {
        match rel {
            | RelFillable::Fill(f) => Fillable::Fill(self.fill(u, *f)),
            | RelFillable::Done(t) => Fillable::Done(self.rel_type(u, t)),
        }
    }

    fn rel_kind(&self, u: usize, rel: &RelKind) -> Kind {
        match rel {
            | RelKind::VType => Kind::VType(VType),
            | RelKind::CType => Kind::CType(CType),
            | RelKind::Arrow(a, b) => Kind::Arrow(Arrow(self.kind(u, *a), self.kind(u, *b))),
            | RelKind::Label(n, inner) => Kind::Label(Label(*n, self.kind(u, *inner))),
        }
    }

    fn rel_kpat(&self, u: usize, rel: &RelKindPattern) -> KindPattern {
        match rel {
            | RelKindPattern::Hole => KindPattern::Hole(Hole),
            | RelKindPattern::Var(d) => KindPattern::Var(self.def(u, *d)),
        }
    }

    fn rel_tpat(&self, u: usize, rel: &RelTypePattern) -> TypePattern {
        match rel {
            | RelTypePattern::Hole => TypePattern::Hole(Hole),
            | RelTypePattern::Var(d) => TypePattern::Var(self.def(u, *d)),
            | RelTypePattern::Named(n, inner) => {
                TypePattern::Named(Named(*n, self.tpat(u, *inner)))
            }
        }
    }

    fn type_binder(&self, u: usize, rel: RelTypeBinder) -> TypeBinder {
        TypeBinder { pattern: self.tpat(u, rel.pattern), witness: self.abst(u, rel.witness) }
    }

    fn rel_type(&self, u: usize, rel: &RelType) -> Type {
        match rel {
            | RelType::Var(d) => Type::Var(self.def(u, *d)),
            | RelType::Abst(a) => Type::Abst(self.abst(u, *a)),
            | RelType::Abs { binder, body } => Type::Abs(TypeAbstraction {
                binder: self.type_binder(u, *binder),
                body: self.ty(u, *body),
            }),
            | RelType::App(f, a) => Type::App(App(self.ty(u, *f), self.ty(u, *a))),
            | RelType::Named(n, t) => Type::Named(Named(*n, self.ty(u, *t))),
            | RelType::Label(n, t) => Type::Label(Label(*n, self.ty(u, *t))),
            | RelType::Proj(t, f) => Type::Proj(Proj(self.ty(u, *t), *f)),
            | RelType::Thk => Type::Thk(ThkTy),
            | RelType::Ret => Type::Ret(RetTy),
            | RelType::Unit => Type::Unit(UnitTy),
            | RelType::Opaque => Type::Opaque(OpaqueTy),
            | RelType::Primitive(p) => Type::Primitive(PrimitiveTy(*p)),
            | RelType::OS => Type::OS(OSTy),
            | RelType::ValPi(pi) => Type::ValPi(Box::new(ValPi {
                binder: match &pi.binder {
                    | RelValPiBinder::Type(b) => ValPiBinder::Type(self.type_binder(u, *b)),
                    | RelValPiBinder::Value(p) => ValPiBinder::Value(ValueParameter {
                        domain: self.ty(u, p.domain),
                        witnesses: p.witnesses.as_ref().map(|ws| {
                            let mut iter = ws.iter().map(|a| self.abst(u, *a));
                            let first = iter.next().expect("nonempty PackTelescope");
                            PackTelescope::new(first, iter)
                        }),
                        witness_projection: p.witness_projection.clone(),
                    }),
                },
                codomain: self.ty(u, pi.codomain),
            })),
            | RelType::Arrow(a, b) => Type::Arrow(Arrow(self.ty(u, *a), self.ty(u, *b))),
            | RelType::Forall(b, body) => {
                Type::Forall(Forall(self.type_binder(u, *b), self.ty(u, *body)))
            }
            | RelType::PackPi(pi) => {
                let mut iter = pi.witnesses.iter().map(|a| self.abst(u, *a));
                let first = iter.next().expect("nonempty PackTelescope");
                Type::PackPi(Box::new(PackPi {
                    domain: self.ty(u, pi.domain),
                    witnesses: PackTelescope::new(first, iter),
                    codomain: self.ty(u, pi.codomain),
                }))
            }
            | RelType::Prod(items) => {
                Type::Prod(Prod(items.iter().map(|t| self.ty(u, *t)).collect()))
            }
            | RelType::Exists { binder, mode, body } => Type::Exists(Box::new(Exists {
                binder: self.type_binder(u, *binder),
                mode: match mode {
                    | RelExistsMode::Abstract => ExistsMode::Abstract,
                    | RelExistsMode::Manifest(d) => ExistsMode::Manifest(self.ty(u, *d)),
                },
                body: self.ty(u, *body),
            })),
            | RelType::ManifestKind { binder, definition, body } => {
                Type::ManifestKind(ManifestKind {
                    binder: self.kpat(u, *binder),
                    definition: self.kind(u, *definition),
                    body: self.ty(u, *body),
                })
            }
            | RelType::Data(d) => Type::Data(self.data(u, *d)),
            | RelType::CoData(c) => Type::CoData(self.codata(u, *c)),
        }
    }

    fn rel_vpat(&self, u: usize, rel: &RelValuePattern) -> ValuePattern {
        match rel {
            | RelValuePattern::Hole => ValuePattern::Hole(Hole),
            | RelValuePattern::Var(d) => ValuePattern::Var(self.def(u, *d)),
            | RelValuePattern::Named(n, p) => ValuePattern::Named(Named(*n, self.vpat(u, *p))),
            | RelValuePattern::Ctor(c, p) => ValuePattern::Ctor(Ctor(*c, self.vpat(u, *p))),
            | RelValuePattern::Lit(l) => ValuePattern::Lit(l.clone()),
            | RelValuePattern::Alias(ps) => {
                let mut mapped: Vec<_> = ps.iter().map(|p| self.vpat(u, *p)).collect();
                let last = mapped.pop().expect("alias pattern has nonempty components");
                ValuePattern::Alias(Alias(ConsN(mapped, last)))
            }
            | RelValuePattern::Triv => ValuePattern::Triv(Triv),
            | RelValuePattern::VCons(ps) => {
                ValuePattern::VCons(ps.iter().map(|p| self.vpat(u, *p)).collect())
            }
            | RelValuePattern::SCons(sp, p) => ValuePattern::SCons(ConsN(
                sp.iter().map(|x| self.static_pat(u, x)).collect(),
                self.vpat(u, *p),
            )),
            | RelValuePattern::View { function, pattern } => {
                ValuePattern::View(Box::new(ViewPattern {
                    function: self.value(u, *function),
                    pattern: self.vpat(u, *pattern),
                }))
            }
        }
    }

    fn rel_value(&self, u: usize, rel: &RelValue) -> Value {
        match rel {
            | RelValue::Hole => Value::Hole(Hole),
            | RelValue::Var(d) => Value::Var(self.def(u, *d)),
            | RelValue::Named(f, v) => Value::Named(Named(*f, self.value(u, *v))),
            | RelValue::Let { binder, bindee, tail } => Value::Let(Let {
                binder: self.vpat(u, *binder),
                bindee: self.value(u, *bindee),
                tail: self.value(u, *tail),
            }),
            | RelValue::ValAbs(binder, body) => Value::ValAbs(Abs(
                match *binder {
                    | RelValBinder::Type(t) => ValBinder::Type(self.tpat(u, t)),
                    | RelValBinder::Value(p) => ValBinder::Value(self.vpat(u, p)),
                },
                self.value(u, *body),
            )),
            | RelValue::ValApp(f, a) => Value::ValApp(App(
                self.value(u, *f),
                match *a {
                    | RelValArgument::Type(t) => ValArgument::Type(self.ty(u, t)),
                    | RelValArgument::Value(v) => ValArgument::Value(self.value(u, v)),
                },
            )),
            | RelValue::Match { scrut, arms } => Value::Match(Match {
                scrut: self.value(u, *scrut),
                arms: arms
                    .iter()
                    .map(|(p, v)| Matcher { binder: self.vpat(u, *p), tail: self.value(u, *v) })
                    .collect(),
            }),
            | RelValue::IntOp { operation, operands: [a, b] } => Value::IntOp(IntValueOp {
                operation: *operation,
                operands: [self.value(u, *a), self.value(u, *b)],
            }),
            | RelValue::Thunk(c) => Value::Thunk(Thunk(self.compu(u, *c))),
            | RelValue::ThunkStub(slot) => Value::Thunk(Thunk(self.resolve_stub_compu(u, *slot))),
            | RelValue::Ctor(c, v) => Value::Ctor(Ctor(*c, self.value(u, *v))),
            | RelValue::Triv => Value::Triv(Triv),
            | RelValue::VCons(vs) => Value::VCons(vs.iter().map(|v| self.value(u, *v)).collect()),
            | RelValue::SCons(st, v) => Value::SCons(ConsN(
                st.iter().map(|x| self.static_term(u, *x)).collect(),
                self.value(u, *v),
            )),
            | RelValue::Proj(v, field) => Value::Proj(Proj(
                self.value(u, *v),
                ResolvedField {
                    name: field.name,
                    target: ProjTarget {
                        products: field
                            .products
                            .iter()
                            .map(|(t, pos)| ProductProjection {
                                product: self.ty(u, *t),
                                position: *pos,
                            })
                            .collect(),
                    },
                },
            )),
            | RelValue::Lit(l) => Value::Lit(l.clone()),
        }
    }

    fn rel_compu(&self, u: usize, rel: &RelComputation) -> Computation {
        match rel {
            | RelComputation::Hole => Computation::Hole(Hole),
            | RelComputation::VAbs(p, c) => {
                Computation::VAbs(Abs(self.vpat(u, *p), self.compu(u, *c)))
            }
            | RelComputation::VApp(c, v) => {
                Computation::VApp(App(self.compu(u, *c), self.value(u, *v)))
            }
            | RelComputation::TAbs(p, c) => {
                Computation::TAbs(Abs(self.tpat(u, *p), self.compu(u, *c)))
            }
            | RelComputation::TApp(c, t) => {
                Computation::TApp(App(self.compu(u, *c), self.ty(u, *t)))
            }
            | RelComputation::Fix(p, c) => {
                Computation::Fix(Fix(self.vpat(u, *p), self.compu(u, *c)))
            }
            | RelComputation::Force(v) => Computation::Force(Force(self.value(u, *v))),
            | RelComputation::Ret(v) => Computation::Ret(Return(self.value(u, *v))),
            | RelComputation::Do { binder, bindee, tail } => Computation::Do(Bind {
                binder: self.vpat(u, *binder),
                bindee: self.compu(u, *bindee),
                tail: self.compu(u, *tail),
            }),
            | RelComputation::Let { binder, bindee, tail } => Computation::Let(Let {
                binder: self.vpat(u, *binder),
                bindee: self.value(u, *bindee),
                tail: self.compu(u, *tail),
            }),
            | RelComputation::Match { scrut, arms } => Computation::Match(Match {
                scrut: self.value(u, *scrut),
                arms: arms
                    .iter()
                    .map(|(p, c)| Matcher { binder: self.vpat(u, *p), tail: self.compu(u, *c) })
                    .collect(),
            }),
            | RelComputation::CoMatch(arms) => Computation::CoMatch(CoMatch {
                arms: arms
                    .iter()
                    .map(|(d, c)| CoMatcher { dtor: *d, tail: self.compu(u, *c) })
                    .collect(),
            }),
            | RelComputation::Dtor(c, d) => Computation::Dtor(Dtor(self.compu(u, *c), *d)),
        }
    }

    fn observation(&self, u: usize, obs: &RelObservation) -> TyckObservation {
        match obs {
            | RelObservation::HoleSolution { site, solution } => TyckObservation::HoleSolution {
                site: *site,
                solution: solution.map(|a| self.ann(u, a)),
            },
            | RelObservation::Debug { metadata, result } => TyckObservation::Debug {
                metadata: metadata.clone(),
                result: self.term_ann(u, *result),
            },
        }
    }

    fn coverage_error(&self, u: usize, err: &RelCoverageError) -> CoverageError {
        match err {
            | RelCoverageError::NonExhaustiveMatch { term, missing, truncated } => {
                CoverageError::NonExhaustiveMatch {
                    term: self.term_id(u, *term),
                    missing: missing.clone(),
                    truncated: *truncated,
                }
            }
            | RelCoverageError::NonExhaustiveCopatternMatch { computation, missing, truncated } => {
                CoverageError::NonExhaustiveCopatternMatch {
                    computation: self.compu(u, *computation),
                    missing: missing.clone(),
                    truncated: *truncated,
                }
            }
            | RelCoverageError::NonExhaustiveCoMatch { computation, missing } => {
                CoverageError::NonExhaustiveCoMatch {
                    computation: self.compu(u, *computation),
                    missing: missing.clone(),
                }
            }
            | RelCoverageError::DuplicateCoMatchArms { computation, duplicates } => {
                CoverageError::DuplicateCoMatchArms {
                    computation: self.compu(u, *computation),
                    duplicates: duplicates.clone(),
                }
            }
        }
    }

    fn error_entry(&self, u: usize, entry: &RelErrorEntry) -> TyckErrorEntry {
        let error = self.tyck_error(u, &entry.error);
        let stack: Vec<_> = entry
            .stack
            .iter()
            .map(|task| match task {
                | RelTyckTask::Pat(p, sw) => TyckTask::Pat(*p, self.switch(u, *sw)),
                | RelTyckTask::Term(t, sw) => TyckTask::Term(*t, self.switch(u, *sw)),
                | RelTyckTask::Lub(a, b) => TyckTask::Lub(self.ann(u, *a), self.ann(u, *b)),
                | RelTyckTask::SignatureGen(a) => TyckTask::SignatureGen(self.ann(u, *a)),
                | RelTyckTask::StructureGen(a) => TyckTask::StructureGen(self.ann(u, *a)),
                | RelTyckTask::MonadicLiftPat(p) => TyckTask::MonadicLiftPat(self.pat_id(u, *p)),
                | RelTyckTask::MonadicLiftTerm(t) => TyckTask::MonadicLiftTerm(self.term_id(u, *t)),
            })
            .collect();
        TyckErrorEntry { error, stack: stack.into() }
    }

    fn switch(&self, u: usize, sw: Switch<RelAnnId>) -> Switch<AnnId> {
        match sw {
            | Switch::Syn => Switch::Syn,
            | Switch::Ana(a) => Switch::Ana(self.ann(u, a)),
        }
    }

    fn tyck_error(&self, u: usize, err: &RelTyckError) -> TyckError {
        match err {
            | RelTyckError::MissingAnnotation => TyckError::MissingAnnotation,
            | RelTyckError::MissingSeal => TyckError::MissingSeal,
            | RelTyckError::InvalidBindingCycle(p) => TyckError::InvalidBindingCycle(*p),
            | RelTyckError::MissingSolution(fs) => {
                TyckError::MissingSolution(fs.iter().map(|f| self.fill(u, *f)).collect())
            }
            | RelTyckError::UnconstrainedInference(fs) => {
                TyckError::UnconstrainedInference(fs.iter().map(|f| self.fill(u, *f)).collect())
            }
            | RelTyckError::OccursCheck(f) => TyckError::OccursCheck(self.fill(u, *f)),
            | RelTyckError::MissingStructure(t) => TyckError::MissingStructure(self.ty(u, *t)),
            | RelTyckError::SortMismatch => TyckError::SortMismatch,
            | RelTyckError::SignatureNotType => TyckError::SignatureNotType,
            | RelTyckError::TypeOfKind => TyckError::TypeOfKind,
            | RelTyckError::KindMismatch => TyckError::KindMismatch,
            | RelTyckError::TypeMismatch { expected, found } => TyckError::TypeMismatch {
                expected: self.ty(u, *expected),
                found: self.ty(u, *found),
            },
            | RelTyckError::TypeExpected { expected, found } => {
                TyckError::TypeExpected { expected: expected.clone(), found: self.ty(u, *found) }
            }
            | RelTyckError::NamedLabelMismatch { expected, found } => {
                TyckError::NamedLabelMismatch { expected: *expected, found: *found }
            }
            | RelTyckError::MissingNamedTypeField { field, found } => {
                TyckError::MissingNamedTypeField { field: *field, found: self.kind(u, *found) }
            }
            | RelTyckError::AmbiguousNamedTypeField { field, found } => {
                TyckError::AmbiguousNamedTypeField { field: *field, found: self.kind(u, *found) }
            }
            | RelTyckError::MissingNamedField { field, found } => {
                TyckError::MissingNamedField { field: *field, found: self.ty(u, *found) }
            }
            | RelTyckError::DuplicateNamedField { field, found } => {
                TyckError::DuplicateNamedField { field: *field, found: self.ty(u, *found) }
            }
            | RelTyckError::SealedNamedField { field, found } => {
                TyckError::SealedNamedField { field: *field, found: self.ty(u, *found) }
            }
            | RelTyckError::StaticNamedField { field, found } => {
                TyckError::StaticNamedField { field: *field, found: self.ty(u, *found) }
            }
            | RelTyckError::PatternAliasRequiresValue => TyckError::PatternAliasRequiresValue,
            | RelTyckError::RefutablePatternAlias => TyckError::RefutablePatternAlias,
            | RelTyckError::RefutableBinding(p) => TyckError::RefutableBinding(*p),
            | RelTyckError::RefutableFieldProjectionPattern => {
                TyckError::RefutableFieldProjectionPattern
            }
            | RelTyckError::UnknownDataConstructor(c) => TyckError::UnknownDataConstructor(*c),
            | RelTyckError::UnknownCoDataDestructor(d) => TyckError::UnknownCoDataDestructor(*d),
            | RelTyckError::CopatternStepMismatch { expected, found } => {
                TyckError::CopatternStepMismatch { expected: *expected, found: found.clone() }
            }
            | RelTyckError::OverlappingCopatternClauses => TyckError::OverlappingCopatternClauses,
            | RelTyckError::MultiplePackPiCopatternClauses => {
                TyckError::MultiplePackPiCopatternClauses
            }
            | RelTyckError::NonExhaustiveCopattern { expected } => {
                TyckError::NonExhaustiveCopattern { expected: self.ty(u, *expected) }
            }
            | RelTyckError::Coverage(c) => TyckError::Coverage(self.coverage_error(u, c)),
            | RelTyckError::StaticElimination(e) => {
                TyckError::StaticElimination(self.static_elim_error(u, e))
            }
            | RelTyckError::PackageWitnessesUnavailable { package } => {
                TyckError::PackageWitnessesUnavailable { package: self.value(u, *package) }
            }
            | RelTyckError::PackageWitnessArityMismatch { expected, found } => {
                TyckError::PackageWitnessArityMismatch { expected: *expected, found: *found }
            }
            | RelTyckError::EscapingExistential { witnesses, result } => {
                TyckError::EscapingExistential {
                    witnesses: witnesses.iter().map(|a| self.abst(u, *a)).collect(),
                    result: self.ty(u, *result),
                }
            }
            | RelTyckError::EscapingDeclaration { declarations, result } => {
                TyckError::EscapingDeclaration {
                    declarations: declarations.iter().map(|a| self.abst(u, *a)).collect(),
                    result: self.ty(u, *result),
                }
            }
            | RelTyckError::InvalidBuiltinAttachment { role, expected } => {
                TyckError::InvalidBuiltinAttachment { role: *role, expected }
            }
            | RelTyckError::InvalidBuiltinSignature(sig) => {
                TyckError::InvalidBuiltinSignature(self.builtin_sig_error(u, sig))
            }
            | RelTyckError::ConflictingBuiltinRole { existing, found } => {
                TyckError::ConflictingBuiltinRole { existing: *existing, found: *found }
            }
            | RelTyckError::InvalidForeignAttachment => TyckError::InvalidForeignAttachment,
            | RelTyckError::InvalidForeignClassifier(fc) => {
                TyckError::InvalidForeignClassifier(self.foreign_classifier_error(u, fc))
            }
            | RelTyckError::ConflictingForeignImport { existing, found } => {
                TyckError::ConflictingForeignImport {
                    existing: existing.clone(),
                    found: found.clone(),
                }
            }
            | RelTyckError::MissingBuiltinTypeRole { role } => {
                TyckError::MissingBuiltinTypeRole { role: *role }
            }
            | RelTyckError::AmbiguousBuiltinTypeRole { role, witnesses } => {
                TyckError::AmbiguousBuiltinTypeRole {
                    role: *role,
                    witnesses: witnesses.iter().map(|a| self.abst(u, *a)).collect(),
                }
            }
            | RelTyckError::IntegerLiteralOutOfRange { value, integer_type } => {
                TyckError::IntegerLiteralOutOfRange { value: *value, integer_type: *integer_type }
            }
            | RelTyckError::FloatLiteralOutOfRange { value, float_type } => {
                TyckError::FloatLiteralOutOfRange { value: *value, float_type: *float_type }
            }
            | RelTyckError::Expressivity(msg) => TyckError::Expressivity(msg),
            | RelTyckError::NotInlinable(d) => TyckError::NotInlinable(self.def(u, *d)),
            | RelTyckError::NotInlinableSeal(a) => TyckError::NotInlinableSeal(self.abst(u, *a)),
        }
    }

    fn static_elim_error(
        &self, u: usize, err: &RelStaticEliminationError,
    ) -> StaticEliminationError {
        match err {
            | RelStaticEliminationError::UnresolvedApplication { function } => {
                StaticEliminationError::UnresolvedApplication { function: self.value(u, *function) }
            }
            | RelStaticEliminationError::UnresolvedMatch { value } => {
                StaticEliminationError::UnresolvedMatch { value: self.value(u, *value) }
            }
            | RelStaticEliminationError::UnresolvedInteger { value } => {
                StaticEliminationError::UnresolvedInteger { value: self.value(u, *value) }
            }
            | RelStaticEliminationError::RuntimeValue { value, ty } => {
                StaticEliminationError::RuntimeValue {
                    value: self.value(u, *value),
                    ty: self.ty(u, *ty),
                }
            }
            | RelStaticEliminationError::RuntimeComputation { computation, ty } => {
                StaticEliminationError::RuntimeComputation {
                    computation: self.compu(u, *computation),
                    ty: self.ty(u, *ty),
                }
            }
            | RelStaticEliminationError::ReductionLimit { function } => {
                StaticEliminationError::ReductionLimit { function: self.value(u, *function) }
            }
        }
    }

    fn builtin_sig_error(&self, u: usize, sig: &RelBuiltinSignatureError) -> BuiltinSignatureError {
        BuiltinSignatureError {
            duplicates: sig
                .duplicates
                .iter()
                .map(|d| match d {
                    | RelDuplicateBuiltinRole::Type { role, witnesses } => {
                        DuplicateBuiltinRole::Type {
                            role: *role,
                            witnesses: witnesses.iter().map(|a| self.abst(u, *a)).collect(),
                        }
                    }
                    | RelDuplicateBuiltinRole::Value { role, entries } => {
                        DuplicateBuiltinRole::Value {
                            role: *role,
                            entries: entries.iter().map(|t| self.ty(u, *t)).collect(),
                        }
                    }
                })
                .collect(),
            classifiers: sig
                .classifiers
                .iter()
                .map(|c| match c {
                    | RelBuiltinClassifierError::Mismatch { role, entry, expected } => {
                        BuiltinClassifierError::Mismatch {
                            role: *role,
                            entry: self.ty(u, *entry),
                            expected: expected.clone(),
                        }
                    }
                })
                .collect(),
        }
    }

    fn foreign_classifier_error(
        &self, u: usize, err: &RelForeignClassifierError,
    ) -> ForeignClassifierError {
        match err {
            | RelForeignClassifierError::Abi => ForeignClassifierError::Abi,
            | RelForeignClassifierError::Unit(unit) => ForeignClassifierError::Unit(match unit {
                | RelUnitClassifierError::Initializer => UnitClassifierError::Initializer,
                | RelUnitClassifierError::Unsupported { classifier } => {
                    UnitClassifierError::Unsupported { classifier: self.ty(u, *classifier) }
                }
                | RelUnitClassifierError::Interface(i) => UnitClassifierError::Interface(i.clone()),
            }),
            | RelForeignClassifierError::UnsupportedExportParameter { index, classifier } => {
                ForeignClassifierError::UnsupportedExportParameter {
                    index: *index,
                    classifier: self.ty(u, *classifier),
                }
            }
            | RelForeignClassifierError::ExpectedThunk { classifier } => {
                ForeignClassifierError::ExpectedThunk { classifier: self.ty(u, *classifier) }
            }
            | RelForeignClassifierError::UnsupportedParameter { index, classifier } => {
                ForeignClassifierError::UnsupportedParameter {
                    index: *index,
                    classifier: self.ty(u, *classifier),
                }
            }
            | RelForeignClassifierError::ExpectedReturn { classifier } => {
                ForeignClassifierError::ExpectedReturn { classifier: self.ty(u, *classifier) }
            }
            | RelForeignClassifierError::UnsupportedResult { classifier } => {
                ForeignClassifierError::UnsupportedResult { classifier: self.ty(u, *classifier) }
            }
            | RelForeignClassifierError::Signature(s) => {
                ForeignClassifierError::Signature(s.clone())
            }
        }
    }
}
