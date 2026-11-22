//! The statics arena for the Zydeco type checker.
//!
//! See [`StaticsArena`] for documentations of each field.

use super::syntax::*;
use crate::SkolemScope;
use crate::surface_syntax as su;
use indexmap::{IndexMap, IndexSet};
use rustc_hash::FxBuildHasher;
use std::{
    borrow::Borrow,
    marker::PhantomData,
    num::NonZeroU32,
    ops::{Deref, DerefMut, Index, IndexMut},
    sync::Arc,
};

use zydeco_derive::{AsMutSelf, AsRefSelf};
pub use zydeco_foundation::dense::{DenseArena, DenseId, IdBitSet, SecondaryMap};
pub use zydeco_surface::arena::*;
pub use zydeco_surface::scoped::arena::*;

/* ---------------------------------- Arena --------------------------------- */

/// Owning storage scope marker Retained for [`DefId`] test allocation fixtures.
#[derive(Debug)]
pub enum StaticsScope {}

impl Allocates<DefId> for StaticsScope {}

/// Static associations introduced by `@[builtin(...)]` package-signature
/// annotations.
#[derive(Clone, Debug, Default)]
pub struct BuiltinRoles {
    witnesses: SecondaryMap<AbstId, BuiltinRole>,
    values: SecondaryMap<TypeId, BuiltinValueRole>,
}

impl BuiltinRoles {
    pub fn attach_type(
        &mut self, witness: AbstId, role: BuiltinTypeRole,
    ) -> Result<(), BuiltinRole> {
        self.attach_witness(witness, BuiltinRole::Type(role))
    }

    pub fn attach_value(
        &mut self, entry: TypeId, role: BuiltinValueRole,
    ) -> Result<(), BuiltinValueRole> {
        match self.values.insert_or_get(entry, role) {
            | None => Ok(()),
            | Some(existing) if existing == role => Ok(()),
            | Some(existing) => Err(existing),
        }
    }

    pub fn witness(&self, witness: AbstId) -> Option<BuiltinRole> {
        self.witnesses.get(witness).copied()
    }

    pub fn value(&self, entry: TypeId) -> Option<BuiltinValueRole> {
        self.values.get(entry).copied()
    }

    pub fn transfer_value(
        &mut self, source: TypeId, target: TypeId,
    ) -> Result<(), BuiltinValueRole> {
        match self.value(source) {
            | Some(role) => self.attach_value(target, role),
            | None => Ok(()),
        }
    }

    pub fn type_witnesses(&self, role: BuiltinTypeRole) -> impl Iterator<Item = AbstId> + '_ {
        self.witnesses.iter().filter_map(move |(witness, found)| {
            (*found == BuiltinRole::Type(role)).then_some(witness)
        })
    }

    pub fn transfer_witness(&mut self, source: AbstId, target: AbstId) -> Result<(), BuiltinRole> {
        match self.witness(source) {
            | Some(role) => self.attach_witness(target, role),
            | None => Ok(()),
        }
    }

    fn attach_witness(&mut self, witness: AbstId, role: BuiltinRole) -> Result<(), BuiltinRole> {
        match self.witnesses.insert_or_get(witness, role) {
            | None => Ok(()),
            | Some(existing) if existing == role => Ok(()),
            | Some(existing) => Err(existing),
        }
    }
}

/// Compact owning storage for pre-normalization kinds.
///
/// `VType` and `CType` dominate this arena and carry no payload. Dense slots
/// retain only a 4-byte node for those variants; fill sites, arrows, and labels
/// occupy a dense side vector whose stable borrow is reconstructed by
/// [`KindArena::get`] and [`KindArena::iter`].
#[derive(Clone, Debug, Default)]
pub struct KindArena {
    nodes: DenseArena<KindId, KindNode>,
    payloads: Vec<Fillable<Kind>>,
}

#[derive(Copy, Clone, Debug)]
enum KindNode {
    VType,
    CType,
    Payload(KindPayloadIndex),
}

#[derive(Copy, Clone, Debug)]
struct KindPayloadIndex(u32);

static FILLED_VTYPE: Fillable<Kind> = Fillable::Done(Kind::VType(VType));
static FILLED_CTYPE: Fillable<Kind> = Fillable::Done(Kind::CType(CType));

impl KindPayloadIndex {
    fn from_offset(offset: usize) -> Self {
        Self(u32::try_from(offset).expect("kind payload arena exhausted its u32 index range"))
    }

    fn offset(self) -> usize {
        self.0 as usize
    }
}

impl KindNode {
    fn split(value: Fillable<Kind>, payload_offset: usize) -> (Self, Option<Fillable<Kind>>) {
        match value {
            | Fillable::Done(Kind::VType(_)) => (Self::VType, None),
            | Fillable::Done(Kind::CType(_)) => (Self::CType, None),
            | payload => {
                (Self::Payload(KindPayloadIndex::from_offset(payload_offset)), Some(payload))
            }
        }
    }
}

impl KindArena {
    pub fn alloc(&mut self, value: Fillable<Kind>) -> KindId {
        let (node, payload) = KindNode::split(value, self.payloads.len());
        let id = self.nodes.alloc(node);
        self.payloads.extend(payload);
        id
    }

    pub fn reserve_capacity(&mut self, additional: usize) {
        self.nodes.reserve_capacity(additional);
    }

    pub fn get(&self, id: impl Borrow<KindId>) -> Option<&Fillable<Kind>> {
        self.nodes.get(id).map(|node| self.value(node))
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = (KindId, &Fillable<Kind>)> + '_ {
        self.nodes.iter().map(move |(id, node)| (id, self.value(node)))
    }

    pub fn ids(&self) -> impl ExactSizeIterator<Item = KindId> + '_ {
        self.nodes.ids()
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    fn value(&self, node: &KindNode) -> &Fillable<Kind> {
        match node {
            | KindNode::VType => &FILLED_VTYPE,
            | KindNode::CType => &FILLED_CTYPE,
            | KindNode::Payload(index) => &self.payloads[index.offset()],
        }
    }
}

impl Index<KindId> for KindArena {
    type Output = Fillable<Kind>;

    fn index(&self, id: KindId) -> &Self::Output {
        self.get(id).expect("key not found")
    }
}

impl Index<&KindId> for KindArena {
    type Output = Fillable<Kind>;

    fn index(&self, id: &KindId) -> &Self::Output {
        self.get(id).expect("key not found")
    }
}

/// Editor-facing facts keyed by one source term.
#[derive(Clone, Debug)]
pub struct TermFacts {
    annotation: TermAnnId,
}

impl TermFacts {
    /// The final annotation recorded for the source term.
    pub fn annotation(&self) -> TermAnnId {
        self.annotation
    }
}

/// One-based index whose zero niche keeps optional source-term slots at four
/// bytes while the wider facts remain densely packed.
#[derive(Copy, Clone, Debug)]
struct TermFactsIndex(NonZeroU32);

impl TermFactsIndex {
    fn from_offset(offset: usize) -> Self {
        let index = u32::try_from(offset)
            .expect("term-facts arena exceeded its index range")
            .checked_add(1)
            .expect("term-facts arena exceeded its index range");
        Self(NonZeroU32::new(index).expect("one-based term-facts index is nonzero"))
    }

    fn offset(self) -> usize {
        (self.0.get() - 1) as usize
    }
}

/// Sparse source-term identity backed by densely stored editor facts.
#[derive(Clone, Debug, Default)]
pub struct TermFactsArena {
    indexes: ArenaPagedAssoc<su::TermId, TermFactsIndex>,
    facts: Vec<TermFacts>,
}

impl TermFactsArena {
    fn reserve_ids(&mut self, ids: impl IntoIterator<Item = su::TermId>) {
        self.indexes.reserve_ids(ids);
    }

    #[must_use]
    fn upsert(&mut self, term: su::TermId, facts: TermFacts) -> Option<TermFacts> {
        match self.indexes.get(&term).copied() {
            | Some(index) => Some(std::mem::replace(&mut self.facts[index.offset()], facts)),
            | None => {
                let index = TermFactsIndex::from_offset(self.facts.len());
                self.facts.push(facts);
                self.indexes.insert_new(term, index);
                None
            }
        }
    }

    fn get(&self, term: &su::TermId) -> Option<&TermFacts> {
        self.indexes.get(term).map(|index| &self.facts[index.offset()])
    }

    pub fn iter(&self) -> impl Iterator<Item = (su::TermId, &TermFacts)> {
        self.indexes.iter().map(|(term, index)| (term, &self.facts[index.offset()]))
    }
}

/// Compact index into the distinct kind annotations used by one type arena.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
struct TypeKindIndex(u32);

/// Compact upper-bound summary bits for constructs reachable from one [`TypeId`]
/// within [`TypeArena`].
///
/// Unsolved fills and external `Data`/`CoData` references conservatively set all bits
/// so normalization, substitution, and scope queries never skip reachable work.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
pub struct TypeSummary(u8);

impl TypeSummary {
    pub const NONE: Self = Self(0);
    pub const HAS_FILL: Self = Self(1 << 0);
    pub const HAS_FREE_VAR: Self = Self(1 << 1);
    pub const HAS_FREE_ABST: Self = Self(1 << 2);
    pub const HAS_PROJ: Self = Self(1 << 3);
    pub const HAS_BETA_OR_SEAL: Self = Self(1 << 4);
    pub const HAS_PRIMITIVE: Self = Self(1 << 5);
    pub const ALL: Self = Self(0b0011_1111);

    #[inline]
    pub const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    #[inline]
    pub const fn contains(self, flag: Self) -> bool {
        (self.0 & flag.0) == flag.0
    }

    #[inline]
    pub const fn intersects(self, mask: Self) -> bool {
        (self.0 & mask.0) != 0
    }
}

/// One typed node, its kind index, and its 1-byte structural summary, co-located under [`TypeId`].
#[derive(Clone, Debug)]
struct TypeNode {
    value: Fillable<Type>,
    kind: TypeKindIndex,
    summary: TypeSummary,
}

/// Dense type storage with compact, arena-local kind annotation indexes.
#[derive(Clone, Debug, Default)]
pub struct TypeArena {
    nodes: DenseArena<TypeId, TypeNode>,
    kinds: Vec<KindId>,
    kind_indexes: rustc_hash::FxHashMap<KindId, TypeKindIndex>,
}

impl TypeArena {
    pub fn alloc(&mut self, value: Fillable<Type>, kind: KindId) -> TypeId {
        let kind = self.intern_kind(kind);
        let summary = self.compute_summary(&value);
        self.nodes.alloc(TypeNode { value, kind, summary })
    }

    pub fn replace_existing(&mut self, id: TypeId, value: Fillable<Type>) {
        let summary = self.compute_summary(&value);
        let node = &mut self.nodes[id];
        node.value = value;
        node.summary = summary;
    }

    #[inline]
    pub fn summary(&self, id: TypeId) -> TypeSummary {
        self.nodes[id].summary
    }

    fn compute_summary(&self, value: &Fillable<Type>) -> TypeSummary {
        let Fillable::Done(ty) = value else {
            return TypeSummary::ALL;
        };
        match ty {
            | Type::Var(_) => TypeSummary::HAS_FREE_VAR,
            | Type::Abst(_) => TypeSummary::HAS_FREE_ABST.union(TypeSummary::HAS_BETA_OR_SEAL),
            | Type::Primitive(_) => TypeSummary::HAS_PRIMITIVE,
            | Type::Thk(_) | Type::Ret(_) | Type::Unit(_) | Type::Opaque(_) | Type::OS(_) => {
                TypeSummary::NONE
            }
            | Type::App(App(function, argument)) => self
                .summary(*function)
                .union(self.summary(*argument))
                .union(TypeSummary::HAS_BETA_OR_SEAL),
            | Type::Proj(Proj(head, _)) => self
                .summary(*head)
                .union(TypeSummary::HAS_PROJ)
                .union(TypeSummary::HAS_BETA_OR_SEAL),
            | Type::Abs(TypeAbstraction { body, .. })
            | Type::Named(Named(_, body))
            | Type::Label(Label(_, body))
            | Type::Forall(Forall(_, body))
            | Type::ManifestKind(ManifestKind { body, .. }) => self.summary(*body),
            | Type::ValPi(pi) => {
                let domain = match &pi.binder {
                    | ValPiBinder::Type(_) => TypeSummary::NONE,
                    | ValPiBinder::Value(parameter) => self.summary(parameter.domain),
                };
                domain.union(self.summary(pi.codomain))
            }
            | Type::Arrow(Arrow(domain, codomain)) => {
                self.summary(*domain).union(self.summary(*codomain))
            }
            | Type::PackPi(pi) => self.summary(pi.domain).union(self.summary(pi.codomain)),
            | Type::Prod(Prod(items)) => {
                items.iter().fold(TypeSummary::NONE, |acc, item| acc.union(self.summary(*item)))
            }
            | Type::Exists(exists) => {
                let mode = match &exists.mode {
                    | ExistsMode::Abstract => TypeSummary::NONE,
                    | ExistsMode::Manifest(definition) => self.summary(*definition),
                };
                mode.union(self.summary(exists.body))
            }
            | Type::Data(_) | Type::CoData(_) => TypeSummary::ALL,
        }
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = (TypeId, &Fillable<Type>)> + '_ {
        self.nodes.iter().map(|(id, node)| (id, &node.value))
    }

    pub fn ids(&self) -> impl ExactSizeIterator<Item = TypeId> + '_ {
        self.nodes.ids()
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    pub fn reserve(&mut self, additional: usize) {
        self.nodes.reserve_capacity(additional);
    }

    pub fn get(&self, id: impl Borrow<TypeId>) -> Option<&Fillable<Type>> {
        self.nodes.get(id).map(|node| &node.value)
    }

    pub fn get_mut(&mut self, id: impl Borrow<TypeId>) -> Option<&mut Fillable<Type>> {
        self.nodes.get_mut(id).map(|node| &mut node.value)
    }

    fn kind_at(&self, id: impl Borrow<TypeId>) -> Option<KindId> {
        let index = self.nodes.get(id)?.kind.0 as usize;
        self.kinds.get(index).copied()
    }

    fn intern_kind(&mut self, kind: KindId) -> TypeKindIndex {
        if self.kind_indexes.len() != self.kinds.len() {
            self.kind_indexes = self
                .kinds
                .iter()
                .enumerate()
                .map(|(index, kind)| {
                    let index =
                        u32::try_from(index).expect("type kind table exhausted u32 indexes");
                    (*kind, TypeKindIndex(index))
                })
                .collect();
        }
        if let Some(index) = self.kind_indexes.get(&kind) {
            return *index;
        }
        let index = TypeKindIndex(
            u32::try_from(self.kinds.len()).expect("type kind table exhausted u32 indexes"),
        );
        self.kinds.push(kind);
        self.kind_indexes.insert(kind, index);
        index
    }

    /// Drop the construction-only reverse index. The compact forward table
    /// remains available to every downstream kind lookup.
    pub(crate) fn strip_kind_index(&mut self) {
        self.kind_indexes = Default::default();
    }
}

impl Index<TypeId> for TypeArena {
    type Output = Fillable<Type>;

    fn index(&self, id: TypeId) -> &Self::Output {
        &self.nodes[id].value
    }
}

impl Index<&TypeId> for TypeArena {
    type Output = Fillable<Type>;

    fn index(&self, id: &TypeId) -> &Self::Output {
        &self.nodes[id].value
    }
}

impl IndexMut<TypeId> for TypeArena {
    fn index_mut(&mut self, id: TypeId) -> &mut Self::Output {
        &mut self.nodes[id].value
    }
}

impl IndexMut<&TypeId> for TypeArena {
    fn index_mut(&mut self, id: &TypeId) -> &mut Self::Output {
        &mut self.nodes[id].value
    }
}

/// One materializing source representative for each typed node.
///
/// Diagnostics and source-span queries only need one source location. Rechecks
/// of a materializing term replace the representative; reference-only wrappers
/// such as import boundaries leave the provider's location intact.
#[derive(Clone, Debug)]
pub struct SourceProvenance<Source, Typed> {
    latest_by_category: [Vec<Option<Source>>; TypedEntityCategory::COUNT],
    marker: PhantomData<fn() -> Typed>,
}

struct TypedEntityKey {
    index: usize,
    category: TypedEntityCategory,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
#[repr(u8)]
enum TypedEntityCategory {
    KindPattern,
    TypePattern,
    ValuePattern,
    KindTerm,
    TypeTerm,
    ValueTerm,
    CompuTerm,
}

impl TypedEntityCategory {
    const COUNT: usize = Self::CompuTerm as usize + 1;

    fn index(self) -> usize {
        self as usize
    }
}

impl From<PatId> for TypedEntityKey {
    fn from(id: PatId) -> Self {
        match id {
            | PatId::Kind(id) => {
                Self { index: id.index(), category: TypedEntityCategory::KindPattern }
            }
            | PatId::Type(id) => {
                Self { index: id.index(), category: TypedEntityCategory::TypePattern }
            }
            | PatId::Value(id) => {
                Self { index: id.index(), category: TypedEntityCategory::ValuePattern }
            }
        }
    }
}

impl From<TermId> for TypedEntityKey {
    fn from(id: TermId) -> Self {
        match id {
            | TermId::Kind(id) => {
                Self { index: id.index(), category: TypedEntityCategory::KindTerm }
            }
            | TermId::Type(id) => {
                Self { index: id.index(), category: TypedEntityCategory::TypeTerm }
            }
            | TermId::Value(id) => {
                Self { index: id.index(), category: TypedEntityCategory::ValueTerm }
            }
            | TermId::Compu(id) => {
                Self { index: id.index(), category: TypedEntityCategory::CompuTerm }
            }
        }
    }
}

impl<Source, Typed> Default for SourceProvenance<Source, Typed> {
    fn default() -> Self {
        Self { latest_by_category: std::array::from_fn(|_| Vec::new()), marker: PhantomData }
    }
}

impl<Source, Typed> SourceProvenance<Source, Typed>
where
    Source: Copy,
{
    fn record_compact(&mut self, source: Source, typed: TypedEntityKey) {
        let slots = &mut self.latest_by_category[typed.category.index()];
        if typed.index >= slots.len() {
            slots.resize(typed.index + 1, None);
        }
        slots[typed.index] = Some(source);
    }

    fn source_compact(&self, typed: TypedEntityKey) -> Option<Source> {
        self.latest_by_category[typed.category.index()].get(typed.index).copied().flatten()
    }

    fn shrink_to_fit(&mut self) {
        self.latest_by_category.iter_mut().for_each(Vec::shrink_to_fit);
    }
}

impl<Source> SourceProvenance<Source, PatId>
where
    Source: Copy,
{
    pub fn record(&mut self, source: Source, typed: PatId) {
        self.record_compact(source, typed.into());
    }

    pub fn source(&self, typed: &PatId) -> Option<Source> {
        self.source_compact((*typed).into())
    }
}

impl<Source> SourceProvenance<Source, TermId>
where
    Source: Copy,
{
    pub fn record(&mut self, source: Source, typed: TermId) {
        self.record_compact(source, typed.into());
    }

    pub fn source(&self, typed: &TermId) -> Option<Source> {
        self.source_compact((*typed).into())
    }
}

/// Immutable normalized classifiers retained for editor queries.
///
/// The checker records these facts once, after every type has been normalized.
#[derive(Clone, Debug, Default)]
pub struct NormalizedAnnotations {
    ids: Box<[TypeId]>,
    types: Box<[Type]>,
}

impl NormalizedAnnotations {
    fn with_parallel(ids: Vec<TypeId>, types: Vec<Type>) -> Self {
        assert_eq!(ids.len(), types.len(), "normalized annotation indexes diverged");
        assert!(ids.windows(2).all(|pair| pair[0] < pair[1]));
        Self { ids: ids.into_boxed_slice(), types: types.into_boxed_slice() }
    }

    pub fn get(&self, id: TypeId) -> Option<&Type> {
        self.ids.binary_search(&id).ok().and_then(|index| self.types.get(index))
    }

    pub fn len(&self) -> usize {
        self.ids.len()
    }

    pub fn is_empty(&self) -> bool {
        self.ids.is_empty()
    }
}

/// Source-bounded static facts retained after the typed occurrence tree is
/// discarded.
#[derive(Clone, Debug, Default)]
pub struct StaticsIndexes {
    /// Representative source pattern for each typed pattern. Rechecking
    /// replaces the representative, while transparent wrappers collapse to
    /// the last wrapper checked.
    pub pats: SourceProvenance<su::PatId, PatId>,
    /// Representative source term for each typed term. Rechecking replaces the
    /// representative, while erased constructs collapse to the last source
    /// term checked.
    pub terms: SourceProvenance<su::TermId, TermId>,
    /// Final annotation for each checked source term.
    pub term_facts: TermFactsArena,
    /// Normalized classifier for each distinct top annotation type. Inner type
    /// nodes have no entry, and terms sharing one annotation ID share one clone.
    pub annotation_norms: NormalizedAnnotations,
    /// Coverage failures recorded during the finish phase, for editor facts.
    pub coverage_errors: Vec<crate::validate::CoverageError>,

    /// arena for abstract types
    pub absts: DenseArena<AbstId, ()>,
    /// the abstract types generated from sealed types
    pub seals: SecondaryMap<AbstId, TypeId>,
    /// name hints for abstract types
    pub abst_hints: SecondaryMap<AbstId, DefId>,
    /// abstract types introduced by existential elimination
    pub existential_skolems: IdBitSet<AbstId>,
    /// Builtin roles attached to existential witnesses and named value entries.
    pub builtin_roles: BuiltinRoles,
    /// Foreign implementations whose classifiers have a supported marshalling protocol.
    pub foreign_imports: SecondaryMap<ValueId, zydeco_syntax::CheckedImport>,
    /// arena for context-constrained flexible metavariables and their source sites
    pub fills: DenseArena<FillId, InferenceSite>,
    /// the annotation assigned to each solved filling site
    pub solus: SecondaryMap<FillId, AnnId>,
    /// existential witnesses that each type hole is allowed to mention
    pub fill_scopes: SecondaryMap<FillId, SkolemScope>,
    /// which holes are introduced by the user and should be reported
    pub fill_hints: IdBitSet<FillId>,
    /// arena for `data`; plural plural
    pub datas: DenseArena<DataId, Data>,
    /// arena for `codata`; plural plural
    pub codatas: DenseArena<CoDataId, CoData>,
    /// hints for values that need data annotations
    pub data_hints: SecondaryMap<ValueId, DataId>,
    /// hints for value patterns that need data annotations
    pub data_pat_hints: SecondaryMap<VPatId, DataId>,
    /// hints for computations that need codata annotations
    pub codata_hints: SecondaryMap<CompuId, CoDataId>,
    /// matches generated to cover value patterns in generalized comatch clauses
    pub copattern_matches: IdBitSet<CompuId>,
    /// package-dependent binders consumed directly from generalized comatch clauses
    pub copattern_pack_pi_binders: SecondaryMap<CompuId, VPatId>,
    /// immutable value aliases available to static inspection
    pub value_aliases: ArenaAssoc<DefId, ValueId>,
    /// package witnesses retained by whole-value aliases in selective package patterns
    pub package_aliases: ArenaAssoc<DefId, Vec<StaticTermId>>,
    /// abstract witness arity contributed by each package-pattern boundary
    pub package_pattern_opened_arity: SecondaryMap<VPatId, usize>,
    /// checked bodies of type definitions available to static inspection
    pub type_definitions: ArenaAssoc<DefId, TypeId>,
    /// arena for inlinable definitions, typically global (necessity modality) definitions
    pub inlinables: ArenaAssoc<DefId, ValueId>,
    /// definitions that are marked global
    pub global_defs: ArenaAssoc<DefId, ()>,
    /// terms that are marked global
    pub global_terms: IndexSet<TermId, FxBuildHasher>,
    /// hints associating typed terms with definition names
    pub def_hints: IndexMap<TermId, DefId, FxBuildHasher>,
    /// Definition names synthesized during typed elaboration. Source definitions
    /// remain in the immutable scoped arena; keeping this small delta here avoids
    /// cloning every resolved syntax and context table just to add names.
    pub generated_defs: ArenaSparse<su::ScopedScope, DefId>,

    // the type of terms under the context it's type checked; "annotation"
    /// annotations for variable definitions
    pub annotations_var: ArenaAssoc<DefId, AnnId>,
    /// annotations for abstract types
    pub annotations_abst: SecondaryMap<AbstId, KindId>,
}

/// Typed arena plus annotation tables and translation metadata.
#[derive(Clone, Debug, Default, AsRefSelf, AsMutSelf)]
pub struct StaticsArena {
    /// Source-bounded facts shared with retained analyses after checking.
    indexes: Arc<StaticsIndexes>,

    /// Shared executable residual program produced by static elaboration. Source
    /// annotations and original nodes remain available for static queries.
    pub core: Option<Arc<zydeco_ir::CoreProgram>>,

    /// kind arena before normalization
    pub kinds_pre: KindArena,
    /// manifest kind-pattern arena
    pub kpats: DenseArena<KPatId, KindPattern>,
    /// type pattern arena
    pub tpats: DenseArena<TPatId, TypePattern>,
    /// type arena before normalization
    pub types_pre: TypeArena,
    /// value pattern arena
    pub vpats: DenseArena<VPatId, ValuePattern>,
    /// value arena
    pub values: DenseArena<ValueId, Value>,
    /// computation arena
    pub compus: DenseArena<CompuId, Computation>,
    /// kind annotations for type patterns
    pub annotations_tpat: SecondaryMap<TPatId, KindId>,
    /// type annotations for value patterns
    pub annotations_vpat: SecondaryMap<VPatId, TypeId>,
    /// type annotations for values
    pub annotations_value: SecondaryMap<ValueId, TypeId>,
    /// type annotations for computations
    pub annotations_compu: SecondaryMap<CompuId, TypeId>,

    // normalized kinds and types after type checking
    /// normalized kind delta for IDs whose pre-normalization form changed
    pub kinds_normalized: SecondaryMap<KindId, Kind>,
    /// normalized type free of holes
    pub types_normalized: SecondaryMap<TypeId, Type>,
}

impl Deref for StaticsArena {
    type Target = StaticsIndexes;

    fn deref(&self) -> &Self::Target {
        &self.indexes
    }
}

impl DerefMut for StaticsArena {
    fn deref_mut(&mut self) -> &mut Self::Target {
        Arc::make_mut(&mut self.indexes)
    }
}

impl StaticsArena {
    /// Pre-reserve retained, source-shaped storage from the name-resolved program.
    pub fn reserve(&mut self, scoped: &su::ScopedArena) {
        self.types_pre.reserve(scoped.terms.len());
        self.term_facts.reserve_ids(scoped.terms.iter().map(|(term, _)| term));
    }

    /// The kind annotation co-located with one type node.
    pub fn type_kind(&self, id: TypeId) -> KindId {
        self.types_pre.kind_at(id).expect("type node has no kind annotation")
    }

    /// The kind annotation of a type node, when the node is present.
    pub fn type_kind_at(&self, id: TypeId) -> Option<KindId> {
        self.types_pre.kind_at(id)
    }

    /// Clone only the keyed indexes of a finished check, leaving the much
    /// larger occurrence payload in its shared materialization.
    pub fn clone_keyed_indexes(&self) -> Self {
        Self { indexes: Arc::clone(&self.indexes), ..Self::default() }
    }

    /// The normalized form of one type, falling back to the pre-normalization
    /// form for nodes the normalization phase left unchanged.
    pub fn normalized_at(&self, id: TypeId) -> Option<&Type> {
        self.types_normalized.get(id).or_else(|| match self.types_pre.get(id)? {
            | Fillable::Done(ty) => Some(ty),
            | Fillable::Fill(_) => None,
        })
    }

    /// The normalized form of one kind, falling back to its unchanged
    /// pre-normalization form when no delta is stored.
    pub fn normalized_kind_at(&self, id: KindId) -> Option<&Kind> {
        self.kinds_normalized.get(id).or_else(|| match self.kinds_pre.get(id)? {
            | Fillable::Done(kind) => Some(kind),
            | Fillable::Fill(_) => None,
        })
    }

    /// Record the final annotation of a source term.
    pub fn record_term_annotation(&mut self, term: su::TermId, annotation: TermAnnId) {
        let _ = self.term_facts.upsert(term, TermFacts { annotation });
    }

    /// Release spare mutable capacity after source judgments stop producing
    /// typed pattern and term representatives.
    pub(crate) fn shrink_source_provenance(&mut self) {
        self.pats.shrink_to_fit();
        self.terms.shrink_to_fit();
    }

    /// Retain one normalized classifier for each distinct, resolved top annotation type.
    pub(crate) fn retain_normalized_annotations(&mut self) {
        let mut annotations: Vec<_> = self
            .term_facts
            .iter()
            .filter_map(|(_, facts)| match facts.annotation {
                | TermAnnId::Type(annotation, _)
                | TermAnnId::Value(_, annotation)
                | TermAnnId::Compu(_, annotation) => Some(annotation),
                | TermAnnId::Kind(_) | TermAnnId::Hole(_) => None,
            })
            .collect();
        annotations.sort_unstable();
        annotations.dedup();

        let (annotations, normalized) = annotations
            .into_iter()
            .filter_map(|annotation| match self.normalized_at(annotation) {
                | Some(normalized) => Some((annotation, normalized.clone())),
                | None if matches!(self.types_pre.get(annotation), Some(Fillable::Fill(_))) => None,
                | None => panic!("top annotation was not normalized"),
            })
            .unzip();
        self.annotation_norms = NormalizedAnnotations::with_parallel(annotations, normalized);
    }

    pub fn term_annotation(&self, term: su::TermId) -> Option<TermAnnId> {
        self.term_facts.get(&term).map(|facts| facts.annotation)
    }

    pub fn normalized_annotation_at(&self, annotation: TypeId) -> Option<&Type> {
        self.annotation_norms.get(annotation)
    }

    /// Look up either a source definition or one synthesized by typed elaboration.
    pub fn def_name<'a>(&'a self, scoped: &'a su::ScopedArena, id: &DefId) -> &'a su::VarName {
        self.generated_defs.get(id).unwrap_or_else(|| &scoped.defs[id])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn common_kinds_stay_inline_while_uncommon_payloads_are_dense() {
        assert!(std::mem::size_of::<KindNode>() <= 8);
        assert!(std::mem::size_of::<KindNode>() < std::mem::size_of::<Fillable<Kind>>());

        let mut kinds = KindArena::default();
        let vtype = kinds.alloc(Fillable::Done(Kind::VType(VType)));
        let ctype = kinds.alloc(Fillable::Done(Kind::CType(CType)));
        let arrow = kinds.alloc(Fillable::Done(Kind::Arrow(Arrow(vtype, ctype))));
        let label = kinds.alloc(Fillable::Done(Kind::Label(Label(FieldName::new("field"), vtype))));
        let fill = FillId::from_index(0);
        let pending = kinds.alloc(Fillable::Fill(fill));

        assert_eq!(kinds.len(), 5);
        assert_eq!(kinds.payloads.len(), 3);
        assert!(matches!(kinds[vtype], Fillable::Done(Kind::VType(_))));
        assert!(matches!(kinds[ctype], Fillable::Done(Kind::CType(_))));
        assert!(matches!(kinds[arrow], Fillable::Done(Kind::Arrow(_))));
        assert!(matches!(kinds[label], Fillable::Done(Kind::Label(_))));
        assert!(matches!(kinds[pending], Fillable::Fill(id) if id == fill));
        assert_eq!(kinds.iter().count(), 5);
    }

    #[test]
    fn type_nodes_store_compact_kind_indexes() {
        assert!(std::mem::size_of::<TypeKindIndex>() <= 4);

        let kind = KindId::from_index(0);
        let mut types = TypeArena::default();
        let first = types.alloc(Fillable::Done(Type::Unit(UnitTy)), kind);
        let second = types.alloc(Fillable::Done(Type::Unit(UnitTy)), kind);

        assert_eq!(types.kinds, vec![kind]);
        assert_eq!(types.kind_at(first), Some(kind));
        assert_eq!(types.kind_at(second), Some(kind));

        types.strip_kind_index();
        assert_eq!(types.kind_at(first), Some(kind));
        let _third = types.alloc(Fillable::Done(Type::Unit(UnitTy)), kind);
        assert_eq!(types.kinds, vec![kind]);
    }

    #[test]
    fn retained_indexes_share_the_finished_generation() {
        let materialized = StaticsArena::default();
        let retained = materialized.clone_keyed_indexes();

        assert!(Arc::ptr_eq(&materialized.indexes, &retained.indexes));
        assert_eq!(retained.types_pre.len(), 0);
    }

    #[test]
    fn source_provenance_retains_the_latest_representative() {
        assert_eq!(std::mem::size_of::<PatId>(), 8);
        assert_eq!(std::mem::size_of::<TermId>(), 8);

        let mut source = IdAllocator::<su::BitterScope>::new();
        let first_source: su::PatId = source.alloc();
        let second_source: su::PatId = source.alloc();
        let first_typed = PatId::Kind(KPatId::from_index(0));
        let second_typed = PatId::Value(VPatId::from_index(0));
        let mut provenance: SourceProvenance<su::PatId, PatId> = SourceProvenance::default();

        provenance.record(first_source, first_typed);
        provenance.record(first_source, first_typed);
        provenance.record(second_source, first_typed);
        provenance.record(first_source, second_typed);

        assert_eq!(provenance.source(&first_typed), Some(second_source));
        assert_eq!(provenance.source(&second_typed), Some(first_source));

        provenance.record(first_source, first_typed);
        provenance.shrink_to_fit();
        assert_eq!(provenance.source(&first_typed), Some(first_source));
    }

    #[test]
    fn normalized_annotation_facts_sort_and_deduplicate_top_types() {
        let mut surface = IdAllocator::<su::ScopedScope>::new();
        let first_term = surface.alloc();
        let second_term = surface.alloc();
        let third_term = surface.alloc();
        let kind_term = surface.alloc();
        let mut statics = StaticsArena::default();
        let kind = statics.kinds_pre.alloc(Fillable::Done(Kind::VType(VType)));
        let first_annotation = statics.types_pre.alloc(Fillable::Done(Type::Unit(UnitTy)), kind);
        let second_annotation = statics.types_pre.alloc(Fillable::Done(Type::Thk(ThkTy)), kind);

        statics.record_term_annotation(first_term, TermAnnId::Type(second_annotation, kind));
        statics.record_term_annotation(second_term, TermAnnId::Type(first_annotation, kind));
        statics.record_term_annotation(third_term, TermAnnId::Type(second_annotation, kind));
        statics.record_term_annotation(kind_term, TermAnnId::Kind(kind));
        statics.retain_normalized_annotations();

        assert!(matches!(statics.normalized_annotation_at(first_annotation), Some(Type::Unit(_))));
        assert!(matches!(statics.normalized_annotation_at(second_annotation), Some(Type::Thk(_))));
        assert_eq!(statics.annotation_norms.len(), 2);
    }

    #[test]
    fn unresolved_annotation_facts_do_not_hide_resolved_classifiers() {
        let mut surface = IdAllocator::<su::ScopedScope>::new();
        let unresolved_term = surface.alloc();
        let resolved_term = surface.alloc();
        let mut statics = StaticsArena::default();
        let kind = statics.kinds_pre.alloc(Fillable::Done(Kind::VType(VType)));
        let fill = statics.fills.alloc(InferenceSite::Term(unresolved_term));
        let unresolved = statics.types_pre.alloc(Fillable::Fill(fill), kind);
        let resolved = statics.types_pre.alloc(Fillable::Done(Type::Unit(UnitTy)), kind);

        statics.record_term_annotation(unresolved_term, TermAnnId::Type(unresolved, kind));
        statics.record_term_annotation(resolved_term, TermAnnId::Type(resolved, kind));
        statics.retain_normalized_annotations();

        assert!(statics.normalized_annotation_at(unresolved).is_none());
        assert!(matches!(statics.normalized_annotation_at(resolved), Some(Type::Unit(_))));
        assert_eq!(statics.annotation_norms.len(), 1);
        assert!(matches!(statics.types_pre.get(unresolved), Some(Fillable::Fill(_))));
    }

    #[test]
    #[should_panic(expected = "top annotation was not normalized")]
    fn missing_annotation_nodes_remain_an_invariant_violation() {
        let mut surface = IdAllocator::<su::ScopedScope>::new();
        let term = surface.alloc();
        let kind = KindId::from_index(0);
        let missing = TypeId::from_index(0);
        let mut statics = StaticsArena::default();
        statics.record_term_annotation(term, TermAnnId::Type(missing, kind));
        statics.retain_normalized_annotations();
    }

    #[test]
    fn term_facts_keep_sparse_slots_compact_and_replace_in_place() {
        assert_eq!(std::mem::size_of::<Option<TermFactsIndex>>(), 4);
        assert_eq!(std::mem::size_of::<TermAnnId>(), 12);
        assert_eq!(std::mem::size_of::<TermFacts>(), 12);
        assert!(
            std::mem::size_of::<Option<TermFactsIndex>>()
                < std::mem::size_of::<Option<TermFacts>>()
        );

        let mut surface = IdAllocator::<su::ScopedScope>::new();
        let term = surface.alloc();
        let first = KindId::from_index(0);
        let second = KindId::from_index(1);
        let mut statics = StaticsArena::default();

        statics.record_term_annotation(term, TermAnnId::Kind(first));
        statics.record_term_annotation(term, TermAnnId::Kind(second));

        assert_eq!(statics.term_facts.facts.len(), 1);
        assert_eq!(statics.term_annotation(term), Some(TermAnnId::Kind(second)));
    }
}
