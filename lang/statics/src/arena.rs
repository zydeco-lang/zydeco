//! The statics arena for the Zydeco type checker.
//!
//! See [`StaticsArena`] for documentations of each field.

use super::syntax::*;
use crate::surface_syntax as su;
use crate::{SkolemScope, TyEnv};
use std::{
    num::NonZeroU32,
    ops::{Deref, DerefMut, Index, IndexMut},
    sync::Arc,
};

use zydeco_derive::{AsMutSelf, AsRefSelf};
pub use zydeco_surface::arena::*;

/* ---------------------------------- Arena --------------------------------- */

/// Allocation and owning storage scope for typed syntax.
#[derive(Debug)]
pub enum StaticsScope {}

impl Allocates<KindId> for StaticsScope {}
impl Allocates<FillId> for StaticsScope {}
impl Allocates<AbstId> for StaticsScope {}
impl Allocates<DataId> for StaticsScope {}
impl Allocates<CoDataId> for StaticsScope {}
impl Allocates<KPatId> for StaticsScope {}
impl Allocates<TPatId> for StaticsScope {}
impl Allocates<TypeId> for StaticsScope {}
impl Allocates<VPatId> for StaticsScope {}
impl Allocates<ValueId> for StaticsScope {}
impl Allocates<CompuId> for StaticsScope {}
impl Allocates<DefId> for StaticsScope {}

impl ArenaSchema<KindId> for StaticsScope {
    type Item = Fillable<Kind>;
}
impl ArenaSchema<KPatId> for StaticsScope {
    type Item = KindPattern;
}
impl ArenaSchema<TPatId> for StaticsScope {
    type Item = TypePattern;
}
impl ArenaSchema<TypeId> for StaticsScope {
    type Item = Fillable<Type>;
}
impl ArenaSchema<VPatId> for StaticsScope {
    type Item = ValuePattern;
}
impl ArenaSchema<ValueId> for StaticsScope {
    type Item = Value;
}
impl ArenaSchema<CompuId> for StaticsScope {
    type Item = Computation;
}
impl ArenaSchema<AbstId> for StaticsScope {
    type Item = ();
}
impl ArenaSchema<FillId> for StaticsScope {
    type Item = InferenceSite;
}
impl ArenaSchema<DataId> for StaticsScope {
    type Item = Data;
}
impl ArenaSchema<CoDataId> for StaticsScope {
    type Item = CoData;
}

pub use zydeco_surface::scoped::arena::*;

/// Static associations introduced by `@[builtin(...)]` package-signature
/// annotations.
#[derive(Clone, Debug, Default)]
pub struct BuiltinRoles {
    witnesses: ArenaAssoc<AbstId, BuiltinRole>,
    values: ArenaAssoc<TypeId, BuiltinValueRole>,
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
        zydeco_utils::arena::ArenaAccess::get(&self.witnesses, &witness).copied()
    }

    pub fn value(&self, entry: TypeId) -> Option<BuiltinValueRole> {
        zydeco_utils::arena::ArenaAccess::get(&self.values, &entry).copied()
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
            (*found == BuiltinRole::Type(role)).then_some(*witness)
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

/// Canonical static identities for the CBPV structure built into the
/// language.
#[derive(Clone, Debug, Default)]
pub struct IntrinsicStatics {
    pub(crate) vtype: Option<KindId>,
    pub(crate) ctype: Option<KindId>,
    pub(crate) thk: Option<TypeId>,
    pub(crate) ret: Option<TypeId>,
    pub(crate) unit: Option<TypeId>,
    pub(crate) primitives: std::collections::BTreeMap<zydeco_syntax::PrimitiveType, TypeId>,
}

/// Compact owning storage for pre-normalization kinds.
///
/// `VType` and `CType` dominate this arena and carry no payload. Sparse hash
/// buckets retain only a compact node for those variants; fill sites, arrows,
/// and labels occupy a dense side vector whose stable borrow is reconstructed
/// by [`KindArena::get`] and [`KindArena::iter`].
#[derive(Clone, Debug, Default)]
pub struct KindArena {
    nodes: ArenaSparse<KindStorageScope, KindId>,
    payloads: Vec<Fillable<Kind>>,
}

#[derive(Debug)]
enum KindStorageScope {}

impl ArenaSchema<KindId> for KindStorageScope {
    type Item = KindNode;
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
    pub fn insert_new(&mut self, id: KindId, value: Fillable<Kind>) {
        let (node, payload) = KindNode::split(value, self.payloads.len());
        self.nodes.insert_new(id, node);
        self.payloads.extend(payload);
    }

    pub fn get(&self, id: &KindId) -> Option<&Fillable<Kind>> {
        self.nodes.get(id).map(|node| self.value(node))
    }

    pub fn iter(&self) -> impl Iterator<Item = (&KindId, &Fillable<Kind>)> {
        self.nodes.iter().map(move |(id, node)| (id, self.value(node)))
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.len() == 0
    }

    fn value(&self, node: &KindNode) -> &Fillable<Kind> {
        match node {
            | KindNode::VType => &FILLED_VTYPE,
            | KindNode::CType => &FILLED_CTYPE,
            | KindNode::Payload(index) => &self.payloads[index.offset()],
        }
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

    pub(crate) fn iter(&self) -> impl Iterator<Item = (su::TermId, &TermFacts)> {
        self.indexes.iter().map(|(term, index)| (term, &self.facts[index.offset()]))
    }
}

/// Compact index into the distinct kind annotations used by one type arena.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
struct TypeKindIndex(u32);

/// One typed node and its classifier, co-located under the same [`TypeId`].
#[derive(Clone, Debug)]
struct TypeNode {
    value: Fillable<Type>,
    kind: TypeKindIndex,
}

/// Paged type storage with compact, arena-local kind annotation indexes.
///
/// Type payloads and their kind annotations have exactly the same key domain.
/// Keeping them in one record removes a parallel page hierarchy, while the
/// local kind table stores the 16-byte [`KindId`] once per distinct annotation
/// rather than once per type occurrence.
#[derive(Clone, Debug, Default)]
pub struct TypeArena {
    nodes: ArenaPagedAssoc<TypeId, TypeNode>,
    kinds: Vec<KindId>,
    kind_indexes: rustc_hash::FxHashMap<KindId, TypeKindIndex>,
}

impl TypeArena {
    pub fn insert_new(&mut self, id: TypeId, value: Fillable<Type>, kind: KindId) {
        let kind = self.intern_kind(kind);
        self.nodes.insert_new(id, TypeNode { value, kind });
    }

    pub fn replace_existing(&mut self, id: TypeId, value: Fillable<Type>) {
        self.nodes[&id].value = value;
    }

    pub fn iter(&self) -> impl Iterator<Item = (TypeId, &Fillable<Type>)> {
        self.nodes.iter().map(|(id, node)| (id, &node.value))
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.len() == 0
    }

    pub fn reserve_pages(&mut self, additional: usize) {
        self.nodes.reserve_pages(additional);
    }

    fn kind_at(&self, id: &TypeId) -> Option<KindId> {
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

impl Index<&TypeId> for TypeArena {
    type Output = Fillable<Type>;

    fn index(&self, id: &TypeId) -> &Self::Output {
        &self.nodes[id].value
    }
}

impl IndexMut<&TypeId> for TypeArena {
    fn index_mut(&mut self, id: &TypeId) -> &mut Self::Output {
        &mut self.nodes[id].value
    }
}

impl ArenaAccess<&TypeId, Fillable<Type>> for TypeArena {
    fn get(&self, id: &TypeId) -> Option<&Fillable<Type>> {
        self.nodes.get(id).map(|node| &node.value)
    }

    fn get_mut(&mut self, id: &TypeId) -> Option<&mut Fillable<Type>> {
        self.nodes.get_mut(id).map(|node| &mut node.value)
    }
}

/// One source occurrence representative for each typed node.
///
/// Diagnostics and source-span queries only need one source location. Rechecks
/// replace the representative, and transparent source wrappers that share a
/// typed node collapse to the most recently checked wrapper.
#[derive(Clone, Debug)]
pub struct SourceProvenance<Source, Typed> {
    latest_by_typed: ArenaAssoc<Typed, Source>,
}

impl<Source, Typed> Default for SourceProvenance<Source, Typed> {
    fn default() -> Self {
        Self { latest_by_typed: ArenaAssoc::default() }
    }
}

impl<Source, Typed> SourceProvenance<Source, Typed>
where
    Source: Copy,
    Typed: Eq + std::hash::Hash,
{
    pub fn record(&mut self, source: Source, typed: Typed) {
        let _ = self.latest_by_typed.upsert(typed, source);
    }

    pub fn source(&self, typed: &Typed) -> Option<Source> {
        self.latest_by_typed.get(typed).copied()
    }
}

/// Source-bounded static facts retained after the typed occurrence tree is
/// discarded.
///
/// A check builds this generation mutably. Once the check is published, the
/// full materialization and its retained [`StaticsArena::clone_keyed_indexes`]
/// view share one immutable allocation.
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
    pub annotation_norms: ArenaAssoc<TypeId, Type>,
    /// Coverage failures recorded during the finish phase, for editor facts.
    pub coverage_errors: Vec<crate::validate::CoverageError>,

    /// arena for abstract types
    pub absts: ArenaSparse<StaticsScope, AbstId>,
    /// the abstract types generated from sealed types
    pub seals: ArenaAssoc<AbstId, TypeId>,
    /// name hints for abstract types
    pub abst_hints: ArenaAssoc<AbstId, DefId>,
    /// abstract types introduced by existential elimination
    pub existential_skolems: ArenaAssoc<AbstId, ()>,
    /// canonical identities for intrinsic kinds and type constructors
    pub intrinsics: IntrinsicStatics,
    /// Builtin roles attached to existential witnesses and named value entries.
    pub builtin_roles: BuiltinRoles,
    /// arena for context-constrained flexible metavariables and their source sites;
    /// only types and kinds are now fillable hole-filling sites, allocated with
    /// derived identifiers so fill states can be query keys
    pub fills: ArenaSparse<StaticsScope, FillId>,
    /// the annotation assigned to each solved filling site
    pub solus: ArenaAssoc<FillId, AnnId>,
    /// existential witnesses that each type hole is allowed to mention
    pub fill_scopes: ArenaAssoc<FillId, SkolemScope>,
    /// which holes are introduced by the user and should be reported
    pub fill_hints: ArenaAssoc<FillId, ()>,
    /// arena for `data`; plural plural
    pub datas: ArenaSparse<StaticsScope, DataId>,
    /// arena for `codata`; plural plural
    pub codatas: ArenaSparse<StaticsScope, CoDataId>,
    /// hints for values that need data annotations
    pub data_hints: ArenaAssoc<ValueId, DataId>,
    /// hints for value patterns that need data annotations
    pub data_pat_hints: ArenaAssoc<VPatId, DataId>,
    /// hints for computations that need codata annotations
    pub codata_hints: ArenaAssoc<CompuId, CoDataId>,
    /// matches generated to cover value patterns in generalized comatch clauses
    pub copattern_matches: ArenaAssoc<CompuId, ()>,
    /// package-dependent binders consumed directly from generalized comatch clauses
    pub copattern_pack_pi_binders: ArenaAssoc<CompuId, VPatId>,
    /// immutable value aliases available to static inspection
    pub value_aliases: ArenaAssoc<DefId, ValueId>,
    /// package witnesses retained by whole-value aliases in selective package patterns
    pub package_aliases: ArenaAssoc<DefId, Vec<StaticTermId>>,
    /// checked bodies of type definitions available to static inspection
    pub type_definitions: ArenaAssoc<DefId, TypeId>,
    /// arena for inlinable definitions, typically global (necessity modality) definitions
    pub inlinables: ArenaAssoc<DefId, ValueId>,
    /// definitions that are marked global
    pub global_defs: ArenaAssoc<DefId, ()>,
    /// terms that are marked global
    pub global_terms: ArenaAssoc<TermId, ()>,
    /// hints associating typed terms with definition names
    pub def_hints: ArenaAssoc<TermId, DefId>,
    /// Definition names synthesized during typed elaboration. Source definitions
    /// remain in the immutable scoped arena; keeping this small delta here avoids
    /// cloning every resolved syntax and context table just to add names.
    pub generated_defs: ArenaSparse<su::ScopedScope, DefId>,

    // the type of terms under the context it's type checked; "annotation"
    /// annotations for variable definitions
    pub annotations_var: ArenaAssoc<DefId, AnnId>,
    /// annotations for abstract types
    pub annotations_abst: ArenaAssoc<AbstId, KindId>,
}

/// Typed arena plus annotation tables and translation metadata.
#[derive(Clone, Debug, Default, AsRefSelf, AsMutSelf)]
pub struct StaticsArena {
    /// Source-bounded facts shared with retained analyses after checking.
    indexes: Arc<StaticsIndexes>,

    /// kind arena before normalization
    pub kinds_pre: KindArena,
    /// manifest kind-pattern arena
    pub kpats: ArenaSparse<StaticsScope, KPatId>,
    /// type pattern arena
    pub tpats: ArenaSparse<StaticsScope, TPatId>,
    /// type arena before normalization
    pub types_pre: TypeArena,
    /// value pattern arena
    pub vpats: ArenaSparse<StaticsScope, VPatId>,
    /// value arena
    pub values: ArenaSparse<StaticsScope, ValueId>,
    /// computation arena
    pub compus: ArenaSparse<StaticsScope, CompuId>,
    /// kind annotations for type patterns
    pub annotations_tpat: ArenaAssoc<TPatId, KindId>,
    /// type annotations for value patterns
    pub annotations_vpat: ArenaAssoc<VPatId, TypeId>,
    /// type annotations for values
    pub annotations_value: ArenaAssoc<ValueId, TypeId>,
    /// type annotations for computations
    pub annotations_compu: ArenaAssoc<CompuId, TypeId>,

    // typing environments during type checking
    /// typing environments for manifest kind patterns
    pub env_kpat: ArenaAssoc<KPatId, TyEnv>,
    /// typing environments for type patterns
    pub env_tpat: ArenaAssoc<TPatId, TyEnv>,
    /// typing environments for types, interned so structurally identical
    /// environments from distinct sites share one allocation
    pub env_type: ArenaPagedAssoc<TypeId, std::sync::Arc<TyEnv>>,
    /// content-addressed cache behind [`Self::env_type`]; checker-transient
    /// and stripped with the environments after checking
    pub env_interner: crate::environment::TyEnvInterner,
    /// typing environments for value patterns
    pub env_vpat: ArenaAssoc<VPatId, TyEnv>,
    /// typing environments for values
    pub env_value: ArenaAssoc<ValueId, TyEnv>,
    /// typing environments for computations
    pub env_compu: ArenaAssoc<CompuId, TyEnv>,

    // normalized kinds and types after type checking
    /// normalized kind delta for IDs whose pre-normalization form changed
    pub kinds_normalized: ArenaAssoc<KindId, Kind>,
    /// normalized type free of holes
    pub types_normalized: ArenaAssoc<TypeId, Type>,
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
    /// Pre-reserve source-shaped pages from the name-resolved program. Term
    /// facts know their exact external ID extents; generated type pages can
    /// reserve only their estimated outer key-space count.
    pub fn reserve(&mut self, scoped: &su::ScopedArena) {
        let type_key_spaces = scoped.terms.len().saturating_add(1) / 2;
        self.types_pre.reserve_pages(type_key_spaces);
        self.env_type.reserve_pages(type_key_spaces);
        self.term_facts.reserve_ids(scoped.terms.iter().map(|(term, _)| term));
    }

    /// Intern one typing environment and return the shared value for storage
    /// in [`Self::env_type`].
    pub fn intern_env(&mut self, env: &TyEnv) -> std::sync::Arc<TyEnv> {
        self.env_interner.intern(env)
    }

    /// Materialize one stored typing environment as an owned value.
    pub fn env_at(&self, id: TypeId) -> TyEnv {
        self.env_type[&id].as_ref().clone()
    }

    /// The kind annotation co-located with one type node.
    pub fn type_kind(&self, id: TypeId) -> KindId {
        self.types_pre.kind_at(&id).expect("type node has no kind annotation")
    }

    /// The kind annotation of a type node, when the node is present.
    pub fn type_kind_at(&self, id: TypeId) -> Option<KindId> {
        self.types_pre.kind_at(&id)
    }

    /// Clone only the keyed indexes of a finished check, leaving the much
    /// larger occurrence payload in its shared materialization. See
    /// `docs/ideas/arena-gc.md` for the L/S classification.
    pub fn clone_keyed_indexes(&self) -> Self {
        Self { indexes: Arc::clone(&self.indexes), ..Self::default() }
    }

    /// The normalized form of one type, falling back to the pre-normalization
    /// form for nodes the normalization phase left unchanged.
    pub fn normalized_at(&self, id: TypeId) -> Option<&Type> {
        self.types_normalized.get(&id).or_else(|| match self.types_pre.get(&id)? {
            | Fillable::Done(ty) => Some(ty),
            | Fillable::Fill(_) => None,
        })
    }

    /// The normalized form of one kind, falling back to its unchanged
    /// pre-normalization form when no delta is stored.
    pub fn normalized_kind_at(&self, id: KindId) -> Option<&Kind> {
        self.kinds_normalized.get(&id).or_else(|| match self.kinds_pre.get(&id)? {
            | Fillable::Done(kind) => Some(kind),
            | Fillable::Fill(_) => None,
        })
    }

    /// Record the final annotation of a source term.
    pub fn record_term_annotation(&mut self, term: su::TermId, annotation: TermAnnId) {
        let _ = self.term_facts.upsert(term, TermFacts { annotation });
    }

    /// Attach the normalized classifier of one distinct top annotation type.
    pub fn record_annotation_normalized(&mut self, annotation: TypeId, normalized: Type) {
        let _ = self.annotation_norms.upsert(annotation, normalized);
    }

    pub fn term_annotation(&self, term: su::TermId) -> Option<TermAnnId> {
        self.term_facts.get(&term).map(|facts| facts.annotation)
    }

    pub fn normalized_annotation_at(&self, annotation: TypeId) -> Option<&Type> {
        self.annotation_norms.get(&annotation)
    }

    /// Look up either a source definition or one synthesized by typed elaboration.
    pub fn def_name<'a>(&'a self, scoped: &'a su::ScopedArena, id: &DefId) -> &'a su::VarName {
        self.generated_defs.get(id).unwrap_or_else(|| &scoped.defs[id])
    }

    /// Clone the definition-name table needed by dynamic and backend lowering.
    /// Other resolved syntax tables are source-only and stay shared.
    pub fn scoped_definitions(
        &self, scoped: &su::ScopedArena,
    ) -> ArenaSparse<su::ScopedScope, DefId> {
        let mut definitions = scoped.defs.clone();
        definitions += self.generated_defs.clone();
        definitions
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn common_kinds_stay_inline_while_uncommon_payloads_are_dense() {
        assert!(std::mem::size_of::<KindNode>() <= 8);
        assert!(std::mem::size_of::<KindNode>() < std::mem::size_of::<Fillable<Kind>>());

        let mut allocator = IdAllocator::<StaticsScope>::new();
        let vtype: KindId = allocator.alloc();
        let ctype: KindId = allocator.alloc();
        let arrow: KindId = allocator.alloc();
        let label: KindId = allocator.alloc();
        let fill: FillId = allocator.alloc();
        let pending: KindId = allocator.alloc();
        let mut kinds = KindArena::default();

        kinds.insert_new(vtype, Fillable::Done(Kind::VType(VType)));
        kinds.insert_new(ctype, Fillable::Done(Kind::CType(CType)));
        kinds.insert_new(arrow, Fillable::Done(Kind::Arrow(Arrow(vtype, ctype))));
        kinds.insert_new(
            label,
            Fillable::Done(Kind::Label(Label(FieldName("field".to_owned()), vtype))),
        );
        kinds.insert_new(pending, Fillable::Fill(fill));

        assert_eq!(kinds.len(), 5);
        assert_eq!(kinds.payloads.len(), 3);
        assert!(matches!(kinds[&vtype], Fillable::Done(Kind::VType(_))));
        assert!(matches!(kinds[&ctype], Fillable::Done(Kind::CType(_))));
        assert!(matches!(kinds[&arrow], Fillable::Done(Kind::Arrow(_))));
        assert!(matches!(kinds[&label], Fillable::Done(Kind::Label(_))));
        assert!(matches!(kinds[&pending], Fillable::Fill(id) if id == fill));
        assert_eq!(kinds.iter().count(), 5);
    }

    #[test]
    fn type_nodes_store_compact_kind_indexes() {
        assert!(
            std::mem::size_of::<Option<TypeNode>>()
                < std::mem::size_of::<Option<Fillable<Type>>>()
                    + std::mem::size_of::<Option<KindId>>()
        );

        let mut allocator = IdAllocator::<StaticsScope>::new();
        let kind: KindId = allocator.alloc();
        let first: TypeId = allocator.alloc();
        let second: TypeId = allocator.alloc();
        let third: TypeId = allocator.alloc();
        let mut types = TypeArena::default();

        [first, second].into_iter().for_each(|id| {
            types.insert_new(id, Fillable::Done(Type::Unit(UnitTy)), kind);
        });
        assert_eq!(types.kinds, vec![kind]);
        assert_eq!(types.kind_at(&first), Some(kind));
        assert_eq!(types.kind_at(&second), Some(kind));

        types.strip_kind_index();
        assert_eq!(types.kind_at(&first), Some(kind));
        types.insert_new(third, Fillable::Done(Type::Unit(UnitTy)), kind);
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
        let mut source = IdAllocator::<su::BitterScope>::new();
        let first_source: su::PatId = source.alloc();
        let second_source: su::PatId = source.alloc();
        let mut typed = IdAllocator::<StaticsScope>::new();
        let first_typed = PatId::Kind(typed.alloc());
        let second_typed = PatId::Value(typed.alloc());
        let mut provenance = SourceProvenance::default();

        provenance.record(first_source, first_typed);
        provenance.record(first_source, first_typed);
        provenance.record(second_source, first_typed);
        provenance.record(first_source, second_typed);

        assert_eq!(provenance.source(&first_typed), Some(second_source));
        assert_eq!(provenance.source(&second_typed), Some(first_source));

        provenance.record(first_source, first_typed);
        assert_eq!(provenance.source(&first_typed), Some(first_source));
    }

    #[test]
    fn shared_annotation_ids_share_one_normalized_fact() {
        let mut surface = IdAllocator::<su::ScopedScope>::new();
        let first_term = surface.alloc();
        let second_term = surface.alloc();
        let mut allocator = IdAllocator::<StaticsScope>::new();
        let kind: KindId = allocator.alloc();
        let annotation: TypeId = allocator.alloc();
        let mut statics = StaticsArena::default();

        [first_term, second_term].into_iter().for_each(|term| {
            statics.record_term_annotation(term, TermAnnId::Type(annotation, kind));
        });
        statics.record_annotation_normalized(annotation, Type::Unit(UnitTy));

        assert!(matches!(statics.normalized_annotation_at(annotation), Some(Type::Unit(_))));
        assert_eq!(statics.annotation_norms.len(), 1);
    }

    #[test]
    fn term_facts_keep_sparse_slots_compact_and_replace_in_place() {
        assert_eq!(std::mem::size_of::<Option<TermFactsIndex>>(), 4);
        assert!(
            std::mem::size_of::<Option<TermFactsIndex>>()
                < std::mem::size_of::<Option<TermFacts>>()
        );

        let mut surface = IdAllocator::<su::ScopedScope>::new();
        let term = surface.alloc();
        let mut allocator = IdAllocator::<StaticsScope>::new();
        let first: KindId = allocator.alloc();
        let second: KindId = allocator.alloc();
        let mut statics = StaticsArena::default();

        statics.record_term_annotation(term, TermAnnId::Kind(first));
        statics.record_term_annotation(term, TermAnnId::Kind(second));

        assert_eq!(statics.term_facts.facts.len(), 1);
        assert_eq!(statics.term_annotation(term), Some(TermAnnId::Kind(second)));
    }
}

/* -------------------------------- LocalFold ------------------------------- */

/// A set of local actions on static arena items.
#[auto_impl::auto_impl(&mut, Box)]
pub trait LocalFoldStatics<Cx> {
    fn action_kind(&mut self, kind: KindId, ctx: &Cx);
    fn action_kpat(&mut self, kpat: KPatId, ctx: &Cx);
    fn action_tpat(&mut self, tpat: TPatId, ctx: &Cx);
    fn action_type(&mut self, r#type: TypeId, ctx: &Cx);
    fn action_vpat(&mut self, vpat: VPatId, ctx: &Cx);
    fn action_value(&mut self, value: ValueId, ctx: &Cx);
    fn action_compu(&mut self, compu: CompuId, ctx: &Cx);
}
