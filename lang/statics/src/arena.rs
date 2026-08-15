//! The statics arena for the Zydeco type checker.
//!
//! See [`StaticsArena`] for documentations of each field.

use super::syntax::*;
use crate::surface_syntax as su;
use crate::{SkolemScope, TyEnv};

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

/// Typed arena plus annotation tables and translation metadata.
#[derive(Clone, Debug, Default, AsRefSelf, AsMutSelf)]
pub struct StaticsArena {
    /// kind arena before normalization
    pub kinds_pre: ArenaSparse<StaticsScope, KindId>,
    /// manifest kind-pattern arena
    pub kpats: ArenaSparse<StaticsScope, KPatId>,
    /// type pattern arena
    pub tpats: ArenaSparse<StaticsScope, TPatId>,
    /// type arena before normalization
    pub types_pre: ArenaSparse<StaticsScope, TypeId>,
    /// value pattern arena
    pub vpats: ArenaSparse<StaticsScope, VPatId>,
    /// value arena
    pub values: ArenaSparse<StaticsScope, ValueId>,
    /// computation arena
    pub compus: ArenaSparse<StaticsScope, CompuId>,
    /// Untyped-to-typed pattern provenance. A surface pattern can be checked
    /// more than once, while transparent wrappers can share a typed pattern.
    pub pats: ArenaBipartite<su::PatId, PatId>,
    /// Untyped-to-typed term provenance. A surface term can be checked more
    /// than once, while erased constructs can share a typed term.
    pub terms: ArenaBipartite<su::TermId, TermId>,

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
    /// only types and kinds are now fillable
    /// hole-filling sites, allocated with derived identifiers like the other
    /// sparse categories so fill states can be query keys
    pub fills: ArenaSparse<StaticsScope, FillId>,
    /// arena for the solutions of fillings,
    /// i.e. the the [`FillId`] should be assigned as the [`AnnId`]
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
    /// TODO: hints for all sorts of terms that can be associated with a definition name
    pub def_hints: ArenaAssoc<TermId, DefId>,

    // the type of terms under the context it's type checked; "annotation"
    /// annotations for variable definitions
    pub annotations_var: ArenaAssoc<DefId, AnnId>,
    /// annotations for abstract types
    pub annotations_abst: ArenaAssoc<AbstId, KindId>,
    /// kind annotations for type patterns
    pub annotations_tpat: ArenaAssoc<TPatId, KindId>,
    /// kind annotations for types
    pub annotations_type: ArenaAssoc<TypeId, KindId>,
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
    pub env_type: ArenaAssoc<TypeId, std::sync::Arc<TyEnv>>,
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
    /// normalized kind free of holes
    pub kinds_normalized: ArenaAssoc<KindId, Kind>,
    /// normalized type free of holes
    pub types_normalized: ArenaAssoc<TypeId, Type>,
}

impl StaticsArena {
    /// Pre-reserve the type-indexed tables from the name-resolved program's
    /// size, so the arena grows toward its final shape with fewer bucket-table
    /// reallocations. A checked program materializes roughly an order of
    /// magnitude more types than it has scoped terms.
    pub fn reserve(&mut self, scoped_terms: usize) {
        let types = scoped_terms.saturating_mul(16);
        self.types_pre.reserve(types);
        self.types_normalized.reserve(types);
        self.annotations_type.reserve(types);
        self.env_type.reserve(types);
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

    /// Drop the occurrence payload of a finished check, retaining only the
    /// keyed indexes from which the typed tree can be re-materialized. See
    /// `docs/ideas/arena-gc.md` for the L/S classification.
    pub fn strip_occurrence_payload(&mut self) {
        self.kinds_pre = Default::default();
        self.kpats = Default::default();
        self.tpats = Default::default();
        self.types_pre = Default::default();
        self.vpats = Default::default();
        self.values = Default::default();
        self.compus = Default::default();
        self.annotations_tpat = Default::default();
        self.annotations_type = Default::default();
        self.annotations_vpat = Default::default();
        self.annotations_value = Default::default();
        self.annotations_compu = Default::default();
        self.kinds_normalized = Default::default();
        self.types_normalized = Default::default();
    }

    /// The normalized form of one type, falling back to the pre-normalization
    /// form for nodes the normalization phase left unchanged.
    pub fn normalized_at(&self, id: TypeId) -> Option<&Type> {
        self.types_normalized.get(&id).or_else(|| match self.types_pre.get(&id)? {
            | Fillable::Done(ty) => Some(ty),
            | Fillable::Fill(_) => None,
        })
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
