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
    pub types_pre: ArenaPaged<StaticsScope, TypeId>,
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
    /// The final annotation of each checked term, keyed by the surface term.
    /// Editor facts read this instead of the per-node annotation tables, which
    /// the occurrence-payload strip discards.
    pub term_anns: ArenaAssoc<su::TermId, TermAnnId>,
    /// The normalized form of each term's annotation type, keyed by the
    /// surface term, so editor facts answer without the occurrence payload.
    pub term_norms: ArenaAssoc<su::TermId, Type>,
    /// The surface term each top annotation type belongs to; inner type nodes
    /// have no entry, so lookups for them answer nothing instead of guessing.
    pub type_sites: ArenaAssoc<TypeId, su::TermId>,
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
    /// Definition names synthesized during typed elaboration. Source definitions
    /// remain in the immutable scoped arena; keeping this small delta here avoids
    /// cloning every resolved syntax and context table just to add names.
    pub generated_defs: ArenaSparse<su::ScopedScope, DefId>,

    // the type of terms under the context it's type checked; "annotation"
    /// annotations for variable definitions
    pub annotations_var: ArenaAssoc<DefId, AnnId>,
    /// annotations for abstract types
    pub annotations_abst: ArenaAssoc<AbstId, KindId>,
    /// kind annotations for type patterns
    pub annotations_tpat: ArenaAssoc<TPatId, KindId>,
    /// kind annotations for types
    pub annotations_type: ArenaPagedAssoc<TypeId, KindId>,
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
    /// normalized kind free of holes
    pub kinds_normalized: ArenaAssoc<KindId, Kind>,
    /// normalized type free of holes
    pub types_normalized: ArenaAssoc<TypeId, Type>,
}

impl StaticsArena {
    /// Pre-reserve the outer type-page tables from the name-resolved program's
    /// size. Measurements put type-producing key spaces at slightly under one
    /// half of scoped terms; the millions of inner slots grow in their pages.
    pub fn reserve(&mut self, scoped_terms: usize) {
        let type_key_spaces = scoped_terms.saturating_add(1) / 2;
        self.types_pre.reserve_pages(type_key_spaces);
        self.annotations_type.reserve_pages(type_key_spaces);
        self.env_type.reserve_pages(type_key_spaces);
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

    /// Clone only the keyed indexes of a finished check, leaving the much
    /// larger occurrence payload in its shared materialization. See
    /// `docs/ideas/arena-gc.md` for the L/S classification.
    pub fn clone_keyed_indexes(&self) -> Self {
        Self {
            pats: self.pats.clone(),
            terms: self.terms.clone(),
            term_anns: self.term_anns.clone(),
            term_norms: self.term_norms.clone(),
            type_sites: self.type_sites.clone(),
            coverage_errors: self.coverage_errors.clone(),
            absts: self.absts.clone(),
            seals: self.seals.clone(),
            abst_hints: self.abst_hints.clone(),
            existential_skolems: self.existential_skolems.clone(),
            intrinsics: self.intrinsics.clone(),
            builtin_roles: self.builtin_roles.clone(),
            fills: self.fills.clone(),
            solus: self.solus.clone(),
            fill_scopes: self.fill_scopes.clone(),
            fill_hints: self.fill_hints.clone(),
            datas: self.datas.clone(),
            codatas: self.codatas.clone(),
            data_hints: self.data_hints.clone(),
            data_pat_hints: self.data_pat_hints.clone(),
            codata_hints: self.codata_hints.clone(),
            copattern_matches: self.copattern_matches.clone(),
            copattern_pack_pi_binders: self.copattern_pack_pi_binders.clone(),
            value_aliases: self.value_aliases.clone(),
            package_aliases: self.package_aliases.clone(),
            type_definitions: self.type_definitions.clone(),
            inlinables: self.inlinables.clone(),
            global_defs: self.global_defs.clone(),
            global_terms: self.global_terms.clone(),
            def_hints: self.def_hints.clone(),
            generated_defs: self.generated_defs.clone(),
            annotations_var: self.annotations_var.clone(),
            annotations_abst: self.annotations_abst.clone(),
            ..Self::default()
        }
    }

    /// The normalized form of one type, falling back to the pre-normalization
    /// form for nodes the normalization phase left unchanged.
    pub fn normalized_at(&self, id: TypeId) -> Option<&Type> {
        self.types_normalized.get(&id).or_else(|| match self.types_pre.get(&id)? {
            | Fillable::Done(ty) => Some(ty),
            | Fillable::Fill(_) => None,
        })
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
