//! The statics arena for the Zydeco type checker.
//!
//! See [`StaticsArena`] for documentations of each field.

use super::syntax::*;
use crate::surface_syntax as su;

use zydeco_derive::{AsMutSelf, AsRefSelf};
pub use zydeco_surface::arena::*;

/* ---------------------------------- Arena --------------------------------- */

/// Allocation and owning storage scope for typed syntax.
#[derive(Debug)]
pub enum StaticsScope {}

impl Allocates<KindId> for StaticsScope {}
impl Allocates<TPatId> for StaticsScope {}
impl Allocates<TypeId> for StaticsScope {}
impl Allocates<VPatId> for StaticsScope {}
impl Allocates<ValueId> for StaticsScope {}
impl Allocates<CompuId> for StaticsScope {}
impl Allocates<DefId> for StaticsScope {}

impl ArenaSchema<KindId> for StaticsScope {
    type Item = Fillable<Kind>;
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
impl ArenaSchema<DeclId> for StaticsScope {
    type Item = Declaration;
}
impl ArenaSchema<AbstId> for StaticsScope {
    type Item = ();
}
impl ArenaSchema<FillId> for StaticsScope {
    type Item = su::TermId;
}
impl ArenaSchema<DataId> for StaticsScope {
    type Item = Data;
}
impl ArenaSchema<CoDataId> for StaticsScope {
    type Item = CoData;
}

pub use zydeco_surface::scoped::arena::*;

/// Typed arena plus annotation tables and translation metadata.
#[derive(Debug, Default, AsRefSelf, AsMutSelf)]
pub struct StaticsArena {
    /// kind arena before normalization
    pub kinds_pre: ArenaSparse<StaticsScope, KindId>,
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
    /// declaration arena
    pub decls: ArenaSparse<StaticsScope, DeclId>,

    /// entry point(s), i.e. declarations that are marked as entry points;
    /// typically the main function, which normally should only be unique
    pub entry: ArenaAssoc<DeclId, ()>,

    /// Untyped-to-typed pattern provenance. A surface pattern can be checked
    /// more than once, while transparent wrappers can share a typed pattern.
    pub pats: ArenaBipartite<su::PatId, PatId>,
    /// Untyped-to-typed term provenance. A surface term can be checked more
    /// than once, while erased constructs can share a typed term.
    pub terms: ArenaBipartite<su::TermId, TermId>,

    /// arena for abstract types
    pub absts: ArenaDense<StaticsScope, AbstId>,
    /// the abstract types generated from sealed types
    pub seals: ArenaAssoc<AbstId, TypeId>,
    /// name hints for abstract types
    pub abst_hints: ArenaAssoc<AbstId, DefId>,
    /// abstract types introduced by existential elimination
    pub existential_skolems: ArenaAssoc<AbstId, ()>,
    /// arena for filling context-constrained holes; the [`su::TermId`] is the site;
    /// only types and kinds are now fillable
    pub fills: ArenaDense<StaticsScope, FillId>,
    /// arena for the solutions of fillings,
    /// i.e. the the [`FillId`] should be assigned as the [`AnnId`]
    pub solus: ArenaAssoc<FillId, AnnId>,
    /// existential witnesses that each type hole is allowed to mention
    pub fill_scopes: ArenaAssoc<FillId, SkolemScope>,
    /// which holes are introduced by the user and should be reported
    pub fill_hints: ArenaAssoc<FillId, ()>,
    /// arena for `data`; plural plural
    pub datas: ArenaDense<StaticsScope, DataId>,
    /// arena for `codata`; plural plural
    pub codatas: ArenaDense<StaticsScope, CoDataId>,
    /// hints for values that need data annotations
    pub data_hints: ArenaAssoc<ValueId, DataId>,
    /// hints for value patterns that need data annotations
    pub data_pat_hints: ArenaAssoc<VPatId, DataId>,
    /// hints for computations that need codata annotations
    pub codata_hints: ArenaAssoc<CompuId, CoDataId>,
    /// immutable value aliases available to static inspection
    pub value_aliases: ArenaAssoc<DefId, ValueId>,
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
    /// typing environments for type patterns
    pub env_tpat: ArenaAssoc<TPatId, TyEnv>,
    /// typing environments for types
    pub env_type: ArenaAssoc<TypeId, TyEnv>,
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
    pub fn new() -> Self {
        Self::default()
    }
}

/* -------------------------------- LocalFold ------------------------------- */

/// A set of local actions on static arena items.
#[auto_impl::auto_impl(&mut, Box)]
pub trait LocalFoldStatics<Cx> {
    fn action_kind(&mut self, kind: KindId, ctx: &Cx);
    fn action_tpat(&mut self, tpat: TPatId, ctx: &Cx);
    fn action_type(&mut self, r#type: TypeId, ctx: &Cx);
    fn action_vpat(&mut self, vpat: VPatId, ctx: &Cx);
    fn action_value(&mut self, value: ValueId, ctx: &Cx);
    fn action_compu(&mut self, compu: CompuId, ctx: &Cx);
    fn action_decl(&mut self, decl: DeclId, ctx: &Cx);
}
