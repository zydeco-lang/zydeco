use super::syntax::*;
use crate::*;

pub use zydeco_utils::arena::*;

/// Post-processed program data after type checking and hole resolution.
#[derive(Debug)]
pub struct WellFormedProgram {
    /* ---------------------------- principle arenas ---------------------------- */
    pub spans: t::SpanArena,
    /// definitions, i.e. variable names
    pub defs: ArenaSparse<DefId, VarName>,

    /// kind arena
    pub kinds: ArenaSparse<KindId, Kind>,
    /// type pattern arena
    pub tpats: ArenaSparse<TPatId, TypePattern>,
    /// type arena
    pub types: ArenaSparse<TypeId, Type>,
    /// value pattern arena
    pub vpats: ArenaSparse<VPatId, ValuePattern>,
    /// value arena
    pub values: ArenaSparse<ValueId, Value>,
    /// computation arena
    pub compus: ArenaSparse<CompuId, Computation>,

    /// entry point(s), i.e. declarations that are marked as entry points;
    /// typically the main function, which normally should only be unique
    pub entry: ArenaAssoc<CompuId, ()>,
    /// externals declarations
    pub externals: ArenaAssoc<DefId, ()>,

    /* -------------------------- translational arenas -------------------------- */
    /// entity maps from textural syntax
    pub textual: ArenaForth<t::EntityId, su::EntityId>,
    /// untyped to typed bijective maps for patterns
    pub pats: ArenaBijective<su::PatId, ss::PatId>,
    /// untyped to typed bijective maps for terms
    pub terms: ArenaBijective<su::TermId, ss::TermId>,
    /// declarations are now compressed into computations
    /// (especially pure `let` bindings)
    pub decls: ArenaBijective<ss::DeclId, CompuId>,

    /* ----------------- analytical arenas from name resolution ----------------- */
    /// def user map
    pub users: ArenaForth<DefId, ss::TermId>,
    /// variables available upon the term
    pub ctxs_term: ArenaAssoc<ss::TermId, su::Context>,
    /// variables that are introduced by the pattern
    pub ctxs_pat_local: ArenaAssoc<ss::PatId, su::Context>,
    /// variables that are free within the pattern (e.g. unbound type variable in annotations)
    pub coctxs_pat_local: ArenaAssoc<ss::PatId, su::CoContext>,
    /// variables that are free within the term
    pub coctxs_term_local: ArenaAssoc<ss::TermId, su::CoContext>,
    // meta annotations to declarations
    pub metas: ArenaAssoc<ss::DeclId, im::Vector<Meta>>,
    /// externs to defs
    pub exts: ArenaAssoc<ss::DeclId, (su::Internal, DefId)>,
    /// non-(optionally-mutual-)recursive declarations
    pub unis: ArenaAssoc<ss::DeclId, ()>,

    /* ------------------ analytical arenas from type checking ------------------ */
    /// arena for abstract types
    pub absts: ArenaDense<AbstId, ()>,
    /// the abstract types generated from sealed types
    pub seals: ArenaAssoc<AbstId, TypeId>,
    /// name hints for abstract types
    pub abst_hints: ArenaAssoc<AbstId, DefId>,
    /// arena for `data`; plural plural
    pub datas: ArenaDense<DataId, ss::Data>,
    /// arena for `codata`; plural plural
    pub codatas: ArenaDense<CoDataId, ss::CoData>,
    /// arena for inlinable definitions, typically global (necessity modality) definitions
    pub inlinables: ArenaAssoc<DefId, ValueId>,
    /// definitions that are marked global
    pub global_defs: ArenaAssoc<DefId, ()>,
    /// terms that are marked global
    pub global_terms: ArenaAssoc<ss::TermId, ()>,

    // the type of terms under the context it's type checked; "annotation"
    /// annotations for variable definitions
    pub annotations_var: ArenaAssoc<DefId, ss::AnnId>,
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
    // Todo: under which environment is something checked?
    // pub env_tpat
    // pub env_type
    // pub env_vpat
    // pub env_value
    // pub env_compu
}
