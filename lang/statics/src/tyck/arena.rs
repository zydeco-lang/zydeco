//! The statics arena for the Zydeco type checker.
//!
//! See [`StaticsArena`] for documentations of each field.

use super::syntax::*;
use crate::surface_syntax as su;

use zydeco_derive::{AsMutSelf, AsRefSelf};
pub use zydeco_surface::arena::*;

/* ---------------------------------- Arena --------------------------------- */

pub use zydeco_surface::scoped::arena::*;

/// Typed arena plus annotation tables and translation metadata.
// Clone is derived only for coping with wf in driver
#[derive(Debug, Clone, AsRefSelf, AsMutSelf)]
pub struct StaticsArena {
    /// kind arena before normalization
    pub kinds_pre: ArenaSparse<KindId, Fillable<Kind>>,
    /// type pattern arena
    pub tpats: ArenaSparse<TPatId, TypePattern>,
    /// type arena before normalization
    pub types_pre: ArenaSparse<TypeId, Fillable<Type>>,
    /// value pattern arena
    pub vpats: ArenaSparse<VPatId, ValuePattern>,
    /// value arena
    pub values: ArenaSparse<ValueId, Value>,
    /// computation arena
    pub compus: ArenaSparse<CompuId, Computation>,
    /// declaration arena
    pub decls: ArenaAssoc<DeclId, Declaration>,

    /// entry point(s), i.e. declarations that are marked as entry points;
    /// typically the main function, which normally should only be unique
    pub entry: ArenaAssoc<DeclId, ()>,

    /// untyped to typed bijective maps for patterns
    pub pats: ArenaBijective<su::PatId, PatId>,
    /// untyped to typed bijective maps for terms
    pub terms: ArenaBijective<su::TermId, TermId>,

    /// arena for abstract types
    pub absts: ArenaDense<AbstId, ()>,
    /// the abstract types generated from sealed types
    pub seals: ArenaAssoc<AbstId, TypeId>,
    /// name hints for abstract types
    pub abst_hints: ArenaAssoc<AbstId, DefId>,
    /// arena for filling context-constrained holes; the [`su::TermId`] is the site;
    /// only types and kinds are now fillable
    pub fills: ArenaDense<FillId, su::TermId>,
    /// arena for the solutions of fillings,
    /// i.e. the the [`FillId`] should be assigned as the [`AnnId`]
    pub solus: ArenaAssoc<FillId, AnnId>,
    /// which holes are introduced by the user and should be reported
    pub fill_hints: ArenaAssoc<FillId, ()>,
    /// arena for `data`; plural plural
    pub datas: ArenaDense<DataId, Data>,
    /// arena for `codata`; plural plural
    pub codatas: ArenaDense<CoDataId, CoData>,
    /// hints for values that need data annotations
    pub data_hints: ArenaAssoc<ValueId, DataId>,
    /// hints for value patterns that need data annotations
    pub data_pat_hints: ArenaAssoc<VPatId, DataId>,
    /// hints for computations that need codata annotations
    pub codata_hints: ArenaAssoc<CompuId, CoDataId>,
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
    pub fn new_arc(alloc: ArcGlobalAlloc) -> Self {
        Self {
            kinds_pre: ArenaSparse::new(alloc.alloc()),
            tpats: ArenaSparse::new(alloc.alloc()),
            types_pre: ArenaSparse::new(alloc.alloc()),
            vpats: ArenaSparse::new(alloc.alloc()),
            values: ArenaSparse::new(alloc.alloc()),
            compus: ArenaSparse::new(alloc.alloc()),
            decls: ArenaAssoc::new(),

            entry: ArenaAssoc::new(),

            pats: ArenaBijective::new(),
            terms: ArenaBijective::new(),

            absts: ArenaDense::new(alloc.alloc()),
            seals: ArenaAssoc::new(),
            abst_hints: ArenaAssoc::new(),
            fills: ArenaDense::new(alloc.alloc()),
            solus: ArenaAssoc::new(),
            fill_hints: ArenaAssoc::new(),
            datas: ArenaDense::new(alloc.alloc()),
            codatas: ArenaDense::new(alloc.alloc()),
            data_hints: ArenaAssoc::new(),
            data_pat_hints: ArenaAssoc::new(),
            codata_hints: ArenaAssoc::new(),
            inlinables: ArenaAssoc::new(),
            global_defs: ArenaAssoc::new(),
            global_terms: ArenaAssoc::new(),
            def_hints: ArenaAssoc::new(),

            annotations_var: ArenaAssoc::new(),
            annotations_abst: ArenaAssoc::new(),
            annotations_tpat: ArenaAssoc::new(),
            annotations_type: ArenaAssoc::new(),
            annotations_vpat: ArenaAssoc::new(),
            annotations_value: ArenaAssoc::new(),
            annotations_compu: ArenaAssoc::new(),

            env_tpat: ArenaAssoc::new(),
            env_type: ArenaAssoc::new(),
            env_vpat: ArenaAssoc::new(),
            env_value: ArenaAssoc::new(),
            env_compu: ArenaAssoc::new(),

            kinds_normalized: ArenaAssoc::new(),
            types_normalized: ArenaAssoc::new(),
        }
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
