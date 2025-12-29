//! The statics arena for the Zydeco type checker.
//!
//! See [`StaticsArena`] for documentations of each field.

use super::syntax::*;
use crate::surface_syntax as su;

pub use zydeco_surface::arena::*;

/* ---------------------------------- Arena --------------------------------- */

pub use zydeco_surface::scoped::arena::*;

/// Item projectors out of the statics arena.
#[auto_impl::auto_impl(&, &mut, Box, Rc, Arc)]
pub trait ArenaStatics {
    fn kind(&self, id: &KindId) -> Fillable<Kind>;
    fn tpat(&self, id: &TPatId) -> TypePattern;
    fn r#type(&self, id: &TypeId) -> Fillable<Type>;
    fn vpat(&self, id: &VPatId) -> ValuePattern;
    fn value(&self, id: &ValueId) -> Value;
    fn compu(&self, id: &CompuId) -> Computation;
    fn decl(&self, id: &DeclId) -> Declaration;
}

/// Typed arena plus annotation tables and translation metadata.
// Clone is derived only for coping with wf in driver
#[derive(Debug, Clone)]
pub struct StaticsArena {
    /// kind arena
    pub kinds: ArenaSparse<KindId, Fillable<Kind>>,
    /// type pattern arena
    pub tpats: ArenaSparse<TPatId, TypePattern>,
    /// type arena
    pub types: ArenaSparse<TypeId, Fillable<Type>>,
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
    /// arena for inlinable definitions, typically global (necessity modality) definitions
    pub inlinables: ArenaAssoc<DefId, ValueId>,
    /// definitions that are marked global
    pub global_defs: ArenaAssoc<DefId, ()>,
    /// terms that are marked global
    pub global_terms: ArenaAssoc<TermId, ()>,

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
    // Todo: under which environment is something checked?
    // pub env_tpat
    // pub env_type
    // pub env_vpat
    // pub env_value
    // pub env_compu
}

impl StaticsArena {
    pub fn new_arc(alloc: ArcGlobalAlloc) -> Self {
        Self {
            kinds: ArenaSparse::new(alloc.alloc()),
            tpats: ArenaSparse::new(alloc.alloc()),
            types: ArenaSparse::new(alloc.alloc()),
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
            inlinables: ArenaAssoc::new(),
            global_defs: ArenaAssoc::new(),
            global_terms: ArenaAssoc::new(),

            annotations_var: ArenaAssoc::new(),
            annotations_abst: ArenaAssoc::new(),
            annotations_tpat: ArenaAssoc::new(),
            annotations_type: ArenaAssoc::new(),
            annotations_vpat: ArenaAssoc::new(),
            annotations_value: ArenaAssoc::new(),
            annotations_compu: ArenaAssoc::new(),
        }
    }
}

impl ArenaStatics for StaticsArena {
    fn kind(&self, id: &KindId) -> Fillable<Kind> {
        self.kinds[id].to_owned()
    }
    fn tpat(&self, id: &TPatId) -> TypePattern {
        self.tpats[id].to_owned()
    }
    fn r#type(&self, id: &TypeId) -> Fillable<Type> {
        self.types[id].to_owned()
    }
    fn vpat(&self, id: &VPatId) -> ValuePattern {
        self.vpats[id].to_owned()
    }
    fn value(&self, id: &ValueId) -> Value {
        self.values[id].to_owned()
    }
    fn compu(&self, id: &CompuId) -> Computation {
        self.compus[id].to_owned()
    }
    fn decl(&self, id: &DeclId) -> Declaration {
        self.decls[id].to_owned()
    }
}

/* -------------------------------- LocalFold ------------------------------- */

/// A set of local actions on static arena items.
#[auto_impl::auto_impl(&mut, Box)]
pub trait LocalFoldStatics<Cx>: ArenaStatics {
    fn action_kind(&mut self, kind: KindId, ctx: &Cx);
    fn action_tpat(&mut self, tpat: TPatId, ctx: &Cx);
    fn action_type(&mut self, r#type: TypeId, ctx: &Cx);
    fn action_vpat(&mut self, vpat: VPatId, ctx: &Cx);
    fn action_value(&mut self, value: ValueId, ctx: &Cx);
    fn action_compu(&mut self, compu: CompuId, ctx: &Cx);
    fn action_decl(&mut self, decl: DeclId, ctx: &Cx);
}
