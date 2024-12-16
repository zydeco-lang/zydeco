use super::syntax::*;
use crate::surface_syntax as su;
use zydeco_utils::arena::*;

/* ---------------------------------- Arena --------------------------------- */

/// An arena of equivalence classes, designed for types, and structurally shared
/// `data` and `codata` definitions.
#[derive(Debug)]
pub struct ArenaEquiv<Id, Definition, Query>
where
    Id: IndexLike<Meta = usize>,
{
    /// arena for definitions
    pub defs: ArenaDense<Id, Definition>,
    /// arena for query hashmap
    pub tbls: ArenaAssoc<Id, Query>,
    /// arena for equivalence classes
    pub eqs: ArenaAssoc<Query, Id>,
}
impl<Id, Definition, Query> ArenaEquiv<Id, Definition, Query>
where
    Id: IndexLike<Meta = usize>,
    Query: Clone + Eq + std::hash::Hash,
{
    pub fn new_arc(alloc: ArcGlobalAlloc) -> Self {
        Self {
            defs: ArenaDense::new(alloc.alloc()),
            tbls: ArenaAssoc::new(),
            eqs: ArenaAssoc::new(),
        }
    }
    pub fn lookup_or_alloc(&mut self, def: Definition, query: Query) -> Id {
        if let Some(id) = self.eqs.get(&query) {
            // if the query is already registered, just return the DataId
            *id
        } else {
            // else, register the query
            let id = self.defs.alloc(def);
            self.tbls.insert(id, query.clone());
            self.eqs.insert(query, id);
            id
        }
    }
}

/// Item projectors out of the statics arena.
#[auto_impl::auto_impl(&, &mut, Box, Rc, Arc)]
pub trait ArenaStatics {
    fn kind(&self, id: &KindId) -> Kind;
    fn tpat(&self, id: &TPatId) -> TypePattern;
    fn r#type(&self, id: &TypeId) -> Type;
    fn vpat(&self, id: &VPatId) -> ValuePattern;
    fn value(&self, id: &ValueId) -> Value;
    fn compu(&self, id: &CompuId) -> Computation;
    fn decl(&self, id: &DeclId) -> Declaration;
}

#[derive(Debug)]
pub struct StaticsArena {
    // arenas
    pub kinds: ArenaSparse<KindId, Kind>,
    pub tpats: ArenaSparse<TPatId, TypePattern>,
    pub types: ArenaSparse<TypeId, Type>,
    pub vpats: ArenaSparse<VPatId, ValuePattern>,
    pub values: ArenaSparse<ValueId, Value>,
    pub compus: ArenaSparse<CompuId, Computation>,
    pub decls: ArenaAssoc<DeclId, Declaration>,

    // untyped to typed bijective maps
    pub pats: ArenaBijective<su::PatId, PatId>,
    pub terms: ArenaBijective<su::TermId, TermId>,

    /// arena for abstract types
    pub absts: ArenaDense<AbstId, ()>,
    /// the abstract types generated from sealed types
    pub seals: ArenaAssoc<AbstId, TypeId>,
    /// name hints for abstract types
    pub abst_hints: ArenaAssoc<AbstId, DefId>,
    /// arena for filling context-constrained holes; the TermId is the site
    pub fills: ArenaDense<FillId, su::TermId>,
    /// arena for the solutions of fillings
    pub solus: ArenaAssoc<FillId, AnnId>,
    /// which holes are introduced by the user and should be reported
    pub fill_hints: ArenaAssoc<FillId, ()>,
    /// arena for `data`
    pub datas: ArenaEquiv<DataId, im::Vector<(CtorName, TypeId)>, Data>,
    /// arena for `codata`
    pub codatas: ArenaEquiv<CoDataId, im::Vector<(DtorName, TypeId)>, CoData>,
    // /// arena for inlinable definitions
    // pub inlinables: ArenaAssoc<DefId, ValueId>,
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

            pats: ArenaBijective::new(),
            terms: ArenaBijective::new(),

            absts: ArenaDense::new(alloc.alloc()),
            seals: ArenaAssoc::new(),
            abst_hints: ArenaAssoc::new(),
            fills: ArenaDense::new(alloc.alloc()),
            solus: ArenaAssoc::new(),
            fill_hints: ArenaAssoc::new(),
            datas: ArenaEquiv::new_arc(alloc.clone()),
            codatas: ArenaEquiv::new_arc(alloc),
            // inlinables: ArenaAssoc::new(),
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
    fn kind(&self, id: &KindId) -> Kind {
        self.kinds[id].to_owned()
    }
    fn tpat(&self, id: &TPatId) -> TypePattern {
        self.tpats[id].to_owned()
    }
    fn r#type(&self, id: &TypeId) -> Type {
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

use super::Tycker;

impl ArenaStatics for Tycker {
    fn kind(&self, id: &KindId) -> Kind {
        self.statics.kind(id)
    }
    fn tpat(&self, id: &TPatId) -> TypePattern {
        self.statics.tpat(id)
    }
    fn r#type(&self, id: &TypeId) -> Type {
        self.statics.r#type(id)
    }
    fn vpat(&self, id: &VPatId) -> ValuePattern {
        self.statics.vpat(id)
    }
    fn value(&self, id: &ValueId) -> Value {
        self.statics.value(id)
    }
    fn compu(&self, id: &CompuId) -> Computation {
        self.statics.compu(id)
    }
    fn decl(&self, id: &DeclId) -> Declaration {
        self.statics.decl(id)
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

