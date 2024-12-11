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
    /// arena for inlinable definitions
    pub inlinables: ArenaAssoc<DefId, ValueId>,

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
            inlinables: ArenaAssoc::new(),

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
