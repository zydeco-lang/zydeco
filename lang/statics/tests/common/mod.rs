use zydeco_statics::{
    Alloc, Tycker,
    surface_syntax::{PrimDefs, ScopedArena, SpanArena},
    tyck::syntax::*,
};

pub struct TestFixture;

impl TestFixture {
    pub fn run(test: impl FnOnce(&mut Tycker<'_>)) {
        let spans = SpanArena::new();
        let prim = PrimDefs::default();
        let deps = Default::default();
        let top = zydeco_utils::prelude::SccGraph::new(&deps, Default::default());
        let mut scoped = ScopedArena {
            defs: Default::default(),
            pats: Default::default(),
            terms: Default::default(),
            decls: Default::default(),
            textual: Default::default(),
            users: Default::default(),
            ctxs_term: Default::default(),
            ctxs_pat_local: Default::default(),
            coctxs_pat_local: Default::default(),
            coctxs_term_local: Default::default(),
            metas: Default::default(),
            exts: Default::default(),
            unis: Default::default(),
            deps,
            top,
        };
        test(&mut Tycker::new(&spans, &prim, &mut scoped));
    }

    pub fn kinds(tycker: &mut Tycker<'_>) -> (KindId, KindId) {
        (Alloc::alloc(tycker, VType, (), &()), Alloc::alloc(tycker, CType, (), &()))
    }
}
