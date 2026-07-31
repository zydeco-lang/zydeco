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
        let mut scoped = ScopedArena {
            defs: Default::default(),
            pats: Default::default(),
            terms: Default::default(),
            textual: Default::default(),
            users: Default::default(),
            ctxs_term: Default::default(),
            ctxs_pat_local: Default::default(),
            coctxs_pat_local: Default::default(),
            coctxs_term_local: Default::default(),
            blocks: Default::default(),
        };
        test(&mut Tycker::new(&spans, &prim, &mut scoped));
    }

    pub fn kinds(tycker: &mut Tycker<'_>) -> (KindId, KindId) {
        (Alloc::alloc(tycker, VType, (), &()), Alloc::alloc(tycker, CType, (), &()))
    }
}
