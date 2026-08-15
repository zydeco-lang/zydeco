use zydeco_statics::{
    Alloc, Tycker,
    surface_syntax::{PrimDefs, ScopedArena, SpanArena},
    syntax::*,
};
use zydeco_utils::prelude::IdAllocator;

#[salsa::db]
#[derive(Clone)]
pub struct TestDb {
    storage: salsa::Storage<Self>,
    pending: std::sync::Arc<
        std::sync::Mutex<Option<std::sync::Arc<zydeco_statics::query::PendingParts>>>,
    >,
}

impl Default for TestDb {
    fn default() -> Self {
        Self {
            storage: salsa::Storage::default(),
            pending: std::sync::Arc::new(std::sync::Mutex::new(None)),
        }
    }
}

#[salsa::db]
impl salsa::Database for TestDb {}

#[salsa::db]
impl zydeco_statics::query::TyckDb for TestDb {
    fn pending_parts(
        &self,
    ) -> &std::sync::Arc<
        std::sync::Mutex<Option<std::sync::Arc<zydeco_statics::query::PendingParts>>>,
    > {
        &self.pending
    }
}

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
            coctxs_term_local: Default::default(),
            blocks: Default::default(),
        };
        let db = TestDb::default();
        let root = IdAllocator::<zydeco_surface::scoped::arena::ScopedScope>::new().alloc();
        *db.pending.lock().unwrap() =
            Some(std::sync::Arc::new(zydeco_statics::query::PendingParts {
                spans: spans.clone(),
                prim: prim.clone(),
                scoped: scoped.clone(),
                root,
            }));
        let data = zydeco_statics::query::intern_pending(&db);
        test(&mut Tycker::new(&db, data, &spans, &prim, &mut scoped));
    }

    pub fn kinds(tycker: &mut Tycker<'_>) -> (KindId, KindId) {
        (Alloc::alloc(tycker, VType, (), &()), Alloc::alloc(tycker, CType, (), &()))
    }
}
