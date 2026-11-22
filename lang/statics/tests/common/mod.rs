use zydeco_foundation::prelude::IdAllocator;
use zydeco_statics::{
    Alloc, Tycker,
    surface_syntax::{ScopedArena, SpanArena},
    syntax::*,
};

#[salsa::db]
#[derive(Clone)]
pub struct TestDb {
    storage: salsa::Storage<Self>,
    generated_def_spaces: std::sync::Arc<
        zydeco_foundation::arena::KeySpaceRegistry<zydeco_statics::surface_syntax::TermId>,
    >,
    pending: std::sync::Arc<
        std::sync::Mutex<Option<std::sync::Arc<zydeco_statics::query::PendingParts>>>,
    >,
}

impl Default for TestDb {
    fn default() -> Self {
        Self {
            storage: salsa::Storage::default(),
            generated_def_spaces: Default::default(),
            pending: std::sync::Arc::new(std::sync::Mutex::new(None)),
        }
    }
}

#[salsa::db]
impl salsa::Database for TestDb {}

#[salsa::db]
impl zydeco_statics::query::TyckDb for TestDb {
    fn generated_def_spaces(
        &self,
    ) -> &std::sync::Arc<
        zydeco_foundation::arena::KeySpaceRegistry<zydeco_statics::surface_syntax::TermId>,
    > {
        &self.generated_def_spaces
    }

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
        let mut scoped = ScopedArena::default();
        let db = TestDb::default();
        let root = IdAllocator::<zydeco_surface::scoped::arena::ScopedScope>::new().alloc();
        scoped.terms.insert_new(root, zydeco_surface::scoped::syntax::Hole.into());
        *db.pending.lock().unwrap() =
            Some(std::sync::Arc::new(zydeco_statics::query::PendingParts {
                spans: spans.clone(),
                scoped: scoped.clone(),
                root,
            }));
        let data = zydeco_statics::query::intern_pending(&db);
        test(&mut Tycker::new(&db, data, &spans, &scoped));
    }

    pub fn kinds(tycker: &mut Tycker<'_>) -> (KindId, KindId) {
        (Alloc::alloc(tycker, VType, (), &()), Alloc::alloc(tycker, CType, (), &()))
    }
}
