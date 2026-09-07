//! Checker regressions for source reuse, inference, projection, and completion.

use super::*;

use std::sync::{Arc, Mutex};

#[salsa::db]
#[derive(Clone)]
pub(super) struct TestDb {
    storage: salsa::Storage<Self>,
    pub(super) pending: Arc<Mutex<Option<Arc<crate::query::PendingParts>>>>,
}

impl Default for TestDb {
    fn default() -> Self {
        Self { storage: salsa::Storage::default(), pending: Arc::new(Mutex::new(None)) }
    }
}

#[salsa::db]
impl salsa::Database for TestDb {}

#[salsa::db]
impl crate::query::TyckDb for TestDb {
    fn pending_parts(&self) -> &Arc<Mutex<Option<Arc<crate::query::PendingParts>>>> {
        &self.pending
    }
}
use crate::environment::TyEnv;

pub(super) fn with_tycker<T>(
    build: impl FnOnce(
        &mut IdAllocator<zydeco_surface::bitter::arena::BitterScope>,
        &mut su::ScopedArena,
    ) -> (su::TermId, T),
    test: impl FnOnce(&mut Tycker<'_>, T),
) {
    let mut allocator = IdAllocator::<zydeco_surface::bitter::arena::BitterScope>::new();
    let mut scoped = su::ScopedArena::default();
    let (root, context) = build(&mut allocator, &mut scoped);

    let spans = su::SpanArena::default();
    let prim = su::PrimDefs::default();
    let db = TestDb::default();
    *db.pending.lock().unwrap() = Some(Arc::new(crate::query::PendingParts {
        spans: spans.clone(),
        prim: prim.clone(),
        scoped: scoped.clone(),
        root,
    }));
    let data = crate::query::intern_pending(&db);
    let mut tycker = Tycker::new(&db, data, &spans, &prim, &scoped);
    test(&mut tycker, context);
}

pub(super) fn with_empty_tycker(test: impl FnOnce(&mut Tycker<'_>)) {
    with_tycker(
        |allocator, scoped| {
            let root = allocator.alloc();
            scoped.terms.insert_new(root, su::Hole.into());
            (root, ())
        },
        |tycker, ()| test(tycker),
    );
}

#[test]
fn kind_normalization_stores_only_changed_forms() {
    with_empty_tycker(|tycker| {
        let unchanged = Alloc::alloc(tycker, ss::VType, (), &());
        let root = tycker.data.root(tycker.db);
        let fill = Alloc::alloc(tycker, root, (), &());
        let pending = Alloc::alloc(tycker, fill, (), &());
        tycker.statics.solus.insert_new(fill, unchanged.into());

        let mut normalizer = crate::normalize::FilledNormalizer::default();
        normalizer.normalize_kind_k(unchanged, tycker).unwrap();
        normalizer.normalize_kind_k(pending, tycker).unwrap();

        assert!(tycker.statics.kinds_normalized.get(&unchanged).is_none());
        assert!(matches!(tycker.statics.normalized_kind_at(unchanged), Some(ss::Kind::VType(_))));
        assert!(matches!(tycker.statics.normalized_kind_at(pending), Some(ss::Kind::VType(_))));
        assert!(tycker.statics.kinds_normalized.get(&pending).is_some());
    });
}

pub(super) fn anonymous_type_binder(
    tycker: &mut Tycker<'_>, kind: ss::KindId, environment: &TyEnv,
) -> ss::TypeBinder {
    let definition: ss::DefId = tycker.fresh();
    let pattern: ss::TPatId = Alloc::alloc(tycker, definition, kind, environment);
    let witness = Alloc::alloc(tycker, pattern, (), &());
    ss::TypeBinder { pattern, witness }
}
