//! Checker regressions for source reuse, inference, projection, and completion.

use super::*;

use std::sync::{Arc, Mutex};

#[salsa::db]
#[derive(Clone)]
pub(super) struct TestDb {
    storage: salsa::Storage<Self>,
    generated_def_spaces: Arc<zydeco_foundation::arena::KeySpaceRegistry<su::TermId>>,
    pub(super) pending: Arc<Mutex<Option<Arc<crate::query::PendingParts>>>>,
}

impl Default for TestDb {
    fn default() -> Self {
        Self {
            storage: salsa::Storage::default(),
            generated_def_spaces: Default::default(),
            pending: Arc::new(Mutex::new(None)),
        }
    }
}

#[salsa::db]
impl salsa::Database for TestDb {}

#[salsa::db]
impl crate::query::TyckDb for TestDb {
    fn generated_def_spaces(&self) -> &Arc<zydeco_foundation::arena::KeySpaceRegistry<su::TermId>> {
        &self.generated_def_spaces
    }

    fn pending_parts(&self) -> &Arc<Mutex<Option<Arc<crate::query::PendingParts>>>> {
        &self.pending
    }
}
use crate::environment::TyEnv;

pub(super) fn with_tycker<T>(
    build: impl FnOnce(
        &mut IdAllocator<zydeco_surface::scoped::arena::ScopedScope>,
        &mut su::ScopedArena,
    ) -> (su::TermId, T),
    test: impl FnOnce(&mut Tycker<'_>, T),
) {
    let mut allocator = IdAllocator::<zydeco_surface::scoped::arena::ScopedScope>::new();
    let mut scoped = su::ScopedArena::default();
    let (root, context) = build(&mut allocator, &mut scoped);

    let spans = su::SpanArena::default();
    let db = TestDb::default();
    *db.pending.lock().unwrap() = Some(Arc::new(crate::query::PendingParts {
        spans: spans.clone(),
        scoped: scoped.clone(),
        root,
    }));
    let data = crate::query::intern_pending(&db);
    let mut tycker = Tycker::new(&db, data, &spans, &scoped);
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
        normalizer.normalize_kind(unchanged, tycker).unwrap();
        normalizer.normalize_kind(pending, tycker).unwrap();

        assert!(tycker.statics.kinds_normalized.get(unchanged).is_none());
        assert!(matches!(tycker.statics.normalized_kind_at(unchanged), Some(ss::Kind::VType(_))));
        assert!(matches!(tycker.statics.normalized_kind_at(pending), Some(ss::Kind::VType(_))));
        assert!(tycker.statics.kinds_normalized.get(pending).is_some());
    });
}

pub(super) fn anonymous_type_binder(
    tycker: &mut Tycker<'_>, kind: ss::KindId, _environment: &TyEnv,
) -> ss::TypeBinder {
    let definition: ss::DefId = tycker.fresh_def();
    let pattern: ss::TPatId = Alloc::alloc(tycker, definition, kind, &());
    let witness = Alloc::alloc(tycker, pattern, (), &());
    ss::TypeBinder { pattern, witness }
}

#[test]
fn def_gen_allocator_replays_per_root_and_distinguishes_roots() {
    let mut source_ids = IdAllocator::<zydeco_surface::scoped::arena::ScopedScope>::new();
    let root_a: su::TermId = source_ids.alloc();
    let root_b: su::TermId = source_ids.alloc();
    let spaces = zydeco_foundation::arena::KeySpaceRegistry::<su::TermId>::default();

    let mut first = crate::alloc::DefGenAllocator::new(&spaces, root_a);
    let first_pair = [first.fresh_def(), first.fresh_def()];
    let mut replay = crate::alloc::DefGenAllocator::new(&spaces, root_a);
    let replay_pair = [replay.fresh_def(), replay.fresh_def()];
    assert_eq!(first_pair, replay_pair);
    assert_ne!(first_pair[0], first_pair[1]);

    let mut other = crate::alloc::DefGenAllocator::new(&spaces, root_b);
    assert_ne!(other.fresh_def(), first_pair[0]);
}
