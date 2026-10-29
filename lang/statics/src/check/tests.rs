//! Checker regressions for source reuse, inference, projection, and completion.

use super::*;

use std::sync::{Arc, Mutex};

#[salsa::db]
#[derive(Clone)]
pub(super) struct TestDb {
    storage: salsa::Storage<Self>,
    derived_ids: std::sync::Arc<crate::DerivedIdRegistry>,
    pub(super) pending: Arc<Mutex<Option<Arc<crate::query::PendingParts>>>>,
}

impl Default for TestDb {
    fn default() -> Self {
        Self {
            storage: salsa::Storage::default(),
            derived_ids: Default::default(),
            pending: Arc::new(Mutex::new(None)),
        }
    }
}

#[salsa::db]
impl salsa::Database for TestDb {}

#[salsa::db]
impl crate::query::TyckDb for TestDb {
    fn derived_ids(&self) -> &std::sync::Arc<crate::DerivedIdRegistry> {
        &self.derived_ids
    }

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

#[test]
fn occurrence_exhaustion_preserves_the_counter_and_enclosing_allocation_site() {
    with_empty_tycker(|tycker| {
        let entity = su::EntityId::Term(tycker.data.root(tycker.db));
        let _ = tycker.check_counts.upsert(entity, u32::MAX - 1);
        let enclosing = tycker.allocator.query_key_space();
        tycker.enter_site(entity);
        assert_eq!(tycker.site_occurrence(), u32::MAX - 1);
        assert_eq!(tycker.check_counts.get(&entity), Some(&u32::MAX));
        tycker.allocator.exit();
        for _ in 0..2 {
            let failure = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                tycker.enter_site(entity);
            }))
            .unwrap_err();
            assert_eq!(
                failure.downcast_ref::<String>().map(String::as_str),
                Some("checking occurrence range exhausted")
            );
            assert_eq!(tycker.check_counts.get(&entity), Some(&u32::MAX));
            assert_eq!(tycker.allocator.query_key_space(), enclosing);
        }
    });
}

#[test]
fn recursive_binding_slot_exhaustion_cannot_alias_earlier_bindings() {
    use crate::query::{InternedBindingIndex, rec_group_abst_judgment_at};
    with_empty_tycker(|tycker| {
        let db = tycker.db;
        let site = tycker.query_site();
        let first_index = InternedBindingIndex::with_usize(db, 0);
        let first = rec_group_abst_judgment_at(db, site, first_index).unwrap();
        let last_valid = (u32::MAX - 2) / 3;
        let last_index = InternedBindingIndex::new(db, last_valid);
        let last = rec_group_abst_judgment_at(db, site, last_index).unwrap();
        assert_ne!(first, last);
        assert_eq!(last.2.raw().into_u32(), last_valid * 3 + 2);
        for rejected in [last_valid + 1, u32::MAX] {
            let index = InternedBindingIndex::new(db, rejected);
            let failure = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                rec_group_abst_judgment_at(db, site, index)
            }))
            .unwrap_err();
            assert_eq!(
                failure.downcast_ref::<String>().map(String::as_str),
                Some("recursive binding slot range exhausted")
            );
            assert_eq!(rec_group_abst_judgment_at(db, site, first_index).unwrap(), first);
        }
        if let Some(too_large) = (u32::MAX as usize).checked_add(1) {
            let failure = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                let _ = InternedBindingIndex::with_usize(db, too_large);
            }))
            .unwrap_err();
            assert!(
                failure
                    .downcast_ref::<String>()
                    .unwrap()
                    .starts_with("recursive binding index range exhausted")
            );
            assert_eq!(rec_group_abst_judgment_at(db, site, first_index).unwrap(), first);
        }
    });
}
