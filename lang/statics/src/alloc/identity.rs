//! Session-owned identity interning and checker-local allocation cursors.

use crate::{query::IntrinsicKey, surface_syntax as su};
use std::sync::Arc;
use zydeco_utils::arena::{ArenaId, KeySpaceId, KeySpaceRegistry, derived_id};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum DerivationFamily {
    Checker,
    Query,
}

/// Root allocation is a separate origin, not a sentinel source identity.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum CheckSite {
    Root(su::TermId),
    Entity { entity: su::EntityId, occurrence: u32 },
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum DerivationSite {
    Allocation { family: DerivationFamily, site: CheckSite },
    Intrinsic { root: su::TermId, key: IntrinsicKey },
}

/// Identity registry shared by a session's checker runs and producer queries.
///
/// Keep this registry alive across query eviction and replay, and share it
/// between database clones. Its entries contain identities, not arena payloads.
#[derive(Debug, Default)]
pub struct DerivedIdRegistry {
    spaces: KeySpaceRegistry<DerivationSite>,
}

impl DerivedIdRegistry {
    fn key_space(&self, family: DerivationFamily, site: CheckSite) -> KeySpaceId {
        self.spaces.intern(DerivationSite::Allocation { family, site })
    }

    pub(crate) fn query_space(
        &self, entity: impl Into<su::EntityId>, occurrence: u32,
    ) -> KeySpaceId {
        self.key_space(
            DerivationFamily::Query,
            CheckSite::Entity { entity: entity.into(), occurrence },
        )
    }

    pub(crate) fn intrinsic_space(&self, root: su::TermId, key: IntrinsicKey) -> KeySpaceId {
        self.spaces.intern(DerivationSite::Intrinsic { root, key })
    }
}

#[derive(Debug)]
struct AllocationFrame {
    site: CheckSite,
    checker: KeySpaceId,
    query: KeySpaceId,
    next: u32,
}

impl AllocationFrame {
    fn new(registry: &DerivedIdRegistry, site: CheckSite) -> Self {
        Self {
            site,
            checker: registry.key_space(DerivationFamily::Checker, site),
            query: registry.key_space(DerivationFamily::Query, site),
            next: 0,
        }
    }
}

/// Checker-local cursors over allocation sites interned by the session.
///
/// Entering a site resolves its checker and producer key spaces once. Local
/// allocations then advance a checked slot counter without registry lookups.
/// Re-entering the same site restarts its slots and reproduces its IDs.
#[derive(Debug)]
pub struct DerivedAllocator {
    registry: Arc<DerivedIdRegistry>,
    sites: Vec<AllocationFrame>,
}

impl DerivedAllocator {
    /// Start a check with an explicit registry and its resolved source root.
    pub fn new(registry: Arc<DerivedIdRegistry>, root: su::TermId) -> Self {
        let frame = AllocationFrame::new(&registry, CheckSite::Root(root));
        Self { registry, sites: vec![frame] }
    }

    pub(crate) fn enter(&mut self, entity: su::EntityId, occurrence: u32) {
        self.sites
            .push(AllocationFrame::new(&self.registry, CheckSite::Entity { entity, occurrence }));
    }

    pub(crate) fn occurrence(&self) -> u32 {
        match self.current().site {
            | CheckSite::Root(_) => 0,
            | CheckSite::Entity { occurrence, .. } => occurrence,
        }
    }

    pub(crate) fn query_key_space(&self) -> KeySpaceId {
        self.current().query
    }

    fn current(&self) -> &AllocationFrame {
        self.sites.last().expect("the root allocation site always exists")
    }

    pub(crate) fn exit(&mut self) {
        assert!(self.sites.len() > 1, "cannot leave the root allocation site");
        self.sites.pop();
    }

    pub(super) fn fresh<Id: ArenaId>(&mut self) -> Id {
        let frame = self.sites.last_mut().expect("the root allocation site always exists");
        let slot = frame.next;
        frame.next = slot.checked_add(1).expect("derived allocation slot range exhausted");
        derived_id(frame.checker, slot)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::TypeId;
    use std::{
        collections::HashSet,
        panic::{AssertUnwindSafe, catch_unwind},
    };
    use zydeco_surface::bitter::arena::BitterScope;
    use zydeco_utils::arena::IdAllocator;

    #[test]
    fn registry_preserves_full_origins_families_and_occurrences() {
        let mut source = IdAllocator::<BitterScope>::new();
        let root: su::TermId = source.alloc();
        let other: su::TermId = source.alloc();
        let registry = DerivedIdRegistry::default();
        let sites = [
            DerivationSite::Allocation {
                family: DerivationFamily::Checker,
                site: CheckSite::Root(root),
            },
            DerivationSite::Allocation {
                family: DerivationFamily::Query,
                site: CheckSite::Root(root),
            },
            DerivationSite::Allocation {
                family: DerivationFamily::Checker,
                site: CheckSite::Root(other),
            },
            DerivationSite::Allocation {
                family: DerivationFamily::Checker,
                site: CheckSite::Entity { entity: root.into(), occurrence: 0 },
            },
            DerivationSite::Allocation {
                family: DerivationFamily::Query,
                site: CheckSite::Entity { entity: root.into(), occurrence: 0 },
            },
            DerivationSite::Allocation {
                family: DerivationFamily::Query,
                site: CheckSite::Entity { entity: root.into(), occurrence: 1 },
            },
            DerivationSite::Allocation {
                family: DerivationFamily::Query,
                site: CheckSite::Entity { entity: other.into(), occurrence: 0 },
            },
            DerivationSite::Intrinsic { root, key: IntrinsicKey::VType },
            DerivationSite::Intrinsic { root, key: IntrinsicKey::CType },
            DerivationSite::Intrinsic { root: other, key: IntrinsicKey::VType },
        ];
        let spaces = sites.map(|site| registry.spaces.intern(site));
        assert_eq!(spaces.into_iter().collect::<HashSet<_>>().len(), sites.len());
        for (site, space) in sites.into_iter().zip(spaces) {
            assert_eq!(registry.spaces.intern(site), space);
        }
    }

    #[test]
    fn source_entity_categories_are_part_of_the_registered_site() {
        let source_space = KeySpaceRegistry::default().intern(());
        let term: su::TermId = derived_id(source_space, 0);
        let pattern: su::PatId = derived_id(source_space, 0);
        let registry = DerivedIdRegistry::default();
        assert_ne!(registry.query_space(term, 0), registry.query_space(pattern, 0));
    }

    #[test]
    fn nested_sites_resume_slots_and_replay_in_the_same_registry() {
        let mut source = IdAllocator::<BitterScope>::new();
        let root: su::TermId = source.alloc();
        let nested: su::TermId = source.alloc();
        let registry = Arc::new(DerivedIdRegistry::default());
        let mut allocator = DerivedAllocator::new(registry.clone(), root);
        let root_id: TypeId = allocator.fresh();
        allocator.enter(root.into(), 0);
        let first: TypeId = allocator.fresh();
        assert_ne!(root_id, first);
        assert_eq!(allocator.query_key_space(), registry.query_space(root, 0));
        assert_ne!(first.key_space(), allocator.query_key_space());

        allocator.enter(nested.into(), 0);
        assert_ne!(allocator.fresh::<TypeId>(), first);
        allocator.exit();
        let next: TypeId = allocator.fresh();
        assert_eq!(next, derived_id(first.key_space(), 1));
        allocator.exit();
        allocator.enter(root.into(), 0);
        assert_eq!(allocator.fresh::<TypeId>(), first);
        assert_eq!(allocator.fresh::<TypeId>(), next);
        allocator.exit();
        allocator.enter(root.into(), 1);
        assert_eq!(allocator.occurrence(), 1);
        assert_ne!(allocator.fresh::<TypeId>(), first);

        let mut replay = DerivedAllocator::new(registry, root);
        assert_eq!(replay.fresh::<TypeId>(), root_id);
        replay.enter(root.into(), 0);
        assert_eq!(replay.fresh::<TypeId>(), first);
        assert_eq!(replay.fresh::<TypeId>(), next);
    }

    #[test]
    fn slot_exhaustion_rejects_without_reusing_an_identifier_or_mutating_the_cursor() {
        let root = IdAllocator::<BitterScope>::new().alloc();
        let mut allocator = DerivedAllocator::new(Default::default(), root);
        allocator.sites.last_mut().unwrap().next = u32::MAX - 1;
        let last: TypeId = allocator.fresh();
        assert_eq!(last.raw().into_u32(), u32::MAX - 1);
        for _ in 0..2 {
            let failure =
                catch_unwind(AssertUnwindSafe(|| allocator.fresh::<TypeId>())).unwrap_err();
            assert_eq!(
                failure.downcast_ref::<String>().map(String::as_str),
                Some("derived allocation slot range exhausted")
            );
            assert_eq!(allocator.current().next, u32::MAX);
            assert_eq!(allocator.current().checker, last.key_space());
        }
    }

    #[test]
    fn leaving_the_root_is_rejected_without_losing_allocation_state() {
        let root = IdAllocator::<BitterScope>::new().alloc();
        let mut allocator = DerivedAllocator::new(Default::default(), root);
        let first: TypeId = allocator.fresh();
        let failure = catch_unwind(AssertUnwindSafe(|| allocator.exit())).unwrap_err();
        assert_eq!(failure.downcast_ref::<&str>(), Some(&"cannot leave the root allocation site"));
        assert_eq!(allocator.current().site, CheckSite::Root(root));
        assert_eq!(allocator.fresh::<TypeId>(), derived_id(first.key_space(), 1));
    }
}
