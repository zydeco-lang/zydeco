//! Allocation of entities in [`StaticsArena`].
//!
//! This module provides the [`Alloc`] trait and all its implementations,
//! which provides a type-safe approach to allocate in a post-type-check arena.

use crate::*;
use zydeco_foundation::arena::{KeySpaceId, KeySpaceRegistry, derived_id};

/// Per-root sequential issuer for synthesized [`DefId`]s.
///
/// Generated definitions share the session's `DefId` space alongside source definitions;
/// keying this issuer on `KeySpaceRegistry<su::TermId>` replays identical generated IDs
/// across query eviction while dense statics nodes allocate straight into `StaticsArena`.
#[derive(Debug)]
pub struct DefGenAllocator {
    key_space: KeySpaceId,
    next: u32,
}

impl DefGenAllocator {
    pub fn new(spaces: &KeySpaceRegistry<su::TermId>, root: su::TermId) -> Self {
        Self { key_space: spaces.intern(root), next: 0 }
    }

    pub fn fresh_def(&mut self) -> DefId {
        let slot = self.next;
        self.next = slot.checked_add(1).expect("generated definition slot range exhausted");
        derived_id(self.key_space, slot)
    }
}

/// Trait for allocating entities in [`StaticsArena`].
/// The only method provided is [`Alloc::alloc`], which takes `&mut` arena,
/// the value of type `Self` to allocate, and the annotation of the value.
///
/// [`Construct`] builds on [`Alloc`]: its constructions infer the annotations that
/// [`Alloc::alloc`] takes explicitly, which is more convenient when they can be inferred.
pub trait Alloc<Arena, T> {
    /// The annotation of this allocation.
    type Ann;
    /// Extra allocation context (for example `SkolemScope` on type fills).
    type Env;
    /// Allocates the value in the arena and returns the allocated identifier.
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, env: &Self::Env) -> T;
}

/// Allocation capability held by a live type-checking pass, separate from its
/// durable [`StaticsArena`] storage.
pub trait StaticsAlloc: AsMut<DefGenAllocator> + AsMut<StaticsArena> {
    fn fresh_def(&mut self) -> DefId {
        AsMut::<DefGenAllocator>::as_mut(self).fresh_def()
    }

    fn alloc_abst(&mut self) -> AbstId {
        AsMut::<StaticsArena>::as_mut(self).absts.alloc(())
    }

    fn alloc_fill(&mut self, site: InferenceSite) -> FillId {
        AsMut::<StaticsArena>::as_mut(self).fills.alloc(site)
    }

    fn alloc_kind_pre(&mut self, value: Fillable<Kind>) -> KindId {
        AsMut::<StaticsArena>::as_mut(self).kinds_pre.alloc(value)
    }

    fn alloc_kpat(&mut self, value: KindPattern) -> KPatId {
        AsMut::<StaticsArena>::as_mut(self).kpats.alloc(value)
    }

    fn alloc_tpat(&mut self, value: TypePattern) -> TPatId {
        AsMut::<StaticsArena>::as_mut(self).tpats.alloc(value)
    }

    fn alloc_type_pre(&mut self, value: Fillable<Type>, kind: KindId) -> TypeId {
        AsMut::<StaticsArena>::as_mut(self).types_pre.alloc(value, kind)
    }

    fn alloc_vpat(&mut self, value: ValuePattern) -> VPatId {
        AsMut::<StaticsArena>::as_mut(self).vpats.alloc(value)
    }

    fn alloc_value(&mut self, value: Value) -> ValueId {
        AsMut::<StaticsArena>::as_mut(self).values.alloc(value)
    }

    fn alloc_compu(&mut self, value: Computation) -> CompuId {
        AsMut::<StaticsArena>::as_mut(self).compus.alloc(value)
    }
}

impl<Arena> StaticsAlloc for Arena where Arena: AsMut<DefGenAllocator> + AsMut<StaticsArena> {}

/* ------------------------------- Definition ------------------------------- */

impl<Arena> Alloc<Arena, DefId> for VarName
where
    Arena: StaticsAlloc,
{
    type Ann = AnnId;
    type Env = ();
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, (): &Self::Env) -> DefId {
        let id = arena.fresh_def();
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        statics.generated_defs.insert_new(id, val);
        statics.annotations_var.insert_new(id, ann);
        id
    }
}

/* -------------------------------- Abstract -------------------------------- */

impl<Arena> Alloc<Arena, AbstId> for DefId
where
    Arena: StaticsAlloc,
{
    type Ann = KindId;
    type Env = ();
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, (): &Self::Env) -> AbstId {
        let abst = arena.alloc_abst();
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        statics.annotations_abst.insert_new(abst, ann);
        statics.abst_hints.insert_new(abst, val);
        abst
    }
}
impl<Arena> Alloc<Arena, AbstId> for Option<DefId>
where
    Arena: StaticsAlloc,
{
    type Ann = KindId;
    type Env = ();
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, (): &Self::Env) -> AbstId {
        let abst = arena.alloc_abst();
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        statics.annotations_abst.insert_new(abst, ann);
        if let Some(def) = val {
            statics.abst_hints.insert_new(abst, def);
        }
        abst
    }
}
impl<Arena> Alloc<Arena, AbstId> for TPatId
where
    Arena: StaticsAlloc + AsRef<StaticsArena>,
{
    type Ann = ();
    type Env = ();
    fn alloc(arena: &mut Arena, val: Self, (): Self::Ann, (): &Self::Env) -> AbstId {
        let (def, kd) = val.try_destruct_def(arena);
        Alloc::alloc(arena, def, kd, &())
    }
}

/* ---------------------------------- Fill ---------------------------------- */

impl<Arena, S> Alloc<Arena, FillId> for S
where
    Arena: StaticsAlloc,
    S: Into<InferenceSite>,
{
    type Ann = ();
    type Env = ();
    fn alloc(arena: &mut Arena, val: Self, (): Self::Ann, (): &Self::Env) -> FillId {
        arena.alloc_fill(val.into())
    }
}

/* ---------------------------------- Kind ---------------------------------- */

impl<Arena> Alloc<Arena, KindId> for FillId
where
    Arena: StaticsAlloc,
{
    type Ann = ();
    type Env = ();
    fn alloc(arena: &mut Arena, val: Self, (): Self::Ann, (): &Self::Env) -> KindId {
        arena.alloc_kind_pre(val.into())
    }
}
impl<Arena, S> Alloc<Arena, KindId> for S
where
    Arena: StaticsAlloc,
    S: Into<Kind>,
{
    type Ann = ();
    type Env = ();
    fn alloc(arena: &mut Arena, val: Self, (): Self::Ann, (): &Self::Env) -> KindId {
        arena.alloc_kind_pre(Fillable::Done(val.into()))
    }
}

/* ------------------------------- KindPattern ------------------------------ */

impl<Arena, S> Alloc<Arena, KPatId> for S
where
    Arena: StaticsAlloc,
    S: Into<KindPattern>,
{
    type Ann = ();
    type Env = ();

    fn alloc(arena: &mut Arena, val: Self, (): Self::Ann, (): &Self::Env) -> KPatId {
        arena.alloc_kpat(val.into())
    }
}

/* ------------------------------- TypePattern ------------------------------ */

impl<Arena, S> Alloc<Arena, TPatId> for S
where
    Arena: StaticsAlloc,
    S: Into<TypePattern>,
{
    type Ann = KindId;
    type Env = ();
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, (): &Self::Env) -> TPatId {
        let tpat = arena.alloc_tpat(val.into());
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        statics.annotations_tpat.insert_new(tpat, ann);
        tpat
    }
}

/* ---------------------------------- Type ---------------------------------- */

impl<Arena> Alloc<Arena, TypeId> for FillId
where
    Arena: StaticsAlloc,
{
    type Ann = KindId;
    type Env = SkolemScope;
    fn alloc(arena: &mut Arena, val: Self, kd: Self::Ann, scope: &Self::Env) -> TypeId {
        let ty = arena.alloc_type_pre(val.into(), kd);
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        if let Some(existing) = statics.fill_scopes.insert_or_get(val, scope.clone()) {
            statics.fill_scopes.replace_existing(val, existing.intersection(scope));
        }
        ty
    }
}
impl<Arena, S> Alloc<Arena, TypeId> for S
where
    Arena: StaticsAlloc,
    S: Into<Type>,
{
    type Ann = KindId;
    type Env = ();
    fn alloc(arena: &mut Arena, val: Self, kd: Self::Ann, (): &Self::Env) -> TypeId {
        arena.alloc_type_pre(Fillable::Done(val.into()), kd)
    }
}

/* ------------------------------ ValuePattern ------------------------------ */

impl<Arena, S> Alloc<Arena, VPatId> for S
where
    Arena: StaticsAlloc,
    S: Into<ValuePattern>,
{
    type Ann = TypeId;
    type Env = ();
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, (): &Self::Env) -> VPatId {
        let vpat = arena.alloc_vpat(val.into());
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        statics.annotations_vpat.insert_new(vpat, ann);
        vpat
    }
}

/* ---------------------------------- Value --------------------------------- */

impl<Arena, S> Alloc<Arena, ValueId> for S
where
    Arena: StaticsAlloc,
    S: Into<Value>,
{
    type Ann = TypeId;
    type Env = ();
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, (): &Self::Env) -> ValueId {
        let value = arena.alloc_value(val.into());
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        statics.annotations_value.insert_new(value, ann);
        value
    }
}

/* ------------------------------- Computation ------------------------------ */

impl<Arena, S> Alloc<Arena, CompuId> for S
where
    Arena: StaticsAlloc,
    S: Into<Computation>,
{
    type Ann = TypeId;
    type Env = ();
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, (): &Self::Env) -> CompuId {
        let compu = arena.alloc_compu(val.into());
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        statics.annotations_compu.insert_new(compu, ann);
        compu
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use zydeco_foundation::arena::IdAllocator;

    #[test]
    fn def_gen_allocator_exhaustion_preserves_counter() {
        let mut source_ids = IdAllocator::<zydeco_surface::bitter::arena::BitterScope>::new();
        let root: su::TermId = source_ids.alloc();
        let spaces = KeySpaceRegistry::<su::TermId>::default();
        let mut allocator = DefGenAllocator::new(&spaces, root);
        allocator.next = u32::MAX - 1;
        assert_eq!(allocator.fresh_def(), derived_id(allocator.key_space, u32::MAX - 1));
        assert_eq!(allocator.next, u32::MAX);
        let failure = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            allocator.fresh_def();
        }))
        .unwrap_err();
        assert_eq!(
            failure.downcast_ref::<String>().map(String::as_str),
            Some("generated definition slot range exhausted")
        );
        // `checked_add(1)` overflowed `u32::MAX` before advancing `next`, so the
        // counter stays pinned at `u32::MAX` after saturation.
        assert_eq!(allocator.next, u32::MAX);
    }
}
