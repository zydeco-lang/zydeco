//! Allocation of entities in [`StaticsArena`].
//!
//! This module provides the [`Alloc`] trait and all its implementations,
//! which provides a type-safe approach to allocate in a post-type-check arena.

use crate::*;
use zydeco_utils::arena::ArenaId;

mod identity;
pub use identity::{DerivedAllocator, DerivedIdRegistry};

/// Trait for allocating entities in [`StaticsArena`].
/// The only method provided is [`Alloc::alloc`], which takes `&mut` [`Tycker`],
/// the value of type `Self` to allocate, and the annotation of the value.
/// Some key parameters are:
///
/// + The parameter `T` is the "target" type allocation.
///   Callers of this trait will get a value of type `T` after allocation.
/// + The parameter [`Alloc::Ann`] is the type of the annotation of the entity.
///   Each implementation will specify a suitable annotation to ensure type safety.
///
/// [`Construct`] builds on [`Alloc`]: its constructions infer the annotations that
/// [`Alloc::alloc`] takes explicitly, which is more convenient when they can be inferred.
pub trait Alloc<Arena, T> {
    /// The annotation of this allocation.
    type Ann;
    /// The environment of this allocation.
    type Env;
    /// Allocates the value in the arena in the [`Tycker`] and returns the allocated value.
    /// See the documentation of trait [`Alloc`] and [`crate::alloc`] for more details.
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, env: &Self::Env) -> T;
}

/// Allocation capability held by a live type-checking pass, separate from its
/// durable [`StaticsArena`] storage.
pub trait StaticsAlloc: AsMut<DerivedAllocator> + AsMut<StaticsArena> {
    fn fresh<Id>(&mut self) -> Id
    where
        Id: ArenaId,
        StaticsScope: Allocates<Id>,
    {
        AsMut::<DerivedAllocator>::as_mut(self).fresh()
    }

    fn alloc_kind_pre(&mut self, value: Fillable<Kind>) -> KindId {
        let id = self.fresh();
        AsMut::<StaticsArena>::as_mut(self).kinds_pre.insert_new(id, value);
        id
    }

    fn alloc_kpat(&mut self, value: KindPattern) -> KPatId {
        let id = self.fresh();
        AsMut::<StaticsArena>::as_mut(self).kpats.insert_new(id, value);
        id
    }

    fn alloc_tpat(&mut self, value: TypePattern) -> TPatId {
        let id = self.fresh();
        AsMut::<StaticsArena>::as_mut(self).tpats.insert_new(id, value);
        id
    }

    fn alloc_type_pre(&mut self, value: Fillable<Type>, kind: KindId) -> TypeId {
        let id = self.fresh();
        AsMut::<StaticsArena>::as_mut(self).types_pre.insert_new(id, value, kind);
        id
    }

    fn alloc_vpat(&mut self, value: ValuePattern) -> VPatId {
        let id = self.fresh();
        AsMut::<StaticsArena>::as_mut(self).vpats.insert_new(id, value);
        id
    }

    fn alloc_value(&mut self, value: Value) -> ValueId {
        let id = self.fresh();
        AsMut::<StaticsArena>::as_mut(self).values.insert_new(id, value);
        id
    }

    fn alloc_compu(&mut self, value: Computation) -> CompuId {
        let id = self.fresh();
        AsMut::<StaticsArena>::as_mut(self).compus.insert_new(id, value);
        id
    }
}

impl<Arena> StaticsAlloc for Arena where Arena: AsMut<DerivedAllocator> + AsMut<StaticsArena> {}

/* ------------------------------- Definition ------------------------------- */

impl<Arena> Alloc<Arena, DefId> for VarName
where
    Arena: StaticsAlloc,
{
    type Ann = AnnId;
    type Env = ();
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, (): &Self::Env) -> DefId {
        let id = arena.fresh();
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
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, _env: &Self::Env) -> AbstId {
        let abst: AbstId = arena.fresh();
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        statics.absts.insert_new(abst, ());
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
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, _env: &Self::Env) -> AbstId {
        let abst: AbstId = arena.fresh();
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        statics.absts.insert_new(abst, ());
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
    fn alloc(arena: &mut Arena, val: Self, (): Self::Ann, env: &Self::Env) -> AbstId {
        let (def, kd) = val.try_destruct_def(arena);
        Alloc::alloc(arena, def, kd, env)
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
    fn alloc(arena: &mut Arena, val: Self, (): Self::Ann, _env: &Self::Env) -> FillId {
        let id = arena.fresh();
        AsMut::<StaticsArena>::as_mut(arena).fills.insert_new(id, val.into());
        id
    }
}

/* ---------------------------------- Kind ---------------------------------- */

impl<Arena> Alloc<Arena, KindId> for FillId
where
    Arena: StaticsAlloc,
{
    type Ann = ();
    type Env = ();
    fn alloc(arena: &mut Arena, val: Self, (): Self::Ann, _env: &Self::Env) -> KindId {
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
    fn alloc(arena: &mut Arena, val: Self, (): Self::Ann, _env: &Self::Env) -> KindId {
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
    type Env = TyEnv;

    fn alloc(arena: &mut Arena, val: Self, (): Self::Ann, env: &Self::Env) -> KPatId {
        let kpat = arena.alloc_kpat(val.into());
        AsMut::<StaticsArena>::as_mut(arena).env_kpat.insert_new(kpat, env.clone());
        kpat
    }
}

/* ------------------------------- TypePattern ------------------------------ */

impl<Arena, S> Alloc<Arena, TPatId> for S
where
    Arena: StaticsAlloc,
    S: Into<TypePattern>,
{
    type Ann = KindId;
    type Env = TyEnv;
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, env: &Self::Env) -> TPatId {
        let tpat = arena.alloc_tpat(val.into());
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        statics.annotations_tpat.insert_new(tpat, ann);
        statics.env_tpat.insert_new(tpat, env.clone());
        tpat
    }
}

/* ---------------------------------- Type ---------------------------------- */

impl<Arena> Alloc<Arena, TypeId> for FillId
where
    Arena: StaticsAlloc,
{
    type Ann = KindId;
    type Env = TyEnv;
    fn alloc(arena: &mut Arena, val: Self, kd: Self::Ann, env: &Self::Env) -> TypeId {
        let ty = arena.alloc_type_pre(val.into(), kd);
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        let scope = env.skolem_scope().clone();
        let env = statics.intern_env(env);
        statics.env_type.insert_new(ty, env);
        if let Some(existing) = statics.fill_scopes.insert_or_get(val, scope.clone()) {
            statics.fill_scopes.replace_existing(val, existing.intersection(&scope));
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
    type Env = TyEnv;
    fn alloc(arena: &mut Arena, val: Self, kd: Self::Ann, env: &Self::Env) -> TypeId {
        let ty = arena.alloc_type_pre(Fillable::Done(val.into()), kd);
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        let env = statics.intern_env(env);
        statics.env_type.insert_new(ty, env);
        ty
    }
}

/* ------------------------------ ValuePattern ------------------------------ */

impl<Arena, S> Alloc<Arena, VPatId> for S
where
    Arena: StaticsAlloc,
    S: Into<ValuePattern>,
{
    type Ann = TypeId;
    type Env = TyEnv;
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, env: &Self::Env) -> VPatId {
        let vpat = arena.alloc_vpat(val.into());
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        statics.annotations_vpat.insert_new(vpat, ann);
        statics.env_vpat.insert_new(vpat, env.clone());
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
    type Env = TyEnv;
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, env: &Self::Env) -> ValueId {
        let value = arena.alloc_value(val.into());
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        statics.annotations_value.insert_new(value, ann);
        statics.env_value.insert_new(value, env.clone());
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
    type Env = TyEnv;
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, env: &Self::Env) -> CompuId {
        let compu = arena.alloc_compu(val.into());
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        statics.annotations_compu.insert_new(compu, ann);
        statics.env_compu.insert_new(compu, env.clone());
        compu
    }
}
