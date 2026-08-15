//! Allocation of entities in [`StaticsArena`].
//!
//! This module provides the [`Alloc`] trait and all its implementations,
//! which provides a type-safe approach to allocate in a post-type-check arena.

use super::syntax::*;
use crate::*;
use zydeco_utils::arena::{ArenaId, KeySpaceId, derived_id};

/// Category tag separating derived identifier key spaces from sequential ones.
pub(crate) const DERIVATION_TAG: u64 = 0x5A59_4445_434F_5155;

/// Category tag for identifiers produced by salsa queries rather than by the
/// checker's in-context allocator. A separate family keeps query-produced
/// identifiers disjoint from checker-produced ones even when both derive from
/// the same allocation site.
pub(crate) const QUERY_DERIVATION_TAG: u64 = 0x5A59_5155_4552_5921;

/// Issuer of derived identifiers for query-friendly type checking.
///
/// Each checking site pushes its `(entity, occurrence)` identity onto a stack;
/// fresh identifiers derive their key space from the top site and take the
/// site-local slot as their raw index, so re-checking a site reproduces its
/// identifiers without a shared cursor. See `docs/logs/query-based-tyck.md`
/// for the derivation scheme.
#[derive(Debug)]
pub struct DerivedAllocator {
    sites: Vec<(u64, u32, u32, u32)>,
}

impl Default for DerivedAllocator {
    fn default() -> Self {
        Self::new()
    }
}

impl DerivedAllocator {
    /// Create an allocator with a root site for allocations outside any entity.
    ///
    /// The root uses a sentinel identity so that no entity-derived site can
    /// collide with it.
    pub fn new() -> Self {
        Self { sites: vec![(u64::MAX, u32::MAX, u32::MAX, 0)] }
    }

    /// Enter one entity's checking site; the site-local slot restarts at zero.
    pub fn enter(&mut self, entity_space: u64, entity_raw: u32, occurrence: u32) {
        self.sites.push((entity_space, entity_raw, occurrence, 0));
    }

    /// The innermost site: `(entity space, entity raw, occurrence)`.
    ///
    /// Producer queries derive their identifiers from this triple under the
    /// query derivation tag, so re-checked entities (fixpoint and recursion
    /// retries) get distinct identifiers exactly as the checker's own
    /// allocator does.
    pub fn current_site(&self) -> (u64, u32, u32) {
        let (entity_space, entity_raw, occurrence, _) =
            *self.sites.last().expect("the root allocation site always exists");
        (entity_space, entity_raw, occurrence)
    }

    /// Leave the innermost site, resuming the enclosing site's slots.
    pub fn exit(&mut self) {
        assert!(self.sites.len() > 1, "cannot leave the root allocation site");
        self.sites.pop();
    }

    fn fresh<Id: ArenaId>(&mut self) -> Id {
        let (entity_space, entity_raw, occurrence, slot) =
            self.sites.last_mut().expect("the root allocation site always exists");
        let key_space = KeySpaceId::derive(DERIVATION_TAG, *entity_space, *entity_raw, *occurrence);
        let id: Id = derived_id(key_space, *slot);
        *slot += 1;
        id
    }
}

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
/// The trait is different from [`Construct`] in that [`Construct::build`] implementations
/// are built on top of [`Alloc`] implementations, and thus are more convenient to use if
/// the type inference is easy, i.e. the annotations are not needed.
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

    fn alloc_type_pre(&mut self, value: Fillable<Type>) -> TypeId {
        let id = self.fresh();
        AsMut::<StaticsArena>::as_mut(self).types_pre.insert_new(id, value);
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
    Arena: AsMut<ScopedArena> + StaticsAlloc,
{
    type Ann = AnnId;
    type Env = ();
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, (): &Self::Env) -> DefId {
        let id = arena.fresh();
        AsMut::<ScopedArena>::as_mut(arena).insert_def(id, val);
        AsMut::<StaticsArena>::as_mut(arena).annotations_var.insert_new(id, ann);
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

impl<Arena> Alloc<Arena, FillId> for InferenceSite
where
    Arena: StaticsAlloc,
{
    type Ann = ();
    type Env = ();
    fn alloc(arena: &mut Arena, val: Self, (): Self::Ann, _env: &Self::Env) -> FillId {
        let id = arena.fresh();
        AsMut::<StaticsArena>::as_mut(arena).fills.insert_new(id, val);
        id
    }
}

impl<Arena> Alloc<Arena, FillId> for su::TermId
where
    Arena: StaticsAlloc,
{
    type Ann = ();
    type Env = ();
    fn alloc(arena: &mut Arena, val: Self, (): Self::Ann, env: &Self::Env) -> FillId {
        Alloc::alloc(arena, InferenceSite::from(val), (), env)
    }
}

impl<Arena> Alloc<Arena, FillId> for su::PatId
where
    Arena: StaticsAlloc,
{
    type Ann = ();
    type Env = ();
    fn alloc(arena: &mut Arena, val: Self, (): Self::Ann, env: &Self::Env) -> FillId {
        Alloc::alloc(arena, InferenceSite::from(val), (), env)
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
impl<Arena> Alloc<Arena, KindId> for Kind
where
    Arena: StaticsAlloc,
{
    type Ann = ();
    type Env = ();
    fn alloc(arena: &mut Arena, val: Self, (): Self::Ann, _env: &Self::Env) -> KindId {
        arena.alloc_kind_pre(Fillable::Done(val))
    }
}
macro_rules! AllocKind {
    ($($t:ty)*) => {
        $(
            impl<Arena> Alloc<Arena, KindId> for $t
            where
                Arena: StaticsAlloc,
            {
                type Ann = ();
                type Env = ();
                fn alloc(arena: &mut Arena, val: Self, (): Self::Ann, env: &Self::Env) -> KindId {
                    Alloc::alloc(arena, Kind::from(val), (), env)
                }
            }
        )*
    };
}
AllocKind! {
    VType
    CType
    ArrowU<KindId>
    Label<FieldName, KindId>
}

/* ------------------------------- KindPattern ------------------------------ */

impl<Arena> Alloc<Arena, KPatId> for KindPattern
where
    Arena: StaticsAlloc,
{
    type Ann = ();
    type Env = TyEnv;

    fn alloc(arena: &mut Arena, val: Self, (): Self::Ann, env: &Self::Env) -> KPatId {
        let kpat = arena.alloc_kpat(val);
        AsMut::<StaticsArena>::as_mut(arena).env_kpat.insert_new(kpat, env.clone());
        kpat
    }
}

macro_rules! AllocKindPattern {
    ($($t:ty)*) => {
        $(
            impl<Arena> Alloc<Arena, KPatId> for $t
            where
                Arena: StaticsAlloc,
            {
                type Ann = ();
                type Env = TyEnv;

                fn alloc(
                    arena: &mut Arena, val: Self, ann: Self::Ann, env: &Self::Env,
                ) -> KPatId {
                    Alloc::alloc(arena, KindPattern::from(val), ann, env)
                }
            }
        )*
    };
}

AllocKindPattern! {
    Hole
    DefId
}

/* ------------------------------- TypePattern ------------------------------ */

impl<Arena> Alloc<Arena, TPatId> for TypePattern
where
    Arena: StaticsAlloc,
{
    type Ann = KindId;
    type Env = TyEnv;
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, env: &Self::Env) -> TPatId {
        let tpat = arena.alloc_tpat(val);
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        statics.annotations_tpat.insert_new(tpat, ann);
        statics.env_tpat.insert_new(tpat, env.clone());
        tpat
    }
}
macro_rules! AllocTypePattern {
    ($($t:ty)*) => {
        $(
            impl<Arena> Alloc<Arena, TPatId> for $t
            where
                Arena: StaticsAlloc,
            {
                type Ann = KindId;
                type Env = TyEnv;
                fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, env: &Self::Env) -> TPatId {
                    Alloc::alloc(arena, TypePattern::from(val), ann, env)
                }
            }
        )*
    };
}
AllocTypePattern! {
    Hole
    DefId
    Named<FieldName, TPatId>
}

/* ---------------------------------- Type ---------------------------------- */

impl<Arena> Alloc<Arena, TypeId> for FillId
where
    Arena: StaticsAlloc,
{
    type Ann = KindId;
    type Env = TyEnv;
    fn alloc(arena: &mut Arena, val: Self, kd: Self::Ann, env: &Self::Env) -> TypeId {
        let ty = arena.alloc_type_pre(val.into());
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        statics.annotations_type.insert_new(ty, kd);
        let scope = env.skolem_scope().clone();
        let env = statics.intern_env(env);
        statics.env_type.insert_new(ty, env);
        if let Some(existing) = statics.fill_scopes.insert_or_get(val, scope.clone()) {
            statics.fill_scopes.replace_existing(val, existing.intersection(&scope));
        }
        ty
    }
}
impl<Arena> Alloc<Arena, TypeId> for Type
where
    Arena: StaticsAlloc,
{
    type Ann = KindId;
    type Env = TyEnv;
    fn alloc(arena: &mut Arena, val: Self, kd: Self::Ann, env: &Self::Env) -> TypeId {
        let ty = arena.alloc_type_pre(Fillable::Done(val));
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        statics.annotations_type.insert_new(ty, kd);
        let env = statics.intern_env(env);
        statics.env_type.insert_new(ty, env);
        ty
    }
}
macro_rules! AllocType {
    ($($t:ty)*) => {
        $(
            impl<Arena> Alloc<Arena, TypeId> for $t
            where
                Arena: StaticsAlloc,
            {
                type Ann = KindId;
                type Env = TyEnv;
                fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, env: &Self::Env) -> TypeId {
                    Alloc::alloc(arena, Type::from(val), ann, env)
                }
            }
        )*
    };
}
AllocType! {
    DefId
    AbstId
    Abs<TPatId, TypeId>
    App<TypeId, TypeId>
    Named<FieldName, TypeId>
    Label<FieldName, TypeId>
    Proj<TypeId, FieldName>
    ThkTy
    RetTy
    UnitTy
    OpaqueTy
    PrimitiveTy
    OSTy
    ValueArrow
    ValueForall
    ValuePackPi
    ArrowU<TypeId>
    Forall
    PackPi
    ProdU<TypeId>
    Exists
    ManifestKind
    DataId
    CoDataId
}

/* ------------------------------ ValuePattern ------------------------------ */

impl<Arena> Alloc<Arena, VPatId> for ValuePattern
where
    Arena: StaticsAlloc,
{
    type Ann = TypeId;
    type Env = TyEnv;
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, env: &Self::Env) -> VPatId {
        let vpat = arena.alloc_vpat(val);
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        statics.annotations_vpat.insert_new(vpat, ann);
        statics.env_vpat.insert_new(vpat, env.clone());
        vpat
    }
}
macro_rules! AllocValuePattern {
    ($($t:ty)*) => {
        $(
            impl<Arena> Alloc<Arena, VPatId> for $t
            where
                Arena: StaticsAlloc,
            {
                type Ann = TypeId;
                type Env = TyEnv;
                fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, env: &Self::Env) -> VPatId {
                    Alloc::alloc(arena, ValuePattern::from(val), ann, env)
                }
            }
        )*
    };
}
AllocValuePattern! {
    Hole
    DefId
    Named<FieldName, VPatId>
    Ctor<CtorName, VPatId>
    Alias<VPatId>
    Triv
    ConsN<VPatId, VPatId>
    ConsN<StaticPatId, VPatId>
}

impl<Arena> Alloc<Arena, VPatId> for ConsN<TPatId, VPatId>
where
    Arena: StaticsAlloc,
{
    type Ann = TypeId;
    type Env = TyEnv;

    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, env: &Self::Env) -> VPatId {
        let ConsN(items, tail) = val;
        Alloc::alloc(
            arena,
            ConsN(items.into_iter().map(StaticPatId::from).collect(), tail),
            ann,
            env,
        )
    }
}

/* ---------------------------------- Value --------------------------------- */

impl<Arena> Alloc<Arena, ValueId> for Value
where
    Arena: StaticsAlloc,
{
    type Ann = TypeId;
    type Env = TyEnv;
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, env: &Self::Env) -> ValueId {
        let value = arena.alloc_value(val);
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        statics.annotations_value.insert_new(value, ann);
        statics.env_value.insert_new(value, env.clone());
        value
    }
}
macro_rules! AllocValue {
    ($($t:ty)*) => {
        $(
            impl<Arena> Alloc<Arena, ValueId> for $t
            where
                Arena: StaticsAlloc,
            {
                type Ann = TypeId;
                type Env = TyEnv;
                fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, env: &Self::Env) -> ValueId {
                    Alloc::alloc(arena, Value::from(val), ann, env)
                }
            }
        )*
    };
}
AllocValue! {
    Hole
    DefId
    Named<FieldName, ValueId>
    Let<VPatId, ValueId, ValueId>
    Abs<VPatId, ValueId>
    App<ValueId, ValueId>
    Abs<TPatId, ValueId>
    App<ValueId, TypeId>
    Thunk<CompuId>
    Ctor<CtorName, ValueId>
    Triv
    ConsN<ValueId, ValueId>
    ConsN<StaticTermId, ValueId>
    Proj<ValueId, ResolvedField>
    Literal
}

impl<Arena> Alloc<Arena, ValueId> for ConsN<TypeId, ValueId>
where
    Arena: StaticsAlloc,
{
    type Ann = TypeId;
    type Env = TyEnv;

    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, env: &Self::Env) -> ValueId {
        let ConsN(items, tail) = val;
        Alloc::alloc(
            arena,
            ConsN(items.into_iter().map(StaticTermId::from).collect(), tail),
            ann,
            env,
        )
    }
}

/* ------------------------------- Computation ------------------------------ */

impl<Arena> Alloc<Arena, CompuId> for Computation
where
    Arena: StaticsAlloc,
{
    type Ann = TypeId;
    type Env = TyEnv;
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, env: &Self::Env) -> CompuId {
        let compu = arena.alloc_compu(val);
        let statics = AsMut::<StaticsArena>::as_mut(arena);
        statics.annotations_compu.insert_new(compu, ann);
        statics.env_compu.insert_new(compu, env.clone());
        compu
    }
}
macro_rules! AllocComputation {
    ($($t:ty)*) => {
        $(
            impl<Arena> Alloc<Arena, CompuId> for $t
            where
                Arena: StaticsAlloc,
            {
                type Ann = TypeId;
                type Env = TyEnv;
                fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann, env: &Self::Env) -> CompuId {
                    Alloc::alloc(arena, Computation::from(val), ann, env)
                }
            }
        )*
    };
}
AllocComputation! {
    Hole
    Abs<VPatId, CompuId>
    App<CompuId, ValueId>
    Abs<TPatId, CompuId>
    App<CompuId, TypeId>
    Fix<VPatId, CompuId>
    Force<ValueId>
    Return<ValueId>
    Bind<VPatId, CompuId, CompuId>
    Let<VPatId, ValueId, CompuId>
    Match<ValueId, VPatId, CompuId>
    CoMatch<DtorName, CompuId>
    Dtor<CompuId, DtorName>
}
