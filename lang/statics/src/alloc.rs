//! Allocation of entities in [`StaticsArena`].
//!
//! This module provides the [`Alloc`] trait and all its implementations,
//! which provides a type-safe approach to allocate in a post-type-check arena.

use crate::*;
use zydeco_utils::arena::{ArenaId, DerivationDomain, KeySpaceId, derived_id};

/// Allocation families owned by static checking and its query producers.
///
/// The [compiler identity account](../../../docs/references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance)
/// describes the derivation scheme and its collision limits.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum DerivationFamily {
    /// Intermediate nodes allocated by the mutable type checker.
    Checker,
    /// Producer nodes, including IDs issued without retaining a Salsa query.
    Query,
}

impl DerivationDomain for DerivationFamily {
    fn tag(self) -> u64 {
        match self {
            | Self::Checker => u64::from_be_bytes(*b"ZYDECOQU"),
            | Self::Query => u64::from_be_bytes(*b"ZYQUERY!"),
        }
    }
}

/// Issuer of derived identifiers for query-friendly type checking.
///
/// Each checking site pushes its `(entity, occurrence)` identity onto a stack;
/// fresh identifiers derive their key space from the top site and take the
/// site-local slot as their raw index, so re-checking a site reproduces its
/// identifiers without a shared cursor. See the
/// [compiler identity account](../../../docs/references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance)
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
    /// The root uses a sentinel identity distinct from entity-site inputs.
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
    /// query derivation family, taking re-checking occurrences (fixpoint and
    /// recursion retries) into account just as the checker's allocator does.
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
        let key_space =
            KeySpaceId::derive(DerivationFamily::Checker, *entity_space, *entity_raw, *occurrence);
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
impl<Arena> Alloc<Arena, TypeId> for Type
where
    Arena: StaticsAlloc,
{
    type Ann = KindId;
    type Env = TyEnv;
    fn alloc(arena: &mut Arena, val: Self, kd: Self::Ann, env: &Self::Env) -> TypeId {
        let ty = arena.alloc_type_pre(Fillable::Done(val), kd);
        let statics = AsMut::<StaticsArena>::as_mut(arena);
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
    TypeAbstraction
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
    ValPi
    ArrowU<TypeId>
    Forall
    PackPi
    Prod<TypeId>
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
    Vec<VPatId>
    ConsN<StaticPatId, VPatId>
    Box<ViewPattern>
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
    Abs<ValBinder, ValueId>
    App<ValueId, ValArgument>
    Thunk<CompuId>
    Ctor<CtorName, ValueId>
    Triv
    Vec<ValueId>
    ConsN<StaticTermId, ValueId>
    Proj<ValueId, ResolvedField>
    Literal
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn derivation_families_preserve_existing_key_spaces() {
        let sites = [(0, 0, 0), (7, 42, 0), (u64::MAX, u32::MAX, u32::MAX)];
        // Captured from the numeric-tag implementation before introducing families.
        for (family, expected) in [
            (
                DerivationFamily::Checker,
                [0xB193_A592_87A0_44C2, 0x7FDD_5A6F_0352_3980, 0x476A_298F_AE64_1563],
            ),
            (
                DerivationFamily::Query,
                [0xA737_660A_18FC_1300, 0x6232_716A_F6F9_3729, 0x873D_FA8A_5F50_5129],
            ),
        ] {
            let actual = sites.map(|(space, raw, occurrence)| {
                KeySpaceId::derive(family, space, raw, occurrence).as_u64()
            });
            assert_eq!(actual, expected, "the {family:?} encoding preserves issued key spaces");
        }
    }

    #[test]
    fn nested_sites_resume_checker_slots_and_replay_identifiers() {
        let mut allocator = DerivedAllocator::new();
        allocator.enter(7, 42, 0);
        let first: TypeId = allocator.fresh();
        assert_eq!(first, derived_id(KeySpaceId::derive(DerivationFamily::Checker, 7, 42, 0), 0));
        let producer: TypeId = derived_id(KeySpaceId::derive(DerivationFamily::Query, 7, 42, 0), 0);
        assert_ne!(first, producer);

        allocator.enter(7, 43, 0);
        let nested: TypeId = allocator.fresh();
        assert_ne!(nested, first);
        allocator.exit();
        assert_eq!(allocator.current_site(), (7, 42, 0));
        let next: TypeId = allocator.fresh();
        assert_eq!(next, derived_id(first.key_space(), 1));
        allocator.exit();

        allocator.enter(7, 42, 0);
        assert_eq!(allocator.fresh::<TypeId>(), first);
        assert_eq!(allocator.fresh::<TypeId>(), next);
        allocator.exit();
        allocator.enter(7, 42, 1);
        assert_ne!(allocator.fresh::<TypeId>(), first);
    }

    #[test]
    fn leaving_the_root_is_rejected_without_losing_allocation_state() {
        let mut allocator = DerivedAllocator::new();
        let first: TypeId = allocator.fresh();
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| allocator.exit()));
        let message = result.unwrap_err();
        assert_eq!(message.downcast_ref::<&str>(), Some(&"cannot leave the root allocation site"));
        assert_eq!(allocator.current_site(), (u64::MAX, u32::MAX, u32::MAX));
        assert_eq!(allocator.fresh::<TypeId>(), derived_id(first.key_space(), 1));
    }
}
