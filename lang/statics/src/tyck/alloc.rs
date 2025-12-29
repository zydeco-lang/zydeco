//! Allocation of entities in [`StaticsArena`].
//!
//! This module provides the [`Alloc`] trait and all its implementations,
//! which provides a type-safe approach to allocate in a post-type-check arena.

use super::syntax::*;
use crate::*;

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
    /// Allocates the value in the arena in the [`Tycker`] and returns the allocated value.
    /// See the documentation of trait [`Alloc`] and [`crate::tyck::alloc`] for more details.
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann) -> T; }

/* ------------------------------- Definition ------------------------------- */

impl<Arena> Alloc<Arena, DefId> for VarName
where
    Arena: AsMut<ScopedArena> + AsMut<StaticsArena>,
{
    type Ann = AnnId;
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann) -> DefId {
        let id = AsMut::<ScopedArena>::as_mut(arena).defs.alloc(val);
        AsMut::<StaticsArena>::as_mut(arena).annotations_var.insert(id, ann);
        id
    }
}

/* -------------------------------- Abstract -------------------------------- */

impl<Arena> Alloc<Arena, AbstId> for DefId
where
    Arena: AsMut<StaticsArena>,
{
    type Ann = KindId;
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann) -> AbstId {
        let abst = arena.as_mut().absts.alloc(());
        arena.as_mut().annotations_abst.insert(abst, ann);
        arena.as_mut().abst_hints.insert(abst, val);
        abst
    }
}
impl<Arena> Alloc<Arena, AbstId> for Option<DefId>
where
    Arena: AsMut<StaticsArena>,
{
    type Ann = KindId;
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann) -> AbstId {
        let abst = arena.as_mut().absts.alloc(());
        arena.as_mut().annotations_abst.insert(abst, ann);
        if let Some(def) = val {
            arena.as_mut().abst_hints.insert(abst, def);
        }
        abst
    }
}
impl<Arena> Alloc<Arena, AbstId> for TPatId
where
    Arena: AsMut<StaticsArena> + AsRef<StaticsArena>,
{
    type Ann = ();
    fn alloc(arena: &mut Arena, val: Self, (): Self::Ann) -> AbstId {
        let (def, kd) = val.try_destruct_def(arena);
        Alloc::alloc(arena, def, kd)
    }
}

/* ---------------------------------- Fill ---------------------------------- */

impl<Arena> Alloc<Arena, FillId> for su::TermId
where
    Arena: AsMut<StaticsArena>,
{
    type Ann = ();
    fn alloc(arena: &mut Arena, val: Self, (): Self::Ann) -> FillId {
        arena.as_mut().fills.alloc(val)
    }
}

/* ---------------------------------- Kind ---------------------------------- */

impl<Arena> Alloc<Arena, KindId> for FillId
where
    Arena: AsMut<StaticsArena>,
{
    type Ann = ();
    fn alloc(arena: &mut Arena, val: Self, (): Self::Ann) -> KindId {
        arena.as_mut().kinds.alloc(val.into())
    }
}
impl<Arena> Alloc<Arena, KindId> for Kind
where
    Arena: AsMut<StaticsArena>,
{
    type Ann = ();
    fn alloc(arena: &mut Arena, val: Self, (): Self::Ann) -> KindId {
        arena.as_mut().kinds.alloc(Fillable::Done(val))
    }
}
macro_rules! AllocKind {
    ($($t:ty)*) => {
        $(
            impl<Arena> Alloc<Arena, KindId> for $t
            where
                Arena: AsMut<StaticsArena>,
            {
                type Ann = ();
                fn alloc(arena: &mut Arena, val: Self, (): Self::Ann) -> KindId {
                    Alloc::alloc(arena, Kind::from(val), ())
                }
            }
        )*
    };
}
AllocKind! {
    VType
    CType
    ArrowU<KindId>
}

/* ------------------------------- TypePattern ------------------------------ */

impl<Arena> Alloc<Arena, TPatId> for TypePattern
where
    Arena: AsMut<StaticsArena>,
{
    type Ann = KindId;
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann) -> TPatId {
        let tpat = arena.as_mut().tpats.alloc(val);
        arena.as_mut().annotations_tpat.insert(tpat, ann);
        tpat
    }
}
macro_rules! AllocTypePattern {
    ($($t:ty)*) => {
        $(
            impl<Arena> Alloc<Arena, TPatId> for $t
            where
                Arena: AsMut<StaticsArena>,
            {
                type Ann = KindId;
                fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann) -> TPatId {
                    Alloc::alloc(arena, TypePattern::from(val), ann)
                }
            }
        )*
    };
}
AllocTypePattern! {
    Hole
    DefId
}

/* ---------------------------------- Type ---------------------------------- */

impl<Arena> Alloc<Arena, TypeId> for FillId
where
    Arena: AsMut<StaticsArena>,
{
    type Ann = KindId;
    fn alloc(arena: &mut Arena, val: Self, kd: Self::Ann) -> TypeId {
        let ty = arena.as_mut().types.alloc(val.into());
        arena
            .as_mut()
            .annotations_type
            .insert_or_else(ty, kd, |_old, _new| -> std::result::Result<KindId, ()> {
                panic!("duplicate keys: {:?} = {:?}, {:?}", ty, _old, _new)
                // // Todo: handle duplicate keys
                // let res: std::result::Result<KindId, ()> = Ok(_new);
                // res
            })
            .unwrap();
        ty
    }
}
impl<Arena> Alloc<Arena, TypeId> for Type
where
    Arena: AsMut<StaticsArena>,
{
    type Ann = KindId;
    fn alloc(arena: &mut Arena, val: Self, kd: Self::Ann) -> TypeId {
        let ty = arena.as_mut().types.alloc(Fillable::Done(val));
        arena
            .as_mut()
            .annotations_type
            .insert_or_else(ty, kd, |_old, _new| -> std::result::Result<KindId, ()> {
                panic!("duplicate keys: {:?} = {:?}, {:?}", ty, _old, _new)
                // // Todo: handle duplicate keys
                // let res: std::result::Result<KindId, ()> = Ok(_new);
                // res
            })
            .unwrap();
        ty
    }
}
macro_rules! AllocType {
    ($($t:ty)*) => {
        $(
            impl<Arena> Alloc<Arena, TypeId> for $t
            where
                Arena: AsMut<StaticsArena>,
            {
                type Ann = KindId;
                fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann) -> TypeId {
                    Alloc::alloc(arena, Type::from(val), ann)
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
    ThkTy
    RetTy
    UnitTy
    IntTy
    CharTy
    StringTy
    OSTy
    ArrowU<TypeId>
    Forall
    ProdU<TypeId>
    Exists
    DataId
    CoDataId
}

/* ------------------------------ ValuePattern ------------------------------ */

impl<Arena> Alloc<Arena, VPatId> for ValuePattern
where
    Arena: AsMut<StaticsArena>,
{
    type Ann = TypeId;
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann) -> VPatId {
        let vpat = arena.as_mut().vpats.alloc(val);
        arena.as_mut().annotations_vpat.insert(vpat, ann);
        vpat
    }
}
macro_rules! AllocValuePattern {
    ($($t:ty)*) => {
        $(
            impl<Arena> Alloc<Arena, VPatId> for $t
            where
                Arena: AsMut<StaticsArena>,
            {
                type Ann = TypeId;
                fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann) -> VPatId {
                    Alloc::alloc(arena, ValuePattern::from(val), ann)
                }
            }
        )*
    };
}
AllocValuePattern! {
    Hole
    DefId
    Ctor<VPatId>
    Triv
    Cons<VPatId, VPatId>
    Cons<TPatId, VPatId>
}

/* ---------------------------------- Value --------------------------------- */

impl<Arena> Alloc<Arena, ValueId> for Value
where
    Arena: AsMut<StaticsArena>,
{
    type Ann = TypeId;
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann) -> ValueId {
        let value = arena.as_mut().values.alloc(val);
        arena.as_mut().annotations_value.insert(value, ann);
        value
    }
}
macro_rules! AllocValue {
    ($($t:ty)*) => {
        $(
            impl<Arena> Alloc<Arena, ValueId> for $t
            where
                Arena: AsMut<StaticsArena>,
            {
                type Ann = TypeId;
                fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann) -> ValueId {
                    Alloc::alloc(arena, Value::from(val), ann)
                }
            }
        )*
    };
}
AllocValue! {
    Hole
    DefId
    Thunk<CompuId>
    Ctor<ValueId>
    Triv
    Cons<ValueId, ValueId>
    Cons<TypeId, ValueId>
    Literal
}

/* ------------------------------- Computation ------------------------------ */

impl<Arena> Alloc<Arena, CompuId> for Computation
where
    Arena: AsMut<StaticsArena>,
{
    type Ann = TypeId;
    fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann) -> CompuId {
        let compu = arena.as_mut().compus.alloc(val);
        arena.as_mut().annotations_compu.insert(compu, ann);
        compu
    }
}
macro_rules! AllocComputation {
    ($($t:ty)*) => {
        $(
            impl<Arena> Alloc<Arena, CompuId> for $t
            where
                Arena: AsMut<StaticsArena>,
            {
                type Ann = TypeId;
                fn alloc(arena: &mut Arena, val: Self, ann: Self::Ann) -> CompuId {
                    Alloc::alloc(arena, Computation::from(val), ann)
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
    CoMatch<CompuId>
    Dtor<CompuId>
}
