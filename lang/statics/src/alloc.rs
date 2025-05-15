//! Allocation of entities in [`StaticsArena`].
//!
//! This module provides the [`Alloc`] trait and all its implementations,
//! which provides a type-safe approach to allocate in a post-type-check arena.

use crate::{syntax::*, *};

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
pub trait Alloc<T> {
    /// The annotation of this allocation.
    type Ann;
    /// Allocates the value in the arena in the [`Tycker`] and returns the allocated value.
    /// See the documentation of trait [`Alloc`] and [`crate::alloc`] for more details.
    fn alloc(tycker: &mut Tycker, val: Self, ann: Self::Ann) -> T;
}

/* ------------------------------- Definition ------------------------------- */

impl Alloc<DefId> for VarName {
    type Ann = AnnId;
    fn alloc(tycker: &mut Tycker, val: Self, ann: Self::Ann) -> DefId {
        let id = tycker.scoped.defs.alloc(val);
        tycker.statics.annotations_var.insert(id, ann);
        id
    }
}

/* -------------------------------- Abstract -------------------------------- */

impl Alloc<AbstId> for DefId {
    type Ann = KindId;
    fn alloc(tycker: &mut Tycker, val: Self, ann: Self::Ann) -> AbstId {
        let abst = tycker.statics.absts.alloc(());
        tycker.statics.annotations_abst.insert(abst, ann);
        tycker.statics.abst_hints.insert(abst, val);
        abst
    }
}
impl Alloc<AbstId> for Option<DefId> {
    type Ann = KindId;
    fn alloc(tycker: &mut Tycker, val: Self, ann: Self::Ann) -> AbstId {
        let abst = tycker.statics.absts.alloc(());
        tycker.statics.annotations_abst.insert(abst, ann);
        if let Some(def) = val {
            tycker.statics.abst_hints.insert(abst, def);
        }
        abst
    }
}
impl Alloc<AbstId> for TPatId {
    type Ann = ();
    fn alloc(tycker: &mut Tycker, val: Self, (): Self::Ann) -> AbstId {
        let (def, kd) = val.try_destruct_def(tycker);
        Alloc::alloc(tycker, def, kd)
    }
}

/* ---------------------------------- Fill ---------------------------------- */

impl Alloc<FillId> for su::TermId {
    type Ann = ();
    fn alloc(tycker: &mut Tycker, val: Self, (): Self::Ann) -> FillId {
        tycker.statics.fills.alloc(val)
    }
}

/* ---------------------------------- Kind ---------------------------------- */

impl Alloc<KindId> for FillId {
    type Ann = ();
    fn alloc(tycker: &mut Tycker, val: Self, (): Self::Ann) -> KindId {
        tycker.statics.kinds.alloc(val.into())
    }
}
impl Alloc<KindId> for Kind {
    type Ann = ();
    fn alloc(tycker: &mut Tycker, val: Self, (): Self::Ann) -> KindId {
        tycker.statics.kinds.alloc(Fillable::Done(val))
    }
}
macro_rules! AllocKind {
    ($($t:ty)*) => {
        $(
            impl Alloc<KindId> for $t {
                type Ann = ();
                fn alloc(tycker: &mut Tycker, val: Self, (): Self::Ann) -> KindId {
                    Alloc::alloc(tycker, Kind::from(val), ())
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

impl Alloc<TPatId> for TypePattern {
    type Ann = KindId;
    fn alloc(tycker: &mut Tycker, val: Self, ann: Self::Ann) -> TPatId {
        let tpat = tycker.statics.tpats.alloc(val);
        tycker.statics.annotations_tpat.insert(tpat, ann);
        tpat
    }
}
macro_rules! AllocTypePattern {
    ($($t:ty)*) => {
        $(
            impl Alloc<TPatId> for $t {
                type Ann = KindId;
                fn alloc(tycker: &mut Tycker, val: Self, ann: Self::Ann) -> TPatId {
                    Alloc::alloc(tycker, TypePattern::from(val), ann)
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

impl Alloc<TypeId> for FillId {
    type Ann = KindId;
    fn alloc(tycker: &mut Tycker, val: Self, kd: Self::Ann) -> TypeId {
        let ty = tycker.statics.types.alloc(val.into());
        tycker
            .statics
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
impl Alloc<TypeId> for Type {
    type Ann = KindId;
    fn alloc(tycker: &mut Tycker, val: Self, kd: Self::Ann) -> TypeId {
        let ty = tycker.statics.types.alloc(Fillable::Done(val));
        tycker
            .statics
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
            impl Alloc<TypeId> for $t {
                type Ann = KindId;
                fn alloc(tycker: &mut Tycker, val: Self, ann: Self::Ann) -> TypeId {
                    Alloc::alloc(tycker, Type::from(val), ann)
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

impl Alloc<VPatId> for ValuePattern {
    type Ann = TypeId;
    fn alloc(tycker: &mut Tycker, val: Self, ann: Self::Ann) -> VPatId {
        let vpat = tycker.statics.vpats.alloc(val);
        tycker.statics.annotations_vpat.insert(vpat, ann);
        vpat
    }
}
macro_rules! AllocValuePattern {
    ($($t:ty)*) => {
        $(
            impl Alloc<VPatId> for $t {
                type Ann = TypeId;
                fn alloc(tycker: &mut Tycker, val: Self, ann: Self::Ann) -> VPatId {
                    Alloc::alloc(tycker, ValuePattern::from(val), ann)
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

impl Alloc<ValueId> for Value {
    type Ann = TypeId;
    fn alloc(tycker: &mut Tycker, val: Self, ann: Self::Ann) -> ValueId {
        let value = tycker.statics.values.alloc(val);
        tycker.statics.annotations_value.insert(value, ann);
        value
    }
}
macro_rules! AllocValue {
    ($($t:ty)*) => {
        $(
            impl Alloc<ValueId> for $t {
                type Ann = TypeId;
                fn alloc(tycker: &mut Tycker, val: Self, ann: Self::Ann) -> ValueId {
                    Alloc::alloc(tycker, Value::from(val), ann)
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

impl Alloc<CompuId> for Computation {
    type Ann = TypeId;
    fn alloc(tycker: &mut Tycker, val: Self, ann: Self::Ann) -> CompuId {
        let compu = tycker.statics.compus.alloc(val);
        tycker.statics.annotations_compu.insert(compu, ann);
        compu
    }
}
macro_rules! AllocComputation {
    ($($t:ty)*) => {
        $(
            impl Alloc<CompuId> for $t {
                type Ann = TypeId;
                fn alloc(tycker: &mut Tycker, val: Self, ann: Self::Ann) -> CompuId {
                    Alloc::alloc(tycker, Computation::from(val), ann)
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
    Ret<ValueId>
    Bind<VPatId, CompuId, CompuId>
    PureBind<VPatId, ValueId, CompuId>
    Match<ValueId, VPatId, CompuId>
    CoMatch<CompuId>
    Dtor<CompuId>
}
