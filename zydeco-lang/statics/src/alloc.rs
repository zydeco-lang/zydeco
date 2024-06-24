use crate::{syntax::*, *};

pub trait Alloc<T> {
    type Ann;
    fn alloc(tycker: &mut Tycker, val: Self, ann: Self::Ann) -> T;
}

/* ---------------------------------- Kind ---------------------------------- */

impl Alloc<KindId> for Kind {
    type Ann = ();
    fn alloc(tycker: &mut Tycker, val: Self, (): Self::Ann) -> KindId {
        tycker.statics.kinds.alloc(val)
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
    Arrow<KindId>
}

/* ---------------------------------- Type ---------------------------------- */

impl Alloc<TPatId> for TypePattern {
    type Ann = KindId;
    fn alloc(tycker: &mut Tycker, val: Self, _ann: Self::Ann) -> TPatId {
        let tpat = tycker.statics.tpats.alloc(val);
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
    Ann<Hole, KindId>
    DefId
}

impl Alloc<TypeId> for Type {
    type Ann = KindId;
    fn alloc(tycker: &mut Tycker, val: Self, kd: Self::Ann) -> TypeId {
        let ty = tycker.statics.types.alloc(val.into());
        tycker
            .statics
            .annotations_type
            .insert_or_else(ty, kd, |_old, _new| {
                // println!("duplicate keys: {:?} = {:?}, {:?}", ty, _old, _new);
                // Todo: handle duplicate keys
                let res: std::result::Result<KindId, ()> = Ok(_new);
                res
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
    FillId
    Abs<TPatId, TypeId>
    App<TypeId, TypeId>
    ThunkTy
    RetTy
    UnitTy
    IntTy
    CharTy
    StringTy
    OSTy
    Arrow<TypeId>
    Forall
    Prod<TypeId>
    Exists
    DataId
    CoDataId
}

/* ---------------------------------- Value --------------------------------- */

impl Alloc<VPatId> for ValuePattern {
    type Ann = TypeId;
    fn alloc(tycker: &mut Tycker, val: Self, _ann: Self::Ann) -> VPatId {
        let vpat = tycker.statics.vpats.alloc(val);
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
    Ann<Hole, TypeId>
    DefId
    Ctor<VPatId>
    Triv
    Cons<VPatId, VPatId>
    Cons<TPatId, VPatId>
}

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
    Rec<VPatId, CompuId>
    Force<ValueId>
    Ret<ValueId>
    Bind<VPatId, CompuId, CompuId>
    PureBind<VPatId, ValueId, CompuId>
    Match
    CoMatch
    Dtor<CompuId>
}
