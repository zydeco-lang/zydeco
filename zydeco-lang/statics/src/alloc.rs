use crate::syntax::*;

pub trait Alloc<T> {
    type Ann;
    fn alloc(statics: &mut StaticsArena, val: Self, ann: Self::Ann) -> T;
}

/* ---------------------------------- Kind ---------------------------------- */

impl Alloc<KindId> for Kind {
    type Ann = ();
    fn alloc(statics: &mut StaticsArena, val: Self, (): Self::Ann) -> KindId {
        statics.kinds.alloc(val)
    }
}
macro_rules! AllocKind {
    ($($t:ty)*) => {
        $(
            impl Alloc<KindId> for $t {
                type Ann = ();
                fn alloc(statics: &mut StaticsArena, val: Self, (): Self::Ann) -> KindId {
                    Alloc::alloc(statics, Kind::from(val), ())
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
    fn alloc(statics: &mut StaticsArena, val: Self, _ann: Self::Ann) -> TPatId {
        let tpat = statics.tpats.alloc(val);
        tpat
    }
}
macro_rules! AllocTypePattern {
    ($($t:ty)*) => {
        $(
            impl Alloc<TPatId> for $t {
                type Ann = KindId;
                fn alloc(statics: &mut StaticsArena, val: Self, ann: Self::Ann) -> TPatId {
                    Alloc::alloc(statics, TypePattern::from(val), ann)
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
    fn alloc(statics: &mut StaticsArena, val: Self, kd: Self::Ann) -> TypeId {
        let ty = statics.types.alloc(val.into());
        statics
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
                fn alloc(statics: &mut StaticsArena, val: Self, ann: Self::Ann) -> TypeId {
                    Alloc::alloc(statics, Type::from(val), ann)
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
    fn alloc(statics: &mut StaticsArena, val: Self, _ann: Self::Ann) -> VPatId {
        let vpat = statics.vpats.alloc(val);
        vpat
    }
}
macro_rules! AllocValuePattern {
    ($($t:ty)*) => {
        $(
            impl Alloc<VPatId> for $t {
                type Ann = TypeId;
                fn alloc(statics: &mut StaticsArena, val: Self, ann: Self::Ann) -> VPatId {
                    Alloc::alloc(statics, ValuePattern::from(val), ann)
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
    fn alloc(statics: &mut StaticsArena, val: Self, ann: Self::Ann) -> ValueId {
        let value = statics.values.alloc(val);
        statics.annotations_value.insert(value, ann);
        value
    }
}
macro_rules! AllocValue {
    ($($t:ty)*) => {
        $(
            impl Alloc<ValueId> for $t {
                type Ann = TypeId;
                fn alloc(statics: &mut StaticsArena, val: Self, ann: Self::Ann) -> ValueId {
                    Alloc::alloc(statics, Value::from(val), ann)
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
    fn alloc(statics: &mut StaticsArena, val: Self, ann: Self::Ann) -> CompuId {
        let compu = statics.compus.alloc(val);
        statics.annotations_compu.insert(compu, ann);
        compu
    }
}
macro_rules! AllocComputation {
    ($($t:ty)*) => {
        $(
            impl Alloc<CompuId> for $t {
                type Ann = TypeId;
                fn alloc(statics: &mut StaticsArena, val: Self, ann: Self::Ann) -> CompuId {
                    Alloc::alloc(statics, Computation::from(val), ann)
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
    Match<ValueId, VPatId, CompuId>
    CoMatch<CompuId>
    Dtor<CompuId>
    WithBlock
}
