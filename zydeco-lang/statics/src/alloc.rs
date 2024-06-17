use crate::{syntax::*, *};

pub trait Alloc<T> {
    fn alloc(tycker: &mut Tycker, val: Self) -> T;
}

/* ---------------------------------- Kind ---------------------------------- */

impl Alloc<KindId> for Kind {
    fn alloc(tycker: &mut Tycker, val: Self) -> KindId {
        let kind = tycker.statics.kinds.alloc(val);
        kind
    }
}
impl Alloc<KindId> for VType {
    fn alloc(tycker: &mut Tycker, val: Self) -> KindId {
        Alloc::alloc(tycker, Kind::from(val))
    }
}
impl Alloc<KindId> for CType {
    fn alloc(tycker: &mut Tycker, val: Self) -> KindId {
        Alloc::alloc(tycker, Kind::from(val))
    }
}
impl Alloc<KindId> for Arrow<KindId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> KindId {
        Alloc::alloc(tycker, Kind::from(val))
    }
}

/* ---------------------------------- Type ---------------------------------- */

impl Alloc<TPatId> for TypePattern {
    fn alloc(tycker: &mut Tycker, val: Self) -> TPatId {
        let tpat = tycker.statics.tpats.alloc(val);
        tpat
    }
}
// impl Alloc<TPatId> for Ann<TPatId, KindId> {
//     fn alloc(tycker: &mut Tycker, val: Self) -> TPatId {
//         Alloc::alloc(tycker, TypePattern::from(val))
//     }
// }
impl Alloc<TPatId> for Hole {
    fn alloc(tycker: &mut Tycker, val: Self) -> TPatId {
        Alloc::alloc(tycker, TypePattern::from(val))
    }
}
impl Alloc<TPatId> for DefId {
    fn alloc(tycker: &mut Tycker, val: Self) -> TPatId {
        Alloc::alloc(tycker, TypePattern::from(val))
    }
}

impl Alloc<TypeId> for Type {
    fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
        let ty = tycker.statics.types.alloc(val.into());
        ty
    }
}
// impl Alloc<TypeId> for Sealed<TypeId> {
//     fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
//         Alloc::alloc(tycker, Type::from(val))
//     }
// }
// impl Alloc<TypeId> for Ann<TypeId, KindId> {
//     fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
//         Alloc::alloc(tycker, Type::from(val))
//     }
// }
// impl Alloc<TypeId> for Hole {
//     fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
//         Alloc::alloc(tycker, Type::from(val))
//     }
// }
impl Alloc<TypeId> for DefId {
    fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
        Alloc::alloc(tycker, Type::from(val))
    }
}
impl Alloc<TypeId> for AbstId {
    fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
        Alloc::alloc(tycker, Type::from(val))
    }
}
impl Alloc<TypeId> for FillId {
    fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
        Alloc::alloc(tycker, Type::from(val))
    }
}
impl Alloc<TypeId> for Abs<TPatId, TypeId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
        Alloc::alloc(tycker, Type::from(val))
    }
}
impl Alloc<TypeId> for App<TypeId, TypeId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
        Alloc::alloc(tycker, Type::from(val))
    }
}
impl Alloc<TypeId> for ThunkTy {
    fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
        Alloc::alloc(tycker, Type::from(val))
    }
}
impl Alloc<TypeId> for RetTy {
    fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
        Alloc::alloc(tycker, Type::from(val))
    }
}
impl Alloc<TypeId> for UnitTy {
    fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
        Alloc::alloc(tycker, Type::from(val))
    }
}
impl Alloc<TypeId> for IntTy {
    fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
        Alloc::alloc(tycker, Type::from(val))
    }
}
impl Alloc<TypeId> for CharTy {
    fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
        Alloc::alloc(tycker, Type::from(val))
    }
}
impl Alloc<TypeId> for StringTy {
    fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
        Alloc::alloc(tycker, Type::from(val))
    }
}
impl Alloc<TypeId> for OSTy {
    fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
        Alloc::alloc(tycker, Type::from(val))
    }
}
impl Alloc<TypeId> for Arrow<TypeId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
        Alloc::alloc(tycker, Type::from(val))
    }
}
impl Alloc<TypeId> for Forall {
    fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
        Alloc::alloc(tycker, Type::from(val))
    }
}
impl Alloc<TypeId> for Prod<TypeId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
        Alloc::alloc(tycker, Type::from(val))
    }
}
impl Alloc<TypeId> for Exists {
    fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
        Alloc::alloc(tycker, Type::from(val))
    }
}
impl Alloc<TypeId> for DataId {
    fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
        Alloc::alloc(tycker, Type::from(val))
    }
}
impl Alloc<TypeId> for CoDataId {
    fn alloc(tycker: &mut Tycker, val: Self) -> TypeId {
        Alloc::alloc(tycker, Type::from(val))
    }
}

/* ---------------------------------- Value --------------------------------- */

impl Alloc<VPatId> for ValuePattern {
    fn alloc(tycker: &mut Tycker, val: Self) -> VPatId {
        let vpat = tycker.statics.vpats.alloc(val);
        vpat
    }
}
impl Alloc<VPatId> for Hole {
    fn alloc(tycker: &mut Tycker, val: Self) -> VPatId {
        Alloc::alloc(tycker, ValuePattern::from(val))
    }
}
impl Alloc<VPatId> for DefId {
    fn alloc(tycker: &mut Tycker, val: Self) -> VPatId {
        Alloc::alloc(tycker, ValuePattern::from(val))
    }
}
impl Alloc<VPatId> for Ctor<VPatId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> VPatId {
        Alloc::alloc(tycker, ValuePattern::from(val))
    }
}
impl Alloc<VPatId> for Triv {
    fn alloc(tycker: &mut Tycker, val: Self) -> VPatId {
        Alloc::alloc(tycker, ValuePattern::from(val))
    }
}
impl Alloc<VPatId> for Cons<VPatId, VPatId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> VPatId {
        Alloc::alloc(tycker, ValuePattern::from(val))
    }
}
impl Alloc<VPatId> for Cons<TPatId, VPatId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> VPatId {
        Alloc::alloc(tycker, ValuePattern::from(val))
    }
}

impl Alloc<ValueId> for Value {
    fn alloc(tycker: &mut Tycker, val: Self) -> ValueId {
        let val = tycker.statics.values.alloc(val);
        val
    }
}
impl Alloc<ValueId> for Hole {
    fn alloc(tycker: &mut Tycker, val: Self) -> ValueId {
        Alloc::alloc(tycker, Value::from(val))
    }
}
impl Alloc<ValueId> for DefId {
    fn alloc(tycker: &mut Tycker, val: Self) -> ValueId {
        Alloc::alloc(tycker, Value::from(val))
    }
}
impl Alloc<ValueId> for Thunk<CompuId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> ValueId {
        Alloc::alloc(tycker, Value::from(val))
    }
}
impl Alloc<ValueId> for Ctor<ValueId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> ValueId {
        Alloc::alloc(tycker, Value::from(val))
    }
}
impl Alloc<ValueId> for Triv {
    fn alloc(tycker: &mut Tycker, val: Self) -> ValueId {
        Alloc::alloc(tycker, Value::from(val))
    }
}
impl Alloc<ValueId> for Cons<ValueId, ValueId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> ValueId {
        Alloc::alloc(tycker, Value::from(val))
    }
}
impl Alloc<ValueId> for Cons<TypeId, ValueId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> ValueId {
        Alloc::alloc(tycker, Value::from(val))
    }
}
impl Alloc<ValueId> for Literal {
    fn alloc(tycker: &mut Tycker, val: Self) -> ValueId {
        Alloc::alloc(tycker, Value::from(val))
    }
}

impl Alloc<CompuId> for Computation {
    fn alloc(tycker: &mut Tycker, val: Self) -> CompuId {
        let compu = tycker.statics.compus.alloc(val);
        compu
    }
}
impl Alloc<CompuId> for Hole {
    fn alloc(tycker: &mut Tycker, val: Self) -> CompuId {
        Alloc::alloc(tycker, Computation::from(val))
    }
}
impl Alloc<CompuId> for Abs<VPatId, CompuId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> CompuId {
        Alloc::alloc(tycker, Computation::from(val))
    }
}
impl Alloc<CompuId> for App<CompuId, ValueId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> CompuId {
        Alloc::alloc(tycker, Computation::from(val))
    }
}
impl Alloc<CompuId> for Rec<VPatId, CompuId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> CompuId {
        Alloc::alloc(tycker, Computation::from(val))
    }
}
impl Alloc<CompuId> for Force<ValueId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> CompuId {
        Alloc::alloc(tycker, Computation::from(val))
    }
}
impl Alloc<CompuId> for Ret<ValueId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> CompuId {
        Alloc::alloc(tycker, Computation::from(val))
    }
}
impl Alloc<CompuId> for Bind<VPatId, CompuId, CompuId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> CompuId {
        Alloc::alloc(tycker, Computation::from(val))
    }
}
impl Alloc<CompuId> for PureBind<VPatId, ValueId, CompuId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> CompuId {
        Alloc::alloc(tycker, Computation::from(val))
    }
}
impl Alloc<CompuId> for Match {
    fn alloc(tycker: &mut Tycker, val: Self) -> CompuId {
        Alloc::alloc(tycker, Computation::from(val))
    }
}
impl Alloc<CompuId> for CoMatch {
    fn alloc(tycker: &mut Tycker, val: Self) -> CompuId {
        Alloc::alloc(tycker, Computation::from(val))
    }
}
impl Alloc<CompuId> for Dtor<CompuId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> CompuId {
        Alloc::alloc(tycker, Computation::from(val))
    }
}
