use crate::*;

pub trait Alloc<T> {
    fn alloc(tycker: &mut Tycker, val: Self) -> T;
}

impl Alloc<ss::KindId> for ss::Kind {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::KindId {
        let kind = tycker.statics.kinds.alloc(val);
        kind
    }
}
impl Alloc<ss::KindId> for ss::VType {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::KindId {
        Alloc::alloc(tycker, ss::Kind::from(val))
    }
}
impl Alloc<ss::KindId> for ss::CType {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::KindId {
        Alloc::alloc(tycker, ss::Kind::from(val))
    }
}
impl Alloc<ss::KindId> for ss::Arrow<ss::KindId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::KindId {
        Alloc::alloc(tycker, ss::Kind::from(val))
    }
}

impl Alloc<ss::TypeId> for ss::Type {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
        let ty = tycker.statics.types.alloc(val.into());
        ty
    }
}
impl Alloc<ss::TypeId> for ss::Sealed<ss::TypeId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
        Alloc::alloc(tycker, ss::Type::from(val))
    }
}
impl Alloc<ss::TypeId> for ss::Hole {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
        Alloc::alloc(tycker, ss::Type::from(val))
    }
}
impl Alloc<ss::TypeId> for ss::Abstract {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
        Alloc::alloc(tycker, ss::Type::from(val))
    }
}
impl Alloc<ss::TypeId> for ss::DefId {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
        Alloc::alloc(tycker, ss::Type::from(val))
    }
}
impl Alloc<ss::TypeId> for ss::Abs<ss::TPatId, ss::TypeId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
        Alloc::alloc(tycker, ss::Type::from(val))
    }
}
impl Alloc<ss::TypeId> for ss::App<ss::TypeId, ss::TypeId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
        Alloc::alloc(tycker, ss::Type::from(val))
    }
}
impl Alloc<ss::TypeId> for ss::ThunkTy {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
        Alloc::alloc(tycker, ss::Type::from(val))
    }
}
impl Alloc<ss::TypeId> for ss::RetTy {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
        Alloc::alloc(tycker, ss::Type::from(val))
    }
}
impl Alloc<ss::TypeId> for ss::UnitTy {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
        Alloc::alloc(tycker, ss::Type::from(val))
    }
}
impl Alloc<ss::TypeId> for ss::IntTy {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
        Alloc::alloc(tycker, ss::Type::from(val))
    }
}
impl Alloc<ss::TypeId> for ss::CharTy {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
        Alloc::alloc(tycker, ss::Type::from(val))
    }
}
impl Alloc<ss::TypeId> for ss::StringTy {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
        Alloc::alloc(tycker, ss::Type::from(val))
    }
}
impl Alloc<ss::TypeId> for ss::OSTy {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
        Alloc::alloc(tycker, ss::Type::from(val))
    }
}
impl Alloc<ss::TypeId> for ss::Arrow<ss::TypeId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
        Alloc::alloc(tycker, ss::Type::from(val))
    }
}
impl Alloc<ss::TypeId> for ss::Forall {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
        Alloc::alloc(tycker, ss::Type::from(val))
    }
}
impl Alloc<ss::TypeId> for ss::Prod<ss::TypeId> {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
        Alloc::alloc(tycker, ss::Type::from(val))
    }
}
impl Alloc<ss::TypeId> for ss::Exists {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
        Alloc::alloc(tycker, ss::Type::from(val))
    }
}
impl Alloc<ss::TypeId> for ss::Data {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
        Alloc::alloc(tycker, ss::Type::from(val))
    }
}
impl Alloc<ss::TypeId> for ss::CoData {
    fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
        Alloc::alloc(tycker, ss::Type::from(val))
    }
}
