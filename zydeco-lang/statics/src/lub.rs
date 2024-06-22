use crate::{syntax::*, *};
use std::collections::HashMap;

/// A type that can be joined with another type, producing their least upper bound.
/// T \/ T ?~~> T'
pub trait Lub<Rhs = Self> {
    type Out;
    fn lub(self, other: Rhs, tycker: &mut Tycker) -> Result<Self::Out>;
}

impl Lub for () {
    type Out = ();
    fn lub(self, (): Self, _tycker: &mut Tycker) -> Result<Self::Out> {
        Ok(())
    }
}

impl Lub for KindId {
    type Out = KindId;

    fn lub(self, other: Self, tycker: &mut Tycker) -> Result<Self::Out> {
        let lhs = tycker.statics.kinds[&self].clone();
        let rhs = tycker.statics.kinds[&other].clone();
        fn fill_kd(tycker: &mut Tycker, fill: FillId, kd: KindId) -> Result<KindId> {
            tycker.statics.solus.insert_or_else(fill, kd.into(), |_old, _new| {
                Err(TyckError::KindMismatch)? // Todo: deal with fill
            })?;
            Ok(kd)
        }
        match (lhs, rhs) {
            | (_, Kind::Fill(rhs)) => fill_kd(tycker, rhs, self),
            | (Kind::Fill(lhs), _) => fill_kd(tycker, lhs, other),
            | (Kind::VType(VType), Kind::VType(VType)) => {
                let kd = Alloc::alloc(tycker, VType);
                Ok(kd)
            }
            | (Kind::CType(CType), Kind::CType(CType)) => {
                let kd = Alloc::alloc(tycker, CType);
                Ok(kd)
            }
            | (Kind::Arrow(lhs), Kind::Arrow(rhs)) => {
                let Arrow(lin, lout) = lhs;
                let Arrow(rin, rout) = rhs;
                let kd_in = lin.lub(rin, tycker)?;
                let kd_out = lout.lub(rout, tycker)?;
                let kd = Alloc::alloc(tycker, Arrow(kd_in, kd_out));
                Ok(kd)
            }
            | (Kind::VType(_), _) | (Kind::CType(_), _) | (Kind::Arrow(_), _) => {
                Err(TyckError::KindMismatch)
            }
        }
    }
}

#[derive(Clone)]
struct Debruijn {
    level: usize,
    lhs: HashMap<DefId, usize>,
    rhs: HashMap<DefId, usize>,
}

impl Debruijn {
    fn new() -> Self {
        Self { level: 0, lhs: HashMap::new(), rhs: HashMap::new() }
    }
    fn insert(mut self, lhs: Option<DefId>, rhs: Option<DefId>) -> Self {
        if let Some(lhs) = lhs {
            self.lhs.insert(lhs, self.level);
        }
        if let Some(rhs) = rhs {
            self.rhs.insert(rhs, self.level);
        }
        self.level += 1;
        self
    }
    fn lub(self, lhs_id: TypeId, rhs_id: TypeId, tycker: &mut Tycker) -> Result<TypeId> {
        let lhs = tycker.statics.types[&lhs_id].clone();
        let rhs = tycker.statics.types[&rhs_id].clone();
        fn fill_ty(tycker: &mut Tycker, fill: FillId, ty: TypeId) -> Result<TypeId> {
            tycker.statics.solus.insert_or_else(fill, ty.into(), |_old, _new| {
                Err(TyckError::TypeMismatch)? // Todo: deal with fill
            })?;
            Ok(ty)
        }
        match (lhs, rhs) {
            | (_, Type::Fill(rhs)) => fill_ty(tycker, rhs, lhs_id),
            | (Type::Fill(lhs), _) => fill_ty(tycker, lhs, rhs_id),
            | (Type::Var(lhs), Type::Var(rhs)) => {
                if self.lhs[&lhs] != self.rhs[&rhs] {
                    Err(TyckError::TypeMismatch)?
                }
                Ok(lhs_id)
            }
            | (Type::Abst(lhs), Type::Abst(rhs)) => {
                if lhs == rhs {
                    Ok(lhs_id)
                } else {
                    Err(TyckError::TypeMismatch)?
                }
            }
            | (Type::Abst(_), _) => Err(TyckError::TypeMismatch)?,
            | (Type::Var(_), _) => Err(TyckError::TypeMismatch)?,
            | (Type::Abs(Abs(lpat, lbody)), Type::Abs(Abs(rpat, rbody))) => {
                let (ldef, lkd) = tycker.extract_tpat(lpat);
                let (rdef, rkd) = tycker.extract_tpat(rpat);
                let _kd = Lub::lub(lkd, rkd, tycker)?;
                let body = self.insert(ldef, rdef).lub(lbody, rbody, tycker)?;
                if body == lbody {
                    Ok(lhs_id)
                } else {
                    let abs = Alloc::alloc(tycker, Abs(lpat, body));
                    Ok(abs)
                }
            }
            | (Type::Abs(_), _) => Err(TyckError::TypeMismatch)?,
            | (Type::App(App(lf, la)), Type::App(App(rf, ra))) => {
                let f = self.clone().lub(lf, rf, tycker)?;
                let a = self.lub(la, ra, tycker)?;
                if f == lf && a == la {
                    Ok(lhs_id)
                } else {
                    let app = Alloc::alloc(tycker, App(f, a));
                    Ok(app)
                }
            }
            | (Type::App(_), _) => Err(TyckError::TypeMismatch)?,
            | (Type::Thunk(ThunkTy), Type::Thunk(ThunkTy)) => Ok(lhs_id),
            | (Type::Thunk(_), _) => Err(TyckError::TypeMismatch)?,
            | (Type::Ret(RetTy), Type::Ret(RetTy)) => Ok(lhs_id),
            | (Type::Ret(_), _) => Err(TyckError::TypeMismatch)?,
            | (Type::Unit(UnitTy), Type::Unit(UnitTy)) => Ok(lhs_id),
            | (Type::Unit(_), _) => Err(TyckError::TypeMismatch)?,
            | (Type::Int(IntTy), Type::Int(IntTy)) => Ok(lhs_id),
            | (Type::Int(_), _) => Err(TyckError::TypeMismatch)?,
            | (Type::Char(CharTy), Type::Char(CharTy)) => Ok(lhs_id),
            | (Type::Char(_), _) => Err(TyckError::TypeMismatch)?,
            | (Type::String(StringTy), Type::String(StringTy)) => Ok(lhs_id),
            | (Type::String(_), _) => Err(TyckError::TypeMismatch)?,
            | (Type::OS(OSTy), Type::OS(OSTy)) => Ok(lhs_id),
            | (Type::OS(_), _) => Err(TyckError::TypeMismatch)?,
            | (Type::Arrow(Arrow(la, lb)), Type::Arrow(Arrow(ra, rb))) => {
                let a = self.clone().lub(la, ra, tycker)?;
                let b = self.lub(lb, rb, tycker)?;
                if a == la && b == lb {
                    Ok(lhs_id)
                } else {
                    let arrow = Alloc::alloc(tycker, Arrow(a, b));
                    Ok(arrow)
                }
            }
            | (Type::Arrow(_), _) => Err(TyckError::TypeMismatch)?,
            | (Type::Forall(Forall(lpat, lbody)), Type::Forall(Forall(rpat, rbody))) => {
                let (ldef, lkd) = tycker.extract_tpat(lpat);
                let (rdef, rkd) = tycker.extract_tpat(rpat);
                let _kd = Lub::lub(lkd, rkd, tycker)?;
                let body = self.insert(ldef, rdef).lub(lbody, rbody, tycker)?;
                if body == lbody {
                    Ok(lhs_id)
                } else {
                    let forall = Alloc::alloc(tycker, Forall(lpat, body));
                    Ok(forall)
                }
            }
            | (Type::Forall(_), _) => Err(TyckError::TypeMismatch)?,
            | (Type::Prod(Prod(la, lb)), Type::Prod(Prod(ra, rb))) => {
                let a = self.clone().lub(la, ra, tycker)?;
                let b = self.lub(lb, rb, tycker)?;
                if a == la && b == lb {
                    Ok(lhs_id)
                } else {
                    let prod = Alloc::alloc(tycker, Prod(a, b));
                    Ok(prod)
                }
            }
            | (Type::Prod(_), _) => Err(TyckError::TypeMismatch)?,
            | (Type::Exists(Exists(lpat, lbody)), Type::Exists(Exists(rpat, rbody))) => {
                let (ldef, lkd) = tycker.extract_tpat(lpat);
                let (rdef, rkd) = tycker.extract_tpat(rpat);
                let _kd = Lub::lub(lkd, rkd, tycker)?;
                let body = self.insert(ldef, rdef).lub(lbody, rbody, tycker)?;
                if body == lbody {
                    Ok(lhs_id)
                } else {
                    let exists = Alloc::alloc(tycker, Exists(lpat, body));
                    Ok(exists)
                }
            }
            | (Type::Exists(_), _) => Err(TyckError::TypeMismatch)?,
            | (Type::Data(lhs), Type::Data(rhs)) => {
                if lhs == rhs {
                    Ok(lhs_id)
                } else {
                    Err(TyckError::TypeMismatch)?
                }
            }
            | (Type::Data(_), _) => Err(TyckError::TypeMismatch)?,
            | (Type::CoData(lhs), Type::CoData(rhs)) => {
                if lhs == rhs {
                    Ok(lhs_id)
                } else {
                    Err(TyckError::TypeMismatch)?
                }
            }
            | (Type::CoData(_), _) => Err(TyckError::TypeMismatch)?,
        }
    }
}

impl Lub for TypeId {
    /// We need to remember the definitions introduced by both sides.
    // Todo..
    type Out = TypeId;

    fn lub(self, other: Self, tycker: &mut Tycker) -> Result<Self::Out> {
        Debruijn::new().lub(self, other, tycker)
    }
}

impl Lub for AnnId {
    type Out = AnnId;

    fn lub(self, other: Self, tycker: &mut Tycker) -> Result<Self::Out> {
        match (self, other) {
            | (AnnId::Set, AnnId::Set) => Ok(AnnId::Set),
            | (AnnId::Set, _) | (_, AnnId::Set) => Err(TyckError::SortMismatch),
            | (AnnId::Kind(lhs), AnnId::Kind(rhs)) => {
                let kd = lhs.lub(rhs, tycker)?;
                Ok(kd.into())
            }
            | (AnnId::Kind(_), _) | (_, AnnId::Kind(_)) => Err(TyckError::SortMismatch),
            | (AnnId::Type(lhs), AnnId::Type(rhs)) => {
                let ty = lhs.lub(rhs, tycker)?;
                Ok(ty.into())
            }
        }
    }
}
