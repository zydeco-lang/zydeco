use crate::{syntax::*, *};
use derive_more::From;
use std::collections::HashMap;

/// A type that can be joined with another type, producing their least upper bound.
/// T \/ T ?~~> T'
pub trait Lub<Rhs = Self>: Sized {
    type Out;
    fn lub_k(self, other: Rhs, tycker: &mut Tycker) -> ResultKont<Self::Out> {
        let res = self.lub(other, tycker);
        tycker.err_p_to_k(res)
    }
    fn lub(self, other: Rhs, tycker: &mut Tycker) -> Result<Self::Out>;
    fn lub_inner(self, other: Rhs, tycker: &mut Tycker) -> Result<Self::Out>;
}

impl Lub for KindId {
    type Out = KindId;

    fn lub(self, other: Self, tycker: &mut Tycker) -> Result<Self::Out> {
        tycker.guarded(|tycker| {
            // administrative
            tycker.stack.push_back(TyckTask::Lub(self.into(), other.into()));
            self.lub_inner(other, tycker)
        })
    }
    fn lub_inner(self, other: Self, tycker: &mut Tycker) -> Result<Self::Out> {
        let lhs = tycker.statics.kinds[&self].clone();
        let rhs = tycker.statics.kinds[&other].clone();
        fn fill_kd(tycker: &mut Tycker, fill: FillId, kd: KindId) -> Result<KindId> {
            match tycker.statics.solus.remove(&fill) {
                | Some(old) => match old {
                    | AnnId::Set | AnnId::Type(_) => {
                        tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | AnnId::Kind(old) => {
                        let kd = Lub::lub(old, kd, tycker)?;
                        tycker.statics.solus.insert(fill, kd.into());
                        Ok(kd)
                    }
                },
                | None => {
                    tycker.statics.solus.insert(fill, kd.into());
                    Ok(kd)
                }
            }
        }
        let res = match (lhs, rhs) {
            | (_, Kind::Fill(rhs)) => fill_kd(tycker, rhs, self)?,
            | (Kind::Fill(lhs), _) => fill_kd(tycker, lhs, other)?,
            | (Kind::VType(VType), Kind::VType(VType)) => {
                let kd = Alloc::alloc(tycker, VType, ());
                kd
            }
            | (Kind::CType(CType), Kind::CType(CType)) => {
                let kd = Alloc::alloc(tycker, CType, ());
                kd
            }
            | (Kind::Arrow(lhs), Kind::Arrow(rhs)) => {
                let Arrow(lin, lout) = lhs;
                let Arrow(rin, rout) = rhs;
                let kd_in = lin.lub(rin, tycker)?;
                let kd_out = lout.lub(rout, tycker)?;
                let kd = Alloc::alloc(tycker, Arrow(kd_in, kd_out), ());
                kd
            }
            | (Kind::VType(_), _) | (Kind::CType(_), _) | (Kind::Arrow(_), _) => {
                tycker.err(TyckError::KindMismatch, std::panic::Location::caller())?
            }
        };
        Ok(res)
    }
}

#[derive(From, Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum BinderId {
    Var(DefId),
    Abst(AbstId),
}

#[derive(Clone)]
struct Debruijn {
    level: usize,
    lhs: HashMap<BinderId, usize>,
    rhs: HashMap<BinderId, usize>,
}

impl Debruijn {
    fn new() -> Self {
        Self { level: 0, lhs: HashMap::new(), rhs: HashMap::new() }
    }
    fn insert<T>(mut self, lhs: Option<T>, rhs: Option<T>) -> Self
    where
        T: Into<BinderId>,
    {
        if let Some(lhs) = lhs {
            self.lhs.insert(lhs.into(), self.level);
        }
        if let Some(rhs) = rhs {
            self.rhs.insert(rhs.into(), self.level);
        }
        self.level += 1;
        self
    }
    fn lookup_lhs<T>(&self, lhs: T) -> Option<usize>
    where
        T: Into<BinderId>,
    {
        self.lhs.get(&lhs.into()).cloned()
    }
    fn lookup_rhs<T>(&self, rhs: T) -> Option<usize>
    where
        T: Into<BinderId>,
    {
        self.rhs.get(&rhs.into()).cloned()
    }
    fn lub(self, lhs_id: TypeId, rhs_id: TypeId, tycker: &mut Tycker) -> Result<TypeId> {
        tycker.guarded(|tycker| {
            // administrative
            tycker.stack.push_back(TyckTask::Lub(lhs_id.into(), rhs_id.into()));
            self.lub_inner(lhs_id, rhs_id, tycker)
        })
    }
    fn lub_inner(self, lhs_id: TypeId, rhs_id: TypeId, tycker: &mut Tycker) -> Result<TypeId> {
        let lhs = tycker.statics.types[&lhs_id].clone();
        let rhs = tycker.statics.types[&rhs_id].clone();
        fn fill_ty(tycker: &mut Tycker, fill: FillId, ty: TypeId) -> Result<TypeId> {
            match tycker.statics.solus.remove(&fill) {
                | Some(old) => match old {
                    | AnnId::Set | AnnId::Kind(_) => {
                        tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | AnnId::Type(old) => {
                        let ty = Lub::lub(old, ty, tycker)?;
                        let ty_ = fill.fill(tycker, ty.into())?.as_type();
                        Ok(ty_)
                    }
                },
                | None => {
                    tycker.statics.solus.insert(fill, ty.into());
                    Ok(ty)
                }
            }
        }
        let res = match (lhs, rhs) {
            | (_, Type::Fill(rhs)) => fill_ty(tycker, rhs, lhs_id)?,
            | (Type::Fill(lhs), _) => fill_ty(tycker, lhs, rhs_id)?,
            | (Type::Var(lhs), Type::Var(rhs)) => {
                match (self.lookup_lhs(lhs), self.lookup_rhs(rhs)) {
                    | (Some(l), Some(r)) if l == r => lhs_id,
                    | _ => tycker.err(
                        TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                        std::panic::Location::caller(),
                    )?,
                }
            }
            | (Type::Abst(lhs), Type::Abst(rhs)) => {
                match (self.lookup_lhs(lhs), self.lookup_rhs(rhs)) {
                    | (Some(l), Some(r)) if l == r => lhs_id,
                    | (None, None) if lhs == rhs => lhs_id,
                    | _ => tycker.err(
                        TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                        std::panic::Location::caller(),
                    )?,
                }
            }
            | (Type::Abst(_), _) => tycker.err(
                TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                std::panic::Location::caller(),
            )?,
            | (Type::Var(_), _) => tycker.err(
                TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                std::panic::Location::caller(),
            )?,
            | (Type::Abs(Abs(lpat, lbody)), Type::Abs(Abs(rpat, rbody))) => {
                let (ldef, lkd) = lpat.try_destruct_def(tycker);
                let (rdef, rkd) = rpat.try_destruct_def(tycker);
                let _kd = Lub::lub(lkd, rkd, tycker)?;
                let body = self.insert(ldef, rdef).lub(lbody, rbody, tycker)?;
                if body == lbody {
                    lhs_id
                } else {
                    let kd = tycker.statics.annotations_type[&lhs_id].clone();
                    let abs = Alloc::alloc(tycker, Abs(lpat, body), kd);
                    abs
                }
            }
            | (Type::Abs(_), _) => tycker.err(
                TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                std::panic::Location::caller(),
            )?,
            | (Type::App(App(lf, la)), Type::App(App(rf, ra))) => {
                let f = self.clone().lub(lf, rf, tycker)?;
                let a = self.lub(la, ra, tycker)?;
                if f == lf && a == la {
                    lhs_id
                } else {
                    let kd = tycker.statics.annotations_type[&lhs_id].clone();
                    let app = Alloc::alloc(tycker, App(f, a), kd);
                    app.normalize(tycker, kd)?
                }
            }
            | (Type::App(_), _) => tycker.err(
                TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                std::panic::Location::caller(),
            )?,
            | (Type::Thk(ThkTy), Type::Thk(ThkTy)) => lhs_id,
            | (Type::Thk(_), _) => tycker.err(
                TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                std::panic::Location::caller(),
            )?,
            | (Type::Ret(RetTy), Type::Ret(RetTy)) => lhs_id,
            | (Type::Ret(_), _) => tycker.err(
                TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                std::panic::Location::caller(),
            )?,
            | (Type::Unit(UnitTy), Type::Unit(UnitTy)) => lhs_id,
            | (Type::Unit(_), _) => tycker.err(
                TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                std::panic::Location::caller(),
            )?,
            | (Type::Int(IntTy), Type::Int(IntTy)) => lhs_id,
            | (Type::Int(_), _) => tycker.err(
                TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                std::panic::Location::caller(),
            )?,
            | (Type::Char(CharTy), Type::Char(CharTy)) => lhs_id,
            | (Type::Char(_), _) => tycker.err(
                TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                std::panic::Location::caller(),
            )?,
            | (Type::String(StringTy), Type::String(StringTy)) => lhs_id,
            | (Type::String(_), _) => tycker.err(
                TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                std::panic::Location::caller(),
            )?,
            | (Type::OS(OSTy), Type::OS(OSTy)) => lhs_id,
            | (Type::OS(_), _) => tycker.err(
                TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                std::panic::Location::caller(),
            )?,
            | (Type::Arrow(Arrow(la, lb)), Type::Arrow(Arrow(ra, rb))) => {
                let a = self.clone().lub(la, ra, tycker)?;
                let b = self.lub(lb, rb, tycker)?;
                if a == la && b == lb {
                    lhs_id
                } else {
                    let kd = tycker.statics.annotations_type[&lhs_id].clone();
                    let arrow = Alloc::alloc(tycker, Arrow(a, b), kd);
                    arrow
                }
            }
            | (Type::Arrow(_), _) => tycker.err(
                TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                std::panic::Location::caller(),
            )?,
            | (Type::Forall(Forall(labst, lbody)), Type::Forall(Forall(rabst, rbody))) => {
                let lkd = tycker.statics.annotations_abst[&labst].to_owned();
                let rkd = tycker.statics.annotations_abst[&rabst].to_owned();
                let _kd = Lub::lub(lkd, rkd, tycker)?;
                let body = self.insert(Some(labst), Some(rabst)).lub(lbody, rbody, tycker)?;
                if body == lbody {
                    lhs_id
                } else {
                    let kd = tycker.statics.annotations_type[&lhs_id].clone();
                    let forall = Alloc::alloc(tycker, Forall(labst, body), kd);
                    forall
                }
            }
            | (Type::Forall(_), _) => tycker.err(
                TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                std::panic::Location::caller(),
            )?,
            | (Type::Prod(Prod(la, lb)), Type::Prod(Prod(ra, rb))) => {
                let a = self.clone().lub(la, ra, tycker)?;
                let b = self.lub(lb, rb, tycker)?;
                if a == la && b == lb {
                    lhs_id
                } else {
                    let kd = tycker.statics.annotations_type[&lhs_id].clone();
                    let prod = Alloc::alloc(tycker, Prod(a, b), kd);
                    prod
                }
            }
            | (Type::Prod(_), _) => tycker.err(
                TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                std::panic::Location::caller(),
            )?,
            | (Type::Exists(Exists(labst, lbody)), Type::Exists(Exists(rabst, rbody))) => {
                let lkd = tycker.statics.annotations_abst[&labst].to_owned();
                let rkd = tycker.statics.annotations_abst[&rabst].to_owned();
                let _kd = Lub::lub(lkd, rkd, tycker)?;
                let body = self.insert(Some(labst), Some(rabst)).lub(lbody, rbody, tycker)?;
                if body == lbody {
                    lhs_id
                } else {
                    let kd = tycker.statics.annotations_type[&lhs_id].clone();
                    let exists = Alloc::alloc(tycker, Exists(labst, body), kd);
                    exists
                }
            }
            | (Type::Exists(_), _) => tycker.err(
                TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                std::panic::Location::caller(),
            )?,
            | (Type::Data(lhs), Type::Data(rhs)) => {
                if lhs != rhs {
                    tycker.err(
                        TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                        std::panic::Location::caller(),
                    )?
                }
                lhs_id
            }
            | (Type::Data(_), _) => tycker.err(
                TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                std::panic::Location::caller(),
            )?,
            | (Type::CoData(lhs), Type::CoData(rhs)) => {
                if lhs != rhs {
                    tycker.err(
                        TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                        std::panic::Location::caller(),
                    )?
                }
                lhs_id
            }
            | (Type::CoData(_), _) => tycker.err(
                TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                std::panic::Location::caller(),
            )?,
        };
        Ok(res)
    }
}

impl Lub for TypeId {
    type Out = TypeId;

    /// We need to remember the definitions introduced by both sides.
    /// We did this by using Debruijn.
    fn lub(self, other: Self, tycker: &mut Tycker) -> Result<Self::Out> {
        self.lub_inner(other, tycker)
    }
    fn lub_inner(self, other: Self, tycker: &mut Tycker) -> Result<Self::Out> {
        Debruijn::new().lub(self, other, tycker)
    }
}

impl Lub for AnnId {
    type Out = AnnId;

    fn lub(self, other: Self, tycker: &mut Tycker) -> Result<Self::Out> {
        self.lub_inner(other, tycker)
    }
    fn lub_inner(self, other: Self, tycker: &mut Tycker) -> Result<Self::Out> {
        let res = match (self, other) {
            | (AnnId::Set, AnnId::Set) => AnnId::Set,
            | (AnnId::Set, _) | (_, AnnId::Set) => {
                tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
            }
            | (AnnId::Kind(lhs), AnnId::Kind(rhs)) => {
                let kd = lhs.lub(rhs, tycker)?;
                kd.into()
            }
            | (AnnId::Kind(_), _) | (_, AnnId::Kind(_)) => {
                tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
            }
            | (AnnId::Type(lhs), AnnId::Type(rhs)) => {
                let ty = lhs.lub(rhs, tycker)?;
                ty.into()
            }
        };
        Ok(res)
    }
}
