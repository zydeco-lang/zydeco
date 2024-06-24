use crate::{syntax::*, *};
use std::collections::HashMap;

/// A type that can be joined with another type, producing their least upper bound.
/// T \/ T ?~~> T'
pub trait Lub<Rhs = Self> {
    type Out;
    fn lub(self, other: Rhs, tycker: &mut Tycker) -> ResultKont<Self::Out>;
}

impl Lub for () {
    type Out = ();
    fn lub(self, (): Self, _tycker: &mut Tycker) -> ResultKont<Self::Out> {
        Ok(())
    }
}

impl Lub for KindId {
    type Out = KindId;

    fn lub(self, other: Self, tycker: &mut Tycker) -> ResultKont<Self::Out> {
        {
            // administrative
            tycker.stack.push_back(TyckTask::Lub(self.into(), other.into()));
        }
        let lhs = tycker.statics.kinds[&self].clone();
        let rhs = tycker.statics.kinds[&other].clone();
        fn fill_kd(tycker: &mut Tycker, fill: FillId, kd: KindId) -> ResultKont<KindId> {
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
                let kd = Alloc::alloc(tycker, VType);
                kd
            }
            | (Kind::CType(CType), Kind::CType(CType)) => {
                let kd = Alloc::alloc(tycker, CType);
                kd
            }
            | (Kind::Arrow(lhs), Kind::Arrow(rhs)) => {
                let Arrow(lin, lout) = lhs;
                let Arrow(rin, rout) = rhs;
                let kd_in = lin.lub(rin, tycker)?;
                let kd_out = lout.lub(rout, tycker)?;
                let kd = Alloc::alloc(tycker, Arrow(kd_in, kd_out));
                kd
            }
            | (Kind::VType(_), _) | (Kind::CType(_), _) | (Kind::Arrow(_), _) => {
                tycker.err(TyckError::KindMismatch, std::panic::Location::caller())?
            }
        };
        {
            // administrative
            tycker.stack.pop_back();
        }
        Ok(res)
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
    fn lub(self, lhs_id: TypeId, rhs_id: TypeId, tycker: &mut Tycker) -> ResultKont<TypeId> {
        {
            // administrative
            tycker.stack.push_back(TyckTask::Lub(lhs_id.into(), rhs_id.into()));
        }
        let lhs = tycker.statics.types[&lhs_id].clone();
        let rhs = tycker.statics.types[&rhs_id].clone();
        fn fill_ty(tycker: &mut Tycker, fill: FillId, ty: TypeId) -> ResultKont<TypeId> {
            match tycker.statics.solus.remove(&fill) {
                | Some(old) => match old {
                    | AnnId::Set | AnnId::Kind(_) => {
                        tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | AnnId::Type(old) => {
                        let ty = Lub::lub(old, ty, tycker)?;
                        tycker.statics.solus.insert(fill, ty.into());
                        Ok(ty)
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
                if self.lhs[&lhs] != self.rhs[&rhs] {
                    tycker.err(TyckError::TypeMismatch, std::panic::Location::caller())?
                }
                lhs_id
            }
            | (Type::Abst(lhs), Type::Abst(rhs)) => {
                if lhs != rhs {
                    tycker.err(TyckError::TypeMismatch, std::panic::Location::caller())?
                }
                lhs_id
            }
            | (Type::Abst(_), _) => {
                tycker.err(TyckError::TypeMismatch, std::panic::Location::caller())?
            }
            | (Type::Var(_), _) => {
                tycker.err(TyckError::TypeMismatch, std::panic::Location::caller())?
            }
            | (Type::Abs(Abs(lpat, lbody)), Type::Abs(Abs(rpat, rbody))) => {
                let (ldef, lkd) = tycker.extract_tpat(lpat);
                let (rdef, rkd) = tycker.extract_tpat(rpat);
                let _kd = Lub::lub(lkd, rkd, tycker)?;
                let body = self.insert(ldef, rdef).lub(lbody, rbody, tycker)?;
                if body == lbody {
                    lhs_id
                } else {
                    let abs = Alloc::alloc(tycker, Abs(lpat, body));
                    abs
                }
            }
            | (Type::Abs(_), _) => {
                tycker.err(TyckError::TypeMismatch, std::panic::Location::caller())?
            }
            | (Type::App(App(lf, la)), Type::App(App(rf, ra))) => {
                let f = self.clone().lub(lf, rf, tycker)?;
                let a = self.lub(la, ra, tycker)?;
                if f == lf && a == la {
                    lhs_id
                } else {
                    let app = Alloc::alloc(tycker, App(f, a));
                    app
                }
            }
            | (Type::App(_), _) => {
                tycker.err(TyckError::TypeMismatch, std::panic::Location::caller())?
            }
            | (Type::Thunk(ThunkTy), Type::Thunk(ThunkTy)) => lhs_id,
            | (Type::Thunk(_), _) => {
                tycker.err(TyckError::TypeMismatch, std::panic::Location::caller())?
            }
            | (Type::Ret(RetTy), Type::Ret(RetTy)) => lhs_id,
            | (Type::Ret(_), _) => {
                tycker.err(TyckError::TypeMismatch, std::panic::Location::caller())?
            }
            | (Type::Unit(UnitTy), Type::Unit(UnitTy)) => lhs_id,
            | (Type::Unit(_), _) => {
                tycker.err(TyckError::TypeMismatch, std::panic::Location::caller())?
            }
            | (Type::Int(IntTy), Type::Int(IntTy)) => lhs_id,
            | (Type::Int(_), _) => {
                tycker.err(TyckError::TypeMismatch, std::panic::Location::caller())?
            }
            | (Type::Char(CharTy), Type::Char(CharTy)) => lhs_id,
            | (Type::Char(_), _) => {
                tycker.err(TyckError::TypeMismatch, std::panic::Location::caller())?
            }
            | (Type::String(StringTy), Type::String(StringTy)) => lhs_id,
            | (Type::String(_), _) => {
                tycker.err(TyckError::TypeMismatch, std::panic::Location::caller())?
            }
            | (Type::OS(OSTy), Type::OS(OSTy)) => lhs_id,
            | (Type::OS(_), _) => {
                tycker.err(TyckError::TypeMismatch, std::panic::Location::caller())?
            }
            | (Type::Arrow(Arrow(la, lb)), Type::Arrow(Arrow(ra, rb))) => {
                let a = self.clone().lub(la, ra, tycker)?;
                let b = self.lub(lb, rb, tycker)?;
                if a == la && b == lb {
                    lhs_id
                } else {
                    let arrow = Alloc::alloc(tycker, Arrow(a, b));
                    arrow
                }
            }
            | (Type::Arrow(_), _) => {
                tycker.err(TyckError::TypeMismatch, std::panic::Location::caller())?
            }
            | (Type::Forall(Forall(lpat, lbody)), Type::Forall(Forall(rpat, rbody))) => {
                let (ldef, lkd) = tycker.extract_tpat(lpat);
                let (rdef, rkd) = tycker.extract_tpat(rpat);
                let _kd = Lub::lub(lkd, rkd, tycker)?;
                let body = self.insert(ldef, rdef).lub(lbody, rbody, tycker)?;
                if body == lbody {
                    lhs_id
                } else {
                    let forall = Alloc::alloc(tycker, Forall(lpat, body));
                    forall
                }
            }
            | (Type::Forall(_), _) => {
                tycker.err(TyckError::TypeMismatch, std::panic::Location::caller())?
            }
            | (Type::Prod(Prod(la, lb)), Type::Prod(Prod(ra, rb))) => {
                let a = self.clone().lub(la, ra, tycker)?;
                let b = self.lub(lb, rb, tycker)?;
                if a == la && b == lb {
                    lhs_id
                } else {
                    let prod = Alloc::alloc(tycker, Prod(a, b));
                    prod
                }
            }
            | (Type::Prod(_), _) => {
                tycker.err(TyckError::TypeMismatch, std::panic::Location::caller())?
            }
            | (Type::Exists(Exists(lpat, lbody)), Type::Exists(Exists(rpat, rbody))) => {
                let (ldef, lkd) = tycker.extract_tpat(lpat);
                let (rdef, rkd) = tycker.extract_tpat(rpat);
                let _kd = Lub::lub(lkd, rkd, tycker)?;
                let body = self.insert(ldef, rdef).lub(lbody, rbody, tycker)?;
                if body == lbody {
                    lhs_id
                } else {
                    let exists = Alloc::alloc(tycker, Exists(lpat, body));
                    exists
                }
            }
            | (Type::Exists(_), _) => {
                tycker.err(TyckError::TypeMismatch, std::panic::Location::caller())?
            }
            | (Type::Data(lhs), Type::Data(rhs)) => {
                if lhs != rhs {
                    tycker.err(TyckError::TypeMismatch, std::panic::Location::caller())?
                }
                lhs_id
            }
            | (Type::Data(_), _) => {
                tycker.err(TyckError::TypeMismatch, std::panic::Location::caller())?
            }
            | (Type::CoData(lhs), Type::CoData(rhs)) => {
                if lhs != rhs {
                    tycker.err(TyckError::TypeMismatch, std::panic::Location::caller())?
                }
                lhs_id
            }
            | (Type::CoData(_), _) => {
                tycker.err(TyckError::TypeMismatch, std::panic::Location::caller())?
            }
        };
        {
            // administrative
            tycker.stack.pop_back();
        }
        Ok(res)
    }
}

impl Lub for TypeId {
    /// We need to remember the definitions introduced by both sides.
    // Todo..
    type Out = TypeId;

    fn lub(self, other: Self, tycker: &mut Tycker) -> ResultKont<Self::Out> {
        Debruijn::new().lub(self, other, tycker)
    }
}

impl Lub for AnnId {
    type Out = AnnId;

    fn lub(self, other: Self, tycker: &mut Tycker) -> ResultKont<Self::Out> {
        // {
        //     // administrative
        //     tycker.stack.push_back(TyckTask::Lub(self, other));
        // }
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
        // {
        //     // administrative
        //     tycker.stack.pop_back();
        // }
        Ok(res)
    }
}
