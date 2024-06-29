use crate::{syntax::*, *};
use std::collections::HashMap;

/// A type that can be joined with another type, producing their least upper bound.
/// T \/ T ?~~> T'
pub trait Lub<Rhs = Self>: Sized {
    type Out;
    fn lub(self, other: Rhs, tycker: &mut Tycker) -> Result<Self::Out>;
    fn lub_k(self, other: Rhs, tycker: &mut Tycker) -> ResultKont<Self::Out> {
        let res = self.lub(other, tycker);
        tycker.err_p_to_k(res)
    }
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
        {
            // administrative
            tycker.stack.push_back(TyckTask::Lub(self.into(), other.into()));
        }
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
                let kd = Alloc::alloc(&mut tycker.statics, VType, ());
                kd
            }
            | (Kind::CType(CType), Kind::CType(CType)) => {
                let kd = Alloc::alloc(&mut tycker.statics, CType, ());
                kd
            }
            | (Kind::Arrow(lhs), Kind::Arrow(rhs)) => {
                let Arrow(lin, lout) = lhs;
                let Arrow(rin, rout) = rhs;
                let kd_in = lin.lub(rin, tycker)?;
                let kd_out = lout.lub(rout, tycker)?;
                let kd = Alloc::alloc(&mut tycker.statics, Arrow(kd_in, kd_out), ());
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
    fn lub(self, lhs_id: TypeId, rhs_id: TypeId, tycker: &mut Tycker) -> Result<TypeId> {
        {
            // administrative
            tycker.stack.push_back(TyckTask::Lub(lhs_id.into(), rhs_id.into()));
        }
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
                        let ty_ = tycker.fill(fill, ty.into())?.as_type();
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
                if self.lhs[&lhs] != self.rhs[&rhs] {
                    tycker.err(
                        TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                        std::panic::Location::caller(),
                    )?
                }
                lhs_id
            }
            | (Type::Abst(lhs), Type::Abst(rhs)) => {
                if lhs != rhs {
                    tycker.err(
                        TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                        std::panic::Location::caller(),
                    )?
                }
                lhs_id
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
                let (ldef, lkd) = tycker.extract_tpat(lpat);
                let (rdef, rkd) = tycker.extract_tpat(rpat);
                let _kd = Lub::lub(lkd, rkd, tycker)?;
                let body = self.insert(ldef, rdef).lub(lbody, rbody, tycker)?;
                if body == lbody {
                    lhs_id
                } else {
                    let kd = tycker.statics.annotations_type[&lhs_id].clone();
                    let abs = Alloc::alloc(&mut tycker.statics, Abs(lpat, body), kd);
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
                    let app = Alloc::alloc(&mut tycker.statics, App(f, a), kd);
                    app.normalize(tycker, kd)?
                }
            }
            | (Type::App(_), _) => tycker.err(
                TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                std::panic::Location::caller(),
            )?,
            | (Type::Thunk(ThunkTy), Type::Thunk(ThunkTy)) => lhs_id,
            | (Type::Thunk(_), _) => tycker.err(
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
                    let arrow = Alloc::alloc(&mut tycker.statics, Arrow(a, b), kd);
                    arrow
                }
            }
            | (Type::Arrow(_), _) => tycker.err(
                TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                std::panic::Location::caller(),
            )?,
            | (Type::Forall(Forall(lpat, lbody)), Type::Forall(Forall(rpat, rbody))) => {
                let (ldef, lkd) = tycker.extract_tpat(lpat);
                let (rdef, rkd) = tycker.extract_tpat(rpat);
                let _kd = Lub::lub(lkd, rkd, tycker)?;
                let body = self.insert(ldef, rdef).lub(lbody, rbody, tycker)?;
                if body == lbody {
                    lhs_id
                } else {
                    let kd = tycker.statics.annotations_type[&lhs_id].clone();
                    let forall = Alloc::alloc(&mut tycker.statics, Forall(lpat, body), kd);
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
                    let prod = Alloc::alloc(&mut tycker.statics, Prod(a, b), kd);
                    prod
                }
            }
            | (Type::Prod(_), _) => tycker.err(
                TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                std::panic::Location::caller(),
            )?,
            | (Type::Exists(Exists(lpat, lbody)), Type::Exists(Exists(rpat, rbody))) => {
                let (ldef, lkd) = tycker.extract_tpat(lpat);
                let (rdef, rkd) = tycker.extract_tpat(rpat);
                let _kd = Lub::lub(lkd, rkd, tycker)?;
                let body = self.insert(ldef, rdef).lub(lbody, rbody, tycker)?;
                if body == lbody {
                    lhs_id
                } else {
                    let kd = tycker.statics.annotations_type[&lhs_id].clone();
                    let exists = Alloc::alloc(&mut tycker.statics, Exists(lpat, body), kd);
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

    fn lub(self, other: Self, tycker: &mut Tycker) -> Result<Self::Out> {
        Debruijn::new().lub(self, other, tycker)
    }
}

impl Lub for AnnId {
    type Out = AnnId;

    fn lub(self, other: Self, tycker: &mut Tycker) -> Result<Self::Out> {
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
