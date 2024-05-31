use crate::{syntax::*, *};

/// A type that can be joined with another type, producing their least upper bound.
/// T \/ T ?~~> T'
pub trait Lub<Rhs = Self> {
    type Ctx;
    type Out;
    fn lub(self, other: Rhs, tycker: &mut Tycker, ctx: Self::Ctx) -> Result<Self::Out>;
}

impl Lub for () {
    type Ctx = ();
    type Out = ();
    fn lub(self, (): Self, _tycker: &mut Tycker, (): Self::Ctx) -> Result<Self::Out> {
        Ok(())
    }
}

impl Lub for &KindId {
    type Ctx = ();
    type Out = KindId;

    fn lub(self, other: Self, tycker: &mut Tycker, ctx: Self::Ctx) -> Result<Self::Out> {
        let lhs = tycker.statics.kinds[self].clone();
        let rhs = tycker.statics.kinds[other].clone();
        match (lhs, rhs) {
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
                let kd_in = lin.lub(&rin, tycker, ctx)?;
                let kd_out = lout.lub(&rout, tycker, ctx)?;
                let kd = Alloc::alloc(tycker, Arrow(kd_in, kd_out));
                Ok(kd)
            }
            | (Kind::VType(_), _) | (Kind::CType(_), _) | (Kind::Arrow(_), _) => {
                Err(TyckError::KindMismatch)
            }
        }
    }
}

// struct Debruijn {
//     defs: im::HashMap<DefId, usize>,
// }
// impl Debruijn {
//     fn new() -> Self {
//         Self { defs: im::HashMap::new() }
//     }
//     fn extend_one(&self, def: DefId) -> Self {
//         let len = self.defs.len();
//         let mut defs = self.defs.clone();
//         let res = defs.insert(def, len);
//         assert!(res.is_none());
//         Self { defs }
//     }
//     fn lookup(&self, def: DefId) -> Option<usize> {
//         self.defs.get(&def).copied()
//     }
// }

pub struct Debruijn {
    lhs: Context<TypeId>,
    rhs: Context<TypeId>,
}
impl Debruijn {
    pub fn new() -> Self {
        Self { lhs: Context::new(), rhs: Context::new() }
    }
    pub fn extend_one(&self, tycker: &mut Tycker, lhs_def: DefId, rhs_def: DefId) -> Self {
        let mut lhs = self.lhs.clone();
        let mut rhs = self.rhs.clone();
        let abst = Alloc::alloc(tycker, Abstract);
        let sealed = Alloc::alloc(tycker, Sealed(abst));
        {
            let res = lhs.defs.insert(lhs_def, sealed);
            assert!(res.is_none());
        }
        {
            let res = rhs.defs.insert(rhs_def, sealed);
            assert!(res.is_none());
        }
        Self { lhs, rhs }
    }
    pub fn lookup(&self, lhs_def: DefId, rhs_def: DefId) -> Option<TypeId> {
        let lhs = self.lhs.defs.get(&lhs_def).copied();
        let rhs = self.rhs.defs.get(&rhs_def).copied();
        match (lhs, rhs) {
            | (Some(lhs), Some(rhs)) if lhs == rhs => Some(lhs),
            | _ => None,
        }
    }
}

impl Lub for &TypeId {
    /// We need to remember the definitions introduced by both sides.
    // Todo..
    type Ctx = Debruijn;
    type Out = TypeId;

    fn lub(self, other: Self, tycker: &mut Tycker, ctx: Self::Ctx) -> Result<Self::Out> {
        let lhs = tycker.statics.types[self].clone();
        let rhs = tycker.statics.types[other].clone();
        match (lhs, rhs) {
            | (Type::Sealed(_), Type::Sealed(_)) => {
                if self == other {
                    Ok(*self)
                } else {
                    Err(TyckError::TypeMismatch)?
                }
            }
            | (Type::Sealed(_), _) | (_, Type::Sealed(_)) => Err(TyckError::TypeMismatch)?,
            | (Type::Hole(_), _) => Ok(*other),
            | (Type::Var(_), Type::Var(_)) => todo!(),
            | (Type::Abs(_), Type::Abs(_)) => todo!(),
            | (Type::App(_), Type::App(_)) => todo!(),
            | (Type::Abst(_), _) | (_, Type::Abst(_)) => todo!(),
            | (Type::Thunk(_), Type::Thunk(_)) => todo!(),
            | (Type::Ret(_), Type::Ret(_)) => todo!(),
            | (Type::Unit(_), Type::Unit(_)) => todo!(),
            | (Type::Int(_), Type::Int(_)) => todo!(),
            | (Type::Char(_), Type::Char(_)) => todo!(),
            | (Type::String(_), Type::String(_)) => todo!(),
            | (Type::OS(_), Type::OS(_)) => todo!(),
            | (Type::Arrow(_), Type::Arrow(_)) => todo!(),
            | (Type::Forall(_), Type::Forall(_)) => todo!(),
            | (Type::Prod(_), Type::Prod(_)) => todo!(),
            | (Type::Exists(_), Type::Exists(_)) => todo!(),
            | (Type::Data(_), Type::Data(_)) => todo!(),
            | (Type::CoData(_), Type::CoData(_)) => todo!(),
            | _ => todo!(),
        }
    }
}
