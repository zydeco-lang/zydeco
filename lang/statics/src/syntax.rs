use std::collections::BTreeMap;

pub use super::arena::*;
pub use zydeco_syntax::*;
pub use zydeco_utils::span::{LocationCtx, Sp, Span};

use crate::surface_syntax as su;
use derive_more::From;
use zydeco_utils::new_key_type;

/* ------------------------------- Identifier ------------------------------- */

pub type DefId = su::DefId;
// PatId and TermId are unsorted, so we've got the following:
new_key_type! {
    pub struct KindId;
    pub struct TPatId;
    pub struct TypeId;
    pub struct VPatId;
    pub struct ValueId;
    pub struct CompuId;
}
// .. and here we have them defined as dispatchers
/// A dispatcher for all patterns.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, From)]
pub enum PatId {
    Type(TPatId),
    Value(VPatId),
}
// .. and here too
/// A dispatcher for all terms.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, From)]
pub enum TermId {
    Kind(KindId),
    Type(TypeId),
    Value(ValueId),
    Compu(CompuId),
}
// and here, a very useful dispatcher for all terms that can show up at annotation sites
/// A dispatcher for all annotations.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, From)]
pub enum AnnId {
    Set,
    Kind(KindId),
    Type(TypeId),
}
// and there are times when we need a proper pair of annotated things
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, From)]
pub enum PatAnnId {
    Type(TPatId, KindId),
    Value(VPatId, TypeId),
}
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, From)]
pub enum TermAnnId {
    Hole(FillId),
    Kind(KindId),
    Type(TypeId, KindId),
    Value(ValueId, TypeId),
    Compu(CompuId, TypeId),
}
/// The declaration identifiers are the same of the surface syntax.
pub type DeclId = su::DeclId;

new_key_type! {
    /// Identifier for abstract types, including:
    /// 1. sealed types, and
    /// 2. type instantiations for forall and exists.
    pub struct AbstId;
    /// Identifier for hole-filling targets with context constraints.
    pub struct FillId;
    /// Identifier for data definitions.
    pub struct DataId;
    /// Identifier for codata definitions.
    pub struct CoDataId;
}

mod impls_identifiers {
    use super::*;
    use crate::*;

    impl AnnId {
        pub fn as_type(self) -> TypeId {
            match self {
                | AnnId::Type(ty) => ty,
                | _ => unreachable!(),
            }
        }
        pub fn as_kind(self) -> KindId {
            match self {
                | AnnId::Kind(kd) => kd,
                | _ => unreachable!(),
            }
        }
    }

    impl PatAnnId {
        pub fn as_pat(self) -> PatId {
            match self {
                | PatAnnId::Type(pat, _) => PatId::Type(pat),
                | PatAnnId::Value(pat, _) => PatId::Value(pat),
            }
        }
        pub fn mk_hole(tycker: &mut Tycker, ann: AnnId) -> Self {
            match ann {
                | ss::AnnId::Set => unreachable!(),
                | ss::AnnId::Kind(kd) => {
                    let tm = Alloc::alloc(tycker, Hole, kd);
                    PatAnnId::Type(tm, kd)
                }
                | ss::AnnId::Type(ty) => {
                    let tm = Alloc::alloc(tycker, Hole, ty);
                    PatAnnId::Value(tm, ty)
                }
            }
        }
        pub fn mk_var(tycker: &mut Tycker, def: DefId, ann: AnnId) -> Self {
            match ann {
                | ss::AnnId::Set => unreachable!(),
                | ss::AnnId::Kind(kd) => {
                    let tm = Alloc::alloc(tycker, def, kd);
                    PatAnnId::Type(tm, kd)
                }
                | ss::AnnId::Type(ty) => {
                    let tm = Alloc::alloc(tycker, def, ty);
                    PatAnnId::Value(tm, ty)
                }
            }
        }
        pub fn as_type(self) -> (TPatId, KindId) {
            match self {
                | PatAnnId::Type(pat, kd) => (pat, kd),
                | PatAnnId::Value(_, _) => unreachable!(),
            }
        }
        pub fn as_value(self) -> (VPatId, TypeId) {
            match self {
                | PatAnnId::Value(pat, ty) => (pat, ty),
                | PatAnnId::Type(_, _) => unreachable!(),
            }
        }
        pub fn try_as_type(
            self, tycker: &mut Tycker, err: TyckError,
            blame: &'static std::panic::Location<'static>,
        ) -> ResultKont<(TPatId, KindId)> {
            match self {
                | PatAnnId::Type(pat, kd) => Ok((pat, kd)),
                | PatAnnId::Value(_, _) => tycker.err_k(err, blame),
            }
        }
        pub fn try_as_value(
            self, tycker: &mut Tycker, err: TyckError,
            blame: &'static std::panic::Location<'static>,
        ) -> ResultKont<(VPatId, TypeId)> {
            match self {
                | PatAnnId::Value(pat, ty) => Ok((pat, ty)),
                | PatAnnId::Type(_, _) => tycker.err_k(err, blame),
            }
        }
    }

    impl TermAnnId {
        pub fn as_term(self) -> Option<TermId> {
            let res = match self {
                | TermAnnId::Kind(k) => TermId::Kind(k),
                | TermAnnId::Type(t, _) => TermId::Type(t),
                | TermAnnId::Value(v, _) => TermId::Value(v),
                | TermAnnId::Compu(c, _) => TermId::Compu(c),
                | TermAnnId::Hole(_) => None?,
            };
            Some(res)
        }
        pub fn as_term_static(self) -> AnnId {
            match self {
                | TermAnnId::Kind(k) => AnnId::Kind(k),
                | TermAnnId::Type(t, _) => AnnId::Type(t),
                | TermAnnId::Hole(_) | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                    unreachable!()
                }
            }
        }
        pub fn try_as_kind(
            self, tycker: &mut Tycker, err: TyckError,
            blame: &'static std::panic::Location<'static>,
        ) -> ResultKont<KindId> {
            match self {
                | TermAnnId::Kind(kd) => Ok(kd),
                | TermAnnId::Hole(_)
                | TermAnnId::Type(_, _)
                | TermAnnId::Value(_, _)
                | TermAnnId::Compu(_, _) => tycker.err_k(err, blame),
            }
        }
        pub fn try_as_type(
            self, tycker: &mut Tycker, err: TyckError,
            blame: &'static std::panic::Location<'static>,
        ) -> ResultKont<(TypeId, KindId)> {
            match self {
                | TermAnnId::Type(ty, kd) => Ok((ty, kd)),
                | TermAnnId::Hole(_)
                | TermAnnId::Kind(_)
                | TermAnnId::Value(_, _)
                | TermAnnId::Compu(_, _) => tycker.err_k(err, blame),
            }
        }
        pub fn try_as_value(
            self, tycker: &mut Tycker, err: TyckError,
            blame: &'static std::panic::Location<'static>,
        ) -> ResultKont<(ValueId, TypeId)> {
            match self {
                | TermAnnId::Value(val, ty) => Ok((val, ty)),
                | TermAnnId::Hole(_)
                | TermAnnId::Kind(_)
                | TermAnnId::Type(_, _)
                | TermAnnId::Compu(_, _) => tycker.err_k(err, blame),
            }
        }
        pub fn try_as_compu(
            self, tycker: &mut Tycker, err: TyckError,
            blame: &'static std::panic::Location<'static>,
        ) -> ResultKont<(CompuId, TypeId)> {
            match self {
                | TermAnnId::Compu(com, ty) => Ok((com, ty)),
                | TermAnnId::Hole(_)
                | TermAnnId::Kind(_)
                | TermAnnId::Type(_, _)
                | TermAnnId::Value(_, _) => tycker.err_k(err, blame),
            }
        }
    }
}

/* --------------------------------- Context -------------------------------- */

pub use su::Context;

/* ------------------------------- Environment ------------------------------ */

#[derive(Clone, Debug)]
pub struct Env<T> {
    pub defs: im::HashMap<DefId, T>,
}

mod impls_env {
    use super::*;
    use std::ops::{Add, AddAssign, Index};

    impl<T> From<im::HashMap<DefId, T>> for Env<T> {
        fn from(defs: im::HashMap<DefId, T>) -> Self {
            Self { defs }
        }
    }

    impl<T> Into<im::HashMap<DefId, T>> for Env<T> {
        fn into(self) -> im::HashMap<DefId, T> {
            self.defs
        }
    }

    impl<T> AsRef<im::HashMap<DefId, T>> for Env<T> {
        fn as_ref(&self) -> &im::HashMap<DefId, T> {
            &self.defs
        }
    }

    impl<T> Add for Env<T>
    where
        T: Clone,
    {
        type Output = Self;
        fn add(self, other: Self) -> Self {
            let Env { mut defs } = self;
            defs.extend(other.defs);
            Self { defs }
        }
    }
    impl<T> Add<(DefId, T)> for Env<T>
    where
        T: Clone,
    {
        type Output = Self;
        fn add(self, (def, t): (DefId, T)) -> Self {
            let Env { mut defs } = self;
            defs.insert(def, t);
            Self { defs }
        }
    }
    impl<T> AddAssign<(DefId, T)> for Env<T>
    where
        T: Clone,
    {
        fn add_assign(&mut self, (def, t): (DefId, T)) {
            *self = self.clone() + (def, t);
        }
    }
    impl<T> Index<&DefId> for Env<T>
    where
        T: Clone,
    {
        type Output = T;
        fn index(&self, def: &DefId) -> &T {
            &self.defs[def]
        }
    }
}

/* -------------------------------- Fillable -------------------------------- */

#[derive(Clone, Debug, From, Hash, PartialEq, Eq)]
pub enum Fillable<T> {
    Fill(FillId),
    #[from(ignore)]
    Done(T),
}

/* ---------------------------------- Kind ---------------------------------- */

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct VType;
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CType;

#[derive(Clone, Debug, From, Hash, PartialEq, Eq)]
pub enum Kind {
    VType(VType),
    CType(CType),
    Arrow(ArrowU<KindId>),
}

/* ---------------------------------- Type ---------------------------------- */

#[derive(From, Clone, Debug)]
pub enum TypePattern {
    Hole(Hole),
    Var(DefId),
}

/// `U`
#[derive(Clone, Debug)]
pub struct ThkTy;

/// `F`
#[derive(Clone, Debug)]
pub struct RetTy;

/// `Unit`
#[derive(Clone, Debug)]
pub struct UnitTy;

/// `Int`
#[derive(Clone, Debug)]
pub struct IntTy;

/// `Char`
#[derive(Clone, Debug)]
pub struct CharTy;

/// `String`
#[derive(Clone, Debug)]
pub struct StringTy;

/// `OS`
#[derive(Clone, Debug)]
pub struct OSTy;

/// `pi (x: A) -> B`
#[derive(Clone, Debug)]
pub struct Forall(pub AbstId, pub TypeId);

/// `sigma (x: A) . A'`
#[derive(Clone, Debug)]
pub struct Exists(pub AbstId, pub TypeId);

/// data | C_1 ty | ... end
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Data {
    arms: BTreeMap<CtorName, TypeId>,
}

/// `codata | .d_1 cp : ty | ... end`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CoData {
    arms: BTreeMap<DtorName, TypeId>,
}

mod impls_structs {
    use super::*;

    impl Data {
        pub fn new(arms: impl Iterator<Item = (CtorName, TypeId)>) -> Self {
            Self { arms: arms.collect() }
        }
        pub fn get(&self, ctor: &CtorName) -> Option<&TypeId> {
            self.arms.get(ctor)
        }
        pub fn iter(&self) -> impl Iterator<Item = (&CtorName, &TypeId)> {
            self.into_iter()
        }
    }

    impl IntoIterator for Data {
        type Item = (CtorName, TypeId);
        type IntoIter = std::collections::btree_map::IntoIter<CtorName, TypeId>;
        fn into_iter(self) -> Self::IntoIter {
            self.arms.into_iter()
        }
    }

    impl<'a> IntoIterator for &'a Data {
        type Item = (&'a CtorName, &'a TypeId);
        type IntoIter = std::collections::btree_map::Iter<'a, CtorName, TypeId>;
        fn into_iter(self) -> Self::IntoIter {
            self.arms.iter()
        }
    }

    impl CoData {
        pub fn new(arms: impl Iterator<Item = (DtorName, TypeId)>) -> Self {
            Self { arms: arms.collect() }
        }
        pub fn get(&self, dtor: &DtorName) -> Option<&TypeId> {
            self.arms.get(dtor)
        }
        pub fn iter(&self) -> impl Iterator<Item = (&DtorName, &TypeId)> {
            self.into_iter()
        }
    }

    impl IntoIterator for CoData {
        type Item = (DtorName, TypeId);
        type IntoIter = std::collections::btree_map::IntoIter<DtorName, TypeId>;
        fn into_iter(self) -> Self::IntoIter {
            self.arms.into_iter()
        }
    }

    impl<'a> IntoIterator for &'a CoData {
        type Item = (&'a DtorName, &'a TypeId);
        type IntoIter = std::collections::btree_map::Iter<'a, DtorName, TypeId>;
        fn into_iter(self) -> Self::IntoIter {
            self.arms.iter()
        }
    }
}

#[derive(From, Clone, Debug)]
pub enum Type {
    Var(DefId),
    Abst(AbstId),
    Abs(Abs<TPatId, TypeId>),
    App(App<TypeId, TypeId>),
    Thk(ThkTy),
    Ret(RetTy),
    Unit(UnitTy),
    Int(IntTy),
    Char(CharTy),
    String(StringTy),
    OS(OSTy),
    Arrow(ArrowU<TypeId>),
    Forall(Forall),
    Prod(ProdU<TypeId>),
    Exists(Exists),
    Data(DataId),
    CoData(CoDataId),
}

mod impls_types {
    use super::*;
    // use crate::err::*;

    impl Type {}
}

/* ---------------------------------- Value --------------------------------- */

#[derive(From, Clone, Debug)]
pub enum ValuePattern {
    Hole(Hole),
    Var(DefId),
    Ctor(Ctor<VPatId>),
    Triv(Triv),
    VCons(Cons<VPatId, VPatId>),
    TCons(Cons<TPatId, VPatId>),
}

#[derive(From, Clone, Debug)]
pub enum Value {
    Hole(Hole),
    Var(DefId),
    Thunk(Thunk<CompuId>),
    Ctor(Ctor<ValueId>),
    Triv(Triv),
    VCons(Cons<ValueId, ValueId>),
    TCons(Cons<TypeId, ValueId>),
    Lit(Literal),
}

/* ------------------------------- Computation ------------------------------ */

#[derive(From, Clone, Debug)]
pub enum Computation {
    Hole(Hole),
    VAbs(Abs<VPatId, CompuId>),
    VApp(App<CompuId, ValueId>),
    TAbs(Abs<TPatId, CompuId>),
    TApp(App<CompuId, TypeId>),
    Fix(Fix<VPatId, CompuId>),
    Force(Force<ValueId>),
    Ret(Ret<ValueId>),
    Do(Bind<VPatId, CompuId, CompuId>),
    Let(PureBind<VPatId, ValueId, CompuId>),
    Match(Match<ValueId, VPatId, CompuId>),
    CoMatch(CoMatch<CompuId>),
    Dtor(Dtor<CompuId>),
    // WithBlock(WithBlock),
}

/* -------------------------------- TopLevel -------------------------------- */

pub struct SccDeclarations<'decl>(pub &'decl std::collections::HashSet<DeclId>);

#[derive(Clone, Debug)]
pub struct TAliasBody {
    pub binder: TPatId,
    pub bindee: TypeId,
}

#[derive(Clone, Debug)]
pub struct VAliasBody {
    pub binder: VPatId,
    pub bindee: ValueId,
}

#[derive(Clone, Debug)]
pub struct VAliasHead {
    pub binder: VPatId,
    pub ty: TypeId,
}

#[derive(Clone, Debug)]
pub struct Exec(pub CompuId);

#[derive(Clone, From, Debug)]
pub enum Declaration {
    TAliasBody(TAliasBody),
    VAliasBody(VAliasBody),
    VAliasHead(VAliasHead),
    Exec(Exec),
}
