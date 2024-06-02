pub use zydeco_syntax::*;
pub use zydeco_utils::span::{LocationCtx, Sp, Span};

use crate::surface_syntax as sc;
use derive_more::From;
use indexmap::IndexMap;
use zydeco_utils::{arena::*, new_key_type};

/* ------------------------------- Identifier ------------------------------- */

pub type DefId = sc::DefId;
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
/// and here, a very useful dispatcher for all terms that can show up at annotation sites
/// A dispatcher for all annotations.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, From)]
pub enum AnnId {
    Kind(KindId),
    Type(TypeId),
}
pub type DeclId = sc::DeclId;
// /// A dispatcher for all entities.
// #[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, From)]
// pub enum EntityId {
//     Def(DefId),
//     Kind(KindId),
//     TPat(TPatId),
//     Type(TypeId),
//     VPat(VPatId),
//     Value(ValueId),
//     Compu(CompuId),
//     Decl(DeclId),
// }

mod impls_identifiers {
    use super::*;
    use crate::err::*;

    impl PatId {
        pub fn as_type(self) -> TPatId {
            match self {
                | PatId::Type(t) => t,
                | _ => panic!("Expected a type pattern"),
            }
        }
        pub fn as_type_or_err(self, f: impl FnOnce() -> TyckError) -> Result<TPatId> {
            match self {
                | PatId::Type(t) => Ok(t),
                | _ => Err(f()),
            }
        }
        pub fn as_value(self) -> VPatId {
            match self {
                | PatId::Value(v) => v,
                | _ => panic!("Expected a value pattern"),
            }
        }
        pub fn as_value_or_err(self, f: impl FnOnce() -> TyckError) -> Result<VPatId> {
            match self {
                | PatId::Value(v) => Ok(v),
                | _ => Err(f()),
            }
        }
    }

    impl TermId {
        pub fn as_kind(self) -> KindId {
            match self {
                | TermId::Kind(k) => k,
                | _ => panic!("Expected a kind"),
            }
        }
        pub fn as_kind_or_err(self, f: impl FnOnce() -> TyckError) -> Result<KindId> {
            match self {
                | TermId::Kind(k) => Ok(k),
                | _ => Err(f()),
            }
        }
        pub fn as_type(self) -> TypeId {
            match self {
                | TermId::Type(t) => t,
                | _ => panic!("Expected a type"),
            }
        }
        pub fn as_type_or_err(self, f: impl FnOnce() -> TyckError) -> Result<TypeId> {
            match self {
                | TermId::Type(t) => Ok(t),
                | _ => Err(f()),
            }
        }
        pub fn as_ann(self) -> AnnId {
            match self {
                | TermId::Type(t) => AnnId::Type(t),
                | TermId::Kind(k) => AnnId::Kind(k),
                | _ => panic!("Expected an annotation"),
            }
        }
        pub fn as_ann_or_err(self, f: impl FnOnce() -> TyckError) -> Result<AnnId> {
            match self {
                | TermId::Type(t) => Ok(AnnId::Type(t)),
                | TermId::Kind(k) => Ok(AnnId::Kind(k)),
                | _ => Err(f()),
            }
        }
        pub fn as_value(self) -> ValueId {
            match self {
                | TermId::Value(v) => v,
                | _ => panic!("Expected a value"),
            }
        }
        pub fn as_value_or_err(self, f: impl FnOnce() -> TyckError) -> Result<ValueId> {
            match self {
                | TermId::Value(v) => Ok(v),
                | _ => Err(f()),
            }
        }
        pub fn as_compu(self) -> CompuId {
            match self {
                | TermId::Compu(c) => c,
                | _ => panic!("Expected a computation"),
            }
        }
        pub fn as_compu_or_err(self, f: impl FnOnce() -> TyckError) -> Result<CompuId> {
            match self {
                | TermId::Compu(c) => Ok(c),
                | _ => Err(f()),
            }
        }
    }

    impl AnnId {
        pub fn as_kind(self) -> KindId {
            match self {
                | AnnId::Kind(k) => k,
                | _ => panic!("Expected a kind"),
            }
        }
        pub fn as_kind_or_err(self, f: impl FnOnce() -> TyckError) -> Result<KindId> {
            match self {
                | AnnId::Kind(k) => Ok(k),
                | _ => Err(f()),
            }
        }
        pub fn as_type(self) -> TypeId {
            match self {
                | AnnId::Type(t) => t,
                | _ => panic!("Expected a type"),
            }
        }
        pub fn as_type_or_err(self, f: impl FnOnce() -> TyckError) -> Result<TypeId> {
            match self {
                | AnnId::Type(t) => Ok(t),
                | _ => Err(f()),
            }
        }
    }
}

/* --------------------------------- Context -------------------------------- */

#[derive(Clone, Debug)]
pub struct Context<T> {
    pub defs: im::HashMap<DefId, T>,
}

#[derive(Clone, Debug)]
pub struct CtxItem {
    pub out: Option<TermId>,
    pub ann: AnnId,
}

/// def, out, ann
#[derive(Clone, Debug)]
pub struct CtxExtend(pub DefId, pub Option<TermId>, pub AnnId);

mod impls_context {
    use super::*;
    use std::ops::{Add, AddAssign, Index};
    impl<T> Context<T>
    where
        T: Clone,
    {
        pub fn new() -> Self {
            Self { defs: im::HashMap::new() }
        }
        pub fn extended(&self, iter: impl IntoIterator<Item = (DefId, T)>) -> Self {
            let Context { mut defs } = self.clone();
            defs.extend(iter);
            Self { defs }
        }
    }
    impl<T> Add for Context<T>
    where
        T: Clone,
    {
        type Output = Self;
        fn add(self, other: Self) -> Self {
            let Context { mut defs } = self;
            defs.extend(other.defs);
            Self { defs }
        }
    }
    impl<T> AddAssign<(DefId, T)> for Context<T>
    where
        T: Clone,
    {
        fn add_assign(&mut self, (def, t): (DefId, T)) {
            let Self { defs } = self;
            let mut defs = defs.clone();
            defs.insert(def, t);
            *self = Self { defs };
        }
    }
    impl AddAssign<CtxExtend> for Context<CtxItem> {
        fn add_assign(&mut self, CtxExtend(def, out, ann): CtxExtend) {
            let Self { defs } = self;
            let mut defs = defs.clone();
            defs.insert(def, CtxItem { out, ann });
            *self = Self { defs };
        }
    }
    impl<T> Index<&DefId> for Context<T>
    where
        T: Clone,
    {
        type Output = T;
        fn index(&self, def: &DefId) -> &T {
            &self.defs[def]
        }
    }

    impl CtxExtend {
        pub fn out_ann(def: DefId, out: TermId, ann: AnnId) -> Self {
            Self(def, Some(out), ann)
        }
    }
}

/* ---------------------------------- Kind ---------------------------------- */

#[derive(Clone, Debug)]
pub struct VType;
#[derive(Clone, Debug)]
pub struct CType;

#[derive(Debug, Clone, From)]
pub enum Kind {
    VType(VType),
    CType(CType),
    Arrow(Arrow<KindId>),
}

/* ---------------------------------- Type ---------------------------------- */

#[derive(From, Clone, Debug)]
pub enum TypePattern {
    Ann(Ann<TPatId, KindId>),
    Hole(Hole),
    Var(DefId),
}

/// an abstract type
#[derive(Clone, Debug)]
pub struct Abstract;

/// `U`
#[derive(Clone, Debug)]
pub struct ThunkTy;

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
pub struct Forall(pub TPatId, pub TypeId);

/// `sigma (x: A) . A'`
#[derive(Clone, Debug)]
pub struct Exists(pub TPatId, pub TypeId);

/// data | C_1 ty | ... end
#[derive(Clone, Debug)]
pub struct Data {
    pub arms: IndexMap<CtorName, TypeId>,
}

/// `codata | .d_1 cp : ty | ... end`
#[derive(Clone, Debug)]
pub struct CoData {
    pub arms: IndexMap<DtorName, TypeId>,
}

#[derive(From, Clone, Debug)]
pub enum Type {
    // Todo: remove it
    Sealed(Sealed<TypeId>),
    Hole(Hole),
    Var(DefId),
    Abs(Abs<TPatId, TypeId>),
    App(App<TypeId, TypeId>),
    Abst(Abstract),
    Thunk(ThunkTy),
    Ret(RetTy),
    Unit(UnitTy),
    Int(IntTy),
    Char(CharTy),
    String(StringTy),
    OS(OSTy),
    Arrow(Arrow<TypeId>),
    Forall(Forall),
    Prod(Prod<TypeId>),
    Exists(Exists),
    Data(Data),
    CoData(CoData),
}

mod impls_types {
    use super::*;
    use crate::err::*;

    impl Type {
        pub fn as_data_or_err(&self, f: impl FnOnce() -> TyckError) -> Result<&Data> {
            match self {
                | Type::Data(data) => Ok(data),
                | _ => Err(f()),
            }
        }
        pub fn as_codata_or_err(&self, f: impl FnOnce() -> TyckError) -> Result<&CoData> {
            match self {
                | Type::CoData(codata) => Ok(codata),
                | _ => Err(f()),
            }
        }
    }
}

/* ---------------------------------- Value --------------------------------- */

#[derive(From, Clone, Debug)]
pub enum ValuePattern {
    // Ann(Ann<VPatId, TypeId>),
    Hole(Hole),
    Var(DefId),
    Ctor(Ctor<VPatId>),
    Triv(Triv),
    VCons(Cons<VPatId, VPatId>),
    TCons(Cons<TPatId, VPatId>),
}

#[derive(From, Clone, Debug)]
pub enum Value {
    // Ann(Ann<ValueId, TypeId>),
    // Hole(Hole),
    Var(DefId),
    Thunk(Thunk<CompuId>),
    Ctor(Ctor<ValueId>),
    Triv(Triv),
    VCons(Cons<ValueId, ValueId>),
    TCons(Cons<TypeId, ValueId>),
    Lit(Literal),
}

/* ------------------------------- Computation ------------------------------ */

/// `match a | C_1 p -> b_1 | ... end`
#[derive(Clone, Debug)]
pub struct Match {
    pub scrut: ValueId,
    pub arms: Vec<Matcher>,
}
#[derive(Clone, Debug)]
pub struct Matcher {
    pub binder: VPatId,
    pub tail: CompuId,
}

/// `comatch | .d_1 -> b_1 | ... end`
#[derive(Clone, Debug)]
pub struct CoMatch {
    pub arms: Vec<CoMatcher>,
}
#[derive(Clone, Debug)]
pub struct CoMatcher {
    pub params: VPatId,
    pub tail: CompuId,
}

#[derive(From, Clone, Debug)]
pub enum Computation {
    // Ann(Ann<CompuId, TypeId>),
    // Hole(Hole),
    Abs(Abs<VPatId, CompuId>),
    App(App<CompuId, ValueId>),
    Rec(Rec<VPatId, CompuId>),
    Force(Force<ValueId>),
    Ret(Ret<ValueId>),
    Do(Bind<VPatId, CompuId, CompuId>),
    Let(PureBind<VPatId, ValueId, CompuId>),
    // UseLet(UseBind),
    Match(Match),
    CoMatch(CoMatch),
    Dtor(Dtor<ValueId>),
}

/* -------------------------------- TopLevel -------------------------------- */

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
pub struct Main(pub CompuId);

#[derive(Clone, From, Debug)]
pub enum Declaration {
    TAliasBody(TAliasBody),
    VAliasBody(VAliasBody),
    VAliasHead(VAliasHead),
    Main(Main),
}

/* ---------------------------------- Arena --------------------------------- */

#[derive(Debug)]
pub struct StaticsArena {
    // arenas
    pub kinds: ArenaSparse<KindId, Kind>,
    pub tpats: ArenaSparse<TPatId, TypePattern>,
    pub types: ArenaSparse<TypeId, Type>,
    pub vpats: ArenaSparse<VPatId, ValuePattern>,
    pub values: ArenaSparse<ValueId, Value>,
    pub compus: ArenaSparse<CompuId, Computation>,
    pub decls: ArenaAssoc<DeclId, Declaration>,

    // unsorted to sorted bijective maps
    pub pats: ArenaBack<sc::PatId, PatId>,
    pub terms: ArenaBack<sc::TermId, TermId>,

    /// the type of terms under the context it's type checked; "annotation"
    pub type_of_terms_under_ctx: ArenaAssoc<TermId, (Context<CtxItem>, AnnId)>,
    // Todo: equivalence-class type arena (or not)
    // Todo: hole arena for pats and terms
}

impl StaticsArena {
    pub fn new(alloc: &mut GlobalAlloc) -> Self {
        Self {
            kinds: ArenaSparse::new(alloc.alloc()),
            tpats: ArenaSparse::new(alloc.alloc()),
            types: ArenaSparse::new(alloc.alloc()),
            vpats: ArenaSparse::new(alloc.alloc()),
            values: ArenaSparse::new(alloc.alloc()),
            compus: ArenaSparse::new(alloc.alloc()),
            decls: ArenaAssoc::new(),

            pats: ArenaBack::new(),
            terms: ArenaBack::new(),

            type_of_terms_under_ctx: ArenaAssoc::new(),
        }
    }
}
