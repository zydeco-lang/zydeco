pub use zydeco_syntax::*;
pub use zydeco_utils::span::{LocationCtx, Sp, Span};

use crate::surface_syntax as sc;
use derive_more::From;
use indexmap::IndexMap;
use zydeco_utils::{arena::*, new_key_type};

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
/// A dispatcher for all entities.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, From)]
pub enum EntityId {
    Def(DefId),
    Kind(KindId),
    TPat(TPatId),
    Type(TypeId),
    VPat(VPatId),
    Value(ValueId),
    Compu(CompuId),
    Decl(DeclId),
}

mod impls_identifiers {
    use super::*;

    impl TermId {
        pub fn as_kind(self) -> KindId {
            match self {
                | TermId::Kind(k) => k,
                | _ => panic!("Expected a kind"),
            }
        }
        pub fn try_as_kind(self) -> Option<KindId> {
            match self {
                | TermId::Kind(k) => Some(k),
                | _ => None,
            }
        }
        pub fn as_type(self) -> TypeId {
            match self {
                | TermId::Type(t) => t,
                | _ => panic!("Expected a type"),
            }
        }
        pub fn try_as_type(self) -> Option<TypeId> {
            match self {
                | TermId::Type(t) => Some(t),
                | _ => None,
            }
        }
        pub fn as_value(self) -> ValueId {
            match self {
                | TermId::Value(v) => v,
                | _ => panic!("Expected a value"),
            }
        }
        pub fn try_as_value(self) -> Option<ValueId> {
            match self {
                | TermId::Value(v) => Some(v),
                | _ => None,
            }
        }
        pub fn as_compu(self) -> CompuId {
            match self {
                | TermId::Compu(c) => c,
                | _ => panic!("Expected a computation"),
            }
        }
        pub fn try_as_compu(self) -> Option<CompuId> {
            match self {
                | TermId::Compu(c) => Some(c),
                | _ => None,
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
        pub fn try_as_kind(self) -> Option<KindId> {
            match self {
                | AnnId::Kind(k) => Some(k),
                | _ => None,
            }
        }
        pub fn as_type(self) -> TypeId {
            match self {
                | AnnId::Type(t) => t,
                | _ => panic!("Expected a type"),
            }
        }
        pub fn try_as_type(self) -> Option<TypeId> {
            match self {
                | AnnId::Type(t) => Some(t),
                | _ => None,
            }
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
    Sealed(Sealed<TypeId>),
    Ann(Ann<TypeId, KindId>),
    Hole(Hole),
    Var(DefId),
    Abs(Abs<TPatId, TypeId>),
    App(App<TypeId, TypeId>),
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

/* ---------------------------------- Value --------------------------------- */

#[derive(From, Clone, Debug)]
pub enum ValuePattern {
    Ann(Ann<VPatId, sc::TermId>),
    Hole(Hole),
    Var(DefId),
    Ctor(Ctor<VPatId>),
    VCons(Cons<VPatId, VPatId>),
    TCons(Cons<TPatId, VPatId>),
}

#[derive(From, Clone, Debug)]
pub enum Value {
    Ann(Ann<ValueId, TypeId>),
    Hole(Hole),
    Var(DefId),
    VCons(Cons<ValueId, ValueId>),
    TCons(Cons<TypeId, ValueId>),
    Thunk(Thunk<CompuId>),
    Ctor(Ctor<ValueId>),
    Lit(Literal),
}

/* ------------------------------- Computation ------------------------------ */

/// `rec (x: A) -> b`
#[derive(Clone, Debug)]
pub struct Rec(pub VPatId, pub CompuId);

/// `ret a` has type `Ret A`
#[derive(Clone, Debug)]
pub struct Return(pub ValueId);
/// `do x <- b; ...`
#[derive(Clone, Debug)]
pub struct Bind {
    pub binder: VPatId,
    pub bindee: CompuId,
    pub tail: CompuId,
}
/// `let x = a in ...`
#[derive(Clone, Debug)]
pub struct PureBind {
    pub binder: VPatId,
    pub bindee: ValueId,
    pub tail: CompuId,
}

// /// `use let x = a in ...`
// #[derive(Clone, Debug)]
// pub struct UseBind {
//     pub uses: UsePath,
//     pub tail: sc::TermId,
// }

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
    Ann(Ann<CompuId, TypeId>),
    Hole(Hole),
    Abs(Abs<VPatId, CompuId>),
    App(App<CompuId, ValueId>),
    Rec(Rec),
    Force(Force<ValueId>),
    Ret(Return),
    Do(Bind),
    Let(PureBind),
    // UseLet(UseBind),
    Match(Match),
    CoMatch(CoMatch),
    Dtor(Dtor<ValueId>),
}

/* -------------------------------- TopLevel -------------------------------- */

#[derive(Clone, Debug)]
pub struct TAlias {
    pub binder: TPatId,
    pub bindee: TypeId,
}

#[derive(Clone, Debug)]
pub struct VAlias {
    pub binder: VPatId,
    pub bindee: ValueId,
}

#[derive(Clone, Debug)]
pub struct Extern {
    pub comp: bool,
    pub binder: VPatId,
    pub params: Option<VPatId>,
    pub ty: Option<sc::TermId>,
}

#[derive(Clone, Debug)]
pub struct Main(pub CompuId);

#[derive(Clone, From, Debug)]
pub enum Declaration {
    TAlias(TAlias),
    VAlias(VAlias),
    Extern(Extern),
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
    pub pats: ArenaBijective<sc::PatId, PatId>,
    pub terms: ArenaBijective<sc::TermId, TermId>,

    /// the type of defs; "context"
    pub type_of_defs: ArenaAssoc<DefId, AnnId>,
    /// the term of defs; "environment"; internal type defs won't inhabit here
    pub defs: ArenaAssoc<DefId, TermId>,
    /// the type of terms; "annotation"
    pub type_of_terms: ArenaAssoc<TermId, AnnId>,
    // Todo: equivalence-class type arena (or not)
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

            pats: ArenaBijective::new(),
            terms: ArenaBijective::new(),

            type_of_defs: ArenaAssoc::new(),
            defs: ArenaAssoc::new(),
            type_of_terms: ArenaAssoc::new(),
        }
    }
}
