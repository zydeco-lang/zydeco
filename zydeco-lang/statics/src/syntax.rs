pub use zydeco_syntax::*;
pub use zydeco_utils::span::{LocationCtx, Sp, Span};

use crate::surface_syntax as su;
use derive_more::From;
use zydeco_utils::{arena::*, new_key_type};

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
/// and here, a very useful dispatcher for all terms that can show up at annotation sites
/// A dispatcher for all annotations.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, From)]
pub enum AnnId {
    Set,
    Kind(KindId),
    Type(TypeId),
}
/// and there are times when we need a proper pair of annotated things
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, From)]
pub enum PatAnnId {
    Type(TPatId, KindId),
    Value(VPatId, TypeId),
}
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, From)]
pub enum TermAnnId {
    Kind(KindId),
    Type(TypeId, KindId),
    Value(ValueId, TypeId),
    Compu(CompuId, TypeId),
}
pub type DeclId = su::DeclId;
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

new_key_type! {
    /// Identifier for abstract types, including:
    /// 1. sealed types, and
    /// 2. type instantiations for forall and exists.
    pub struct AbstId;
    /// Identifier for hole-filling targets with context constraints.
    pub struct FillId;
    // Identifier for data and codata definitions.
    pub struct DataId;
    pub struct CoDataId;
}

mod impls_identifiers {
    use super::*;
    use crate::err::*;
    use crate::*;

    impl PatId {
        pub fn as_type_or_err(self, f: impl FnOnce() -> TyckError) -> Result<TPatId> {
            match self {
                | PatId::Type(t) => Ok(t),
                | _ => Err(f()),
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
        pub fn as_kind_or_err(self, f: impl FnOnce() -> TyckError) -> Result<KindId> {
            match self {
                | TermId::Kind(k) => Ok(k),
                | _ => Err(f()),
            }
        }
        pub fn as_type_or_err(self, f: impl FnOnce() -> TyckError) -> Result<TypeId> {
            match self {
                | TermId::Type(t) => Ok(t),
                | _ => Err(f()),
            }
        }
        pub fn as_ann_or_err(self, f: impl FnOnce() -> TyckError) -> Result<AnnId> {
            match self {
                | TermId::Type(t) => Ok(AnnId::Type(t)),
                | TermId::Kind(k) => Ok(AnnId::Kind(k)),
                | _ => Err(f()),
            }
        }
        pub fn as_value_or_err(self, f: impl FnOnce() -> TyckError) -> Result<ValueId> {
            match self {
                | TermId::Value(v) => Ok(v),
                | _ => Err(f()),
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

    impl PatAnnId {
        pub fn as_pat(self) -> PatId {
            match self {
                | PatAnnId::Type(t, _) => t.into(),
                | PatAnnId::Value(v, _) => v.into(),
            }
        }
        pub fn as_ann(self) -> AnnId {
            match self {
                | PatAnnId::Type(_, ann) => ann.into(),
                | PatAnnId::Value(_, ann) => ann.into(),
            }
        }
        pub fn mk_hole(tycker: &mut Tycker, ann: AnnId) -> Self {
            match ann {
                | ss::AnnId::Set => unreachable!(),
                | ss::AnnId::Kind(kd) => {
                    let tm = Alloc::alloc(tycker, Hole);
                    PatAnnId::Type(tm, kd)
                }
                | ss::AnnId::Type(ty) => {
                    let tm = Alloc::alloc(tycker, Hole);
                    PatAnnId::Value(tm, ty)
                }
            }
        }
        pub fn mk_var(tycker: &mut Tycker, def: DefId, ann: AnnId) -> Self {
            match ann {
                | ss::AnnId::Set => unreachable!(),
                | ss::AnnId::Kind(kd) => {
                    let tm = Alloc::alloc(tycker, def);
                    PatAnnId::Type(tm, kd)
                }
                | ss::AnnId::Type(ty) => {
                    let tm = Alloc::alloc(tycker, def);
                    PatAnnId::Value(tm, ty)
                }
            }
        }
    }

    impl TermAnnId {
        pub fn as_term(self) -> TermId {
            match self {
                | TermAnnId::Kind(k) => k.into(),
                | TermAnnId::Type(t, _) => t.into(),
                | TermAnnId::Value(v, _) => v.into(),
                | TermAnnId::Compu(c, _) => c.into(),
            }
        }
        pub fn as_term_static_or_err(self, f: impl FnOnce() -> TyckError) -> Result<AnnId> {
            match self.as_term() {
                | TermId::Kind(k) => Ok(AnnId::Kind(k)),
                | TermId::Type(t) => Ok(AnnId::Type(t)),
                | TermId::Value(_) => Err(f()),
                | TermId::Compu(_) => Err(f()),
            }
        }
        pub fn as_ann(self) -> AnnId {
            match self {
                | TermAnnId::Kind(_) => AnnId::Set,
                | TermAnnId::Type(_, ann) => ann.into(),
                | TermAnnId::Value(_, ann) => ann.into(),
                | TermAnnId::Compu(_, ann) => ann.into(),
            }
        }
    }
}

/* --------------------------------- Context -------------------------------- */

pub use su::Context;

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
    // Ann(Ann<TPatId, KindId>),
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
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Data {
    // Fixme: define correct behavior for hash and eq to deduplicate
    pub arms: im::HashMap<CtorName, TypeId>,
}

/// `codata | .d_1 cp : ty | ... end`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CoData {
    // Fixme: define correct behavior for hash and eq to deduplicate
    pub arms: im::HashMap<DtorName, TypeId>,
}

#[derive(From, Clone, Debug)]
pub enum Type {
    // Todo: remove it
    // Ann(Ann<TypeId, KindId>),
    // Hole(Hole),
    Var(DefId),
    Abst(AbstId),
    Fill(FillId),
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
    pub dtor: DtorName,
    pub tail: CompuId,
}

#[derive(From, Clone, Debug)]
pub enum Computation {
    Hole(Hole),
    Abs(Abs<VPatId, CompuId>),
    App(App<CompuId, ValueId>),
    Rec(Rec<VPatId, CompuId>),
    Force(Force<ValueId>),
    Ret(Ret<ValueId>),
    Do(Bind<VPatId, CompuId, CompuId>),
    Let(PureBind<VPatId, ValueId, CompuId>),
    Match(Match),
    CoMatch(CoMatch),
    Dtor(Dtor<CompuId>),
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
    pub pats: ArenaBack<su::PatId, PatId>,
    pub terms: ArenaBack<su::TermId, TermId>,

    // the type of terms under the context it's type checked; "annotation"
    /// kind annotations for types
    pub annotations_type: ArenaAssoc<TypeId, (Context<AnnId>, KindId)>,
    /// type annotations for values
    pub annotations_value: ArenaAssoc<ValueId, (Context<AnnId>, TypeId)>,
    /// type annotations for computations
    pub annotations_compu: ArenaAssoc<CompuId, (Context<AnnId>, TypeId)>,

    /// arena for abstract types
    pub absts: ArenaDense<AbstId, ()>,
    /// the abstract types generated from sealed
    pub seals: ArenaAssoc<AbstId, TypeId>,
    /// arena for filling context-constrained holes
    pub fills: ArenaDense<FillId, Context<AnnId>>,
    /// arena for the solutions of fillings
    pub solus: ArenaAssoc<FillId, TypeId>,
    /// arena for `data` definitions
    pub defs_data: ArenaDense<DataId, im::Vector<(CtorName, TypeId)>>,
    /// arena for `data` hashmap
    pub tbls_data: ArenaAssoc<DataId, Data>,
    /// arena for `data` equivalence classes
    pub eqs_data: ArenaAssoc<Data, DataId>,
    /// arena for `codata` definitions
    pub defs_codata: ArenaDense<CoDataId, im::Vector<(DtorName, TypeId)>>,
    /// arena for `codata` hashmap
    pub tbls_codata: ArenaAssoc<CoDataId, CoData>,
    /// arena for `codata` equivalence classes
    pub eqs_codata: ArenaAssoc<CoData, CoDataId>,
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

            annotations_type: ArenaAssoc::new(),
            annotations_value: ArenaAssoc::new(),
            annotations_compu: ArenaAssoc::new(),

            absts: ArenaDense::new(alloc.alloc()),
            seals: ArenaAssoc::new(),
            fills: ArenaDense::new(alloc.alloc()),
            solus: ArenaAssoc::new(),
            defs_data: ArenaDense::new(alloc.alloc()),
            tbls_data: ArenaAssoc::new(),
            eqs_data: ArenaAssoc::new(),
            defs_codata: ArenaDense::new(alloc.alloc()),
            tbls_codata: ArenaAssoc::new(),
            eqs_codata: ArenaAssoc::new(),
        }
    }
}
