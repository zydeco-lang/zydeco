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
    Hole,
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
    use crate::*;

    impl AnnId {
        pub fn as_type(self) -> TypeId {
            match self {
                | AnnId::Type(ty) => ty,
                | _ => unreachable!(),
            }
        }
    }

    impl PatAnnId {
        pub fn mk_hole(statics: &mut StaticsArena, ann: AnnId) -> Self {
            match ann {
                | ss::AnnId::Set => unreachable!(),
                | ss::AnnId::Kind(kd) => {
                    let tm = Alloc::alloc(statics, Ann { tm: Hole, ty: kd }, kd);
                    PatAnnId::Type(tm, kd)
                }
                | ss::AnnId::Type(ty) => {
                    let tm = Alloc::alloc(statics, Ann { tm: Hole, ty }, ty);
                    PatAnnId::Value(tm, ty)
                }
            }
        }
        pub fn mk_var(statics: &mut StaticsArena, def: DefId, ann: AnnId) -> Self {
            match ann {
                | ss::AnnId::Set => unreachable!(),
                | ss::AnnId::Kind(kd) => {
                    let tm = Alloc::alloc(statics, def, kd);
                    PatAnnId::Type(tm, kd)
                }
                | ss::AnnId::Type(ty) => {
                    let tm = Alloc::alloc(statics, def, ty);
                    PatAnnId::Value(tm, ty)
                }
            }
        }
    }

    impl TermAnnId {
        pub fn as_term_static(self) -> AnnId {
            match self {
                | TermAnnId::Kind(k) => AnnId::Kind(k),
                | TermAnnId::Type(t, _) => AnnId::Type(t),
                | TermAnnId::Hole | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
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
                | TermAnnId::Hole
                | TermAnnId::Type(_, _)
                | TermAnnId::Value(_, _)
                | TermAnnId::Compu(_, _) => tycker.err(err, blame),
            }
        }
        pub fn try_as_type(
            self, tycker: &mut Tycker, err: TyckError,
            blame: &'static std::panic::Location<'static>,
        ) -> ResultKont<(TypeId, KindId)> {
            match self {
                | TermAnnId::Type(ty, kd) => Ok((ty, kd)),
                | TermAnnId::Hole
                | TermAnnId::Kind(_)
                | TermAnnId::Value(_, _)
                | TermAnnId::Compu(_, _) => tycker.err(err, blame),
            }
        }
        pub fn try_as_value(
            self, tycker: &mut Tycker, err: TyckError,
            blame: &'static std::panic::Location<'static>,
        ) -> ResultKont<(ValueId, TypeId)> {
            match self {
                | TermAnnId::Value(val, ty) => Ok((val, ty)),
                | TermAnnId::Hole
                | TermAnnId::Kind(_)
                | TermAnnId::Type(_, _)
                | TermAnnId::Compu(_, _) => tycker.err(err, blame),
            }
        }
        pub fn try_as_compu(
            self, tycker: &mut Tycker, err: TyckError,
            blame: &'static std::panic::Location<'static>,
        ) -> ResultKont<(CompuId, TypeId)> {
            match self {
                | TermAnnId::Compu(com, ty) => Ok((com, ty)),
                | TermAnnId::Hole
                | TermAnnId::Kind(_)
                | TermAnnId::Type(_, _)
                | TermAnnId::Value(_, _) => tycker.err(err, blame),
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
    impl<T> Env<T>
    where
        T: Clone,
    {
        pub fn new() -> Self {
            Self { defs: im::HashMap::new() }
        }
        pub fn singleton(def: DefId, t: T) -> Self {
            let mut defs = im::HashMap::new();
            defs.insert(def, t);
            Self { defs }
        }
        pub fn get(&self, def: &DefId) -> Option<&T> {
            self.defs.get(def)
        }
        pub fn extended(&self, iter: impl IntoIterator<Item = (DefId, T)>) -> Self {
            let Env { mut defs } = self.clone();
            defs.extend(iter);
            Self { defs }
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
    impl<T> AddAssign<(DefId, T)> for Env<T>
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

/* ---------------------------------- Kind ---------------------------------- */

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct VType;
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CType;

#[derive(Debug, Clone, From, Hash, PartialEq, Eq)]
pub enum Kind {
    Fill(FillId),
    VType(VType),
    CType(CType),
    Arrow(Arrow<KindId>),
}

/* ---------------------------------- Type ---------------------------------- */

#[derive(From, Clone, Debug)]
pub enum TypePattern {
    // Ann(Ann<TPatId, KindId>),
    Hole(Ann<Hole, KindId>),
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
    Hole(Ann<Hole, TypeId>),
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
    VAbs(Abs<VPatId, CompuId>),
    VApp(App<CompuId, ValueId>),
    TAbs(Abs<TPatId, CompuId>),
    TApp(App<CompuId, TypeId>),
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
pub struct Exec(pub CompuId);

#[derive(Clone, From, Debug)]
pub enum Declaration {
    TAliasBody(TAliasBody),
    VAliasBody(VAliasBody),
    VAliasHead(VAliasHead),
    Exec(Exec),
}

/* ---------------------------------- Arena --------------------------------- */

/// Structurally shared arena for `data` and `codata` definitions.
#[derive(Debug)]
pub struct StructArena<Id, Definition, Query> {
    /// arena for definitions
    pub defs: ArenaDense<Id, Definition>,
    /// arena for hashmap
    pub tbls: ArenaAssoc<Id, Query>,
    /// arena for equivalence classes
    pub eqs: ArenaAssoc<Query, Id>,
}

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
    /// annotations for variable definitions
    pub annotations_var: ArenaAssoc<DefId, AnnId>,
    /// kind annotations for types
    pub annotations_type: ArenaAssoc<TypeId, KindId>,
    /// type annotations for values
    pub annotations_value: ArenaAssoc<ValueId, TypeId>,
    /// type annotations for computations
    pub annotations_compu: ArenaAssoc<CompuId, TypeId>,

    /// arena for abstract types
    pub absts: ArenaDense<AbstId, ()>,
    /// the abstract types generated from sealed types
    pub seals: ArenaAssoc<AbstId, TypeId>,
    /// arena for filling context-constrained holes; the TermId is the site
    pub fills: ArenaDense<FillId, su::TermId>,
    /// arena for the solutions of fillings
    pub solus: ArenaAssoc<FillId, AnnId>,
    /// arena for `data`
    pub datas: StructArena<DataId, im::Vector<(CtorName, TypeId)>, Data>,
    /// arena for `codata`
    pub codatas: StructArena<CoDataId, im::Vector<(DtorName, TypeId)>, CoData>,
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

            annotations_var: ArenaAssoc::new(),
            annotations_type: ArenaAssoc::new(),
            annotations_value: ArenaAssoc::new(),
            annotations_compu: ArenaAssoc::new(),

            absts: ArenaDense::new(alloc.alloc()),
            seals: ArenaAssoc::new(),
            fills: ArenaDense::new(alloc.alloc()),
            solus: ArenaAssoc::new(),
            datas: StructArena {
                defs: ArenaDense::new(alloc.alloc()),
                tbls: ArenaAssoc::new(),
                eqs: ArenaAssoc::new(),
            },
            codatas: StructArena {
                defs: ArenaDense::new(alloc.alloc()),
                tbls: ArenaAssoc::new(),
                eqs: ArenaAssoc::new(),
            },
        }
    }
}
