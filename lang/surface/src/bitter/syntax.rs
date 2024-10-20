//! Desugaring of the zydeco surface syntax.

pub use crate::syntax::*;
use crate::{arena::*, textual::syntax as t};
use derive_more::From;
use std::ops::AddAssign;
pub use zydeco_syntax::*;
use zydeco_utils::cells::MultiCell;
pub use zydeco_utils::span::{LocationCtx, Sp, Span};

/* ------------------------------- Identifier ------------------------------- */

new_key_type! {
    pub struct DefId;
    pub struct PatId;
    pub struct TermId;
    pub struct DeclId;
}

#[derive(From, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum EntityId {
    Def(DefId),
    Pat(PatId),
    Term(TermId),
    Decl(DeclId),
}

/* --------------------------------- Binder --------------------------------- */

pub use t::{NameDef, NameRef};

/* ----------------------------------- Use ---------------------------------- */

pub use t::{UseAlias, UseAll, UseEnum, UsePath, Uses};

/* --------------------------------- Pattern -------------------------------- */

#[derive(From, Clone, Debug)]
pub enum Pattern {
    Ann(Ann<PatId, TermId>),
    Hole(Hole),
    Var(DefId),
    Ctor(Ctor<PatId>),
    Triv(Triv),
    Cons(Cons<PatId, PatId>),
}

#[derive(From, Clone, Debug)]
pub enum CoPatternItem {
    Pat(PatId),
    Dtor(DtorName),
}

/* ---------------------------------- Term ---------------------------------- */

/// `pi (x: A) -> B`
#[derive(Clone, Debug)]
pub struct Pi(pub PatId, pub TermId);

/// `sigma (x: A) . A'`
#[derive(Clone, Debug)]
pub struct Sigma(pub PatId, pub TermId);

// /// `use let x = a in ...`
// #[derive(Clone, Debug)]
// pub struct UseBind {
//     pub uses: UsePath,
//     pub tail: TermId,
// }

/// data | C_1 ty | ... end
#[derive(Clone, Debug)]
pub struct Data {
    pub arms: Vec<DataArm>,
}
#[derive(Clone, Debug)]
pub struct DataArm {
    pub name: CtorName,
    pub param: TermId,
}

/// `codata | .d_1 cp : ty | ... end`
#[derive(Clone, Debug)]
pub struct CoData {
    pub arms: Vec<CoDataArm>,
}
#[derive(Clone, Debug)]
pub struct CoDataArm {
    pub name: DtorName,
    pub out: TermId,
}

/// `import f : B = V`
#[derive(Clone, Debug)]
pub struct Import {
    pub binder: PatId,
    pub ty: TermId,
    pub body: TermId,
}

/// `with mo inline x import f : B = V begin M end`
#[derive(Clone, Debug)]
pub struct WithBlock {
    pub structs: Vec<TermId>,
    pub inlines: Vec<(DefId, TermId)>,
    pub imports: Vec<Import>,
    pub body: TermId,
}

/// `with_mo mo begin M end`
#[derive(Clone, Debug)]
pub struct MBlock {
    pub mo: TermId,
    pub body: TermId,
}

/// `with_alg alg begin M end`
#[derive(Clone, Debug)]
pub struct WBlock {
    pub alg: TermId,
    pub body: TermId,
}

#[derive(From, Clone, Debug)]
pub enum Term<Ref> {
    Internal(Internal),
    Sealed(Sealed<TermId>),
    Ann(Ann<TermId, TermId>),
    Hole(Hole),
    #[from(ignore)]
    Var(Ref),
    Triv(Triv),
    Cons(Cons<TermId, TermId>),
    Abs(Abs<PatId, TermId>),
    App(App<TermId, TermId>),
    Fix(Fix<PatId, TermId>),
    Pi(Pi),
    // Arrow(Arrow),
    // Forall(Forall),
    Sigma(Sigma),
    // Prod(Prod),
    // Exists(Exists),
    Thunk(Thunk<TermId>),
    Force(Force<TermId>),
    Ret(Ret<TermId>),
    Do(Bind<PatId, TermId, TermId>),
    Let(PureBind<PatId, TermId, TermId>),
    // UseLet(UseBind),
    Data(Data),
    CoData(CoData),
    Ctor(Ctor<TermId>),
    Match(Match<TermId, PatId, TermId>),
    CoMatch(CoMatch<TermId>),
    Dtor(Dtor<TermId>),
    WithBlock(WithBlock),
    MBlock(MBlock),
    WBlock(WBlock),
    Lit(Literal),
}

/* -------------------------------- TopLevel -------------------------------- */

#[derive(Clone, Debug)]
pub struct AliasBody {
    pub binder: PatId,
    pub bindee: TermId,
}

#[derive(Clone, Debug)]
pub struct AliasHead {
    pub binder: PatId,
    pub ty: Option<TermId>,
}

#[derive(Clone, Debug)]
pub struct Module {
    pub name: Option<NameRef<VarName>>,
    pub top: TopLevel,
}

// #[derive(Clone, Debug)]
// pub struct Layer {
//     pub name: Option<NameRef<VarName>>,
//     pub uses: Vec<Modifiers<UsePath>>,
//     pub top: TopLevel,
// }

// #[derive(From, Clone, Debug)]
// pub struct UseDef(pub UsePath);

// #[derive(Clone, Debug)]
// pub struct UseBlock {
//     pub uses: UsePath,
//     pub top: TopLevel,
// }

#[derive(Clone, Debug)]
pub struct Exec(pub TermId);

#[derive(Clone, From, Debug)]
pub enum Declaration {
    AliasBody(AliasBody),
    AliasHead(AliasHead),
    Module(Module),
    // Layer(Layer),
    // UseDef(UseDef),
    // UseBlock(UseBlock),
    Exec(Exec),
}

#[derive(From, Clone, Debug)]
pub enum ReplInput {
    Declaration(Modifiers<Declaration>),
    Term(TermId),
}

#[derive(Clone, Debug)]
pub struct TopLevel(pub Vec<DeclId>);
impl AddAssign for TopLevel {
    fn add_assign(&mut self, rhs: TopLevel) {
        self.0.extend(rhs.0);
    }
}

/* ---------------------------------- Arena --------------------------------- */

#[derive(Clone, Debug, derive_more::AddAssign)]
pub struct Arena {
    // arenas
    pub defs: ArenaSparse<DefId, VarName>,
    pub pats: ArenaSparse<PatId, Pattern>,
    pub terms: ArenaSparse<TermId, Term<NameRef<VarName>>>,
    pub decls: ArenaSparse<DeclId, Modifiers<Declaration>>,

    /// entity maps from textural syntax
    pub textual: ArenaForth<t::EntityId, EntityId>,
}

impl Arena {
    pub fn new(alloc: &mut GlobalAlloc) -> Self {
        Arena {
            defs: ArenaSparse::new(alloc.alloc()),
            pats: ArenaSparse::new(alloc.alloc()),
            terms: ArenaSparse::new(alloc.alloc()),
            decls: ArenaSparse::new(alloc.alloc()),

            textual: ArenaForth::new(),
        }
    }
    pub fn new_arc(alloc: ArcGlobalAlloc) -> Self {
        Arena {
            defs: ArenaSparse::new(alloc.alloc()),
            pats: ArenaSparse::new(alloc.alloc()),
            terms: ArenaSparse::new(alloc.alloc()),
            decls: ArenaSparse::new(alloc.alloc()),

            textual: ArenaForth::new(),
        }
    }
}

/* -------------------------------- Primitive ------------------------------- */

/// Primitive Terms
///
/// Collects those terms that are `Internal` within the bitter syntax.
/// Will be used to guide the name resolution of those internal terms.
/// The whole point of this is to avoid references introduced by our desugaring
/// being captured by user name accidentally.
///
/// To add a new primitive term, add a field here and follow instructions at
/// [`crate::scoped::syntax::PrimDef`]
#[derive(Clone, Default, derive_more::AddAssign)]
pub struct PrimTerms {
    /// VType kind
    pub vtype: MultiCell<TermId>,
    /// CType kind
    pub ctype: MultiCell<TermId>,
    /// Thk type
    pub thk: MultiCell<TermId>,
    /// Ret type
    pub ret: MultiCell<TermId>,
    /// Unit type
    pub unit: MultiCell<TermId>,
    /// Int type
    pub int: MultiCell<TermId>,
    /// Char type
    pub char: MultiCell<TermId>,
    /// String type
    pub string: MultiCell<TermId>,
    /// Top type
    pub top: MultiCell<TermId>,
    /// OS type
    pub os: MultiCell<TermId>,
    /// Monad type
    pub monad: MultiCell<TermId>,
    /// Algebra type
    pub algebra: MultiCell<TermId>,
}
