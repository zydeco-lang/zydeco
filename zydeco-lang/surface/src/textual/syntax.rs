//! The surface syntax of zydeco.

pub use crate::{arena::*, syntax::*};
pub use zydeco_syntax::*;
pub use zydeco_utils::span::{LocationCtx, Sp, Span};

use derive_more::From;

/* ------------------------------- Identifier ------------------------------- */

new_key_type! {
    pub struct DefId;
    pub struct PatId;
    pub struct CoPatId;
    pub struct TermId;
    pub struct DeclId;
}
impl DefPtr for DefId {}
impl PatPtr for PatId {}
impl CoPatPtr for CoPatId {}
impl TermPtr for TermId {}
impl DeclPtr for DeclId {}

/// An entity dispatcher.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, From)]
pub enum EntityId {
    Def(DefId),
    Pat(PatId),
    CoPat(CoPatId),
    Term(TermId),
    Decl(DeclId),
}

/* --------------------------------- Pattern -------------------------------- */

#[derive(From, Clone, Debug)]
pub enum Pattern {
    Ann(Ann<PatId, TermId>),
    Hole(Hole),
    Var(DefId),
    Ctor(Ctor<PatId>),
    Paren(Paren<PatId>),
}

#[derive(From, Clone, Debug)]
pub enum CoPattern {
    Pat(PatId),
    Dtor(DtorName),
    App(Appli<CoPatId>),
}

/* ---------------------------------- Term ---------------------------------- */

/// general binding structure
#[derive(Clone, Debug)]
pub struct GenBind<Bindee> {
    pub rec: bool,
    pub comp: bool,
    pub binder: PatId,
    pub params: Option<CoPatId>,
    pub ty: Option<TermId>,
    pub bindee: Bindee,
}

/// `pi (x: A) -> B`
#[derive(Clone, Debug)]
pub struct Pi(pub CoPatId, pub TermId);
/// `forall (x: A) . B`
#[derive(Clone, Debug)]
pub struct Forall(pub CoPatId, pub TermId);

/// `sigma (x: A) . A'`
#[derive(Clone, Debug)]
pub struct Sigma(pub CoPatId, pub TermId);
/// `exists (x: A) . A'`
#[derive(Clone, Debug)]
pub struct Exists(pub CoPatId, pub TermId);

/// `let x = a in ...`
#[derive(Clone, Debug)]
pub struct GenPureBind {
    pub binding: GenBind<TermId>,
    pub tail: TermId,
}

/// `use let x = a in ...`
#[derive(Clone, Debug)]
pub struct UseBind {
    pub uses: UsePath,
    pub tail: TermId,
}

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

/// `match a | C_1 p -> b_1 | ... end`
#[derive(Clone, Debug)]
pub struct Match {
    pub scrut: TermId,
    pub arms: Vec<Matcher>,
}
#[derive(Clone, Debug)]
pub struct Matcher {
    pub binder: PatId,
    pub tail: TermId,
}

/// `codata | .d_1 cp : ty | ... end`
#[derive(Clone, Debug)]
pub struct CoData {
    pub arms: Vec<CoDataArm>,
}
#[derive(Clone, Debug)]
pub struct CoDataArm {
    pub name: DtorName,
    pub params: Option<CoPatId>,
    pub out: TermId,
}

/// `comatch | .d_1 -> b_1 | ... end`
#[derive(Clone, Debug)]
pub struct CoMatch {
    pub arms: Vec<CoMatcher>,
}
#[derive(Clone, Debug)]
pub struct CoMatcher {
    pub params: CoPatId,
    pub tail: TermId,
}

#[derive(From, Clone, Debug)]
pub enum Term {
    Ann(Ann<TermId, TermId>),
    Hole(Hole),
    Var(NameRef<VarName>),
    Paren(Paren<TermId>),
    Abs(Abs<CoPatId, TermId>),
    App(Appli<TermId>),
    Rec(Rec<PatId, TermId>),
    Pi(Pi),
    Forall(Forall),
    Arrow(Arrow<TermId>),
    Sigma(Sigma),
    Exists(Exists),
    Prod(Prod<TermId>),
    Thunk(Thunk<TermId>),
    Force(Force<TermId>),
    Ret(Ret<TermId>),
    Do(Bind<PatId, TermId, TermId>),
    Let(GenPureBind),
    // UseLet(UseBind),
    Data(Data),
    CoData(CoData),
    Ctor(Ctor<TermId>),
    Match(Match),
    CoMatch(CoMatch),
    Dtor(Dtor<TermId>),
    Lit(Literal),
}

/* -------------------------------- TopLevel -------------------------------- */

#[derive(Clone, Debug)]
pub struct DataDef {
    pub name: DefId,
    pub params: Vec<PatId>,
    pub def: Data,
}

#[derive(Clone, Debug)]
pub struct CoDataDef {
    pub name: DefId,
    pub params: Vec<PatId>,
    pub def: CoData,
}

#[derive(Clone, Debug)]
pub struct Define(pub GenBind<TermId>);

#[derive(Clone, Debug)]
pub struct Alias(pub GenBind<TermId>);

#[derive(Clone, Debug)]
pub struct Extern(pub GenBind<()>);

#[derive(Clone, Debug)]
pub struct Layer {
    pub name: Option<NameRef<VarName>>,
    pub uses: Vec<Modifiers<UsePath>>,
    pub top: TopLevel,
}

#[derive(From, Clone, Debug)]
pub struct UseDef(pub UsePath);

// #[derive(Clone, Debug)]
// pub struct UseBlock {
//     pub uses: UsePath,
//     pub top: TopLevel,
// }

#[derive(Clone, Debug)]
pub struct Main(pub TermId);

#[derive(Clone, From, Debug)]
pub enum Declaration {
    DataDef(DataDef),
    CoDataDef(CoDataDef),
    Define(Define),
    Alias(Alias),
    Extern(Extern),
    // Layer(Layer),
    // UseDef(UseDef),
    // UseBlock(UseBlock),
    Main(Main),
}

#[derive(From, Clone, Debug)]
pub enum ReplInput {
    Declaration(DeclId),
    Term(TermId),
}

#[derive(Clone, Debug)]
pub struct TopLevel(pub Vec<DeclId>);

/* ---------------------------------- Arena --------------------------------- */

#[derive(Default, Debug)]
pub struct Arena {
    pub defs: ArenaAssoc<DefId, VarName>,
    pub pats: ArenaAssoc<PatId, Pattern>,
    pub copats: ArenaAssoc<CoPatId, CoPattern>,
    pub terms: ArenaAssoc<TermId, Term>,
    pub decls: ArenaAssoc<DeclId, Modifiers<Declaration>>,
}

// Fixme: unify allocation for all entities
pub type SpanArena = ArenaGen<Span, DefId, PatId, CoPatId, TermId, DeclId>;

/* --------------------------------- Parser --------------------------------- */

pub struct Parser {
    pub spans: SpanArena,
    pub arena: Arena,
}

impl Parser {
    pub fn new(alloc: &mut GlobalAlloc) -> Self {
        Self { spans: SpanArena::new(alloc), arena: Arena::default() }
    }
    pub fn def(&mut self, def: Sp<VarName>) -> DefId {
        let id = self.spans.defs.alloc(def.info);
        self.arena.defs.insert(id, def.inner);
        id
    }
    pub fn pat(&mut self, pat: Sp<Pattern>) -> PatId {
        let id = self.spans.pats.alloc(pat.info);
        self.arena.pats.insert(id, pat.inner);
        id
    }
    pub fn copat(&mut self, copat: Sp<CoPattern>) -> CoPatId {
        let id = self.spans.copats.alloc(copat.info);
        self.arena.copats.insert(id, copat.inner);
        id
    }
    pub fn term(&mut self, term: Sp<Term>) -> TermId {
        let id = self.spans.terms.alloc(term.info);
        self.arena.terms.insert(id, term.inner);
        id
    }
    pub fn decl(&mut self, decl: Sp<Modifiers<Declaration>>) -> DeclId {
        let id = self.spans.decls.alloc(decl.info);
        self.arena.decls.insert(id, decl.inner);
        id
    }
}
