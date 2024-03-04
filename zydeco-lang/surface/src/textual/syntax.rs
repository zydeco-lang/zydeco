//! The surface syntax of zydeco.

pub use crate::arena::*;
pub use zydeco_syntax::*;
pub use zydeco_utils::span::{LocationCtx, Sp, Span};

use derive_more::From;
use std::fmt::Debug;
use zydeco_utils::arena::*;

/* --------------------------------- Binder --------------------------------- */

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NameDef<T>(pub T);
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NameRef<T>(pub Vec<ModName>, pub T);

/* ----------------------------------- Use ---------------------------------- */

#[derive(Clone, Debug)]
pub struct UseAll;
#[derive(Clone, Debug)]
pub struct UseAlias(pub VarName, pub VarName);
#[derive(Clone, From, Debug)]
pub enum UseEnum {
    Name(VarName),
    Alias(UseAlias),
    All(UseAll),
    Cluster(Uses),
}
#[derive(Clone, Debug)]
pub struct UsePath(pub NameRef<UseEnum>);
#[derive(Clone, Debug)]
pub struct Uses(pub Vec<UsePath>);

/* --------------------------------- Pattern -------------------------------- */

#[derive(From, Clone, Debug)]
pub enum Pattern {
    Ann(Ann<PatternId, TermId>),
    Hole(Hole),
    Var(DefId),
    Ctor(Ctor<PatternId>),
    Paren(Paren<PatternId>),
}

#[derive(From, Clone, Debug)]
pub enum CoPattern {
    Pat(PatternId),
    Dtor(DtorName),
    App(App<CoPatternId>),
}

/* ---------------------------------- Term ---------------------------------- */

/// general binding structure
#[derive(Clone, Debug)]
pub struct GenBind<Bindee> {
    pub rec: bool,
    pub comp: bool,
    pub binder: PatternId,
    pub params: Option<CoPatternId>,
    pub ty: Option<TermId>,
    pub bindee: Bindee,
}

/// any binding structure
#[derive(Clone, Debug)]
pub struct Abs<Tail>(pub CoPatternId, pub Tail);
/// `rec (x: A) -> b`
#[derive(Clone, Debug)]
pub struct Rec(pub PatternId, pub TermId);

/// `pi (x: A) -> B`
#[derive(Clone, Debug)]
pub struct Pi(pub CoPatternId, pub TermId);
/// `a -> b`
#[derive(Clone, Debug)]
pub struct Arrow(pub TermId, pub TermId);
/// `forall (x: A) . B`
#[derive(Clone, Debug)]
pub struct Forall(pub CoPatternId, pub TermId);
/// `sigma (x: A) . A'`
#[derive(Clone, Debug)]
pub struct Sigma(pub CoPatternId, pub TermId);
/// `A * ...`
#[derive(Clone, Debug)]
pub struct Prod(pub Vec<TermId>);
/// `exists (x: A) . A'`
#[derive(Clone, Debug)]
pub struct Exists(pub CoPatternId, pub TermId);

/// `{ b }` has type `Thunk B`
#[derive(Clone, Debug)]
pub struct Thunk(pub TermId);
/// `! a` has type `B` where `A = Thunk B`
#[derive(Clone, Debug)]
pub struct Force(pub TermId);

/// `ret a` has type `Ret A`
#[derive(Clone, Debug)]
pub struct Return(pub TermId);
/// `do x <- b; ...`
#[derive(Clone, Debug)]
pub struct Bind<Tail> {
    pub binder: PatternId,
    pub bindee: TermId,
    pub tail: Tail,
}
/// `let x = a in ...`
#[derive(Clone, Debug)]
pub struct PureBind<Tail> {
    pub binding: GenBind<TermId>,
    pub tail: Tail,
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
pub struct Match<Tail> {
    pub scrut: TermId,
    pub arms: Vec<Matcher<Tail>>,
}
#[derive(Clone, Debug)]
pub struct Matcher<Tail> {
    pub binder: PatternId,
    pub tail: Tail,
}

/// `codata | .d_1 cp : ty | ... end`
#[derive(Clone, Debug)]
pub struct CoData {
    pub arms: Vec<CoDataArm>,
}
#[derive(Clone, Debug)]
pub struct CoDataArm {
    pub name: DtorName,
    pub params: Option<CoPatternId>,
    pub out: TermId,
}

/// `comatch | .d_1 -> b_1 | ... end`
#[derive(Clone, Debug)]
pub struct CoMatch<Tail> {
    pub arms: Vec<CoMatcher<Tail>>,
}
#[derive(Clone, Debug)]
pub struct CoMatcher<Tail> {
    pub params: CoPatternId,
    pub tail: Tail,
}

#[derive(From, Clone, Debug)]
pub enum Term<Ref> {
    Ann(Ann<TermId, TermId>),
    Hole(Hole),
    #[from(ignore)]
    Var(Ref),
    Paren(Paren<TermId>),
    Abs(Abs<TermId>),
    App(App<TermId>),
    Rec(Rec),
    Pi(Pi),
    Arrow(Arrow),
    Forall(Forall),
    Sigma(Sigma),
    Prod(Prod),
    Exists(Exists),
    Thunk(Thunk),
    Force(Force),
    Ret(Return),
    Do(Bind<TermId>),
    Let(PureBind<TermId>),
    UseLet(UseBind),
    Data(Data),
    CoData(CoData),
    Ctor(Ctor<TermId>),
    Match(Match<TermId>),
    CoMatch(CoMatch<TermId>),
    Dtor(Dtor<TermId>),
    Lit(Literal),
}

/* -------------------------------- TopLevel -------------------------------- */

#[derive(Clone, Debug)]
pub struct DataDef {
    pub name: DefId,
    pub params: Vec<PatternId>,
    pub def: Option<Data>,
}

#[derive(Clone, Debug)]
pub struct CoDataDef {
    pub name: DefId,
    pub params: Vec<PatternId>,
    pub def: Option<CoData>,
}

#[derive(Clone, Debug)]
pub struct Define(pub GenBind<TermId>);

#[derive(Clone, Debug)]
pub struct Alias(pub GenBind<TermId>);

#[derive(Clone, Debug)]
pub struct Extern(pub GenBind<()>);

#[derive(Clone, Debug)]
pub struct Module {
    pub name: ModName,
    pub top: Option<TopLevel>,
}

#[derive(From, Clone, Debug)]
pub struct UseDef(pub UsePath);

#[derive(Clone, Debug)]
pub struct UseBlock {
    pub uses: UsePath,
    pub top: TopLevel,
}

#[derive(Clone, Debug)]
pub struct Main(pub TermId);

#[derive(Clone, From, Debug)]
pub enum Declaration {
    DataDef(DataDef),
    CoDataDef(CoDataDef),
    Define(Define),
    Alias(Alias),
    Extern(Extern),
    Module(Module),
    UseDef(UseDef),
    UseBlock(UseBlock),
    Main(Main),
}

#[derive(Clone, Debug)]
pub struct Modifiers<T> {
    pub public: bool,
    pub inner: T,
}
impl<T> Modifiers<T> {
    pub fn try_map_ref<F, U, E>(&self, f: F) -> Result<Modifiers<U>, E>
    where
        F: FnOnce(&T) -> Result<U, E>,
    {
        let Modifiers { public, inner } = self;
        Ok(Modifiers { public: *public, inner: f(inner)? })
    }
}

#[derive(From, Clone, Debug)]
pub enum ReplInput {
    Declaration(Modifiers<Declaration>),
    Term(TermId),
}

#[derive(Clone, Debug)]
pub struct TopLevel(pub Vec<Modifiers<Declaration>>);

/* --------------------------------- Context -------------------------------- */

#[derive(Default, Debug)]
pub struct Ctx {
    // arenas
    pub defs: ArenaAssoc<DefId, VarName>,
    pub pats: ArenaAssoc<PatternId, Pattern>,
    pub copats: ArenaAssoc<CoPatternId, CoPattern>,
    pub terms: ArenaAssoc<TermId, Term<NameRef<VarName>>>,
}

pub struct Parser {
    pub spans: SpanArena,
    pub ctx: Ctx,
}

impl Parser {
    pub fn new(alloc: &mut GlobalAlloc) -> Self {
        Self { spans: SpanArena::new(alloc), ctx: Ctx::default() }
    }
    pub fn def(&mut self, def: Sp<VarName>) -> DefId {
        let id = self.spans.defs.alloc(def.info);
        self.ctx.defs.insert(id, def.inner);
        id
    }
    pub fn pat(&mut self, pat: Sp<Pattern>) -> PatternId {
        let id = self.spans.pats.alloc(pat.info);
        self.ctx.pats.insert(id, pat.inner);
        id
    }
    pub fn copat(&mut self, copat: Sp<CoPattern>) -> CoPatternId {
        let id = self.spans.copats.alloc(copat.info);
        self.ctx.copats.insert(id, copat.inner);
        id
    }
    pub fn term(&mut self, term: Sp<Term<NameRef<VarName>>>) -> TermId {
        let id = self.spans.terms.alloc(term.info);
        self.ctx.terms.insert(id, term.inner);
        id
    }
}
