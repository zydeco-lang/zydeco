//! The surface syntax of zydeco.

use derive_more::From;
use std::{fmt::Debug, ops::AddAssign};
pub use zydeco_syntax::*;
pub use zydeco_utils::{
    arena::*,
    new_key_type,
    span::{LocationCtx, Sp, Span},
};

/* --------------------------------- Binder --------------------------------- */

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NameDef<T>(pub T);
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NameRef<T>(pub Vec<ModName>, pub T);

/* --------------------------------- Pattern -------------------------------- */

#[derive(From, Clone, Debug)]
pub enum Pattern {
    Ann(Annotation<PatternId, TermId>),
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
    pub fun: bool,
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
/// `exists (x: A) . B`
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

/// `match a | C_1(x_11, ...) -> b_1 | ...`
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

/// `comatch | .d_1(x_11, ...) -> b_1 | ...`
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
    Ann(Annotation<TermId, TermId>),
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
    Exists(Exists),
    Thunk(Thunk),
    Force(Force),
    Ret(Return),
    Do(Bind<TermId>),
    Let(PureBind<TermId>),
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
    pub arms: Option<Vec<DataArm>>,
}
#[derive(Clone, Debug)]
pub struct DataArm {
    pub name: CtorName,
    pub param: TermId,
}

#[derive(Clone, Debug)]
pub struct CoDataDef {
    pub name: DefId,
    pub params: Vec<PatternId>,
    pub arms: Option<Vec<CoDataArm>>,
}
#[derive(Clone, Debug)]
pub struct CoDataArm {
    pub name: DtorName,
    pub params: Option<CoPatternId>,
    pub out: TermId,
}

#[derive(Clone, Debug)]
pub struct Define(pub GenBind<TermId>);

#[derive(Clone, Debug)]
pub struct Extern(pub GenBind<()>);

#[derive(Clone, Debug)]
pub struct Module {
    pub name: NameDef<ModName>,
    pub top: Option<TopLevel>,
}

#[derive(Clone, Debug)]
pub struct UseAll;
#[derive(Clone, Debug)]
pub struct UseAlias(pub VarName, pub VarName);
#[derive(Clone, Debug)]
pub struct UseCluster(pub Vec<UseDef>);
#[derive(Clone, From, Debug)]
pub enum UseEnum {
    Name(VarName),
    Alias(UseAlias),
    All(UseAll),
    Cluster(UseCluster),
}
#[derive(Clone, Debug)]
pub struct UseDef(pub NameRef<UseEnum>);

#[derive(Clone, Debug)]
pub struct Main(pub TermId);

#[derive(Clone, From, Debug)]
pub enum Declaration {
    DataDef(DataDef),
    CoDataDef(CoDataDef),
    Define(Define),
    Extern(Extern),
    Module(Module),
    UseDef(UseDef),
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

new_key_type! {
    pub struct DefId;
    pub struct PatternId;
    pub struct CoPatternId;
    pub struct TermId;
}

/// keeps all ids and spans, the corresponding source location
#[derive(Debug)]
pub struct SpanArena {
    pub defs: ArenaSparse<DefId, Span>,
    pub patterns: ArenaSparse<PatternId, Span>,
    pub co_patterns: ArenaSparse<CoPatternId, Span>,
    pub terms: ArenaSparse<TermId, Span>,
}

impl AddAssign<SpanArena> for SpanArena {
    fn add_assign(&mut self, rhs: SpanArena) {
        self.defs.extend(rhs.defs);
        self.patterns.extend(rhs.patterns);
        self.co_patterns.extend(rhs.co_patterns);
        self.terms.extend(rhs.terms);
    }
}

mod span_arena_impl {
    use super::*;
    impl std::ops::Index<DefId> for SpanArena {
        type Output = Span;

        fn index(&self, id: DefId) -> &Self::Output {
            self.defs.get(id).unwrap()
        }
    }

    impl std::ops::Index<PatternId> for SpanArena {
        type Output = Span;

        fn index(&self, id: PatternId) -> &Self::Output {
            self.patterns.get(id).unwrap()
        }
    }

    impl std::ops::Index<CoPatternId> for SpanArena {
        type Output = Span;

        fn index(&self, id: CoPatternId) -> &Self::Output {
            self.co_patterns.get(id).unwrap()
        }
    }

    impl std::ops::Index<TermId> for SpanArena {
        type Output = Span;

        fn index(&self, id: TermId) -> &Self::Output {
            self.terms.get(id).unwrap()
        }
    }
}

#[derive(Debug)]
pub struct Ctx {
    // span arena
    pub spans: SpanArena,
    // arenas
    pub defs: ArenaAssoc<DefId, VarName>,
    pub pats: ArenaAssoc<PatternId, Pattern>,
    pub copats: ArenaAssoc<CoPatternId, CoPattern>,
    pub terms: ArenaAssoc<TermId, Term<NameRef<VarName>>>,
}

impl Ctx {
    pub fn new(alloc: &mut GlobalAlloc) -> Self {
        Self {
            spans: SpanArena {
                defs: ArenaSparse::new(alloc.alloc()),
                patterns: ArenaSparse::new(alloc.alloc()),
                co_patterns: ArenaSparse::new(alloc.alloc()),
                terms: ArenaSparse::new(alloc.alloc()),
            },
            defs: ArenaAssoc::new(),
            pats: ArenaAssoc::new(),
            copats: ArenaAssoc::new(),
            terms: ArenaAssoc::new(),
        }
    }
    pub fn def(&mut self, def: Sp<VarName>) -> DefId {
        let id = self.spans.defs.alloc(def.info);
        self.defs.insert(id, def.inner);
        id
    }
    pub fn pat(&mut self, pattern: Sp<Pattern>) -> PatternId {
        let id = self.spans.patterns.alloc(pattern.info);
        self.pats.insert(id, pattern.inner);
        id
    }
    pub fn copat(&mut self, co_pattern: Sp<CoPattern>) -> CoPatternId {
        let id = self.spans.co_patterns.alloc(co_pattern.info);
        self.copats.insert(id, co_pattern.inner);
        id
    }
    pub fn term(&mut self, term: Sp<Term<NameRef<VarName>>>) -> TermId {
        let id = self.spans.terms.alloc(term.info);
        self.terms.insert(id, term.inner);
        id
    }
}

impl AddAssign<Ctx> for Ctx {
    fn add_assign(&mut self, rhs: Ctx) {
        self.defs += rhs.defs;
        self.pats += rhs.pats;
        self.copats += rhs.copats;
        self.terms += rhs.terms;
        self.spans += rhs.spans;
    }
}
