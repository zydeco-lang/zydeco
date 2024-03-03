//! The surface syntax of zydeco is defined in this module.

use derive_more::From;
use slotmap::{SecondaryMap, SlotMap};
use std::fmt::Debug;
pub use zydeco_utils::span::{LocationCtx, Sp, Span};

/* --------------------------------- Binder --------------------------------- */

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ModName(pub String);
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct VarName(pub String);
#[derive(Clone, Debug)]
pub struct CtorName(pub String);
#[derive(Clone, Debug)]
pub struct DtorName(pub String);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NameDef<T>(pub T);
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NameRef<T>(pub Vec<ModName>, pub T);

/* ------------------------------- Structural ------------------------------- */

/// `(...)` as paren-shaped container
#[derive(From, Clone, Debug)]
pub struct Paren<T>(pub Vec<T>);

/// `e1 e2` shaped application
#[derive(Clone, Debug)]
pub struct App<T>(pub Vec<T>);

/// `(...: t)` for analyze mode motivator
#[derive(Clone, Debug)]
pub struct Annotation<Tm, Ty> {
    pub tm: Tm,
    pub ty: Ty,
}
/// `_` for synthesize mode motivator
#[derive(Clone, Debug)]
pub struct Hole;

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

/// `C(a_1, ...)`
#[derive(Clone, Debug)]
pub struct Ctor<Tail>(pub CtorName, pub Tail);
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
/// `b .d_i`
#[derive(Clone, Debug)]
pub struct Dtor<Head>(pub Head, pub DtorName);

/// literals in term
#[derive(From, Clone, Debug)]
pub enum Literal {
    Int(i64),
    String(String),
    Char(char),
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

slotmap::new_key_type! {
    pub struct DefId;
    pub struct PatternId;
    pub struct CoPatternId;
    pub struct TermId;
}

/// keeps all ids and spans, the corresponding source location
#[derive(Default, Clone, Debug)]
pub struct SpanArena {
    pub defs: SlotMap<DefId, Span>,
    pub patterns: SlotMap<PatternId, Span>,
    pub co_patterns: SlotMap<CoPatternId, Span>,
    pub terms: SlotMap<TermId, Span>,
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

#[derive(Default, Debug)]
pub struct Ctx {
    // span arena
    pub spans: SpanArena,
    // arenas
    pub defs: SecondaryMap<DefId, VarName>,
    pub patterns: SecondaryMap<PatternId, Pattern>,
    pub co_patterns: SecondaryMap<CoPatternId, CoPattern>,
    pub terms: SecondaryMap<TermId, Term<NameRef<VarName>>>,
}

impl Ctx {
    pub fn def(&mut self, def: Sp<VarName>) -> DefId {
        let id = self.spans.defs.insert(def.info);
        self.defs.insert(id, def.inner);
        id
    }
    pub fn pattern(&mut self, pattern: Sp<Pattern>) -> PatternId {
        let id = self.spans.patterns.insert(pattern.info);
        self.patterns.insert(id, pattern.inner);
        id
    }
    pub fn co_pattern(&mut self, co_pattern: Sp<CoPattern>) -> CoPatternId {
        let id = self.spans.co_patterns.insert(co_pattern.info);
        self.co_patterns.insert(id, co_pattern.inner);
        id
    }
    pub fn term(&mut self, term: Sp<Term<NameRef<VarName>>>) -> TermId {
        let id = self.spans.terms.insert(term.info);
        self.terms.insert(id, term.inner);
        id
    }

    pub fn merge(&mut self, other: &Ctx) {
        self.defs.extend(other.defs.clone());
        // There will be only one definition
        self.patterns.extend(other.patterns.clone());
        self.terms.extend(other.terms.clone());
        self.spans = other.spans.clone();
    }

    pub fn clear_added_id(&mut self) {}
}
