//! The surface syntax of zydeco is defined in this module.

use slotmap::SlotMap;
use zydeco_derive::IntoEnum;
use zydeco_utils::span::FileInfo;
pub use zydeco_utils::span::{Sp, Span};

/* --------------------------------- Binder --------------------------------- */

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct ModName(pub String);
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct VarName(pub String);
#[derive(Clone)]
pub struct CtorName(pub String);
#[derive(Clone)]
pub struct DtorName(pub String);
#[derive(IntoEnum, Clone)]
pub enum TypeArmName {
    Ctor(CtorName),
    Dtor(DtorName),
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct NameDef<T>(pub T);
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct NameRef<T>(pub Vec<ModName>, pub T);

/* ----------------------------- Bi-Directional ----------------------------- */

/// `(...: t)` for analyze mode motivator
#[derive(Clone)]
pub struct Annotation<Term, Type> {
    pub term: Term,
    pub ty: Type,
}
/// `_` for synthesize mode motivator
#[derive(Clone)]
pub struct Hole;

/* --------------------------------- Pattern -------------------------------- */

#[derive(IntoEnum, Clone)]
pub enum Pattern {
    Var(DefId),
    Ann(Annotation<PatternId, TermId>),
    Hole(Hole),
}

/* ---------------------------------- Term ---------------------------------- */

/// general binding structure
#[derive(Clone)]
pub struct GenBind {
    pub rec: bool,
    pub fun: bool,
    pub binder: PatternId,
    pub params: Vec<PatternId>,
    pub ty: Option<TermId>,
    pub bindee: Option<TermId>,
}

/// any binding structure
#[derive(Clone)]
pub struct Abstraction<Tail>(pub Vec<PatternId>, pub Tail);
/// any application
#[derive(Clone)]
pub struct Application(pub TermId, pub Vec<TermId>);
/// `rec (x: A) -> b`
#[derive(Clone)]
pub struct Recursion(pub PatternId, pub TermId);

// `pi (x: A) -> B`
#[derive(Clone)]
pub struct Pi(pub Vec<PatternId>, pub TermId);
// `a -> b`
#[derive(Clone)]
pub struct Arrow(pub TermId, pub TermId);
// `forall (x: A) . B`
#[derive(Clone)]
pub struct Forall(pub Vec<PatternId>, pub TermId);
// `exists (x: A) . B`
#[derive(Clone)]
pub struct Exists(pub Vec<PatternId>, pub TermId);

/// `{ b }` has type `Thunk B`
#[derive(Clone)]
pub struct Thunk(pub TermId);
/// `! a` has type `B` where `A = Thunk B`
#[derive(Clone)]
pub struct Force(pub TermId);

/// `ret a` has type `Ret A`
#[derive(Clone)]
pub struct Return(pub TermId);
/// `do x <- b; ...`
#[derive(Clone)]
pub struct Bind<Tail> {
    pub binder: PatternId,
    pub bindee: TermId,
    pub tail: Tail,
}
/// `let x = a in ...`
#[derive(Clone)]
pub struct PureBind<Tail> {
    pub binding: GenBind,
    pub tail: Tail,
}

/// `C(a_1, ...)`
#[derive(Clone)]
pub struct Constructor(pub NameRef<CtorName>, pub Vec<TermId>);
/// `match a | C_1(x_11, ...) -> b_1 | ...`
#[derive(Clone)]
pub struct Match<Tail> {
    pub scrut: TermId,
    pub arms: Vec<Matcher<Tail>>,
}
#[derive(Clone)]
pub struct Matcher<Tail> {
    pub name: NameRef<CtorName>,
    pub binders: Vec<PatternId>,
    pub tail: Tail,
}

/// `comatch | .d_1(x_11, ...) -> b_1 | ...`
#[derive(Clone)]
pub struct CoMatch<Tail> {
    pub arms: Vec<CoMatcher<Tail>>,
}
#[derive(Clone)]
pub struct CoMatcher<Tail> {
    pub name: NameRef<DtorName>,
    pub binders: Vec<PatternId>,
    pub tail: Tail,
}
/// `b .d(a_1, ...)`
#[derive(Clone)]
pub struct Destructor(pub TermId, pub NameRef<DtorName>, pub Vec<TermId>);

/// literals in term
#[derive(IntoEnum, Clone)]
pub enum Literal {
    Int(i64),
    String(String),
    Char(char),
}

#[derive(IntoEnum, Clone)]
pub enum Term<Ref> {
    Ann(Annotation<TermId, TermId>),
    Hole(Hole),
    #[skip]
    Var(Ref),
    Abs(Abstraction<TermId>),
    App(Application),
    Rec(Recursion),
    Pi(Pi),
    Arrow(Arrow),
    Forall(Forall),
    Exists(Exists),
    Thunk(Thunk),
    Force(Force),
    Ret(Return),
    Do(Bind<TermId>),
    Let(PureBind<TermId>),
    Ctor(Constructor),
    Match(Match<TermId>),
    CoMatch(CoMatch<TermId>),
    Dtor(Destructor),
    Lit(Literal),
}

/* -------------------------------- TopLevel -------------------------------- */

#[derive(Clone)]
pub enum TypeDefHead {
    Data,
    CoData,
}

#[derive(Clone)]
pub struct TypeDef {
    /// `data` or `codata`
    pub head: TypeDefHead,
    pub name: DefId,
    pub params: Vec<PatternId>,
    pub arms: Option<Vec<TypeArm>>,
}
#[derive(Clone)]
pub struct TypeArm {
    pub name: NameDef<TypeArmName>,
    pub args: Vec<TermId>,
    pub out: Option<TermId>,
}

#[derive(Clone)]
pub struct Define(pub GenBind);

pub struct Module {
    pub name: NameDef<ModName>,
    pub top: Option<TopLevel>,
}

pub struct UseAll;
pub struct UseAlias(pub VarName, pub VarName);
pub struct UseCluster(pub Vec<UseDef>);
#[derive(IntoEnum)]
pub enum UseEnum {
    Name(VarName),
    Alias(UseAlias),
    All(UseAll),
    Cluster(UseCluster),
}
pub struct UseDef(pub NameRef<UseEnum>);

pub struct Main(pub TermId);

#[derive(IntoEnum)]
pub enum Declaration {
    Type(TypeDef),
    Define(Define),
    Module(Module),
    UseDef(UseDef),
    Main(Main),
}

pub struct Modifiers<T> {
    pub public: bool,
    pub external: bool,
    pub inner: T,
}

#[derive(IntoEnum)]
pub enum ReplInput {
    Declaration(Modifiers<Declaration>),
    Term(TermId),
}

pub struct TopLevel(pub Vec<Modifiers<Declaration>>);

/* --------------------------------- Context -------------------------------- */

slotmap::new_key_type! {
    pub struct PatternId;
    pub struct TermId;
    pub struct DefId;
}

pub struct Context {
    // arenas
    pub patterns: SlotMap<PatternId, Sp<Pattern>>,
    pub terms: SlotMap<TermId, Sp<Term<NameRef<VarName>>>>,
    pub defs: SlotMap<DefId, Sp<VarName>>,
    // meta
    pub project: Option<String>,
    pub deps: Vec<String>,
}
impl Default for Context {
    fn default() -> Self {
        Self {
            patterns: Default::default(),
            terms: Default::default(),
            defs: Default::default(),
            project: Default::default(),
            deps: Default::default(),
        }
    }
}

impl Context {
    pub fn pattern(&mut self, pattern: Sp<Pattern>) -> PatternId {
        self.patterns.insert(pattern)
    }
    pub fn term(&mut self, term: Sp<Term<NameRef<VarName>>>) -> TermId {
        self.terms.insert(term)
    }
    pub fn def(&mut self, def: Sp<VarName>) -> DefId {
        self.defs.insert(def)
    }
    pub fn span_map(&mut self, file_info: &FileInfo) {
        for (_, pattern) in self.patterns.iter_mut() {
            pattern.info.set_info(file_info);
        }
        for (_, term) in self.terms.iter_mut() {
            term.info.set_info(file_info);
        }
    }
}
