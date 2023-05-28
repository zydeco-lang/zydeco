//! The surface syntax of zydeco is defined in this module.

use derive_more::From;
use slotmap::SlotMap;
use zydeco_utils::span::FileInfo;
pub use zydeco_utils::span::{Sp, Span};

/* --------------------------------- Binder --------------------------------- */

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ModName(pub String);
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct VarName(pub String);
#[derive(Clone, Debug)]
pub struct CtorName(pub String);
#[derive(Clone, Debug)]
pub struct DtorName(pub String);
#[derive(From, Clone, Debug)]
pub enum TypeArmName {
    Ctor(CtorName),
    Dtor(DtorName),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NameDef<T>(pub T);
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NameRef<T>(pub Vec<ModName>, pub T);

/* ----------------------------- Bi-Directional ----------------------------- */

/// `(...: t)` for analyze mode motivator
#[derive(Clone, Debug)]
pub struct Annotation<Term, Type> {
    pub term: Term,
    pub ty: Type,
}
/// `_` for synthesize mode motivator
#[derive(Clone, Debug)]
pub struct Hole;

/* --------------------------------- Pattern -------------------------------- */

#[derive(From, Clone, Debug)]
pub enum Pattern {
    Var(DefId),
    Ann(Annotation<PatternId, TermId>),
    Hole(Hole),
}
impl Pattern {
    pub fn get_def_id(&self, ctx: &Ctx) -> Option<DefId> {
        match self {
            Pattern::Var(id) => Some(*id),
            Pattern::Ann(Annotation { term, ty: _ }) => ctx.patterns[*term].inner.get_def_id(ctx),
            Pattern::Hole(Hole) => None,
        }
    }
}

/* ---------------------------------- Term ---------------------------------- */

/// general binding structure
#[derive(Clone, Debug)]
pub struct GenBind {
    pub rec: bool,
    pub fun: bool,
    pub binder: PatternId,
    pub params: Vec<PatternId>,
    pub ty: Option<TermId>,
    pub bindee: Option<TermId>,
}

/// any binding structure
#[derive(Clone, Debug)]
pub struct Abstraction<Tail>(pub Vec<PatternId>, pub Tail);
/// any application
#[derive(Clone, Debug)]
pub struct Application(pub TermId, pub Vec<TermId>);
/// `rec (x: A) -> b`
#[derive(Clone, Debug)]
pub struct Recursion(pub PatternId, pub TermId);

// `pi (x: A) -> B`
#[derive(Clone, Debug)]
pub struct Pi(pub Vec<PatternId>, pub TermId);
// `a -> b`
#[derive(Clone, Debug)]
pub struct Arrow(pub TermId, pub TermId);
// `forall (x: A) . B`
#[derive(Clone, Debug)]
pub struct Forall(pub Vec<PatternId>, pub TermId);
// `exists (x: A) . B`
#[derive(Clone, Debug)]
pub struct Exists(pub Vec<PatternId>, pub TermId);

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
    pub binding: GenBind,
    pub tail: Tail,
}

/// `C(a_1, ...)`
#[derive(Clone, Debug)]
pub struct Constructor(pub CtorName, pub Vec<TermId>);
/// `match a | C_1(x_11, ...) -> b_1 | ...`
#[derive(Clone, Debug)]
pub struct Match<Tail> {
    pub scrut: TermId,
    pub arms: Vec<Matcher<Tail>>,
}
#[derive(Clone, Debug)]
pub struct Matcher<Tail> {
    pub name: CtorName,
    pub binders: Vec<PatternId>,
    pub tail: Tail,
}

/// `comatch | .d_1(x_11, ...) -> b_1 | ...`
#[derive(Clone, Debug)]
pub struct CoMatch<Tail> {
    pub arms: Vec<CoMatcher<Tail>>,
}
#[derive(Clone, Debug)]
pub struct CoMatcher<Tail> {
    pub name: DtorName,
    pub binders: Vec<PatternId>,
    pub tail: Tail,
}
/// `b .d(a_1, ...)`
#[derive(Clone, Debug)]
pub struct Destructor(pub TermId, pub DtorName, pub Vec<TermId>);

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

#[derive(Clone, Debug)]
pub enum TypeDefHead {
    Data,
    CoData,
}

#[derive(Clone, Debug)]
pub struct TypeDef {
    /// `data` or `codata`
    pub head: TypeDefHead,
    pub name: DefId,
    pub params: Vec<PatternId>,
    pub arms: Option<Vec<TypeArm>>,
}
#[derive(Clone, Debug)]
pub struct TypeArm {
    pub name: TypeArmName,
    pub args: Vec<TermId>,
    pub out: Option<TermId>,
}

#[derive(Clone, Debug)]
pub struct Define(pub GenBind);

pub struct Module {
    pub name: NameDef<ModName>,
    pub top: Option<TopLevel>,
}

pub struct UseAll;
pub struct UseAlias(pub VarName, pub VarName);
pub struct UseCluster(pub Vec<UseDef>);
#[derive(From)]
pub enum UseEnum {
    Name(VarName),
    Alias(UseAlias),
    All(UseAll),
    Cluster(UseCluster),
}
pub struct UseDef(pub NameRef<UseEnum>);

pub struct Main(pub TermId);

#[derive(From)]
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

#[derive(From)]
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

#[derive(Default)]
pub struct Ctx {
    // arenas
    pub patterns: SlotMap<PatternId, Sp<Pattern>>,
    pub terms: SlotMap<TermId, Sp<Term<NameRef<VarName>>>>,
    pub defs: SlotMap<DefId, Sp<VarName>>,
    // meta
    pub project: Option<String>,
    pub deps: Vec<String>,
    pub mod_decls: Vec<Vec<String>>,
    // temp
    pub mod_stack: Vec<String>,
}

impl Ctx {
    pub fn pattern(&mut self, pattern: Sp<Pattern>) -> PatternId {
        self.patterns.insert(pattern)
    }
    pub fn term(&mut self, term: Sp<Term<NameRef<VarName>>>) -> TermId {
        self.terms.insert(term)
    }
    pub fn def(&mut self, def: Sp<VarName>) -> DefId {
        self.defs.insert(def)
    }

    pub fn enter_mod(&mut self, mod_name: NameDef<ModName>) -> NameDef<ModName> {
        let NameDef(ModName(name)) = mod_name.clone();
        self.mod_stack.push(name);
        mod_name
    }
    pub fn mod_decl(&mut self, mod_name: NameDef<ModName>) {
        let NameDef(ModName(name)) = mod_name;
        let mut stack = self.mod_stack.clone();
        stack.push(name);
        self.mod_decls.push(stack);
    }
    pub fn exit_mod(&mut self) {
        self.mod_stack.pop();
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
