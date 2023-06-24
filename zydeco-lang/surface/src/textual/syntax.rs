//! The surface syntax of zydeco is defined in this module.

use derive_more::From;
use slotmap::{SecondaryMap, SlotMap};
use std::path::PathBuf;
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
            Pattern::Ann(Annotation { term, ty: _ }) => ctx.patterns[*term].get_def_id(ctx),
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

#[derive(Clone)]
pub struct UseAll;
#[derive(Clone)]
pub struct UseAlias(pub VarName, pub VarName);
#[derive(Clone)]
pub struct UseCluster(pub Vec<UseDef>);
#[derive(Clone, From)]
pub enum UseEnum {
    Name(VarName),
    Alias(UseAlias),
    All(UseAll),
    Cluster(UseCluster),
}
#[derive(Clone)]
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
impl<T> Modifiers<T> {
    pub fn try_map_ref<F, U, E>(&self, f: F) -> Result<Modifiers<U>, E>
    where
        F: FnOnce(&T) -> Result<U, E>,
    {
        let Modifiers { public, external, inner } = self;
        Ok(Modifiers { public: *public, external: *external, inner: f(inner)? })
    }
}

#[derive(From)]
pub enum ReplInput {
    Declaration(Modifiers<Declaration>),
    Term(TermId),
}

pub struct TopLevel(pub Vec<Modifiers<Declaration>>);

/* ------------------------------- Dependency ------------------------------- */

#[derive(Clone)]
pub enum Dependency {
    DirectImport(PathBuf),
    ManagedImport(PathBuf),
    Hierachy(Vec<String>),
}

/* --------------------------------- Context -------------------------------- */

slotmap::new_key_type! {
    pub struct DefId;
    pub struct PatternId;
    pub struct TermId;
}

/// keeps all ids and spans, the corresponding source location
#[derive(Default, Clone)]
pub struct SpanArena {
    pub defs: SlotMap<DefId, Span>,
    pub patterns: SlotMap<PatternId, Span>,
    pub terms: SlotMap<TermId, Span>,
}

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

impl std::ops::Index<TermId> for SpanArena {
    type Output = Span;

    fn index(&self, id: TermId) -> &Self::Output {
        self.terms.get(id).unwrap()
    }
}

#[derive(Default)]
pub struct Ctx {
    // span arena
    pub spans: SpanArena,
    // arenas
    pub defs: SecondaryMap<DefId, VarName>,
    pub patterns: SecondaryMap<PatternId, Pattern>,
    pub terms: SecondaryMap<TermId, Term<NameRef<VarName>>>,
    // meta
    pub project: Option<String>,
    pub deps: Vec<Dependency>,
    // temp
    pub mod_stack: Vec<String>,
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
    pub fn term(&mut self, term: Sp<Term<NameRef<VarName>>>) -> TermId {
        let id = self.spans.terms.insert(term.info);
        self.terms.insert(id, term.inner);
        id
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
        self.deps.push(Dependency::Hierachy(stack));
    }
    pub fn exit_mod(&mut self) {
        self.mod_stack.pop();
    }

    pub fn span_map(&mut self, file_info: &FileInfo) {
        for (_, info) in self.spans.defs.iter_mut() {
            info.set_info(file_info);
        }
        for (_, info) in self.spans.patterns.iter_mut() {
            info.set_info(file_info);
        }
        for (_, info) in self.spans.terms.iter_mut() {
            info.set_info(file_info);
        }
    }
}
