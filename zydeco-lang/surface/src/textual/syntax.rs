//! The surface syntax of zydeco is defined in this module.

use derive_more::From;
use im::HashSet;
use slotmap::{SecondaryMap, SlotMap};
use std::{
    fmt::{Debug, Display},
    path::PathBuf,
};
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

/* ------------------------------- Structural ------------------------------- */

/// `(...)` as paren-shaped container
#[derive(From, Clone, Debug)]
pub struct Paren<T>(pub Vec<T>);

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
    Ann(Annotation<PatternId, TermId>),
    Hole(Hole),
    Var(DefId),
    Paren(Paren<PatternId>),
}
impl Pattern {
    pub fn get_def_id(&self, ctx: &Ctx) -> Option<DefId> {
        match self {
            Pattern::Ann(Annotation { term, ty: _ }) => ctx.patterns[*term].get_def_id(ctx),
            Pattern::Hole(Hole) => None,
            Pattern::Var(id) => Some(*id),
            Pattern::Paren(Paren(pats)) => pats.iter().find_map(|pat| ctx.patterns[*pat].get_def_id(ctx)),
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

/// `pi (x: A) -> B`
#[derive(Clone, Debug)]
pub struct Pi(pub Vec<PatternId>, pub TermId);
/// `a -> b`
#[derive(Clone, Debug)]
pub struct Arrow(pub TermId, pub TermId);
/// `forall (x: A) . B`
#[derive(Clone, Debug)]
pub struct Forall(pub Vec<PatternId>, pub TermId);
/// `exists (x: A) . B`
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
pub struct Constructor(pub CtorName, pub TermId);
/// `match a | C_1(x_11, ...) -> b_1 | ...`
#[derive(Clone, Debug)]
pub struct Match<Tail> {
    pub scrut: TermId,
    pub arms: Vec<Matcher<Tail>>,
}
#[derive(Clone, Debug)]
pub struct Matcher<Tail> {
    pub name: CtorName,
    pub binders: PatternId,
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
    pub binders: PatternId,
    pub tail: Tail,
}
/// `b .d(a_1, ...)`
#[derive(Clone, Debug)]
pub struct Destructor(pub TermId, pub DtorName, pub TermId);

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

#[derive(Clone, Debug)]
pub struct Module {
    pub name: NameDef<ModName>,
    pub top: Option<TopLevel>,
}

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let NameDef(ModName(mod_name)) = self.name.clone();
        write!(f, "{}", mod_name)
    }
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
    Type(TypeDef),
    Define(Define),
    Module(Module),
    UseDef(UseDef),
    Main(Main),
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Declaration::Type(_) => write!(f, "data/codata"),
            Declaration::Define(_) => write!(f, "define"),
            Declaration::Module(m) => write!(f, "module: {}", m),
            Declaration::UseDef(_) => write!(f, "use"),
            Declaration::Main(_) => write!(f, "main"),
        }
    }
}

#[derive(Clone, Debug)]
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

#[derive(Clone)]
pub struct TopLevel(pub Vec<Modifiers<Declaration>>);

impl Debug for TopLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let TopLevel(inside) = self;
        writeln!(f, "Length: {}", inside.len()).unwrap();
        for Modifiers { inner, .. } in inside {
            writeln!(f, "{}", inner)?;
        }
        writeln!(f, "")
    }
}

impl Display for TopLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let TopLevel(inside) = self;
        writeln!(f, "Length: {}", inside.len()).unwrap();
        for Modifiers { inner, .. } in inside {
            writeln!(f, "{}", inner)?;
        }
        writeln!(f, "")
    }
}

/* ------------------------------- Dependency ------------------------------- */

#[derive(Clone, Debug)]
pub enum Dependency {
    DirectImport(PathBuf),
    ManagedImport(PathBuf),
    Hierachy(Vec<String>),
    Use(NameRef<UseEnum>),
}

/* --------------------------------- Context -------------------------------- */

slotmap::new_key_type! {
    pub struct DefId;
    pub struct PatternId;
    pub struct TermId;
}

/// keeps all ids and spans, the corresponding source location
#[derive(Default, Clone, Debug)]
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

#[derive(Default, Debug)]
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
    pub added_id: (HashSet<DefId>, HashSet<PatternId>, HashSet<TermId>),
}

impl Ctx {
    pub fn def(&mut self, def: Sp<VarName>) -> DefId {
        let id = self.spans.defs.insert(def.info);
        self.defs.insert(id, def.inner);
        self.added_id.0.insert(id);
        id
    }
    pub fn pattern(&mut self, pattern: Sp<Pattern>) -> PatternId {
        let id = self.spans.patterns.insert(pattern.info);
        self.patterns.insert(id, pattern.inner);
        self.added_id.1.insert(id);
        id
    }
    pub fn term(&mut self, term: Sp<Term<NameRef<VarName>>>) -> TermId {
        let id = self.spans.terms.insert(term.info);
        self.terms.insert(id, term.inner);
        self.added_id.2.insert(id);
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

    pub fn update_dep_pairs(&mut self, use_def: &NameRef<UseEnum>) {
        self.deps.push(Dependency::Use(use_def.clone()));
    }

    pub fn exit_mod(&mut self) {
        self.mod_stack.pop();
    }

    pub fn span_map(&mut self, file_info: &FileInfo) {
        for (_, info) in self.spans.defs.iter_mut().filter(|(id, _)| self.added_id.0.contains(id)) {
            info.set_info(file_info);
        }
        for (_, info) in
            self.spans.patterns.iter_mut().filter(|(id, _)| self.added_id.1.contains(id))
        {
            info.set_info(file_info);
        }
        for (_, info) in self.spans.terms.iter_mut().filter(|(id, _)| self.added_id.2.contains(id))
        {
            info.set_info(file_info);
        }
    }

    pub fn merge(&mut self, other: &Ctx) {
        self.defs.extend(other.defs.clone());
        // There will be only one definition
        self.patterns.extend(other.patterns.clone());
        self.terms.extend(other.terms.clone());
        self.spans = other.spans.clone();
    }

    pub fn clear_added_id(&mut self) {
        self.added_id.0.clear();
        self.added_id.1.clear();
        self.added_id.2.clear();
    }
}

type FileId = usize;
#[derive(Clone, Debug, PartialEq)]
pub struct ModuleTree {
    pub root: (String, Option<FileId>),
    pub children: Vec<ModuleTree>,
}

impl Default for ModuleTree {
    fn default() -> Self {
        Self { root: (String::new(), None), children: vec![] }
    }
}

impl ModuleTree {
    pub fn new(root: String) -> Self {
        Self { root: (root, None), children: vec![] }
    }

    pub fn add_child(&mut self, mod_name: String) {
        if !self.children.iter().any(|c| c.root.0 == mod_name) {
            self.children.push(ModuleTree { root: (mod_name, None), children: vec![] })
        }
    }

    pub fn get_children(&self) -> &Vec<ModuleTree> {
        self.children.as_ref()
    }

    pub fn set_file_id(&mut self, path: &Vec<String>, id: FileId) {
        if path.len() == 1 {
            self.root.1 = Some(id);
        } else {
            for child in self.children.iter_mut() {
                if child.root.0 == path[1].as_str() {
                    child.set_file_id(&path[1..].to_vec(), id);
                }
            }
        }
    }

    // get the module_tree entry of the given path
    pub fn get_node_path_mut(&mut self, path: &Vec<String>) -> Option<&mut ModuleTree> {
        if path.len() == 1 && self.root.0 == path[0] {
            return Some(self);
        } else {
            for child in self.children.iter_mut() {
                if child.root.0 == path[1].as_str() {
                    return child.get_node_path_mut(&path[1..].to_vec());
                }
            }
        }
        None
    }

    pub fn get_node_path(&self, path: &Vec<String>) -> Option<ModuleTree> {
        if path.len() == 1 && self.root.0 == path[0] {
            return Some(self.clone());
        } else {
            for child in self.children.iter() {
                if child.root.0 == path[1].as_str() {
                    return child.get_node_path(&path[1..].to_vec());
                }
            }
        }
        None
    }

    pub fn get_id_path(&self, path: &Vec<String>) -> Option<FileId> {
        if path.len() == 1 && self.root.0 == path[0] {
            return self.root.1;
        } else if path.len() <= 1 && self.root.0 != path[0] {
            return None;
        } else {
            for child in self.children.iter() {
                if child.root.0 == path[1].as_str() {
                    return child.get_id_path(&path[1..].to_vec());
                }
            }
        }
        None
    }
}
