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

    pub struct EntityId;
}

mod impls_identifier {
    use super::*;
    impl From<DefId> for EntityId {
        fn from(DefId(meta, idx): DefId) -> Self {
            EntityId::new(meta, idx)
        }
    }
    impl Into<DefId> for EntityId {
        fn into(self) -> DefId {
            let EntityId(meta, idx) = self;
            DefId::new(meta, idx)
        }
    }
    impl From<PatId> for EntityId {
        fn from(PatId(meta, idx): PatId) -> Self {
            EntityId::new(meta, idx)
        }
    }
    impl Into<PatId> for EntityId {
        fn into(self) -> PatId {
            let EntityId(meta, idx) = self;
            PatId::new(meta, idx)
        }
    }
    impl From<CoPatId> for EntityId {
        fn from(CoPatId(meta, idx): CoPatId) -> Self {
            EntityId::new(meta, idx)
        }
    }
    impl Into<CoPatId> for EntityId {
        fn into(self) -> CoPatId {
            let EntityId(meta, idx) = self;
            CoPatId::new(meta, idx)
        }
    }
    impl From<TermId> for EntityId {
        fn from(TermId(meta, idx): TermId) -> Self {
            EntityId::new(meta, idx)
        }
    }
    impl Into<TermId> for EntityId {
        fn into(self) -> TermId {
            let EntityId(meta, idx) = self;
            TermId::new(meta, idx)
        }
    }
    impl From<DeclId> for EntityId {
        fn from(DeclId(meta, idx): DeclId) -> Self {
            EntityId::new(meta, idx)
        }
    }
    impl Into<DeclId> for EntityId {
        fn into(self) -> DeclId {
            let EntityId(meta, idx) = self;
            DeclId::new(meta, idx)
        }
    }
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

/// `do M ; N`
#[derive(Clone, Debug)]
pub struct KontCall {
    pub body: TermId,
    pub tail: TermId,
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
    pub params: Option<CoPatId>,
    pub out: TermId,
}

/// `comatch | .d_1 -> b_1 | ... end`
#[derive(Clone, Debug)]
pub struct CoMatchParam {
    pub arms: Vec<CoMatcherParam>,
}
#[derive(Clone, Debug)]
pub struct CoMatcherParam {
    pub params: CoPatId,
    pub tail: TermId,
}

/// `import f : B = V`
#[derive(Clone, Debug)]
pub struct Import {
    pub binder: PatId,
    pub ty: TermId,
    pub body: TermId,
}

/// `with mo import f : B = V begin M end`
#[derive(Clone, Debug)]
pub struct WithBlock {
    pub structs: Vec<TermId>,
    pub imports: Vec<Import>,
    pub body: TermId,
}

#[derive(From, Clone, Debug)]
pub enum Term {
    Ann(Ann<TermId, TermId>),
    Hole(Hole),
    Var(NameRef<VarName>),
    Paren(Paren<TermId>),
    Abs(Abs<CoPatId, TermId>),
    App(Appli<TermId>),
    KontCall(KontCall),
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
    Match(Match<TermId, PatId, TermId>),
    CoMatch(CoMatchParam),
    Dtor(Dtor<TermId>),
    WithBlock(WithBlock),
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
pub struct Define(pub GenBind<Option<TermId>>);

#[derive(Clone, Debug)]
pub struct Alias(pub GenBind<TermId>);

#[derive(Clone, Debug)]
pub struct Module {
    pub name: Option<NameRef<VarName>>,
    pub top: TopLevel,
}

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

// Todo: Add a way to specify the expected output of the execution
#[derive(Clone, Debug)]
pub enum ExecType {
    Main,
    Test,
    Fail,
}

#[derive(Clone, Debug)]
pub struct Exec(pub TermId);

#[derive(Clone, From, Debug)]
pub enum Declaration {
    DataDef(DataDef),
    CoDataDef(CoDataDef),
    Define(Define),
    Alias(Alias),
    Module(Module),
    // Layer(Layer),
    // UseDef(UseDef),
    // UseBlock(UseBlock),
    Exec(Exec),
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

pub type SpanArena = ArenaSparse<EntityId, Span>;

/* --------------------------------- Parser --------------------------------- */

pub struct Parser {
    pub spans: SpanArena,
    pub arena: Arena,
}

impl Parser {
    pub fn new(allocator: IndexAlloc<usize>) -> Self {
        Self { spans: ArenaSparse::new(allocator), arena: Arena::default() }
    }
    pub fn def(&mut self, def: Sp<VarName>) -> DefId {
        let id = self.spans.alloc(def.info).into();
        self.arena.defs.insert(id, def.inner);
        id
    }
    pub fn pat(&mut self, pat: Sp<Pattern>) -> PatId {
        let id = self.spans.alloc(pat.info).into();
        self.arena.pats.insert(id, pat.inner);
        id
    }
    pub fn copat(&mut self, copat: Sp<CoPattern>) -> CoPatId {
        let id = self.spans.alloc(copat.info).into();
        self.arena.copats.insert(id, copat.inner);
        id
    }
    pub fn term(&mut self, term: Sp<Term>) -> TermId {
        let id = self.spans.alloc(term.info).into();
        self.arena.terms.insert(id, term.inner);
        id
    }
    pub fn decl(&mut self, decl: Sp<Modifiers<Declaration>>) -> DeclId {
        let id = self.spans.alloc(decl.info).into();
        self.arena.decls.insert(id, decl.inner);
        id
    }
}
