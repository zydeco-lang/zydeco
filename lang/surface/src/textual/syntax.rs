//! The surface syntax of zydeco.

pub use super::arena::*;
pub use crate::{arena::*, syntax::*};
pub use zydeco_syntax::*;
pub use zydeco_utils::span::{LocationCtx, Sp, Span};

use derive_more::From;

/* ------------------------------- Identifier ------------------------------- */

zydeco_utils::new_key_type! {
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
    /// Whether this binding uses `fix`.
    pub fix: bool,
    /// Whether this binding is a computation binding (`!`).
    pub comp: bool,
    /// Binder pattern.
    pub binder: PatId,
    /// Optional parameter list (curried).
    pub params: Option<CoPatId>,
    /// Optional type annotation.
    pub ty: Option<TermId>,
    /// Bound term or placeholder for externs.
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
pub struct GenLet {
    pub binding: GenBind<TermId>,
    pub tail: TermId,
}

/// `monadic ... end`
#[derive(Clone, Debug)]
pub struct MoBlock(pub TermId);

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

#[derive(From, Clone, Debug)]
pub enum Term {
    Meta(MetaT<TermId>),
    Ann(Ann<TermId, TermId>),
    Hole(Hole),
    Var(NameRef<VarName>),
    Paren(Paren<TermId>),
    Abs(Abs<CoPatId, TermId>),
    App(Appli<TermId>),
    KontCall(KontCall),
    Fix(Fix<PatId, TermId>),
    Pi(Pi),
    Forall(Forall),
    Arrow(ArrowU<TermId>),
    Sigma(Sigma),
    Exists(Exists),
    Prod(ProdU<TermId>),
    Thunk(Thunk<TermId>),
    Force(Force<TermId>),
    Ret(Return<TermId>),
    Do(Bind<PatId, TermId, TermId>),
    Let(GenLet),
    MoBlock(MoBlock),
    Data(Data),
    CoData(CoData),
    Ctor(Ctor<TermId>),
    Match(Match<TermId, PatId, TermId>),
    CoMatch(CoMatchParam),
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
pub struct Define(pub GenBind<Option<TermId>>);

#[derive(Clone, Debug)]
pub struct Alias(pub GenBind<TermId>);

#[derive(Clone, Debug)]
pub struct Module {
    pub name: Option<NameRef<VarName>>,
    pub top: TopLevel,
}

#[derive(From, Clone, Debug)]
pub struct UseDef(pub UsePath);

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
    Meta(MetaT<DeclId>),
    DataDef(DataDef),
    CoDataDef(CoDataDef),
    Define(Define),
    Alias(Alias),
    Module(Module),
    Exec(Exec),
}

#[derive(From, Clone, Debug)]
pub enum ReplInput {
    Declaration(DeclId),
    Term(TermId),
}

#[derive(Clone, Debug)]
pub struct TopLevel(pub Vec<DeclId>);

/* --------------------------------- Parser --------------------------------- */

pub struct Parser {
    pub spans: SpanArena,
    pub arena: TextArena,
}

impl Parser {
    /// Create a parser with arenas backed by the given allocator.
    pub fn new(allocator: IndexAlloc<usize>) -> Self {
        Self { spans: SpanArena::new(allocator), arena: TextArena::default() }
    }
    /// Allocate a definition node and record its span.
    pub fn def(&mut self, def: Sp<VarName>) -> DefId {
        let id = self.spans.alloc(def.info).into();
        self.arena.defs.insert(id, def.inner);
        id
    }
    /// Allocate a pattern node and record its span.
    pub fn pat(&mut self, pat: Sp<Pattern>) -> PatId {
        let id = self.spans.alloc(pat.info).into();
        self.arena.pats.insert(id, pat.inner);
        id
    }
    /// Allocate a copattern node and record its span.
    pub fn copat(&mut self, copat: Sp<CoPattern>) -> CoPatId {
        let id = self.spans.alloc(copat.info).into();
        self.arena.copats.insert(id, copat.inner);
        id
    }
    /// Allocate a term node and record its span.
    pub fn term(&mut self, term: Sp<Term>) -> TermId {
        let id = self.spans.alloc(term.info).into();
        self.arena.terms.insert(id, term.inner);
        id
    }
    /// Allocate a declaration node and record its span.
    pub fn decl(&mut self, decl: Sp<Modifiers<Declaration>>) -> DeclId {
        let id = self.spans.alloc(decl.info).into();
        self.arena.decls.insert(id, decl.inner);
        id
    }
}
