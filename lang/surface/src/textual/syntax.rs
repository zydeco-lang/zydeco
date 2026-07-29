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
}

/// Identifier for any textual entity. The tag prevents cross-category casts.
#[derive(From, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
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
    Named(Named<FieldName, PatId>),
    Ctor(Ctor<CtorName, PatId>),
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
    Var(VarName),
    Named(Named<FieldName, TermId>),
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
    Ctor(Ctor<CtorName, TermId>),
    Match(Match<TermId, PatId, TermId>),
    CoMatch(CoMatchParam),
    Dtor(Dtor<TermId, DtorName>),
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
    allocator: IdAllocator<TextualScope>,
    pub spans: SpanArena,
    pub arena: TextArena,
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}

impl Parser {
    /// Create a parser with one ID issuer for all textual entity categories.
    pub fn new() -> Self {
        Self { allocator: IdAllocator::new(), spans: SpanArena::new(), arena: TextArena::default() }
    }
    /// Finish parsing, dropping the issuer and returning only durable storage.
    pub fn finish(self) -> (SpanArena, TextArena) {
        (self.spans, self.arena)
    }
    fn alloc<Id>(&mut self, span: Span) -> Id
    where
        Id: ArenaId + Into<EntityId>,
        TextualScope: Allocates<Id>,
    {
        let id = self.allocator.alloc();
        self.spans.insert_new(id, span);
        id
    }
    /// Allocate a definition node and record its span.
    pub fn def(&mut self, def: Sp<VarName>) -> DefId {
        let id = self.alloc(def.info);
        self.arena.defs.insert_new(id, def.inner);
        id
    }
    /// Allocate a pattern node and record its span.
    pub fn pat(&mut self, pat: Sp<Pattern>) -> PatId {
        let id = self.alloc(pat.info);
        self.arena.pats.insert_new(id, pat.inner);
        id
    }
    /// Allocate a copattern node and record its span.
    pub fn copat(&mut self, copat: Sp<CoPattern>) -> CoPatId {
        let id = self.alloc(copat.info);
        self.arena.copats.insert_new(id, copat.inner);
        id
    }
    /// Allocate a term node and record its span.
    pub fn term(&mut self, term: Sp<Term>) -> TermId {
        let id = self.alloc(term.info);
        self.arena.terms.insert_new(id, term.inner);
        id
    }
    /// Allocate a declaration node and record its span.
    pub fn decl(&mut self, decl: Sp<Modifiers<Declaration>>) -> DeclId {
        let id = self.alloc(decl.info);
        self.arena.decls.insert_new(id, decl.inner);
        id
    }
}
