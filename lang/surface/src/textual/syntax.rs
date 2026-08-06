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
}

/// Identifier for any textual entity. The tag prevents cross-category casts.
#[derive(From, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum EntityId {
    Def(DefId),
    Pat(PatId),
    CoPat(CoPatId),
    Term(TermId),
}

/* --------------------------------- Pattern -------------------------------- */

#[derive(From, Clone, Debug)]
pub enum Pattern {
    Ann(Ann<PatId, TermId>),
    Hole(Hole),
    Var(DefId),
    Named(Named<FieldName, PatId>),
    Ctor(Ctor<CtorName, PatId>),
    Project(ProjectionPattern<FieldName, PatId>),
    Alias(Alias<PatId>),
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
    /// Bound term.
    pub bindee: Bindee,
}

/// `do M ; N`
#[derive(Clone, Debug)]
pub struct KontCall {
    pub body: TermId,
    pub tail: TermId,
}

/// `pi (x : A) (y : B) . C`
#[derive(Clone, Debug)]
pub struct Pi(pub CoPatId, pub TermId);
/// `forall (x : A) (y : B) . C`
#[derive(Clone, Debug)]
pub struct Forall(pub CoPatId, pub TermId);

/// `sigma (x : A) (y : B) . C`
#[derive(Clone, Debug)]
pub struct Sigma(pub CoPatId, pub TermId);
/// One abstract or manifest binder in an existential telescope.
#[derive(Clone, Debug)]
pub enum ExistentialParameter {
    Abstract(PatId),
    Manifest(ManifestParameter),
}

/// `(X as A : K)` or `(X as A)`
#[derive(Clone, Debug)]
pub struct ManifestParameter {
    pub binder: PatId,
    pub definition: TermId,
    pub classifier: Option<TermId>,
}

/// `exists (x : A) (X as B : K) . C`
#[derive(Clone, Debug)]
pub struct Exists {
    pub parameters: Vec<ExistentialParameter>,
    pub body: TermId,
}

/// `let x = a in ...`
#[derive(Clone, Debug)]
pub struct GenLet {
    pub binding: GenBind<TermId>,
    pub tail: TermId,
}

/// Whether a context-forming binder stays lexical or moves to its nearest
/// enclosing block.
#[derive(Copy, Clone, Debug)]
pub enum Placement {
    In,
    That,
}

/// The identity discipline of a term-level definition.
#[derive(Copy, Clone, Debug)]
pub enum DefinitionMode {
    Transparent,
    Nominal,
}

/// `param p in e` or `param p that e`.
#[derive(Clone, Debug)]
pub struct Param {
    pub binder: PatId,
    pub placement: Placement,
    pub tail: TermId,
}

/// `let p = e ...` or `def p = e ...`.
#[derive(Clone, Debug)]
pub struct ContextBind {
    pub mode: DefinitionMode,
    pub binding: GenBind<TermId>,
    pub placement: Placement,
    pub tail: TermId,
}

/// `begin ... end`
#[derive(Clone, Debug)]
pub struct Block(pub TermId);

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

/// `comatch | .d_1 => b_1 | ... end`
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
    SourceBoundary(SourceBoundary<TermId>),
    Ann(Ann<TermId, TermId>),
    Hole(Hole),
    Var(VarName),
    Named(Named<FieldName, TermId>),
    Label(Label<FieldName, TermId>),
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
    Param(Param),
    ContextBind(ContextBind),
    Block(Block),
    MoBlock(MoBlock),
    Data(Data),
    CoData(CoData),
    Ctor(Ctor<CtorName, TermId>),
    Match(Match<TermId, PatId, TermId>),
    CoMatch(CoMatchParam),
    Dtor(Dtor<TermId, DtorName>),
    Proj(Proj<TermId, FieldName>),
    Lit(Literal),
}

/* ------------------------------- Source Unit ------------------------------ */

/// A complete source file represented by one root term.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct SourceUnit {
    pub root: TermId,
}

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
    /// Expand `= field` into a named term whose payload is the same-spelled
    /// variable, optionally annotated.
    pub fn pun_term(&mut self, field: Sp<FieldName>, ty: Option<Sp<TermId>>) -> Term {
        let variable = self.term(field.mk(VarName(field.inner.0.clone()).into()));
        let inner = match ty {
            | Some(ty) => {
                let annotation = Ann { tm: variable, ty: ty.inner };
                self.term(ty.mk(annotation.into()))
            }
            | None => variable,
        };
        Named(field.inner, inner).into()
    }
    /// Expand `= field` into a named pattern whose payload is a fresh
    /// same-spelled binder, optionally annotated.
    pub fn pun_pattern(&mut self, field: Sp<FieldName>, ty: Option<Sp<TermId>>) -> Pattern {
        let inner = self.punned_pattern_payload(&field, ty);
        Named(field.inner, inner).into()
    }
    /// Expand `/field` into a projection pattern whose payload is a fresh
    /// same-spelled binder, optionally annotated.
    pub fn pun_projection_pattern(
        &mut self, field: Sp<FieldName>, ty: Option<Sp<TermId>>,
    ) -> Pattern {
        let inner = self.punned_pattern_payload(&field, ty);
        ProjectionPattern(field.inner, inner).into()
    }
    fn punned_pattern_payload(&mut self, field: &Sp<FieldName>, ty: Option<Sp<TermId>>) -> PatId {
        let binder = self.def(field.mk(VarName(field.inner.0.clone())));
        let variable = self.pat(field.mk(binder.into()));
        match ty {
            | Some(ty) => {
                let annotation = Ann { tm: variable, ty: ty.inner };
                self.pat(ty.mk(annotation.into()))
            }
            | None => variable,
        }
    }
}
