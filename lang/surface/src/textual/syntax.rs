//! The surface syntax of zydeco.

pub use super::arena::*;
pub use super::intention::*;
pub use super::trivia::*;
pub use crate::{arena::*, syntax::*};
pub use zydeco_syntax::*;
pub use zydeco_utils::span::{LocationCtx, Sp, Span};

use derive_more::From;
use zydeco_utils::span::FileInfo;

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
    Manifest(ManifestPattern),
    Hole(Hole),
    Var(DefId),
    Named(Named<FieldName, PatId>),
    Ctor(Ctor<CtorName, PatId>),
    Project(ProjectionPattern<FieldName, PatId>),
    Alias(Alias<PatId>),
    Paren(Paren<PatId>),
}

/// A type binder whose definition is disclosed by an enclosing existential.
///
/// In `exists (field = X as A : K) . B`, this node represents `X as A`.
/// Ordinary named-pattern and annotation nodes wrap it to preserve the
/// surface structure `field = ((X as A) : K)`.
#[derive(Clone, Debug)]
pub struct ManifestPattern {
    pub binder: PatId,
    pub definition: TermId,
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
/// One binder in an existential telescope.
#[derive(Clone, Debug)]
pub struct ExistentialParameter {
    /// Metadata attached to this parameter's pattern.
    pub annotations: Vec<Sp<Meta>>,
    /// The complete binder pattern. A manifest parameter contains one
    /// `Pattern::Manifest` beneath its ordinary named and annotation wrappers.
    pub binder: PatId,
}

impl ExistentialParameter {
    pub fn binder(&self) -> PatId {
        self.binder
    }
}

/// `exists @[meta] (x : A) (X as B : K) . C`
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
    /// Retain source presentation after one public parser entry point succeeds.
    ///
    /// Existing spans let printers reuse selected layout decisions without
    /// introducing layout-only variants into the textual AST.
    pub fn capture_source_information(&mut self, source: &str) {
        let file_info = FileInfo::new(source, None);
        let entities = self
            .spans
            .iter()
            .filter(|(entity, _)| self.arena.intentions.line_extent(**entity).is_none())
            .filter_map(|(entity, span)| {
                let (start, end) = span.get_cursor1();
                source.get(start..end)?;
                Some(SpannedEntity::new(*entity, start, end))
            })
            .collect::<Vec<_>>();
        let comments = CommentCapture::new(source, &entities);
        let extents = entities
            .iter()
            .map(|entity| {
                let occupied_end = entity.end().saturating_sub(1).max(entity.start());
                let first = file_info.trans_span2(entity.start()).line;
                let last = file_info.trans_span2(occupied_end).line;
                (entity.entity(), LineExtent::new(first, last))
            })
            .collect::<Vec<_>>();
        self.arena.intentions.record_source_line_extents(
            source,
            comments.layout_exclusions(),
            extents,
        );
        self.arena.trivia.record_comments(comments);
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
    /// Insert a manifest binder beneath the leading named-pattern wrappers.
    ///
    /// This turns the parsed prefix `field = binder` plus the existential
    /// suffix `as definition : classifier` into the structural pattern
    /// `field = ((binder as definition) : classifier)`.
    pub fn manifest_pattern(
        &mut self, binder: PatId, definition: TermId, classifier: Option<TermId>, end: usize,
        loc: &LocationCtx,
    ) -> PatId {
        let (start, _) = self.spans[&EntityId::Pat(binder)].get_cursor1();
        let span = Span::new(start, end).under_loc_ctx(loc);
        match self.arena.pats[&binder].clone() {
            | Pattern::Named(Named(field, inner)) => {
                let inner = self.manifest_pattern(inner, definition, classifier, end, loc);
                self.arena.pats[&binder] = Named(field, inner).into();
                self.spans.replace(binder, span);
                binder
            }
            | _ => {
                let manifest = ManifestPattern { binder, definition };
                let manifest = self.pat(span.make(manifest.into()));
                classifier
                    .map(|ty| self.pat(span.make(Ann { tm: manifest, ty }.into())))
                    .unwrap_or(manifest)
            }
        }
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
