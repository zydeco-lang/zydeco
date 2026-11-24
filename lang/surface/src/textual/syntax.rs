//! The surface syntax of zydeco.

pub use super::arena::*;
pub use super::text_block::*;
pub use crate::{arena::*, syntax::*};
pub use zydeco_foundation::span::{FileMap, SourceMap, Sp, Span};
pub use zydeco_syntax::*;

use derive_more::From;

/* ------------------------------- Identifier ------------------------------- */

zydeco_foundation::new_key_type! {
    pub struct DefId;
    pub struct PatId;
    pub struct CoPatId;
    pub struct MetaId;
    pub struct TermId;
}

/// Identifier for any textual entity. The tag prevents cross-category casts.
#[derive(From, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum EntityId {
    Def(DefId),
    Pat(PatId),
    CoPat(CoPatId),
    Meta(MetaId),
    Term(TermId),
}

/// Compact tag used when storage retains an entity's key space and raw ID
/// separately.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
pub(crate) enum EntityCategory {
    Definition,
    Pattern,
    CoPattern,
    Metadata,
    Term,
}

impl EntityId {
    pub(crate) fn into_parts(self) -> (EntityCategory, KeySpaceId, RawIdx) {
        match self {
            | Self::Def(id) => (EntityCategory::Definition, id.key_space(), id.raw()),
            | Self::Pat(id) => (EntityCategory::Pattern, id.key_space(), id.raw()),
            | Self::CoPat(id) => (EntityCategory::CoPattern, id.key_space(), id.raw()),
            | Self::Meta(id) => (EntityCategory::Metadata, id.key_space(), id.raw()),
            | Self::Term(id) => (EntityCategory::Term, id.key_space(), id.raw()),
        }
    }
}

impl EntityCategory {
    pub(crate) fn restore(self, key_space: KeySpaceId, raw: RawIdx) -> EntityId {
        match self {
            | Self::Definition => EntityId::Def(restore_id(key_space, raw)),
            | Self::Pattern => EntityId::Pat(restore_id(key_space, raw)),
            | Self::CoPattern => EntityId::CoPat(restore_id(key_space, raw)),
            | Self::Metadata => EntityId::Meta(restore_id(key_space, raw)),
            | Self::Term => EntityId::Term(restore_id(key_space, raw)),
        }
    }
}

/* -------------------------------- Metadata -------------------------------- */

/// One parsed metadata value.
///
/// Textual metadata refers to nested values by ID so every value has its own
/// source span and trivia anchor. Desugaring converts this representation to
/// the span-free [`zydeco_syntax::Meta`] used by later compiler phases.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MetaNode {
    Ident(String),
    String(String),
    Integer(i64),
    Apply { callee: String, args: Vec<MetaId> },
}

impl MetaNode {
    pub fn callee(&self) -> Option<&str> {
        match self {
            | Self::Ident(name) | Self::Apply { callee: name, .. } => Some(name),
            | Self::String(_) | Self::Integer(_) => None,
        }
    }

    pub fn arguments(&self) -> &[MetaId] {
        match self {
            | Self::Apply { args, .. } => args,
            | Self::Ident(_) | Self::String(_) | Self::Integer(_) => &[],
        }
    }

    pub fn is(&self, name: &str) -> bool {
        self.callee() == Some(name)
    }
}

/// A metadata annotation paired with its payload.
#[derive(Clone, Debug)]
pub struct MetaTerm<T>(pub MetaId, pub T);

/* --------------------------------- Pattern -------------------------------- */

#[derive(From, Clone, Debug)]
pub enum Pattern {
    Ann(Ann<PatId, TermId>),
    Manifest(ManifestPattern),
    Hole(Hole),
    Var(DefId),
    Named(Named<FieldName, PatId>),
    Ctor(Ctor<CtorName, PatId>),
    Lit(Literal),
    Project(ProjectionPattern<FieldName, PatId>),
    View(ViewPattern),
    Alias(Alias<PatId>),
    Paren(Paren<PatId>),
}

/// `function ~> pattern`.
#[derive(Clone, Debug)]
pub struct ViewPattern {
    pub function: TermId,
    pub pattern: PatId,
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
    /// How the binding sugar constructs its classifier and bindee.
    pub flavor: BindingFlavor,
    /// Binder pattern.
    pub binder: PatId,
    /// Optional parameter list (curried).
    pub params: Option<CoPatId>,
    /// Optional type annotation.
    pub ty: Option<TermId>,
    /// Bound term.
    pub bindee: Bindee,
}

/// The elaboration discipline selected by a binding header.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BindingFlavor {
    Plain,
    Value,
    Computation,
    Recursive,
}

/// `pi (x : A) (y : B) . C`
#[derive(Clone, Debug)]
pub struct Pi(pub CoPatId, pub TermId);
/// `val pi (x : A) (y : B) . C`
#[derive(Clone, Debug)]
pub struct ValPi(pub CoPatId, pub TermId);
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
    pub annotations: Vec<Sp<MetaId>>,
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

/// `pack (X as A : K) ... where c_1, ..., c_n end`
///
/// The body is one `Paren` node over the comma-separated payload components,
/// so a single component unwraps and multiple components form one tuple.
#[derive(Clone, Debug)]
pub struct Pack {
    pub parameters: Vec<PackParameter>,
    pub body: TermId,
}

/// One `pack` telescope entry: the parameter shared with `exists`,
/// plus sealed evidence after `is` for abstract binders.
#[derive(Clone, Debug)]
pub struct PackParameter {
    pub parameter: ExistentialParameter,
    pub evidence: Option<TermId>,
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

/// `param p in e`, `param val p in e`, or their block-mobile `that` forms.
#[derive(Clone, Debug)]
pub struct Param {
    pub flavor: ParameterFlavor,
    pub binder: PatId,
    pub placement: Placement,
    pub tail: TermId,
}

/// A value-function application retaining its selected surface direction.
#[derive(Clone, Debug)]
pub struct Pipeline {
    pub direction: PipelineDirection,
    pub subject: TermId,
    pub function: TermId,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum PipelineDirection {
    Forward,
    Backward,
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

/// `seal term : Sig`
#[derive(Clone, Debug)]
pub struct Seal {
    pub tm: TermId,
    pub ty: TermId,
}

#[derive(From, Clone, Debug)]
pub enum Term {
    Meta(MetaTerm<TermId>),
    SourceBoundary(SourceBoundary<TermId>),
    SignatureBoundary(SignatureBoundary<TermId>),
    Ann(Ann<TermId, TermId>),
    Seal(Seal),
    Hole(Hole),
    Var(VarName),
    Named(Named<FieldName, TermId>),
    Label(Label<FieldName, TermId>),
    Paren(Paren<TermId>),
    Abs(Abs<CoPatId, TermId>),
    #[from(ignore)]
    ValAbs(Abs<CoPatId, TermId>),
    App(Appli<TermId>),
    Fix(Fix<PatId, TermId>),
    Pi(Pi),
    ValPi(ValPi),
    Forall(Forall),
    Arrow(ArrowU<TermId>),
    Sigma(Sigma),
    Exists(Exists),
    Pack(Pack),
    Prod(Prod<TermId>),
    Thunk(Thunk<TermId>),
    Force(Force<TermId>),
    Ret(Return<TermId>),
    Do(Bind<PatId, TermId, TermId>),
    Let(GenLet),
    Param(Param),
    Pipeline(Pipeline),
    ContextBind(ContextBind),
    Block(Block),
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

/// Source boundary markers retained only when parsing for formatting.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ArmPrefix {
    pub first: EntityId,
    pub payload: EntityId,
    pub start: usize,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ExistentialPrefix {
    pub parameter: PatId,
    /// Byte offset where the whole parameter begins, before its annotations.
    pub start: usize,
    /// Byte offset of the grammar-owned `(` that opens the binder.
    pub open: usize,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct MetadataPrefix {
    pub metadata: MetaId,
    pub start: usize,
}

/// Optional structural prefixes collected when parsing for formatting.
#[derive(Clone, Default, Debug)]
pub struct FormatPrefixes {
    pub arms: Vec<ArmPrefix>,
    pub existentials: Vec<ExistentialPrefix>,
    pub metadata: Vec<MetadataPrefix>,
}

pub struct Parser {
    allocator: IdAllocator<TextualScope>,
    format_prefixes: Option<FormatPrefixes>,
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
        Self {
            allocator: IdAllocator::new(),
            format_prefixes: None,
            spans: SpanArena::new(),
            arena: TextArena::default(),
        }
    }
    /// Create a parser that retains structural prefix offsets for formatting.
    pub fn for_formatting() -> Self {
        Self { format_prefixes: Some(FormatPrefixes::default()), ..Self::new() }
    }
    /// Structural prefix offsets recorded when [`Parser::for_formatting`] is used.
    pub fn format_prefixes(&self) -> Option<&FormatPrefixes> {
        self.format_prefixes.as_ref()
    }
    /// Finish parsing, dropping the issuer and returning only durable storage.
    pub fn finish(mut self) -> (SpanArena, TextArena) {
        self.spans.shrink_to_fit();
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
    /// Allocate a metadata node and record its span.
    pub fn meta(&mut self, meta: Sp<MetaNode>) -> MetaId {
        let id = self.alloc(meta.info);
        self.arena.metas.insert_new(id, meta.inner);
        id
    }
    /// Allocate a term node and record its span.
    pub fn term(&mut self, term: Sp<Term>) -> TermId {
        let id = self.alloc(term.info);
        self.arena.terms.insert_new(id, term.inner);
        id
    }
    /// Extend an infix product chain to the left: `A * B * C` builds one
    /// flat product, while a parenthesized left operand stays a distinct
    /// nested component because parentheses allocate their own node kind.
    pub fn extend_product(&mut self, left: TermId, right: TermId) -> Prod<TermId> {
        let components = match &mut self.arena.terms[&left] {
            | Term::Prod(prod) => {
                let mut components = std::mem::take(&mut prod.0);
                components.push(right);
                components
            }
            | _ => vec![left, right],
        };
        Prod(components)
    }
    /// Record an arm marker. `first` owns comments before the arm, while
    /// `payload` supplies the layout boundary after its header.
    pub fn arm_prefix(
        &mut self, first: impl Into<EntityId>, payload: impl Into<EntityId>, start: usize,
    ) {
        if let Some(prefixes) = &mut self.format_prefixes {
            prefixes.arms.push(ArmPrefix { first: first.into(), payload: payload.into(), start });
        }
    }
    /// Record the first token of an existential parameter together with its
    /// grammar-owned open delimiter, neither of which is represented by the
    /// binder.
    pub fn existential_prefix(&mut self, parameter: PatId, start: usize, open: usize) {
        if let Some(prefixes) = &mut self.format_prefixes {
            prefixes.existentials.push(ExistentialPrefix { parameter, start, open });
        }
    }
    /// Record the `@` that prefixes standalone metadata attached to a pattern.
    pub fn metadata_prefix(&mut self, metadata: MetaId, start: usize) {
        if let Some(prefixes) = &mut self.format_prefixes {
            prefixes.metadata.push(MetadataPrefix { metadata, start });
        }
    }
    pub(crate) fn record_text_blocks(
        &mut self, source: &str, start_entity: usize, text_lines: Vec<std::ops::Range<usize>>,
    ) {
        self.arena.text_blocks.record(source, text_lines, self.spans.iter_from(start_entity));
    }
    /// Expand `= field` into a named term whose payload is the same-spelled
    /// variable, optionally annotated.
    pub fn pun_term(&mut self, field: Sp<FieldName>, ty: Option<Sp<TermId>>) -> Term {
        let variable = self.term(field.mk(VarName::from_sym(field.inner.sym()).into()));
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
    ) -> PatId {
        let start = self.spans[&EntityId::Pat(binder)].lo();
        let span = Span::new(start, end);
        match self.arena.pats[&binder].clone() {
            | Pattern::Named(Named(field, inner)) => {
                let inner = self.manifest_pattern(inner, definition, classifier, end);
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
        let binder = self.def(field.mk(VarName::from_sym(field.inner.sym())));
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
