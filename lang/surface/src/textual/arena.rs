use super::syntax::*;
use super::{SurfaceIntentions, SurfaceTrivia};
use std::{collections::HashSet, sync::Arc};

/* ---------------------------------- Arena --------------------------------- */

/// Allocation and storage scope for parsed textual syntax.
#[derive(Debug)]
pub enum TextualScope {}

impl Allocates<DefId> for TextualScope {}
impl Allocates<PatId> for TextualScope {}
impl Allocates<CoPatId> for TextualScope {}
impl Allocates<MetaId> for TextualScope {}
impl Allocates<TermId> for TextualScope {}

impl ArenaSchema<DefId> for TextualScope {
    type Item = VarName;
}
impl ArenaSchema<PatId> for TextualScope {
    type Item = Pattern;
}
impl ArenaSchema<CoPatId> for TextualScope {
    type Item = CoPattern;
}
impl ArenaSchema<MetaId> for TextualScope {
    type Item = MetaNode;
}
impl ArenaSchema<TermId> for TextualScope {
    type Item = Term;
}
/// Parsed nodes keyed by textual IDs.
#[derive(Clone, Default, Debug)]
pub struct TextArena {
    pub defs: ArenaSparse<TextualScope, DefId>,
    pub pats: ArenaSparse<TextualScope, PatId>,
    pub copats: ArenaSparse<TextualScope, CoPatId>,
    pub metas: ArenaSparse<TextualScope, MetaId>,
    pub terms: ArenaSparse<TextualScope, TermId>,
    /// Author-selected layout that does not change canonical syntax.
    pub intentions: SurfaceIntentions,
    /// Source content retained without adding syntax variants.
    pub trivia: SurfaceTrivia,
}

impl TextArena {
    /// Direct AST edges, in stable structural order, excluding trivia and allocation bookkeeping.
    /// Keep the exhaustive cases here so reachability has one structural definition.
    pub(crate) fn children(&self, entity: EntityId) -> Vec<EntityId> {
        match entity {
            | EntityId::Def(_) => Vec::new(),
            | EntityId::Meta(meta) => {
                self.metas[&meta].arguments().iter().copied().map(Into::into).collect()
            }
            | EntityId::CoPat(copat) => match &self.copats[&copat] {
                | CoPattern::Pat(pattern) => vec![(*pattern).into()],
                | CoPattern::Dtor(_) => Vec::new(),
                | CoPattern::App(Appli(patterns)) => {
                    patterns.iter().copied().map(Into::into).collect()
                }
            },
            | EntityId::Pat(pattern) => match &self.pats[&pattern] {
                | Pattern::Hole(_) | Pattern::Lit(_) => Vec::new(),
                | Pattern::Var(definition) => vec![(*definition).into()],
                | Pattern::Ann(Ann { tm, ty }) => vec![(*tm).into(), (*ty).into()],
                | Pattern::Manifest(ManifestPattern { binder, definition }) => {
                    vec![(*binder).into(), (*definition).into()]
                }
                | Pattern::Named(Named(_, inner))
                | Pattern::Ctor(Ctor(_, inner))
                | Pattern::Project(ProjectionPattern(_, inner)) => vec![(*inner).into()],
                | Pattern::View(ViewPattern { function, pattern }) => {
                    vec![(*function).into(), (*pattern).into()]
                }
                | Pattern::Alias(Alias(ConsN(patterns, last))) => {
                    patterns.iter().chain(std::iter::once(last)).copied().map(Into::into).collect()
                }
                | Pattern::Paren(Paren(patterns)) => {
                    patterns.iter().copied().map(Into::into).collect()
                }
            },
            | EntityId::Term(term) => match &self.terms[&term] {
                | Term::Hole(_) | Term::Var(_) | Term::Lit(_) => Vec::new(),
                | Term::Meta(MetaTerm(meta, inner)) => vec![(*meta).into(), (*inner).into()],
                | Term::SourceBoundary(SourceBoundary(inner))
                | Term::SignatureBoundary(SignatureBoundary(inner))
                | Term::Named(Named(_, inner))
                | Term::Label(Label(_, inner))
                | Term::Thunk(Thunk(inner))
                | Term::Force(Force(inner))
                | Term::Ret(Return(inner))
                | Term::Block(Block(inner))
                | Term::Ctor(Ctor(_, inner))
                | Term::Dtor(Dtor(inner, _))
                | Term::Proj(Proj(inner, _)) => vec![(*inner).into()],
                | Term::Ann(Ann { tm, ty }) => vec![(*tm).into(), (*ty).into()],
                | Term::Arrow(Arrow(left, right)) => vec![(*left).into(), (*right).into()],
                | Term::Paren(Paren(terms)) | Term::App(Appli(terms)) | Term::Prod(Prod(terms)) => {
                    terms.iter().copied().map(Into::into).collect()
                }
                | Term::Abs(Abs(params, body))
                | Term::ValAbs(Abs(params, body))
                | Term::Pi(Pi(params, body))
                | Term::ValPi(ValPi(params, body))
                | Term::Forall(Forall(params, body))
                | Term::Sigma(Sigma(params, body)) => vec![(*params).into(), (*body).into()],
                | Term::Fix(Fix(binder, body)) => vec![(*binder).into(), (*body).into()],
                | Term::Exists(Exists { parameters, body }) => parameters
                    .iter()
                    .flat_map(Self::parameter_children)
                    .chain(std::iter::once((*body).into()))
                    .collect(),
                | Term::Pack(Pack { parameters, body }) => parameters
                    .iter()
                    .flat_map(|parameter| {
                        Self::parameter_children(&parameter.parameter)
                            .chain(parameter.evidence.map(Into::into))
                    })
                    .chain(std::iter::once((*body).into()))
                    .collect(),
                | Term::Do(Bind { binder, bindee, tail }) => {
                    vec![(*binder).into(), (*bindee).into(), (*tail).into()]
                }
                | Term::Let(GenLet { binding, tail })
                | Term::ContextBind(ContextBind { binding, tail, .. }) => {
                    Self::binding_children(binding).chain(std::iter::once((*tail).into())).collect()
                }
                | Term::Param(Param { binder, tail, .. }) => vec![(*binder).into(), (*tail).into()],
                | Term::Pipeline(Pipeline { subject, function, .. }) => {
                    vec![(*subject).into(), (*function).into()]
                }
                | Term::Data(Data { arms }) => arms.iter().map(|arm| arm.param.into()).collect(),
                | Term::CoData(CoData { arms }) => arms
                    .iter()
                    .flat_map(|arm| arm.params.map(Into::into).into_iter().chain([arm.out.into()]))
                    .collect(),
                | Term::Match(Match { scrut, arms }) => std::iter::once((*scrut).into())
                    .chain(arms.iter().flat_map(|arm| [arm.binder.into(), arm.tail.into()]))
                    .collect(),
                | Term::CoMatch(CoMatchParam { arms }) => {
                    arms.iter().flat_map(|arm| [arm.params.into(), arm.tail.into()]).collect()
                }
            },
        }
    }

    fn parameter_children(parameter: &ExistentialParameter) -> impl Iterator<Item = EntityId> + '_ {
        parameter
            .annotations
            .iter()
            .map(|annotation| annotation.inner.into())
            .chain(std::iter::once(parameter.binder.into()))
    }

    fn binding_children(binding: &GenBind<TermId>) -> impl Iterator<Item = EntityId> {
        [
            Some(binding.binder.into()),
            binding.params.map(Into::into),
            binding.ty.map(Into::into),
            Some(binding.bindee.into()),
        ]
        .into_iter()
        .flatten()
    }

    /// Allocations can outlive a popped parser stack entry. Only nodes reachable
    /// from the returned root belong to that parse tree.
    pub(crate) fn reachable_from(&self, root: EntityId) -> HashSet<EntityId> {
        let mut reached = HashSet::new();
        let mut pending = vec![root];
        while let Some(entity) = pending.pop() {
            if reached.insert(entity) {
                pending.extend(self.children(entity));
            }
        }
        reached
    }

    /// Lower one parsed metadata tree to the span-free representation shared
    /// by the later compiler phases and metadata decoders.
    pub fn semantic_meta(&self, meta: MetaId) -> zydeco_syntax::Meta {
        match &self.metas[&meta] {
            | MetaNode::Ident(name) => zydeco_syntax::Meta::ident(name),
            | MetaNode::String(value) => zydeco_syntax::Meta::string(value),
            | MetaNode::Integer(value) => zydeco_syntax::Meta::integer(*value),
            | MetaNode::Apply { callee, args } => zydeco_syntax::Meta::apply(
                callee,
                args.iter().map(|argument| self.semantic_meta(*argument)),
            ),
        }
    }
}

/// Dense span storage for the entities issued by one textual parser.
#[derive(Clone, Default, Debug)]
pub struct SpanArena {
    key_space: Option<KeySpaceId>,
    categories: Vec<EntityCategory>,
    spans: Vec<Span>,
    map: Option<Arc<SourceMap>>,
}

mod impl_span_arena {
    use super::*;
    use std::ops::Index;

    impl SpanArena {
        /// Create empty span storage.
        pub fn new() -> Self {
            Self::default()
        }
        /// Associate an externally issued textual ID with a span.
        pub fn insert_new<Id>(&mut self, id: Id, span: Span)
        where
            Id: Into<EntityId>,
        {
            let (category, key_space, raw) = id.into().into_parts();
            match self.key_space {
                | Some(existing) => {
                    assert_eq!(existing, key_space, "span ID belongs to another parser")
                }
                | None => self.key_space = Some(key_space),
            }
            assert_eq!(
                raw.into_u32() as usize,
                self.spans.len(),
                "span IDs must follow the parser allocation sequence",
            );
            self.categories.push(category);
            self.spans.push(span);
        }
        /// Replace the span of an existing textual entity.
        pub fn replace<Id>(&mut self, id: Id, span: Span)
        where
            Id: Into<EntityId>,
        {
            let index = self.index_of(id.into()).expect("span ID not found");
            self.spans[index] = span;
        }
        /// Iterate over stored spans with their IDs.
        pub fn iter(&self) -> impl Iterator<Item = (EntityId, &Span)> {
            debug_assert_eq!(self.categories.len(), self.spans.len());
            let key_space = self.key_space;
            self.categories.iter().copied().zip(&self.spans).enumerate().map(
                move |(raw, (category, span))| {
                    let key_space = key_space.expect("a nonempty span arena has a key space");
                    let raw = u32::try_from(raw).expect("span arena exceeded the raw ID range");
                    (category.restore(key_space, RawIdx::from_u32(raw)), span)
                },
            )
        }
        /// Number of textual entities currently carrying spans.
        pub fn len(&self) -> usize {
            self.spans.len()
        }
        /// Whether no textual entity currently carries a span.
        pub fn is_empty(&self) -> bool {
            self.spans.is_empty()
        }
        /// Attach the source map that decodes the stored spans' address space.
        ///
        /// Template-local arenas keep `None`: their spans are file-relative and
        /// resolve through the template's own `FileMap`.
        pub fn attach_map(&mut self, map: Arc<SourceMap>) {
            self.map = Some(map);
        }
        /// The source map decoding the stored spans, when one was attached.
        pub fn source_map(&self) -> Option<&SourceMap> {
            self.map.as_deref()
        }
        /// Release geometric vector growth after parsing finishes.
        pub(crate) fn shrink_to_fit(&mut self) {
            self.categories.shrink_to_fit();
            self.spans.shrink_to_fit();
        }
        fn index_of(&self, entity: EntityId) -> Option<usize> {
            let (category, key_space, raw) = entity.into_parts();
            (self.key_space == Some(key_space))
                .then_some(raw.into_u32() as usize)
                .filter(|index| self.categories.get(*index) == Some(&category))
        }
    }

    impl Index<&EntityId> for SpanArena {
        type Output = Span;

        fn index(&self, entity: &EntityId) -> &Self::Output {
            let index = self.index_of(*entity).expect("span ID not found");
            &self.spans[index]
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use std::mem::size_of;

        #[test]
        fn span_arena_round_trips_dense_typed_ids() {
            let mut allocator = IdAllocator::<TextualScope>::new();
            let definition: DefId = allocator.alloc();
            let pattern: PatId = allocator.alloc();
            let copattern: CoPatId = allocator.alloc();
            let metadata: MetaId = allocator.alloc();
            let term: TermId = allocator.alloc();
            let expected =
                [definition.into(), pattern.into(), copattern.into(), metadata.into(), term.into()];
            let mut spans = SpanArena::new();

            spans.insert_new(definition, Span::new(0, 1));
            spans.insert_new(pattern, Span::new(1, 2));
            spans.insert_new(copattern, Span::new(2, 3));
            spans.insert_new(metadata, Span::new(3, 4));
            spans.insert_new(term, Span::new(4, 5));

            assert_eq!(size_of::<EntityCategory>(), 1);
            assert_eq!(spans.iter().map(|(entity, _)| entity).collect::<Vec<_>>(), expected);
            assert_eq!(spans[&EntityId::Meta(metadata)].range(), 3..4);
            assert_eq!(spans[&EntityId::Term(term)].range(), 4..5);

            spans.replace(pattern, Span::new(10, 20));
            assert_eq!(spans[&EntityId::Pat(pattern)].range(), 10..20);

            let wrong_category: PatId = restore_id(definition.key_space(), definition.raw());
            assert!(spans.index_of(wrong_category.into()).is_none());
        }

        #[test]
        #[should_panic(expected = "span IDs must follow the parser allocation sequence")]
        fn span_arena_rejects_allocation_gaps() {
            let mut allocator = IdAllocator::<TextualScope>::new();
            let _: DefId = allocator.alloc();
            let term: TermId = allocator.alloc();
            SpanArena::new().insert_new(term, Span::dummy());
        }

        #[test]
        #[should_panic(expected = "span ID belongs to another parser")]
        fn span_arena_rejects_another_parser_key_space() {
            let mut first_allocator = IdAllocator::<TextualScope>::new();
            let mut second_allocator = IdAllocator::<TextualScope>::new();
            let first: DefId = first_allocator.alloc();
            let second: PatId = second_allocator.alloc();
            let mut spans = SpanArena::new();
            spans.insert_new(first, Span::dummy());
            spans.insert_new(second, Span::dummy());
        }
    }
}
