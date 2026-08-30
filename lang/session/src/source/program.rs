use crate::source::{SourceGraph, SourceId, SourceKind, TextualProgramError};
use std::collections::HashMap;
use std::sync::Arc;
use zydeco_surface::textual::syntax as t;
use zydeco_utils::span::{BytePos, FileSource, SourceMap, Span};

/// The complete program in textual syntax, merged from one source graph.
///
/// `SourceGraph::parse` clones every reachable template into one fresh
/// arena: imports become source boundaries, companion signatures become
/// annotations, and `@[literal]` splices become string literals.
pub struct TextualProgram {
    pub spans: t::SpanArena,
    pub arena: t::TextArena,
    pub unit: t::SourceUnit,
}

impl SourceGraph {
    pub fn parse(&self) -> Result<TextualProgram, TextualProgramError> {
        TextualProgramBuilder::new(self).build()
    }
}

struct TextualProgramBuilder<'graph> {
    graph: &'graph SourceGraph,
    parser: t::Parser,
    /// Global base of each file's span range, assigned in provider order.
    bases: HashMap<SourceId, BytePos>,
}

impl<'graph> TextualProgramBuilder<'graph> {
    fn new(graph: &'graph SourceGraph) -> Self {
        Self { graph, parser: t::Parser::new(), bases: Self::assign_bases(graph) }
    }

    /// Give every reachable file a disjoint global offset range, starting
    /// after the dummy position `0`.
    fn assign_bases(graph: &'graph SourceGraph) -> HashMap<SourceId, BytePos> {
        let mut base = 1u32;
        graph
            .provider_order()
            .into_iter()
            .map(|source| {
                let len = graph.sources[&source].source.len();
                let assigned = BytePos(base);
                base += u32::try_from(len).expect("program address space exceeds u32");
                (source, assigned)
            })
            .collect()
    }

    fn build(mut self) -> Result<TextualProgram, TextualProgramError> {
        let root = self.source(self.graph.root)?;
        let map = self.source_map();
        let (mut spans, arena) = self.parser.finish();
        spans.attach_map(Arc::new(map));
        Ok(TextualProgram { spans, arena, unit: t::SourceUnit { root } })
    }

    /// The address space of this program: one entry per file, in base order.
    fn source_map(&self) -> SourceMap {
        let order = self.bases_order();
        SourceMap::from_sources(order.into_iter().map(|source| {
            let file = &self.graph.sources[&source];
            FileSource {
                path: Some(Arc::new(file.path.clone())),
                source: file.source.as_str().into(),
            }
        }))
    }

    /// Files ordered by their assigned base.
    fn bases_order(&self) -> Vec<SourceId> {
        let mut order: Vec<_> = self.graph.provider_order();
        order.sort_by_key(|source| self.bases[source]);
        order
    }

    fn source(&mut self, source: SourceId) -> Result<t::TermId, TextualProgramError> {
        let file = &self.graph.sources[&source];
        let root = file.unit.root;
        let kind = file.kind();
        let signature = file.signature;
        let body = self.term(source, root)?;
        let body = match kind {
            | SourceKind::Signature => self
                .parser
                .term(self.span(source, root.into()).make(t::SignatureBoundary(body).into())),
            | SourceKind::Implementation | SourceKind::Program => body,
        };
        let Some(signature) = signature else {
            return Ok(body);
        };
        let ty = self.source(signature)?;
        let signature_root = self.graph.sources[&signature].unit.root;
        Ok(self
            .parser
            .term(self.span(signature, signature_root.into()).make(t::Ann { tm: body, ty }.into())))
    }

    fn span(&self, source: SourceId, entity: t::EntityId) -> Span {
        self.graph.sources[&source].spans[&entity].rebase(self.bases[&source])
    }

    fn definition(&mut self, source: SourceId, definition: t::DefId) -> t::DefId {
        let file = &self.graph.sources[&source];
        let name = file.arena.defs[&definition].clone();
        self.parser.def(self.span(source, definition.into()).make(name))
    }

    fn pattern(
        &mut self, source: SourceId, pattern: t::PatId,
    ) -> Result<t::PatId, TextualProgramError> {
        let syntax = self.graph.sources[&source].arena.pats[&pattern].clone();
        let syntax = match syntax {
            | t::Pattern::Ann(t::Ann { tm, ty }) => {
                t::Ann { tm: self.pattern(source, tm)?, ty: self.term(source, ty)? }.into()
            }
            | t::Pattern::Manifest(t::ManifestPattern { binder, definition }) => {
                t::ManifestPattern {
                    binder: self.pattern(source, binder)?,
                    definition: self.term(source, definition)?,
                }
                .into()
            }
            | t::Pattern::Hole(_) => t::Hole.into(),
            | t::Pattern::Var(definition) => self.definition(source, definition).into(),
            | t::Pattern::Named(t::Named(name, inner)) => {
                t::Named(name, self.pattern(source, inner)?).into()
            }
            | t::Pattern::Ctor(t::Ctor(name, inner)) => {
                t::Ctor(name, self.pattern(source, inner)?).into()
            }
            | t::Pattern::Project(t::ProjectionPattern(field, inner)) => {
                t::ProjectionPattern(field, self.pattern(source, inner)?).into()
            }
            | t::Pattern::Alias(t::Alias(patterns)) => {
                let patterns = patterns
                    .into_iter()
                    .map(|pattern| self.pattern(source, pattern))
                    .collect::<Result<Vec<_>, _>>()?;
                t::Alias(t::ConsN::from_vec(patterns).unwrap()).into()
            }
            | t::Pattern::Paren(t::Paren(patterns)) => t::Paren(
                patterns
                    .into_iter()
                    .map(|pattern| self.pattern(source, pattern))
                    .collect::<Result<Vec<_>, _>>()?,
            )
            .into(),
        };
        Ok(self.parser.pat(self.span(source, pattern.into()).make(syntax)))
    }

    fn copattern(
        &mut self, source: SourceId, pattern: t::CoPatId,
    ) -> Result<t::CoPatId, TextualProgramError> {
        let syntax = self.graph.sources[&source].arena.copats[&pattern].clone();
        let syntax = match syntax {
            | t::CoPattern::Pat(pattern) => self.pattern(source, pattern)?.into(),
            | t::CoPattern::Dtor(name) => name.into(),
            | t::CoPattern::App(t::Appli(patterns)) => t::Appli(
                patterns
                    .into_iter()
                    .map(|pattern| self.copattern(source, pattern))
                    .collect::<Result<Vec<_>, _>>()?,
            )
            .into(),
        };
        Ok(self.parser.copat(self.span(source, pattern.into()).make(syntax)))
    }

    fn existential_parameter(
        &mut self, source: SourceId, parameter: t::ExistentialParameter,
    ) -> Result<t::ExistentialParameter, TextualProgramError> {
        let t::ExistentialParameter { annotations, binder } = parameter;
        let binder = self.pattern(source, binder)?;
        Ok(t::ExistentialParameter { annotations, binder })
    }

    fn pack_parameter(
        &mut self, source: SourceId, parameter: t::PackParameter,
    ) -> Result<t::PackParameter, TextualProgramError> {
        let t::PackParameter { parameter, evidence } = parameter;
        let parameter = self.existential_parameter(source, parameter)?;
        let evidence = evidence.map(|evidence| self.term(source, evidence)).transpose()?;
        Ok(t::PackParameter { parameter, evidence })
    }

    fn binding(
        &mut self, source: SourceId, binding: t::GenBind<t::TermId>,
    ) -> Result<t::GenBind<t::TermId>, TextualProgramError> {
        let t::GenBind { fix, comp, binder, params, ty, bindee } = binding;
        Ok(t::GenBind {
            fix,
            comp,
            binder: self.pattern(source, binder)?,
            params: params.map(|params| self.copattern(source, params)).transpose()?,
            ty: ty.map(|ty| self.term(source, ty)).transpose()?,
            bindee: self.term(source, bindee)?,
        })
    }

    fn term(
        &mut self, source: SourceId, term: t::TermId,
    ) -> Result<t::TermId, TextualProgramError> {
        let file = &self.graph.sources[&source];
        let syntax = file.arena.terms[&term].clone();
        if let t::Term::Meta(t::MetaT(meta, _)) = &syntax
            && meta.is("import")
        {
            return self.import(source, term);
        }
        if let t::Term::Meta(t::MetaT(meta, _)) = &syntax
            && meta.is("literal")
        {
            return self.literal(source, term);
        }

        let syntax = match syntax {
            | t::Term::Meta(t::MetaT(meta, inner)) => {
                t::MetaT(meta, self.term(source, inner)?).into()
            }
            | t::Term::SourceBoundary(t::SourceBoundary(inner)) => {
                t::SourceBoundary(self.term(source, inner)?).into()
            }
            | t::Term::SignatureBoundary(t::SignatureBoundary(inner)) => {
                t::SignatureBoundary(self.term(source, inner)?).into()
            }
            | t::Term::Ann(t::Ann { tm, ty }) => {
                t::Ann { tm: self.term(source, tm)?, ty: self.term(source, ty)? }.into()
            }
            | t::Term::Hole(_) => t::Hole.into(),
            | t::Term::Var(name) => name.into(),
            | t::Term::Named(t::Named(name, inner)) => {
                t::Named(name, self.term(source, inner)?).into()
            }
            | t::Term::Label(t::Label(name, inner)) => {
                t::Label(name, self.term(source, inner)?).into()
            }
            | t::Term::Paren(t::Paren(terms)) => t::Paren(
                terms
                    .into_iter()
                    .map(|term| self.term(source, term))
                    .collect::<Result<Vec<_>, _>>()?,
            )
            .into(),
            | t::Term::Abs(t::Abs(pattern, body)) => {
                t::Abs(self.copattern(source, pattern)?, self.term(source, body)?).into()
            }
            | t::Term::App(t::Appli(terms)) => t::Appli(
                terms
                    .into_iter()
                    .map(|term| self.term(source, term))
                    .collect::<Result<Vec<_>, _>>()?,
            )
            .into(),
            | t::Term::Fix(t::Fix(pattern, body)) => {
                t::Fix(self.pattern(source, pattern)?, self.term(source, body)?).into()
            }
            | t::Term::Pi(t::Pi(pattern, body)) => {
                t::Pi(self.copattern(source, pattern)?, self.term(source, body)?).into()
            }
            | t::Term::Forall(t::Forall(pattern, body)) => {
                t::Forall(self.copattern(source, pattern)?, self.term(source, body)?).into()
            }
            | t::Term::Arrow(t::Arrow(input, output)) => {
                t::Arrow(self.term(source, input)?, self.term(source, output)?).into()
            }
            | t::Term::Sigma(t::Sigma(pattern, body)) => {
                t::Sigma(self.copattern(source, pattern)?, self.term(source, body)?).into()
            }
            | t::Term::Exists(t::Exists { parameters, body }) => t::Exists {
                parameters: parameters
                    .into_iter()
                    .map(|parameter| self.existential_parameter(source, parameter))
                    .collect::<Result<Vec<_>, _>>()?,
                body: self.term(source, body)?,
            }
            .into(),
            | t::Term::Pack(t::Pack { parameters, body }) => t::Pack {
                parameters: parameters
                    .into_iter()
                    .map(|parameter| self.pack_parameter(source, parameter))
                    .collect::<Result<Vec<_>, _>>()?,
                body: self.term(source, body)?,
            }
            .into(),
            | t::Term::Prod(t::Prod(left, right)) => {
                t::Prod(self.term(source, left)?, self.term(source, right)?).into()
            }
            | t::Term::Thunk(t::Thunk(body)) => t::Thunk(self.term(source, body)?).into(),
            | t::Term::Force(t::Force(body)) => t::Force(self.term(source, body)?).into(),
            | t::Term::Ret(t::Return(body)) => t::Return(self.term(source, body)?).into(),
            | t::Term::Do(t::Bind { binder, bindee, tail }) => t::Bind {
                binder: self.pattern(source, binder)?,
                bindee: self.term(source, bindee)?,
                tail: self.term(source, tail)?,
            }
            .into(),
            | t::Term::Let(t::GenLet { binding, tail }) => t::GenLet {
                binding: self.binding(source, binding)?,
                tail: self.term(source, tail)?,
            }
            .into(),
            | t::Term::Param(t::Param { binder, placement, tail }) => t::Param {
                binder: self.pattern(source, binder)?,
                placement,
                tail: self.term(source, tail)?,
            }
            .into(),
            | t::Term::ContextBind(t::ContextBind { mode, binding, placement, tail }) => {
                t::ContextBind {
                    mode,
                    binding: self.binding(source, binding)?,
                    placement,
                    tail: self.term(source, tail)?,
                }
                .into()
            }
            | t::Term::Block(t::Block(body)) => t::Block(self.term(source, body)?).into(),
            | t::Term::Data(t::Data { arms }) => t::Data {
                arms: arms
                    .into_iter()
                    .map(|t::DataArm { name, param }| {
                        Ok(t::DataArm { name, param: self.term(source, param)? })
                    })
                    .collect::<Result<Vec<_>, TextualProgramError>>()?,
            }
            .into(),
            | t::Term::CoData(t::CoData { arms }) => t::CoData {
                arms: arms
                    .into_iter()
                    .map(|t::CoDataArm { name, params, out }| {
                        Ok(t::CoDataArm {
                            name,
                            params: params
                                .map(|params| self.copattern(source, params))
                                .transpose()?,
                            out: self.term(source, out)?,
                        })
                    })
                    .collect::<Result<Vec<_>, TextualProgramError>>()?,
            }
            .into(),
            | t::Term::Ctor(t::Ctor(name, body)) => t::Ctor(name, self.term(source, body)?).into(),
            | t::Term::Match(t::Match { scrut, arms }) => t::Match {
                scrut: self.term(source, scrut)?,
                arms: arms
                    .into_iter()
                    .map(|t::Matcher { binder, tail }| {
                        Ok(t::Matcher {
                            binder: self.pattern(source, binder)?,
                            tail: self.term(source, tail)?,
                        })
                    })
                    .collect::<Result<Vec<_>, TextualProgramError>>()?,
            }
            .into(),
            | t::Term::CoMatch(t::CoMatchParam { arms }) => t::CoMatchParam {
                arms: arms
                    .into_iter()
                    .map(|t::CoMatcherParam { params, tail }| {
                        Ok(t::CoMatcherParam {
                            params: self.copattern(source, params)?,
                            tail: self.term(source, tail)?,
                        })
                    })
                    .collect::<Result<Vec<_>, TextualProgramError>>()?,
            }
            .into(),
            | t::Term::Dtor(t::Dtor(body, name)) => t::Dtor(self.term(source, body)?, name).into(),
            | t::Term::Proj(t::Proj(body, name)) => t::Proj(self.term(source, body)?, name).into(),
            | t::Term::Lit(literal) => literal.into(),
        };
        Ok(self.parser.term(self.span(source, term.into()).make(syntax)))
    }

    fn import(
        &mut self, source: SourceId, term: t::TermId,
    ) -> Result<t::TermId, TextualProgramError> {
        let file = &self.graph.sources[&source];
        let import = file
            .imports
            .iter()
            .find(|import| self.graph.imports[import].term == term)
            .copied()
            .ok_or_else(|| TextualProgramError::MissingImport { path: file.path.clone(), term })?;
        let imported = self.graph.imports[&import].imported;
        let body = self.source(imported)?;
        Ok(self.parser.term(self.span(source, term.into()).make(t::SourceBoundary(body).into())))
    }

    fn literal(
        &mut self, source: SourceId, term: t::TermId,
    ) -> Result<t::TermId, TextualProgramError> {
        let file = &self.graph.sources[&source];
        let text = file
            .literals
            .iter()
            .find(|site| site.term == term)
            .map(|site| site.directive.text.text.as_ref())
            .ok_or_else(|| TextualProgramError::MissingLiteralText {
                path: file.path.clone(),
                term,
            })?;
        let syntax = t::Term::Lit(t::Literal::String(text.into()));
        Ok(self.parser.term(self.span(source, term.into()).make(syntax)))
    }
}
