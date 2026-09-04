use crate::source::{SourceGraph, SourceTemplate};
use pulldown_cmark::{Event, Parser as MarkdownParser, Tag};
use std::{
    collections::{HashMap, HashSet},
    ops::Range,
    path::{Path, PathBuf},
    sync::Arc,
};
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::{
    scoped::{arena::ScopedArena, syntax as s},
    textual::{DocumentationSite, syntax as t},
};
use zydeco_utils::arena::ArenaAccess;

mod links;
pub use links::*;

/// An authored site in one immutable documentation index.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct DocumentationId(usize);

/// The source construct described by an attachment. Names are display data;
/// lookup uses the compiler's entity identities and exact source origins.
#[derive(Clone, Debug)]
pub enum DocumentationSubject {
    Expression,
    Overview,
    Binding(s::VarName),
    Member(s::FieldName),
}

/// Authored prose and its original location, independent of any use-site type.
#[derive(Clone, Debug)]
pub struct Documentation {
    pub id: DocumentationId,
    pub subject: DocumentationSubject,
    pub path: PathBuf,
    pub range: Range<usize>,
    pub payload: Range<usize>,
    pub markdown: Arc<str>,
    pub links: Vec<DocumentationLink>,
    annotation: Range<usize>,
    comment: Range<usize>,
}

impl Documentation {
    pub fn summary(&self) -> &str {
        self.summary_range().map_or("", |range| self.markdown[range].trim())
    }

    fn summary_range(&self) -> Option<Range<usize>> {
        let mut depth = 0usize;
        for (event, range) in MarkdownParser::new(&self.markdown).into_offset_iter() {
            match event {
                | Event::Start(Tag::Paragraph) if depth == 0 => return Some(range),
                | Event::Start(_) => depth += 1,
                | Event::End(_) => depth -= 1,
                | _ => {}
            }
        }
        None
    }

    pub fn source_range(&self, source: &str, range: Range<usize>) -> Option<Range<usize>> {
        t::TextBlock { text: Arc::clone(&self.markdown), range: self.comment.clone() }
            .source_range(source, range)
    }

    pub fn markdown_with_links(
        &self, summary: bool, resolve: &mut impl FnMut(&DocumentationLinkTarget) -> Option<String>,
    ) -> String {
        let range =
            if summary { self.summary_range().unwrap_or(0..0) } else { 0..self.markdown.len() };
        let mut text = self.markdown[range.clone()].to_owned();
        self.links
            .iter()
            .rev()
            .filter(|link| range.start <= link.range.start && link.range.end <= range.end)
            .for_each(|link| {
                if let Ok(target) = &link.target
                    && let Some(url) = resolve(target)
                {
                    text.replace_range(
                        link.range.start - range.start..link.range.end - range.start,
                        &url,
                    );
                }
            });
        text.trim().to_owned()
    }
}

/// Documentation selected for an occurrence, in context-before-origin order.
#[derive(Clone, Debug, Default)]
pub struct DocumentationContent<'index> {
    entries: Vec<&'index Documentation>,
}

impl<'index> DocumentationContent<'index> {
    pub fn entries(&self) -> impl Iterator<Item = &'index Documentation> + '_ {
        self.entries.iter().copied()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn markdown(&self) -> String {
        self.entries().map(|entry| entry.markdown.as_ref()).collect::<Vec<_>>().join("\n\n")
    }

    pub fn summary(&self) -> String {
        self.entries().map(Documentation::summary).collect::<Vec<_>>().join("\n\n")
    }

    pub fn markdown_with_links(
        &self, summary: bool, mut resolve: impl FnMut(&DocumentationLinkTarget) -> Option<String>,
    ) -> String {
        self.entries()
            .map(|entry| entry.markdown_with_links(summary, &mut resolve))
            .collect::<Vec<_>>()
            .join("\n\n")
    }
}

/// A revision-owned graph joining authored sites to resolved subjects.
///
/// Edges follow source bindings and checker-recorded member declarations.
/// Ordinary expression children do not inherit their enclosing documentation.
#[derive(Clone, Debug, Default)]
pub struct DocumentationIndex {
    entries: Vec<Documentation>,
    direct: HashMap<s::EntityId, Vec<DocumentationId>>,
    origins: HashMap<s::EntityId, DocumentationRelation>,
}

#[derive(Clone, Copy, Debug)]
enum DocumentationRelation {
    Alias(s::EntityId),
    Contract { interface: s::TermId, implementation: s::TermId },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct SourceAnchor {
    path: PathBuf,
    range: Range<usize>,
}

impl DocumentationIndex {
    pub(crate) fn new(
        graph: &SourceGraph, spans: &t::SpanArena, scoped: &ScopedArena, statics: &StaticsArena,
    ) -> Self {
        let mut index = Self::default();
        let mut attachments: HashMap<SourceAnchor, Vec<DocumentationId>> = HashMap::new();
        graph.documentation().into_iter().for_each(|entry| {
            let id = DocumentationId(index.entries.len());
            let source = SourceDocumentation::with_template(entry.file);
            let Some(documentation) = source.entry(id, entry.site) else { return };
            source.targets(entry.site).into_iter().for_each(|entity| {
                attachments.entry(source.anchor(entity)).or_default().push(id);
            });
            index.entries.push(documentation);
        });
        if index.entries.is_empty() {
            return index;
        }
        let entities = scoped
            .defs
            .iter()
            .map(|(id, _)| s::EntityId::Def(*id))
            .chain(scoped.terms.iter().map(|(id, _)| s::EntityId::Term(id)));
        entities.for_each(|entity| {
            let Some(origin) = scoped.origins.source(&entity) else { return };
            let Some((file, range)) = spans.source_map().and_then(|map| map.range(spans[&origin]))
            else {
                return;
            };
            if let Some(ids) = attachments.get(&SourceAnchor { path: file.path(), range }) {
                index.direct.insert(entity, ids.clone());
            }
        });
        scoped.terms.iter().for_each(|(term, syntax)| {
            let alias = match syntax {
                | s::Term::Var(definition) => Some((*definition).into()),
                | s::Term::Meta(meta) => Some(meta.1.into()),
                | s::Term::SourceBoundary(s::SourceBoundary(inner))
                | s::Term::SignatureBoundary(s::SignatureBoundary(inner))
                | s::Term::Sealed(s::Sealed(inner))
                | s::Term::Residual(s::Residual(inner)) => Some((*inner).into()),
                | s::Term::Named(s::Named(_, inner)) => Some((*inner).into()),
                | s::Term::Block(s::Block(inner)) => Some(
                    scoped.blocks.get(&term).map_or(*inner, |block| block.body.residual).into(),
                ),
                | s::Term::Ann(s::Ann { tm, ty }) => {
                    // Inference placeholders share the binding's source span, but do
                    // not establish a public contract that can replace its origin.
                    if matches!(&scoped.terms[ty], s::Term::Hole(_)) {
                        Some((*tm).into())
                    } else {
                        index.origins.insert(
                            term.into(),
                            DocumentationRelation::Contract { interface: *ty, implementation: *tm },
                        );
                        None
                    }
                }
                | s::Term::Let(binding) => {
                    index.binding(scoped, binding.binder, binding.bindee);
                    Some(binding.tail.into())
                }
                | s::Term::RecGroup(group) => {
                    group
                        .definitions
                        .iter()
                        .for_each(|binding| index.binding(scoped, binding.binder, binding.bindee));
                    Some(group.tail.into())
                }
                | s::Term::Proj(_) => {
                    statics.member_provenance.declaration(term.into()).map(Into::into)
                }
                | _ => None,
            };
            if let Some(origin) = alias {
                index.origins.insert(term.into(), DocumentationRelation::Alias(origin));
            }
        });
        scoped.blocks.iter().for_each(|(_, block)| {
            block.context.nodes.iter().flat_map(|(_, node)| node.bindings()).for_each(|binding| {
                if let s::BindingForm::Definition(definition) = &binding.inner {
                    index.binding(scoped, definition.binder, definition.bindee);
                }
            });
        });
        scoped.pats.iter().for_each(|(pattern, syntax)| {
            if let s::Pattern::Project(s::ProjectionPattern(_, payload)) = syntax
                && let Some(definition) = Self::simple_definition(scoped, *payload)
                && let Some(origin) = statics.member_provenance.declaration(pattern.into())
            {
                index
                    .origins
                    .insert(definition.into(), DocumentationRelation::Alias(origin.into()));
            }
        });
        index.resolve_links(graph, spans, scoped, statics);
        index
    }

    /// Retain only attachments present in the recovered current source tree.
    /// No semantic edges or dependency facts are guessed during this fallback.
    pub(crate) fn recover(path: &Path, text: &str) -> Self {
        let mut parser = t::Parser::new();
        let Some(unit) =
            zydeco_surface::textual::RecoveringParser::new(text).source(&mut parser).syntax
        else {
            return Self::default();
        };
        let source = SourceDocumentation { path, arena: &parser.arena, spans: &parser.spans };
        let entries = unit
            .documentation(&parser.arena, &parser.spans)
            .iter()
            .filter(|site| {
                site.directive
                    .comment
                    .as_ref()
                    .is_some_and(|comment| !comment.text.trim().is_empty())
            })
            .enumerate()
            .filter_map(|(id, site)| source.entry(DocumentationId(id), site))
            .collect();
        Self { entries, ..Self::default() }
    }

    fn binding(&mut self, scoped: &ScopedArena, binder: s::PatId, bindee: s::TermId) {
        if let Some(definition) = Self::simple_definition(scoped, binder) {
            self.origins.insert(definition.into(), DocumentationRelation::Alias(bindee.into()));
        }
    }

    fn simple_definition(scoped: &ScopedArena, pattern: s::PatId) -> Option<s::DefId> {
        match &scoped.pats[&pattern] {
            | s::Pattern::Var(definition) => Some(*definition),
            | s::Pattern::Ann(s::Ann { tm, .. }) => Self::simple_definition(scoped, *tm),
            | _ => None,
        }
    }

    pub fn entries(&self) -> &[Documentation] {
        &self.entries
    }

    pub fn subjects(&self, document: DocumentationId) -> impl Iterator<Item = s::EntityId> + '_ {
        self.direct.iter().filter_map(move |(subject, documents)| {
            documents.contains(&document).then_some(*subject)
        })
    }

    pub fn for_definition(&self, definition: s::DefId) -> DocumentationContent<'_> {
        self.content(definition.into())
    }

    pub fn for_term(&self, term: s::TermId) -> DocumentationContent<'_> {
        self.content(term.into())
    }

    pub fn at(&self, path: &Path, offset: usize) -> DocumentationContent<'_> {
        // The annotation header and its prose are direct source facts. Its
        // enclosing payload may contain unrelated subjects, so uses within it
        // must come through for_term/for_definition instead of containment.
        let entry = self
            .entries
            .iter()
            .filter(|entry| {
                entry.path == path && (entry.range.start..entry.payload.start).contains(&offset)
            })
            .min_by_key(|entry| entry.range.len());
        DocumentationContent { entries: entry.into_iter().collect() }
    }

    fn content(&self, entity: s::EntityId) -> DocumentationContent<'_> {
        let mut visited = HashSet::new();
        let mut seen = HashSet::new();
        let entries = self
            .collect(entity, &mut visited)
            .into_iter()
            .filter(|id| seen.insert(*id))
            .map(|id| &self.entries[id.0])
            .collect();
        DocumentationContent { entries }
    }

    fn collect(
        &self, entity: s::EntityId, active: &mut HashSet<s::EntityId>,
    ) -> Vec<DocumentationId> {
        if !active.insert(entity) {
            return Vec::new();
        }
        let inherited = match self.origins.get(&entity) {
            | Some(DocumentationRelation::Alias(origin)) => self.collect(*origin, active),
            | Some(DocumentationRelation::Contract { interface, implementation }) => {
                let contract = self.collect((*interface).into(), active);
                if contract.is_empty() {
                    self.collect((*implementation).into(), active)
                } else {
                    contract
                }
            }
            | None => Vec::new(),
        };
        active.remove(&entity);
        self.direct.get(&entity).into_iter().flatten().copied().chain(inherited).collect()
    }
}

struct SourceDocumentation<'file> {
    path: &'file Path,
    arena: &'file t::TextArena,
    spans: &'file t::SpanArena,
}

impl<'file> SourceDocumentation<'file> {
    fn with_template(file: &'file SourceTemplate) -> Self {
        Self { path: &file.path, arena: &file.arena, spans: &file.spans }
    }

    fn entry(&self, id: DocumentationId, site: &DocumentationSite) -> Option<Documentation> {
        let comment =
            site.directive.comment.as_ref().filter(|comment| !comment.text.trim().is_empty())?;
        Some(Documentation {
            id,
            subject: self.subject(site),
            path: self.path.to_owned(),
            range: comment.range.start..site.directive.span.range().end,
            payload: self.spans[&site.payload.into()].range(),
            markdown: Arc::clone(&comment.text),
            links: Vec::new(),
            annotation: site.directive.span.range(),
            comment: comment.range.clone(),
        })
    }

    fn anchor(&self, entity: t::EntityId) -> SourceAnchor {
        SourceAnchor { path: self.path.to_owned(), range: self.spans[&entity].range() }
    }

    fn transparent_terms(&self, term: t::TermId) -> Vec<t::TermId> {
        std::iter::successors(Some(term), |term| match &self.arena.terms[term] {
            | t::Term::Meta(t::MetaTerm(_, inner)) => Some(*inner),
            | t::Term::Paren(t::Paren(items)) if items.len() == 1 => Some(items[0]),
            | _ => None,
        })
        .collect()
    }

    fn simple_definition(&self, pattern: t::PatId) -> Option<t::DefId> {
        match &self.arena.pats[&pattern] {
            | t::Pattern::Var(definition) => Some(*definition),
            | t::Pattern::Ann(t::Ann { tm, .. }) => self.simple_definition(*tm),
            | t::Pattern::Paren(t::Paren(items)) if items.len() == 1 => {
                self.simple_definition(items[0])
            }
            | _ => None,
        }
    }

    fn subject(&self, site: &DocumentationSite) -> DocumentationSubject {
        let terms = self.transparent_terms(site.payload);
        match &self.arena.terms[terms.last().unwrap()] {
            | t::Term::ContextBind(binding) => self
                .simple_definition(binding.binding.binder)
                .map(|id| DocumentationSubject::Binding(self.arena.defs[&id].clone()))
                .unwrap_or(DocumentationSubject::Expression),
            | t::Term::Let(binding) => self
                .simple_definition(binding.binding.binder)
                .map(|id| DocumentationSubject::Binding(self.arena.defs[&id].clone()))
                .unwrap_or(DocumentationSubject::Expression),
            | t::Term::Named(t::Named(name, _)) | t::Term::Label(t::Label(name, _)) => {
                DocumentationSubject::Member(name.clone())
            }
            | t::Term::Block(_) => DocumentationSubject::Overview,
            | _ => DocumentationSubject::Expression,
        }
    }

    fn targets(&self, site: &DocumentationSite) -> Vec<t::EntityId> {
        let terms = self.transparent_terms(site.term);
        let binder = match &self.arena.terms[terms.last().unwrap()] {
            | t::Term::Let(binding) => Some(binding.binding.binder),
            | t::Term::ContextBind(binding) => Some(binding.binding.binder),
            | _ => None,
        };
        if let Some(binder) = binder {
            return self.simple_definition(binder).map(Into::into).into_iter().collect();
        }
        terms.into_iter().map(Into::into).collect()
    }
}

#[cfg(test)]
mod tests;
