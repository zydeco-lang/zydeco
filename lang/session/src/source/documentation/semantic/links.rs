use super::*;
use crate::source::{DocumentationExposureQuery, DocumentationPath};
use pulldown_cmark::{LinkType, TagEnd};
use thiserror::Error;

#[derive(Clone, Debug)]
pub enum DocumentationLinkTarget {
    Definition(s::DefId),
    Member { owner: s::DefId, path: DocumentationPath, declaration: Option<s::TermId> },
}

impl DocumentationLinkTarget {
    pub fn source_entity(&self) -> s::EntityId {
        match self {
            | Self::Definition(definition) => (*definition).into(),
            | Self::Member { owner, declaration, .. } => {
                declaration.map_or_else(|| (*owner).into(), Into::into)
            }
        }
    }
}

#[derive(Clone, Debug, Error, strum::IntoStaticStr)]
pub enum DocumentationLinkError {
    #[error(
        "semantic documentation links must use `[label](zydeco:name:Name)` or `[label](zydeco:member:Owner/path)`"
    )]
    #[strum(serialize = "doc.link.syntax")]
    Syntax,
    #[error("documentation authoring scope is unavailable")]
    #[strum(serialize = "doc.link.scope")]
    UnavailableScope,
    #[error("documentation name `{0}` is not in the authoring scope")]
    #[strum(serialize = "doc.link.name")]
    UnknownName(String),
    #[error("documentation member `{owner}/{path}` is not exposed by that owner")]
    #[strum(serialize = "doc.link.member")]
    UnknownMember { owner: String, path: DocumentationPath },
    #[error(transparent)]
    #[strum(serialize = "doc.link.exposure")]
    Exposure(#[from] crate::source::DocumentationExposureError),
}

impl DocumentationLinkError {
    pub fn code(&self) -> &'static str {
        self.into()
    }
}

#[derive(Clone, Debug)]
pub struct DocumentationLink {
    /// Destination bytes in stripped Markdown, retained for renderer-specific URLs.
    pub range: Range<usize>,
    /// Corresponding bytes in the authored source, including Unicode/CRLF mapping.
    pub source: Range<usize>,
    pub target: Result<DocumentationLinkTarget, DocumentationLinkError>,
}

#[derive(Clone, Debug)]
pub enum DocumentationDestination {
    Name(s::VarName),
    Member { owner: s::VarName, path: DocumentationPath },
}

impl DocumentationDestination {
    fn parse(destination: &str) -> Result<Self, DocumentationLinkError> {
        if let Some(name) = destination.strip_prefix("zydeco:name:").filter(|name| !name.is_empty())
        {
            return Ok(Self::Name(s::VarName(name.to_owned())));
        }
        if let Some(member) = destination.strip_prefix("zydeco:member:") {
            let (owner, selector) = member
                .split_once('/')
                .filter(|(owner, path)| !owner.is_empty() && !path.is_empty())
                .ok_or(DocumentationLinkError::Syntax)?;
            return Ok(Self::Member {
                owner: s::VarName(owner.to_owned()),
                path: DocumentationPath::parse(selector),
            });
        }
        Err(DocumentationLinkError::Syntax)
    }
}

pub struct DocumentationLinkSyntax {
    pub range: Range<usize>,
    pub destination: Result<DocumentationDestination, DocumentationLinkError>,
}

impl DocumentationLinkSyntax {
    pub fn collect(markdown: &str) -> Vec<Self> {
        let mut events = MarkdownParser::new(markdown).into_offset_iter();
        let mut links = Vec::new();
        while let Some((event, range)) = events.next() {
            let Event::Start(Tag::Link { dest_url, link_type, .. }) = event else { continue };
            // The parser's child ranges locate the end of the label, including
            // code spans and images. Searching for URL text could select a
            // repeated occurrence inside the label or optional link title.
            let mut label_end = range.start + 1;
            for (child, child_range) in events.by_ref() {
                if matches!(child, Event::End(TagEnd::Link)) {
                    break;
                }
                label_end = label_end.max(child_range.end);
            }
            if !dest_url.starts_with("zydeco:") {
                continue;
            }
            let destination = (link_type == LinkType::Inline)
                .then(|| {
                    let suffix =
                        markdown.get(label_end..range.end)?.strip_prefix("](")?.trim_start();
                    let suffix = suffix.strip_prefix('<').unwrap_or(suffix);
                    suffix.starts_with(dest_url.as_ref()).then_some(range.end - suffix.len())
                })
                .flatten();
            let (range, destination) = match destination {
                | Some(start) => {
                    (start..start + dest_url.len(), DocumentationDestination::parse(&dest_url))
                }
                | None => (range, Err(DocumentationLinkError::Syntax)),
            };
            links.push(Self { range, destination });
        }
        links
    }
}

impl DocumentationIndex {
    pub(super) fn resolve_links(
        &mut self, graph: &SourceGraph, spans: &t::SpanArena, scoped: &ScopedArena,
        statics: &StaticsArena,
    ) {
        let scopes = scoped
            .documentation_scopes
            .iter()
            .filter_map(|(term, scope)| {
                let origin = scoped.origins.source(&(*term).into())?;
                let (file, range) = spans.source_map()?.range(spans[&origin])?;
                Some((SourceAnchor { path: file.path(), range }, scope))
            })
            .collect::<HashMap<_, _>>();
        self.entries.iter_mut().for_each(|entry| {
            let Some(file) = graph
                .sources
                .iter()
                .find_map(|(_, file)| (file.path == entry.path).then_some(file))
            else {
                return;
            };
            let scope = scopes
                .get(&SourceAnchor { path: entry.path.clone(), range: entry.annotation.clone() })
                .copied();
            let resolver = DocumentationLinkResolver { scope, statics };
            entry.links = DocumentationLinkSyntax::collect(&entry.markdown)
                .into_iter()
                .map(|syntax| {
                    let DocumentationLinkSyntax { range, destination } = syntax;
                    let target = destination.and_then(|destination| resolver.resolve(destination));
                    let source = entry
                        .source_range(&file.source, range.clone())
                        .unwrap_or_else(|| entry.comment.clone());
                    DocumentationLink { range, source, target }
                })
                .collect();
        });
    }
}

struct DocumentationLinkResolver<'arena> {
    scope: Option<&'arena zydeco_surface::scoped::ScopeSnapshot>,
    statics: &'arena StaticsArena,
}

impl DocumentationLinkResolver<'_> {
    fn definition(&self, name: &str) -> Result<s::DefId, DocumentationLinkError> {
        self.scope
            .ok_or(DocumentationLinkError::UnavailableScope)?
            .definitions
            .iter()
            .find(|definition| definition.name.0 == name)
            .map(|definition| definition.definition)
            .ok_or_else(|| DocumentationLinkError::UnknownName(name.to_owned()))
    }

    fn resolve(
        &self, destination: DocumentationDestination,
    ) -> Result<DocumentationLinkTarget, DocumentationLinkError> {
        match destination {
            | DocumentationDestination::Name(name) => {
                self.definition(&name.0).map(DocumentationLinkTarget::Definition)
            }
            | DocumentationDestination::Member { owner: name, path } => {
                let owner = self.definition(&name.0)?;
                let member = DocumentationExposureQuery::new(self.statics)
                    .of_definition(owner)?
                    .into_iter()
                    .find(|member| member.path == path)
                    .ok_or_else(|| DocumentationLinkError::UnknownMember {
                        owner: name.0.clone(),
                        path: path.clone(),
                    })?;
                Ok(DocumentationLinkTarget::Member { owner, path, declaration: member.declaration })
            }
        }
    }
}
