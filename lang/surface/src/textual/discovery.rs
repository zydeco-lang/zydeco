use super::source::*;
use super::syntax::*;
use crate::diagnostic::Diagnostics;
use crate::metadata::{DiscoveryAnnotationError, DiscoveryRule, MetadataKind};
use thiserror::Error;

#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum DiscoveryDirectiveError {
    #[error("discover at {span} must annotate the file root")]
    Placement { span: Span },
    #[error("duplicate discover annotation at {span}; first declared at {first}")]
    Duplicate { span: Span, first: Span },
    #[error("invalid discovery annotation at {span}: {source}")]
    Invalid { span: Span, source: DiscoveryAnnotationError },
}

impl DiscoveryDirectiveError {
    pub fn span(&self) -> Span {
        match self {
            | Self::Placement { span }
            | Self::Duplicate { span, .. }
            | Self::Invalid { span, .. } => *span,
        }
    }
}

/// File-level placement and rule validation, independent of other source directives.
pub struct DiscoveryAnalyzer {
    root_meta: Option<MetaId>,
    sites: Vec<(MetaId, Span)>,
    result: SourceAnalysis<Vec<Sp<DiscoveryRule>>, DiscoveryDirectiveError>,
}

impl DiscoveryAnalyzer {
    pub fn new(source: SourceView<'_>) -> Self {
        Self {
            root_meta: source.arena.annotation(source.unit.root, MetadataKind::Discover),
            sites: Vec::new(),
            result: SourceAnalysis::default(),
        }
    }

    fn decode(
        annotation: &TermAnnotation<'_>, source: SourceView<'_>,
    ) -> Result<Vec<Sp<DiscoveryRule>>, DiscoveryDirectiveError> {
        let meta = annotation.metadata;
        let rules = DiscoveryRule::decode(annotation.semantic.arguments()).map_err(|error| {
            let index = match &error {
                | DiscoveryAnnotationError::Rule { index }
                | DiscoveryAnnotationError::Pattern { index, .. } => *index,
            };
            let argument = source.arena.metas[&meta].arguments()[index];
            let at = match &error {
                | DiscoveryAnnotationError::Pattern { pattern, .. } => {
                    source.arena.metas[&argument].arguments()[*pattern]
                }
                | _ => argument,
            };
            DiscoveryDirectiveError::Invalid {
                span: source.spans[&EntityId::Meta(at)],
                source: error,
            }
        })?;
        Ok(rules
            .into_iter()
            .zip(
                source.arena.metas[&meta]
                    .arguments()
                    .iter()
                    .flat_map(|rule| source.arena.metas[rule].arguments()),
            )
            .map(|(inner, meta)| Sp { inner, info: source.spans[&EntityId::Meta(*meta)] })
            .collect())
    }
}

impl SourceAnalyzer for DiscoveryAnalyzer {
    type Output = SourceAnalysis<Vec<Sp<DiscoveryRule>>, DiscoveryDirectiveError>;
    fn interests(&self) -> SourceInterest {
        SourceInterest {
            annotations: [MetadataKind::Discover].into(),
            reachability: true,
            ..Default::default()
        }
    }
    fn observe(&mut self, event: &SourceEvent<'_>, source: SourceView<'_>) {
        if let SourceEvent::Term(annotation) = event
            && annotation.kind == MetadataKind::Discover
            && annotation.reachable
        {
            self.sites
                .push((annotation.metadata, source.spans[&EntityId::Meta(annotation.metadata)]));
            match Self::decode(annotation, source) {
                | Ok(rules) => self.result.facts.extend(rules),
                | Err(error) => self.result.diagnostics.push(error),
            }
        }
    }
    fn finish(mut self) -> Self::Output {
        self.sites.sort_by_key(|(_, span)| span.lo());
        if let Some((meta, first)) = self.sites.first() {
            if self.sites.len() == 1 && self.root_meta != Some(*meta) {
                self.result.diagnostics.push(DiscoveryDirectiveError::Placement { span: *first });
            }
            self.result.diagnostics.extend(self.sites.iter().skip(1).map(|(_, span)| {
                DiscoveryDirectiveError::Duplicate { span: *span, first: *first }
            }));
        }
        self.result.facts.sort_by_key(|rule| rule.info.lo());
        self.result
    }
}

impl SourceUnit {
    /// Parse discovery instructions only; ordinary source loading never expands them.
    pub fn discovery(
        &self, arena: &TextArena, spans: &SpanArena,
    ) -> Result<Vec<Sp<DiscoveryRule>>, Diagnostics<DiscoveryDirectiveError>> {
        let source = SourceView { unit: self, arena, spans };
        SourceScan::run(source, DiscoveryAnalyzer::new(source)).into_result()
    }
}
