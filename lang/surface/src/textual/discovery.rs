use super::syntax::*;
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

impl SourceUnit {
    /// Parse discovery instructions only; ordinary source loading never expands them.
    pub fn discovery(
        &self, arena: &TextArena, spans: &SpanArena,
    ) -> Result<Vec<Sp<DiscoveryRule>>, DiscoveryDirectiveError> {
        let sites = self.annotations(MetadataKind::Discover, arena, spans);
        let Some((meta, _)) = sites.first().copied() else { return Ok(Vec::new()) };
        let span = spans[&EntityId::Meta(meta)];
        if let Some((second, _)) = sites.get(1) {
            return Err(DiscoveryDirectiveError::Duplicate {
                span: spans[&EntityId::Meta(*second)],
                first: span,
            });
        }
        if arena.annotation(self.root, MetadataKind::Discover) != Some(meta) {
            return Err(DiscoveryDirectiveError::Placement { span });
        }
        let rules =
            DiscoveryRule::decode(arena.semantic_meta(meta).arguments()).map_err(|source| {
                let index = match &source {
                    | DiscoveryAnnotationError::Rule { index }
                    | DiscoveryAnnotationError::Pattern { index, .. } => *index,
                };
                let argument = arena.metas[&meta].arguments()[index];
                let at = match &source {
                    | DiscoveryAnnotationError::Pattern { pattern, .. } => {
                        arena.metas[&argument].arguments()[*pattern]
                    }
                    | _ => argument,
                };
                DiscoveryDirectiveError::Invalid { span: spans[&EntityId::Meta(at)], source }
            })?;
        Ok(rules
            .into_iter()
            .zip(
                arena.metas[&meta]
                    .arguments()
                    .iter()
                    .flat_map(|rule| arena.metas[rule].arguments()),
            )
            .map(|(inner, meta)| Sp { inner, info: spans[&EntityId::Meta(*meta)] })
            .collect())
    }
}
