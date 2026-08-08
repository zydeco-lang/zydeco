use crate::source::{SourceFile, SourceGraph, SourceId};
use std::{ops::Range, path::Path};
use zydeco_surface::textual::UnattachedDocumentationWarning;

/// A non-fatal issue discovered while interpreting one parsed source file.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SourceWarning {
    UnattachedDocumentation(UnattachedDocumentationWarning),
}

impl SourceWarning {
    /// Stable tooling identifier for this warning kind.
    pub fn code(&self) -> &'static str {
        match self {
            | Self::UnattachedDocumentation(_) => "unattached-documentation-comment",
        }
    }

    /// Primary user-facing warning text.
    pub fn message(&self) -> &'static str {
        match self {
            | Self::UnattachedDocumentation(_) => {
                "documentation comment is not attached to `@[doc]` and has no effect"
            }
        }
    }

    /// Actionable guidance shared by diagnostic frontends.
    pub fn note(&self) -> &'static str {
        match self {
            | Self::UnattachedDocumentation(_) => {
                "Place `@[doc]` immediately after the block, or use `--` for an ordinary comment."
            }
        }
    }

    /// Byte range occupied by the ineffective source construct.
    pub fn range(&self) -> &Range<usize> {
        match self {
            | Self::UnattachedDocumentation(warning) => &warning.range,
        }
    }
}

impl From<UnattachedDocumentationWarning> for SourceWarning {
    fn from(warning: UnattachedDocumentationWarning) -> Self {
        Self::UnattachedDocumentation(warning)
    }
}

/// One source warning together with its source-graph provenance.
#[derive(Clone, Copy, Debug)]
pub struct SourceWarningSite<'graph> {
    pub source: SourceId,
    pub file: &'graph SourceFile,
    pub warning: &'graph SourceWarning,
}

impl SourceWarningSite<'_> {
    pub fn path(&self) -> &Path {
        &self.file.path
    }

    pub fn warning_source(&self) -> &str {
        &self.file.source[self.warning.range().clone()]
    }
}

impl SourceGraph {
    /// Return every source warning in deterministic provider-before-consumer
    /// and source order.
    pub fn warnings(&self) -> Vec<SourceWarningSite<'_>> {
        self.provider_order()
            .into_iter()
            .flat_map(|source| {
                let file = &self.sources[&source];
                file.warnings.iter().map(move |warning| SourceWarningSite { source, file, warning })
            })
            .collect()
    }
}
